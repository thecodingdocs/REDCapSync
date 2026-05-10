#' @noRd
save_project_dataset <- function(project, dataset_name) {
  id_col <- project$metadata$id_col
  dataset_details <- project$datasets[[dataset_name]]
  data_list <- load_project_dataset(project = project,
                                    dataset_name = dataset_name)
  records <- extract_project_records(data_list)[[id_col]]
  # consider also saving as Rdata to lazy load
  data_list <- save_project_data_list(data_list,
                                      with_links = dataset_details$with_links,
                                      separate = dataset_details$separate,
                                      use_csv = dataset_details$use_csv,
                                      dir_other = dataset_details$dir_other,
                                      file_name = dataset_details$file_name)
  dataset_details <- data_list$dataset_details # account potential for conflicts
  last_save_time <- dataset_details$last_save_time
  final_form_tab_names <- dataset_details$final_form_tab_names
  project$datasets[[dataset_name]]$n_records <- dataset_details$n_records
  project$datasets[[dataset_name]]$last_save_time <- last_save_time
  project$datasets[[dataset_name]]$final_form_tab_names <- final_form_tab_names
  row_new <- which(project$record_summary[[id_col]] %in% records)
  project$record_summary[[dataset_name]][row_new] <- TRUE
  project$record_summary$was_saved[row_new] <- TRUE
  project$internals$last_dataset_save <- now_time()
  invisible(project)
}
#' @noRd
save_project_data_list <- function(data_list,
                                   with_links = TRUE,
                                   separate = FALSE,
                                   use_csv = FALSE,
                                   dir_other = NULL,
                                   file_name = NULL) {
  id_col <- data_list$metadata$id_col
  dataset_details <- data_list$dataset_details
  dataset_name <- dataset_details$dataset_name
  file_extension <- ifelse(use_csv, ".csv", ".xlsx")
  if (is.null(dir_other)) {
    dir_path <- data_list$project_details$dir_path
    dir_other <- file.path(dir_path, "output")
  }
  if (is.null(file_name)) {
    project_name <- data_list$project_details$project_name
    file_name <- paste0(project_name, "_", dataset_name)
  }
  file_path <- file.path(dir_other, paste0(file_name, file_extension))
  dataset_details$with_links <- with_links
  dataset_details$separate <- separate
  dataset_details$use_csv <- use_csv
  dataset_details$dir_other <- dir_other
  dataset_details$file_name <- file_name
  dataset_details$file_path <- file_path
  # add headers-------
  form_names <- names(data_list$data)
  header_df_list <- construct_header_list(data_list)
  key_cols_list <- get_key_col_list(data_list)
  cols_start <- 4L
  if (dataset_name == "REDCapSync_raw") {
    cols_start <- 1L
    header_df_list <- NULL
  }
  # track form names
  link_col_list <- list()
  if (dataset_details$with_links) {
    if (length(data_list$data) > 0L) {
      data_list$data <- data_list$data |>
        lapply(function(form) {
          add_redcap_links(form, data_list)
        })
      if (dataset_details$include_records) {
          data_list$records <- add_redcap_links(form = data_list$records,
                                                project = data_list)
      }
      #check for conflicting name
      link_col_list <- list("redcap_link")
      names(link_col_list) <- id_col
    }
  }
  data_list <- data_list_to_save(data_list)
  # save -----
  last_save_time <- now_time()
  final_form_tab_names <- rename_list_names_excel(list_names = names(data_list))
  names(final_form_tab_names) <- names(data_list)
  dataset_details$last_save_time <- last_save_time
  dataset_details$final_form_tab_names <- final_form_tab_names
  dataset_details$raw_form_names <- form_names
  retained_details <- dataset_details
  dataset_details$cols_start <- cols_start
  value <- dataset_details |>
    lapply(function(x_row) {
      paste0(x_row, collapse = " | ")
    }) |>
    unlist() |>
    unname()
  data_list$dataset_details <- data.frame(
    paramater = names(dataset_details),
    value = value
  )
  if (dataset_details$use_csv) {
    list_to_csv(
      input_list = data_list,
      dir = dataset_details$dir_other,
      file_name = dataset_details$file_name,
      overwrite = TRUE
    ) # account for links with CSV like new column
  } else {
    list_to_excel(
      input_list = data_list,
      dir = dataset_details$dir_other,
      separate = dataset_details$separate,
      link_col_list = link_col_list,
      key_cols_list = key_cols_list,
      file_name = dataset_details$file_name,
      header_df_list = header_df_list,
      overwrite = TRUE
    )
  }
  data_list$dataset_details <- retained_details
  invisible(data_list)
}
#' @noRd
save_project_datasets <- function(project, hard_reset = FALSE) {
  assert_setup_project(project)
  if (is_something(project$data)) {
    dataset_names <- check_datasets(project)
    if (hard_reset) {
      dataset_names <- names(project$datasets)
    }
    if (is_something(dataset_names)) {
      for (dataset_name in dataset_names) {
        warning_message <- "Failed to save dataset `{dataset_name}`."
        project <- tryCatch(expr = save_project_dataset(project, dataset_name),
                            error = function(e) {
                              cli_alert_warning(warning_message)
                              invisible(project)
                            })
      }
    }
  }
  invisible(project)
}
#' @noRd
generate_project_dataset <- function(project,
                                     dataset_name,
                                     transformation_type = "default",
                                     merge_form_name = "merged",
                                     filter_field = NULL,
                                     filter_choices = NULL,
                                     filter_list = NULL,
                                     filter_strict = TRUE,
                                     field_names = NULL,
                                     form_names = NULL,
                                     exclude_identifiers = TRUE,
                                     exclude_free_text = FALSE,
                                     date_handling = "none",
                                     labelled = TRUE,
                                     clean = TRUE,
                                     drop_blanks = FALSE,
                                     drop_missing_codes = FALSE,
                                     drop_others = NULL,
                                     include_metadata = TRUE,
                                     include_users = TRUE,
                                     include_records = TRUE,
                                     include_log = FALSE,
                                     annotate_from_log = TRUE,
                                     include_comments = FALSE) {
  assert_setup_project(project)
  id_col <- project$metadata$id_col
  if (missing(dataset_name)) {
    cli_abort("You must name generated datasets with 'dataset_name'")
  }
  assert_env_name(dataset_name, max.chars = 31L)
  assert_env_name(merge_form_name, max.chars = 31L)
  assert_choice(date_handling, choices = DATE_HANDLING_CHOICES)
  # add more asserts
  # function to do asserts here
  assert_choice(transformation_type, TRANFORMATION_TYPES)
  data_list <- NULL
  # add new fields
  if (labelled != project$settings$labelled) {
    if (project$settings$labelled) {
      project <- labelled_to_raw_project(project)
    } else {
      project <- raw_to_labelled_project(project)
    }
  }
  data_list$metadata <- project$metadata
  data_list$data <- project$data
  data_list$redcap <- project$redcap
  data_list$links <- project$links
  record_sum <- project$record_summary
  data_list <- metadata_add_default_cols(data_list)
  #cache or store these to make it faster?
  if (transformation_type == "default") {
    data_list <- merge_non_repeating(
      data_list = data_list,
      merge_form_name = merge_form_name,
      merge_to_rep = TRUE
    )
  }
  if (transformation_type == "merge_non_repeating") {
    data_list <- merge_non_repeating(
      data_list = data_list,
      merge_form_name = merge_form_name,
      merge_to_rep = FALSE
    )
  }
  data_list$data <- filter_data_list(
    data_list = data_list,
    field_names = field_names,
    form_names = form_names,
    filter_field = filter_field,
    filter_choices = filter_choices,
    filter_list = filter_list,
    filter_strict = filter_strict
  )
  data_list$data <- deidentify_data_list(
    data_list = data_list,
    exclude_identifiers = exclude_identifiers,
    exclude_free_text = exclude_free_text,
    date_handling = date_handling
  )
  if (clean) {
    #include warning for if missing codes will prevent uploads
    if (is_something(data_list$metadata$missing_codes)) {
      if (drop_missing_codes) {
        if (labelled) {
          exclude_these <- data_list$metadata$missing_codes$name
        } else {
          exclude_these <- data_list$metadata$missing_codes$code
        }
        drop_others <- drop_others |>
          append(exclude_these) |>
          unique() |>
          drop_nas()
      }
    }
    data_list$data <- clean_data_list(
      data_list = data_list,
      labelled = labelled,
      drop_blanks = drop_blanks,
      drop_others = drop_others
    )
  }
  if (include_metadata) {
    if (is_something(data_list$metadata)) {
      data_list$metadata$forms <- annotate_forms(
        data_list = data_list,
        annotate = TRUE,
        drop_blanks = drop_blanks
      )
      data_list$metadata$fields <- annotate_fields(
        data_list = data_list,
        annotate = TRUE,
        drop_blanks = drop_blanks
      )
      data_list$metadata$choices <- annotate_choices(
        data_list = data_list,
        annotate = TRUE,
        drop_blanks = drop_blanks
      )
    }
  }
  records <- extract_project_records(data_list)[[1L]]
  record_sum <- record_sum[which(record_sum[[id_col]] %in% records), ]
  rownames(record_sum) <- NULL
  data_list$records <- record_sum
  if (!data_list$redcap$has_log_access) {
    cli_alert_warning("You don't have log access so that data can't be used.")
    include_log <- FALSE
    include_comments <- FALSE
    annotate_from_log <- FALSE
  } else{
    data_list$log <- extract_log(data_list = data_list, records = records)
    if (include_comments) {
      data_list$comments <- generate_comment_table(redcap_log = data_list$log,
                                                   only_most_recent = TRUE)
    }
  }
  if (include_records) {
    if (!is.null(records)) {
      data_list$records <- annotate_records(data_list = data_list,
                                            annotate = annotate_from_log)
    }
  }
  if (include_users) {
    if (data_list$redcap$has_user_access) {
      data_list$users <- annotate_users(
        data_list = data_list,
        records = records,
        annotate = annotate_from_log,
        drop_blanks = drop_blanks
      )
    } else {
      cli_alert_warning("You don't have user access that can't be included.")
      include_users <- FALSE
    }
  }
  data_list$redcap <- NULL
  if (is.null(filter_list)) {
    if (!is.null(filter_choices) && !is.null(filter_field)) {
      filter_list <- list(filter_choices)
      names(filter_list) <- filter_field
    } else {
      # warning
    }
  }
  # add headers-------
  form_names <- names(data_list$data)
  n_records <- length(records)
  data_list$dataset_details <- list(
    dataset_name = dataset_name,
    transformation_type = transformation_type,
    merge_form_name = merge_form_name,
    filter_list = filter_list,
    filter_strict = filter_strict,
    field_names = field_names,
    form_names = form_names,
    exclude_identifiers = exclude_identifiers,
    exclude_free_text = exclude_free_text,
    date_handling = date_handling,
    labelled = labelled,
    clean = clean,
    drop_blanks = drop_blanks,
    drop_missing_codes = drop_missing_codes,
    drop_others = drop_others,
    include_metadata = include_metadata,
    include_records = include_records,
    include_users = include_users,
    include_log = include_log,
    annotate_from_log = annotate_from_log,
    include_comments = include_comments,
    with_links = NULL,
    separate = NULL,
    use_csv = NULL,
    dir_other = NULL,
    file_name = NULL,
    file_path = NULL,
    n_records = n_records,
    last_save_time = NULL,
    final_form_tab_names = NULL
  )
  data_list$project_details <- list(
    project_name = project$project_name,
    dir_path = project$dir_path,
    n_records = nrow(project$record_summary),
    help = project$links$help,
    is_test = project$internals$is_test
  )
  invisible(data_list)
}
#' @noRd
load_project_dataset <- function(project, dataset_name) {
  assert_setup_project(project)
  assert_env_name(dataset_name, max.chars = 31L)
  if (!dataset_name %in% names(project$datasets)) {
    cli_abort("{dataset_name} is not included in the current project datasets")
  } # can save to R_objects for faster retrieval
  dataset_details <- project$datasets[[dataset_name]]#warning for other params?
  data_list <- generate_project_dataset( # filter for adding new params
    project = project,
    dataset_name = dataset_name,
    transformation_type = dataset_details$transformation_type, # verificaiton?
    merge_form_name = dataset_details$merge_form_name,
    filter_list = dataset_details$filter_list,
    filter_strict = dataset_details$filter_strict,
    field_names = dataset_details$field_names,
    form_names = dataset_details$form_names,
    exclude_identifiers = dataset_details$exclude_identifiers,
    exclude_free_text = dataset_details$exclude_free_text,
    date_handling = dataset_details$date_handling,
    labelled = dataset_details$labelled,
    clean = dataset_details$clean,
    drop_blanks = dataset_details$drop_blanks,
    drop_missing_codes = dataset_details$drop_missing_codes,
    drop_others = dataset_details$drop_others,
    include_metadata = dataset_details$include_metadata,
    include_records = dataset_details$include_records,
    include_users = dataset_details$include_users,
    include_log = dataset_details$include_log,
    annotate_from_log = dataset_details$annotate_from_log,
    include_comments = dataset_details$include_comments
  )
  invisible(data_list)
}
#' @noRd
add_project_dataset <- function(project,
                                dataset_name,
                                transformation_type = "default",
                                merge_form_name = "merged",
                                filter_field = NULL,
                                filter_choices = NULL,
                                filter_list = NULL,
                                filter_strict = TRUE,
                                field_names = NULL,
                                form_names = NULL,
                                exclude_identifiers = TRUE,
                                exclude_free_text = FALSE,
                                date_handling = "none",
                                labelled = TRUE,
                                clean = TRUE,
                                drop_blanks = FALSE,
                                drop_missing_codes = FALSE,
                                drop_others = NULL,
                                include_metadata = TRUE,
                                include_records = TRUE,
                                include_users = TRUE,
                                include_log = FALSE,
                                annotate_from_log = TRUE,
                                include_comments = TRUE,
                                with_links = TRUE,
                                separate = FALSE,
                                use_csv = FALSE,
                                dir_other = NULL,
                                file_name = NULL,
                                hard_reset = FALSE) {
  # sync_frequency ... project$settings$sync_frequency
  assert_env_name(dataset_name, max.chars = 31L)
  if (is.null(dir_other)) {
    dir_other <- file.path(project$dir_path, "output")
  }# else warn will not work on different computer
  if (is.null(file_name)) {
    file_name <- paste0(project$project_name, "_", dataset_name)
  }
  if (is.null(filter_list)) {
    if (!is.null(filter_choices) && !is.null(filter_field)) {
      filter_list <- list(filter_choices)
      names(filter_list) <- filter_field
    } else {
      # warning
    }
  }
  file_extension <- ifelse(use_csv, ".csv", ".xlsx")
  dataset_list_new <- list(
    dataset_name = dataset_name,
    transformation_type = transformation_type,
    merge_form_name = merge_form_name,
    filter_list = filter_list,
    filter_strict = filter_strict,
    field_names = field_names,
    form_names = form_names,
    exclude_identifiers = exclude_identifiers,
    exclude_free_text = exclude_free_text,
    date_handling = date_handling,
    labelled = labelled,
    clean = clean,
    drop_blanks = drop_blanks,
    drop_missing_codes = drop_missing_codes,
    drop_others = drop_others,
    include_metadata = include_metadata,
    include_records = include_records,
    include_users = include_users,
    include_log = include_log,
    annotate_from_log = annotate_from_log,
    include_comments = include_comments,
    with_links = with_links,
    separate = separate,
    use_csv = use_csv,
    dir_other = dir_other,
    file_name = file_name,
    file_path = file.path(dir_other, paste0(file_name, file_extension)),
    n_records = NULL,
    last_save_time = NULL,
    final_form_tab_names = NULL
  )
  dataset_list_old <- project$datasets[[dataset_name]]
  if (!is.null(dataset_list_old) && !hard_reset) {
    important_vars <- names(dataset_list_new) |>
      vec1_not_in_vec2(NOT_IMPORTANT_DATASET_NAMES)
    are_identical <- identical(dataset_list_new[important_vars],
                               dataset_list_old[important_vars])
    if (are_identical) {
      # optional message?
      return(invisible(project))
    }
  }
  project$datasets[[dataset_name]] <- dataset_list_new
  project$record_summary[[dataset_name]] <- FALSE
  invisible(project)
}
#' @noRd
deidentify_data_list <- function(data_list,
                                 exclude_identifiers = TRUE,
                                 exclude_free_text = FALSE,
                                 exclude_additional = NULL,
                                 date_handling = "none") {
  # assert_data_list contains data and metadata with forms and fields
  assert_choice(date_handling, choices = DATE_HANDLING_CHOICES)
  assert_logical(exclude_identifiers)
  assert_logical(exclude_free_text)
  data_forms <- data_list$data
  metadata <- data_list$metadata
  fields <- metadata$fields
  exclusions <- exclude_additional
  if (exclude_identifiers) {
    is_marked_id <- fields$identifier == "y"
    validation_type <- fields$text_validation_type_or_show_slider_number
    is_likely_id <- validation_type %in% REDCAP_MAYBE_IDS_STRICT
    initial_identifiers <- fields$field_name[which(is_marked_id | is_likely_id)]
    if (length(initial_identifiers) == 0L) {
      warning(
        "You have no identifiers marked in ",
        "`project$metadata$fields$identifier`. ",
        "You can set it in REDCap Project Setup and update ",
        "project OR define your idenitifiers in this functions ",
        "`identifiers` argument.",
        call. = TRUE,
        immediate. = TRUE
      )
    }
    exclusions <- exclusions |>
      append(initial_identifiers) |>
      unique()
  }
  bad_identifiers <- exclusions[which(!exclusions %in% fields$field_name)]
  if (length(bad_identifiers) > 0L) {
    stop(
      "There is a bad identifier. see `fields$field_name`: ",
      toString(bad_identifiers)
    )
  }
  id_cols <- metadata$form_key_cols |> unlist() |> unique()
  if (is_something(id_cols) && any(id_cols %in% exclusions)) {
    stop("ID cols not allowed... ",
         toString(id_cols),
         " <-- use hashing (in dev)")
  }
  if (exclude_free_text) {
    # get_identifier_fields here
    # drop free text only if there is no validation
    # make function for that ?external
    is_text_field <- fields$field_type == "text"
    is_note_field <- fields$field_type == "notes"
    not_id <- !fields$field_name %in% id_cols
    has_no_valid <- is.na(fields$text_validation_type_or_show_slider_number)
    # ? & fields$in_original_redcap
    is_free_text <- is_text_field & has_no_valid & not_id
    free_text_rows <- which(is_note_field | is_free_text)
    free_text_fields <- fields$field_name[free_text_rows]
    exclusions <- exclusions |>
      append(free_text_fields) |>
      unique()
  }
  if (is_something(data_forms)) {
    date_vector <- fields$field_name[which(fields$field_type_r == "date")]
    date_list <- Map(
      f = function(x, col_names) {
        date_vector[which(date_vector %in% col_names)]
      },
      names(data_forms),
      lapply(data_forms, colnames)
    )
    if (date_handling != "none") {
      if (date_handling == "exclude_dates") {
        exclusions <- exclusions |>
          append(date_vector) |>
          unique()
      }
      if (date_handling %in% c("random_shift_by_record",
                               "random_shift_by_project",
                               "zero_by_record",
                               "zero_by_project")) {
        number <- 90L # can set in options
        shift_range <- setdiff(-number:number, 0L)
        min_dates <- get_min_dates(data_list)
        if (date_handling == "random_shift_by_record") {
          min_dates$shift_amount <- sample(shift_range,
                                           size = nrow(min_dates),
                                           replace = TRUE)
        }
        if (date_handling == "random_shift_by_project") {
          min_dates$shift_amount <-
            sample(shift_range, size = 1L, replace = TRUE)
        }
        if (date_handling == "zero_by_record") {
          # should you edit fields to now be field_type_r integer?
          min_dates$shift_amount <- min_dates$date
        }
        if (date_handling == "zero_by_project") {
          # should you edit fields to now be field_type_r integer?
          min_dates$shift_amount <- min(min_dates$date)
        }
        for (form_name in names(date_list)) {
          field_record <- data_forms[[form_name]][[id_cols[1L]]]
          match_date_diff <- match(field_record, min_dates$record_id)
          difference <- min_dates$shift_amount[match_date_diff]
          for (field_name in date_list[[form_name]]) {
            field <- data_forms[[form_name]][[field_name]]
            data_forms[[form_name]][[field_name]] <-
              as.character(as.Date(field) - difference)
          }
        }
      }
      # if you have dates you already mutated no need to drop anymore
    }
    drop_list <- Map(function(x, col_names) {
      exclusions[which(exclusions %in% col_names)]
    }, names(data_forms), lapply(data_forms, colnames))
    drop_list <- drop_list[unlist(lapply(drop_list, length)) > 0L]
    for (form_name in names(drop_list)) {
      for (field_name in drop_list[[form_name]]) {
        data_forms[[form_name]][[field_name]] <- NULL
      }
    }
  }
  invisible(data_forms)
}
#' @noRd
filter_data_list <- function(data_list,
                             field_names = NULL,
                             form_names = NULL,
                             filter_field = NULL,
                             filter_choices = NULL,
                             filter_list = NULL,
                             filter_strict = TRUE) {
  if (is.null(field_names)) {
    field_names <- get_all_field_names(data_list)
  }
  # add assert/warning for accurate field_names
  if (is.null(form_names)) {
    form_names <- data_list$metadata$forms$form_name
  }
  the_rows <- which(!field_names %in% data_list$metadata$raw_structure_cols)
  field_names_minus <- field_names[the_rows]
  if (length(field_names_minus) > 0L) {
    form_names_minus <- field_to_form_names(project = data_list,
                                            field_names = field_names_minus,
                                            strict = TRUE)
    form_names <- vec1_in_vec2(form_names, form_names_minus)
  }
  # missing_filter ?
  out_list <- list()
  if (is.null(filter_list)) {
    if (!is.null(filter_field) && !is.null(filter_choices)) {
      filter_list <- list(filter_choices)
      names(filter_list) <- filter_field
    }
  } else if (!is.null(filter_field) || !is.null(filter_choices)) {
    warning_message <- "use `filter_list` or `filter_field` & `filter_choices`"
    cli_alert_warning(warning_message)
  }
  filter_field_names <- NULL
  if (!is.null(filter_list)) {
    filter_field_names <- filter_list |>
      names() |>
      drop_if("")
    # should be unique
    filter_form <- field_to_form_names(project = data_list,
                                       field_names = filter_field_names)
    if (length(filter_field_names) == 1L) {
      if (filter_field_names == data_list$metadata$id_col) {
        filter_form <- data_list$metadata$forms$form_name[1L]
        # RISKY? id_position like REDCapR, add to setup
      }
    }
    # should be length 1
    if (length(filter_form) > 1L) {
      stop("You can only filter_list by multiple columns part of one form")
    }
    form_key_cols <- data_list$metadata$form_key_cols |>
      unlist() |>
      unique()
    is_key <- all(filter_field_names %in% form_key_cols)
  }
  # can use this to have repeats capture non-rep events
  for (form_name in form_names) {
    form <- data_list$data[[form_name]]
    if (is_something(form)) {
      row_index <- seq_len(nrow(form))
      if (!is.null(filter_list)) {
        row_logic <- NULL
        for (filter_field_name in filter_field_names) {
          filter_field_final <- filter_field_name
          filter_choices_final <- filter_list[[filter_field_name]]
          if (!filter_strict) {
            if (!is_key) {
              # need to account for instances
              if (form_name != filter_form) {
                filter_field_final <- data_list$metadata$id_col
                filtered_form <- data_list$data[[filter_form]]
                name_filtered <- filtered_form[[filter_field_name]]
                final_filtered <- filtered_form[[filter_field_final]]
                the_rows <- which(name_filtered %in% filter_choices_final)
                filter_choices_final <- unique(final_filtered[the_rows])
              }
            }
          }
          index_test <- rep(FALSE, nrow(form))
          if (filter_field_final %in% colnames(data_list$data[[form_name]])) {
            index_test <- data_list$data[[form_name]][[filter_field_final]] %in%
              filter_choices_final
          }
          if (is.null(row_logic)) {
            row_logic <- index_test
          }
          field_index <- which(names(filter_list) == filter_field_name)
          op_index <- (field_index - 1L)
          if (op_index <= length(filter_list)) {
            if (field_index != 1L) {
              is_and <- filter_list[[op_index]] == "and"
              if (is_and) {
                row_logic <- row_logic & index_test
              } else {
                row_logic <- row_logic | index_test
              }
            }
          }
        }
        if (is.null(row_logic))
          row_logic <- NA
        row_index <- which(row_logic)
      }
      field_names_adj <- c(field_names, filter_field_names)
      col_names <- colnames(form)[which(colnames(form) %in% field_names_adj)]
      if (length(row_index) > 0L && length(col_names) > 0L) {
        col_names <- colnames(form)[which(colnames(form) %in% unique(
          c(data_list$metadata$form_key_cols[[form_name]], field_names_adj)
        ))]
        out_list[[form_name]] <- form[row_index, col_names, drop = FALSE]
      }
    }
  }
  invisible(out_list)
}
#' @noRd
clean_data_list <- function(data_list,
                            labelled,
                            drop_blanks = TRUE,
                            drop_others = NULL) {
  # assert data list
  data_forms <- data_list$data
  metadata <- data_list$metadata
  for (form_name in names(data_forms)) {
    data_forms[[form_name]] <- clean_form(
      form = data_forms[[form_name]],
      fields = metadata$fields,
      labelled = labelled,
      drop_blanks = drop_blanks,
      drop_others = drop_others
    )
  }
  invisible(data_forms)
}
#' @noRd
merge_non_repeating <- function(data_list,
                                merge_form_name,
                                merge_to_rep = FALSE) {
  forms_transformed <- data_list$metadata$forms
  form_colnames <- colnames(data_list$metadata$forms)
  is_longitudinal <- "repeating_via_events" %in% form_colnames
  if (is_longitudinal) {
    new_order <- order(forms_transformed$repeating_via_events)
    forms_transformed <- forms_transformed[new_order, ]
  }
  new_order <- order(forms_transformed$repeating)
  forms_transformed <- forms_transformed[new_order, ]
  forms_transformed$form_name_remap <- forms_transformed$form_name
  forms_transformed$form_label_remap <- forms_transformed$form_label
  row_check <- !forms_transformed$repeating
  if (is_longitudinal) {
    row_check <- row_check & !forms_transformed$repeating_via_events
  }
  forms_transformed$form_name_remap[which(row_check)] <- merge_form_name
  merge_form_name_label <- merge_form_name # can captialize here
  if (merge_form_name %in% forms_transformed$form_name) {
    label_match <- which(forms_transformed$form_name == merge_form_name)
    merge_form_name_label <- forms_transformed$form_label[label_match]
  }
  non_rep_form_names <- forms_transformed$form_name[which(row_check)]
  forms_transformed$form_label_remap[which(row_check)] <- merge_form_name_label
  forms_transformed_original <- forms_transformed
  cols_to_keep <- c(
    "form_name_remap",
    "form_label_remap",
    "repeating",
    "repeating_via_events",
    "key_cols",
    "key_names",
    ""
  )
  included_cols <- which(cols_to_keep %in% colnames(forms_transformed))
  cols_to_keep <- cols_to_keep[included_cols]
  forms_transformed <- unique(forms_transformed[, cols_to_keep])
  name_cols <- which(colnames(forms_transformed) == "form_name_remap")
  colnames(forms_transformed)[name_cols] <- "form_name"
  label_cols <- which(colnames(forms_transformed) == "form_label_remap")
  colnames(forms_transformed)[label_cols] <- "form_label"
  forms_transformed$original_form_name <- forms_transformed$form_name |>
    lapply(function(form_name) {
      keep <- which(forms_transformed_original$form_name_remap == form_name)
      paste0(forms_transformed_original$form_name[keep], collapse = " | ")
    }) |>
    unlist() |>
    as.character()
  data_list$metadata$forms <- forms_transformed
  fields <- data_list$metadata$fields
  fields$original_form_name <- fields$form_name
  new_match <- match(fields$form_name, forms_transformed_original$form_name)
  fields$form_name <- forms_transformed_original$form_name_remap[new_match]
  new_order <- order(match(fields$form_name, forms_transformed$form_name))
  fields <- fields[new_order, ]
  # new function RosyUtils
  first <- 1L:which(colnames(fields) == "form_name")
  move <- which(colnames(fields) == "original_form_name")
  last <- which(colnames(fields) != "original_form_name")[-first]
  fields <- fields[, c(first, move, last)]
  data_list$metadata$fields <- fields
  data_list$metadata$choices <- fields_to_choices(fields)
  data_list
  data_list$metadata$form_key_cols <- get_key_col_list(data_list = data_list)
  if (is_something(data_list$data)) {
    merge_form <- NULL
    keep_rows <- non_rep_form_names |>
      lapply(function(non_rep_form_name) {
        is_something(data_list$data[[non_rep_form_name]])
      }) |>
      unlist() |>
      which()
    non_rep_form_names <- non_rep_form_names[keep_rows]
    i <- 0L
    for (non_rep_form_name in non_rep_form_names) {
      if (non_rep_form_name == non_rep_form_names[[1L]]) {
        merge_form <- data_list$data[[non_rep_form_name]]
      } else {
        to_be_merged <- data_list$data[[non_rep_form_name]]
        to_be_merged$arm_number <- NULL
        merge_form <- merge(
          x = merge_form,
          y = to_be_merged,
          by = data_list$metadata$id_col,
          all = TRUE,
          sort = FALSE,
          suffixes = c("", paste0("_merged_", i))
        )
      }
      data_list$data[[non_rep_form_name]] <- NULL
      i <- i + 1L
    }
    data_list$data[[merge_form_name]] <- merge_form
    other_forms <- setdiff(names(data_list$data), merge_form_name)
    if (merge_to_rep) {
      for (other_form in other_forms) {
        data_list$data[[other_form]] <- merge(
          x = data_list$data[[other_form]],
          y = merge_form,
          by = data_list$metadata$id_col,
          all.x = TRUE,
          sort = FALSE,
          suffixes = c("", "_merged")
        )
      }
    }
    data_list$data <- data_list$data[c(merge_form_name, other_forms)]
  }
  data_list
}
#' @noRd
fields_to_choices <- function(fields) {
  fields <- fields[which(fields$field_type %in% REDCAP_FACTOR_FIELDS), ]
  fields <- fields[which(!is.na(fields$select_choices_or_calculations)), ]
  choices <- NULL
  for (i in seq_len(nrow(fields))) {
    field_name <- fields$field_name[i]
    form_name <- fields$form_name[i]
    field_label <- fields$field_label[i]
    field_type <- fields$field_type[i]
    selections <- split_choices(fields$select_choices_or_calculations[i])
    choices <- choices |> bind_rows(
      data.frame(
        form_name = form_name,
        field_name = field_name,
        field_type = field_type,
        field_label = ifelse(!is.na(field_label), field_label, field_name),
        code = selections$code,
        name = selections$name
      )
    )
  }
  choices$label <- paste(choices$form_name,
                         "-",
                         choices$field_label,
                         "-",
                         choices$name)
  rownames(choices) <- NULL
  choices
}
#' @noRd
add_labels_to_checkbox <- function(fields) {
  row_index <- which(fields$field_type == "checkbox_choice")
  x <- fields$field_name[row_index] |>
    strsplit("___") |>
    lapply(function(x) {
      x[[1L]]
    }) |>
    unlist()
  y <- fields$field_label[row_index]
  z <- paste0(fields$field_label[match(x, fields$field_name)], " - ", y)
  fields$field_label[row_index] <- z
  fields
}
#' @noRd
annotate_fields <- function(data_list,
                            annotate = TRUE,
                            drop_blanks = FALSE) {
  fields <- data_list$metadata$fields # [,colnames(data_list$metadata$fields)]
  if (drop_blanks) {
    keep_rows <- which(fields$field_name %in% get_all_field_names(data_list))
    fields <- fields[keep_rows, ]
    #fix transform dropping missing fields
  }
  if (nrow(fields) > 0L && is_something(data_list$data)) {
    if (annotate) {
      skimmed <- NULL
      for (form_name in drop_nas(unique(fields$form_name))) {
        col_names <- fields$field_name[which(fields$form_name == form_name)]
        form <- data_list$data[[form_name]]
        col_names <- col_names[which(col_names %in% colnames(form))]
        skimmed <- bind_rows(skimmed, skimr::skim(form[, col_names]))
      }
      field_names <- fields$field_name
      fields <-  merge(x = fields,
                       y = skimmed,
                       by.x = "field_name",
                       by.y = "skim_variable",
                       all = TRUE)
      fields <- field_names |>
        lapply(function(x) {
          fields[which(fields$field_name == x), ]
        }) |>
        bind_rows()
    }
  }
  fields
}
#' @noRd
annotate_forms <- function(data_list,
                           annotate = TRUE,
                           drop_blanks = FALSE) {
  forms <- data_list$metadata$forms
  if (drop_blanks) {
    forms <- forms[which(forms$form_name %in% names(data_list$data)), ]
  }
  if (nrow(forms) > 0L) {
    # add metadata info like n fields
    if (annotate) {
      var_list <- forms$form_name |> lapply(function(form_name) {
        paste0(form_name, "_complete")
      })
      if ("original_form_name" %in% colnames(forms)) {
        var_list <- forms$original_form_name |>
          strsplit(" [|] ") |>
          lapply(function(form_name) {
            paste0(form_name, "_complete")
          })
      }
      names(var_list) <- forms$form_name
      for (status in c("Incomplete", "Unverified", "Complete")) {
        forms[[tolower(status)]] <- forms$form_name |>
          lapply(function(form_name) {
            var_list[[form_name]] |>
              lapply(function(var_name) {
                (data_list$data[[form_name]][[var_name]] == status) |>
                  which() |>
                  length()
              }) |>
              unlist() |>
              paste0(collapse = " | ")
          }) |>
          unlist()
      }
    }
  }
  forms
}
#' @noRd
annotate_choices <- function(data_list,
                             annotate = TRUE,
                             drop_blanks = FALSE) {
  choices <- data_list$metadata$choices
  if (drop_blanks) {
    keep_rows <- which(choices$field_name %in% get_all_field_names(data_list))
    choices <- choices[keep_rows, ]
  }
  #used to have more code
  if (annotate) {
    choices$n <- seq_len(nrow(choices)) |>
      lapply(function(i) {
        form <- data_list$data[[choices$form_name[i]]]
        if (is.null(form)) {
          return(0L)
        }
        if (nrow(form) == 0L) {
          return(0L)
        }
        the_col <- choices$field_name[i]
        if (!the_col %in% colnames(form)) {
          return(0L)
        }
        sum(form[, the_col] == choices$name[i], na.rm = TRUE)
      }) |>
      unlist()
    choices$n_total <- seq_len(nrow(choices)) |>
      lapply(function(i) {
        form <- data_list$data[[choices$form_name[i]]]
        if (is.null(form)) {
          return(0L)
        }
        if (nrow(form) == 0L) {
          return(0L)
        }
        the_col <- choices$field_name[i]
        if (!the_col %in% colnames(form)) {
          return(0L)
        }
        sum(!is.na(form[, the_col]), na.rm = TRUE)
      }) |>
      unlist()
    choices$perc <- round((choices$n / choices$n_total), 4L)
    choices$perc_text <- (choices$perc * 100L) |>
      round(1L) |>
      paste0("%")
  }
  choices
}
#' @noRd
annotate_records <- function(data_list, annotate = TRUE) {
  id_col <- data_list$metadata$id_col
  records <- extract_project_records(data_list)[[1L]]
  the_rows <- which(data_list$records[[id_col]] %in% records)
  all_records <- data_list$records[the_rows, ]
  redcap_log <- extract_log(data_list = data_list, records = records)
  # convert to date level?
  if (!is_something(all_records) || !is_something(redcap_log)) {
    return(all_records)
  }
  if (annotate) {
    if (is_something(redcap_log)) {
      redcap_log <- redcap_log[, c("timestamp", "username", "record")]
      has_users <- is_something(data_list$redcap$users)
      if (has_users) {
        users <- data_list$redcap$users
        users <- users[, c("username", "firstname", "lastname", "email")]
        redcap_log <- merge(redcap_log, users, by = "username", all.x = TRUE)
      }
      cool_list <- split(redcap_log, redcap_log$record)
      if (length(cool_list) > 1L) {
        cool_df <- seq_along(cool_list) |> lapply(function(i) {
          x <- cool_list[[i]]
          the_last <- first(x)
          the_first <- last(x)
          out_df <- data.frame(
            record = the_first$record,
            first_timestamp = the_first$timestamp,
            last_timestamp = the_last$timestamp,
            unique_users = length_unique(x$username)
          )
          if (has_users) {
            out_df$last_username <- the_last$username
            out_df$last_user <- paste(the_last$firstname, the_last$lastname)
          }
          out_df
        }) |>
          bind_rows()
        colnames(cool_df)[1L] <- id_col
        all_records <- merge(all_records, cool_df, by = id_col, all.x = TRUE)
      }
    }
  }
  invisible(all_records)
}
#' @noRd
clean_form <- function(form,
                       fields,
                       labelled,
                       drop_blanks = TRUE,
                       drop_others = NULL) {
  name_code <- ifelse(labelled, 2L, 1L)
  for (field_name in colnames(form)) {
    if (field_name %in% fields$field_name) {
      x_row <- which(fields$field_name == field_name)
      x_units <- NULL
      if (!is.na(fields$units[x_row])) {
        x_units <- fields$units[x_row]
      }
      x_class <- fields$field_type_r[x_row][[1L]]
      label <- ifelse(is.na(fields$field_label[x_row]),
                      field_name,
                      fields$field_label[x_row])[[1L]]
      x_levels <- NULL
      if (!is.na(x_class)) {
        if (is_something(drop_others)) {
          erase_rows <- which(form[[field_name]] %in% drop_others)
          form[[field_name]][erase_rows] <- NA
        }
        if (x_class == "factor") {
          select_choices <- fields$select_choices_or_calculations[x_row]
          if (!is.na(select_choices)) {
            x_levels <- split_choices(select_choices)[[name_code]]
          } else {
            x_levels <- unique(form[[field_name]]) |> drop_nas()
          }
          if (anyDuplicated(x_levels) > 0L) {
            duplicate_levels <- x_levels |>
              duplicated() |>
              which()
            warning(
              "You have a variable (",
              field_name,
              ") with dupplicate names (",
              toString(x_levels[duplicate_levels]),
              "). This is not great but for this proccess they will be merged",
              "and treated as identical responses."
            )
            x_levels <- unique(x_levels)
          }
          if (drop_blanks) {
            x_levels <-
              x_levels[which(x_levels %in% unique(form[[field_name]]))]
          }
          if (is_something(drop_others)) {
            x_levels <- x_levels[which(!x_levels %in% drop_others)]
          }
        }
        if (x_class == "integer") {
          #warning about missing codes?
          form[[field_name]] <- as.integer(form[[field_name]])
        }
        if (x_class == "numeric") {
          #warning about missing codes?
          form[[field_name]] <- as.numeric(form[[field_name]])
        }
        form
      }
      form[[field_name]] <- clean_column_for_table(
        field = form[[field_name]],
        class = x_class,
        label = label,
        units = x_units,
        levels = x_levels
      )
    }
  }
  form
}
#' @noRd
clean_column_for_table <- function(field,
                                   class = NULL,
                                   label = NULL,
                                   units = NULL,
                                   levels = NULL) {
  if (is_something(class)) {
    if (class == "integer") {
      field <- as.integer(field)
    }
    if (class == "factor") {
      field <- factor(x = field, levels = levels, ordered = TRUE)
    }
    if (class == "numeric") {
      field <- as.numeric(field)
    }
  }
  if (is_something(label)) {
    attr(field, "label") <- label
  }
  if (is_something(units)) {
    attr(field, "units") <- units
  }
  field
}
#' @noRd
NOT_IMPORTANT_DATASET_NAMES <- c("n_records",
                                 "last_save_time",
                                 "final_form_tab_names")
#' @noRd
data_list_to_save <- function(data_list) {
  to_save_list <- NULL
  for (form_name in names(data_list$data)) {
    to_save_list[[form_name]] <- data_list$data[[form_name]]
  }
  if (data_list$dataset_details$include_metadata) {
    metadata_form_names <- c("forms", "fields", "choices", "missing_codes")
    metadata_names <- data_list$metadata[metadata_form_names] |>
      process_df_list(silent = TRUE) |>
      names()
    metadata_names_alt <- metadata_names
    if (any(metadata_names %in% names(data_list$data))) {
      metadata_names_alt <- paste0("redcap_", metadata_names)
      #check for conflicts
    }
    for (i in seq_along(metadata_names)) {
      to_save_list[[metadata_names_alt[i]]] <-
        data_list$metadata[[metadata_names[i]]]
    }
  }
  if (data_list$dataset_details$include_users) {
    users_name <- "users"
    users_name_alt <- users_name
    if (any(users_name %in% names(data_list$data))) {
      users_name_alt <- paste0("redcap_", users_name)
      #check for conflicts
    }
    if ("users" %in% names(data_list)) {
      to_save_list[[users_name_alt]] <- data_list$users
    }
  }
  if (data_list$dataset_details$include_records) {
    records_name <- "records"
    records_name_alt <- records_name
    if (any(records_name %in% names(data_list$data))) {
      records_name_alt <- paste0("redcap_", records_name)
      #check for conflicts
    }
    if ("records" %in% names(data_list)) {
      to_save_list[[records_name_alt]] <- data_list$records
    }
  }
  if (data_list$dataset_details$include_log) {
    log_name <- "log"
    log_name_alt <- log_name
    if (any(log_name %in% names(data_list$data))) {
      log_name_alt <- paste0("redcap_", log_name)
      #check for conflicts
    }
    if ("log" %in% names(data_list)) {
      to_save_list[[log_name_alt]] <- data_list$log
    }
  }
  if (data_list$dataset_details$include_comments) {
    comments_name <- "comments"
    comments_name_alt <- comments_name
    if (any(comments_name %in% names(data_list$data))) {
      comments_name_alt <- paste0("redcap_", comments_name)
      #check for conflicts
    }
    if ("comments" %in% names(data_list)) {
      to_save_list[[comments_name_alt]] <- data_list$comments
    }
  }
  to_save_list
}
#' @noRd
TRANFORMATION_TYPES <- c("default", "none", "merge_non_repeating")
#' @noRd
metadata_add_default_cols <- function(data_list) {
  fields <- data_list$metadata$fields
  fields <- fields[which(fields$field_type != "descriptive"), ]
  fields <- add_labels_to_checkbox(fields)
  fields <- fields[which(fields$field_type != "checkbox"), ]
  fields$field_label[which(is.na(fields$field_label))] <-
    fields$field_name[which(is.na(fields$field_label))]
  fields <- unique(fields$form_name) |>
    lapply(function(x) {
      fields[which(fields$form_name == x), ]
    }) |>
    bind_rows()
  fields$field_type_r <- field_types_to_r(fields)
  fields$in_original_redcap <- TRUE
  fields$original_form_name <- fields$form_name
  if (!"units" %in% colnames(fields)) {
    fields$units <- NA
  }
  if (!"field_label_short" %in% colnames(fields)) {
    fields$field_label_short <- fields$field_label
  }
  data_list$metadata$fields <- fields
  data_list
}
#' @noRd
field_types_to_r <- function(fields) {
  field_types_r <- rep("character", nrow(fields))
  field_types <- fields$field_type
  field_validations <- fields$text_validation_type_or_show_slider_number
  field_types_r[which(field_types %in% REDCAP_FACTOR_FIELDS)] <- "factor"
  is_text <- field_types == "text"
  is_date <- is_text & field_validations %in% REDCAP_TEXT_DATE_FIELDS
  is_datetime <- is_text & field_validations %in% REDCAP_TEXT_DATETIME_FIELDS
  is_int <- is_text & field_validations %in% REDCAP_INTEGER_FIELDS
  is_num <- is_text & field_validations %in% REDCAP_NUMERIC_FIELDS
  field_types_r[which(is_date)] <- "date"
  field_types_r[which(is_datetime)] <- "datetime"
  field_types_r[which(is_int)] <- "integer"
  field_types_r[which(is_num)] <- "numeric"
  field_types_r
}
#' @noRd
REDCAP_FACTOR_FIELDS <- c("radio",
                          "yesno",
                          "dropdown",
                          "checkbox_choice",
                          "truefalse")
#' @noRd
REDCAP_LOGICAL_FIELDS <- c("yesno", "checkbox_choice", "truefalse") # tbd
#' @noRd
REDCAP_TEXT_DATE_FIELDS <- c("date_mdy", "date_ymd", "date_dmy")
#' @noRd
REDCAP_TEXT_DATETIME_FIELDS <- c("datetime_dmy", "datetime_seconds_ymd")
#' @noRd
REDCAP_INTEGER_FIELDS <- "integer"
#' @noRd
REDCAP_NUMERIC_FIELDS <- "number"
#' @noRd
REDCAP_MAYBE_IDS_STRICT <- c("email", "phone", "vmrn", "zipcode")
#' @noRd
REDCAP_MAYBE_IDS_SUPER_STRICT <- c("date_dmy",
                                   "date_mdy",
                                   "date_ymd",
                                   "datetime_dmy",
                                   "datetime_mdy",
                                   "datetime_seconds_dmy",
                                   "datetime_seconds_mdy",
                                   "datetime_seconds_ymd",
                                   "datetime_ymd",
                                   "email",
                                   "phone",
                                   "vmrn",
                                   "zipcode")
#' @noRd
FIELD_TYPES_NOT_IN_DATA <- c("descriptive", "checkbox")
#' @noRd
clear_project_datasets <- function(project, dataset_names = NULL) {
  assert_setup_project(project)
  all_dataset_names <- names(project$datasets)
  if (is.null(dataset_names)) {
    dataset_names <- all_dataset_names
  }
  for (dataset_name in dataset_names) {
    project$datasets[[dataset_name]] <- NULL
    project$record_summary[[dataset_name]] <- NULL
  }
  cli_alert_success("Cleared project datasets!")
  invisible(project)
}
#' @noRd
extract_log <- function(data_list, records) {
  redcap_log <- data_list$redcap$log
  redcap_log <- redcap_log[which(!is.na(redcap_log$username)), ]
  redcap_log <- redcap_log[which(!is.na(redcap_log$record)), ]
  # if(drop_exports){
  #   redcap_log <- redcap_log[which(redcap_log$action_type != "Exports" |
  #is.na(redcap_log$action_type)), ]
  # }
  if (!missing(records)) {
    if (!is.null(records)) {
      redcap_log <- redcap_log[which(redcap_log$record %in% records), ]
    }
  }
  redcap_log
}
#' @noRd
annotate_users <- function(data_list,
                           records = NULL,
                           annotate = TRUE,
                           drop_blanks = FALSE) {
  if (is.null(records)) {
    id_col <- data_list$metadata$id_col
    records <- data_list$records[[id_col]]
  }
  redcap_log <- extract_log(data_list, records)
  # role_label not inculded now
  users_df <- data_list$redcap$users |>
    select(c("username", "email", "firstname", "lastname"))
  user_groups <- NULL
  if (is_something(redcap_log)) {
    user_groups <- split(redcap_log, redcap_log$username)
  }
  names_in_log <- names(user_groups)
  if (!is_something(user_groups) || length(names_in_log) == 0L) {
    return(users_df)
  }
  if (annotate) {
    only_in_log <- vec1_not_in_vec2(names_in_log, users_df$username)
    if (length(only_in_log) > 0L) {
      users_df <- users_df |>
        bind_rows(data.frame(username = only_in_log))
    }
    users_df$first_timestamp <- NA
    users_df$last_timestamp <- NA
    users_df$unique_records_n <- NA
    for (user_group_name in names_in_log) {
      the_row <- match(user_group_name, users_df$username)
      group <- user_groups[[user_group_name]]
      users_df$first_timestamp[the_row] <- last(group$timestamp)
      users_df$last_timestamp[the_row] <- group$timestamp[[1L]]
      users_df$unique_records_n[the_row] <- length_unique(group$record)
    }
  }
  if (drop_blanks) {
    keep_rows <- which(users_df$username %in% names_in_log)
    users_df <- users_df[keep_rows, ]
    keep_rows <- drop_nas(match(users_df$username, names_in_log))
    user_groups <- user_groups[keep_rows]
  }
  users_df
}
#' @noRd
get_dataset_records <- function(project, dataset_name) {
  id_col <- project$metadata$id_col
  if (missing(dataset_name)) {
    return(project$record_summary[[id_col]])
  }
  dataset_details <- project$datasets[[dataset_name]]
  data_list <- generate_project_dataset(
    project = project,
    dataset_name = dataset_name,
    transformation_type = dataset_details$transformation_type,
    filter_list = dataset_details$filter_list,
    filter_strict = FALSE,
    form_names = dataset_details$form_names,
    field_names = project$metadata$id_col,
    exclude_identifiers = FALSE,
    exclude_free_text = FALSE,
    clean = FALSE,
    include_metadata = FALSE,
    include_records = FALSE,
    include_users = FALSE,
    include_log = FALSE,
    annotate_from_log = FALSE,
    include_comments = FALSE
  )
  data_list$records[[id_col]]
}
#' @noRd
dataset_records_due <- function(project, dataset_name) {
  dataset_names <- names(project$datasets)
  if (!dataset_name %in% dataset_names) {
    cli_alert_danger("{dataset_name} not included in current dataset_names!")
    return(FALSE)
  }
  dataset_details <- project$datasets[[dataset_name]]
  id_col <- project$metadata$id_col
  if (is.null(dataset_details$last_save_time)) {
    return(TRUE)
  }
  if (!file.exists(dataset_details$file_path) && !dataset_details$separate) {
    # can't do this for separate = TRUE unless more code is written
    return(TRUE)
  }
  old_rec_rows <- which(project$record_summary[[dataset_name]])
  old_records <- project$record_summary[[id_col]][old_rec_rows]
  records <- get_dataset_records(project = project, dataset_name = dataset_name)
  if (!identical(records, old_records)) {
    return(TRUE)
  }
  record_rows <- which(project$record_summary[[id_col]] %in% records)
  record_cols <- c(id_col, "last_api_call", "was_saved", dataset_name)
  relevant_records <- project$record_summary[record_rows, record_cols]
  if (!all(relevant_records$was_saved)) {
    return(TRUE)
  }
  if (any(relevant_records$last_api_call > dataset_details$last_save_time)) {
    return(TRUE)
  }
  FALSE
}
#' @noRd
check_datasets <- function(project, dataset_names) {
  if (missing(dataset_names)) {
    dataset_names <- names(project$datasets)
  }
  needs_refresh <- NULL
  if (is.null(dataset_names)) {
    cli_alert_wrap("No datasets. Use `add_project_dataset()`!")
  }
  # need_to_check <- any(project$record_summary$last_api_call >
  # dataset_details$last_save_time)
  for (dataset_name in dataset_names) {
    test_dataset <- dataset_records_due(project = project,
                                        dataset_name = dataset_name)
    if (test_dataset) {
      needs_refresh <- append(needs_refresh, dataset_name)
    }
  }
  if (is.null(needs_refresh)) {
    cli_alert_wrap("Refresh of datasets not needed!", bullet_type = "v")
  }
  needs_refresh
}
#' @noRd
add_default_datasets <- function(project,
                                 exclude_identifiers = TRUE,
                                 exclude_free_text = TRUE,
                                 date_handling = "none",
                                 use_csv = FALSE) {
  with_links <- FALSE
  if (is_something(project$data)) {
    with_links <- nrow(project$record_summary) <= 3000L
  }
  assert_logical(exclude_identifiers)
  assert_logical(exclude_free_text)
  dataset_name <- "REDCapSync_raw"
  project <- add_project_dataset(
    project = project,
    dataset_name = dataset_name,
    transformation_type = "none",
    filter_list = NULL,
    exclude_identifiers = exclude_identifiers,
    exclude_free_text = exclude_free_text,
    date_handling = "none",
    labelled = FALSE,
    clean = FALSE,
    drop_blanks = FALSE,
    drop_others = NULL,
    include_metadata = TRUE,
    include_records = FALSE,
    include_users = TRUE,
    include_log = FALSE,
    annotate_from_log = FALSE,
    include_comments = TRUE,
    with_links = with_links,
    separate = TRUE,
    use_csv = use_csv,
    dir_other = file.path(project$dir_path, "REDCap", project$project_name),
    file_name = project$project_name
  )
  dataset_name <- "REDCapSync"
  project <- add_project_dataset(
    project = project,
    dataset_name = dataset_name,
    transformation_type = "default",
    filter_list = NULL,
    exclude_identifiers = exclude_identifiers,
    exclude_free_text = exclude_free_text,
    date_handling = "none",
    labelled = TRUE,
    clean = TRUE,
    drop_blanks = FALSE,
    drop_others = NULL,
    include_metadata = TRUE,
    include_records = TRUE,
    include_users = TRUE,
    include_log = FALSE,
    annotate_from_log = TRUE,
    include_comments = TRUE,
    with_links = with_links,
    separate = FALSE,
    use_csv = use_csv,
    dir_other = file.path(project$dir_path, "output"),
    file_name = paste0(project$project_name, "_", dataset_name)
  )
  invisible(project)
}
#' @noRd
DATE_HANDLING_CHOICES <- c(
  "none",
  "exclude_dates",
  "random_shift_by_record",
  "random_shift_by_project",
  "zero_by_record",
  "zero_by_project"
)
#' @noRd
read_dataset_from_file <- function(project, dataset_name, file_path) {
  if (!missing(dataset_name)) {
    if (!missing(file_path)) {
      cli_alert_warning("`file_path` only needed if dataset_name not provided.")
      cli_alert_info("Using `file_path` from `dataset_name`...")
    }
    if (!dataset_name %in% names(project$dataset)) {
      stop("`dataset_name` is not one of `project$dataset`")
    }
    file_path <- project$dataset[[dataset_name]]$file_path
  } else {
    # add data_updates check
    if (!endsWith(file_path, ".xlsx")) {
      stop("File type must be '.xlsx' --> ", file_path)
    }
    if (!file.exists(file_path)) {
      stop("Path does not exist --> ", file_path)
    }
  }
  data_list <- excel_to_list(file_path)
  the_row <- which(data_list$dataset_details$paramater == "raw_form_names")
  form_names <- data_list$dataset_details$value[the_row] |>
    strsplit(" [|] ") |>
    unlist()
  drop_names <- names(data_list) |> setdiff(form_names)
  # use dataset_details to drop non-uploadcompatibles and warn
  for (drop_sheet in drop_names) {
    data_list[[drop_sheet]] <- NULL
  }
  if (length(data_list) == 0L) {
    message("nothing to return")
    return(NULL)
  }
  #check data not already there
  data_list
}
