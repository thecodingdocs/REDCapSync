#' @noRd
fields_to_choices <- function(fields) {
  fields <- fields[which(fields$field_type %in% .redcap_factor_fields), ]
  # fields$field_name[which(fields$field_type=="checkbox_choice")] <-
  # fields$field_name[which(fields$field_type=="checkbox_choice")] |>
  # strsplit("___") |> lapply(function(X){X[[1]]})
  fields <- fields[which(!is.na(fields$select_choices_or_calculations)), ]
  choices <- NULL
  for (i in seq_len(nrow(fields))) {
    field_name <- fields$field_name[i]
    form_name <- fields$form_name[i]
    field_label <- fields$field_label[i]
    field_type <- fields$field_type[i]
    selections <- fields$select_choices_or_calculations[i] |> split_choices()
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
      x[[1]]
    }) |>
    unlist()
  y <- fields$field_label[row_index]
  z <- paste0(fields$field_label[match(x, fields$field_name)], " - ", y)
  fields$field_label[row_index] <- z
  fields
}
#' @noRd
annotate_fields <- function(data_list,
                            summarize_data = TRUE,
                            drop_blanks = FALSE) {
  fields <- data_list$metadata$fields # [,colnames(data_list$metadata$fields)]
  if (drop_blanks) {
    fields <- fields[
      which(fields$field_name %in% get_all_field_names(data_list)), ]
    #fix transform dropping missing fields
  }
  if (nrow(fields) > 0 && is_something(data_list$data)) {
    if (summarize_data) {
      skimmed <- NULL
      for (form_name in drop_nas(unique(fields$form_name))) {
        col_names <- fields$field_name[which(fields$form_name == form_name)]
        form <- data_list$data[[form_name]]
        col_names <- col_names[which(col_names %in% colnames(form))]
        skimmed <- skimmed |> bind_rows(form[, col_names] |> skimr::skim())
      }
      field_names <- fields$field_name
      fields <- fields |> merge(skimmed,
                                 by.x = "field_name",
                                 by.y = "skim_variable",
                                 all = TRUE)
      fields <- field_names |>
        lapply(function(x) {
          fields[which(fields$field_name == x), ]
        }) |>
        bind_rows()
    }
    # cli_alert_wrap("Annotated `data_list$metadata$fields`",bullet_type = "v")
  }
  fields
}
#' @noRd
annotate_forms <- function(data_list,
                           summarize_data = TRUE,
                           drop_blanks = FALSE) {
  forms <- data_list$metadata$forms
  if (drop_blanks) {
    forms <- forms[which(forms$form_name %in% names(data_list$data)), ]
  }
  if (nrow(forms) > 0) {
    # add metadata info like n fields
    if (summarize_data) {
      var_list <- forms$form_name |> lapply(function(form_name) {
        paste0(form_name, "_complete")
      })
      if ("original_form_name" %in% colnames(forms)) {
        var_list <- forms$original_form_name |> strsplit(" [:|:] ") |>
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
                             summarize_data = TRUE,
                             drop_blanks = FALSE) {
  # forms <- data_list$metadata$forms
  # fields <- data_list$metadata$fields
  choices <- data_list$metadata$choices
  if (drop_blanks) {
    choices <- choices[
      which(choices$field_name %in% get_all_field_names(data_list)), ]
  }
  # choices$field_name_raw <- choices$field_name
  # choices$field_name_raw[which(choices$field_type=="checkbox_choice")] <-
  #choices$field_name[which(choices$field_type=="checkbox_choice")] |>
  #   strsplit("___") |>
  #   lapply(function(X){X[[1]]})
  # choices$field_label_raw <- choices$field_label
  # choices$field_label_raw[which(choices$field_type=="checkbox_choice")] <-
  #choices$field_name_raw[which(choices$field_type=="checkbox_choice")] |>
  #   lapply(function(X){
  #     data_list$metadata$fields$field_label[which(fields$field_name==X)] |>
  # unique()
  #   })
  if (summarize_data) {
    choices$n <- seq_len(nrow(choices)) |>
      lapply(function(i) {
        form <- data_list$data[[choices$form_name[i]]]
        if (is.null(form)) {
          return(0)
        }
        if (nrow(form) == 0) {
          return(0)
        }
        the_col <- choices$field_name[i]
        if (!the_col %in% colnames(form)) {
          return(0)
        }
        sum(form[, the_col] == choices$name[i], na.rm = TRUE)
      }) |>
      unlist()
    choices$n_total <- seq_len(nrow(choices)) |>
      lapply(function(i) {
        form <- data_list$data[[choices$form_name[i]]]
        if (is.null(form)) {
          return(0)
        }
        if (nrow(form) == 0) {
          return(0)
        }
        the_col <- choices$field_name[i]
        if (!the_col %in% colnames(form)) {
          return(0)
        }
        sum(!is.na(form[, the_col]), na.rm = TRUE)
      }) |>
      unlist()
    choices$perc <- (choices$n / choices$n_total) |> round(4)
    choices$perc_text <- (choices$perc * 100) |>
      round(1) |>
      paste0("%")
    # cli_alert_wrap("Annotated `data_list$summary$choices`",bullet_type = "v")
  }
  choices
}
#' @noRd
annotate_records <- function(data_list, summarize_data = TRUE) {
  id_col <- data_list$metadata$id_col
  records <- extract_project_records(data_list)[[1]]
  the_rows <- which(data_list$summary$all_records[[id_col]] %in% records)
  all_records <- data_list$summary$all_records[the_rows, ]
  redcap_log <- get_log(data_list = data_list, records = records)
  # redcap_log$date <- redcap_log$timestamp |> strsplit(" ") |>
  #lapply(first) |> unlist()
  if (!is_something(all_records) || !is_something(redcap_log)) {
    return(all_records)
  }
  if (summarize_data) {
    if (is_something(redcap_log)) {
      redcap_log <- redcap_log[, c("timestamp", "username", "record")]
      has_users <- is_something(data_list$redcap$users)
      if (has_users) {
        users <- data_list$redcap$users
        users <- users[, c("username", "firstname", "lastname", "email")]
        redcap_log <- merge(redcap_log, users, by = "username", all.x = TRUE)
      }
      cool_list <- split(redcap_log, redcap_log$record)
      if (length(cool_list) > 1) {
        cool_df <- seq_len(length(cool_list)) |> lapply(function(i) {
          df <- cool_list[[i]]
          the_last <- first(df)
          the_first <- last(df)
          out_df <- data.frame(
            record = the_first$record,
            first_timestamp = the_first$timestamp,
            last_timestamp = the_last$timestamp,
            unique_users = length_unique(df$username)
          )
          if (has_users) {
            out_df$last_username <- the_last$username
            out_df$last_user <- the_last$firstname |> paste(the_last$lastname)
          }
          out_df
        }) |>
          bind_rows()
        colnames(cool_df)[1] <- id_col
        all_records <- merge(all_records, cool_df, by = id_col, all.x = TRUE)
      }
    }
  }
  invisible(all_records)
}
#' @noRd
clean_form <- function(form,
                       fields,
                       drop_blanks = TRUE,
                       drop_others = NULL) {
  for (field_name in colnames(form)) {
    if (field_name %in% fields$field_name) {
      row <- which(fields$field_name == field_name)
      units <- NULL
      if (!is.na(fields$units[row])) {
        units <- fields$units[row]
      }
      class <- fields$field_type_R[row][[1]]
      label <- ifelse(is.na(fields$field_label[row]),
                      field_name,
                      fields$field_label[row])[[1]]
      levels <- NULL
      if (!is.na(class)) {
        if (class == "factor") {
          select_choices <- fields$select_choices_or_calculations[row]
          if (!is.na(select_choices)) {
            levels <- split_choices(select_choices)[[2]]
          } else {
            levels <- unique(form[[field_name]]) |> drop_nas()
          }
          if (anyDuplicated(levels) > 0) {
            duplicate_levels <- levels |>
              duplicated() |>
              which()
            warning(
              "You have a variable (",
              field_name,
              ") with dupplicate names (",
              levels[duplicate_levels] |> toString(),
              "). This is not great but for this proccess they will be merged",
              "and treated as identical responses."
            )
            levels <- levels |> unique()
          }
          if (drop_blanks) {
            levels <- levels[which(levels %in% unique(form[[field_name]]))]
          }
          if (!is.null(drop_others)) {
            if (length(drop_others) > 0)
              levels <- levels[which(!levels %in% drop_others)]
          }
        }
        if (class == "integer") {
          form[[field_name]] <- as.integer(form[[field_name]])
        }
        if (class == "numeric") {
          form[[field_name]] <- as.numeric(form[[field_name]])
        }
        form
      }
      form[[field_name]] <- form[[field_name]] |> clean_column_for_table(
        class = class,
        label = label,
        units = units,
        levels = levels
      )
    }
  }
  form
}
#' @noRd
clean_column_for_table <- function(field, class, label, units, levels) {
  if (!missing(class)) {
    if (!is.null(class)) {
      if (!is.na(class)) {
        if (class == "integer") {
          field <- field |> as.integer()
        }
        if (class == "factor") {
          field <- field |> factor(levels = levels, ordered = TRUE)
        }
        if (class == "numeric") {
          field <- field |> as.numeric()
        }
      }
    }
  }
  if (!missing(label)) {
    if (!is.null(label)) {
      if (!is.na(label)) {
        attr(field, "label") <- label
      }
    }
  }
  if (!missing(units)) {
    if (!is.null(units)) {
      if (!is.na(units)) {
        attr(field, "units") <- units
      }
    }
  }
  field
}
#' @title Add a Subset to a REDCap Database
#' @description
#' Creates a summary of the main REDCap database (`project`) based on specific
#' filter criteria and saves it to a specified directory. The summary can be
#' further customized with additional forms, fields, and deidentification
#' options.
#'
#' @inheritParams save_project
#' @inheritParams setup_project
#' @inheritParams generate_project_summary
#' @param summary_name Character. The name of the summary to create.
#' @param hard_reset Logical. If `TRUE`, overwrite existing summary files with
#' the same name. Default is `FALSE`.
#' @param with_links Optional logical (TRUE/FALSE) for including links in Excel
#' sheets. Default is `FALSE`.
#' @param separate Optional logical (TRUE/FALSE) separating each form into
#' separate files as opposed to multi-tab Excel. Default is `FALSE`.
#' @param dir_other Character. The directory where the summary file will be
#' saved. Default is the `output` folder within the database directory.
#' @param file_name Character. The base name of the file where the summary will
#' be saved. Default is `<project$project_name>_<summary_name>`.
#' @return
#' A modified `project` object that includes the newly created summary.
#' The summary is also saved as a file in the specified directory.
#'
#' @details
#' This function filters the main REDCap database using the specified
#' `filter_field`
#' and `filter_choices`, then creates a new summary with optional
#' deidentification. It can be customized to include only specific forms or
#' fields. The resulting summary is saved to a file for future use.
#'
#' @seealso
#' \code{\link{save_project}} for saving the main database or summaries.
#' @keywords internal
add_project_summary <- function(project,
                                summary_name,
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
                                with_links = TRUE,
                                separate = FALSE,
                                use_csv = FALSE,
                                dir_other = NULL,
                                file_name = NULL,
                                hard_reset = FALSE) {
  # sync_frequency ... project$internals$sync_frequency
  forbiden_summary_names <- c(project$metadata$id_col, .forbiden_summary_names)
  if (summary_name %in% forbiden_summary_names) {
    stop(summary_name,
         " is a forbidden summary name. Used for REDCapSync.")
  }
  if (is.null(dir_other)) {
    dir_other <- file.path(project$dir_path, "output")
  }
  if (is.null(file_name)) {
    file_name <- paste0(project$project_name, "_", summary_name)
  }
  if (is.null(filter_list)) {
    if (!is.null(filter_choices) && !is.null(filter_field)) {
      filter_list <- list(filter_choices)
      names(filter_list) <- filter_field
    } else {
      # warning
    }
  }
  file_ext <- ifelse(use_csv, ".csv", ".xlsx")
  summary_list_new <- list(
    summary_name = summary_name,
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
    with_links = with_links,
    separate = separate,
    use_csv = use_csv,
    dir_other = dir_other,
    file_name = file_name,
    file_path = file.path(dir_other, paste0(file_name, file_ext)),
    n_records = NULL,
    last_save_time = NULL,
    final_form_tab_names = NULL
  )
  summary_list_old <- project$summary[[summary_name]]
  if (!is.null(summary_list_old) && !hard_reset) {
    important_vars <- names(summary_list_new) |>
      vec1_not_in_vec2(.not_important_summary_names)
    are_identical <-
      identical(
        summary_list_new[important_vars], summary_list_old[important_vars])
    if (are_identical) {
      # optional message?
      return(invisible(project))
    }
  }
  project$summary[[summary_name]] <- summary_list_new
  project$summary$all_records[[summary_name]] <- FALSE
  invisible(project)
}
#' @noRd
.forbiden_summary_names <- c("last_api_call",
                             "all_records",
                             "was_saved")
#' @noRd
.not_important_summary_names <-
  c("n_records", "last_save_time", "final_form_tab_names")
#' @noRd
data_list_to_save <- function(data_list,
                              include_metadata,
                              include_users,
                              include_records,
                              include_log) {
  to_save_list <- NULL
  for (form_name in names(data_list$data)) {
    to_save_list[[form_name]] <- data_list$data[[form_name]]
  }
  if (include_metadata) {
    metadata_form_names <- c("forms", "fields", "choices", "missing_codes")
    metadata_names <- data_list$metadata[metadata_form_names] |>
      process_df_list(silent = TRUE) |>
      names()
    metadata_names_alt <- metadata_names
    if (any(metadata_names %in% names(data_list$data))) {
      metadata_names_alt <- paste0("redcap_", metadata_names)
      #check for conflicts
    }
    for (i in seq_len(length(metadata_names))) {
      to_save_list[[metadata_names_alt[i]]] <-
        data_list$metadata[[metadata_names[i]]]
    }
  }
  if (include_users) {
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
  if (include_records) {
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
  if (include_log) {
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
  to_save_list
}
#' @noRd
save_summary <- function(project, summary_name) {
  id_col <- project$metadata$id_col
  summary_list <- project$summary[[summary_name]]
  data_list <- project |>
    generate_project_summary(summary_name = summary_name, internal_use = TRUE)
  # add headers-------
  form_names <- names(data_list$data)
  header_df_list <- construct_header_list(data_list)
  key_cols_list <- get_key_col_list(data_list)
  cols_start <- 4
  if (summary_name == "REDCapSync_raw") {
    cols_start <- 1
    header_df_list <- NULL
  }
  records <- data_list$data[form_names] |>
    lapply(function(form) {
      form[[id_col]]
    }) |>
    unlist() |>
    sort() |>
    unique()
  n_records <- length(records)
  # track form names
  data_list <- data_list_to_save(
    data_list = data_list,
    include_metadata = summary_list$include_metadata,
    include_users = summary_list$include_users,
    include_records = summary_list$include_records,
    include_log = summary_list$include_log
  )
  link_col_list <- list()
  if (summary_list$with_links) {
    if (project$internals$project_type == "redcap") {
      add_links <- which(names(data_list) %in% form_names)
      if (length(add_links) > 0) {
        data_list[add_links] <- data_list[add_links] |>
          lapply(function(form) {
            add_redcap_links_to_form(form, project)
          })
        if (summary_list$include_records) {
          if ("records" %in% names(data_list)) {
            data_list$records <- data_list$records |>
              add_redcap_links_to_form(project)
          }
        }
        #check for conflicting name
        link_col_list <- list("redcap_link")
        names(link_col_list) <- id_col
      }
    }
  }
  # maybe move this up
  # check for conflicts
  # data_list
  # with_links = TRUE
  # separate = FALSE
  # use_csv
  # dir_other = file.path(project$dir_path, "output")
  # file_name = paste0(project$project_name, "_", summary_name)
  # hard_reset = FALSE
  # save -----
  last_save_time <- now_time()
  final_form_tab_names <-
    rename_list_names_excel(list_names = names(data_list))
  names(final_form_tab_names) <- names(data_list)
  last_summary <- now_time()
  summary_details <- project$summary[[summary_name]]
  summary_details$n_records <- n_records
  summary_details$last_save_time <- last_save_time
  summary_details$final_form_tab_names <- final_form_tab_names
  summary_details$raw_form_names <- form_names
  summary_details$cols_start <- cols_start
  data_list$summary_details <- data.frame(
    paramater = names(summary_details),
    value = summary_details |> lapply(function(row) {
      row |> paste0(collapse = " | ")
    }) |>
      unlist() |>
      unname()
  )
  if (summary_list$use_csv) {
    data_list |> list_to_csv(
      dir = summary_list$dir_other,
      file_name = summary_list$file_name,
      overwrite = TRUE
    ) # account for links with CSV like new column
  } else {
    list_to_excel(
      input_list = data_list,
      dir = summary_list$dir_other,
      separate = summary_list$separate,
      link_col_list = link_col_list,
      key_cols_list = key_cols_list,
      # derived_cols_list = derived_cols_list,
      file_name = summary_list$file_name,
      header_df_list = header_df_list,
      overwrite = TRUE
    )
  }
  project$summary[[summary_name]]$n_records <- n_records
  project$summary[[summary_name]]$last_save_time <- last_save_time
  project$summary[[summary_name]]$final_form_tab_names <- final_form_tab_names
  row_match <- which(project$summary$all_records[[id_col]] %in% records)
  project$summary$all_records[[summary_name]][row_match] <- TRUE
  project$summary$all_records$was_saved[row_match] <- TRUE
  project$internals$last_summary <- last_summary
  invisible(project)
}
#' @title Generate a Summary from a Subset Name
#' @description
#' Generates a summary from a REDCap project.
#' The summary can be customized based on various options, such as cleaning the
#' data, including metadata, and annotating metadata.
#'
#' @inheritParams setup_project
#' @inheritParams save_project
#' @param summary_name Character. The name of the summary from which to generate
#' the summary. *If you provide `summary_name` all other parameters are
#' inherited according to what was set with `add_project_summary`.
#' @param transformation_type Character vector.
#' Default is "default".
#' Also have "none", "flat", "merge_non_repeating"
#' "default" first merges non-repeating and if there are repeating forms it
#' merges non-repeating variables to the right of repeating instruments
#' "flat" is one-record, one-row, even if there are repeating forms
#' "none" does not transform anything
#' "merge_non_repeating" still merges all non-repeating instruments but
#' does not merge them to repeating instruments
#' @param merge_form_name A character string representing the name of the merged
#' form. Default is "merged".
#' @param filter_field Character. The name of the field in the database to
#' filter on.
#' @param filter_choices Vector. The values of `filter_field` used to define the
#' summary.
#' @param filter_list Vector. The values of `filter_field` used to define the
#' summary.
#' @param filter_strict Logical. If `TRUE`, all forms will be filtered by
#' criteria. If `FALSE`, will convert original filter to id column and filter
#' all other forms by that record. Default is `TRUE`.
#' @param form_names Character vector. Names of forms to include in the summary.
#' Default is `NULL`, which includes all forms.
#' @param field_names Character vector. Names of specific fields to include in
#' the summary. Default is `NULL`, which includes all fields.
#' @param exclude_identifiers Logical. Whether to exlude identifiers in the data
#' in the summary. Default is `TRUE`.
#' @param exclude_free_text Logical for excluding free text. Default is `FALSE`.
#' @param date_handling character string. One of `none`,`exclude_dates`,
#' `random_shift_by_record`, `random_shift_by_project`, `zero_by_record`, or
#' `zero_by_project` random shift is +/- 90 unless changed with options
#' @param clean Logical. If `TRUE`, the data will be cleaned before summarizing.
#' Default is `TRUE`. If missing codes present AND number or date type, R will
#' convert to those to NA and would make that variable not upload compatible
#' @param drop_blanks Logical. If `TRUE`, records with blank fields will be
#' dropped. Default is `TRUE`.
#' @param drop_missing_codes Logical. If `TRUE`, will convert missing codes to
#' NA. Default is `FALSE`.
#' @param drop_others Character vector of other values that should be dropped.
#' @param include_metadata Logical. If `TRUE`, metadata will be included in the
#' summary. Default is `TRUE`.
#' @param include_users Logical. If `TRUE`, user-related information will be
#' included in the summary. Default is `TRUE`.
#' @param include_records Logical. If `TRUE`, a record summary will be
#' included in the generated summary. Default is `TRUE`.
#' @param include_log Logical. If `TRUE`, the log of changes will be included in
#' the summary. Default is `TRUE`.
#' @param annotate_from_log Logical. If `TRUE`, the metadata, users, and records
#' will be annotated using the log. Default is `TRUE`.
#' @param internal_use A logical flag (`TRUE` or `FALSE`). If `TRUE`, then will
#' return data_list meant for internal use. Defaults to `FALSE`.
#' @return
#' A list containing the generated summary based on the specified options. The
#' list includes filtered and cleaned data, metadata, and other summary details.
#'
#' @details
#' This function allows you to generate a summary of data from a specific
#' summary of records within the REDCap project. The function provides flexible
#' options for cleaning, annotating, and including metadata, as well as
#' controlling whether to include record summaries, user information, and logs.
#' @keywords internal
generate_project_summary <- function(project,
                                     summary_name,
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
                                     internal_use = FALSE) {
  assert_env_name(merge_form_name, max.chars = 31)
  provided_summary_name <- !missing(summary_name)
  if (provided_summary_name) {
    if (!summary_name %in% names(project$summary)) {
      stop(summary_name,
           " is not included in the current project summaries")
    }
    summary_list <- project$summary[[summary_name]]
    # warning about other params?
    transformation_type <- summary_list$transformation_type
    merge_form_name <- summary_list$merge_form_name
    filter_list <- summary_list$filter_list
    filter_strict <- summary_list$filter_strict
    field_names <- summary_list$field_names
    form_names <- summary_list$form_names
    exclude_identifiers <- summary_list$exclude_identifiers
    exclude_free_text <- summary_list$exclude_free_text
    date_handling <- summary_list$date_handling
    labelled <- summary_list$labelled
    clean <- summary_list$clean
    drop_blanks <- summary_list$drop_blanks
    drop_missing_codes <- summary_list$drop_missing_codes
    drop_others <- summary_list$drop_others
    include_metadata <- summary_list$include_metadata
    include_records <- summary_list$include_records
    include_users <- summary_list$include_users
    include_log <- summary_list$include_log
    annotate_from_log <- summary_list$annotate_from_log
  }
  data_list <- NULL
  data_list$metadata <- project$metadata
  if (labelled != project$internals$labelled) {
    if (project$internals$labelled) {
      project <- labelled_to_raw_data_list(project)
    }
    if (!project$internals$labelled) {
      project <- raw_to_labelled_data_list(project)
    }
  }
  data_list$data <- project$data
  data_list <- metadata_add_default_cols(data_list)
  data_list$data <- filter_data_list(
    data_list = data_list,
    field_names = field_names,
    form_names = form_names,
    filter_field = filter_field,
    filter_choices = filter_choices,
    filter_list = filter_list,
    filter_strict = filter_strict
  )
  # if (is_something(project$transformation$field_functions)) {
  #   add_fields <- TRUE
  #   if (provided_summary_name) {
  #     if (summary_name == "REDCapSync_raw") {
  #       add_fields <- FALSE
  #     }
  #   }
  #   if (add_fields) {
  #     data_list <- add_fields_to_data_list(
  # data_list = data_list, transformation = project$transformation)
  #   }
  # }
  data_list$data <- deidentify_data_list(
    data_list = data_list,
    exclude_identifiers = exclude_identifiers,
    exclude_free_text = exclude_free_text,
    date_handling = date_handling
  )
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
  if (transformation_type == "flat") {
    data_list <- flatten_redcap(data_list = data_list)
  }
  if (clean) {
    #include warning for if missing codes will prevent uploads
    if (is_something(data_list$metadata$missing_codes)) {
      if (drop_missing_codes) {
        if (labelled) {
          exclude_these <- data_list$metadata$missing_codes$name
        }else {
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
      drop_blanks = drop_blanks,
      drop_others = drop_others
    )
  }
  if (include_metadata) {
    if (is_something(data_list$metadata)) {
      data_list$metadata$forms <- annotate_forms(
        data_list = data_list,
        summarize_data = TRUE,
        drop_blanks = drop_blanks
      )
      data_list$metadata$fields <- annotate_fields(
        data_list = data_list,
        summarize_data = TRUE,
        drop_blanks = drop_blanks
      )
      data_list$metadata$choices <- annotate_choices(
        data_list = data_list,
        summarize_data = TRUE,
        drop_blanks = drop_blanks
      )
    }
  }
  records <- extract_project_records(data_list)[[1]]
  data_list$redcap <- project$redcap
  data_list$summary$all_records <- project$summary$all_records
  if (include_log) {
    if (!data_list$redcap$has_log_access) {
      cli_alert_warning("You don't have log access so that can't be included.")
      include_log <- FALSE
    } else {
      data_list$log <- get_log(data_list = data_list, records = records)
    }
  }
  if (annotate_from_log && (include_users || include_records)) {
    if (!data_list$redcap$has_log_access) {
      cli_alert_warning("You don't have log access so that data can't be used.")
      annotate_from_log <- FALSE
    }
  }
  if (include_records) {
    if (!is.null(records)) {
      data_list$records <- annotate_records(
        data_list = data_list, summarize_data = annotate_from_log)
    }
  }
  if (include_users) {
    if (!data_list$redcap$has_user_access) {
      cli_alert_warning("You don't have user access that can't be included.")
      include_users <- FALSE
    } else {
      data_list$users <- annotate_users(
        data_list = data_list,
        records = records,
        summarize_data = annotate_from_log,
        drop_blanks = drop_blanks
      )
    }
  }
  data_list$redcap <- NULL
  data_list$summary <- NULL
  if (internal_use) {
    return(invisible(data_list))
  }
  to_save_list <- data_list_to_save(
    data_list = data_list,
    include_metadata = include_metadata,
    include_users = include_users,
    include_records = include_records,
    include_log = include_log
  )
  invisible(to_save_list)
}
#' @noRd
merge_non_repeating <- function(data_list,
                                merge_form_name,
                                merge_to_rep = FALSE) {
  # data_list$metadata$id_col assert
  # data_list$metadata
  # data_list$metadata$forms
  forms_transformation <- data_list$metadata$forms
  is_longitudinal <-
    "repeating_via_events" %in% colnames(data_list$metadata$forms)
  if (is_longitudinal) {
    forms_transformation <- forms_transformation[
      order(forms_transformation$repeating_via_events), ]
  }
  forms_transformation <- forms_transformation[
    order(forms_transformation$repeating), ]
  forms_transformation$form_name_remap <- forms_transformation$form_name
  forms_transformation$form_label_remap <- forms_transformation$form_label
  row_check <- !forms_transformation$repeating
  if (is_longitudinal) {
    row_check <- row_check & !forms_transformation$repeating_via_events
  }
  forms_transformation$form_name_remap[which(row_check)] <- merge_form_name
  merge_form_name_label <- merge_form_name # can captialize here
  if (merge_form_name %in% forms_transformation$form_name) {
    merge_form_name_label <-
      forms_transformation$form_label[
        which(forms_transformation$form_name == merge_form_name)]
  }
  non_rep_form_names <- forms_transformation$form_name[which(row_check)]
  forms_transformation$form_label_remap[which(row_check)] <-
    merge_form_name_label
  forms_transformation_original <- forms_transformation
  cols_to_keep <- c(
    "form_name_remap",
    "form_label_remap",
    "repeating",
    "repeating_via_events",
    "key_cols",
    "key_names",
    ""
  )
  cols_to_keep <- cols_to_keep[
    which(cols_to_keep %in% colnames(forms_transformation))]
  forms_transformation <- forms_transformation[, cols_to_keep] |> unique()
  colnames(forms_transformation)[
    which(colnames(forms_transformation) == "form_name_remap")] <- "form_name"
  colnames(forms_transformation)[
    which(colnames(forms_transformation) == "form_label_remap")] <- "form_label"
  forms_transformation$original_form_name <- forms_transformation$form_name |>
    lapply(function(form_name) {
      forms_transformation_original$form_name[
        which(forms_transformation_original$form_name_remap == form_name)] |>
        paste0(collapse = " | ")
    }) |>
    unlist() |>
    as.character()
  data_list$metadata$forms <- forms_transformation
  # data_list$metadata$fields
  # fields <- combine_project_fields(data_list)
  fields <- data_list$metadata$fields
  fields$original_form_name <- fields$form_name
  fields$form_name <- forms_transformation_original$form_name_remap[
    match(fields$form_name, forms_transformation_original$form_name)]
  fields <- fields[
    order(match(fields$form_name, forms_transformation$form_name)), ]
  # new function RosyUtils
  first <- 1:which(colnames(fields) == "form_name")
  move <- which(colnames(fields) == "original_form_name")
  last <- which(colnames(fields) != "original_form_name")[-first]
  fields <- fields[, c(first, move, last)]
  data_list$metadata$fields <- fields
  # data_list$metadata$choices
  data_list$metadata$choices <- fields_to_choices(fields)
  data_list
  data_list$metadata$form_key_cols <- get_key_col_list(data_list = data_list)
  # data_list$data
  if (is_something(data_list$data)) {
    merge_form <- NULL
    non_rep_form_names <-
      non_rep_form_names[
        non_rep_form_names |>
          lapply(function(non_rep_form_name) {
            is_something(data_list$data[[non_rep_form_name]])
          }) |>
          unlist() |>
          which()]
    # non_rep_form_name <- non_rep_form_names[[2]]
    i <- 0
    for (non_rep_form_name in non_rep_form_names) {
      if (non_rep_form_name == non_rep_form_names[[1]]) {
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
      i <- i + 1
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
flatten_redcap <- function(data_list) {
}
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
  fields$field_type_R <- field_types_to_R(fields)
  # for (x in names(.redcap_field_conversion2)){
  #   y <- .redcap_field_conversion2[[x]]
  #   fields$field_type_R[which(fields$field_type %in% y)] <- x
  # }
  fields$in_original_redcap <- TRUE
  fields$original_form_name <- fields$form_name
  if (!"units" %in% colnames(fields))
    fields$units <- NA
  if (!"field_label_short" %in% colnames(fields))
    fields$field_label_short <- fields$field_label
  data_list$metadata$fields <- fields
  data_list
}
#' @noRd
field_types_to_R <- function(fields) {
  field_types_R <- rep("character", nrow(fields))
  field_types <- fields$field_type
  field_validations <- fields$text_validation_type_or_show_slider_number
  field_types_R[which(field_types %in% .redcap_factor_fields)] <- "factor"
  field_types_R[
    which(field_types == "text" &
            field_validations %in% .redcap_text_date_fields)] <- "date"
  field_types_R[
    which(field_types == "text" &
            field_validations %in% .redcap_text_datetime_fields)] <- "datetime"
  field_types_R[
    which(field_types == "text" &
            field_validations %in% .redcap_integer_fields)] <- "integer"
  field_types_R[
    which(field_types == "text" &
            field_validations %in% .redcap_numeric_fields)] <- "numeric"
  field_types_R
}
#' @noRd
.redcap_factor_fields <-
  c("radio", "yesno", "dropdown", "checkbox_choice", "truefalse")
#' @noRd
.redcap_logical_fields <- c("yesno", "checkbox_choice", "truefalse") # tbd
#' @noRd
.redcap_text_date_fields <- c("date_mdy", "date_ymd", "date_dmy")
#' @noRd
.redcap_text_datetime_fields <- c("datetime_dmy", "datetime_seconds_ymd")
#' @noRd
.redcap_integer_fields <- c("integer")
#' @noRd
.redcap_numeric_fields <- c("number")
#' @noRd
.redcap_possible_id_fields_strict <- c("email",
                                       "phone",
                                       "vmrn",
                                       "zipcode")
#' @noRd
.redcap_possible_id_fields_super_strict <- c("date_dmy",
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
summarize_project <- function(project, hard_reset = FALSE) {
  assert_setup_project(project)
  if (is_something(project$data)) {
    summary_names <- check_summaries(project)
    if (hard_reset) {
      summary_names <- project$summary |> names() |> setdiff("all_records")
    }
    if (is_something(summary_names)) {
      for (summary_name in summary_names) {
        project <- tryCatch(
          expr = project |> save_summary(summary_name),
          error = function(e) {
            cli_alert_warning("Failed to save summary `{summary_name}`.")
            invisible(project)
          })
      }
    }
  }
  invisible(project)
}
#' @title clear_project_summaries
#' @description clears project summaries
#' @inheritParams save_project
#' @return invisible return of project and a message
#' @keywords internal
clear_project_summaries <- function(project, summary_names = NULL) {
  assert_setup_project(project)
  all_summary_names <- project$summary |>
    names() |>
    setdiff("all_records")
  if (is.null(summary_names)) {
    summary_names <- all_summary_names
  }
  # assert(summary_names)
  for (summary_name in summary_names) {
    project$summary[[summary_name]] <- NULL
    project$summary$all_records[[summary_name]] <- NULL
  }
  cli_alert_success("Cleared project summaries!")
  invisible(project)
}
#' @noRd
extract_values_from_form_list <- function(form_list, col_name) {
  names(form_list) |>
    lapply(function(form_name) {
      form_list[[form_name]][[col_name]]
    }) |>
    unlist() |>
    drop_nas() |>
    unique()
}
#' @noRd
extract_project_records <- function(data_list) {
  all_records <- NULL
  id_col <- data_list$metadata$id_col
  if (data_list$data |> is_something()) {
    record_id_col <- extract_values_from_form_list(
      form_list = data_list$data,
      col_name = id_col
    )
    all_records <- data.frame(
      record_id_col = record_id_col,
      last_api_call = NA,
      was_saved = FALSE,
      stringsAsFactors = FALSE
    )
    rownames(all_records) <- NULL
    colnames(all_records)[
      which(colnames(all_records) == "record_id_col")] <- id_col
  }
  all_records
}
#' @noRd
get_log <- function(data_list, records) {
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
                           records,
                           summarize_data = TRUE,
                           drop_blanks = FALSE) {
  redcap_log <- get_log(data_list, records)
  # role_label not inculded now
  summary_users <- data_list$redcap$users |>
    select(c("username", "email", "firstname", "lastname"))
  user_groups <- redcap_log |> split(redcap_log$username)
  names_in_log <- names(user_groups)
  if (!is_something(user_groups) || length(names_in_log) == 0) {
    return(summary_users)
  }
  # maybe dropping people at this step
  if (drop_blanks) {
    summary_users <- summary_users[
      which(summary_users$username %in% names_in_log), ]
    user_groups <- user_groups[
      drop_nas(match(summary_users$username, names_in_log))]
  }
  if (summarize_data) {
    only_in_log <- names_in_log |> vec1_not_in_vec2(summary_users$username)
    if (length(only_in_log) > 0) {
      summary_users <- summary_users |>
        bind_rows(data.frame(username = only_in_log))
    }
    summary_users$first_timestamp <- NA
    summary_users$last_timestamp <- NA
    # summary_users$last_record_timestamp <- NA
    summary_users$unique_records_n <- NA
    for (user_group_name in names_in_log) {
      the_row <- match(user_group_name, summary_users$username)
      group <- user_groups[[user_group_name]]
      # record_rows <- which(!is.na(group$record))
      summary_users$first_timestamp[the_row] <- group$timestamp |> last()
      summary_users$last_timestamp[the_row] <- group$timestamp[[1]]
      # if(length(record_rows)>0){
      #   summary_users$last_record_timestamp[the_row] <-
      #group$timestamp[record_rows][[1]]
      # }
      summary_users$unique_records_n[the_row] <- length_unique(group$record)
    }
  }
  # summary_users$timestamp_diff_days<-
  #(as.Date(summary_users$last_timestamp) -
  #as.Date(summary_users$first_timestamp)+1) |> as.integer()
  # summary_users$records_per_day <-
  #as.integer(summary_users$unique_records_n/summary_users$timestamp_diff_days)
  summary_users
}
#' @noRd
get_summary_records <- function(project, summary_name) {
  id_col <- project$metadata$id_col
  if (missing(summary_name)) {
    return(project$summary$all_records[[id_col]])
  }
  summary_list <- project$summary[[summary_name]]
  data_list <- generate_project_summary(
    project = project,
    transformation_type = summary_list$transformation_type,
    filter_list = summary_list$filter_list,
    filter_strict = FALSE,
    form_names = summary_list$form_names,
    field_names = project$metadata$id_col,
    exclude_identifiers = FALSE,
    exclude_free_text = FALSE,
    clean = FALSE,
    include_metadata = FALSE,
    include_records = FALSE,
    include_users = FALSE,
    include_log = FALSE,
    annotate_from_log = FALSE,
    internal_use = TRUE
  )
  records <- data_list$data |>
    lapply(function(form) {
      form[[id_col]]
    }) |>
    unlist() |>
    sort() |>
    unique()
  record_rows <- which(project$summary$all_records[[id_col]] %in% records)
  summary_records <- project$summary$all_records[[id_col]][record_rows] |>
    sort() |>
    unique()
  summary_records
}
#' @noRd
summary_records_due <- function(project, summary_name) {
  summary_list <- project$summary[[summary_name]]
  id_col <- project$metadata$id_col
  if (is.null(summary_list$last_save_time)) {
    return(TRUE)
  }
  if (!file.exists(summary_list$file_path) &&
      !summary_list$separate) {
    # can't do this for separate = TRUE unless more code is written
    return(TRUE)
  }
  old_rec_rows <- which(project$summary$all_records[[summary_name]])
  old_records <- project$summary$all_records[[id_col]][old_rec_rows] |> sort()
  records <- get_summary_records(project = project,
                                 summary_name = summary_name) |> sort()
  if (!identical(records, old_records)) {
    return(TRUE)
  }
  record_rows <- which(project$summary$all_records[[id_col]] %in% records)
  record_cols <- c(id_col, "last_api_call", "was_saved", summary_name)
  relevant_records <- project$summary$all_records[record_rows, record_cols]
  if (length(which(!relevant_records$was_saved)) > 0) {
    return(TRUE) # maybe was_saved not needed at all
  }
  if (any(relevant_records$last_api_call > summary_list$last_save_time)) {
    return(TRUE)
  }
  FALSE
}
#' @noRd
check_summaries <- function(project, summary_names) {
  if (missing(summary_names)) {
    summary_names <- project$summary |> names() |> setdiff("all_records")
  }
  needs_refresh <- NULL
  if (is.null(summary_names)) {
    cli_alert_wrap("No summaries. Use `add_project_summary()`!")
  }
  # need_to_check <- any(project$summary$all_records$last_api_call >
  # summary_list$last_save_time)
  for (summary_name in summary_names) {
    test_summary <-
      summary_records_due(project = project, summary_name = summary_name)
    if (test_summary) {
      needs_refresh <- needs_refresh |> append(summary_name)
    }
  }
  if (is.null(needs_refresh)) {
    cli_alert_wrap("Refresh of summaries not needed!", bullet_type = "v")
  }
  needs_refresh
}
#' @noRd
add_default_summaries <- function(project,
                                  exclude_identifiers = TRUE,
                                  exclude_free_text = TRUE,
                                  date_handling = "none",
                                  use_csv = FALSE) {
  with_links <- FALSE
  if (is_something(project$data)) {
    with_links <- nrow(project$summary$all_records) <= 3000
  }
  assert_logical(exclude_identifiers)
  assert_logical(exclude_free_text)
  summary_name <- "REDCapSync_raw"
  project <- add_project_summary(
    project = project,
    summary_name = summary_name,
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
    with_links = with_links,
    separate = TRUE,
    use_csv = use_csv,
    dir_other = file.path(project$dir_path, "REDCap", project$project_name),
    file_name = project$project_name
  )
  summary_name <- "REDCapSync"
  project <- add_project_summary(
    project = project,
    summary_name = summary_name,
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
    with_links = with_links,
    separate = FALSE,
    use_csv = use_csv,
    dir_other = file.path(project$dir_path, "output"),
    file_name = paste0(project$project_name, "_", summary_name)
  )
  invisible(project)
}
#' @title Clean to Raw REDCap forms
#' @inheritParams save_project
#' @param form data.frame of labelled REDCap to be converted to raw REDCap
#' (for uploads)
#' @return project object that has been filtered to only include the specified
#' records
#' @export
labelled_to_raw_form <- function(form, project) {
  form <- all_character_cols(form)
  use_missing_codes <- is.data.frame(project$metadata$missing_codes)
  fields <- filter_fields_from_form(form = form, project = project)
  for (i in seq_len(nrow(fields))) {
    # i <-  seq_len(nrow(fields) |> sample(1)
    field_name <- fields$field_name[i]
    has_choices <- fields$has_choices[i]
    if (has_choices) {
      z <- fields$select_choices_or_calculations[i] |> split_choices()
      form[[field_name]] <- form[[field_name]] |>
        lapply(function(x) {
          form <- NA
          if (!is.na(x)) {
            coded_redcap <- which(z$name == x)
            if (length(coded_redcap) > 0) {
              form <- z$code[coded_redcap]
            } else {
              if (use_missing_codes) {
                coded_redcap2 <- which(project$metadata$missing_codes$name == x)
                if (length(coded_redcap2) > 0) {
                  form <- project$metadata$missing_codes$code[coded_redcap2]
                } else {
                  stop(
                    "Mismatch in choices compared to REDCap (above)! Column: ",
                    field_name,
                    ", Choice: ",
                    x
                  )
                }
              } else {
                stop(
                  "Mismatch in choices compared to REDCap (above)! Column: ",
                  field_name,
                  ", Choice: ",
                  x,
                  ". Also not a missing code."
                )
              }
            }
          }
          form
        }) |>
        unlist() |>
        as.character()
    } else {
      if (use_missing_codes) {
        form[[field_name]] <- form[[field_name]] |>
          lapply(function(x) {
            field <- x
            if (!is.na(x)) {
              code_row <- which(project$metadata$missing_codes$name == x)
              if (length(code_row) > 0) {
                field <- project$metadata$missing_codes$code[code_row]
              }
            }
            field
          }) |>
          unlist() |>
          as.character()
      }
    }
  }
  form
}
#' @title Raw to Labelled REDCap forms
#' @param form data.frame of raw REDCap to be converted to labelled REDCap
#' @inheritParams save_project
#' @return project object
#' @export
raw_to_labelled_form <- function(form, project) {
  if (project$metadata$has_coding_conflicts) {
    stop(
      "You cannot use labelled = 'TRUE' because you have a coding conflict ",
      "in your data dictionary... Try `setup_project` with labelled = 'FALSE'.",
      "The conflicts are from: ",
      project$metadata$coding_conflict_field_names |> paste0(collapse = ", ")
    )
  }
  if (nrow(form) > 0) {
    use_missing_codes <- is.data.frame(project$metadata$missing_codes)
    fields <- filter_fields_from_form(form = form, project = project)
    for (i in seq_len(nrow(fields))) {
      # i <-  seq_len(nrow(fields)) |> sample(1)
      field_name <- fields$field_name[i]
      has_choices <- fields$has_choices[i]
      if (has_choices) {
        z <- fields$select_choices_or_calculations[i] |> split_choices()
        form[[field_name]] <- form[[field_name]] |>
          lapply(function(x) {
            form <- NA
            if (!is.na(x)) {
              coded_redcap <- which(z$code == x)
              if (length(coded_redcap) > 0) {
                form <- z$name[coded_redcap]
              } else {
                if (use_missing_codes) {
                  coded_redcap2 <-
                    which(project$metadata$missing_codes$code == x)
                  if (length(coded_redcap2) > 0) {
                    form <- project$metadata$missing_codes$name[coded_redcap2]
                  } else {
                    warning(
                      "Mismatch in choices compared to REDCap! Column: ",
                      field_name,
                      ", Choice: ",
                      x,
                      ". Also not a missing code."
                    )
                  }
                } else {
                  warning(
                    "Mismatch in choices compared to REDCap! Column: ",
                    field_name,
                    ", Choice: ",
                    x
                  )
                }
              }
            }
            form
          }) |>
          unlist() |>
          as.character()
      } else {
        if (use_missing_codes) {
          z <- project$metadata$missing_codes
          form[[field_name]] <- form[[field_name]] |>
            lapply(function(x) {
              field <- x
              if (!is.na(x)) {
                code_row <- which(z$code == x)
                if (length(code_row) > 0) {
                  field <- z$name[code_row]
                }
              }
              field
            }) |>
            unlist() |>
            as.character()
        }
      }
    }
  }
  form
}
#' @noRd
labelled_to_raw_data_list <- function(project) {
  project <- assert_blank_project(project)
  if (!project$internals$labelled) {
    stop("project is already raw/coded (not labelled values)")
  }
  for (form_name in names(project$data)) {
    project$data[[form_name]] <- project$data[[form_name]] |>
      labelled_to_raw_form(project = project)
  }
  project$internals$labelled <- FALSE
  invisible(project)
}
#' @noRd
raw_to_labelled_data_list <- function(project) {
  project <- assert_blank_project(project)
  if (project$internals$labelled) {
    stop("project is already labelled (not raw/coded values)")
  }
  if (project$metadata$has_coding_conflicts) {
    stop("project has codebook conflict(s), so will not convert to labelled!")
  }
  for (form_name in names(project$data)) {
    project$data[[form_name]] <- project$data[[form_name]] |>
      raw_to_labelled_form(project = project)
  }
  project$internals$labelled <- TRUE
  invisible(project)
}
#' @noRd
get_all_field_names <- function(data_list) {
  data_list$data |>
    lapply(colnames) |>
    unlist() |>
    unique()
}
#' @noRd
get_identifier_fields <- function(data_list,
                                  get_type = "deidentified",
                                  invert = FALSE) {
  assert_choice(get_type, choices = setdiff(.get_type, "identified"))
  fields <- data_list$metadata$fields
  all_fields <- fields$field_name
  if (get_type == "deidentified") {
    id_fields <- fields$field_name[which(fields$identifier == "y")]
  }
  if (get_type == "deidentified_strict") {
    id_fields <- fields$field_name[which(
      fields$identifier == "y" |
        fields$text_validation_type_or_show_slider_number %in%
        .redcap_possible_id_fields_strict
      )]
  }
  if (get_type == "deidentified_super_strict") {
    id_fields <- fields$field_name[which(
      fields$identifier == "y" |
        fields$text_validation_type_or_show_slider_number %in%
        .redcap_possible_id_fields_super_strict
    )]
  }
  final_output <- id_fields
  if (invert) {
    final_output <- setdiff(all_fields, id_fields)
  }
  final_output
}
#' @noRd
field_names_to_form_names <- function(project,
                                      field_names,
                                      transform = FALSE,
                                      strict = FALSE) {
  metadata <- project$metadata
  if (transform) {
    metadata <- project$transformation$metadata
  }
  fields <- metadata$fields
  form_key_cols <- metadata$form_key_cols |>
    unlist() |>
    unique()
  field_names_keys <- field_names[which(field_names %in% form_key_cols)]
  form_names_keys <- field_names_keys |>
    lapply(function(field_name) {
      metadata$form_key_cols |>
        names() |>
        lapply(function(form_name) {
          if (!field_name %in% metadata$form_key_cols[[form_name]]) {
            return(NULL)
          }
          form_name
        }) |>
        unlist()
    }) |>
    unlist() |>
    as.character() |>
    unique()
  field_names_not_keys <-
    field_names[which(!field_names %in% form_key_cols)] |> unique()
  form_names_not_keys <-
    fields$form_name[match(field_names_not_keys, fields$field_name)] |>
    drop_nas() |>
    unique()
  form_names <- form_names_not_keys
  if (!strict) {
    form_names <- form_names |>
      append(form_names_keys) |>
      unique()
  }
  form_names
}
#' @noRd
construct_header_list <- function(data_list,
                                  md_elements = c("form_name",
                                                  "field_type",
                                                  "field_label")) {
  fields <- data_list$metadata$fields
  if (anyDuplicated(fields$field_name) > 0)
    stop("dup names not allowed in fields")
  data_field_list <- data_list$data |> lapply(colnames)
  header_df_list <- data_field_list |> lapply(function(field_names) {
    x <- field_names |>
      lapply(function(field_name) {
        row <- which(fields$field_name == field_name)
        if (length(row) > 0) {
          return(as.character(fields[md_elements][row, ]))
        }
        rep("", length(md_elements))
      }) |>
      as.data.frame()
    colnames(x) <- field_names
    x <- x[which(apply(x, 1, function(row) {
      any(row != "")
    })), ]
    x
  }) |>
    process_df_list(silent = TRUE)
  header_df_list
}
#' @noRd
field_names_metadata <- function(project, field_names, col_names) {
  fields <- project$metadata$fields # project$metadata$fields
  # if(!deparse(substitute(form_name))%in%project$metadata$forms$form_name)
  # stop("To avoid potential issues the form name
  # should match one of the instrument names" )
  bad_field_names <- field_names[which(
    !field_names %in% c(
      project$metadata$fields$field_name,
      project$metadata$raw_structure_cols,
      "arm_number",
      "event_name"
    )
  )]
  if (length(bad_field_names) > 0)
    stop(
      "All column names in your form must match items in your metadata, ",
      "`project$metadata$fields$field_name`... ",
      toString(bad_field_names)
    )
  # metadata <- project$metadata$fields[
  #which(project$metadata$fields$form_name%in%instruments),]
  fields <- fields[which(fields$field_name %in% field_names), ]
  # metadata <- metadata[which(metadata$field_name%in%field_names),]
  if (!missing(col_names)) {
    if (is_something(col_names))
      fields <- fields[[col_names]]
  }
  fields
}
#' @noRd
filter_fields_from_form <- function(form, project) {
  forms <- project |> field_names_to_form_names(field_names = colnames(form))
  if (any(forms %in% project$metadata$forms$repeating)) {
    stop(
      "All column names in your form must match only one form in your",
      "metadata, `project$metadata$forms$form_name`, unless they are",
      " all non-repeating"
    )
  }
  fields <- project |> field_names_metadata(field_names = colnames(form))
  fields <- fields[which(fields$field_type != "descriptive"), ]
  fields$has_choices <- !is.na(fields$select_choices_or_calculations)
  fields$has_choices[
    which(fields$field_type %in% c("calc", "text", "slider"))] <- FALSE
  fields
}
