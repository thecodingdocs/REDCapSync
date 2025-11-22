#' @noRd
deidentify_data_list <- function(data_list,
                                 exclude_identifiers = TRUE,
                                 exclude_free_text = FALSE,
                                 exclude_additional = NULL,
                                 date_handling = "none") {
  # assert_data_list contains data and metadata with forms and fields
  assert_choice(date_handling, choices = .date_handling_choices)
  assert_logical(exclude_identifiers)
  assert_logical(exclude_free_text)
  data <- data_list$data
  metadata <- data_list$metadata
  fields <- metadata$fields
  exclusions <- exclude_additional
  if (exclude_identifiers) {
    initial_identifiers <- fields$field_name[which(
      fields$identifier == "y" |
        fields$text_validation_type_or_show_slider_number %in%
        .redcap_possible_id_fields_strict
    )]
    if (length(initial_identifiers) == 0L) {
      warning(
        "You have no identifiers marked in ",
        "`project$metadata$fields$identifier`. ",
        "You can set it in REDCap Project Setup and update ",
        "project OR define your idenitifiers in this functions ",
        "`identifiers` argument.",
        immediate. = TRUE
      )
    }
    exclusions <- exclusions %>%
      append(initial_identifiers) %>%
      unique()
  }
  bad_identifiers <- exclusions[which(!exclusions %in% fields$field_name)]
  if (length(bad_identifiers) > 0L) {
    stop("There is a bad identifier. see `fields$field_name`: ",
         toString(bad_identifiers))
  }
  id_cols <- metadata$form_key_cols %>%
    unlist() %>%
    unique()
  if (is_something(id_cols)) {
    if (any(id_cols %in% exclusions)) {
      stop("ID cols not allowed... ",
           toString(id_cols),
           " <-- use hashing (in dev)")
    }
  }
  if (exclude_free_text) {
    # placeholder
    # drop free text only if there is no validation
    # make function for that ?external
    free_text_rows <- which(
      fields$field_type == "notes" |
        (
          fields$field_type == "text" &
            is.na(fields$text_validation_type_or_show_slider_number)
        ) &
        !fields$field_name %in% id_cols &
        fields$in_original_redcap
    )
    free_text_fields <- fields$field_name[free_text_rows]
    exclusions <- exclusions %>%
      append(free_text_fields) %>%
      unique()
  }
  if (is_something(data)) {
    date_vector <- fields$field_name[which(fields$field_type_R == "date")]
    # records <- lapply(data,function(form){
    #   form[[id_cols[1]]]
    # }) %>% unlist() %>% unique()
    date_list <- Map(
      f = function(x, col_names) {
        date_vector[which(date_vector %in% col_names)]
      },
      names(data),
      lapply(data, colnames)
    )
    if (date_handling != "none") {
      if (date_handling == "exclude_dates") {
        exclusions <- exclusions %>%
          append(date_vector) %>%
          unique()
      }
      if (date_handling %in% c("random_shift_by_record",
                               "random_shift_by_project",
                               "zero_by_record",
                               "zero_by_project")) {
        number <- 90 # can set in options
        shift_range <- -number:number
        min_dates <- get_min_dates(data_list)
        if (date_handling == "random_shift_by_record") {
          min_dates$shift_amount <- sample(shift_range,
                                           size = nrow(min_dates),
                                           replace = TRUE)
        }
        if (date_handling == "random_shift_by_project") {
          min_dates$shift_amount <- sample(shift_range,size = 1,replace = TRUE)
        }
        if (date_handling == "zero_by_record") {
          # should you edit fields to now be field_type_R integer?
          min_dates$shift_amount <- min_dates$date
        }
        if (date_handling == "zero_by_project") {
          # should you edit fields to now be field_type_R integer?
          min_dates$shift_amount <- min_dates$date %>% min()
        }
        for (form_name in names(date_list)) {
          field_record <- data[[form_name]][[id_cols[1]]]
          match_date_diff <- match(field_record, min_dates$record_id)
          difference <- min_dates$shift_amount[match_date_diff]
          for (field_name in date_list[[form_name]]) {
            field <- data[[form_name]][[field_name]]
            data[[form_name]][[field_name]] <-
              as.character(as.Date(field) - difference)
          }
        }
      }
      # if you have dates you already mutated no need to drop anymore
      # exclusions <- exclusions[which(!exclusions %in% date_vector)]
    }
    drop_list <- Map(function(x, col_names) {
      exclusions[which(exclusions %in% col_names)]
    }, names(data), lapply(data, colnames))
    drop_list <- drop_list[unlist(lapply(drop_list, length)) > 0L]
    for (form_name in names(drop_list)) {
      for (field_name in drop_list[[form_name]]) {
        data[[form_name]][[field_name]] <- NULL
      }
    }
  }
  invisible(data)
}
#' @noRd
filter_data_list <- function(data_list,
                             field_names = NULL,
                             form_names = NULL,
                             filter_field = NULL,
                             filter_choices = NULL,
                             filter_list = NULL,
                             filter_strict = TRUE) {
  if (is.null(field_names))
    field_names <- data_list %>% get_all_field_names()
  if (is.null(form_names))
    form_names <- data_list$metadata$forms$form_name
  the_rows <- which(!field_names %in% data_list$metadata$raw_structure_cols)
  field_names_minus <- field_names[the_rows]
  if (length(field_names_minus) > 0) {
    form_names_minus <- data_list %>%
      field_names_to_form_names(field_names = field_names_minus, strict = TRUE)
    form_names <- form_names %>% vec1_in_vec2(form_names_minus)
  }
  missing_filter <- is.null(filter_list) &&
    is.null(filter_choices) &&
    is.null(filter_field)
  out_list <- list()
  if (is.null(filter_list)) {
    if (!is.null(filter_field) && !is.null(filter_choices)) {
      filter_list <- list(filter_choices)
      names(filter_list) <- filter_field
    }
  } else {
    if (!is.null(filter_field) || !is.null(filter_choices)) {
      cli_alert_warning(
        "use `filter_list` or `filter_field` & `filter_choices`")
    }
  }
  filter_field_names <- NULL
  if (!is.null(filter_list)) {
    filter_field_names <- filter_list %>%
      names() %>%
      drop_if("")
    # should be unique
    # filter_field_names %>%
    # vec1_not_in_vec2(data_list$metadata$fields$field_name) # should be empty
    filter_form <- data_list %>%
      field_names_to_form_names(field_names = filter_field_names)
    if (length(filter_field_names) == 1L) {
      if (filter_field_names == data_list$metadata$id_col) {
        filter_form <- data_list$metadata$forms$form_name[1L]
        # RISKY? id_position like REDCapR, add to setup
      }
    }
    # should be length 1
    if (length(filter_form) > 1) {
      stop("You can only filter_list by multiple columns part of one form")
    }
    form_key_cols <- data_list$metadata$form_key_cols %>%
      unlist() %>%
      unique()
    is_key <- all(filter_field_names %in% form_key_cols)
    is_repeating_filter_form <- filter_form %in%
      data_list$metadata$forms$form_name[which(data_list$metadata$forms$repeating)]
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
                filter_choices_final <- data_list$data[[filter_form]][[filter_field_final]][which(data_list$data[[filter_form]][[filter_field_name]] %in% filter_choices_final)] %>% unique()
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
          op_index <- (field_index - 1)
          if (op_index <= length(filter_list)) {
            if (field_index != 1) {
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
      if (length(row_index) > 0 && length(col_names) > 0) {
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
                            drop_blanks = TRUE,
                            drop_others = NULL) {
  # assert data list
  data <- data_list$data
  metadata <- data_list$metadata
  for (form_name in names(data)) {
    data[[form_name]] <- clean_form(
      form = data[[form_name]],
      fields = metadata$fields,
      drop_blanks = drop_blanks,
      drop_others = drop_others
    )
  }
  invisible(data)
}
#' @noRd
get_min_dates <- function(data_list) {
  # assert_data_list contains data and metadata with forms and fields
  data <- data_list$data
  metadata <- data_list$metadata
  fields <- metadata$fields
  id_cols <- metadata$form_key_cols %>%
    unlist() %>%
    unique()
  empty <- data.frame(
    record_id = character(),
    min_date = as.Date(character()),
    stringsAsFactors = FALSE
  )
  if (!is_something(data)) {
    return(empty)
  }
  date_vector <- fields$field_name[which(fields$field_type_R == "date")]
  # records <- lapply(data,function(form){
  #   form[[id_cols[1]]]
  # }) %>% unlist() %>% unique()
  all_dates <- list()
  # Loop through each form in the list
  for (form in data) {
    if (id_cols[1] %in% names(form)) {
      # Find the date fields that exist in this form
      existing_fields <- intersect(names(form), date_vector)
      if (length(existing_fields) > 0L) {
        df_subset <- form[, c(id_cols[1], existing_fields), drop = FALSE]
        df_long <- stats::reshape(
          df_subset,
          varying = existing_fields,
          v.names = "date",
          times = existing_fields,
          timevar = "field",
          direction = "long"
        )
        df_long$date <- as.Date(df_long$date, format = "%Y-%m-%d")
        all_dates <- c(all_dates, list(df_long))
      }
    }
  }
  if (length(all_dates) == 0) {
    return(empty)
  }
  combined <- do.call(rbind, all_dates)
  combined <- combined[!is.na(combined$date), ]
  min_dates <- stats::aggregate(date ~ record_id, data = combined, FUN = min)
  min_dates
}
#' @rdname Links
#' @title Open Links to REDCap Pages
#' @description
#' Opens browser page for a given project object.
#' @details
#' Uses [utils::browseURL()] to open the specified REDCap page.
#' In order for the function to work you must have ran
#' \code{project <- sync_project(project)} successfully at least once.
#' If the link brings you to a page that doesn't work check the URL. It's
#' possible your institution may have changed redcap versions, which is part of
#' the URL. In that case run \code{project <- sync_project(project)} again.
#' You may have to be signed into REDCap for it to work properly.
#' When in doubt, just seek out the page by navigating on your own in REDCap.
#' Report issues if you can.
#' @param project A validated `project` object containing REDCap project data
#' and settings. Generated using \code{project <- \link{load_project}("PROJ")}
#' or \link{setup_project}()
#' @param link_type choose one of "base", "home", "record_home",
#' "records_dashboard", "api", "api_playground", "codebook", "user_rights",
#' "setup", "logging", "designer", "dictionary", "data_quality", "identifiers"
#' @param open_browser logical for launching the link in internet browser
#' @return internet link
#' @keywords internal
get_project_url <- function(project,
                            link_type = "home",
                            open_browser = TRUE) {
  assert_choice(
    link_type,
    .link_types
  )
  the_link <- project$links[[paste0("redcap_",link_type)]]
  if(open_browser){
    utils::browseURL(the_link)
    return(invisible())
  }
  the_link
}
#' @param record REDCap record id or study id etc, any column names that match
#' `project$metadata$id_col`
#' @param page REDCap page for the record. Must be one of
#' `project$metadata$forms$form_name`
#' @param instance REDCap instance if it's a repeating instrument
#' @param open_browser logical for launching the link in internet browser
#' @rdname Links
#' @keywords internal
get_record_url <- function(project,
                           record,
                           page,
                           instance,
                           open_browser = TRUE) {
  # FIX
  link <- paste0(
    project$links$redcap_base,
    "redcap_v",
    project$redcap$version,
    "/DataEntry/record_home.php?pid=",
    project$redcap$project_id
  )
  id_col <- project$metadata$id_col
  if (!missing(record)) {
    if (!record %in% project$summary$all_records[[id_col]]) {
      stop(record, " is not one of the records inside project")
    }
    if ("arm_number" %in% colnames(project$summary$all_records)) {
      arm_row <- which(project$summary$all_records[[id_col]] == record)
      link <- paste0(link,
                     "&arm=",
                     project$summary$all_records$arm_number[arm_row])
    }
    link <- link %>% paste0("&id=", record)
  }
  if (!missing(page)) {
    link <- gsub("record_home", "index", link)
    if (!page %in% project$metadata$forms$form_name) {
      stop(
        page,
        " has to be one of the instrument names: ",
        toString(project$metadata$forms$form_name)
      )
    }
    link <- link %>% paste0("&page=", page)
    if (!missing(instance)) {
      repeating_form_names <- project$metadata$forms$form_name[
        which(project$metadata$forms$repeating)]
      if (!page %in% repeating_form_names) {
        stop(
          "If you provide an instance, it has to be a repeating instrument: ",
          repeating_form_names %>% toString()
        )
      }
      link <- link %>% paste0("&instance=", instance)
    }
  }
  if (open_browser) {
    utils::browseURL(link)
    return(invisible())
  }
  link
}
#' @noRd
# construct_key_col_list <- function(project) {
#   form_list <- project$data
#   data_field_list <- form_list %>% lapply(colnames)
#   form_names <- names(form_list)
#   key_cols_list <- form_names %>% lapply(function(form_name) {
#     key_cols <- which(
#       data_field_list[[form_name]] %in% project$metadata$raw_structure_cols
#     )
#     data_field_list[[form_name]][key_cols]
#   })
#   names(key_cols_list) <- form_names
#   key_cols_list
# }
#' @noRd
get_key_col_list <- function(data_list) {
  forms <- data_list$metadata$forms
  if (!is_something(forms)) {
    stop("Empty --> `project$metadata$forms`")
  }
  out_list <- seq_len(nrow(forms)) %>% lapply(function(i) {
    out <- data_list$metadata$id_col
    if (data_list$metadata$is_longitudinal)
      out <- append(out, "redcap_event_name")
    if (forms$repeating[i]) {
      out <- append(out, "redcap_repeat_instrument")
      out <- append(out, "redcap_repeat_instance")
    }
    out
  })
  names(out_list) <- forms$form_name
  out_list
}
#' @noRd
normalize_redcap <- function(denormalized, project, labelled) {
  forms <- project$metadata$forms
  fields <- project$metadata$fields
  events <- project$metadata$events
  event_mapping <- project$metadata$event_mapping
  form_list <- list()
  if (nrow(denormalized) > 0) {
    is_longitudinal <- project$metadata$is_longitudinal
    denormalized <- denormalized %>% all_character_cols()
    add_ons <- c(
      project$metadata$id_col,
      "arm_number",
      "event_name",
      "redcap_event_name",
      "redcap_repeat_instrument",
      "redcap_repeat_instance"
    )
    if (is_longitudinal) {
      denormalized$id_temp <- seq_len(nrow(denormalized))
      denormalized <- merge(
        denormalized,
        events[, c("arm_number", "event_name", "unique_event_name")],
        by.x = "redcap_event_name",
        by.y = "unique_event_name",
        sort = FALSE,
        all.x = TRUE
      )
      add_ons <- add_ons[which(add_ons %in% colnames(denormalized))]
      col_names <- c(add_ons, colnames(denormalized)) %>% unique()
      denormalized <- denormalized[order(denormalized$id_temp), col_names %>%
                                     lapply(function(c) {
                                       which(colnames(denormalized) == c)
                                     }) %>%
                                     unlist() %>%
                                     as.integer()]
      denormalized$id_temp <- NULL
    }
    add_ons <- add_ons[which(add_ons %in% colnames(denormalized))]
    if (!all(project$metadata$raw_structure_cols %in% colnames(denormalized))) {
      stop(
        "denormalized is missing one of the following... and that's weird: ",
        toString(project$metadata$raw_structure_cols)
      )
    }
    form_rows <- which(forms$form_name %in% unique(fields$form_name))
    form_names <- forms$form_name[form_rows]
    has_repeating_forms <- project$metadata$has_repeating_forms
    repeating_forms <- forms$form_name[which(forms$repeating)]
    for (form_name in form_names) {
      form_field_names <- fields$field_name[which(
        fields$form_name == form_name &
          fields$field_name %in% colnames(denormalized) &
          fields$field_name != project$metadata$id_col
      )]
      if (length(form_field_names) == 0) {
        cli_alert_danger(paste0(
          "You might not have access to ",
          form_name,
          ". Unable to obtain."
        ))
      }
      # consider message for what variables you are missing with vec1_not_in_vec2
      row_index <- NULL
      if (length(form_field_names) > 0) {
        add_ons_x <- add_ons
        is_repeating_form <- form_name %in% repeating_forms
        row_index <- seq_len(nrow(denormalized))
        if (is_repeating_form) {
          if (!"redcap_repeat_instrument" %in% colnames(denormalized)) {
            stop("redcap_repeat_instrument not in colnames(denormalized)")
          }
          if (is_longitudinal) {
            row_index <- which(
              denormalized$redcap_repeat_instrument == form_name |
                denormalized$redcap_event_name %in%
                event_mapping$unique_event_name[which(!event_mapping$repeating &
                                                        event_mapping$form == form_name)]
            )
          }
          if (!is_longitudinal) {
            row_index <- which(denormalized$redcap_repeat_instrument == form_name)
          }
        }
        if (!is_repeating_form) {
          add_ons_x <- add_ons_x[which(!add_ons_x %in% c(
            "redcap_repeat_instrument",
            "redcap_repeat_instance"
          ))]
        }
        if (!is_repeating_form &&
            is_longitudinal && has_repeating_forms) {
          row_index <- which(
            is.na(denormalized$redcap_repeat_instrument) &
              denormalized$redcap_event_name %in% unique(event_mapping$unique_event_name[which(event_mapping$form == form_name)])
          )
        }
        if (!is_repeating_form &&
            is_longitudinal && !has_repeating_forms) {
          row_index <- which(
            denormalized$redcap_event_name %in% unique(event_mapping$unique_event_name[which(event_mapping$form == form_name)])
          )
        }
        if (!is_repeating_form &&
            !is_longitudinal && has_repeating_forms) {
          row_index <- which(is.na(denormalized$redcap_repeat_instrument))
        }
      }
      if (is_something(row_index)) {
        col_names <- unique(c(add_ons_x, form_field_names))
        raw_subset <- denormalized[row_index, col_names]
        if (labelled) {
          raw_subset <- raw_to_labelled_form(form = raw_subset, project = project)
        }
        form_list[[form_name]] <- raw_subset
      }
    }
  }
  form_list
}
#' @noRd
sort_redcap_log <- function(redcap_log) {
  if (nrow(redcap_log) == 0) {
    return(redcap_log)
  }
  unique(redcap_log[order(redcap_log$timestamp, decreasing = TRUE), ])
}
#' @noRd
clean_redcap_log <- function(redcap_log) {
  redcap_log <- unique(redcap_log)
  redcap_log$record_id <- NA
  redcap_log$action_type <- NA
  redcap_log <- redcap_log %>%
    lapply(function(x) {
      x %>% trimws(whitespace = "[\\h\\v]")
    }) %>%
    as.data.frame()
  design_test <- redcap_log$action == "Manage/Design"
  design_rows <- which(design_test)
  not_design_rows <- which(!design_test)
  # notdesign action -----
  record_rows <- not_design_rows[starts_with(match = .log_action_records, vars = redcap_log$action[not_design_rows])]
  redcap_log$record_id[record_rows] <- gsub(
    "Update record|Delete record|Create record|[:(:]API[:):]|Auto|calculation|Lock/Unlock Record | |[:):]|[:(:]",
    "",
    redcap_log$action[record_rows]
  )
  redcap_log$action_type[record_rows] <- redcap_log$action[record_rows] %>%
    strsplit(" ") %>%
    lapply(function(x) {
      x[[1]]
    }) %>%
    unlist()
  redcap_log$action_type[not_design_rows[starts_with(match = .log_action_exports, vars = redcap_log$action[not_design_rows])]] <- "Exports"
  redcap_log$action_type[not_design_rows[starts_with(match = .log_action_users, vars = redcap_log$action[not_design_rows])]] <- "Users"
  redcap_log$action_type[not_design_rows[starts_with(match = .log_action_no_changes, vars = redcap_log$action[not_design_rows])]] <- "No Changes"
  # design details  -------------------
  comment_rows <- design_rows[starts_with(match = .log_details_comments, vars = redcap_log$details[design_rows])]
  redcap_log$record_id[comment_rows] <- stringr::str_extract(string = redcap_log$details[comment_rows], pattern = "(?<=Record: )[^,]+")
  redcap_log$action_type[comment_rows] <- "Comment"
  redcap_log$action_type[design_rows[starts_with(match = .log_details_exports, vars = redcap_log$details[design_rows])]] <- "Exports"
  redcap_log$action_type[design_rows[starts_with(match = .log_details_metadata_major, vars = redcap_log$details[design_rows])]] <- "Metadata Change Major"
  redcap_log$action_type[design_rows[starts_with(match = .log_details_metadata_minor, vars = redcap_log$details[design_rows])]] <- "Metadata Change Minor"
  redcap_log$action_type[design_rows[starts_with(match = .log_details_no_changes, vars = redcap_log$details[design_rows])]] <- "No Changes"
  redcap_log$action_type[design_rows[starts_with(match = .log_details_tokens, vars = redcap_log$details[design_rows])]] <- "Tokens"
  redcap_log$action_type[design_rows[starts_with(match = .log_details_repository, vars = redcap_log$details[design_rows])]] <- "Repository"
  # end ------------
  row_index <- which(is.na(redcap_log$record) &
                       !is.na(redcap_log$record_id))
  redcap_log$record[row_index] <- redcap_log$record_id[row_index]
  row_index <- which(!is.na(redcap_log$record) &
                       is.na(redcap_log$record_id))
  redcap_log$action_type[row_index] <- "Users"
  redcap_log$record_id <- NULL
  redcap_log$username[which(redcap_log$username == "[survey respondent]")] <- NA
  # add drop exports?
  redcap_log <- sort_redcap_log(redcap_log)
  redcap_log
}
#' @noRd
.log_action_exports <- c("Data export", "Download uploaded ")
#' @noRd
.log_details_exports <- c("Export ", "Download ")
#' @noRd
.log_action_users <- c(
  "User assigned to role ",
  "Add user ",
  "Edit user ",
  "Delete user ",
  "Rename user role",
  "User removed from user role",
  "Create user role"
)
#' @noRd
.log_details_comments <- c("Add field comment ",
                           "Edit field comment ",
                           "Delete field comment ")
#' @noRd
.log_action_records <- c("Update record ",
                         "Delete record ",
                         "Lock/Unlock Record ",
                         "Create record ")
#' @noRd
.log_action_no_changes <- c(
  "Enable external module ",
  "Disable external module ",
  "Modify configuration for external module "
)
#' @noRd
.log_details_no_changes <- c(
  "Switch DAG ",
  "Modify custom record dashboard",
  "Delete custom record dashboard",
  "Create custom record dashboard",
  "Create project dashboard",
  "Edit project dashboard",
  "Delete project dashboard",
  "Create project bookmark",
  "Click project bookmark",
  "Edit project bookmark",
  "Delete project bookmark",
  "Edit settings for Form Render Skip Logic",
  "Enter draft mode",
  "Reorder project bookmarks",
  "Multi-Language Management",
  "Edit report",
  "Create report",
  "Reorder report",
  "Copy report",
  "Delete report",
  "Approve production project modifications",
  "Cancel draft mode",
  "Enable auto variable",
  "Disable auto variable",
  "Request approval for",
  "Delete data access group",
  "Create data access group",
  "Send email ",
  "Checked off item in project checklist",
  "Reject production proj",
  "Execute data quality rule",
  "Send request to copy project"
)
#' @noRd
.log_details_tokens <- c("Create API token", "User regenerate own API token")
#' @noRd
.log_details_repository <- c(
  "Upload file to File Repository",
  "Delete file from File Repository",
  "Delete folder from File Repository",
  "Create folder in File Repository",
  "Upload document to file repository"
)
#' @noRd
.log_details_metadata_minor <- c(
  "Tag new identifier fields",
  "Add/edit branching logic",
  "Reorder project fields",
  "Move project field",
  "Delete section header",
  "Reorder data collection instruments"
)
#' @noRd
.log_details_metadata_major <- c(
  "Edit project field",
  "Delete project field",
  "Create project field",
  "Make project customizations",
  "Delete data collection instrument",
  "Download instrument from Shared Library",
  "Create data collection instrument",
  "Copy data collection instrument",
  "Copy project field",
  "Rename data collection instrument",
  "Upload data dictionary",
  "Set up repeating instruments",
  "Modify project settings",
  "Move project ",
  "Copy project as",
  "Create project "
)
#' @noRd
.all_missing_codes <- data.frame(
  code = c(
    "NI",
    "INV",
    "UNK",
    "NASK",
    "ASKU",
    "NAV",
    "MSK",
    "NA",
    "NAVU",
    "NP",
    "QS",
    "QI",
    "TRC",
    "UNC",
    "DER",
    "PINF",
    "NINF",
    "OTH"
  ),
  name = c(
    "No information",
    "Invalid",
    "Unknown",
    "Not asked",
    "Asked but unknown",
    "Temporarily unavailable",
    "Masked",
    "Not applicable",
    "Not available",
    "Not present",
    "Sufficient quantity",
    "Insufficient quantity",
    "Trace",
    "Unencoded",
    "Derived",
    "Positive infinity",
    "Negative infinity",
    "Other"
  ),
  stringsAsFactors = FALSE
)
#' @noRd
check_missing_codes <- function(project) {
  included <- "missing_data_codes" %in% colnames(project$redcap$project_info)
  if (included) {
    is_na <- is.na(project$redcap$project_info$missing_data_codes)
    if (!is_na) {
      return(project$redcap$project_info$missing_data_codes %>% split_choices())
    }
    return(NA)
  }
  NA
}
#' @noRd
.link_types <- c(
  "base",
  "home",
  "record_home",
  "records_dashboard",
  "api",
  "api_playground",
  "codebook",
  "user_rights",
  "setup",
  "logging",
  "designer",
  "dictionary",
  "data_quality",
  "identifiers"
)
#' @noRd
.date_handling_choices <- c(
  "none",
  "exclude_dates",
  "random_shift_by_record",
  "random_shift_by_project",
  "zero_by_record",
  "zero_by_project"
)
