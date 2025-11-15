#' @noRd
project_rcon <- function(project) {
  assert_setup_project(project)
  rcon <- redcapAPI::redcapConnection(url = project$links$redcap_uri,
                                      token = get_project_token(project))
  # test connection
  rcon
}
#' @noRd
get_REDCap_metadata <- function(project, include_users = TRUE) {
  assert_setup_project(project)
  project$internals$last_metadata_update <- now_time()
  project$metadata <- list()
  rcon <- project_rcon(project)
  # info ----------
  project$redcap$project_info <- REDCapR::redcap_project_info_read(redcap_uri = project$links$redcap_uri,
                                                                   token = get_project_token(project))[["data"]] # remove at some point
  project$redcap$project_title <- project$redcap$project_info$project_title
  project$redcap$project_id <- project$redcap$project_info$project_id %>% as.character()
  project$metadata$is_longitudinal <- project$redcap$project_info$is_longitudinal
  project$metadata$missing_codes <- missing_codes2(project)
  project$redcap$has_log_access <- test_redcap_log_access(project)
  # instruments --------
  project$metadata$forms <- REDCapR::redcap_instruments(redcap_uri = project$links$redcap_uri,
                                                        token = get_project_token(project))[["data"]] %>% rename_forms_redcap_to_default()
  project$metadata$forms$repeating <- FALSE
  project$metadata$has_repeating_forms <- FALSE
  project$metadata$has_repeating_events <- FALSE
  project$metadata$has_repeating_forms_or_events <- project$redcap$project_info$has_repeating_instruments_or_events
  # if(project$redcap$project_info$has_repeating_instruments_or_events=="1")
  repeating_forms_events <- redcapAPI::exportRepeatingInstrumentsEvents(rcon = rcon)
  if (is.data.frame(repeating_forms_events)) {
    # TODOPLEASE test if you can do this if you dont have designer privilages or would have to use another package
    if (nrow(repeating_forms_events) > 0) {
      project$metadata$forms$repeating <- project$metadata$forms$form_name %in% repeating_forms_events$form_name
    }
  }
  if (any(project$metadata$forms$repeating)) {
    project$metadata$has_repeating_forms <- TRUE
  }
  # metadata ----------
  project$metadata$fields <- REDCapR::redcap_metadata_read(redcap_uri = project$links$redcap_uri,
                                                           token = get_project_token(project))[["data"]]
  project$metadata$fields$section_header <- project$metadata$fields$section_header %>% remove_html_tags()
  project$metadata$fields$field_label <- project$metadata$fields$field_label %>% remove_html_tags()
  project$metadata$id_col <- project$metadata$fields[1, 1] %>% as.character() # RISKY?
  project$metadata$form_key_cols <- get_key_col_list(data_list = project)
  project$metadata$raw_structure_cols <- project$metadata$form_key_cols %>%
    unlist() %>%
    unique()
  project$metadata$fields <- add_field_elements(project$metadata$fields)
  # project$metadata$fields <- project %>% annotate_fields(summarize_data = FALSE, drop_blanks = FALSE)
  project$metadata$choices <- fields_to_choices(fields = project$metadata$fields)
  # is longitudinal ------
  if (project$metadata$is_longitudinal) {
    project$metadata$raw_structure_cols <- c(project$metadata$raw_structure_cols,
                                             "arm_number",
                                             "event_name") %>% unique()
    project$metadata$arms <- REDCapR::redcap_arm_export(redcap_uri = project$links$redcap_uri,
                                                        token = get_project_token(project))[["data"]] %>% all_character_cols()
    project$metadata$has_arms <- TRUE
    project$metadata$has_multiple_arms <- nrow(project$metadata$arms) > 1
    project$metadata$has_arms_that_matter <- project$metadata$has_multiple_arms
    project$metadata$event_mapping <- REDCapR::redcap_event_instruments(redcap_uri = project$links$redcap_uri,
                                                                        token = get_project_token(project))[["data"]] %>% all_character_cols()
    project$metadata$events <- REDCapR::redcap_event_read(redcap_uri = project$links$redcap_uri,
                                                          token = get_project_token(project))[["data"]] %>% all_character_cols()
    colnames(project$metadata$events)[which(colnames(project$metadata$events) == "arm_num")] <- "arm_number"
    project$metadata$events$repeating <- FALSE
    project$metadata$event_mapping$repeating <- FALSE
    if (is.data.frame(repeating_forms_events)) {
      project$metadata$events$repeating <- project$metadata$events$unique_event_name %in% repeating_forms_events$event_name[which(is.na(repeating_forms_events$form_name))]
      repeatingFormsEvents_ind <- repeating_forms_events[which(
        !is.na(repeating_forms_events$event_name) &
          !is.na(repeating_forms_events$form_name)
      ), ]
      if (nrow(repeatingFormsEvents_ind) > 0) {
        rows_event_mapping <- seq_len(nrow(repeatingFormsEvents_ind)) %>%
          lapply(function(i) {
            which(
              project$metadata$event_mapping$unique_event_name == repeatingFormsEvents_ind$event_name[i] &
                project$metadata$event_mapping$form == repeatingFormsEvents_ind$form_name[i]
            )
          }) %>%
          unlist()
        project$metadata$event_mapping$repeating[rows_event_mapping] <- TRUE
      }
    }
    project$metadata$events$forms <- project$metadata$events$unique_event_name %>%
      lapply(function(events) {
        project$metadata$event_mapping$form[which(project$metadata$event_mapping$unique_event_name == events)] %>%
          unique() %>%
          paste0(collapse = " | ")
      }) %>%
      unlist()
    if (project$metadata$has_arms_that_matter) {
      project$metadata$has_arms_that_matter <- project$metadata$arms$arm_number %>%
        lapply(function(arm) {
          project$metadata$event_mapping$form[which(project$metadata$event_mapping$arm_number == arm)]
        }) %>%
        check_match() %>%
        magrittr::not()
    }
    # if(is.data.frame(project$unique_events)){
    #   project$metadata$events <- data.frame(
    #     event_name = unique(project$unique_events$event_name),
    #     arms = unique(project$unique_events$event_name) %>% lapply(function(event_name){
    #       project$unique_events$arm_number[which(project$unique_events$event_name==event_name)] %>% unique() %>% paste0(collapse = " | ")
    #     })
    #   )
    # }
    project$metadata$forms$repeating_via_events <- FALSE
    project$metadata$forms$repeating_via_events[which(unlist(lapply(project$metadata$forms$form_name, function(form_name) {
      anyDuplicated(project$metadata$event_mapping$arm_num[which(project$metadata$event_mapping$form == form_name)]) > 0
    })))] <- TRUE
  } else {
    project$metadata$has_arms <- FALSE
    project$metadata$has_multiple_arms <- FALSE
    project$metadata$has_arms_that_matter <- FALSE
    project$metadata$event_mapping <- NA
    project$metadata$events <- NA
  }
  # other-------
  if (include_users) {
    project <- get_REDCap_users(project)
  }
  # add a check for exisiting conflict possibilities
  project$metadata$has_coding_conflicts <- FALSE
  field_names <- project$metadata$choices$field_name %>% unique()
  if (length(field_name) > 0) {
    row_of_conflicts <- field_names %>%
      lapply(function(field_name) {
        anyDuplicated(project$metadata$choices$name[which(project$metadata$choices$field_name == field_name)]) > 0
      }) %>%
      unlist()
    project$metadata$has_coding_conflicts <- any(row_of_conflicts)
    if (project$metadata$has_coding_conflicts) {
      project$metadata$coding_conflict_field_names <- field_names[which(row_of_conflicts)]
    }
  }
  project <- update_project_links(project)
  invisible(project)
}
#' @noRd
add_field_elements <- function(fields) {
  #assert_fields should be here
  form_names <- unique(fields$form_name)
  for (form_name in form_names) {
    field_name <- paste0(form_name, "_complete")
    new_row <- data.frame(
      field_name = field_name, # check if conflicts,
      form_name = form_name,
      field_type = "radio",
      field_label = field_name %>%
        strsplit("_") %>%
        unlist() %>%
        stringr::str_to_title() %>%
        paste(collapse = " "),
      select_choices_or_calculations = "0, Incomplete | 1, Unverified | 2, Complete",
      stringsAsFactors = FALSE
    )
    last_row <- nrow(fields)
    row_index <- which(fields$form_name == form_name)
    if (length(row_index) == 0) {
      row_index <- last_row
    }
    row <- dplyr::last(row_index)
    top <- fields[1:row, ]
    bottom <- NULL
    if (last_row > row) {
      bottom <- fields[(row + 1):last_row, ]
    }
    fields <- top %>%
      dplyr::bind_rows(new_row) %>%
      dplyr::bind_rows(bottom)
  }
  if (any(fields$field_type == "checkbox")) {
    for (field_name in fields$field_name[which(fields$field_type == "checkbox")]) {
      x <- fields$select_choices_or_calculations[which(fields$field_name == field_name)] %>% split_choices()
      new_rows <- data.frame(
        field_name = paste0(field_name, "___", x$code),
        form_name = fields$form_name[which(fields$field_name == field_name)],
        field_label = x$name,
        field_type = "checkbox_choice",
        select_choices_or_calculations = "0, Unchecked | 1, Checked",
        stringsAsFactors = FALSE
      )
      row <- which(fields$field_name == field_name)
      last_row <- nrow(fields)
      top <- fields[1:row, ]
      bottom <- NULL
      if (last_row > row) {
        bottom <- fields[(row + 1):last_row, ]
      }
      fields <- top %>%
        dplyr::bind_rows(new_rows) %>%
        dplyr::bind_rows(bottom)
    }
  }
  if (any(fields$field_type == "yesno")) {
    fields$select_choices_or_calculations[which(fields$field_type == "yesno")] <- "0, No | 1, Yes"
  }
  fields
}
#' @noRd
update_project_links <- function(project) {
  project$links$redcap_home <- paste0(
    project$links$redcap_base,
    "redcap_v",
    project$redcap$version,
    "/index.php?pid=",
    project$redcap$project_id
  )
  project$links$redcap_record_home <- paste0(
    project$links$redcap_base,
    "redcap_v",
    project$redcap$version,
    "/DataEntry/record_home.php?pid=",
    project$redcap$project_id
  )
  project$links$redcap_record_subpage <- paste0(
    project$links$redcap_base,
    "redcap_v",
    project$redcap$version,
    "/DataEntry/index.php?pid=",
    project$redcap$project_id
  )
  project$links$redcap_records_dashboard <- paste0(
    project$links$redcap_base,
    "redcap_v",
    project$redcap$version,
    "/DataEntry/record_status_dashboard.php?pid=",
    project$redcap$project_id
  )
  project$links$redcap_api <- paste0(
    project$links$redcap_base,
    "redcap_v",
    project$redcap$version,
    "/API/project_api.php?pid=",
    project$redcap$project_id
  )
  project$links$redcap_api_playground <- paste0(
    project$links$redcap_base,
    "redcap_v",
    project$redcap$version,
    "/API/playground.php?pid=",
    project$redcap$project_id
  )
  project$links$redcap_codebook <- paste0(
    project$links$redcap_base,
    "redcap_v",
    project$redcap$version,
    "/Design/data_dictionary_codebook.php?pid=",
    project$redcap$project_id
  )
  project$links$redcap_user_rights <- paste0(
    project$links$redcap_base,
    "redcap_v",
    project$redcap$version,
    "/UserRights/index.php?pid=",
    project$redcap$project_id
  )
  invisible(project)
}
#' @noRd
get_REDCap_files <- function(project,
                             original_file_names = FALSE,
                             overwrite = FALSE) {
  file_rows <- which(project$metadata$fields$field_type == "file")
  out_dir <- file.path(project$dir_path, "REDCap", project$short_name, "files")
  if (length(file_rows) > 0) {
    dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
    for (field_name in project$metadata$fields$field_name[file_rows]) {
      out_dir_folder <- file.path(out_dir, field_name)
      dir.create(out_dir_folder,
                 showWarnings = FALSE,
                 recursive = TRUE)
      form_name <- project$metadata$fields$form_name[which(project$metadata$fields$field_name == field_name)]
      is_repeating <- project$metadata$forms$repeating[which(project$metadata$forms$form_name == form_name)]
      form <- project$data[[form_name]]
      rows_to_save <- which(!is.na(form[[field_name]]))
      for (i in rows_to_save) {
        file_name <- form[[field_name]][i]
        record_id <- form[[project$metadata$id_col]][i]
        repeat_instance <- form[["redcap_repeat_instance"]][i]
        redcap_event_name <- form[["redcap_event_name"]][i]
        if (!original_file_names) {
          if (anyDuplicated(file_name) > 0) {
            warning(
              "You have duplicate file names in ",
              form_name,
              ", ",
              field_name,
              ". Therefore will use system generated names",
              immediate. = TRUE
            )
            original_file_names <- FALSE
          }
        }
        file_name <- ifelse(
          original_file_names,
          file_name,
          paste0(
            form_name,
            "_",
            field_name,
            "_",
            ifelse(is_repeating, "inst_", ""),
            repeat_instance,
            "ID_",
            record_id,
            ".",
            tools::file_ext(file_name)
          )
        )
        if (!file.exists(file.path(out_dir_folder, file_name)) ||
            overwrite) {
          REDCapR::redcap_file_download_oneshot(
            redcap_uri = project$links$redcap_uri,
            token = get_project_token(project),
            field = field_name,
            record = form[[project$metadata$id_col]][i],
            directory = out_dir_folder,
            file_name = file_name,
            event = redcap_event_name,
            repeat_instance = repeat_instance,
            verbose = FALSE
          )
          cli_alert_wrap(paste0("`", file_name, "` saved."), bullet_type = ">")
        }
      }
    }
  }
  cli_alert_wrap("Checked for files! Stored at ...",
                 file = out_dir,
                 bullet_type = "v")
}
#' @noRd
get_REDCap_users <- function(project) {
  assert_setup_project(project)
  x <- REDCapR::redcap_users_export(redcap_uri = project$links$redcap_uri,
                                    token = sanitize_token(Sys.getenv(project$redcap$token_name)))
  if (!x$success) {
    return(invisible(project))
  }
  data_user <- x$data_user
  # data_user_form <- x$data_user_form
  # add feedback of access
  # rcon <- project_rcon(project)
  # users <- redcapAPI::exportUsers(
  #   rcon = rcon,
  #   labels = FALSE,
  #   form_rights = FALSE
  # )
  # user_roles <- redcapAPI::exportUserRoles(
  #   rcon = rcon,
  #   labels = FALSE,
  #   form_rights = FALSE
  # )
  # user_role_assignments <- redcapAPI::exportUserRoleAssignments(rcon = rcon)
  # final <- merge(merge(user_roles[, c("unique_role_name", "role_label")], user_role_assignments, by = "unique_role_name"), users, by = "username", all.y = TRUE)
  # final
  project$redcap$users <- x$data_user
  invisible(project)
}
#' @noRd
get_REDCap_log <- function(project,
                           log_begin_date = Sys.Date() - 10L,
                           clean = TRUE,
                           record = NULL,
                           user = NULL) {
  assert_setup_project(project)
  assert_logical(clean)
  assert_date(log_begin_date)
  if (log_begin_date == Sys.Date()) {
    log_begin_date <- log_begin_date - 1 # keep getting errors for same day checks?
  }
  redcap_log <- tryCatch(
    expr = {
      REDCapR::redcap_log_read(
        redcap_uri = project$links$redcap_uri,
        token = get_project_token(project),
        log_begin_date = log_begin_date,
        record = record,
        user = user
      )[["data"]]
    },
    error = function(e) {
      NULL
    }
  )
  if (is.data.frame(redcap_log)) {
    if (clean) {
      redcap_log <- redcap_log %>% clean_redcap_log()
    }
  }
  redcap_log # deal with if NA if user does not have log privileges.
}
get_REDCap_log2 <- function(project,
                            log_begin_date = Sys.Date() - 10L,
                            clean = TRUE,
                            record = NULL,
                            user = NULL) {
  assert_setup_project(project)
  assert_logical(clean)
  assert_date(log_begin_date)
  response <- httr::POST(
    url = project$links$redcap_uri,
    body = list(
      token = get_project_token(project),
      content = "log",
      logtype = "",
      user = user,
      record = record,
      beginTime = as.character(log_begin_date) %>% paste("00:00:00"),
      endTime = "",
      format = "json",
      returnFormat = "json"
    ),
    encode = "form"
  )
  if (httr::http_error(response)) {
    message(httr::content(response)$error)
    return(NULL)
  }
  redcap_log <- httr::content(response) %>% dplyr::bind_rows()
  if (is.data.frame(redcap_log)) {
    if (nrow(redcap_log) > 0) {
      redcap_log[redcap_log == ""] <- NA
      if (clean) {
        redcap_log <- redcap_log %>% clean_redcap_log()
      }
    }
  }
  redcap_log # deal with if NA if user does not have log privileges.
}
test_redcap_log_access <- function(project) {
  the_test <- get_REDCap_log2(project = project, log_begin_date = Sys.Date())
  ! is.null(the_test)
}
#' @noRd
get_REDCap_denormalized <- function(project,
                                    labelled = FALSE,
                                    records = NULL,
                                    batch_size = 1000) {
  denormalized <- REDCapR::redcap_read(
    redcap_uri = project$links$redcap_uri,
    token = get_project_token(project),
    batch_size = batch_size,
    interbatch_delay = 0.1,
    records = records,
    raw_or_label = ifelse(labelled, "label", "raw")
  )$data %>% all_character_cols()
  return(denormalized)
}
#' @title Get REDCap Report
#' @inheritParams save_project
#' @param report_id character or integer of REDCap report ID. This can be found
#' at the end of the URL of the report.
#' @return data.frame of REDCap report
#' @export
get_REDCap_report <- function(project, report_id, silent = TRUE) {
  report_id <- as.integer(report_id)
  report <- REDCapR::redcap_report(
    redcap_uri = project$links$redcap_uri,
    token = get_project_token(project),
    report_id = report_id,
    verbose = !silent
  )
  return(report)
}
#' @noRd
get_REDCap_data <- function(project,
                            labelled = TRUE,
                            records = NULL,
                            batch_size = 2000) {
  form_list <- list()
  denormalized <- get_REDCap_denormalized(
    project = project,
    labelled = FALSE,
    records = records,
    batch_size = batch_size
  ) # add check for dag and api
  form_list <- denormalized %>% normalize_redcap(project = project, labelled = labelled)
  return(form_list)
}
#' @noRd
rename_forms_redcap_to_default <- function(forms) {
  the_names <- colnames(forms)
  the_names[which(the_names == "instrument_name")] <- "form_name"
  the_names[which(the_names == "instrument_label")] <- "form_label"
  colnames(forms) <- the_names
  forms
}
