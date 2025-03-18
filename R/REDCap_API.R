#' @noRd
project_rcon <- function(project) {
  assert_setup_project(project)
  rcon <- redcapAPI::redcapConnection(
    url = project$links$redcap_uri,
    token = get_project_token(project)
  )
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
  project$redcap$project_info <- REDCapR::redcap_project_info_read(
    redcap_uri = project$links$redcap_uri,
    token = get_project_token(project)
  )[["data"]] # remove at some point
  project$redcap$project_title <- project$redcap$project_info$project_title
  project$redcap$project_id <- project$redcap$project_info$project_id %>% as.character()
  project$redcap$is_longitudinal <- project$redcap$project_info$is_longitudinal
  project$metadata$missing_codes <- missing_codes2(project)
  # instruments --------
  project$metadata$forms <- REDCapR::redcap_instruments(
    redcap_uri = project$links$redcap_uri,
    token = get_project_token(project)
  )[["data"]] %>% rename_forms_redcap_to_default()
  project$metadata$forms$repeating <- FALSE
  project$redcap$has_repeating_forms <- FALSE
  project$redcap$has_repeating_events <- FALSE
  project$redcap$has_repeating_forms_or_events <- project$redcap$project_info$has_repeating_instruments_or_events
  # if(project$redcap$project_info$has_repeating_instruments_or_events=="1")
  repeating_forms_events <- redcapAPI::exportRepeatingInstrumentsEvents(rcon = rcon)
  if (is.data.frame(repeating_forms_events)) {
    if (nrow(repeating_forms_events) > 0) {
      project$metadata$forms$repeating <- project$metadata$forms$form_name %in% repeating_forms_events$form_name
    }
  }
  if (any(project$metadata$forms$repeating)) {
    project$redcap$has_repeating_forms <- TRUE
  }
  # metadata ----------
  project$metadata$fields <- REDCapR::redcap_metadata_read(
    redcap_uri = project$links$redcap_uri,
    token = get_project_token(project)
  )[["data"]]
  project$metadata$fields$section_header <- project$metadata$fields$section_header %>% remove_html_tags()
  project$metadata$fields$field_label <- project$metadata$fields$field_label %>% remove_html_tags()
  project$redcap$id_col <- project$metadata$fields[1, 1] %>% as.character() # RISKY?
  project$metadata$form_key_cols <- get_key_col_list(project, transform = FALSE)
  project$redcap$raw_structure_cols <- project$metadata$form_key_cols %>%
    unlist() %>%
    unique()
  form_names <- project$metadata$forms$form_name # [which(project$metadata$forms$form_name%in%unique(project$metadata$fields$form_name))]
  for (form_name in form_names) {
    new_row <- data.frame(
      field_name = paste0(form_name, "_complete"),
      form_name = form_name,
      field_type = "radio",
      field_label = paste0(form_name, "_complete") %>%
        strsplit("_") %>%
        unlist() %>%
        stringr::str_to_title() %>%
        paste0(collapse = " "),
      select_choices_or_calculations = "0, Incomplete | 1, Unverified | 2, Complete"
    )
    last_row <- nrow(project$metadata$fields)
    rows <- which(project$metadata$fields$form_name == form_name)
    if (length(rows) == 0) {
      rows <- last_row
    }
    row <- dplyr::last(rows)
    top <- project$metadata$fields[1:row, ]
    bottom <- NULL
    if (last_row > row) {
      bottom <- project$metadata$fields[(row + 1):last_row, ]
    }
    project$metadata$fields <- top %>%
      dplyr::bind_rows(new_row) %>%
      dplyr::bind_rows(bottom)
  }
  if (any(project$metadata$fields$field_type == "checkbox")) {
    for (field_name in project$metadata$fields$field_name[which(project$metadata$fields$field_type == "checkbox")]) {
      x <- project$metadata$fields$select_choices_or_calculations[which(project$metadata$fields$field_name == field_name)] %>% split_choices()
      new_rows <- data.frame(
        field_name = paste0(field_name, "___", x$code),
        form_name = project$metadata$fields$form_name[which(project$metadata$fields$field_name == field_name)],
        field_label = x$name,
        field_type = "checkbox_choice",
        select_choices_or_calculations = c("0, Unchecked | 1, Checked")
      )
      row <- which(project$metadata$fields$field_name == field_name)
      last_row <- nrow(project$metadata$fields)
      top <- project$metadata$fields[1:row, ]
      bottom <- NULL
      if (last_row > row) {
        bottom <- project$metadata$fields[(row + 1):last_row, ]
      }
      project$metadata$fields <- top %>%
        dplyr::bind_rows(new_rows) %>%
        dplyr::bind_rows(bottom)
    }
  }
  if (any(project$metadata$fields$field_type == "yesno")) {
    project$metadata$fields$select_choices_or_calculations[which(project$metadata$fields$field_type == "yesno")] <- c("0, No | 1, Yes")
  }
  project$metadata$fields <- project %>% annotate_fields(summarize_data = FALSE, drop_blanks = FALSE)
  project$metadata$choices <- fields_to_choices(fields = project$metadata$fields)
  # is longitudinal ------
  if (project$redcap$is_longitudinal) {
    project$redcap$raw_structure_cols <- c(
      project$redcap$raw_structure_cols,
      "arm_number",
      "event_name"
    ) %>% unique()
    project$metadata$arms <- REDCapR::redcap_arm_export(
      redcap_uri = project$links$redcap_uri,
      token = get_project_token(project)
    )[["data"]] %>% all_character_cols()
    project$redcap$has_arms <- TRUE
    project$redcap$has_multiple_arms <- nrow(project$metadata$arms) > 1
    project$redcap$has_arms_that_matter <- project$redcap$has_multiple_arms
    project$metadata$event_mapping <- REDCapR::redcap_event_instruments(
      redcap_uri = project$links$redcap_uri,
      token = get_project_token(project)
    )[["data"]] %>% all_character_cols()
    project$metadata$events <- REDCapR::redcap_event_read(
      redcap_uri = project$links$redcap_uri,
      token = get_project_token(project)
    )[["data"]] %>% all_character_cols()
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
    if (project$redcap$has_arms_that_matter) {
      project$redcap$has_arms_that_matter <- project$metadata$arms$arm_number %>%
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
    project$metadata$forms$repeating_via_events[which(project$metadata$forms$form_name %>% lapply(function(form_name) {
      anyDuplicated(project$metadata$event_mapping$arm_number[which(project$metadata$event_mapping$form == form_name)]) > 0
    }) %>% unlist())] <- TRUE
  } else {
    project$redcap$has_arms <- FALSE
    project$redcap$has_multiple_arms <- FALSE
    project$redcap$has_arms_that_matter <- FALSE
    project$metadata$event_mapping <- NA
    project$metadata$events <- NA
  }
  # other-------
  if (include_users) {
    project$redcap$users <- get_REDCap_users(project)
    project$redcap$log <- get_REDCap_log(project, log_begin_date = Sys.Date())
    project$redcap$users$current_user <- project$redcap$users$username == project$redcap$log$username[which(project$redcap$log$details == "Export REDCap version (API)") %>% dplyr::first()]
  }
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
  project$links$redcap_API <- paste0(
    project$links$redcap_base,
    "redcap_v",
    project$redcap$version,
    "/API/project_api.php?pid=",
    project$redcap$project_id
  )
  project$links$redcap_API_playground <- paste0(
    project$links$redcap_base,
    "redcap_v",
    project$redcap$version,
    "/API/playground.php?pid=",
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
      dir.create(
        out_dir_folder,
        showWarnings = FALSE,
        recursive = TRUE
      )
      form_name <- project$metadata$fields$form_name[which(project$metadata$fields$field_name == field_name)]
      is_repeating <- project$metadata$forms$repeating[which(project$metadata$forms$form_name == form_name)]
      form <- project$data[[form_name]]
      rows_to_save <- which(!is.na(form[[field_name]]))
      for (i in rows_to_save) {
        file_name <- form[[field_name]][i]
        record_id <- form[[project$redcap$id_col]][i]
        repeat_instance <- form[["redcap_repeat_instance"]][i]
        redcap_event_name <- form[["redcap_event_name"]][i]
        if (!original_file_names) {
          if (anyDuplicated(file_name) > 0) {
            warning(
              paste0(
                "You have duplicate file names in ",
                form_name,
                ", ",
                field_name,
                ". Therefore will use system generated names"
              ),
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
        if (!file.exists(file.path(out_dir_folder, file_name)) || overwrite) {
          REDCapR::redcap_file_download_oneshot(
            redcap_uri = project$links$redcap_uri,
            token = get_project_token(project),
            field = field_name,
            record = form[[project$redcap$id_col]][i],
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
  cli_alert_wrap(
    "Checked for files! Stored at ...",
    file = out_dir,
    bullet_type = "v"
  )
}
get_REDCap_users <- function(project) {
  assert_setup_project(project)
  rcon <- project_rcon(project)
  users <- redcapAPI::exportUsers(
    rcon = rcon,
    labels = FALSE,
    form_rights = FALSE
  )
  user_roles <- redcapAPI::exportUserRoles(
    rcon = rcon,
    labels = FALSE,
    form_rights = FALSE
  )
  user_role_assignments <- redcapAPI::exportUserRoleAssignments(rcon = rcon)
  final <- merge(merge(user_roles[, c("unique_role_name", "role_label")], user_role_assignments, by = "unique_role_name"), users, by = "username", all.y = TRUE)
  final
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
  if(log_begin_date == Sys.Date()){
    log_begin_date <- log_begin_date - 1 # keep getting errors for same day checks?
  }
  log <- tryCatch(
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
  if (is.data.frame(log)) {
    if (clean) {
      log <- log %>% clean_redcap_log()
    }
  }
  log # deal with if NA if user does not have log privileges.
}
#' @noRd
get_REDCap_raw_data <- function(
    project,
    labelled = FALSE,
    records = NULL,
    batch_size = 1000) {
  raw <- REDCapR::redcap_read(
    redcap_uri = project$links$redcap_uri,
    token = get_project_token(project),
    batch_size = batch_size,
    interbatch_delay = 0.1,
    records = records,
    raw_or_label = ifelse(labelled, "label", "raw")
  )$data %>% all_character_cols()
  return(raw)
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
  raw <- get_REDCap_raw_data(
    project = project,
    labelled = FALSE,
    records = records,
    batch_size = batch_size
  )
  form_list <- raw %>% raw_process_redcap(project = project, labelled = labelled)
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
#' @noRd
rename_forms_default_to_redcap <- function(forms) {
  the_names <- colnames(forms)
  the_names[which(the_names == "form_name")] <- "instrument_name"
  the_names[which(the_names == "form_label")] <- "instrument_label"
  colnames(forms) <- the_names
  forms
}
