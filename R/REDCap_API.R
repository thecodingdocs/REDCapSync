#' @noRd
rcon_result <- function(project) {
  rcon <- redcapConnection(url = project$links$redcap_uri,
                                      token = get_project_token(project))
  list(
    project_info = tryCatch(
      expr = rcon$projectInformation(),
      error = function(e) {NULL}),
    arms = tryCatch(
      expr = rcon$arms(),
      error = function(e) {NULL}),
    events = tryCatch(
      expr = rcon$events(),
      error = function(e) {NULL}),
    mapping = tryCatch(
      expr = rcon$mapping(),
      error = function(e) {NULL}),
    forms = tryCatch(
      expr = rcon$instruments(),
      error = function(e) {NULL}),
    repeating = tryCatch(
      expr = rcon$repeatInstrumentEvent(),
      error = function(e) {NULL}),
    fields = tryCatch(
      expr = rcon$metadata(),
      error = function(e) {NULL}),
    # logging needed
    logging = tryCatch(
      expr = exportLogging(rcon = rcon, beginTime = Sys.time() - 100000),
      error = function(e) {NULL}),
    # user access needed
    users = tryCatch(
      expr = rcon$users(),
      error = function(e) {NULL}),
    user_roles = tryCatch(
      expr = rcon$user_roles(),
      error = function(e) {NULL}),
    user_role_assignment = tryCatch(
      expr = rcon$user_role_assignment(),
      error = function(e) {NULL}),
    #DAG access needed
    dags = tryCatch(
      expr = rcon$dags(),
      error = function(e) {NULL}),
    dag_assignment = tryCatch(
      expr = rcon$dag_assignment(),
      error = function(e) {NULL}),
    # file repo needed
    file_repository = tryCatch(
      expr = rcon$fileRepository(),
      error = function(e) {NULL})
  )
}
#' @noRd
get_redcap_metadata <- function(project, include_users = TRUE) {
  assert_setup_project(project)
  result <- rcon_result(project)
  project$metadata <- list()
  project$redcap$project_info <- result$project_info
  # info ----------
  project$redcap$project_title <- project$redcap$project_info$project_title
  project$redcap$project_id <- project$redcap$project_info$project_id %>%
    as.character()
  project$metadata$is_longitudinal <-
    project$redcap$project_info$is_longitudinal %>% as.logical()
  missing_data_codes <- NA
  if ("missing_data_codes" %in% colnames(project$redcap$project_info)) {
    missing_data_codes <- project$redcap$project_info$missing_data_codes
    if (!is.na(missing_data_codes)) {
      missing_data_codes <- missing_data_codes %>% split_choices()
    }
  }
  project$metadata$missing_codes <- missing_data_codes
  project$redcap$has_log_access <- !is.null(result$logging)
  project$redcap$has_dag_access <- !is.null(result$dags)
  project$redcap$has_user_access <- !is.null(result$users)
  project$redcap$has_file_repository_access <- !is.null(result$file_repository)
  # instruments --------
  project$metadata$forms <- rename_forms_redcap_to_default(result$forms)
  project$metadata$repeating_forms_events <- result$repeating
  project$metadata$forms$repeating <- FALSE
  project$metadata$has_repeating_forms <- FALSE
  project$metadata$has_repeating_events <- FALSE
  project$metadata$has_repeating_forms_or_events <-
    project$redcap$project_info$has_repeating_instruments_or_events %>%
    as.logical()
  # if(project$redcap$project_info$has_repeating_instruments_or_events=="1")
  if (is.data.frame(project$metadata$repeating_forms_events)) {
    # TODOPLEASE test if you can do this if you dont have designer privileges
    # or would have to use another package
    if (nrow(project$metadata$repeating_forms_events) > 0) {
      project$metadata$forms$repeating <-
        project$metadata$forms$form_name %in%
        project$metadata$repeating_forms_events$form_name
    }
  }
  if (any(project$metadata$forms$repeating)) {
    project$metadata$has_repeating_forms <- TRUE
  }
  # metadata ----------
  project$metadata$fields <- result$fields
  project$metadata$fields$section_header <-
    project$metadata$fields$section_header %>% remove_html_tags()
  project$metadata$fields$field_label <-
    project$metadata$fields$field_label %>% remove_html_tags()
  # RISKY? add to setup project and simple test of unique
  project$metadata$id_col <- project$metadata$fields[1, 1] %>% as.character()
  project$metadata$form_key_cols <- get_key_col_list(data_list = project)
  project$metadata$raw_structure_cols <- project$metadata$form_key_cols %>%
    unlist() %>%
    unique()
  project$metadata$fields <- add_field_elements(project$metadata$fields)
  # project$metadata$fields <-
  # project %>% annotate_fields(summarize_data = FALSE, drop_blanks = FALSE)
  project$metadata$choices <-
    fields_to_choices(fields = project$metadata$fields)
  # add a check for existing conflict possibilities
  project$metadata$has_coding_conflicts <- FALSE
  field_names <- project$metadata$choices$field_name %>% unique()
  if (length(field_names) > 0) {
    choices <- project$metadata$choices
    row_of_conflicts <- field_names %>%
      lapply(function(field_name) {
        anyDuplicated(choices$name[which(choices$field_name == field_name)]) > 0
      }) %>%
      unlist()
    project$metadata$has_coding_conflicts <- any(row_of_conflicts)
    if (project$metadata$has_coding_conflicts) {
      project$metadata$coding_conflict_field_names <-
        field_names[which(row_of_conflicts)]
    }
  }
  # is longitudinal ------
  if (project$metadata$is_longitudinal) {
    raw_structure_cols_vector <- c(project$metadata$raw_structure_cols,
                                   "arm_number",
                                   "event_name")
    project$metadata$raw_structure_cols <- unique(raw_structure_cols_vector)
    project$metadata$arms <- result$arms
    colnames(project$metadata$arms)[
      which(colnames(project$metadata$arms) == "arm_num")] <- "arm_number"
    project$metadata$has_arms <- TRUE
    project$metadata$has_multiple_arms <- nrow(project$metadata$arms) > 1
    project$metadata$has_arms_that_matter <- project$metadata$has_multiple_arms
    project$metadata$event_mapping <- result$mapping
    project$metadata$events <- result$events
    colnames(project$metadata$events)[
      which(colnames(project$metadata$events) == "arm_num")] <- "arm_number"
    project$metadata$events$repeating <- FALSE
    project$metadata$event_mapping$repeating <- FALSE
    if (is.data.frame(project$metadata$repeating_forms_events)) {
      project$metadata$events$repeating <-
        project$metadata$events$unique_event_name %in%
        project$metadata$repeating_forms_events$event_name[
          which(is.na(project$metadata$repeating_forms_events$form_name))]
      repeatingFormsEvents_ind <- project$metadata$repeating_forms_events[
        which(
          !is.na(project$metadata$repeating_forms_events$event_name) &
            !is.na(project$metadata$repeating_forms_events$form_name)
        ), ]
      if (nrow(repeatingFormsEvents_ind) > 0) {
        rows_event_mapping <- seq_len(nrow(repeatingFormsEvents_ind)) %>%
          lapply(function(i) {
            which(
              project$metadata$event_mapping$unique_event_name ==
                repeatingFormsEvents_ind$event_name[i] &
                project$metadata$event_mapping$form ==
                repeatingFormsEvents_ind$form_name[i]
            )
          }) %>%
          unlist()
        project$metadata$event_mapping$repeating[rows_event_mapping] <- TRUE
      }
    }
    project$metadata$events$forms <-
      project$metadata$events$unique_event_name %>%
      lapply(function(events) {
        project$metadata$event_mapping$form[
          which(project$metadata$event_mapping$unique_event_name == events)] %>%
          unique() %>%
          paste0(collapse = " | ")
      }) %>%
      unlist()
    if (project$metadata$has_arms_that_matter) {
      project$metadata$has_arms_that_matter <-
        project$metadata$arms$arm_number %>%
        lapply(function(arm) {
          project$metadata$event_mapping$form[
            which(project$metadata$event_mapping$arm_number == arm)]
        }) %>%
        check_match() %>%
        magrittr::not()
    }
    # if(is.data.frame(project$unique_events)){
    #   project$metadata$events <- data.frame(
    #     event_name = unique(project$unique_events$event_name),
    #     arms = unique(project$unique_events$event_name) %>%
    # lapply(function(event_name){
    #       project$unique_events$arm_number[
    #which(project$unique_events$event_name==event_name)] %>% unique() %>%
    #paste0(collapse = " | ")
    #     })
    #   )
    # }
    project$metadata$forms$repeating_via_events <- FALSE
    project$metadata$forms$repeating_via_events[
      which(
        unlist(lapply(project$metadata$forms$form_name, function(form_name) {
      anyDuplicated(
        project$metadata$event_mapping$arm_num[
          which(project$metadata$event_mapping$form == form_name)]) > 0
    })))] <- TRUE
  } else {
    project$metadata$has_arms <- FALSE
    project$metadata$has_multiple_arms <- FALSE
    project$metadata$has_arms_that_matter <- FALSE
    project$metadata$event_mapping <- NA
    project$metadata$events <- NA
  }
  # other-------
  project$redcap$users <- NA
  if (include_users && project$redcap$has_user_access) {
    keep_cols <- c("unique_role_name", "role_label")
    project$redcap$users <- result$user_roles[, keep_cols] %>%
      merge(result$user_role_assignment, by = "unique_role_name") %>%
      merge(result$users, by = "username", all.y = TRUE)
  }
  project$redcap$file_repository <- NA
  get_file_repository <- project$internals$get_file_repository
  if (get_file_repository && project$redcap$has_file_repository_access) {
    project$redcap$file_repository <- result$file_repository
  }
  #dags
  project$internals$last_metadata_update <- now_time()
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
      select_choices_or_calculations =
        "0, Incomplete | 1, Unverified | 2, Complete",
      stringsAsFactors = FALSE
    )
    last_row <- nrow(fields)
    row_index <- which(fields$form_name == form_name)
    if (length(row_index) == 0) {
      row_index <- last_row
    }
    row <- last(row_index)
    top <- fields[1:row, ]
    bottom <- NULL
    if (last_row > row) {
      bottom <- fields[(row + 1):last_row, ]
    }
    fields <- top %>%
      bind_rows(new_row) %>%
      bind_rows(bottom)
  }
  if (any(fields$field_type == "checkbox")) {
    for (field_name in fields$field_name[
      which(fields$field_type == "checkbox")]) {
      x <- fields$select_choices_or_calculations[
        which(fields$field_name == field_name)] %>% split_choices()
      new_rows <- data.frame(
        field_name = paste0(field_name, "___", x$code),
        form_name = fields$form_name[which(fields$field_name == field_name)],
        field_label = x$name,
        field_type = "checkbox_choice",
        select_choices_or_calculations = "1, Checked | 0, Unchecked",
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
        bind_rows(new_rows) %>%
        bind_rows(bottom)
    }
  }
  if (any(fields$field_type == "yesno")) {
    yn_rows <- which(fields$field_type == "yesno")
    fields$select_choices_or_calculations[yn_rows] <- "1, Yes | 0, No"
  }
  if (any(fields$field_type == "truefalse")) {
    tf_rows <- which(fields$field_type == "truefalse")
    fields$select_choices_or_calculations[tf_rows] <- "1, True | 0, False"
  }
  fields
}
#' @noRd
update_project_links <- function(project) {
  redcap_base <- project$links$redcap_base
  version <- project$redcap$version
  head <- paste0(redcap_base, "redcap_v", version)
  tail <- paste0("?pid=", project$redcap$project_id)
  home <- "/index.php" %>% paste0(tail)
  record_home <- "/DataEntry/record_home.php" %>% paste0(tail)
  # record_subpage <- "/DataEntry/index.php" %>% paste0(tail)
  records_dashboard <- "/DataEntry/record_status_dashboard.php" %>% paste0(tail)
  api <- "/API/project_api.php" %>% paste0(tail)
  api_playground <- "/API/playground.php" %>% paste0(tail)
  setup <- "/ProjectSetup/index.php" %>% paste0(tail)
  user_rights <- "/UserRights/index.php" %>% paste0(tail)
  logging <- "/Logging/index.php" %>% paste0(tail)
  designer <- "/Design/online_designer.php" %>% paste0(tail)
  codebook <- "/Design/data_dictionary_codebook.php" %>% paste0(tail)
  dictionary <- "/Design/data_dictionary_upload.php" %>% paste0(tail)
  data_quality <- "/DataQuality/index.php" %>% paste0(tail)
  identifiers <- home %>% paste0("&route=IdentifierCheckController:index")
  project$links$redcap_home <- paste0(head, home)
  project$links$redcap_record_home <- paste0(head, record_home)
  # project$links$redcap_record_subpage <- paste0(head, record_subpage)
  project$links$redcap_records_dashboard <- paste0(head, records_dashboard)
  project$links$redcap_api <- paste0(head, api)
  project$links$redcap_api_playground <- paste0(head, api_playground)
  project$links$redcap_setup <- paste0(head, setup)
  project$links$redcap_user_rights <- paste0(head, user_rights)
  project$links$redcap_logging <- paste0(head, logging)
  project$links$redcap_designer <- paste0(head, designer)
  project$links$redcap_codebook <- paste0(head, codebook)
  project$links$redcap_dictionary <- paste0(head, dictionary)
  project$links$redcap_data_quality <- paste0(head, data_quality)
  project$links$redcap_identifiers <- paste0(head, identifiers)
  invisible(project)
}
#' @noRd
get_redcap_files <- function(project,
                             original_file_names = FALSE,
                             overwrite = FALSE) {
  file_rows <- which(project$metadata$fields$field_type == "file")
  out_dir <- file.path(
    project$dir_path, "REDCap", project$project_name, "files")
  if (length(file_rows) > 0) {
    dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
    for (field_name in project$metadata$fields$field_name[file_rows]) {
      out_dir_folder <- file.path(out_dir, field_name)
      dir.create(out_dir_folder,
                 showWarnings = FALSE,
                 recursive = TRUE)
      form_name <- project$metadata$fields$form_name[
        which(project$metadata$fields$field_name == field_name)]
      is_repeating <- project$metadata$forms$repeating[
        which(project$metadata$forms$form_name == form_name)]
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
            file_ext_alias(file_name)
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
get_redcap_log <- function(project,
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
  redcap_log <- httr::content(response) %>% bind_rows()
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
#' @noRd
get_redcap_denormalized <- function(project,
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
#' @keywords internal
get_redcap_report <- function(project, report_id, silent = TRUE) {
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
get_redcap_data <- function(project,
                            labelled = TRUE,
                            records = NULL,
                            batch_size = 2000) {
  form_list <- list()
  denormalized <- get_redcap_denormalized(
    project = project,
    labelled = FALSE,
    records = records,
    batch_size = batch_size
  ) # add check for dag and api
  form_list <- denormalized %>%
    normalize_redcap(project = project, labelled = labelled)
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
