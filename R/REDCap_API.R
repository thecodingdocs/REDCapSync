#' @noRd
redcap_api_request <- function(url, token, additional_args = NULL) {
  body <- list(
    "token" = token
  )
  if (!is.null(additional_args)) {
    body <- body %>% append(additional_args)
  }
  httr::POST(
    url = url,
    body = body,
    encode = "form"
  )
}
#' @noRd
process_redcap_response <- function(response, error_action = "warn", method, show_method_help = TRUE) {
  bind <- TRUE
  if (!missing(method)) {
    if (method == "exp_rc_v") {
      content <- response %>%
        httr::content(as = "text") %>%
        as.character()
      bind <- FALSE
    }
  }
  if (bind) {
    content <- httr::content(response) %>%
      dplyr::bind_rows() %>%
      all_character_cols()
  }
  if (httr::http_error(response)) {
    if (!error_action %in% c("stop", "warn")) stop("error_action must be 'stop' or 'warn'")
    general_error <- response$status_code
    specific_error <- http_errors$Description[which(http_errors$Value == response$status_code)]
    message <- paste0("HTTP error ", general_error, ". ", specific_error, ". ")
    if ("error" %in% names(content)) message <- paste0(message, content[["error"]])
    if (error_action == "stop") stop(message)
    warning(message, immediate. = TRUE)
    if (!missing(method) && show_method_help) {
      show_redcap_api_method_info(method)
    }
    return(NA)
  }
  return(content)
}
#' @noRd
run_redcap_api_method <- function(project, url, token, method, error_action = "warn", additional_args = NULL, only_get = FALSE, show_method_help = TRUE) {
  if (!missing(project)) {
    url <- project$links$redcap_uri
    token <- assert_REDCap_token(project)
  }
  allowed_methods <- REDCap_API$methods$method_short_name %>% sort()
  if (only_get) {
    allowed_methods <- REDCap_API$methods$method_short_name[which(REDCap_API$methods$action_type == "export")] %>% sort()
  }
  if (!method %in% allowed_methods) stop("Can only use the following methods... ", as_comma_string(allowed_methods))
  method_param_df <- REDCap_API$params[which(REDCap_API$params$method_short_name == method), ]
  method_param_df <- method_param_df[which(method_param_df$Parameter != "token"), ]
  base_rows <- which(!method_param_df$non_base)
  if (!is.null(additional_args)) {
    if ("content" %in% names(additional_args)) stop("Do not supply default-defined content additional_args... This is automatically tied to method!")
  }
  for (i in base_rows) {
    if (!method_param_df$Parameter[i] %in% names(additional_args)) {
      additional_args <- additional_args %>% append(
        stats::setNames(method_param_df$default[i], method_param_df$Parameter[i]) %>% as.list()
      )
    }
  }
  redcap_api_request(url = url, token = token, additional_args = additional_args) %>%
    process_redcap_response(error_action = error_action, method = method, show_method_help = show_method_help) %>%
    return()
}
#' @noRd
get_REDCap_method <- function(project, method, error_action = "warn", additional_args = NULL, show_method_help = TRUE) {
  return(
    run_redcap_api_method(
      url = project$links$redcap_uri,
      token = assert_REDCap_token(project),
      method = method,
      additional_args = additional_args,
      error_action = error_action,
      only_get = TRUE,
      show_method_help = show_method_help
    )
  )
}
#' @noRd
get_REDCap_metadata <- function(project, include_users = TRUE) {
  project$internals$last_metadata_update <- Sys.time()
  project$metadata <- list()
  # info ----------
  project$redcap$project_info <- get_REDCap_method(project, method = "exp_proj")
  project$redcap$project_title <- project$redcap$project_info$project_title
  project$redcap$project_id <- project$redcap$project_info$project_id
  project$redcap$is_longitudinal <- project$redcap$project_info$is_longitudinal == "1"
  project$metadata$missing_codes <- missing_codes2(project)
  # instruments --------
  project$metadata$forms <- get_REDCap_method(project, method = "exp_instr") %>% rename_forms_redcap_to_default()
  project$metadata$forms$repeating <- FALSE
  project$redcap$has_repeating_forms <- FALSE
  project$redcap$has_repeating_events <- FALSE
  project$redcap$has_repeating_forms_or_events <- project$redcap$project_info$has_repeating_instruments_or_events == "1"
  # if(project$redcap$project_info$has_repeating_instruments_or_events=="1")
  repeatingFormsEvents <- get_REDCap_method(project, method = "exp_repeating_forms_events")
  if (is.data.frame(repeatingFormsEvents)) {
    project$metadata$forms$repeating <- project$metadata$forms$form_name %in% repeatingFormsEvents$form_name
  }
  if (any(project$metadata$forms$repeating)) {
    project$redcap$has_repeating_forms <- TRUE
  }
  # metadata ----------
  project$metadata$fields <- get_REDCap_method(project, method = "exp_metadata", error_action = "stop")
  project$metadata$fields$section_header <- project$metadata$fields$section_header %>% remove_html_tags()
  project$metadata$fields$field_label <- project$metadata$fields$field_label %>% remove_html_tags()
  project$redcap$id_col <- project$metadata$fields[1, 1] %>% as.character() # RISKY?
  project$metadata$form_key_cols <- get_key_col_list(project)
  project$redcap$raw_structure_cols <- project$metadata$form_key_cols %>%
    unlist() %>%
    unique()
  form_names <- project$metadata$forms$form_name # [which(project$metadata$forms$form_name%in%unique(project$metadata$fields$form_name))]
  for (form_name in form_names) {
    new_row <- data.frame(
      field_name = paste0(form_name, "_complete"),
      form_name = form_name,
      field_type = "radio",
      field_label = paste0(form_name, "_complete") %>% strsplit("_") %>% unlist() %>% stringr::str_to_title() %>% paste0(collapse = " "),
      select_choices_or_calculations = "0, Incomplete | 1, Unverified | 2, Complete"
    )
    last_row <- nrow(project$metadata$fields)
    rows <- which(project$metadata$fields$form_name == form_name)
    if (length(rows) == 0) rows <- last_row
    row <- dplyr::last(rows)
    top <- project$metadata$fields[1:row, ]
    bottom <- NULL
    if (last_row > row) {
      bottom <- project$metadata$fields[(row + 1):last_row, ]
    }
    project$metadata$fields <- top %>%
      dplyr::bind_rows(
        new_row
      ) %>%
      dplyr::bind_rows(
        bottom
      )
  }
  if (any(project$metadata$fields$field_type == "checkbox")) {
    for (field_name in project$metadata$fields$field_name[which(project$metadata$fields$field_type == "checkbox")]) {
      x <- project$metadata$fields$select_choices_or_calculations[which(project$metadata$fields$field_name == field_name)] %>% split_choices()
      new_rows <- data.frame(
        field_name = paste0(field_name, "___", x$code),
        form_name = project$metadata$fields$form_name[which(project$metadata$fields$field_name == field_name)],
        field_label = x$name,
        # field_label_full=paste0(project$metadata$fields$field_label[which(project$metadata$fields$field_name==field_name)]," - ",x$name),
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
        dplyr::bind_rows(
          new_rows
        ) %>%
        dplyr::bind_rows(
          bottom
        )
    }
  }
  if (any(project$metadata$fields$field_type == "yesno")) {
    project$metadata$fields$select_choices_or_calculations[which(project$metadata$fields$field_type == "yesno")] <- c("0, No | 1, Yes")
  }
  project$metadata$fields <- project %>% annotate_fields(summarize_data = FALSE)
  project$metadata$choices <- fields_to_choices(fields = project$metadata$fields)
  # is longitudinal ------
  if (project$redcap$is_longitudinal) {
    project$redcap$raw_structure_cols <- c(project$redcap$raw_structure_cols, "arm_num", "event_name") %>% unique()
    project$metadata$arms <- get_REDCap_method(project, method = "exp_arms")
    project$redcap$has_arms <- TRUE
    project$redcap$has_multiple_arms <- nrow(project$metadata$arms) > 1
    project$redcap$has_arms_that_matter <- project$redcap$has_multiple_arms
    project$metadata$event_mapping <- get_REDCap_method(project, method = "exp_inst_event_maps")
    project$metadata$events <- get_REDCap_method(project, method = "exp_events")
    project$metadata$events$repeating <- FALSE
    project$metadata$event_mapping$repeating <- FALSE
    if (is.data.frame(repeatingFormsEvents)) {
      project$metadata$events$repeating <- project$metadata$events$unique_event_name %in% repeatingFormsEvents$event_name[which(is.na(repeatingFormsEvents$form_name))]
      repeatingFormsEvents_ind <- repeatingFormsEvents[which(!is.na(repeatingFormsEvents$event_name) & !is.na(repeatingFormsEvents$form_name)), ]
      if (nrow(repeatingFormsEvents_ind) > 0) {
        rows_event_mapping <- seq_len(nrow(repeatingFormsEvents_ind)) %>%
          lapply(function(i) {
            which(project$metadata$event_mapping$unique_event_name == repeatingFormsEvents_ind$event_name[i] & project$metadata$event_mapping$form == repeatingFormsEvents_ind$form_name[i])
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
      project$redcap$has_arms_that_matter <- project$metadata$arms$arm_num %>%
        lapply(function(arm) {
          project$metadata$event_mapping$form[which(project$metadata$event_mapping$arm_num == arm)]
        }) %>%
        check_match() %>%
        magrittr::not()
    }
    # if(is.data.frame(project$unique_events)){
    #   project$metadata$events <- data.frame(
    #     event_name = unique(project$unique_events$event_name),
    #     arms = unique(project$unique_events$event_name) %>% lapply(function(event_name){
    #       project$unique_events$arm_num[which(project$unique_events$event_name==event_name)] %>% unique() %>% paste0(collapse = " | ")
    #     })
    #   )
    # }
    project$metadata$forms$repeating_via_events <- FALSE
    project$metadata$forms$repeating_via_events[
      which(
        project$metadata$forms$form_name %>% lapply(function(form_name) {
          # form_name <- forms$form_name %>% sample(1)
          anyDuplicated(project$metadata$event_mapping$arm_num[which(project$metadata$event_mapping$form == form_name)]) > 0
        }) %>% unlist()
      )
    ] <- TRUE
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
    project$redcap$log <- get_REDCap_log(project, last = 2, units = "mins")
    project$redcap$users$current_user <- project$redcap$users$username == project$redcap$log$username[which(project$redcap$log$details == "Export REDCap version (API)") %>% dplyr::first()]
  }
  project$links$redcap_home <- paste0(project$links$redcap_base, "redcap_v", project$redcap$version, "/index.php?pid=", project$redcap$project_id)
  project$links$redcap_record_home <- paste0(project$links$redcap_base, "redcap_v", project$redcap$version, "/DataEntry/record_home.php?pid=", project$redcap$project_id)
  project$links$redcap_record_subpage <- paste0(project$links$redcap_base, "redcap_v", project$redcap$version, "/DataEntry/index.php?pid=", project$redcap$project_id)
  project$links$redcap_records_dashboard <- paste0(project$links$redcap_base, "redcap_v", project$redcap$version, "/DataEntry/record_status_dashboard.php?pid=", project$redcap$project_id)
  project$links$redcap_API <- paste0(project$links$redcap_base, "redcap_v", project$redcap$version, "/API/project_api.php?pid=", project$redcap$project_id)
  project$links$redcap_API_playground <- paste0(project$links$redcap_base, "redcap_v", project$redcap$version, "/API/playground.php?pid=", project$redcap$project_id)
  return(project)
}
#' @noRd
get_REDCap_version <- function(project, show_method_help = TRUE) {
  return(
    get_REDCap_method(
      project = project,
      method = "exp_rc_v",
      show_method_help = show_method_help
    )
  )
}
#' @noRd
get_REDCap_files <- function(project, original_file_names = FALSE, overwrite = FALSE) {
  file_rows <- which(project$metadata$fields$field_type == "file")
  out_dir <- file.path(project$dir_path, "REDCap", project$short_name, "files")
  if (length(file_rows) > 0) {
    dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
    for (field_name in project$metadata$fields$field_name[file_rows]) {
      out_dir_folder <- file.path(out_dir, field_name)
      dir.create(out_dir_folder, showWarnings = FALSE, recursive = TRUE)
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
            warning(paste0("You have duplicate file names in ", form_name, ", ", field_name, ". Therefore will use system generated names"), immediate. = TRUE)
            original_file_names <- FALSE
          }
        }
        file_name <- ifelse(original_file_names, file_name, paste0(form_name, "_", field_name, "_", ifelse(is_repeating, "inst_", ""), repeat_instance, "ID_", record_id, ".", tools::file_ext(file_name)))
        if (!file.exists(file.path(out_dir_folder, file_name)) || overwrite) {
          REDCapR::redcap_file_download_oneshot(
            redcap_uri = project$links$redcap_uri,
            token = assert_REDCap_token(project),
            field = field_name,
            record = form[[project$redcap$id_col]][i],
            directory = out_dir_folder,
            file_name = file_name,
            event = redcap_event_name,
            repeat_instance = repeat_instance,
            verbose = FALSE
          )
          bullet_in_console(paste0("`", file_name, "` saved."), bullet_type = ">")
        }
      }
    }
  }
  bullet_in_console("Checked for files! Stored at ...", file = out_dir, bullet_type = "v")
}
#' @noRd
get_REDCap_users <- function(project) {
  users <- get_REDCap_method(project, method = "exp_users")
  userRole <- get_REDCap_method(project, method = "exp_user_roles") %>% dplyr::select("unique_role_name", "role_label")
  userRoleMapping <- get_REDCap_method(project, method = "exp_user_role_maps")
  final <- merge(merge(userRole, userRoleMapping, by = "unique_role_name"), users, by = "username", all.y = TRUE)
  return(final)
}
#' @noRd
get_REDCap_structure <- function(project) {
  get_REDCap_metadata(project, include_users = FALSE)
}
#' @noRd
get_REDCap_log <- function(project, last = 24, user = "", units = "hours", begin_time = "", clean = TRUE, record = "") {
  if (units == "days") {
    x <- (Sys.time() - lubridate::days(last)) %>%
      format("%Y-%m-%d %H:%M") %>%
      as.character()
  }
  if (units == "hours") {
    x <- (Sys.time() - lubridate::hours(last)) %>%
      format("%Y-%m-%d %H:%M") %>%
      as.character()
  }
  if (units == "mins") {
    x <- (Sys.time() - lubridate::minutes(last)) %>%
      format("%Y-%m-%d %H:%M") %>%
      as.character()
  }
  if (units == "years") {
    x <- (Sys.time() - lubridate::years(last)) %>%
      format("%Y-%m-%d %H:%M") %>%
      as.character()
  }
  if (begin_time != "") {
    x <- begin_time
  }
  log <- get_REDCap_method(project, "exp_logging", additional_args = list(beginTime = x, record = record, user = user))
  if (is.data.frame(log)) {
    if (clean) {
      log <- log %>% clean_redcap_log()
    }
  }
  log # deal with if NA if user does not have log privileges.
}
#' @noRd
get_REDCap_raw_data <- function(project, labelled = FALSE, records = NULL, batch_size = 1000) {
  raw <- REDCapR::redcap_read(
    redcap_uri = project$links$redcap_uri,
    token = assert_REDCap_token(project),
    # forms = forms,
    # events = events,
    batch_size = batch_size,
    interbatch_delay = 0.1,
    records = records,
    raw_or_label = ifelse(labelled, "label", "raw")
  )$data %>% all_character_cols()
  return(raw)
}
#' @title Delete Records from REDCap
#' @description
#' This function deletes one or more records from a REDCap project using the REDCap API. It sends a `delete` action request for each specified record and ensures that only records present in the database are processed.
#'
#' @inheritParams save_project
#' @param records A character vector of record IDs to be deleted from the REDCap project.
#' @return NULL. The function does not return a value but will print a message confirming deletion.
#'
#' @details
#' The function checks whether the records to be deleted are included in the `project$summary$all_records` list. If any records are not found, an error is thrown. Otherwise, the function proceeds to delete each specified record from the REDCap project via the REDCap API.
#' The `delete` action is executed using the `httr::POST` method, sending the necessary request to the REDCap server.
#'
#' @export
delete_REDCap_records <- function(project, records) {
  BAD <- records[which(!records %in% project$summary$all_records[[project$redcap$id_col]])]
  if (length(BAD) > 0) stop("Records not included in project: ", records %>% paste0(collapse = ", "))
  for (record in records) {
    httr::POST(
      url = project$links$redcap_uri,
      body = list(
        "token" = assert_REDCap_token(project),
        content = "record",
        action = "delete",
        `records[0]` = record,
        returnFormat = "json"
      ),
      encode = "form"
    ) %>% process_redcap_response(error_action = "warn")
  }
  message("Records deleted!")
}
#' @noRd
show_REDCap_API_methods <- function() {
  show_REDCap_API_methods_table()[["method_short_name"]] %>% sort()
}
#' @noRd
show_REDCap_API_methods_table <- function() {
  return(REDCap_API$methods)
}
#' @noRd
show_redcap_api_method_info <- function(method) {
  if (missing(method)) {
    bullet_in_console("Missing method, so one will be chosen at random!", bullet_type = "x")
    method <- REDCap_API$methods$method_short_name %>% sample1()
  }
  if (!method %in% REDCap_API$methods$method_short_name) stop("Must use the following methods... ", as_comma_string(REDCap_API$methods$method_short_name))
  method_df <- REDCap_API$methods[which(REDCap_API$methods$method_short_name == method), ]
  method_param_df <- REDCap_API$params[which(REDCap_API$params$method_short_name == method), ]
  method_param_df_base <- method_param_df[which(!method_param_df$non_base), ]
  method_param_df_req <- method_param_df[which(method_param_df$non_base_req_by_user), ]
  method_param_df_opt <- method_param_df[which(method_param_df$non_base_opt_by_user), ]
  bullet_in_console(paste0("REDCap API method '", method, "'..."))
  bullet_in_console("Help is here: ", url = method_df$help_url)
  bullet_in_console(paste0(method_df$description))
  if (nrow(method_param_df_base) > 0) {
    bullet_in_console(paste0("Default: ", method_param_df_base$Parameter %>% as_comma_string()), bullet_type = "v")
  }
  if (nrow(method_param_df_req) > 0) {
    x <- method_param_df_req$Parameter
    x[which(method_param_df_req$is_longitudinal_only)] <- paste0(x[which(method_param_df_req$is_longitudinal_only)], " (if longitudinal)")
    x[which(method_param_df_req$has_repeating_instruments_or_events_only)] <- paste0(x[which(method_param_df_req$has_repeating_instruments_or_events_only)], " (if has repeating)")
    bullet_in_console(paste0("Required by User: ", as_comma_string(x)), bullet_type = ">")
  }
  if (nrow(method_param_df_opt) > 0) {
    bullet_in_console(paste0("Optional by User: ", method_param_df_opt$Parameter %>% as_comma_string()), bullet_type = ">")
  }
}
#' @noRd
rename_forms_redcap_to_default <- function(forms) {
  the_names <- colnames(forms)
  the_names[which(the_names == "instrument_name")] <- "form_name"
  the_names[which(the_names == "instrument_label")] <- "form_label"
  colnames(forms) <- the_names
  return(forms)
}
#' @noRd
rename_forms_default_to_redcap <- function(forms) {
  the_names <- colnames(forms)
  the_names[which(the_names == "form_name")] <- "instrument_name"
  the_names[which(the_names == "form_label")] <- "instrument_label"
  colnames(forms) <- the_names
  return(forms)
}
