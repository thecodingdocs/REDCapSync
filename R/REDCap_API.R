#' @noRd
get_redcap_metadata <- function(project, include_users = TRUE) {
  assert_setup_project(project)
  result <- rcon_result(project)
  project$metadata <- list()
  project$redcap$project_info <- result$project_info
  # info ----------
  project$redcap$project_title <- project$redcap$project_info$project_title
  project$redcap$project_id <-
    as.character(project$redcap$project_info$project_id)
  is_longitudinal <- project$redcap$project_info$is_longitudinal == "1"
  project$metadata$is_longitudinal <- is_longitudinal
  missing_data_codes <- NA
  if ("missing_data_codes" %in% colnames(project$redcap$project_info)) {
    missing_data_codes <- project$redcap$project_info$missing_data_codes
    if (!is.na(missing_data_codes)) {
      missing_data_codes <- split_choices(missing_data_codes)
    }
  }
  project$metadata$missing_codes <- missing_data_codes
  project$redcap$has_log_access <- !is.null(result$logging)
  project$redcap$has_dag_access <- !is.null(result$dags)
  project$redcap$has_user_access <- !is.null(result$users)
  project$redcap$has_file_repository_access <- !is.null(result$file_repository)
  # instruments --------
  project$metadata$forms <-
    rename(
      result$forms,
      form_name  = "instrument_name",
      form_label = "instrument_label"
    )
  project$metadata$repeating_forms_events <- result$repeating
  project$metadata$forms$repeating <- FALSE
  project$metadata$has_repeating_forms <- FALSE
  project$metadata$has_repeating_events <- FALSE
  has_repeat_forms_or_events <-
    project$redcap$project_info$has_repeating_instruments_or_events == "1"
  project$metadata$has_repeating_forms_or_events <- has_repeat_forms_or_events
  # if(project$redcap$project_info$has_repeating_instruments_or_events=="1")
  if (is.data.frame(project$metadata$repeating_forms_events)) {
    #  test if you can do this if you dont have designer privileges
    # or would have to use another package
    if (nrow(project$metadata$repeating_forms_events) > 0L) {
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
  section_header <- project$metadata$fields$section_header
  project$metadata$fields$section_header <- remove_html_tags(section_header)
  field_label <- project$metadata$fields$field_label
  project$metadata$fields$field_label <- remove_html_tags(field_label)
  # RISKY? add to setup project and simple test of unique
  project$metadata$id_col <- as.character(project$metadata$fields[1L, 1L])
  project$metadata$form_key_cols <- get_key_col_list(data_list = project)
  form_key_cols <- project$metadata$form_key_cols
  project$metadata$raw_structure_cols <- form_key_cols |> unlist() |> unique()
  project$metadata$fields <- add_field_elements(project$metadata$fields)
  project$metadata$choices <- fields_to_choices(project$metadata$fields)
  # add a check for existing conflict possibilities
  project$metadata$has_coding_conflicts <- FALSE
  field_names <- unique(project$metadata$choices$field_name)
  if (length(field_names) > 0L) {
    choices <- project$metadata$choices
    has_conflict <- field_names |>
      lapply(function(field_name) {
        anyDuplicated(
          choices$name[which(choices$field_name == field_name)]) > 0L
      }) |>
      unlist()
    project$metadata$has_coding_conflicts <- any(has_conflict)
    if (project$metadata$has_coding_conflicts) {
      conflict_rows <- which(has_conflict)
      project$metadata$coding_conflict_field_names <- field_names[conflict_rows]
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
    project$metadata$has_multiple_arms <- nrow(project$metadata$arms) > 1L
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
      if (nrow(repeatingFormsEvents_ind) > 0L) {
        rows_event_mapping <- seq_len(nrow(repeatingFormsEvents_ind)) |>
          lapply(function(i) {
            which(
              project$metadata$event_mapping$unique_event_name ==
                repeatingFormsEvents_ind$event_name[i] &
                project$metadata$event_mapping$form ==
                repeatingFormsEvents_ind$form_name[i]
            )
          }) |>
          unlist()
        project$metadata$event_mapping$repeating[rows_event_mapping] <- TRUE
      }
    }
    project$metadata$events$forms <-
      project$metadata$events$unique_event_name |>
      lapply(function(events) {
        project$metadata$event_mapping$form[
          which(project$metadata$event_mapping$unique_event_name == events)] |>
          unique() |>
          paste0(collapse = " | ")
      }) |>
      unlist()
    if (project$metadata$has_arms_that_matter) {
      project$metadata$has_arms_that_matter <-
        !(project$metadata$arms$arm_number |>
            lapply(function(arm) {
              project$metadata$event_mapping$form[
                which(project$metadata$event_mapping$arm_number == arm)]
            }) |>
            check_match())
    }
    project$metadata$forms$repeating_via_events <- FALSE
    project$metadata$forms$repeating_via_events[
      which(
        unlist(lapply(project$metadata$forms$form_name, function(form_name) {
          anyDuplicated(
            project$metadata$event_mapping$arm_num[
              which(project$metadata$event_mapping$form == form_name)]) > 0L
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
  has_users <- is_something(result$users)
  if (include_users && project$redcap$has_user_access && has_users) {
    keep_cols <- c("unique_role_name", "role_label")
    project$redcap$users <- result$user_roles[, keep_cols] |>
      merge(result$user_role_assignment, by = "unique_role_name") |>
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
rcon_result <- function(project) {
  rcon <- redcapConnection(url = project$links$redcap_uri,
                           token = get_project_token(project))
  result <- list(
    project_info = try_else_null(all_character_cols(rcon$projectInformation())),
    arms = try_else_null(all_character_cols(rcon$arms())),
    events = try_else_null(all_character_cols(rcon$events())),
    mapping = try_else_null(all_character_cols(rcon$mapping())),
    forms = try_else_null(all_character_cols(rcon$instruments())),
    repeating = try_else_null(all_character_cols(rcon$repeatInstrumentEvent())),
    fields = try_else_null(all_character_cols(rcon$metadata())),
    logging = try_else_null(all_character_cols(
      exportLogging(rcon = rcon, beginTime = Sys.time() - 100000L)
    )),
    users = try_else_null(all_character_cols(rcon$users())),
    user_roles = try_else_null(all_character_cols(rcon$user_roles())),
    user_role_assignment = try_else_null(all_character_cols(
      rcon$user_role_assignment()
    )),
    dags = try_else_null(all_character_cols(rcon$dags())),
    dag_assignment = try_else_null(all_character_cols(rcon$dag_assignment())),
    file_repository = try_else_null(all_character_cols(rcon$file_repository()))
  )
  result
}
#' @noRd
add_field_elements <- function(fields) {
  #assert_fields should be here
  form_names <- unique(fields$form_name)
  for (form_name in form_names) {
    field_name <- paste0(form_name, "_complete")
    field_label <- field_name |>
      strsplit("_") |>
      unlist() |>
      str_to_title() |>
      paste(collapse = " ")
    new_row <- data.frame(
      field_name = field_name, # check if conflicts,
      form_name = form_name,
      field_type = "radio",
      field_label = field_label,
      select_choices_or_calculations =
        "0, Incomplete | 1, Unverified | 2, Complete",
      stringsAsFactors = FALSE
    )
    last_row <- nrow(fields)
    row_index <- which(fields$form_name == form_name)
    if (length(row_index) == 0L) {
      row_index <- last_row
    }
    x_row <- last(row_index)
    top <- fields[1L:x_row, ]
    bottom <- NULL
    if (last_row > x_row) {
      bottom <- fields[(x_row + 1L):last_row, ]
    }
    fields <- top |>
      bind_rows(new_row) |>
      bind_rows(bottom)
  }
  if (any(fields$field_type == "checkbox")) {
    for (field_name in fields$field_name[
      which(fields$field_type == "checkbox")]) {
      x <- fields$select_choices_or_calculations[
        which(fields$field_name == field_name)] |> split_choices()
      new_rows <- data.frame(
        field_name = paste0(field_name, "___", x$code),
        form_name = fields$form_name[which(fields$field_name == field_name)],
        field_label = x$name,
        field_type = "checkbox_choice",
        select_choices_or_calculations = "1, Checked | 0, Unchecked",
        stringsAsFactors = FALSE
      )
      x_row <- which(fields$field_name == field_name)
      last_row <- nrow(fields)
      top <- fields[1L:x_row, ]
      bottom <- NULL
      if (last_row > x_row) {
        bottom <- fields[(x_row + 1L):last_row, ]
      }
      fields <- top |>
        bind_rows(new_rows) |>
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
get_redcap_data <- function(project,
                            labelled = TRUE,
                            records = NULL,
                            batch_size = 2000L) {
  form_list <- list()
  denormalized <- get_redcap_denormalized(
    project = project,
    labelled = FALSE,
    records = records,
    batch_size = batch_size
  ) # add check for dag and api
  form_list <- normalize_redcap(denormalized = denormalized,
                                project = project,
                                labelled = labelled)
  form_list
}
#' @noRd
get_redcap_denormalized <- function(project,
                                    labelled = FALSE,
                                    records = NULL,
                                    batch_size = 1000L) {
  denormalized <- redcap_read(
    redcap_uri = project$links$redcap_uri,
    token = get_project_token(project),
    batch_size = batch_size,
    interbatch_delay = 0.1,
    records = records,
    raw_or_label = ifelse(labelled, "label", "raw")
  )$data
  denormalized <- all_character_cols(denormalized)
  denormalized
}
#' @noRd
get_redcap_files <- function(project,
                             original_file_names = FALSE,
                             overwrite = FALSE) {
  file_rows <- which(project$metadata$fields$field_type == "file")
  project_name <- project$project_name
  out_dir <- file.path(project$dir_path, "REDCap", project_name, "files")
  if (length(file_rows) > 0L) {
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
        repeat_instance <- ifelse(
          is.null(repeat_instance), "", repeat_instance)
        redcap_event_name <- form[["redcap_event_name"]][i]
        redcap_event_name <- ifelse(
          is.null(redcap_event_name), "", repeat_instance)
        if (!original_file_names) {
          if (anyDuplicated(file_name) > 0L) {
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
        out_file_path <- file.path(out_dir_folder, file_name)
        if (!file.exists(out_file_path) || overwrite) {
          #try catch?
          redcap_file_download_oneshot(
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
          succeeded <- file.exists(out_file_path)
          if (succeeded) {
            cli_alert_wrap(text = paste0("`", file_name, "` saved."),
                           bullet_type = ">")
          } else {
            cli_alert_wrap(text = paste0("`", file_name, "` failed to save!"),
                           bullet_type = "x")
          }
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
                           log_begin_date = Sys.Date() - 10L) {
  assert_setup_project(project)
  assert_date(log_begin_date)
  log_begin_time <- as.character(log_begin_date) |>
    paste("00:00:00") |>
    as.POSIXct()
  redcap_log <- exportLogging(
    rcon = redcapConnection(url = project$links$redcap_uri,
                            token = get_project_token(project)),
    logtype = character(0L),
    user = character(0L),
    record = character(0L),
    beginTime = log_begin_time,
    endTime = as.POSIXct(character(0L))
  )
  #addtrycatch NULL
  if (is.null(redcap_log)) {
    return(NULL)
  }
  if (is.data.frame(redcap_log)) {
    if (nrow(redcap_log) > 0L) {
      redcap_log <- clean_redcap_log(redcap_log)
    }
  }
  redcap_log # deal with if NA if user does not have log privileges.
}
#' @noRd
get_redcap_records <- function(project) {
  assert_setup_project(project)
  id_col <- project$metadata$id_col
  records <- suppressMessages({
    redcap_read_oneshot(
      redcap_uri = project$links$redcap_uri,
      token = get_project_token(project),
      fields = id_col,
      raw_or_label = "raw"
    )
  })
  # consider error check
  records <- records$data[[id_col]] |> unique() |> as.character()
  records
}
#' @noRd
upload_form_to_redcap <- function(to_be_uploaded, project, batch_size = 500L) {
  redcap_write(
    ds_to_write = as_tibble(all_character_cols(to_be_uploaded)),
    batch_size = batch_size,
    interbatch_delay = 0.2,
    continue_on_error = FALSE,
    redcap_uri = project$links$redcap_uri,
    token = get_project_token(project),
    overwrite_with_blanks = TRUE
  )
}
#' @noRd
.log_colnames <- c(
  "timestamp",
  "username",
  "action",
  "details",
  "record",
  "action_type"
)
#' @noRd
.users_colnames <- c(
  "username",
  "email",
  "firstname",
  "lastname",
  "expiration",
  "data_access_group",
  "data_access_group_id",
  "design",
  "alerts",
  "user_rights",
  "data_access_groups",
  "reports",
  "stats_and_charts",
  "manage_survey_participants",
  "calendar",
  "data_import_tool",
  "data_comparison_tool",
  "logging",
  "file_repository",
  "data_quality_create",
  "data_quality_execute",
  "api_export",
  "api_import",
  "api_modules",
  "mobile_app",
  "mobile_app_download_data",
  "record_create",
  "record_rename",
  "record_delete",
  "lock_records_all_forms",
  "lock_records",
  "lock_records_customization",
  "forms_export"
)
#' @noRd
.field_colnames <- c(
  "field_name",
  "form_name",
  "section_header",
  "field_type",
  "field_label",
  "select_choices_or_calculations",
  "field_note",
  "text_validation_type_or_show_slider_number",
  "text_validation_min",
  "text_validation_max",
  "identifier",
  "branching_logic",
  "required_field",
  "custom_alignment",
  "question_number",
  "matrix_group_name",
  "matrix_ranking",
  "field_annotation"
)
#' @noRd
.arms_colnames <- c(
  "arm_number",
  "arm_name"
)
#' @noRd
.events_colnames <- c(
  "event_name",
  "arm_number",
  "unique_event_name",
  "custom_event_label",
  "event_id"
)
#' @noRd
.event_mapping_colnames <- c(
  "arm_num",
  "unique_event_name",
  "form"
)
#' @noRd
.forms_colnames <- c(
  "form_name",
  "form_label"
)
.choices_colnames <- c(
  "field_name",
  "code",
  "name"
)
#' @noRd
.project_info_colnames <- c(
  "project_id",
  "project_title",
  "creation_time",
  "production_time",
  "in_production",
  "project_language",
  "purpose",
  "purpose_other",
  "project_notes",
  "custom_record_label",
  "secondary_unique_field",
  "is_longitudinal",
  "has_repeating_instruments_or_events",
  "surveys_enabled",
  "scheduling_enabled",
  "record_autonumbering_enabled",
  "randomization_enabled",
  "ddp_enabled",
  "project_irb_number",
  "project_grant_number",
  "project_pi_firstname",
  "project_pi_lastname",
  "display_today_now_button",
  "missing_data_codes",
  "external_modules",
  "bypass_branching_erase_field_prompt"
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
