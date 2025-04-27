deidentify_data_list <- function(data_list,
                                 identifiers = NULL,
                                 date_handling = "none",
                                 exclude_free_text = FALSE) {
  # assert_data_list contains data and metadata with forms and fields
  data <- data_list$data
  metadata <- data_list$metadata
  fields <- metadata$fields
  initial_identifiers <- fields$field_name[
    which(fields$identifier == "y")
  ]
  if (length(initial_identifiers) == 0L) {
    warning(
      "You have no identifiers marked in ",
      "`project$metadata$fields$identifier`. ",
      "You can set it in REDCap Project Setup and update ",
      "project OR define your idenitifiers in this functions ",
      "`identifiers` argument."
      ,
      immediate. = TRUE
    )
  }
  identifiers <- initial_identifiers %>% append(identifiers) %>% unique()
  bad_identifiers <- identifiers[
    which(
      !identifiers %in% fields$field_name
    )
  ]
  if (length(bad_identifiers) > 0L) {
    stop(
      "There is a bad identifier. see `fields$field_name`: ",
      toString(bad_identifiers)
    )
  }
  id_cols <- metadata$form_key_cols %>% unlist() %>% unique()
  if (is_something(id_cols)) {
    if (any(id_cols %in% identifiers)) {
      stop(
        "ID cols not allowed... ",
        toString(id_cols),
        " <-- use hashing (in dev)"
      )
    }
  }
  if (exclude_free_text) { # placeholder
    #drop free text only if there is no validation
    #make function for that ?external
    free_text_rows <- which(
      fields$field_type == "notes" |
        (
          fields$field_type == "text" &
            is.na(fields$text_validation_type_or_show_slider_number)
        ) &
        !fields$field_name %in% id_cols
    )
    identifiers <- identifiers %>%
      append(fields$field_name[free_text_rows]) %>%
      unique()
  }
  if (is_something(data)) {
    date_vector <- fields$field_name[which(fields$field_type_R == "date")]
    # records <- lapply(data,function(form){
    #   form[[id_cols[1]]]
    # }) %>% unlist() %>% unique()
    date_list <- Map(
      f = function(x, cols) {
        date_vector[which(date_vector %in% cols)]
      },
      names(data),
      lapply(data, colnames)
    )
    if (date_handling != "none") {
      min_dates <- get_min_dates(data_list)
      # if(date_handling == "lowest-record-zero"){
      #
      # }
      min_dates$difference <- (min_dates$date - as.Date(date_handling))
      for (form_name in names(date_list)){
        field_record <- data[[form_name]][[id_cols[1]]]
        match_date_diff <- match(field_record, min_dates$record_id)
        difference <- min_dates$difference[match_date_diff]
        for (field_name in date_list[[form_name]]) {
          field <- data[[form_name]][[field_name]]
          data[[form_name]][[field_name]] <- as.Date(field) - difference
        }
      }
      #if you have dates you already mutated no need to drop anymore
      identifiers <- identifiers[which(!identifiers %in% date_vector)]
    }
    drop_list <- Map(function(x, cols) {
      identifiers[which(identifiers %in% cols)]
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
get_min_dates <- function(data_list) {
  # assert_data_list contains data and metadata with forms and fields
  data <- data_list$data
  metadata <- data_list$metadata
  fields <- metadata$fields
  id_cols <- metadata$form_key_cols %>% unlist() %>% unique()
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
        df_long <- stats::reshape(df_subset, varying = existing_fields,
                                  v.names = "date", times = existing_fields,
                                  timevar = "field", direction = "long")
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
#' @return Nothing will be returned in R. Instead, a browser link
#' @family Link Functions
#' @export
link_API_token <- function(project) {
  utils::browseURL(project$links$redcap_API)
}
#' @rdname Links
#' @export
link_API_playground <- function(project) {
  utils::browseURL(project$links$redcap_API_playground)
}
#' @rdname Links
#' @export
link_REDCap_home <- function(project) {
  utils::browseURL(project$links$redcap_base)
}
#' @rdname Links
#' @export
link_REDCap_project <- function(project) {
  utils::browseURL(project$links$redcap_home)
}
#' @param record REDCap record id or study id etc, any column names that match
#' `project$redcap$id_col`
#' @param page REDCap page for the record. Must be one of
#' `project$metadata$forms$form_name`
#' @param instance REDCap instance if it's a repeating instrument
#' @param text_only logical for only returning text
#' @rdname Links
#' @export
link_REDCap_record <- function(project,
                               record,
                               page,
                               instance,
                               text_only = FALSE) {
  #FIX
  link <- paste0(
    project$links$redcap_base,
    "redcap_v",
    project$redcap$version,
    "/DataEntry/record_home.php?pid=",
    project$redcap$project_id
  )
  id_col <- project$redcap$id_col
  if (!missing(record)) {
    if (!record %in% project$summary$all_records[[id_col]]) {
      stop(record, " is not one of the records inside project")
    }
    if ("arm_number" %in% colnames(project$summary$all_records)) {
      arm_row <- which(project$summary$all_records[[id_col]] == record)
      link <- paste0(link, "&arm=", project$summary$all_records$arm_number[arm_row])
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
        which(project$metadata$forms$repeating)
      ]
      if (!page %in% repeating_form_names) {
        stop(
          "If you provide an instance, it has to be a repeating instrument: ",
          repeating_form_names %>% toString()
        )
      }
      link <- link %>% paste0("&instance=", instance)
    }
  }
  if (text_only) {
    return(link)
  }
  utils::browseURL(link)
}
#' @noRd
construct_key_col_list <- function(project) {
  form_list <- project$data
  data_field_list <- form_list %>% lapply(colnames)
  form_names <- names(form_list)
  key_cols_list <- form_names %>% lapply(function(form_name) {
    key_cols <- which(
      data_field_list[[form_name]] %in% project$redcap$raw_structure_cols
    )
    data_field_list[[form_name]][key_cols]
  })
  names(key_cols_list) <- form_names
  key_cols_list
}
#' @noRd
get_key_col_list <- function(project, transform = FALSE) {
  forms <- project$metadata$forms
  if (transform) {
    forms <- project$transformation$metadata$forms
  }
  if (!is_something(forms)) {
    stop("Empty --> `project$metadata$forms`")
  }
  out_list <- seq_len(nrow(forms)) %>% lapply(function(i) {
    out <- project$redcap$id_col
    if (project$redcap$is_longitudinal) out <- append(out, "redcap_event_name")
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
raw_process_redcap <- function(raw, project, labelled) {
  forms <- project$metadata$forms
  fields <- project$metadata$fields
  events <- project$metadata$events
  event_mapping <- project$metadata$event_mapping
  form_list <- list()
  if (nrow(raw) > 0) {
    raw <- raw %>% all_character_cols()
    add_ons <- c(
      project$redcap$id_col,
      "arm_number",
      "event_name",
      "redcap_event_name",
      "redcap_repeat_instrument",
      "redcap_repeat_instance"
    )
    if (project$redcap$is_longitudinal) {
      raw$id_temp <- seq_len(nrow(raw))
      raw <- merge(
        raw,
        events[, c("arm_number", "event_name", "unique_event_name")],
        by.x = "redcap_event_name",
        by.y = "unique_event_name",
        sort = FALSE,
        all.x = TRUE
      )
      add_ons <- add_ons[which(add_ons %in% colnames(raw))]
      cols <- c(add_ons, colnames(raw)) %>% unique()
      raw <- raw[
        order(raw$id_temp),
        cols %>% lapply(function(c) {
          which(colnames(raw) == c)
        }) %>%
          unlist() %>%
          as.integer()
      ]
      raw$id_temp <- NULL
    }
    add_ons <- add_ons[which(add_ons %in% colnames(raw))]
    if (!all(project$redcap$raw_structure_cols %in% colnames(raw))) {
      stop(
        "raw is missing one of the following... and that's weird: ",
        toString(project$redcap$raw_structure_cols)
      )
    }
    form_rows <- which(forms$form_name %in% unique(fields$form_name))
    form_names <- forms$form_name[form_rows]
    has_repeating_forms <- project$redcap$has_repeating_forms
    for (form_name in form_names) {
      form_field_names <- fields$field_name[which(
        fields$form_name == form_name &
          fields$field_name %in% colnames(raw) &
          fields$field_name != project$redcap$id_col
      )]
      if (length(form_field_names) == 0) {
        cli_alert_danger(
          paste0(
            "You might not have access to ",
            form_name,
            ". Unable to obtain."
          )
        )
      }
      if (length(form_field_names) > 0) {
        add_ons_x <- add_ons
        repeating_forms <- forms$form_name[which(forms$repeating)]
        is_repeating_form <- form_name %in% repeating_forms
        is_longitudinal <- project$redcap$is_longitudinal
        rows <- seq_len(nrow(raw))
        if (is_repeating_form) {
          if (!"redcap_repeat_instrument" %in% colnames(raw)) {
            stop("redcap_repeat_instrument not in colnames(raw)")
          }
          if (is_longitudinal) {
            rows <- which(
              raw$redcap_repeat_instrument == form_name |
                raw$redcap_event_name %in% event_mapping$unique_event_name[which(!event_mapping$repeating &
                                                                                   event_mapping$form == form_name)]
            )
          }
          if (!is_longitudinal) {
            rows <- which(raw$redcap_repeat_instrument == form_name)
          }
        }
        if (!is_repeating_form) {
          add_ons_x <- add_ons_x[which(!add_ons_x %in% c("redcap_repeat_instrument", "redcap_repeat_instance"))]
          if (is_longitudinal) {
            rows <- which(raw$redcap_event_name %in% unique(event_mapping$unique_event_name[which(event_mapping$form == form_name)]))
          }
          if (!is_longitudinal) {
            if (has_repeating_forms) rows <- which(is.na(raw$redcap_repeat_instrument))
          }
        }
        if (is_something(rows)) {
          cols <- unique(c(add_ons_x, form_field_names))
          raw_subset <- raw[rows, cols]
          if (labelled) {
            raw_subset <- raw_to_labelled_form(
              form = raw_subset,
              project = project
            )
          }
          form_list[[form_name]] <- raw_subset
        }
      }
    }
  }
  form_list
}
#' @noRd
sort_redcap_log <- function(log) {
  unique(log[order(log$timestamp, decreasing = TRUE), ])
}
#' @noRd
clean_redcap_log <- function(log) {
  log <- unique(log)
  log$record_id <- NA
  log$action_type <- NA
  log <- log %>%
    lapply(function(x) {
      x %>% trimws(whitespace = "[\\h\\v]")
    }) %>%
    as.data.frame()
  design_test <- log$action == "Manage/Design"
  design_rows <- which(design_test)
  not_design_rows <- which(!design_test)
  # notdesign action -----
  record_rows <- not_design_rows[dplyr::starts_with(match = .log_action_records, vars = log$action[not_design_rows])]
  log$record_id[record_rows] <- gsub(
    "Update record|Delete record|Create record|[:(:]API[:):]|Auto|calculation|Lock/Unlock Record | |[:):]|[:(:]",
    "",
    log$action[record_rows]
  )
  log$action_type[record_rows] <- log$action[record_rows] %>%
    strsplit(" ") %>%
    lapply(function(x) {
      x[[1]]
    }) %>%
    unlist()
  log$action_type[
    not_design_rows[
      dplyr::starts_with(
        match = .log_action_exports,
        vars = log$action[not_design_rows]
      )
    ]
  ] <- "Exports"
  log$action_type[
    not_design_rows[
      dplyr::starts_with(
        match = .log_action_users,
        vars = log$action[not_design_rows]
      )
    ]
  ] <- "Users"
  log$action_type[
    not_design_rows[
      dplyr::starts_with(
        match = .log_action_no_changes,
        vars = log$action[not_design_rows]
      )
    ]
  ] <- "No Changes"
  # design details  -------------------
  comment_rows <- design_rows[
    dplyr::starts_with(
      match = .log_details_comments,
      vars = log$details[design_rows]
    )
  ]
  log$record_id[comment_rows] <- stringr::str_extract(
    string = log$details[comment_rows],
    pattern = "(?<=Record: )[^,]+"
  )
  log$action_type[comment_rows] <- "Comment"
  log$action_type[
    design_rows[
      dplyr::starts_with(
        match = .log_details_exports,
        vars = log$details[design_rows]
      )
    ]
  ] <- "Exports"
  log$action_type[
    design_rows[
      dplyr::starts_with(
        match = .log_details_metadata_major,
        vars = log$details[design_rows]
      )
    ]
  ] <- "Metadata Change Major"
  log$action_type[
    design_rows[
      dplyr::starts_with(
        match = .log_details_metadata_minor,
        vars = log$details[design_rows]
      )
    ]
  ] <- "Metadata Change Minor"
  log$action_type[
    design_rows[
      dplyr::starts_with(
        match = .log_details_no_changes,
        vars = log$details[design_rows]
      )
    ]
  ] <- "No Changes"
  log$action_type[
    design_rows[
      dplyr::starts_with(
        match = .log_details_tokens,
        vars = log$details[design_rows]
      )
    ]
  ] <- "Tokens"
  log$action_type[
    design_rows[
      dplyr::starts_with(
        match = .log_details_repository,
        vars = log$details[design_rows]
      )
    ]
  ] <- "Repository"
  # end ------------
  rows <- which(is.na(log$record) & !is.na(log$record_id))
  log$record[rows] <- log$record_id[rows]
  rows <- which(!is.na(log$record) & is.na(log$record_id))
  log$action_type[rows] <- "Users"
  log$record_id <- NULL
  log$username[which(log$username == "[survey respondent]")] <- NA
  # add drop exports?
  sort_redcap_log(log)
}
#' @noRd
.log_action_exports <- c(
  "Data export",
  "Download uploaded "
)
#' @noRd
.log_details_exports <- c(
  "Export ",
  "Download "
)
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
.log_details_comments <- c(
  "Add field comment ",
  "Edit field comment ",
  "Delete field comment "
)
#' @noRd
.log_action_records <- c(
  "Update record ",
  "Delete record ",
  "Lock/Unlock Record ",
  "Create record "
)
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
.log_details_tokens <- c(
  "Create API token",
  "User regenerate own API token"
)
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
missing_codes2 <- function(project) {
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
