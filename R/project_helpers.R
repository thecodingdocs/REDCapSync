#' @title add REDCap ID to any dataframe using a ref_id
#' @description
#'  add REDCap ID to any dataframe using a ref_id
#' @inheritParams save_project
#' @param DF dataframe
#' @param ref_id column name that matches a REDCap variable name that could be
#' an ALT id such as MRN
#' @return original dataframe with REDCap id_col added as the first column
#' @export
add_ID_to_DF <- function(DF, project, ref_id) {
  if (!ref_id %in% project$metadata$fields$field_name) {
    stop("The ref_id not valid. Must be a REDCap raw colname")
  }
  form <- project$metadata$fields$form_name[
    which(project$metadata$fields$field_name == ref_id)
  ]
  id_col <- DF[[ref_id]] %>%
    lapply(function(ID) {
      project$data[[form]][[project$redcap$id_col]][
        which(project$data[[form]][[ref_id]] == ID)
      ]
    }) %>%
    unlist() %>%
    as.data.frame()
  colnames(id_col) <- project$redcap$id_col
  DF <- cbind(id_col, DF)
  DF
}
#' @title Deidentify the REDCap Database
#' @description
#' Removes or masks identifying information from the REDCap database (`project`).
#' This can be done either based on the `identifier` field in the metadata or
#' by specifying custom identifiers.
#'
#' @inheritParams save_project
#' @param identifiers Optional character vector of column names that should be
#' excluded from the `project`. If not provided, fields where
#' `project$metadata$fields$identifier == "y"` will be used as the default.
#' @param drop_free_text Logical. If `TRUE`, columns containing free text
#' will also be excluded from the `project`. Default is `FALSE`.
#'
#' @return
#' A `project` object with deidentified forms.
#'
#' @details
#' This function modifies the `project` object to exclude specified identifiers or
#' any columns flagged as identifiers in the metadata. Free-text fields can
#' also be optionally removed, ensuring the resulting dataset complies with
#' deidentification standards.
#'
#' @seealso
#' \code{\link{save_project}} for saving the modified database.
#'
#' @export
deidentify_project <- function(project, identifiers, drop_free_text = FALSE) {
  project <- assert_project(project)
  missing_identifiers <- missing(identifiers)
  if (!missing_identifiers) {
    identifiers <- identifiers %>% unique()
    bad_identifiers <- identifiers[
      which(
        !identifiers %in% project$metadata$fields$field_name
      )
    ]
    if (length(bad_identifiers) > 0) {
      stop(
        "There is a bad identifier... see `project$metadata$fields$field_name`: ",
        bad_identifiers %>% paste0(collapse = ", ")
      )
    }
    if (project$redcap$id_col %in% identifiers) {
      # If you want to pass a new set of random IDs to make this data use
      # `scramble_ID_project(project)`."
      stop(
        "Your REDCap ID, ",
        project$redcap$id_col,
        ", should not be deidentified."
      )
    }
  }
  if (missing_identifiers) {
    identifiers <- project$metadata$fields$field_name[
      which(project$metadata$fields$identifier == "y")
    ]
    if (length(identifiers) == 0) {
      warning("You have no identifiers marked in `project$metadata$fields$identifier`. You can set it in REDCap Project Setup and update project OR define your idenitifiers in this functions `identifiers` argument.", immediate. = TRUE)
    }
  }
  if (drop_free_text) { # placeholder
    identifiers <- identifiers %>%
      append(
        project$metadata$fields$field_name[which(project$metadata$fields$field_type == "notes")]
      ) %>%
      unique()
  }
  if (is_something(project$data)) {
    drop_list <- Map(function(NAME, COLS) {
      identifiers[which(identifiers %in% COLS)]
    }, names(project$data), lapply(project$data, colnames))
    drop_list <- drop_list[unlist(lapply(drop_list, length)) > 0]
    if (length(drop_list) == 0) {
      bullet_in_console(paste0("Nothing to deidentify from --> ", identifiers %>% paste0(collapse = ", ")), bullet_type = "x")
    } else {
      bullet_in_console(paste0("Deidentified ", project$short_name), bullet_type = "v")
    }
    for (FORM in names(drop_list)) {
      for (DROP in drop_list[[FORM]]) {
        project$data[[FORM]][[DROP]] <- NULL
        # message("Dropped '",DROP,"' from '",data_choice,"' --> '", FORM,"'")
      }
    }
  }
  return(project)
}
#' @rdname Links
#' @title Open Links to REDCap Pages
#' @description
#' Opens browser page for a given project object.
#' @details
#' Uses [utils::browseURL()] to open the specified REDCap page.
#' In order for the function to work you must have ran \code{project <- sync_project(project)} successfully at least once.
#' If the link brings you to a page that doesn't work check the url. It's possible your institution may have changed redcap versions, which is part of the URL. In that case run \code{project <- sync_project(project)} again.
#' You may have to be signed into REDCap for it to work properly.
#' When in doubt, just seek out the page by navigating on your own in REDCap. Report issues if you can.
#' @param project A validated `project` object containing REDCap project data and settings. Generated using \code{project <- \link{load_project}("PROJ")} or \link{setup_project}()
#' @return Nothing will be returned in R. Instead, a browser link
#' @family Link Functions
#' @export
link_API_token <- function(project) {
  project$links$redcap_API %>% utils::browseURL()
}
#' @rdname Links
#' @export
link_API_playground <- function(project) {
  project$links$redcap_API_playground %>% utils::browseURL()
}
#' @rdname Links
#' @export
link_REDCap_home <- function(project) {
  project$links$redcap_base %>% utils::browseURL()
}
#' @rdname Links
#' @export
link_REDCap_project <- function(project) {
  project$links$redcap_home %>% utils::browseURL()
}
#' @param record REDCap record id or study id etc, any column names that match `project$redcap$id_col`
#' @param page REDCap page for the record. Must be one of `project$metadata$forms$form_name`
#' @param instance REDCap instance if it's a repeating instrument
#' @param text_only logical for only returning text
#' @rdname Links
#' @export
link_REDCap_record <- function(project, record, page, instance, text_only = FALSE) {
  link <- paste0(project$links$redcap_base, "redcap_v", project$redcap$version, "/DataEntry/record_home.php?pid=", project$redcap$project_id)
  if (!missing(record)) {
    if (!record %in% project$summary$all_records[[project$redcap$id_col]]) stop(record, " is not one of the records inside project")
    if ("arm_num" %in% colnames(project$summary$all_records)) {
      link <- link %>% paste0("&arm=", project$summary$all_records$arm_num[which(project$summary$all_records$participant_id == record)])
    }
    link <- link %>% paste0("&id=", record)
  }
  if (!missing(page)) {
    link <- gsub("record_home", "index", link)
    if (!page %in% project$metadata$forms$form_name) stop(page, " has to be one of the instrument names: ", paste0(project$metadata$forms$form_name, collapse = ", "))
    link <- link %>% paste0("&page=", page)
    if (!missing(instance)) {
      if (!page %in% project$metadata$forms$form_name[which(project$metadata$forms$repeating)]) stop("If you provide an instance, it has to be one of the repeating instrument names: ", paste0(project$metadata$forms$form_name[which(project$metadata$forms$repeating)], collapse = ", "))
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
  # fields <- project$metadata$fields
  df_list <- project$data
  df_col_list <- df_list %>% lapply(colnames)
  forms <- names(df_list)
  key_cols_list <- forms %>% lapply(function(form) {
    df_col_list[[form]][which(df_col_list[[form]] %in% project$redcap$raw_structure_cols)]
  })
  names(key_cols_list) <- forms
  return(key_cols_list)
}
#' @noRd
get_key_col_list <- function(project) {
  if (!is_something(project$metadata$forms)) stop("Empty --> `project$metadata$forms`")
  out_list <- seq_len(nrow(project$metadata$forms)) %>% lapply(function(i) {
    out <- project$redcap$id_col
    if (project$redcap$is_longitudinal) out <- append(out, "redcap_event_name")
    if (project$metadata$forms$repeating[i]) {
      out <- append(out, "redcap_repeat_instrument")
      out <- append(out, "redcap_repeat_instance")
    }
    return(out)
  })
  names(out_list) <- project$metadata$forms$form_name
  return(out_list)
}
#' @noRd
raw_process_redcap <- function(raw, project, labelled) {
  # key_cols <-project$redcap$raw_structure_cols
  # key_cols <- key_cols[which(!key_cols%in%c("arm_num","event_name"))]
  # paste0(raw[[project$redcap$id_col]],"_",raw$redcap_event_name,"_",raw$redcap_repeat_instrument,"_",raw$redcap_repeat_instance)
  forms <- get_original_forms(project)
  fields <- get_original_fields(project)
  # arms <- project$metadata$arms
  events <- project$metadata$events
  event_mapping <- project$metadata$event_mapping
  data_list <- list()
  if (nrow(raw) > 0) {
    raw <- raw %>% all_character_cols()
    add_ons <- c(project$redcap$id_col, "arm_num", "event_name", "redcap_event_name", "redcap_repeat_instrument", "redcap_repeat_instance")
    if (project$redcap$is_longitudinal) {
      raw$id_temp <- seq_len(nrow(raw))
      raw <- merge(raw, events[, c("arm_num", "event_name", "unique_event_name")], by.x = "redcap_event_name", by.y = "unique_event_name", sort = FALSE, all.x = TRUE)
      add_ons <- add_ons[which(add_ons %in% colnames(raw))]
      cols <- c(add_ons, colnames(raw)) %>% unique()
      raw <- raw[order(raw$id_temp), cols %>% lapply(function(c) {
        which(colnames(raw) == c)
      }) %>% unlist() %>% as.integer()]
      raw$id_temp <- NULL
    }
    add_ons <- add_ons[which(add_ons %in% colnames(raw))]
    if (any(!project$redcap$raw_structure_cols %in% colnames(raw))) stop("raw is missing one of the following... and that's weird: ", project$redcap$raw_structure_cols %>% paste0(collapse = ", "))
    form_names <- forms$form_name[which(forms$form_name %in% unique(fields$form_name))]
    # form_name <- form_names %>% sample1()
    has_repeating_forms <- project$redcap$has_repeating_forms
    for (form_name in form_names) {
      form_field_names <- fields$field_name[which(fields$form_name == form_name & fields$field_name %in% colnames(raw) & fields$field_name != project$redcap$id_col)]
      if (length(form_field_names) == 0) {
        bullet_in_console(paste0("You might not have access to ", form_name, ". Unable to obtain."), bullet_type = "x")
      }
      if (length(form_field_names) > 0) {
        add_ons_x <- add_ons
        # form_name <-  forms$form_name %>% sample(1)
        is_repeating_form <- form_name %in% forms$form_name[which(forms$repeating)]
        is_longitudinal <- project$redcap$is_longitudinal
        rows <- seq_len(nrow(raw))
        if (is_repeating_form) {
          if (!"redcap_repeat_instrument" %in% colnames(raw)) stop("redcap_repeat_instrument not in colnames(raw)")
          if (is_longitudinal) {
            # rows <- which(raw$redcap_repeat_instrument==form_name)
            rows <- which(raw$redcap_repeat_instrument == form_name | raw$redcap_event_name %in% event_mapping$unique_event_name[which(!event_mapping$repeating & event_mapping$form == form_name)])
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
            raw_subset <- raw_to_labelled_form(FORM = raw_subset, project = project)
          }
          data_list[[form_name]] <- raw_subset
        }
      }
    }
  }
  return(data_list)
}
#' @noRd
sort_redcap_log <- function(log) {
  log[order(log$timestamp, decreasing = TRUE), ]
}
#' @noRd
clean_redcap_log <- function(log) {
  log <- unique(log)
  log$record_id <- NA
  log$action_type <- NA
  design_test <- log$action == "Manage/Design"
  design_rows <- which(design_test)
  not_design_rows <- which(!design_test)
  # notdesign action -----
  record_rows <- not_design_rows[dplyr::starts_with(match = internal_log_action_records, vars = log$action[not_design_rows])]
  log$record_id[record_rows] <- gsub("Update record|Delete record|Create record|[:(:]API[:):]|Auto|calculation|Lock/Unlock Record | |[:):]|[:(:]", "", log$action[record_rows])
  log$action_type[record_rows] <- log$action[record_rows] %>%
    strsplit(" ") %>%
    lapply(function(A) {
      A[[1]]
    }) %>%
    unlist()
  # record_rows2 <- not_design_rows[which(!is.na(log$record[not_design_rows]))]
  # record_rows %>% vec1_not_in_vec2(record_rows2)
  # log$record[record_rows2 %>% vec1_not_in_vec2(record_rows)]# users
  log$action_type[
    not_design_rows[
      dplyr::starts_with(
        match = internal_log_action_exports,
        vars = log$action[not_design_rows]
      )
    ]
  ] <- "Exports"
  log$action_type[
    not_design_rows[
      dplyr::starts_with(
        match = internal_log_action_users,
        vars = log$action[not_design_rows]
      )
    ]
  ] <- "Users"
  log$action_type[
    not_design_rows[
      dplyr::starts_with(
        match = internal_log_action_no_changes,
        vars = log$action[not_design_rows]
      )
    ]
  ] <- "No Changes"
  # x <- log[not_design_rows,]
  # x <- x[which(is.na(x$action_type)),]
  # x$action %>% table() %>% sort(decreasing = T)
  # design details  -------------------
  comment_rows <- design_rows[
    dplyr::starts_with(
      match = internal_log_details_comments,
      vars = log$details[design_rows]
    )
  ]
  log$record_id[comment_rows] <- stringr::str_extract(log$details[comment_rows], "(?<=Record: )[^,]+")
  log$action_type[comment_rows] <- "Comment"
  log$action_type[
    design_rows[
      dplyr::starts_with(
        match = internal_log_details_exports,
        vars = log$details[design_rows]
      )
    ]
  ] <- "Exports"
  log$action_type[
    design_rows[
      dplyr::starts_with(
        match = internal_log_details_metadata_major,
        vars = log$details[design_rows]
      )
    ]
  ] <- "Metadata Change Major"
  log$action_type[
    design_rows[
      dplyr::starts_with(
        match = internal_log_details_metadata_minor,
        vars = log$details[design_rows]
      )
    ]
  ] <- "Metadata Change Minor"
  log$action_type[
    design_rows[
      dplyr::starts_with(
        match = internal_log_details_no_changes,
        vars = log$details[design_rows]
      )
    ]
  ] <- "No Changes"
  log$action_type[
    design_rows[
      dplyr::starts_with(
        match = internal_log_details_tokens,
        vars = log$details[design_rows]
      )
    ]
  ] <- "Tokens"
  log$action_type[
    design_rows[
      dplyr::starts_with(
        match = internal_log_details_repository,
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
  log <- sort_redcap_log(log)
  return(log)
}
#' @noRd
internal_log_action_exports <- c(
  "Data export",
  "Download uploaded "
)
#' @noRd
internal_log_details_exports <- c(
  "Export ",
  "Download "
)
#' @noRd
internal_log_action_users <- c(
  "User assigned to role ",
  "Add user ",
  "Edit user ",
  "Delete user ",
  "Rename user role",
  "User removed from user role",
  "Create user role"
)
#' @noRd
internal_log_details_comments <- c(
  "Add field comment ",
  "Edit field comment ",
  "Delete field comment "
)
#' @noRd
internal_log_action_records <- c(
  "Update record ",
  "Delete record ",
  "Lock/Unlock Record ",
  "Create record "
)
#' @noRd
internal_log_action_no_changes <- c(
  "Enable external module ",
  "Disable external module ",
  "Modify configuration for external module "
)
#' @noRd
internal_log_details_no_changes <- c(
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
internal_log_details_tokens <- c(
  "Create API token",
  "User regenerate own API token"
)
#' @noRd
internal_log_details_repository <- c(
  "Upload file to File Repository",
  "Delete file from File Repository",
  "Delete folder from File Repository",
  "Create folder in File Repository",
  "Upload document to file repository"
)
#' @noRd
internal_log_details_metadata_minor <- c(
  "Tag new identifier fields",
  "Add/edit branching logic",
  "Reorder project fields",
  "Move project field",
  "Delete section header",
  "Reorder data collection instruments"
)
#' @noRd
internal_log_details_metadata_major <- c(
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
all_missing_codes <- function() {
  data.frame(
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
    )
  )
}
#' @noRd
missing_codes2 <- function(project) {
  included <- "missing_data_codes" %in% colnames(project$redcap$project_info)
  if (included) {
    is_na <- is.na(project$redcap$project_info$missing_data_codes)
    if (!is_na) {
      return(project$redcap$project_info$missing_data_codes %>% split_choices())
    }
    if (is_na) {
      return(NA)
    }
  }
  if (!included) {
    return(NA)
  }
}
