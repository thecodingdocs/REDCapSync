#' @noRd
labelled_to_raw_form <- function(form, project) {
  form <- all_character_cols(form)
  if (nrow(form) == 0L) {
    return(form)
  }
  fields <- filter_fields_from_form(form = form, project = project)
  for (i in seq_len(nrow(fields))) {
    field_row <- fields[i, ]
    field_name <- field_row$field_name
    conversion_table <- generate_choices_table(field_row, project)
    match_rows <- match(form[[field_name]], conversion_table$name)
    if (field_row$has_choices) {
      no_match <- which(is.na(match_rows) & !is.na(form[[field_name]]))
      if (length(no_match) > 0L) {
        bad_choices <- form[[field_name]][no_match] |> unique() |> toString()
        cli_alert_danger(form[[field_name]][no_match])
        cli_abort("Mismatched REDCap! `{field_name}` = {bad_choices}")
      }
      form[[field_name]] <- conversion_table$code[match_rows]
    } else {
      map_row <- which(!is.na(match_rows))
      form[[field_name]][map_row] <- conversion_table$name[match_rows[map_row]]
    }
  }
  form
}
#' @noRd
raw_to_labelled_form <- function(form, project) {
  if (project$metadata$has_coding_conflicts) {
    stop(
      "You cannot use labelled = 'TRUE' because you have a coding conflict ",
      "in your data dictionary... Try `setup_project` with labelled = 'FALSE'.",
      "The conflicts are from: ",
      toString(project$metadata$coding_conflict_field_names)
    )
  }
  form <- all_character_cols(form)
  if (nrow(form) == 0L) {
    return(form)
  }
  fields <- filter_fields_from_form(form = form, project = project)
  for (i in seq_len(nrow(fields))) {
    field_row <- fields[i, ]
    field_name <- field_row$field_name
    conversion_table <- generate_choices_table(field_row, project)
    match_rows <- match(form[[field_name]], conversion_table$code)
    if (field_row$has_choices) {
      no_match <- which(is.na(match_rows) & !is.na(form[[field_name]]))
      if (length(no_match) > 0L) {
        bad_choices <- form[[field_name]][no_match] |> unique() |> toString()
        cli_alert_warning("Mismatched REDCap! {field_name} = {bad_choices}")
        affected <- form[[project$metadata$id_col]][no_match] |> toString()
        cli_alert_danger("Affected records: {affected}")
      }
      form[[field_name]] <- conversion_table$name[match_rows]
    } else {
      map_row <- which(!is.na(match_rows))
      form[[field_name]][map_row] <- conversion_table$name[match_rows[map_row]]
    }
  }
  form
}
#' @noRd
generate_choices_table <- function(field_row, project) {
  missing_codes <- project$metadata$missing_codes
  use_missing_codes <- is.data.frame(project$metadata$missing_codes)
  has_choices <- field_row$has_choices
  conversion_table <- NULL
  if (has_choices) {
    choice_table <- split_choices(field_row$select_choices_or_calculations)
    conversion_table <- bind_rows(conversion_table, choice_table)
  }
  if (use_missing_codes) {
    conversion_table <- bind_rows(conversion_table, missing_codes)
  }
  conversion_table
}
#' @noRd
labelled_to_raw_data_list <- function(project) {
  project <- assert_blank_project(project)
  if (!project$internals$labelled) {
    stop("project is already raw or coded (not labelled values)")
  }
  for (form_name in names(project$data)) {
    form <- project$data[[form_name]]
    project$data[[form_name]] <- labelled_to_raw_form(form = form,
                                                      project = project)
  }
  project$internals$labelled <- FALSE
  invisible(project)
}
#' @noRd
raw_to_labelled_data_list <- function(project) {
  project <- assert_blank_project(project)
  if (project$internals$labelled) {
    stop("project is already labelled (not raw or coded values)")
  }
  if (project$metadata$has_coding_conflicts) {
    stop("project has codebook conflict(s), so will not convert to labelled!")
  }
  for (form_name in names(project$data)) {
    form <- project$data[[form_name]]
    project$data[[form_name]] <- raw_to_labelled_form(form = form,
                                                      project = project)
  }
  project$internals$labelled <- TRUE
  invisible(project)
}
#' @noRd
normalize_redcap <- function(denormalized, project, labelled) {
  forms <- project$metadata$forms
  fields <- project$metadata$fields
  events <- project$metadata$events
  event_mapping <- project$metadata$event_mapping
  form_list <- list()
  if (nrow(denormalized) == 0L) {
    return(form_list)
  }
  is_longitudinal <- project$metadata$is_longitudinal
  denormalized <- all_character_cols(denormalized)
  add_ons <- c(project$metadata$id_col,
               "arm_number",
               "event_name",
               "redcap_event_name",
               .redcap_repeat_cols)
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
    col_names <- c(add_ons, colnames(denormalized)) |> unique()
    new_order <- order(denormalized$id_temp)
    cols_to_keep <- col_names |>
      lapply(function(c) {
        which(colnames(denormalized) == c)
      }) |>
      unlist() |>
      as.integer()
    denormalized <- denormalized[new_order, cols_to_keep]
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
    if (length(form_field_names) == 0L) {
      cli_alert_danger("Possibly no access to '{form_name}'. Unable to obtain.")
    }
    # consider message for what vars are missing with vec1_not_in_vec2
    row_index <- NULL
    if (length(form_field_names) > 0L) {
      add_ons_x <- add_ons
      is_repeating_form <- form_name %in% repeating_forms
      row_index <- seq_len(nrow(denormalized))
      if (is_repeating_form) {
        if (!"redcap_repeat_instrument" %in% colnames(denormalized)) {
          stop("redcap_repeat_instrument not in colnames(denormalized)")
        }
        if (is_longitudinal) {
          has_form_event <- event_mapping$form == form_name
          no_rep_form_event <- which(!event_mapping$repeating & has_form_event)
          no_rep_event <- event_mapping$unique_event_name[no_rep_form_event]
          form_no_rep_event <- denormalized$redcap_event_name %in% no_rep_event
          form_has_rep <- denormalized$redcap_repeat_instrument == form_name
          row_index <- which(form_has_rep | form_no_rep_event)
        }
        if (!is_longitudinal) {
          row_index <- which(denormalized$redcap_repeat_instrument == form_name)
        }
      }
      if (!is_repeating_form) {
        add_ons_x <- add_ons_x[which(!add_ons_x %in% .redcap_repeat_cols)]
      }
      if (!is_repeating_form && is_longitudinal && has_repeating_forms) {
        form_event_rows <- which(event_mapping$form == form_name)
        event_forms <- unique(event_mapping$unique_event_name[form_event_rows])
        has_event_name <- denormalized$redcap_event_name %in% event_forms
        has_rep_instrument <- is.na(denormalized$redcap_repeat_instrument)
        row_index <- which(has_rep_instrument & has_event_name)
      }
      if (!is_repeating_form && is_longitudinal && !has_repeating_forms) {
        form_event_rows <- which(event_mapping$form == form_name)
        event_forms <- unique(event_mapping$unique_event_name[form_event_rows])
        row_index <- which(denormalized$redcap_event_name %in% event_forms)
      }
      if (!is_repeating_form && !is_longitudinal && has_repeating_forms) {
        row_index <- which(is.na(denormalized$redcap_repeat_instrument))
      }
    }
    if (is_something(row_index)) {
      col_names <- unique(c(add_ons_x, form_field_names))
      subset <- denormalized[row_index, col_names]
      if (labelled) {
        subset <- raw_to_labelled_form(form = subset, project = project)
      }
      form_list[[form_name]] <- subset
    }
  }
  form_list <- all_character_cols_list(form_list)
  form_list
}
#' @noRd
get_min_dates <- function(data_list) {
  # assert_data_list contains data and metadata with forms and fields
  data_forms <- data_list$data
  metadata <- data_list$metadata
  fields <- metadata$fields
  id_cols <- metadata$form_key_cols |>
    unlist() |>
    unique()
  empty <- data.frame(
    record_id = character(),
    min_date = as.Date(character()),
    stringsAsFactors = FALSE
  )
  if (!is_something(data_forms)) {
    return(empty)
  }
  date_vector <- fields$field_name[which(fields$field_type_r == "date")]
  all_dates <- list()
  # Loop through each form in the list
  for (form in data_forms) {
    if (id_cols[1L] %in% names(form)) {
      # Find the date fields that exist in this form
      existing_fields <- intersect(names(form), date_vector)
      if (length(existing_fields) > 0L) {
        df_subset <- form[, c(id_cols[1L], existing_fields), drop = FALSE]
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
  if (length(all_dates) == 0L) {
    return(empty)
  }
  combined <- do.call(rbind, all_dates)
  combined <- combined[!is.na(combined$date), ]
  min_dates <- stats::aggregate(date ~ record_id, data = combined, FUN = min)
  min_dates
}
#' @noRd
get_project_url <- function(project,
                            link_type = "home",
                            open_browser = TRUE) {
  assert_choice(link_type, .link_types)
  the_link <- project$links[[paste0("redcap_", link_type)]]
  if (open_browser) {
    browseURL(the_link)
  }
  invisible(the_link)
}
#' @noRd
get_record_url <- function(project,
                           record = NULL,
                           page = NULL,
                           instance = NULL,
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
  if (!is.null(record)) {
    if (!record %in% project$summary$all_records[[id_col]]) {
      stop(record, " is not one of the records inside project")
    }
    if ("arm_number" %in% colnames(project$summary$all_records)) {
      arm_row <- which(project$summary$all_records[[id_col]] == record)
      link <- paste0(link,
                     "&arm=",
                     project$summary$all_records$arm_number[arm_row])
    }
    link <- paste0(link, "&id=", record)
  }
  if (!is.null(page)) {
    link <- gsub("record_home", "index", link)
    if (!page %in% project$metadata$forms$form_name) {
      stop(
        page,
        " has to be one of the instrument names: ",
        toString(project$metadata$forms$form_name)
      )
    }
    link <- paste0(link, "&page=", page)
    if (!is.null(instance)) {
      rep_rows <- which(project$metadata$forms$repeating)
      repeating_form_names <- project$metadata$forms$form_name[rep_rows]
      if (!page %in% repeating_form_names) {
        stop(
          "If you provide an instance, it has to be a repeating instrument: ",
          toString(repeating_form_names)
        )
      }
      link <- paste0(link, "&instance=", instance)
    }
  }
  if (open_browser) {
    browseURL(link)
  }
  invisible(link)
}
#' @noRd
get_key_col_list <- function(data_list) {
  forms <- data_list$metadata$forms
  if (!is_something(forms)) {
    stop("Empty --> `project$metadata$forms`")
  }
  out_list <- seq_len(nrow(forms)) |> lapply(function(i) {
    out <- data_list$metadata$id_col
    if (data_list$metadata$is_longitudinal) {
      out <- append(out, "redcap_event_name")
    }
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
  # assert data list
  # what to do when record_id is marked as identifier? add psuedo
  # handle dates seprately if shifted
  id_cols <- data_list$metadata$form_key_cols |> unlist() |> unique()
  fields <- data_list$metadata$fields
  fields$validation_type <- fields$text_validation_type_or_show_slider_number
  not_id <- !fields$field_name %in% id_cols
  is_identifier <- fields$identifier == "y"
  is_likely_identifier <- fields$validation_type %in% .redcap_maybe_ids_strict
  if (get_type == "deidentified") {
    keep_rows <- which(is_identifier)
  }
  if (get_type == "deidentified_strict") {
    keep_rows <- which(is_identifier | is_likely_identifier)
  }
  if (get_type == "deidentified_super_strict") {
    is_notes <- fields$field_type == "notes"
    is_text <- fields$field_type == "text"
    has_validation <- !is.na(fields$validation_type)
    is_free_text <- is_notes | (is_text & !has_validation) & not_id
    keep_rows <- which(is_identifier | is_likely_identifier | is_free_text)
    #account for drops
  }
  id_fields <- fields$field_name[keep_rows]
  if (invert) {
    id_fields <- setdiff(fields$field_name, id_fields)
  }
  id_fields
}
#' @noRd
field_to_form_names <- function(project,
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
  if (anyDuplicated(fields$field_name) > 0L)
    stop("duplicate names not allowed in fields")
  data_field_list <- lapply(data_list$data, colnames)
  header_df_list <- data_field_list |> lapply(function(field_names) {
    x <- field_names |>
      lapply(function(field_name) {
        x_row <- which(fields$field_name == field_name)
        if (length(x_row) > 0L) {
          return(as.character(fields[md_elements][x_row, ]))
        }
        rep("", length(md_elements))
      }) |>
      as.data.frame()
    colnames(x) <- field_names
    x <- x[which(apply(x, 1L, function(x_row) {
      any(nzchar(x_row)) #consider use NA?
    })), ]
    x
  }) |>
    process_df_list(silent = TRUE)
  header_df_list
}
#' @noRd
field_names_metadata <- function(project, field_names, col_names) {
  fields <- project$metadata$fields
  bad_field_names <- field_names[which(
    !field_names %in% c(
      project$metadata$fields$field_name,
      project$metadata$raw_structure_cols,
      "arm_number",
      "event_name"
    )
  )]
  if (length(bad_field_names) > 0L)
    stop(
      "All column names in your form must match items in your metadata, ",
      "`project$metadata$fields$field_name`... ",
      toString(bad_field_names)
    )
  fields <- fields[which(fields$field_name %in% field_names), ]
  if (!missing(col_names)) {
    if (is_something(col_names))
      fields <- fields[[col_names]]
  }
  fields
}
#' @noRd
filter_fields_from_form <- function(form, project) {
  forms <- field_to_form_names(project = project,
                               field_names = colnames(form),
                               strict = TRUE)
  repeating_form_rows <- which(project$metadata$forms$repeating)
  repeating_forms <- project$metadata$forms$form_name[repeating_form_rows]
  if (any(forms %in% repeating_forms) && length(forms) > 1L) {
    stop(
      "All column names in your form must match only one form in your",
      "metadata, `project$metadata$forms$form_name`, unless they are",
      " all non-repeating"
    )
  }
  fields <- project |> field_names_metadata(field_names = colnames(form))
  fields <- fields[which(fields$field_type != "descriptive"), ]
  fields$has_choices <- !is.na(fields$select_choices_or_calculations)
  rows_no_choices <- which(fields$field_type %in% c("calc", "text", "slider"))
  fields$has_choices[rows_no_choices] <- FALSE
  fields
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
  if (is_something(data_list$data)) {
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
    id_col_match <- which(colnames(all_records) == "record_id_col")
    colnames(all_records)[id_col_match] <- id_col
  }
  all_records
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
.redcap_repeat_cols <- c("redcap_repeat_instrument", "redcap_repeat_instance")
