#' @title upload_transform_to_project Transform
#' @inheritParams save_project
#' @return project object
#' @export
upload_transform_to_project <- function(project) {
  if (is_something(project$transformation$data_updates)) {
    for (i in seq_along(project$transformation$data_updates)) {
      project$transformation$data_updates[[i]] %>%
        labelled_to_raw_form(project) %>%
        upload_form_to_REDCap(project)
    }
    cli_alert_wrap("Successfully uploaded to REDCap!", bullet_type = "v")
    project$transformation$data_updates <- list()
  } else {
    cli_alert_wrap("Nothing to upload!")
  }
  invisible(project)
}
#' @rdname default-transformations
#' @title Add Default Forms Transformation to the Database
#' @description
#' Applies default transformations to specific forms within the REDCap database
#' (`project`).
#' This function modifies the `project` object to include default
#' transformations, which may
#' involve adjustments, calculations, or reformatting of data in predefined
#' forms.
#'
#' @inheritParams save_project
#' @param forms_transformation a data.frame that matches instruments. See
#' `default_project_transformation` for an example.
#' @return
#' The updated `project` object with default transformations applied to the
#' specified forms.
#'
#' @details
#' This function is designed to streamline and standardize data processing by
#' applying default transformations to the database forms. The transformations
#' are predefined within the function and ensure consistency across datasets.
#'
#' @seealso
#' \code{\link{save_project}} for saving the database or summaries.
#' @export
add_default_project_transformation <- function(project) {
  project <- add_project_transformation(
    project = project,
    forms_transformation = default_project_transformation(project = project)
  )
  invisible(project)
}
#' @rdname default-transformations
#' @export
add_project_defaults <- function(project) {
  project <- add_default_project_transformation(project)
  project <- add_default_project_fields(project)
  project <- add_default_project_summary(project)
  invisible(project)
}
#' @rdname default-transformations
#' @export
default_project_transformation <- function(project) {
  assert_setup_project(project)
  forms_transformation <- non_rep_project_transformation(project)
  forms_transformation$merge_to <- project$internals$merge_form_name
  forms_transformation$by.y <- forms_transformation$by.x <- project$redcap$id_col
  # Will avoid doing this for now
  #forms_transformation$merge_to %>%
  # lapply(function(form_name) {
  #   if (form_name %in% names(project$metadata$form_key_cols)) {
  #     project$metadata$form_key_cols[[form_name]] %>%
  #       paste0(collapse = "+") %>%
  #       return()
  #   } else {
  #     rows <- which(!forms_transformation$repeating)
  #     if (length(rows) == 0) {
  #       return(NA)
  #     }
  #     form_name <- forms_transformation$form_name[rows[[1]]]
  #     project$metadata$form_key_cols[[form_name]] %>%
  #       paste0(collapse = "+") %>%
  #       return()
  #   }
  # }) %>%
  # unlist()
  forms_transformation$x_first <- FALSE
  forms_transformation$x_first[which(forms_transformation$repeating)] <- TRUE
  if (project$redcap$is_longitudinal) {
    forms_transformation$x_first[which(forms_transformation$repeating_via_events)] <- TRUE
  }
  forms_transformation
}
#' @noRd
non_rep_project_transformation <- function(project) {
  assert_setup_project(project)
  forms_transformation <- project$metadata$forms
  is_longitudinal <- project$redcap$is_longitudinal
  if (is_longitudinal) {
    forms_transformation <- forms_transformation[order(forms_transformation$repeating_via_events), ]
  }
  forms_transformation <- forms_transformation[order(forms_transformation$repeating), ]
  merge_form_name <- project$internals$merge_form_name
  forms_transformation$form_name_remap <- forms_transformation$form_name
  forms_transformation$form_label_remap <- forms_transformation$form_label
  row_check <- !forms_transformation$repeating
  if (is_longitudinal) {
    row_check <- row_check & !forms_transformation$repeating_via_events
  }
  forms_transformation$form_name_remap[which(row_check)] <- merge_form_name
  merge_form_name_label <- merge_form_name
  if (merge_form_name %in% forms_transformation$form_name) {
    merge_form_name_label <- forms_transformation$form_label[which(forms_transformation$form_name == merge_form_name)]
  }
  forms_transformation$form_label_remap[which(row_check)] <- merge_form_name_label
  forms_transformation$merge_to <- NA
  forms_transformation$by.y <- forms_transformation$by.x <- forms_transformation$merge_to
  forms_transformation$x_first <- FALSE
  forms_transformation
}
#' @rdname default-transformations
#' @export
add_default_project_fields <- function(project) {
  forms <- project$metadata$forms
  last_non_rep <- forms$form_name[which(!forms$repeating)] %>% dplyr::last()
  form_names <- forms$form_name[which(forms$repeating)]
  if (!project$redcap$is_longitudinal) {
    has_non_rep <- length(last_non_rep) > 0
    if (has_non_rep) {
      for (form_name in form_names) {
        form_label <- forms$form_label[which(forms$form_name == form_name)]
        project <- project %>% add_project_field(
          field_name = paste0("n_forms_", form_name),
          form_name = last_non_rep,
          field_type = "text",
          field_type_R = "integer",
          field_label = paste0(form_label, " Forms"),
          units = "n",
          data_func = function(project, field_name, form_name) {
            form <- gsub("n_forms_", "", field_name)
            #need another way to count if multiple id cols
            id_col <- project$metadata$form_key_cols[[form_name]]
            final_vector <- project$data[[form_name]][[id_col]] %>%
              matches(project$data[[form]][[id_col]], count_only = TRUE) %>%
              as.character()
            final_vector
          }
        )
      }
    }
  }
  for (form_name in form_names) {
    form_label <- forms$form_label[which(forms$form_name == form_name)]
    project <- project %>% add_project_field(
      field_name = paste0(form_name, "_compound_key"),
      form_name = form_name,
      field_type = "text",
      field_type_R = "character",
      field_label = paste(form_label, "Compound Key"),
      data_func = function(project, field_name, form_name) {
        cols <- project$metadata$form_key_cols[[form_name]]
        form <- NULL
        while (length(cols) > 0) {
          if (is.null(form)) {
            form <- project$data[[form_name]][[cols[1]]]
          } else {
            form <- form %>% paste0("_", project$data[[form_name]][[cols[1]]])
          }
          cols <- cols[-1]
        }
        form
      }
    )
  }
  invisible(project)
}
#' @rdname default-transformations
#' @export
add_project_transformation <- function(project,
                                       forms_transformation) {
  if (missing(forms_transformation)) {
    forms_transformation <- default_project_transformation(project)
  }
  forms_tranformation_cols <- c(
    "form_name",
    "form_label",
    "repeating",
    "form_name_remap",
    "form_label_remap",
    "merge_to",
    "by.x",
    "by.y",
    "x_first"
  )
  if (project$redcap$is_longitudinal) {
    forms_tranformation_cols <- forms_tranformation_cols %>%
      append("repeating_via_events")
  }
  if (!all(names(forms_transformation) %in% forms_tranformation_cols)) {
    cli_alert_wrap(
      "Use `add_default_forms_transformation(project)` is an example!"
    )
    stop(
      "forms_transformation needs the following colnames... ",
      forms_tranformation_cols %>% toString()
    )
  }
  if (!is.null(project$transformation$forms)) {
    if (identical(project$transformation$forms, forms_transformation)) {
      #message
      return(invisible(project))
    }
  }
  # add more checks
  project$transformation$forms <- forms_transformation
  project$summary$all_records$was_transformed <- FALSE
  invisible(project)
}
#' @title Add Field Transformation to the Database
#' @description
#' `r lifecycle::badge("experimental")`
#' Adds a new field transformation to the REDCap database (`project`). This
#' allows users to define custom transformations for a specific field in a form,
#' including its type, label, choices, and associated function for data
#' manipulation.
#'
#' @inheritParams save_project
#' @param field_name Character. The name of the field to which the
#' transformation
#' will be applied.
#' @param form_name Character. The name of the form containing the field.
#' @param field_type Character. The type of the field in REDCap (e.g., "text",
#' "checkbox", "dropdown").
#' @param field_type_R Character. The corresponding R data type for the field.
#' Default is `NA`.
#' @param field_label Character. The label for the field. Default is `NA`.
#' @param select_choices_or_calculations Character. A string specifying the
#' choices (for dropdown, radio, or checkbox fields) or calculations (for
#' calculated fields). Default is `NA`.
#' @param field_note Character. An optional note or comment for the field.
#' Default is `NA`.
#' @param identifier Character. A string indicating whether the field is an
#' identifier (e.g., "Y" for yes). Default is an empty string (`""`).
#' @param units Character. The units of measurement for the field, if
#' applicable. Default is `NA`.
#' @param data_func Function or NA. An optional function to transform or
#' validate the data in the field. Default is `NA`.
#'
#' @return
#' The updated `project` object with the field transformation added.
#'
#' @details
#' This function facilitates the addition of a new field transformation to a
#' REDCap database. The transformation includes metadata such as the field's
#' type, label, and choices, along with an optional function to process the
#' data. This is particularly useful for customizing or extending the
#' functionality of existing REDCap forms and fields.
#'
#' @seealso
#' \code{\link{save_project}} for saving the database or summaries.
#'
#' @export
add_project_field <- function(
    project,
    field_name,
    form_name,
    field_type,
    field_type_R = NA,
    field_label = NA,
    select_choices_or_calculations = NA,
    field_note = NA,
    identifier = "",
    units = NA,
    data_func = NA) {
  project <- assert_blank_project(project, silent = TRUE)
  if (which_length(project$transformation$fields$field_name == field_name) > 0) {
    project$transformation$fields <- project$transformation$fields[which(project$transformation$fields$field_name != field_name), ]
  }
  # if(!project$data %>% is_something())stop("Must have transformed data to add new vars.")
  fields <- project$metadata$fields
  in_original_redcap <- field_name %in% fields$field_name
  if (is_something(select_choices_or_calculations)) {
    select_choices_or_calculations <- choice_vector_string(select_choices_or_calculations)
  }
  if (in_original_redcap) {
    original_fields_row <- fields[which(fields$field_name == field_name), ]
    if (missing(form_name)) form_name <- original_fields_row$form_name
    if (missing(field_type)) {
      field_type <- original_fields_row$field_type
      field_type_R <- original_fields_row$field_type_R
    }
    if (is.na(field_label)) field_label <- original_fields_row$field_label
    if (is.na(select_choices_or_calculations)) {
      select_choices_or_calculations <- original_fields_row$select_choices_or_calculations
    }
    if (is.na(field_note)) field_note <- original_fields_row$field_note
    if (identifier == "") identifier <- original_fields_row$identifier
  }
  if (!is_something(data_func)) {
    warning("if no `data_func` is provided, the column is only added to the metadata", immediate. = TRUE)
  }
  if (is_something(data_func)) {
    func_template <- "data_func = function(project,field_name){YOUR FUNCTION}"
    if (!is.function(data_func)) {
      stop("`data_func` must be a function ... ", func_template)
    }
    allowed_args <- c("project", "field_name", "form_name")
    if (!any(allowed_args %in% names(formals(data_func))) ||
        !all(names(formals(data_func)) %in% allowed_args)) {
      stop("`data_func` must have two arguments (project and field_name) ... ",
           func_template)
    }
  }
  # add check for identical and if not identical was_transformed = FALSE
  field_row <- data.frame(
    field_name = field_name,
    form_name = form_name,
    field_type = field_type,
    field_label = field_label,
    select_choices_or_calculations = select_choices_or_calculations,
    field_note = field_note,
    identifier = identifier,
    field_type_R = field_type_R,
    units = units,
    in_original_redcap = in_original_redcap,
    field_label_short = field_label,
    field_func = function_to_string(data_func),
    stringsAsFactors = FALSE
  )
  project$transformation$fields <-
    project$transformation$fields %>%
    dplyr::bind_rows(field_row)
  project$transformation$field_functions[[field_name]] <-
    data_func %>% clean_function()
  message("added '", field_name, "' column")
  invisible(project)
}
#' @noRd
combine_project_fields <- function(project) {
  the_names <- project$transformation$fields$field_name
  fields <- project$metadata$fields
  if (is.null(the_names)) {
    cli_alert_danger("Nothing to add. Use `add_project_field()`")
    return(fields)
  }
  for (field_name in the_names) {
    field_row <- project$transformation$fields[which(project$transformation$fields$field_name == field_name), ]
    form_name <- field_row$form_name
    # if(any(fields$field_name==field_name))stop("field_name already included")
    current_row <- which(fields$field_name == field_name)
    if (length(current_row) > 0) {
      fields <- fields[-current_row, ]
      i <- current_row
      if (i > 1) i <- i - 1
    } else {
      i <- which(fields$form_name == form_name & fields$field_name == paste0(form_name, "_complete"))
      if (length(i) > 0) {
        if (i[[1]] > 1) {
          i <- i - 1
        }
      }
      if (length(i) == 0) {
        i <- which(fields$form_name == form_name)
      }
      if (length(i) > 1) {
        i <- i[[1]]
      }
      if (length(i) == 0) i <- nrow(fields)
    }
    if (length(i) == 0) stop("insert_after error")
    top <- fields[1:i, ]
    bottom <- NULL
    if (i < nrow(fields)) bottom <- fields[(i + 1):nrow(fields), ]
    fields <- top %>%
      dplyr::bind_rows(field_row) %>%
      dplyr::bind_rows(bottom)
  }
  fields
}
#' @rdname default-transformations
#' @title Add Default Forms Transformation to the Database
#' @export
add_default_project_summary <- function(project) {
  summary_name <- "REDCapSync_raw"
  project <- add_project_summary(
    project = project,
    summary_name = summary_name,
    transform = FALSE,
    filter_list = NULL,
    exclude_identifiers = FALSE,
    exclude_free_text = FALSE,
    date_handling = "none",
    clean = FALSE,
    drop_blanks = FALSE,
    drop_others = NULL,
    include_metadata = TRUE,
    annotate_metadata = FALSE,
    include_record_summary = FALSE,
    include_users = TRUE,
    include_log = TRUE,
    with_links = nrow(project$summary$all_records) <= 3000,
    separate = TRUE,
    use_csv = project$internals$use_csv,
    dir_other = file.path(project$dir_path, "REDCap", project$short_name),
    file_name = project$short_name
  )
  summary_name <- "REDCapSync"
  project <- add_project_summary(
    project = project,
    summary_name = summary_name,
    transform = TRUE,
    filter_list = NULL,
    exclude_identifiers = TRUE,
    exclude_free_text = FALSE,
    date_handling = "none",
    upload_compatible = TRUE,
    clean = TRUE,
    drop_blanks = FALSE,
    drop_others = NULL,
    include_metadata = TRUE,
    annotate_metadata = TRUE,
    include_record_summary = TRUE,
    include_users = TRUE,
    include_log = FALSE,
    with_links = nrow(project$summary$all_records) <= 3000,
    separate = FALSE,
    use_csv = project$internals$use_csv,
    dir_other = file.path(project$dir_path, "output"),
    file_name = paste0(project$short_name, "_", summary_name)
  )
  invisible(project)
}
#' @title transform_project
#' @noRd
transform_project <- function(project) {
  has_transformation <- is_something(project$transformation$forms)
  the_names <- project$transformation$fields$field_name
  has_fields <- !is.null(the_names)
  forms_transformation <- project$transformation$forms
  forms_transformation_original <- forms_transformation
  named_df_list <- project$data
  has_data <- is_something(named_df_list)
  original_fields <- project$metadata$fields
  if (!has_data) {
    cli_alert_warning("No data... nothing to do!")
    return(invisible(NULL))
  }
  if (!has_fields) {
    cli_alert_danger("No additional fields. Use `add_project_field()`")
  }
  if (!has_transformation) {
    cli_alert_warning("No transformation. Use `add_project_transformation()`")
  }
  the_names_existing <- the_names[which(the_names %in% original_fields$field_name)]
  the_names_new <- the_names[which(!the_names %in% original_fields$field_name)]
  field_names <- c(the_names_existing, the_names_new)
  for (field_name in field_names) {
    field <- NA
    row_of_interest <- project$transformation$fields[which(project$transformation$fields$field_name == field_name), ]
    form_name <- row_of_interest$form_name
    field_func <- project$transformation$field_functions[[field_name]]
    environment(field_func) <- environment()
    if (is_something(field_func)) {
      if (form_name %in% names(named_df_list)) {
        field <- field_func(project = project, field_name = field_name, form_name = form_name)
      }
    }
    if (field_name %in% the_names_existing) {
      form_old <- named_df_list[[form_name]][[field_name]]
      if (!identical(field, form_old)) {
        ref_cols <- project$metadata$form_key_cols[[form_name]]
        new <- old <- named_df_list[[form_name]][, c(ref_cols, field_name)]
        new[[field_name]] <- field
        form <- find_form_diff2(
          new = new,
          old = old,
          ref_cols = ref_cols,
          view_old = FALSE,
          message_pass = paste0(form_name, " - ", field_name, ": ")
        )
        if (is_something(form)) {
          project$transformation$data_updates[[field_name]] <- form
        }
      }
    }
    if (form_name %in% names(named_df_list)) {
      named_df_list[[form_name]][[field_name]] <- field
    }
  }
  cli_alert_wrap(paste0("Added new fields to ", project$short_name, " `project$data`"), bullet_type = "v")
  form_list <- NULL
  for (i in (seq_len(nrow(forms_transformation)))) {
    form_name <- forms_transformation$form_name[i]
    ref <- named_df_list[[form_name]]
    if (!is.null(ref)) {
      if (is_something(ref)) {
        a <- forms_transformation[i, ]
        z <- as.list(a)
        ref <- named_df_list[[form_name]]
        rownames(ref) <- NULL
        by.x <- z$by.x <- z$by.x %>%
          strsplit("\\+") %>%
          unlist()
        by.y <- z$by.y <- z$by.y %>%
          strsplit("\\+") %>%
          unlist()
        if (length(z$by.x) != length(z$by.y)) {
          stop(
            "by.x and by.y must be same length... [",
            z$form_name,
            "] (",
            z$by.x %>% toString(),
            ") AND (",
            z$by.y %>% toString(),
            ")"
          )
        }
        if (form_name == z$merge_to) {
          form_list[[z$form_name_remap]] <- ref
        } else {
          mer <- named_df_list[[z$merge_to]]
          if (z$merge_to %in% names(form_list)) {
            mer <- form_list[[z$merge_to]]
          }
          ref_names <- names(ref)
          mer_names <- names(mer)
          new_names <- ref_names %>%
            vec1_in_vec2(mer_names) %>%
            vec1_not_in_vec2(by.x)
          for (new_name in new_names) {
            col <- which(colnames(mer) == new_name)
            replace_name <- paste0(new_name, "_merged")
            a <- mer[, 1:col]
            a[[replace_name]] <- a[[col]]
            b <- mer[, (col + 1):ncol(mer)]
            mer <- cbind(a, b)
          }
          bad_cols <- which(!by.x %in% by.y)
          z$by.x[bad_cols]
          z$by.y[bad_cols]
          if (length(bad_cols) > 0) {
            for (col in bad_cols) {
              new_col_name <- paste0(z$by.y[col], "_merged")
              ref[[new_col_name]] <- ref[[z$by.x[col]]]
              z$by.x[col] <- new_col_name
            }
          }
          by.x <- z$by.x
          by.y <- z$by.y
          ref_names <- names(ref) %>% vec1_not_in_vec2(
            by.x %>% vec1_not_in_vec2(by.y)
          )
          mer_names <- names(mer)
          del_names <- mer_names %>%
            vec1_in_vec2(ref_names) %>%
            vec1_not_in_vec2(by.y)
          mer[, del_names] <- NULL
          ref$sort_me_ftlog <- seq_len(nrow(ref))
          if (is.null(mer)) {
            a <- ref
          } else {
            a <- merge(
              x = ref,
              y = mer,
              by.x = by.x,
              by.y = by.y,
              all.x = TRUE,
              sort = FALSE
            )
          }
          a <- a[order(a$sort_me_ftlog), ]
          all_names <- c(ref_names, names(mer)) %>% unique()
          if (is_something(z$x_first)) {
            if (!z$x_first) {
              all_names <- c(by.y %>% vec1_in_vec2(by.x), names(mer) %>% vec1_not_in_vec2(by.y), ref_names) %>% unique()
            }
          }
          a <- a[, match(all_names, names(a))]
          rownames(a) <- NULL
          form_list[[z$form_name_remap]] <- a
        }
      }
    }
  }
  if (!all(names(form_list) %in% unique(forms_transformation$form_name_remap))) {
    stop("not all names in form_list objext. Something wrong with transform_project()")
  }
  if (is_something(form_list)) {
    project$transformation$data <- form_list
  }
  if (!is.null(project$metadata$form_key_cols)) {
    forms_transformation$key_cols <- forms_transformation$form_name %>%
      lapply(function(x) {
        project$metadata$form_key_cols[[x]] %>% paste0(collapse = "+")
      }) %>%
      unlist()
    forms_transformation$key_names <- forms_transformation$form_name %>%
      lapply(function(x) {
        row_match <- which(forms_transformation$form_name == x)
        if (!forms_transformation$repeating[row_match]) {
          return(project$metadata$form_key_cols[[x]])
        }
        paste0(forms_transformation$form_name[row_match], "_key")
      })
  }
  project$internals$is_transformed <- TRUE
  cli_alert_wrap(
    paste0(
      project$short_name,
      " transformed according to `project$transformation`"
    ),
    bullet_type = "v"
  )
  # forms ---------
  # new function RosyUtils
  cols_to_keep <- c("form_name_remap", "form_label_remap", "repeating", "repeating_via_events", "key_cols", "key_names")
  cols_to_keep <- cols_to_keep[which(cols_to_keep %in% colnames(forms_transformation))]
  forms_transformation <- forms_transformation[, cols_to_keep] %>% unique()
  colnames(forms_transformation)[which(colnames(forms_transformation) == "form_name_remap")] <- "form_name"
  colnames(forms_transformation)[which(colnames(forms_transformation) == "form_label_remap")] <- "form_label"
  forms_transformation$original_form_name <- forms_transformation$form_name %>%
    lapply(function(form_name) {
      forms_transformation_original$form_name[which(forms_transformation_original$form_name_remap == form_name)] %>% paste0(collapse = " | ")
    }) %>%
    unlist() %>%
    as.character()
  project$transformation$metadata$forms <- forms_transformation
  # fields------------
  fields <- combine_project_fields(project)
  fields$original_form_name <- fields$form_name
  fields$form_name <- forms_transformation_original$form_name_remap[match(fields$form_name, forms_transformation_original$form_name)]
  fields <- fields[order(match(fields$form_name, forms_transformation$form_name)), ]
  # new function RosyUtils
  first <- 1:which(colnames(fields) == "form_name")
  move <- which(colnames(fields) == "original_form_name")
  last <- which(colnames(fields) != "original_form_name")[-first]
  fields <- fields[, c(first, move, last)]
  project$transformation$metadata$fields <- fields
  cli_alert_wrap(
    paste0(
      "Added mod fields to ",
      project$short_name,
      " `project$transformation`"
    ),
    bullet_type = "v"
  )
  project$transformation$metadata$choices <- fields_to_choices(fields)
  project$transformation$metadata$form_key_cols <-
    get_key_col_list(project = project, transform = TRUE)
  project$transformation$metadata$missing_codes <-
    project$metadata$missing_codes
  project$internals$last_data_transformation <- now_time()
  invisible(project)
}
#' @noRd
missing_form_names <- function(project) {
  form_names <- names(project$data)
  form_names <- form_names[which(!form_names %in% project$metadata$forms$form_name)]
  form_names
}
