#' @title Horizontal Transform
#' @description
#' This function performs a horizontal transformation on the data in the
#' `project` object, transforming the data format so that each record is
#' represented in a horizontal layout. It is useful when you want to reshape
#' or pivot the data for further analysis or presentation.
#'
#' @inheritParams save_project
#' @param records A vector of records to be transformed. The transformation applies only to the specified records from the `project` object.
#'
#' @return A transformed version of the `project` object, where the data is rearranged horizontally, typically in a format where columns represent different variables or field values for each record.
#'
#' @details
#' The function begins by validating the `project` object and checking that the records to be transformed exist. It then reshapes the data from a vertical to a horizontal format, allowing for easier access and analysis, especially when dealing with wide datasets.
#' @export
generate_horizontal_transform <- function(project, records) {
  project <- assert_blank_project(project)
  if (missing(records)) records <- project$summary$all_records[[project$redcap$id_col]]
  data <- generate_summary(
    project,
    filter_field = project$redcap$id_col,
    filter_choices = records,
    include_log = FALSE,
    include_metadata = FALSE,
    include_record_summary = FALSE,
    include_users = FALSE
  )
  FINAL_out <- NULL
  forms <- names(data)
  col_names <- NULL
  max_by_record <- NULL
  ID_col <- project$redcap$id_col
  max_by_record <- data.frame(
    record = records,
    max = records %>% lapply(function(record) {
      forms %>%
        lapply(function(form) {
          length(which(data[[form]][[ID_col]] == record))
        }) %>%
        unlist() %>%
        max()
    }) %>% unlist()
  )
  for (form in forms) { # form <- forms %>% sample(1)
    col_names <- col_names %>% dplyr::bind_rows(
      data.frame(
        number = NA,
        form_name = form,
        field_name = names(data[[form]])
      )
    )
  }
  col_names$number <- seq_len(nrow(col_names))
  form_list <- forms %>%
    lapply(function(IN) {
      col_names$number[which(col_names$form_name == IN)]
    }) %>%
    unlist()
  for (form in forms) { # form <- forms %>% sample(1)
    the_cols <- form_list[[form]]
    out <- NULL
    # out <- data.frame(matrix(nrow = 0,ncol = the_cols %>% length()))
    # colnames(out) <- the_cols
    for (record in records) { # record <- records %>% sample(1)
      the_max <- max_by_record$max[which(max_by_record$record == record)]
      df <- data[[form]][which(data[[form]][[ID_col]] == record), ]
      colnames(df) <- the_cols
      n_blanks <- the_max - nrow(df)
      if (n_blanks > 0) {
        df_blanks <- data.frame(matrix(NA, ncol = ncol(df), nrow = n_blanks))
        colnames(df_blanks) <- colnames(df)
        df <- rbind(df, df_blanks)
      }
      out <- out %>% dplyr::bind_rows(df)
    }
    message("Done with ", form)
    FINAL_out <- FINAL_out %>% dplyr::bind_cols(out)
  }
  col_names2 <- col_names
  col_names2$number <- NULL
  col_names2 <- col_names2 %>%
    t() %>%
    as.data.frame()
  colnames(col_names2) <- colnames(FINAL_out)
  col_names2 <- all_character_cols(col_names2)
  FINAL_out <- all_character_cols(FINAL_out)
  FINAL_out <- col_names2 %>% dplyr::bind_rows(FINAL_out)
  return(FINAL_out)
}
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
    bullet_in_console("Successfully uploaded to REDCap!", bullet_type = "v")
    project$transformation$data_updates <- NULL
  } else {
    bullet_in_console("Nothing to upload!")
  }
  return(invisible(project))
}
#' @noRd
extract_form_from_merged <- function(project, form_name) {
  merged <- project$data[[project$internals$merge_form_name]]
  if (nrow(merged) > 0) {
    add_ons <- c(project$redcap$id_col, "arm_number", "event_name", "redcap_event_name", "redcap_repeat_instrument", "redcap_repeat_instance")
    add_ons <- add_ons[which(add_ons %in% colnames(merged))]
    if (!form_name %in% project$metadata$forms$form_name) stop("form_name must be included in set of project$metadata$forms$form_name")
    # form_name <-  project$metadata$forms$form_name %>% sample(1)
    is_repeating_form <- form_name %in% project$metadata$forms$form_name[which(project$metadata$forms$repeating)]
    rows <- seq_len(nrow(merged))
    if (is_repeating_form) {
      # rows <- which(merged$redcap_repeat_form==form_name)
    }
    if (!is_repeating_form) {
      rows <- which(!is.na(merged[[paste0(form_name, "_complete")]]))
    }
    #
    # if(!project$redcap$is_longitudinal){
    #   if("redcap_repeat_form"%in%colnames(merged)){
    #     if(is_repeating_form){
    #       rows <- which(merged$redcap_repeat_instrument==form_name)
    #     }
    #     if(!is_repeating_form){
    #       rows <- which(is.na(merged$redcap_repeat_instrument))
    #     }
    #   }
    # }
    # if(project$redcap$is_longitudinal){
    #   events_ins <- project$metadata$event_mapping$unique_event_name[which(project$metadata$event_mapping$form==form_name)] %>% unique()
    #   rows <- which(merged$redcap_event_name%in%events_ins)
    # }
    # if(!is_repeating_form){
    #   add_ons <- add_ons[which(!add_ons%in%c("redcap_repeat_instrument","redcap_repeat_instance"))]
    # }
    cols <- unique(c(add_ons, project$metadata$fields$field_name[which(project$metadata$fields$form_name == form_name & project$metadata$fields$field_name %in% colnames(merged))]))
    return(merged[rows, cols])
  }
}
#' @rdname default-transformations
#' @title Add Default Forms Transformation to the Database
#' @description
#' Applies default transformations to specific forms within the REDCap database (`project`).
#' This function modifies the `project` object to include default transformations, which may
#' involve adjustments, calculations, or reformatting of data in predefined forms.
#'
#' @inheritParams save_project
#' @param forms_transformation a data.frame that matches instruments. See `default_project_transformation` for an example.
#' @param ask logical for asking in console about changes to project forms_transformation
#' @return
#' The updated `project` object with default transformations applied to the specified forms.
#'
#' @details
#' This function is designed to streamline and standardize data processing by applying
#' default transformations to the database forms. The transformations are predefined
#' within the function and ensure consistency across datasets.
#'
#' @seealso
#' \code{\link{save_project}} for saving the database or subsets.
#' @export
add_default_project_transformation <- function(project) {
  project <- add_project_transformation(
    project = project,
    forms_transformation = default_project_transformation(project = project)
  )
  return(invisible(project))
}
#' @rdname default-transformations
#' @export
default_project_transformation <- function(project) {
  assert_setup_project(project)
  forms_transformation <- merge_non_repeating_project_transformation(project)
  forms_transformation$merge_to <- project$internals$merge_form_name
  forms_transformation$by.y <- forms_transformation$by.x <- forms_transformation$merge_to %>%
    lapply(function(form_name) {
      if (form_name %in% names(project$metadata$form_key_cols)) {
        project$metadata$form_key_cols[[form_name]] %>%
          paste0(collapse = "+") %>%
          return()
      } else {
        rows <- which(!forms_transformation$repeating)
        if (length(rows) == 0) {
          return(NA)
        }
        form_name <- forms_transformation$form_name[rows[[1]]]
        project$metadata$form_key_cols[[form_name]] %>%
          paste0(collapse = "+") %>%
          return()
      }
    }) %>%
    unlist()
  forms_transformation$x_first <- FALSE
  forms_transformation$x_first[which(forms_transformation$repeating)] <- TRUE
  return(forms_transformation)
}
#' @rdname default-transformations
#' @export
add_default_project_fields <- function(project) {
  forms <- project$metadata$forms
  last_non_rep <- forms$form_name[which(!forms$repeating)] %>% dplyr::last()
  form_names <- forms$form_name[which(forms$repeating)]
  # id_col <- project$metadata$form_key_cols[[last_non_rep]]
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
          id_col <- project$metadata$form_key_cols[[form_name]]
          project$data[[form_name]][[id_col]] %>%
            matches(project$data[[form]][[id_col]], count_only = TRUE) %>%
            as.character() %>%
            return()
        }
      )
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
        OUT <- NULL
        while (length(cols) > 0) {
          if (is.null(OUT)) {
            OUT <- project$data[[form_name]][[cols[1]]]
          } else {
            OUT <- OUT %>% paste0("_", project$data[[form_name]][[cols[1]]])
          }
          cols <- cols[-1]
        }
        return(OUT)
      }
    )
  }
  return(invisible(project))
}
#' @rdname default-transformations
#' @export
add_project_transformation <- function(project, forms_transformation, ask = TRUE) {
  if (missing(forms_transformation)) forms_transformation <- default_project_transformation(project)
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
    forms_tranformation_cols <- forms_tranformation_cols %>% append("repeating_via_events")
  }
  if (any(!names(forms_transformation) %in% forms_tranformation_cols)) {
    bullet_in_console("Use `add_default_forms_transformation(project)` is an example!")
    stop("forms_transformation needs the following colnames... ", forms_tranformation_cols %>% as_comma_string())
  }
  choice <- TRUE
  if (!is.null(project$transformation$forms)) {
    if (!identical(project$transformation$forms, forms_transformation)) {
      if (ask) {
        choice <- utils::askYesNo("Do you want to add transformation? (it doesn't match previous transform)")
        if (!choice) {
          stop("Stopped as you asked.")
        }
      }
    }
  }
  # add more checks
  project$transformation$forms <- forms_transformation
  return(invisible(project))
}
#' @title Add Field Transformation to the Database
#' @description
#' Adds a new field transformation to the REDCap database (`project`). This allows users to define custom transformations for a specific field in a form, including its type, label, choices, and associated function for data manipulation.
#'
#' @inheritParams save_project
#' @param field_name Character. The name of the field to which the transformation will be applied.
#' @param form_name Character. The name of the form containing the field.
#' @param field_type Character. The type of the field in REDCap (e.g., "text", "checkbox", "dropdown").
#' @param field_type_R Character. The corresponding R data type for the field. Default is `NA`.
#' @param field_label Character. The label for the field. Default is `NA`.
#' @param select_choices_or_calculations Character. A string specifying the choices (for dropdown, radio, or checkbox fields) or calculations (for calculated fields). Default is `NA`.
#' @param field_note Character. An optional note or comment for the field. Default is `NA`.
#' @param identifier Character. A string indicating whether the field is an identifier (e.g., "Y" for yes). Default is an empty string (`""`).
#' @param units Character. The units of measurement for the field, if applicable. Default is `NA`.
#' @param data_func Function or NA. An optional function to transform or validate the data in the field. Default is `NA`.
#'
#' @return
#' The updated `project` object with the field transformation added.
#'
#' @details
#' This function facilitates the addition of a new field transformation to a REDCap database. The transformation includes metadata such as the field's type, label, and choices, along with an optional function to process the data. This is particularly useful for customizing or extending the functionality of existing REDCap forms and fields.
#'
#' @seealso
#' \code{\link{save_project}} for saving the database or subsets.
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
  if (wl(project$transformation$fields$field_name == field_name) > 0) {
    project$transformation$fields <- project$transformation$fields[which(project$transformation$fields$field_name != field_name), ]
  }
  # if(!project$data %>% is_something())stop("Must have transformed data to add new vars.")
  fields <- project$metadata$fields
  in_original_redcap <- field_name %in% fields$field_name
  if (is_something(select_choices_or_calculations)) select_choices_or_calculations <- choice_vector_string(select_choices_or_calculations)
  if (in_original_redcap) {
    original_fields_row <- fields[which(fields$field_name == field_name), ]
    if (missing(form_name)) form_name <- original_fields_row$form_name
    if (missing(field_type)) {
      field_type <- original_fields_row$field_type
      field_type_R <- original_fields_row$field_type_R
    }
    if (is.na(field_label)) field_label <- original_fields_row$field_label
    if (is.na(select_choices_or_calculations)) select_choices_or_calculations <- original_fields_row$select_choices_or_calculations
    if (is.na(field_note)) field_note <- original_fields_row$field_note
    if (identifier == "") identifier <- original_fields_row$identifier
  }
  if (!is_something(data_func)) warning("if no `data_func` is provided, the column is only added to the metadata", immediate. = TRUE)
  if (is_something(data_func)) {
    func_template <- "data_func = function(project,field_name){YOUR FUNCTION}"
    if (!is.function(data_func)) stop("`data_func` must be a function ... ", func_template)
    allowed_args <- c("project", "field_name", "form_name")
    if (all(!allowed_args %in% names(formals(data_func)))) stop("`data_func` must have two aruguments (project and field_name) ... ", func_template)
    if (any(!names(formals(data_func)) %in% allowed_args)) stop("`data_func` can only have two aruguments (project and field_name) ... ", func_template)
  }
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
    field_func = data_func %>% function_to_string()
  )
  project$transformation$fields <- project$transformation$fields %>% dplyr::bind_rows(field_row)
  project$transformation$field_functions[[field_name]] <- data_func %>% clean_function()
  message("added '", field_name, "' column")
  return(invisible(project))
}
#' @noRd
combine_original_transformed_fields <- function(project) {
  the_names <- project$transformation$fields$field_name
  fields <- project$metadata$fields
  if (is.null(the_names)) {
    bullet_in_console("Nothing to add. Use `add_project_field()`", bullet_type = "x")
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
  return(fields)
}
#' @noRd
run_fields_transformation <- function(project) {
  the_names <- project$transformation$fields$field_name
  if (is.null(the_names)) {
    bullet_in_console("Nothing to run. Use `add_project_field()`", bullet_type = "x")
    return(invisible(project))
  }
  original_fields <- project$metadata$fields
  the_names_existing <- the_names[which(the_names %in% original_fields$field_name)]
  the_names_new <- the_names[which(!the_names %in% original_fields$field_name)]
  # fields_to_update <- NULL
  field_names <- c(the_names_existing, the_names_new)
  for (field_name in field_names) {
    OUT <- NA
    row_of_interest <- project$transformation$fields[which(project$transformation$fields$field_name == field_name), ]
    form_name <- row_of_interest$form_name
    field_func <- project$transformation$field_functions[[field_name]]
    environment(field_func) <- environment()
    if (is_something(field_func)) {
      if (form_name %in% names(project$data)) {
        OUT <- field_func(project = project, field_name = field_name, form_name = form_name)
      }
    }
    if (field_name %in% the_names_existing) {
      OLD <- project$data[[form_name]][[field_name]]
      if (!identical(OUT, OLD)) {
        ref_cols <- project$metadata$form_key_cols[[form_name]]
        new <- old <- project$data[[form_name]][, c(ref_cols, field_name)]
        new[[field_name]] <- OUT
        DF <- find_df_diff2(
          new = new,
          old = old,
          ref_cols = ref_cols,
          view_old = FALSE,
          message_pass = paste0(form_name, " - ", field_name, ": ")
        )
        if (is_something(DF)) {
          project$transformation$data_updates[[field_name]] <- DF
        }
      }
    }
    if (form_name %in% names(project$data)) {
      project$transformation$data[[form_name]][[field_name]] <- OUT
    }
  }
  bullet_in_console(paste0("Added new fields to ", project$short_name, " `project$data`"), bullet_type = "v")
  return(invisible(project))
}
#' @title transform_project
#' @description
#' Transforms the REDCap database (`project` object) by applying the necessary field transformations.
#' This function modifies the structure of the data and records according to the transformation rules specified.
#'
#' @details
#' This function checks if the database has already been transformed and applies the transformation if not. It stores the original column names before transforming the data. The transformation process can include modifying field values and renaming columns based on predefined transformation rules.
#'
#' @inheritParams save_project
#' @param reset Logical that forces transformation if TRUE. Default is `FALSE`.
#' @return The transformed `project` object.
#' @seealso
#' \code{\link[REDCapSync]{save_project}} for saving the transformed database object.
#' @family db_functions
#' @export
transform_project <- function(project, reset = FALSE) {
  has_transformation <- is_something(project$transformation$forms)
  has_data <- is_something(process_df_list(project$data, silent = TRUE))
  is_transformed <- project$internals$is_transformed
  if (!has_data) {
    bullet_in_console("No data... nothing to do!", bullet_type = "x")
    return(invisible(project))
  }
  if (!has_transformation) {
    bullet_in_console("Nothing to run. Use `add_project_field()`", bullet_type = "x")
    return(invisible(project))
  }
  if (is_transformed && !reset) {
    bullet_in_console("Already transformed... nothing to do!", bullet_type = "x")
    return(invisible(project))
  }
  if (!is_transformed || reset) {
    forms_transformation <- project$transformation$forms
    forms_transformation_original <- forms_transformation
    project$transformation$data <- project$data
    project <- run_fields_transformation(project)
    named_df_list <- project$transformation$data
    OUT <- NULL
    for (i in (seq_len(nrow(forms_transformation)))) {
      TABLE <- forms_transformation$form_name[i]
      ref <- named_df_list[[TABLE]]
      if (!is.null(ref)) {
        if (is_something(ref)) {
          a <- forms_transformation[i, ]
          z <- as.list(a)
          ref <- named_df_list[[TABLE]]
          rownames(ref) <- NULL
          by.x <- z$by.x <- z$by.x %>%
            strsplit("\\+") %>%
            unlist()
          by.y <- z$by.y <- z$by.y %>%
            strsplit("\\+") %>%
            unlist()
          if (length(z$by.x) != length(z$by.y)) stop("by.x and by.y must be same length... [", z$form_name, "] (", z$by.x %>% as_comma_string(), ") AND (", z$by.y %>% as_comma_string(), ")")
          if (TABLE == z$merge_to) {
            OUT[[z$form_name_remap]] <- ref
          } else {
            mer <- named_df_list[[z$merge_to]]
            if (z$merge_to %in% names(OUT)) {
              mer <- OUT[[z$merge_to]]
            }
            ref_names <- names(ref)
            mer_names <- names(mer)
            # new_name <- by.x %>% vec1_not_in_vec2(by.y)
            new_names <- ref_names %>%
              vec1_in_vec2(mer_names) %>%
              vec1_not_in_vec2(by.x)
            for (new_name in new_names) {
              COL <- which(colnames(mer) == new_name)
              replace_name <- paste0(new_name, "_merged")
              a <- mer[, 1:COL]
              a[[replace_name]] <- a[[COL]]
              b <- mer[, (COL + 1):ncol(mer)]
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
            # new_name <- by.x %>% vec1_not_in_vec2(by.y)
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
            OUT[[z$form_name_remap]] <- a
          }
        }
      }
    }
    if (any(!names(OUT) %in% unique(forms_transformation$form_name_remap))) stop("not all names in OUT objext. Something wrong with transform_project()")
    if (is_something(OUT)) {
      project$transformation$data <- OUT
    }
    # forms_transformation <- annotate_forms(project,summarize_data = FALSE)
    if (!is.null(project$metadata$form_key_cols)) {
      forms_transformation$key_cols <- forms_transformation$form_name %>%
        lapply(function(IN) {
          project$metadata$form_key_cols[[IN]] %>% paste0(collapse = "+")
        }) %>%
        unlist()
      forms_transformation$key_names <- forms_transformation$form_name %>%
        lapply(function(IN) {
          row_match <- which(forms_transformation$form_name == IN)
          if (!forms_transformation$repeating[row_match]) {
            return(project$metadata$form_key_cols[[IN]])
          }
          return(paste0(forms_transformation$form_name[row_match], "_key"))
        })
    }
    project$internals$is_transformed <- TRUE
    bullet_in_console(paste0(project$short_name, " transformed according to `project$transformation`"), bullet_type = "v")
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
    fields <- combine_original_transformed_fields(project)
    fields$original_form_name <- fields$form_name
    fields$form_name <- forms_transformation_original$form_name_remap[match(fields$form_name, forms_transformation_original$form_name)]
    fields <- fields[order(match(fields$form_name, forms_transformation$form_name)), ]
    # new function RosyUtils
    first <- 1:which(colnames(fields) == "form_name")
    move <- which(colnames(fields) == "original_form_name")
    last <- which(colnames(fields) != "original_form_name")[-first]
    fields <- fields[, c(first, move, last)]
    project$transformation$metadata$fields <- fields
    bullet_in_console(paste0("Added mod fields to ", project$short_name, " `project$transformation`"), bullet_type = "v")
    project$transformation$metadata$choices <- fields_to_choices(fields)
    project$transformation$metadata$form_key_cols <- get_key_col_list(project = project, transform = TRUE)
    project$transformation$metadata$missing_codes <- project$metadata$missing_codes
    project$internals$last_data_transformation <- now_time()
  }
  return(invisible(project))
}
#' @noRd
missing_form_names <- function(project) {
  form_names <- names(project$data)
  form_names <- form_names[which(!form_names %in% project$metadata$forms$form_name)]
  return(form_names)
}
#' @noRd
missing_field_names <- function(project) {
  # md <- data.frame(
  #   field_name = project$metadata$fields$field_name,
  #   form_name = project$metadata$fields$form_name
  # )
  # d <- project$data %>%
  #   names() %>%
  #   lapply(function(form_name) {
  #     data.frame(
  #       form_name = form_name,
  #       field_name = colnames(project$data[[form_name]])
  #     )
  #   }) %>%
  #   dplyr::bind_rows()
  # # return(form_names)
}
