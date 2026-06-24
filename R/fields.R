#' @noRd
add_project_field <- function(project,
                              field_name,
                              form_name,
                              field_type_r = "character",
                              field_label = NA,
                              field_choices = NA,
                              field_note = NA,
                              identifier = "",
                              units = NA,
                              data_func = NA) {
  project <- assert_blank_project(project)
  assert_env_name(field_name, max.chars = 31L) # unsure max name length
  assert_choice(field_type_r, choices = FIELD_TYPES_R)
  fields <- project$metadata$fields
  in_original_redcap <- field_name %in% fields$field_name
  if (is_something(field_choices)) {
    field_choices <- choice_vector_string(field_choices)
  }
  if (in_original_redcap) {
    # add check for
    original_fields_row <- fields[which(fields$field_name == field_name), ]
    if (!missing(form_name)) {# check this?
      # warning if chose wrong form?
      if (!identical(form_name, original_fields_row$form_name)){
        stop("If you modify existing field, you cannot change form_name")
      }
    }
    form_name <- original_fields_row$form_name
    if (is.na(field_type_r)) {
      field_type_r <- field_types_to_r(original_fields_row)
    }
    if (is.na(field_label)) {
      field_label <- original_fields_row$field_label
    }
    if (is.na(field_choices)) {
      field_choices <- original_fields_row$select_choices_or_calculations
    }
    if (is.na(field_note)) {
      field_note <- original_fields_row$field_note
    }
    if (identifier == "") {
      identifier <- original_fields_row$identifier
    }
  }
  assert_choice(form_name, project$metadata$forms$form_name)
  #other asserts
  has_data_func <- FALSE
  if (!is_something(data_func)) {
    warning("if no `data_func` provided, then field only added to metadata",
            immediate. = TRUE)
  }
  if (is_something(data_func)) {
    func_template <- "data_func = function(project){...YOUR FUNCTION...}"
    if (!is.function(data_func)) {
      stop("`data_func` must be a function ... ", func_template)
    }
    allowed_args <- c("project", "field_name", "form_name")
    if (!any(allowed_args %in% names(formals(data_func))) ||
        !all(names(formals(data_func)) %in% allowed_args)) {
      stop(
        "`data_func` must have \"project\" as only paramter...",
        func_template # add vignettte
      )
    }
    data_func <- clean_function(data_func)
    has_data_func <- TRUE
  }
  field_row <- data.frame(
    field_name = as.character(field_name),
    form_name = as.character(form_name),
    field_type = as.character("text"),
    field_label = as.character(field_label),
    select_choices_or_calculations = as.character(field_choices),
    field_note = as.character(field_note),
    identifier = as.character(identifier),
    field_type_r = as.character(field_type_r),
    units = as.character(units),
    in_original_redcap = as.logical(in_original_redcap),
    field_label_short = as.character(field_label),
    stringsAsFactors = FALSE
  )
  row_match <- which(project$transformation$fields$field_name == field_name)
  included_already <- length(row_match) > 0L
  if (included_already) {
    compare_this <- project$transformation$fields[row_match, ]
    rownames(compare_this) <- NULL
    rownames(field_row) <- NULL
    if (identical(field_row, compare_this)) {
      # should there be a message?
      return(invisible(project)) #return if the same
    }
  }
  row_match <- which(project$transformation$fields$field_name != field_name)
  project$transformation$fields <- project$transformation$fields[row_match, ]
  project$transformation$fields <-
    project$transformation$fields |>
    dplyr::bind_rows(field_row)
  project$transformation$field_functions[[field_name]] <- data_func
  if(has_data_func) {
    project$transformation$data[[form_name]][[field_name]] <- project |>
      render_field(field_name) # should this be moved up or checked?
  }
  project <- reset_project_datasets(project)
  # ADD clear of datasets if new field added
  cli_alert_success("added \"{field_name}\" column")
  invisible(project)
}
#' @noRd
clean_function <- function(func) {
  if (!is.function(func)) {
    stop("Input must be a function")
  }
  environment(func) <- emptyenv()
  func
}
#' @noRd
choice_vector_string <- function(vec) {
  if (!is_something(vec)) {
    return(NA)
  }
  return(paste0(paste0(seq_along(vec), ", ", vec), collapse = " | "))
}
#' @noRd
remove_project_fields <- function(project) {
  assert_setup_project(project)
  project$transformation <- list(
    custom = NULL,
    data = NULL,
    fields = NULL,
    field_functions = NULL,
    data_updates = NULL
  )
  cli_alert_success("Cleared project transformations!")
  invisible(project)
}
#' @noRd
render_field <- function(project, field_name) {
  assert_choice(field_name, project$transformation$fields$field_name)
  assert_choice(field_name, names(project$transformation$field_functions))
  field <- NA
  row_match <- which(project$transformation$fields$field_name == field_name)
  form_name <- project$transformation$fields$form_name[row_match]
  field_func <- project$transformation$field_functions[[field_name]]
  field <- NULL
  if(is_something(field_func)) {
    environment(field_func) <- environment()
    field <- try_else_null({field_func(project = project)})
    if(!is.null(field)) {
      if(length(field) != nrow(project$data[[form_name]])) {
        cli_alert_warning(paste0("added field `{field_name}` has different ",
                                "length than `nrow(project$data${form_name})`"))
        field <- NULL
      }
    }
    if(is.null(field)) {
      cli_alert_danger("Failed to render added field `{field_name}`")
    }
  } # consider requring to return dataframe with keys and field_name
  field
}
#' @noRd
rerender_fields <- function(project) {
  if (is_something(project$transformation$fields)) {
    for(field_name in project$transformation$fields$field_name) {
      row_match <- which(project$transformation$fields$field_name == field_name)
      form_name <- project$transformation$fields$form_name[row_match]
      project$transformation$data[[form_name]][[field_name]] <- project |>
        render_field(field_name)
    }
  }
  project
}
#' @noRd
combine_project_fields <- function(data_list, transformation) {
  the_names <- transformation$fields$field_name
  fields <- data_list$metadata$fields
  if (is.null(the_names)) {
    cli_alert_danger("Nothing to add. Use `add_project_field()`")
    return(fields)
  }
  for (field_name in the_names) {
    row_match <- which(transformation$fields$field_name == field_name)
    field_row <- transformation$fields[row_match, ]
    form_name <- field_row$form_name
    # if(any(fields$field_name==field_name))stop("field_name already included")
    current_row <- which(fields$field_name == field_name)
    if (length(current_row) > 0L) {
      fields <- fields[-current_row, ]
      i <- current_row
      if (i > 1L) {
        i <- i - 1L
      }
    } else {
      i <- which(
        fields$form_name == form_name &
          fields$field_name == paste0(form_name, "_complete")
      )
      if (length(i) > 0L) {
        if (i[[1L]] > 1L) {
          i <- i - 1L
        }
      }
      if (length(i) == 0L) {
        i <- which(fields$form_name == form_name)
      }
      if (length(i) > 1L) {
        i <- i[[1L]]
      }
      if (length(i) == 0L) {
        i <- nrow(fields)
      }
    }
    if (length(i) == 0L) {
      stop("insert_after error")
    }
    top <- fields[1L:i, ]
    bottom <- NULL
    if (i < nrow(fields)) {
      bottom <- fields[(i + 1L):nrow(fields), ]
    }
    fields <- top |>
      dplyr::bind_rows(field_row) |>
      dplyr::bind_rows(bottom)
  }
  fields
}
#' @noRd
add_fields_to_data_list <- function(data_list, transformation) {
  metadata <- data_list$metadata
  named_df_list <- data_list$data
  the_names <- transformation$fields$field_name
  has_fields <- !is.null(the_names)
  original_fields <- metadata$fields
  names_existing <- the_names[which(the_names %in% original_fields$field_name)]
  names_new <- the_names[which(!the_names %in% original_fields$field_name)]
  field_names <- c(names_existing, names_new) |> unique()
  for (field_name in field_names) {
    row_match <- which(transformation$fields$field_name == field_name)
    form_name <- transformation$fields$form_name[row_match]
    if (form_name %in% names(named_df_list)) {
      field <- transformation$data[[form_name]][[field_name]]
      if (!is.null(field)) {
        # warning here?
        named_df_list[[form_name]][[field_name]] <- field
      }
    }
  }
  fields <- combine_project_fields(data_list, transformation)
  fields$original_form_name <- fields$form_name
  # fields$form_name <- forms_transformation_original$form_name_remap[match(fields$form_name, forms_transformation_original$form_name)]
  # fields <- fields[order(match(fields$form_name, forms_transformation$form_name)), ]
  # new function RosyUtils
  first <- 1L:which(colnames(fields) == "form_name")
  move <- which(colnames(fields) == "original_form_name")
  last <- which(colnames(fields) != "original_form_name")[-first]
  fields <- fields[, c(first, move, last)]
  metadata$fields <- fields
  metadata$choices <- fields_to_choices(fields)
  metadata$form_key_cols <- get_key_col_list(data_list = data_list)
  cli_alert_success("Added new fields")
  data_list$metadata <- metadata
  data_list$data <- named_df_list
  data_list
}
