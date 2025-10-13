#' @noRd
fields_to_choices <- function(fields) {
  fields <- fields[which(fields$field_type %in% c("radio", "dropdown", "checkbox_choice", "yesno")), ]
  # fields$field_name[which(fields$field_type=="checkbox_choice")] <- fields$field_name[which(fields$field_type=="checkbox_choice")] %>% strsplit("___") %>% lapply(function(X){X[[1]]})
  fields <- fields[which(!is.na(fields$select_choices_or_calculations)), ]
  choices <- NULL
  for (i in seq_len(nrow(fields))) {
    field_name <- fields$field_name[i]
    form_name <- fields$form_name[i]
    field_label <- fields$field_label[i]
    field_type <- fields$field_type[i]
    selections <- fields$select_choices_or_calculations[i] %>% split_choices()
    choices <- choices %>% dplyr::bind_rows(
      data.frame(
        form_name = form_name,
        field_name = field_name,
        field_type = field_type,
        field_label = ifelse(!is.na(field_label), field_label, field_name),
        code = selections$code,
        name = selections$name
      )
    )
  }
  choices$label <- paste(choices$form_name, "-", choices$field_label, "-", choices$name)
  rownames(choices) <- NULL
  choices
}
#' @noRd
add_labels_to_checkbox <- function(fields) {
  row_index <- which(fields$field_type == "checkbox_choice")
  x <- fields$field_name[row_index] %>%
    strsplit("___") %>%
    lapply(function(x) {
      x[[1]]
    }) %>%
    unlist()
  y <- fields$field_label[row_index]
  z <- paste0(fields$field_label[match(x, fields$field_name)], " - ", y)
  fields$field_label[row_index] <- z
  fields
}
#' @noRd
annotate_fields <- function(project, summarize_data = TRUE, drop_blanks = TRUE) {
  fields <- project$metadata$fields # [,colnames(project$metadata$fields)]
  if (drop_blanks) {
    fields <- fields[which(fields$field_name %in% get_all_field_names(project)), ]
  }
  if (nrow(fields) > 0) {
    fields <- fields[which(fields$field_type != "descriptive"), ]
    fields <- add_labels_to_checkbox(fields)
    fields <- fields[which(fields$field_type != "checkbox"), ]
    fields$field_label[which(is.na(fields$field_label))] <- fields$field_name[which(is.na(fields$field_label))]
    fields <- unique(fields$form_name) %>%
      lapply(function(x) {
        fields[which(fields$form_name == x), ]
      }) %>%
      dplyr::bind_rows()
    if (!"field_type_R" %in% colnames(fields)) fields$field_type_R <- "character"
    fields$field_type_R[which(fields$field_type %in% c("radio", "yesno", "dropdown", "checkbox_choice"))] <- "factor"
    fields$field_type_R[which(fields$text_validation_type_or_show_slider_number == "integer")] <- "integer"
    fields$field_type_R[which(fields$text_validation_type_or_show_slider_number == "date_mdy")] <- "date"
    fields$field_type_R[which(fields$text_validation_type_or_show_slider_number == "date_ymd")] <- "date"
    fields$field_type_R[which(fields$text_validation_type_or_show_slider_number == "datetime_dmy")] <- "datetime"
    fields$in_original_redcap <- TRUE
    fields$original_form_name <- fields$form_name
    if (project$internals$is_transformed) {
      fields$in_original_redcap <- fields$field_name %in% project$transformation$original_fields$field_name
      fields$original_form_name <- project$transformation$original_fields$form_name[match(fields$field_name, project$transformation$original_fields$field_name)]
    }
    if (!"units" %in% colnames(fields)) fields$units <- NA
    if (!"field_label_short" %in% colnames(fields)) fields$field_label_short <- fields$field_label
    # if(!"field_label_short" %in% colnames(fields))fields$ <- fields$field_label
    if (summarize_data) {
      skimmed <- NULL
      for (form_name in unique(fields$form_name)) {
        col_names <- fields$field_name[which(fields$form_name == form_name)]
        form <- project$data[[form_name]]
        col_names <- col_names[which(col_names %in% colnames(form))]
        skimmed <- skimmed %>% dplyr::bind_rows(form[, col_names] %>% skimr::skim())
      }
      field_names <- fields$field_name
      fields <- fields %>% merge(skimmed, by.x = "field_name", by.y = "skim_variable", all = TRUE)
      fields <- field_names %>%
        lapply(function(x) {
          fields[which(fields$field_name == x), ]
        }) %>%
        dplyr::bind_rows()
    }
    # cli_alert_wrap("Annotated `project$metadata$fields`",bullet_type = "v")
  }
  fields
}
#' @noRd
annotate_forms <- function(project, summarize_data = TRUE, drop_blanks = TRUE) {
  forms <- project$metadata$forms
  if (drop_blanks) {
    forms <- forms[which(forms$form_name %in% names(project$data)), ]
  }
  if (nrow(forms) > 0) {
    # add metadata info like n fields
    if (summarize_data) {
      for (status in c("Incomplete", "Unverified", "Complete")) {
        forms[[tolower(status)]] <- forms$form_name %>%
          lapply(function(form_name) {
            form_name %>%
              strsplit(" [:|:] ") %>%
              unlist() %>%
              lapply(function(form_name) {
                (project$data[[form_name]][[paste0(form_name, "_complete")]] == status) %>%
                  which() %>%
                  length()
              }) %>%
              unlist() %>%
              paste0(collapse = " | ")
          }) %>%
          unlist()
      }
    }
  }
  forms
}
#' @noRd
annotate_choices <- function(project, summarize_data = TRUE, drop_blanks = TRUE) {
  # forms <- project$metadata$forms
  # fields <- project$metadata$fields
  choices <- project$metadata$choices
  if (drop_blanks) {
    choices <- choices[which(choices$field_name %in% get_all_field_names(project)), ]
  }
  # choices$field_name_raw <- choices$field_name
  # choices$field_name_raw[which(choices$field_type=="checkbox_choice")] <- choices$field_name[which(choices$field_type=="checkbox_choice")] %>%
  #   strsplit("___") %>%
  #   lapply(function(X){X[[1]]})
  # choices$field_label_raw <- choices$field_label
  # choices$field_label_raw[which(choices$field_type=="checkbox_choice")] <- choices$field_name_raw[which(choices$field_type=="checkbox_choice")] %>%
  #   lapply(function(X){
  #     project$metadata$fields$field_label[which(fields$field_name==X)] %>% unique()
  #   })
  if (summarize_data) {
    choices$n <- seq_len(nrow(choices)) %>%
      lapply(function(i) {
        form <- project$data[[choices$form_name[i]]]
        if (is.null(form)) {
          return(0)
        }
        if (nrow(form) == 0) {
          return(0)
        }
        sum(form[, choices$field_name[i]] == choices$name[i], na.rm = TRUE)
        # print(i)
      }) %>%
      unlist()
    choices$n_total <- seq_len(nrow(choices)) %>%
      lapply(function(i) {
        form <- project$data[[choices$form_name[i]]]
        if (is.null(form)) {
          return(0)
        }
        if (nrow(form) == 0) {
          return(0)
        }
        sum(!is.na(form[, choices$field_name[i]]), na.rm = TRUE)
      }) %>%
      unlist()
    choices$perc <- (choices$n / choices$n_total) %>% round(4)
    choices$perc_text <- choices$perc %>%
      magrittr::multiply_by(100) %>%
      round(1) %>%
      paste0("%")
    # project$summary$choices <- choices
    # cli_alert_wrap("Annotated `project$summary$choices`",bullet_type = "v")
  }
  choices
}
#' @noRd
annotate_records <- function(project) {
  id_col <- project$redcap$id_col
  all_records <- project$summary$all_records
  redcap_log <- project$redcap$log[which(!is.na(project$redcap$log$record)), ]
  if (! is_something(all_records) || ! is_something(redcap_log)) {
    return(project)
  }
  redcap_log <- redcap_log[which(redcap_log$action_type != "Users"), ]
  redcap_log <- redcap_log[which(redcap_log$record %in% all_records[[id_col]]), ]
  cool_list <- split(redcap_log$timestamp, redcap_log$record)
  cool_list_match <- cool_list %>% names() %>% match(all_records[[id_col]])
  # test matched all_records[[id_col]][cool_list_match]
  cool_list_first <- cool_list %>% lapply(dplyr::first) %>% unlist()
  cool_list_last <- cool_list %>% lapply(dplyr::last) %>% unlist()
  all_records$first_timestamp[cool_list_match] <- cool_list_first
  all_records$last_timestamp[cool_list_match] <- cool_list_last
  invisible(project)
}
#' @noRd
fields_with_no_data <- function(project) {
  project$metadata$fields$field_name[which(is.na(project$metadata$fields$complete_rate) & !project$metadata$fields$field_type %in% .field_types_not_in_data)]
}
#' @noRd
clean_form <- function(form, fields, drop_blanks = TRUE, drop_others = NULL) {
  for (field_name in colnames(form)) {
    if (field_name %in% fields$field_name) {
      row <- which(fields$field_name == field_name)
      units <- NULL
      if (!is.na(fields$units[row])) {
        units <- fields$units[row]
      }
      class <- fields$field_type_R[row][[1]]
      label <- ifelse(is.na(fields$field_label[row]), field_name, fields$field_label[row])[[1]]
      levels <- NULL
      if (!is.na(class)) {
        if (class == "factor") {
          select_choices <- fields$select_choices_or_calculations[row]
          if (!is.na(select_choices)) {
            levels <- split_choices(select_choices)[[2]]
          } else {
            levels <- unique(form[[field_name]]) %>% drop_nas()
          }
          if (anyDuplicated(levels) > 0) {
            duplicate_levels <- levels %>%
              duplicated() %>%
              which()
            warning("You have a variable (", field_name, ") with dupplicate names (", levels[duplicate_levels] %>% toString(), "). This is not great but for this proccess they will be merged and treated as identical responses.")
            levels <- levels %>% unique()
          }
          if (drop_blanks) {
            levels <- levels[which(levels %in% unique(form[[field_name]]))]
          }
          if (!is.null(drop_others)) {
            if (length(drop_others) > 0) levels <- levels[which(!levels %in% drop_others)]
          }
        }
        if (class == "integer") {
          form[[field_name]] <- as.integer(form[[field_name]])
        }
        if (class == "numeric") {
          form[[field_name]] <- as.numeric(form[[field_name]])
        }
        form
      }
      form[[field_name]] <- form[[field_name]] %>% clean_column_for_table(
        class = class,
        label = label,
        units = units,
        levels = levels
      )
    }
  }
  form
}
#' @noRd
clean_column_for_table <- function(field, class, label, units, levels) {
  if (!missing(class)) {
    if (!is.null(class)) {
      if (!is.na(class)) {
        if (class == "integer") {
          field <- field %>% as.integer()
        }
        if (class == "factor") {
          field <- field %>% factor(levels = levels, ordered = TRUE)
        }
        if (class == "numeric") {
          field <- field %>% as.numeric()
        }
      }
    }
  }
  if (!missing(label)) {
    if (!is.null(label)) {
      if (!is.na(label)) {
        attr(field, "label") <- label
      }
    }
  }
  if (!missing(units)) {
    if (!is.null(units)) {
      if (!is.na(units)) {
        attr(field, "units") <- units
      }
    }
  }
  field
}
#' @title Add a Subset to a REDCap Database
#' @description
#' `r lifecycle::badge("experimental")`
#' Creates a summary of the main REDCap database (`project`) based on specific
#' filter criteria and saves it to a specified directory. The summary can be
#' further customized with additional forms, fields, and deidentification
#' options.
#'
#' @inheritParams save_project
#' @inheritParams setup_project
#' @param summary_name Character. The name of the summary to create.
#' @param transform Logical. Whether to transform the data in the summary.
#' Default
#' is `TRUE`.
#' @param filter_field Character. The name of the field in the database to
#' filter on.
#' @param filter_choices Vector. The values of `filter_field` used to define the
#' summary.
#' @param filter_list Vector. The values of `filter_field` used to define the
#' summary.
#' @param filter_strict Logical. If `TRUE`, all forms will be filtered by
#' criteria. If `FALSE`, will convert original filter to id column and filter
#' all other forms by that record. Default is `TRUE`.
#' @param dir_other Character. The directory where the summary file will be
#' saved. Default is the `output` folder within the database directory.
#' @param file_name Character. The base name of the file where the summary will
#' be saved. Default is `<project$short_name>_<summary_name>`.
#' @param form_names Character vector. Names of forms to include in the summary.
#' Default is `NULL`, which includes all forms.
#' @param field_names Character vector. Names of specific fields to include in
#' the summary. Default is `NULL`, which includes all fields.
#' @param exclude_identifiers Logical. Whether to exlude identifiers in the data
#' in the summary. Default is `TRUE`.
#' @param exclude_free_text Logical for excluding free text. Default is `FALSE`.
#' @param date_handling character string. One of `none`,`lowest-overall-zero`,
#' `lowest-record-zero`, `shuffle-record-randomly`, or zero date date in form of
#' `2012-12-05`
#' @param upload_compatible Logical. If `TRUE`, the data will be compatible with
#' REDCap API upload. The main conflict is numeric or date variables in a
#' project with missing codes while `clean` = `TRUE`. R converts these to `NA`.
#' Default is `TRUE`.
#' @param clean Logical. If `TRUE`, the data will be cleaned before summarizing.
#' Default is `TRUE`.
#' @param drop_blanks Logical. If `TRUE`, records with blank fields will be
#' dropped. Default is `TRUE`.
#' @param drop_missings Logical. If `TRUE`, will convert missing codes to NA.
#' Default is `FALSE`.
#' @param drop_others Character vector of other values that should be dropped.
#' @param include_metadata Logical. If `TRUE`, metadata will be included in the
#' summary. Default is `TRUE`.
#' @param annotate_metadata Logical. If `TRUE`, metadata will be annotated in
#' the summary. Default is `TRUE`.
#' @param include_record_summary Logical. If `TRUE`, a record summary will be
#' included in the generated summary. Default is `TRUE`.
#' @param include_users Logical. If `TRUE`, user-related information will be
#' included in the summary. Default is `TRUE`.
#' @param include_log Logical. If `TRUE`, the log of changes will be included in
#' the summary. Default is `TRUE`.
#' @param no_duplicate_cols A logical flag (`TRUE` or `FALSE`). If `TRUE`, the
#' function will avoid including duplicate columns in the output. Defaults to
#' `FALSE`.
#' @param hard_reset Logical. If `TRUE`, overwrite existing summary files with the
#' same name. Default is `FALSE`.
#' @param with_links Optional logical (TRUE/FALSE) for including links in Excel
#' sheets. Default is `FALSE`.
#' @param separate Optional logical (TRUE/FALSE) separating each form into
#' separate files as opposed to multi-tab Excel. Default is `FALSE`.
#' @return
#' A modified `project` object that includes the newly created summary.
#' The summary is also saved as a file in the specified directory.
#'
#' @details
#' This function filters the main REDCap database using the specified
#' `filter_field`
#' and `filter_choices`, then creates a new summary with optional
#' deidentification. It can be customized to include only specific forms or
#' fields. The resulting summary is saved to a file for future use.
#'
#' @seealso
#' \code{\link{save_project}} for saving the main database or summaries.
#' @export
add_project_summary <- function(
    project,
    summary_name,
    transform = TRUE,
    filter_field = NULL,
    filter_choices = NULL,
    filter_list = NULL,
    filter_strict = TRUE,
    field_names = NULL,
    form_names = NULL,
    no_duplicate_cols = FALSE,
    exclude_identifiers = TRUE,
    exclude_free_text = FALSE,
    date_handling = "none",
    upload_compatible = TRUE,
    labelled = TRUE,
    clean = TRUE,
    drop_blanks = TRUE,
    drop_missings = FALSE,
    drop_others = NULL,
    include_metadata = TRUE,
    annotate_metadata = TRUE,
    include_record_summary = TRUE,
    include_users = TRUE,
    include_log = FALSE,
    with_links = TRUE,
    separate = FALSE,
    use_csv,
    dir_other = file.path(project$dir_path, "output"),
    file_name = paste0(project$short_name, "_", summary_name),
    hard_reset = FALSE) {
  lifecycle::signal_stage("experimental", "add_project_summary()")
  # sync_frequency ... project$internals$sync_frequency
  forbiden_summary_names <- c(
    project$redcap$id_col,
    .forbiden_summary_names
  )
  if (summary_name %in% forbiden_summary_names) {
    stop(summary_name, " is a forbidden summary name. Used for REDCapSync.")
  }
  if (missing(use_csv)) use_csv <- project$internals$use_csv
  if (is.null(filter_list)) {
    if (!is.null(filter_choices) && !is.null(filter_field)) {
      filter_list <- list(filter_choices)
      names(filter_list) <- filter_field
    } else {
      # warning
    }
  }
  file_ext <- ifelse(use_csv, ".csv", ".xlsx")
  summary_list_new <- list(
    summary_name = summary_name,
    transform = transform,
    filter_list = filter_list,
    filter_strict = filter_strict,
    field_names = field_names,
    form_names = form_names,
    no_duplicate_cols = no_duplicate_cols,
    exclude_identifiers = exclude_identifiers,
    exclude_free_text = exclude_free_text,
    date_handling = date_handling,
    upload_compatible = upload_compatible,
    labelled = labelled,
    clean = clean,
    drop_blanks = drop_blanks,
    drop_missings = drop_missings,
    drop_others = drop_others,
    include_metadata = include_metadata,
    annotate_metadata = annotate_metadata,
    include_record_summary = include_record_summary,
    include_users = include_users,
    include_log = include_log,
    with_links = with_links,
    separate = separate,
    use_csv = use_csv,
    dir_other = dir_other,
    file_name = file_name,
    file_path = file.path(dir_other, paste0(file_name, file_ext)),
    n_records = NULL,
    last_save_time = NULL,
    final_form_tab_names = NULL
  )
  summary_list_old <- project$summary[[summary_name]]
  if (!is.null(summary_list_old) && ! hard_reset) {
    important_vars <- names(summary_list_new) %>%
      vec1_not_in_vec2(.not_important_summary_names)
    are_identical <- identical(
      summary_list_new[important_vars],
      summary_list_old[important_vars]
    )
    if (are_identical) {
      # optional message?
      return(invisible(project))
    }
  }
  project$summary[[summary_name]] <- summary_list_new
  project$summary$all_records[[summary_name]] <- FALSE
  invisible(project)
}
#' @noRd
.forbiden_summary_names <- c(
  "first_timestamp",
  "last_timestamp",
  "last_api_call",
  "all_records",
  "was_transformed",
  "was_saved"
)
.not_important_summary_names <- c(
  "n_records",
  "last_save_time",
  "final_form_tab_names"
)
#' @noRd
save_summary <- function(project, summary_name) {
  id_col <- project$redcap$id_col
  summary_list <- project$summary[[summary_name]]
  to_save_list <- project %>%
    generate_project_summary(
      summary_name = summary_name
    )
  link_col_list <- list()
  if (summary_list$with_links) {
    if (project$internals$project_type == "redcap") {
      form_names <- names(project$data)
      if(summary_list$transform){
        form_names <- names(project$transformation$data)
      }
      add_links <- which(names(to_save_list) %in% form_names)
      if (length(add_links) > 0) {
        to_save_list[add_links] <- to_save_list[add_links] %>%
          lapply(function(form) {
            add_redcap_links_to_form(form, project)
          })
        link_col_list <- list(
          "redcap_link"
        )
        names(link_col_list) <- id_col
      }
    }
  }
  header_df_list <- to_save_list %>%
    construct_header_list(fields = project$metadata$fields) %>%
    process_df_list(silent = TRUE)
  key_cols_list <- project$metadata$form_key_cols
  if(summary_list$transform){
    key_cols_list <- project$transformation$metadata$form_key_cols
  }
  if (summary_name == "REDCapSync_raw") {
    header_df_list <- NULL
    key_cols_list <- NULL
  }
  if (summary_list$use_csv) {
    to_save_list %>% list_to_csv(
      dir = summary_list$dir_other,
      file_name = summary_list$file_name,
      overwrite = TRUE
    ) # account for links with CSV like new column
  } else {
    list_to_excel(
      list = to_save_list,
      dir = summary_list$dir_other,
      separate = summary_list$separate,
      link_col_list = link_col_list,
      key_cols_list = key_cols_list,
      # derived_cols_list = derived_cols_list,
      file_name = summary_list$file_name,
      header_df_list = header_df_list,
      overwrite = TRUE
    )
  }
  records <- to_save_list %>%
    lapply(function(form) {
      form[[id_col]]
    }) %>%
    unlist() %>%
    sort() %>%
    unique()
  project$summary[[summary_name]]$n_records <- length(records)
  project$summary[[summary_name]]$last_save_time <-  now_time()
  project$summary[[summary_name]]$final_form_tab_names <-
    rename_list_names_excel(list_names = names(to_save_list))
  names(project$summary[[summary_name]]$final_form_tab_names) <-
    names(to_save_list)
  row_match <- which(project$summary$all_records[[id_col]] %in% records)
  project$summary$all_records[[summary_name]][row_match] <- TRUE
  project$summary$all_records$was_saved[row_match] <- TRUE
  project$internals$last_summary <- now_time()
  invisible(project)
}
#' @title Generate a Summary from a Subset Name
#' @description
#' `r lifecycle::badge("experimental")`
#' Generates a summary from a predefined summary of data within a REDCap project.
#' The summary can be customized based on various options, such as cleaning the
#' data, including metadata, and annotating metadata.
#'
#' @inheritParams save_project
#' @inheritParams add_project_summary
#' @param summary_name Character. The name of the summary from which to generate
#' the summary. *If you provide `summary_name` all other parameters are
#' inherited according to what was set with `add_project_summary`.
#' @return
#' A list containing the generated summary based on the specified options. The
#' list includes filtered and cleaned data, metadata, and other summary details.
#'
#' @details
#' This function allows you to generate a summary of data from a specific summary
#' of records within the REDCap project. The function provides flexible options
#' for cleaning, annotating, and including metadata, as well as controlling
#' whether to include record summaries, user information, and logs.
#' @export
generate_project_summary <- function(
    project,
    summary_name,
    transform,
    filter_field = NULL,
    filter_choices = NULL,
    filter_list = NULL,
    filter_strict = TRUE,
    field_names = NULL,
    form_names = NULL,
    no_duplicate_cols = FALSE,
    exclude_identifiers = TRUE,
    exclude_free_text = FALSE,
    date_handling = "none",
    upload_compatible = TRUE,
    labelled = TRUE,
    clean = TRUE,
    drop_blanks = TRUE,
    drop_missings = FALSE,
    drop_others = NULL,
    include_metadata = TRUE,
    annotate_metadata = TRUE,
    include_record_summary = TRUE,
    include_users = TRUE,
    include_log = FALSE) {
  lifecycle::signal_stage("experimental", "generate_project_summary()")
  provided_summary_name <- !missing(summary_name)
  if (provided_summary_name) {
    if (!summary_name %in% names(project$summary)) {
      stop(summary_name, " is not included in the current project summaries")
    }
    summary_list <- project$summary[[summary_name]]
    #warning about other params?
    transform <- summary_list$transform
    filter_list <- summary_list$filter_list
    filter_strict <- summary_list$filter_strict
    field_names <- summary_list$field_names
    form_names <- summary_list$form_names
    no_duplicate_cols <- summary_list$no_duplicate_cols
    exclude_identifiers <- summary_list$exclude_identifiers
    exclude_free_text <- summary_list$exclude_free_text
    date_handling <- summary_list$date_handling
    upload_compatible <- summary_list$upload_compatible
    labelled <- summary_list$labelled
    clean <- summary_list$clean
    drop_blanks <- summary_list$drop_blanks
    drop_missings <- summary_list$drop_missings
    drop_others <- summary_list$drop_others
    include_metadata <- summary_list$include_metadata
    annotate_metadata <- summary_list$annotate_metadata
    include_record_summary <- summary_list$include_record_summary
    include_users <- summary_list$include_users
    include_log <- summary_list$include_log
  }
  if (missing(transform)) {
    transform <- project$internals$is_transformed
  }
  if (transform) {
    project$metadata <- project$transformation$metadata
    project$data <- project$transformation$data
  }
  project$data <- filter_data_list(
    data_list = project,
    field_names = field_names,
    form_names = form_names,
    filter_field = filter_field,
    filter_choices = filter_choices,
    filter_list = filter_list,
    filter_strict = filter_strict
  )
  if (!identical(labelled,project$internals$labelled)){
    if(!labelled && project$internals$labelled){
      stop(
        "We do not recommend converting labelled values back to raw values ",
        "except for at the upload step. If you want to edit only coded values,",
        " choose `labelled = FALSE` with `setup_project()`"
      ) #complicates things
      project <- labelled_to_raw_project(project)
    }
    if(labelled && !project$internals$labelled){
      project <- raw_to_labelled_project(project)
    }
  }
  if (exclude_identifiers) {
    project$data <- deidentify_data_list(
      data_list = project,
      exclude_free_text = exclude_free_text,
      date_handling = date_handling
    )
  }
  if (clean) {
    project$data <- clean_data_list(
      data_list = project,
      drop_blanks = drop_blanks,
      drop_others = drop_others
    )
  }
  to_save_list <- project$data
  if (include_metadata) {
    if (annotate_metadata && is_something(to_save_list)) {
      to_save_list$forms <- annotate_forms(project)
      to_save_list$fields <- annotate_fields(project)
      to_save_list$choices <- annotate_choices(project)
    } else {
      to_save_list$forms <- project$metadata$forms
      to_save_list$fields <- project$metadata$fields
      to_save_list$choices <- project$metadata$choices
    }
  }
  records <- extract_project_records(project)[[1]]
  if (include_record_summary) {
    if (!is.null(records)) {
      to_save_list$records <- summarize_records_from_log(project, records = records)
    }
  }
  if (include_users) {
    to_save_list$users <- summarize_users_from_log(project, records = records)
  }
  if (include_log) {
    to_save_list$log <- get_log(project, records = records)
  }
  invisible(to_save_list)
}
#' @title Summarize REDCap Database
#' @noRd
summarize_project <- function(
    project,
    hard_reset = FALSE) {
  assert_setup_project(project)
  if(is_something(project$data)){
    summary_names <- check_summaries(project)
    if (hard_reset) {
      summary_names <- project$summary %>% names()
    }
    summary_names <- summary_names[which(summary_names != "all_records")]
    if (is_something(summary_names)) {
      for (summary_name in summary_names) {
        project <- project %>% save_summary(summary_name)
      }
    }
  }
  invisible(project)
}
#' @title clear_project_summaries
#' @description
#' `r lifecycle::badge("experimental")`
#' @inheritParams save_project
#' @export
clear_project_summaries <- function(project) {
  lifecycle::signal_stage("experimental", "clear_project_summaries()")
  assert_setup_project(project)
  summary_names <- names(project$summary)
  summary_names <- summary_names[which(summary_names!="all_records")]
  for(summary_name in summary_names){
    project$summary[[summary_name]]<- NULL
    project$summary$all_records[[summary_name]]<- NULL
  }
  cli_alert_success("Cleared project summaries!")
  invisible(project)
}
#' @title Run Quality Checks
#' @inheritParams save_project
#' @return project object
#' @export
run_quality_checks <- function(project) {
  project <- assert_blank_project(project)
  if (is_something(project$quality_checks)) {
    for (qual_check in names(project$quality_checks)) {
      the_function <- project$quality_checks[[qual_check]]
      if (is.function(the_function)) {
        project <- the_function(project)
      }
    }
  }
  invisible(project)
}
#' @noRd
extract_values_from_form_list <- function(form_list, col_name) {
  names(form_list) %>% lapply(function(form_name) {
    form_list[[form_name]][[col_name]]
  }) %>%
    unlist() %>%
    drop_nas() %>%
    unique()
}
#' @noRd
extract_project_records <- function(project) {
  all_records <- NULL
  id_col <- project$redcap$id_col
  if (project$data %>% is_something()) {
    record_id_col <- extract_values_from_form_list(
      form_list = project$data,
      col_name = id_col
    )
    all_records <- data.frame(
      record_id_col = record_id_col,
      first_timestamp = NA,
      last_timestamp = NA,
      last_api_call = NA,
      was_transformed = FALSE,
      was_saved = FALSE,
      stringsAsFactors = FALSE
    )
    rownames(all_records) <- NULL
    colnames(all_records)[which(colnames(all_records) == "record_id_col")] <- id_col
  }
  all_records
}
#' @noRd
get_log <- function(project, records) {
  redcap_log <- project$redcap$log
  redcap_log <- redcap_log[which(!is.na(redcap_log$username)), ]
  redcap_log <- redcap_log[which(!is.na(redcap_log$record)), ]
  # if(drop_exports){
  #   redcap_log <- redcap_log[which(redcap_log$action_type != "Exports" | is.na(redcap_log$action_type)), ]
  # }
  if (!missing(records)) {
    if (!is.null(records)) {
      redcap_log <- redcap_log[which(redcap_log$record %in% records), ]
    }
  }
  redcap_log
}
#' @noRd
summarize_users_from_log <- function(project, records) {
  redcap_log <- get_log(project, records)
  #role_label not inculded now
  summary_users <- project$redcap$users %>% dplyr::select(c("username", "email", "firstname", "lastname"))
  user_groups <- redcap_log %>% split(redcap_log$username)
  if (!is_something(user_groups)) {
    return(summary_users)
  }
  #maybe dropping people at this step
  summary_users <- summary_users[which(summary_users$username %in% names(user_groups)), ]
  user_groups <- user_groups[drop_nas(match(summary_users$username, names(user_groups)))]
  summary_users$first_timestamp <- user_groups %>%
    lapply(function(group) {
      group$timestamp %>% dplyr::last()
    }) %>%
    unlist()
  summary_users$last_timestamp <- user_groups %>%
    lapply(function(group) {
      group$timestamp[[1]]
    }) %>%
    unlist()
  summary_users$unique_records_n <- user_groups %>%
    lapply(function(group) {
      unique_length(group$record)
    }) %>%
    unlist() %>%
    as.integer()
  # summary_users$timestamp_diff_days<- (as.Date(summary_users$last_timestamp) - as.Date(summary_users$first_timestamp)+1) %>% as.integer()
  # summary_users$records_per_day <- as.integer(summary_users$unique_records_n/summary_users$timestamp_diff_days)
  summary_users
}
#' @noRd
summarize_comments_from_log <- function(project, records) {
  redcap_log <- get_log(project, records)
  # redcap_log$action_type == "Comment"
  if (nrow(redcap_log) == 0) {
    return(NULL)
  }
  redcap_log
}
#' @noRd
summarize_records_from_log <- function(project, records) {
  redcap_log <- project$redcap$log
  redcap_log <- redcap_log[which(!is.na(redcap_log$username)), ]
  redcap_log <- redcap_log[which(!is.na(redcap_log$record)), ]
  if (!missing(records)) {
    if (!is.null(records)) {
      redcap_log <- redcap_log[which(redcap_log$record %in% records), ]
    }
  }
  # records -------------
  # all_records <- unique(redcap_log$record)
  summary_records <- project$summary$all_records
  record_groups <- redcap_log %>% split(redcap_log$record)
  summary_records <- summary_records[which(summary_records[[project$redcap$id_col]] %in% names(record_groups)), , drop = FALSE]
  # users_log_rows <- users %>% lapply(function(user){which(redcap_log$username==user)})
  # records_log_rows <- records %>% lapply(function(record){which(redcap_log$record==record)})
  record_groups <- record_groups[match(summary_records[[project$redcap$id_col]], names(record_groups))]
  summary_records$last_timestamp <- record_groups %>%
    lapply(function(group) {
      group$timestamp[[1]]
    }) %>%
    unlist()
  summary_records$first_timestamp <- record_groups %>%
    lapply(function(group) {
      group$timestamp %>% dplyr::last()
    }) %>%
    unlist()
  summary_records$last_user <- record_groups %>%
    lapply(function(group) {
      group$username[[1]]
    }) %>%
    unlist()
  user_rows <- match(summary_records$last_user, project$redcap$users$username)
  summary_records$last_user_name <- paste(
    project$redcap$users$firstname[user_rows],
    project$redcap$users$lastname[user_rows]
  )
  summary_records$unique_users_n <- record_groups %>%
    lapply(function(group) {
      unique_length(group$username)
    }) %>%
    unlist() %>%
    as.integer()
  summary_records
}
#' @noRd
get_summary_records <- function(project, summary_name) {
  id_col <- project$redcap$id_col
  if (missing(summary_name)) {
    return(project$summary$all_records[[id_col]])
  }
  summary_list <- project$summary[[summary_name]]
  to_save_list <- generate_project_summary(
    project = project,
    transform = summary_list$transform,
    filter_list = summary_list$filter_list,
    filter_strict = FALSE,
    form_names = summary_list$form_names,
    field_names = project$redcap$id_col,
    exclude_identifiers = FALSE,
    exclude_free_text = FALSE,
    clean = FALSE,
    include_metadata = FALSE,
    annotate_metadata = FALSE,
    include_record_summary = FALSE,
    include_users = FALSE,
    include_log = FALSE,
    no_duplicate_cols = TRUE
  )
  records <- to_save_list %>%
    lapply(function(form) {
      form[[id_col]]
    }) %>%
    unlist() %>%
    sort() %>%
    unique()
  record_rows <- which(project$summary$all_records[[id_col]] %in% records)
  summary_records <- project$summary$all_records[[id_col]][record_rows] %>%
    sort() %>%
    unique()
  summary_records
}
#' @noRd
summary_records_due <- function(project, summary_name) {
  summary_list <- project$summary[[summary_name]]
  id_col <- project$redcap$id_col
  if (is.null(summary_list$last_save_time)) {
    return(TRUE)
  }
  if (!file.exists(summary_list$file_path) && !summary_list$separate) {
    # can't do this for separate = T unless more code is written
    return(TRUE)
  }
  old_records <- project$summary$all_records[[id_col]][which(project$summary$all_records[[summary_name]])] %>% sort()
  records <- get_summary_records(
    project = project,
    summary_name = summary_name
  ) %>% sort()
  if(!identical(records,old_records)) {
    return(TRUE)
  }
  record_rows <- which(project$summary$all_records[[id_col]] %in% records)
  relevant_records <- project$summary$all_records[record_rows,c(id_col,"last_api_call","was_saved",summary_name)]
  if(length(which(!relevant_records$was_saved))>0) {
    return(TRUE)   # maybe was_saved not needed at all
  }
  if(any(relevant_records$last_api_call > summary_list$last_save_time)) {
    return(TRUE)
  }
  FALSE
}
#' @noRd
check_summaries <- function(project, summary_names) {
  if (missing(summary_names)) {
    summary_names <- project$summary %>% names()
    summary_names <- summary_names[which(summary_names!="all_records")]
  }
  needs_refresh <- NULL
  if (is.null(summary_names)) {
    cli_alert_wrap("No summaries. Use `add_project_summary()`!")
  }
  # need_to_check <- any(project$summary$all_records$last_api_call > summary_list$last_save_time)
  for (summary_name in summary_names) {
    test_summary <- summary_records_due(
      project = project,
      summary_name = summary_name
      )
    if (test_summary) {
      needs_refresh <- needs_refresh %>% append(summary_name)
    }
  }
  if (is.null(needs_refresh)) {
    cli_alert_wrap("Refresh of summaries not needed!", bullet_type = "v")
  }
  needs_refresh
}
#' @title rmarkdown_project
#' @description
#' `r lifecycle::badge("experimental")`
#' Generates an RMarkdown report for the given REDCap database (`project`
#' object). This function creates an RMarkdown file in the specified directory
#' or default directory, allowing users to create custom reports based on the
#' database content.
#'
#' @details
#' This function checks if a directory is specified, and if not, defaults to the
#' `output` folder within the project's directory. It generates the RMarkdown
#' file that can then be used for further processing or rendering into HTML,
#' PDF, or other formats.
#'
#' @inheritParams save_project
#' @param dir_other Character string specifying the directory where the
#' RMarkdown report will be saved. If not provided, it defaults to the `output`
#' directory inside the project's main directory.
#' @return A message indicating the creation of the RMarkdown report and the
#' path to the generated file.
#' @seealso
#' \code{\link[REDCapSync]{save_project}} for saving the `project` object.
#' @family db_functions
#' @export
rmarkdown_project <- function(project, dir_other) {
  if (missing(dir_other)) {
    dir <- get_dir(project) %>% file.path("output")
  } else {
    dir <- dir_other
  }
  filename <- paste0(project$short_name, "_full_summary_", gsub("-", "_", Sys.Date()), ".pdf")
  rmarkdown::render(
    input = system.file("rmarkdown", "pdf.Rmd", package = pkg_name),
    output_format = "pdf_document",
    output_file = dir %>% file.path(filename),
    output_dir = dir,
    quiet = FALSE
  )
}
#' @title Clean to Raw REDCap forms
#' @inheritParams save_project
#' @param form data.frame of labelled REDCap to be converted to raw REDCap
#' (for uploads)
#' @return project object that has been filtered to only include the specified
#' records
#' @export
labelled_to_raw_form <- function(form, project) {
  use_missing_codes <- is.data.frame(project$metadata$missing_codes)
  fields <- filter_fields_from_form(form = form, project = project)
  for (i in seq_len(nrow(fields))) { # i <-  seq_len(nrow(fields) %>% sample(1)
    field_name <- fields$field_name[i]
    has_choices <- fields$has_choices[i]
    if (has_choices) {
      z <- fields$select_choices_or_calculations[i] %>% split_choices()
      form[[field_name]] <- form[[field_name]] %>%
        lapply(function(x) {
          form <- NA
          if (!is.na(x)) {
            coded_redcap <- which(z$name == x)
            if (length(coded_redcap) > 0) {
              form <- z$code[coded_redcap]
            } else {
              if (use_missing_codes) {
                coded_redcap2 <- which(project$metadata$missing_codes$name == x)
                if (length(coded_redcap2) > 0) {
                  form <- project$metadata$missing_codes$code[coded_redcap2]
                } else {
                  stop("Mismatch in choices compared to REDCap (above)! Column: ", field_name, ", Choice: ", x)
                }
              } else {
                stop("Mismatch in choices compared to REDCap (above)! Column: ", field_name, ", Choice: ", x, ". Also not a missing code.")
              }
            }
          }
          form
        }) %>%
        unlist() %>%
        as.character()
    } else {
      if (use_missing_codes) {
        form[[field_name]] <- form[[field_name]] %>%
          lapply(function(x) {
            field <- x
            if (!is.na(x)) {
              code_row <- which(project$metadata$missing_codes$name == x)
              if (length(code_row) > 0) {
                field <- project$metadata$missing_codes$code[code_row]
              }
            }
            field
          }) %>%
          unlist() %>%
          as.character()
      }
    }
  }
  form
}
#' @title Raw to Labelled REDCap forms
#' @param form data.frame of raw REDCap to be converted to labelled REDCap
#' @inheritParams save_project
#' @return project object
#' @export
raw_to_labelled_form <- function(form, project) {
  if (nrow(form) > 0) {
    use_missing_codes <- is.data.frame(project$metadata$missing_codes)
    metadata <- filter_fields_from_form(form = form, project = project)
    for (i in seq_len(nrow(metadata))) { # i <-  seq_len(nrow(metadata)) %>% sample(1)
      field_name <- metadata$field_name[i]
      has_choices <- metadata$has_choices[i]
      if (has_choices) {
        z <- metadata$select_choices_or_calculations[i] %>% split_choices()
        form[[field_name]] <- form[[field_name]] %>%
          lapply(function(x) {
            form <- NA
            if (!is.na(x)) {
              coded_redcap <- which(z$code == x)
              if (length(coded_redcap) > 0) {
                form <- z$name[coded_redcap]
              } else {
                if (use_missing_codes) {
                  coded_redcap2 <- which(project$metadata$missing_codes$code == x)
                  if (length(coded_redcap2) > 0) {
                    form <- project$metadata$missing_codes$name[coded_redcap2]
                  } else {
                    warning("Mismatch in choices compared to REDCap (above)! Column: ", field_name, ", Choice: ", x, ". Also not a missing code.")
                  }
                } else {
                  warning("Mismatch in choices compared to REDCap (above)! Column: ", field_name, ", Choice: ", x)
                }
              }
            }
            form
          }) %>%
          unlist() %>%
          as.character()
      } else {
        if (use_missing_codes) {
          z <- project$metadata$missing_codes
          form[[field_name]] <- form[[field_name]] %>%
            lapply(function(x) {
              field <- x
              if (!is.na(x)) {
                code_row <- which(z$code == x)
                if (length(code_row) > 0) {
                  field <- z$name[code_row]
                }
              }
              field
            }) %>%
            unlist() %>%
            as.character()
        }
      }
    }
  }
  form
}
#' @noRd
stack_vars <- function(project, vars, new_name, drop_na = TRUE) {
  project <- assert_blank_project(project)
  fields <- project$metadata$fields
  if (!all(vars %in% fields$field_name)) stop("all vars must be in metadata.")
  the_stack <- NULL
  for (var in vars) { # var <- vars %>% sample1()
    form <- generate_project_summary(
      project,
      field_names = var,
      include_log = FALSE,
      include_metadata = FALSE,
      include_record_summary = FALSE,
      include_users = FALSE
    )[[1]]
    colnames(form)[which(colnames(form) == var)] <- new_name
    the_stack <- the_stack %>% dplyr::bind_rows(form)
  }
  if (drop_na) {
    the_stack <- the_stack[which(!is.na(the_stack[[new_name]])), ]
  }
  return(the_stack)
}
#' @noRd
get_all_field_names <- function(project) {
  project$data %>%
    lapply(colnames) %>%
    unlist() %>%
    unique()
}
#' @noRd
field_names_to_form_names <- function(project, field_names, transform = FALSE, strict = FALSE) {
  metadata <- project$metadata
  if (transform) {
    metadata <- project$transformation$metadata
  }
  fields <- metadata$fields
  form_key_cols <- metadata$form_key_cols %>%
    unlist() %>%
    unique()
  field_names_keys <- field_names[which(field_names %in% form_key_cols)]
  form_names_keys <- field_names_keys %>%
    lapply(function(field_name) {
      metadata$form_key_cols %>%
        names() %>%
        lapply(function(form_name) {
          if (!field_name %in% metadata$form_key_cols[[form_name]]) {
            return(NULL)
          }
          form_name
        }) %>%
        unlist()
    }) %>%
    unlist() %>%
    as.character() %>%
    unique()
  field_names_not_keys <- field_names[which(!field_names %in% form_key_cols)] %>% unique()
  form_names_not_keys <- fields$form_name[match(field_names_not_keys, fields$field_name)] %>% drop_nas() %>% unique()
  form_names <- form_names_not_keys
  if (!strict) {
    form_names <- form_names %>% append(form_names_keys) %>% unique()
  }
  form_names
}
#' @noRd
form_names_to_field_names <- function(form_names, project, original_only = FALSE) {
  field_names <- NULL
  if (original_only) {
    fields <- project$metadata$fields
  } else {
    fields <- project$metadata$fields
  }
  for (form_name in form_names) {
    field_names <- field_names %>% append(fields$field_name[which(fields$form_name == form_name)])
  }
  return(unique(field_names))
}
#' @noRd
form_names_to_form_labels <- function(form_names, project) {
  project$metadata$forms$form_label[
    match(
      x = form_names,
      table = project$metadata$forms$form_name
    )
  ]
}
#' @noRd
form_labels_to_form_names <- function(form_labels, project) {
  project$metadata$forms$form_name[
    match(
      x = form_labels,
      table = project$metadata$forms$form_label
    )
  ]
}
#' @noRd
field_names_to_field_labels <- function(field_names, project) {
  project$metadata$fields$field_label[
    match(
      x = field_names,
      table = project$metadata$fields$field_name
    )
  ]
}
#' @noRd
construct_header_list <- function(form_list,
                                  md_elements = c("form_name", "field_type", "field_label"),
                                  fields) {
  if (anyDuplicated(fields$field_name) > 0) stop("dup names not allowed in fields")
  data_field_list <- form_list %>% lapply(colnames)
  header_df_list <- data_field_list %>% lapply(function(field_names) {
    x <- field_names %>%
      lapply(function(field_name) {
        row <- which(fields$field_name == field_name)
        if (length(row) > 0) {
          return(as.character(fields[md_elements][row, ]))
        }
        rep("", length(md_elements))
      }) %>%
      as.data.frame()
    colnames(x) <- field_names
    x <- x[which(apply(x, 1, function(row) {
      any(row != "")
    })), ]
    x
  })
  header_df_list
}
#' @noRd
stripped_project <- function(project) {
  project$redcap$log <- list()
  project$data <- list()
  project$data_updates <- list()
  invisible(project)
}
#' @noRd
field_names_metadata <- function(project, field_names, col_names) {
  fields <- project$metadata$fields # project$metadata$fields
  # if(!deparse(substitute(form_name))%in%project$metadata$forms$form_name)stop("To avoid potential issues the form name should match one of the instrument names" )
  bad_field_names <- field_names[which(!field_names %in% c(project$metadata$fields$field_name, project$redcap$raw_structure_cols, "arm_number", "event_name"))]
  if (length(bad_field_names) > 0) stop("All column names in your form must match items in your metadata, `project$metadata$fields$field_name`... ", toString(bad_field_names))
  # metadata <- project$metadata$fields[which(project$metadata$fields$form_name%in%instruments),]
  fields <- fields[which(fields$field_name %in% field_names), ]
  # metadata <- metadata[which(metadata$field_name%in%field_names),]
  if (!missing(col_names)) {
    if (is_something(col_names)) fields <- fields[[col_names]]
  }
  fields
}
#' @noRd
filter_fields_from_form <- function(form, project) {
  forms <- project %>% field_names_to_form_names(field_names = colnames(form))
  if (any(forms %in% project$metadata$forms$repeating)) {
    stop(
      "All column names in your form must match only one form in your",
      "metadata, `project$metadata$forms$form_name`, unless they are",
      " all non-repeating"
    )
  }
  fields <- project %>% field_names_metadata(field_names = colnames(form))
  fields <- fields[which(fields$field_type != "descriptive"), ]
  fields$has_choices <- !is.na(fields$select_choices_or_calculations)
  fields$has_choices[which(fields$field_type == "calc")] <- FALSE
  fields
}
#' @noRd
labelled_to_raw_project <- function(project) {
  project <- assert_blank_project(project)
  if (!project$internals$labelled) {
    stop("project is already raw/coded (not labelled values)")
  }
  for (form_name in names(project$data)) {
    project$data[[form_name]] <- project$data[[form_name]] %>%
      labelled_to_raw_form(project = project)
  }
  project$internals$labelled <- FALSE
  invisible(project)
}
#' @noRd
raw_to_labelled_project <- function(project) {
  project <- assert_blank_project(project)
  if (project$internals$labelled) {
    stop("project is already labelled (not raw/coded values)")
  }
  for (form_name in names(project$data)) {
    project$data[[form_name]] <- project$data[[form_name]] %>%
      raw_to_labelled_form(project = project)
  }
  project$internals$labelled <- FALSE
  invisible(project)
}
#' @noRd
form_list_to_text <- function(form_list, project, drop_nas = TRUE, clean_names = TRUE) {
  output_list <- NULL
  for (i in seq_along(form_list)) {
    form <- form_list[[i]]
    the_raw_name <- names(form_list)[[i]]
    the_name <- the_raw_name
    if (clean_names) the_name <- project$metadata$forms$form_label[which(project$metadata$forms$form_name == the_raw_name)]
    df_name <- paste0("----- ", the_name, " Table -----")
    output_list <- output_list %>%
      append(paste0("&nbsp;&nbsp;<strong>", df_name, "</strong><br>"))
    key_col_names <- project$metadata$form_key_cols[[the_raw_name]]
    for (j in seq_len(nrow(form))) {
      for (col_name in colnames(form)) {
        entry <- form[j, col_name]
        if (!col_name %in% key_col_names) {
          if (!is.na(entry) || !drop_nas) {
            entry <- gsub("\\n", "<br>", entry)
            col_name_clean <- col_name
            if (clean_names) col_name_clean <- project$metadata$fields$field_label[which(project$metadata$fields$field_name == col_name)]
            output_list <- output_list %>%
              append(paste0("&nbsp;&nbsp;<strong>", col_name_clean, ":</strong> <br>&nbsp;&nbsp;&nbsp;&nbsp;", entry, "<br>"))
          }
        }
      }
      # output_list <- c(output_list, "<br>")
    }
    output_list <- c(output_list, "<br>")
  }
  output_list
}
