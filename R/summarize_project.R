#' @title Clean project columns for plotting using the metadata
#' @description
#' This function cleans the columns of a `project` object, transforming choice fields into factors and ensuring numeric columns are set correctly for table processing or plotting (e.g., using `table1`). It handles the transformation of missing values and optional removal of certain codes based on user input.
#'
#' @inheritParams save_project
#' @param drop_blanks Logical. If TRUE, will drop choice fields with zero occurrences (n = 0). Default is FALSE.
#' @param other_drops A list of additional fields or choices to drop from the data. Defaults to NULL.
#'
#' @return A cleaned `project` object ready for table or plot processing.
#'
#' @details
#' The function works by cleaning up the data frame list (`project$data`) according to the metadata (`project$metadata$fields`). It converts choice fields into factors, numeric fields are treated appropriately, and any unwanted or missing codes can be dropped based on the parameters provided. The function also ensures that the data is only cleaned once by checking the internal `is_clean` flag.
#'
#' @note
#' The function will not proceed with cleaning if `project$internals$is_clean` is already TRUE, signaling that the project has already been cleaned.
#' @export
clean_project <- function(project, drop_blanks = FALSE, other_drops = NULL) { # problematic because setting numeric would delete missing codes
  # project <-  project %>% annotate_fields(skim = FALSE)
  if (!is_something(project)) {
    return(project)
  }
  if (!is_something(project$data)) {
    return(project)
  }
  if (project$internals$is_clean) {
    bullet_in_console("Already Clean", bullet_type = "v")
    return(project)
  }
  project$data <- clean_DF_list(
    DF_list = project$data,
    fields = project$metadata$fields,
    drop_blanks = drop_blanks,
    other_drops = other_drops
  )
  project$internals$is_clean <- TRUE
  return(project)
}
#' @noRd
fields_to_choices <- function(fields) {
  fields <- fields[which(fields$field_type %in% c("radio", "dropdown", "checkbox_choice", "yesno")), ]
  # fields$field_name[which(fields$field_type=="checkbox_choice")] <- fields$field_name[which(fields$field_type=="checkbox_choice")] %>% strsplit("___") %>% lapply(function(X){X[[1]]})
  fields <- fields[which(!is.na(fields$select_choices_or_calculations)), ]
  choices <- NULL
  for (i in seq_len(nrow(fields))) {
    field_name <- fields$field_name[i]
    form_name <- fields$form_name[i]
    # form_label <- fields$form_label[i]
    field_label <- fields$field_label[i]
    field_type <- fields$field_type[i]
    selections <- fields$select_choices_or_calculations[i] %>% split_choices()
    choices <- choices %>% dplyr::bind_rows(
      data.frame(
        form_name = form_name,
        # form_label = form_label,
        field_name = field_name,
        field_type = field_type,
        field_label = ifelse(!is.na(field_label), field_label, field_name),
        code = selections$code,
        name = selections$name
      )
    )
  }
  choices$label <- paste(choices$form_name, "-", choices$field_label, "-", choices$name)
  # choices$label2 <- paste(choices$form_label,"-",choices$field_label,"-",choices$name)
  rownames(choices) <- NULL
  return(choices)
}
#' @noRd
add_labels_to_checkbox <- function(fields) {
  rows <- which(fields$field_type == "checkbox_choice")
  x <- fields$field_name[rows] %>%
    strsplit("___") %>%
    lapply(function(x) {
      x[[1]]
    }) %>%
    unlist()
  y <- fields$field_label[rows]
  z <- paste0(fields$field_label[match(x, fields$field_name)], " - ", y)
  fields$field_label[rows] <- z
  return(fields)
}
#' @noRd
annotate_fields <- function(project, summarize_data = TRUE) {
  fields <- project$metadata$fields # [,colnames(get_original_fields(project))]
  fields <- fields[which(fields$field_type != "descriptive"), ]
  fields <- add_labels_to_checkbox(fields)
  fields <- fields[which(fields$field_type != "checkbox"), ]
  fields$field_label[which(is.na(fields$field_label))] <- fields$field_name[which(is.na(fields$field_label))]
  fields <- unique(fields$form_name) %>%
    lapply(function(IN) {
      fields[which(fields$form_name == IN), ]
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
    for (form in unique(fields$form_name)) {
      COLS <- fields$field_name[which(fields$form_name == form)]
      CHECK_THIS <- project$data[[form]]
      COLS <- COLS[which(COLS %in% colnames(CHECK_THIS))]
      skimmed <- skimmed %>% dplyr::bind_rows(CHECK_THIS[, COLS] %>% skimr::skim())
    }
    FOR_ORDERING <- fields$field_name
    fields <- fields %>% merge(skimmed, by.x = "field_name", by.y = "skim_variable", all = TRUE)
    fields <- FOR_ORDERING %>%
      lapply(function(IN) {
        fields[which(fields$field_name == IN), ]
      }) %>%
      dplyr::bind_rows()
  }
  # bullet_in_console("Annotated `project$metadata$fields`",bullet_type = "v")
  return(fields)
}
#' @noRd
annotate_forms <- function(project, summarize_data = TRUE) {
  forms <- project$metadata$forms
  # forms <- get_original_forms(project)
  # if(!is.null(project$metadata$form_key_cols)){
  #   forms$key_cols <- forms$form_name %>% lapply(function(IN){
  #     project$metadata$form_key_cols[[IN]] %>% paste0(collapse = "+")
  #   })
  #   forms$key_names <- forms$form_name %>% lapply(function(IN){
  #     row_match <- which(forms$form_name==IN)
  #     if(!forms$repeating[row_match])return(project$metadata$form_key_cols[[IN]])
  #     return(paste0(forms$form_name[row_match],"_key"))
  #   })
  # }
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
  return(forms)
}
#' @noRd
annotate_choices <- function(project, summarize_data = TRUE) {
  # forms <- project$metadata$forms
  # fields <- project$metadata$fields
  choices <- project$metadata$choices
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
        DF <- project$data[[choices$form_name[i]]]
        if (is.null(DF)) {
          return(0)
        }
        if (nrow(DF) == 0) {
          return(0)
        }
        sum(DF[, choices$field_name[i]] == choices$name[i], na.rm = TRUE)
        # print(i)
      }) %>%
      unlist()
    choices$n_total <- seq_len(nrow(choices)) %>%
      lapply(function(i) {
        DF <- project$data[[choices$form_name[i]]]
        if (is.null(DF)) {
          return(0)
        }
        if (nrow(DF) == 0) {
          return(0)
        }
        sum(!is.na(DF[, choices$field_name[i]]), na.rm = TRUE)
      }) %>%
      unlist()
    choices$perc <- (choices$n / choices$n_total) %>% round(4)
    choices$perc_text <- choices$perc %>%
      magrittr::multiply_by(100) %>%
      round(1) %>%
      paste0("%")
    # project$summary$choices <- choices
    # bullet_in_console("Annotated `project$summary$choices`",bullet_type = "v")
  }
  return(choices)
}
#' @noRd
fields_with_no_data <- function(project) {
  project$metadata$fields$field_name[which(is.na(project$metadata$fields$complete_rate) & !project$metadata$fields$field_type %in% c("checkbox", "descriptive"))]
}
#' @noRd
reverse_clean_project <- function(project) { # problematic because setting numeric would delete missing codes
  project$data <- all_character_cols_list(project$data)
  project$data_updates <- project$data_updates %>% all_character_cols_list()
  project$internals$is_clean <- FALSE
  return(project)
}
#' @noRd
clean_DF_list <- function(DF_list, fields, drop_blanks = TRUE, other_drops = NULL) {
  # add check for DF_list#
  for (TABLE in names(DF_list)) {
    DF_list[[TABLE]] <- clean_DF(
      DF = DF_list[[TABLE]],
      fields = fields,
      drop_blanks = drop_blanks,
      other_drops = other_drops
    )
  }
  return(DF_list)
}
#' @noRd
clean_DF <- function(DF, fields, drop_blanks = TRUE, other_drops = NULL) {
  for (COLUMN in colnames(DF)) {
    if (COLUMN %in% fields$field_name) {
      ROW <- which(fields$field_name == COLUMN)
      units <- NULL
      if (!is.na(fields$units[ROW])) {
        units <- fields$units[ROW]
      }
      class <- fields$field_type_R[ROW][[1]]
      label <- ifelse(is.na(fields$field_label[ROW]), COLUMN, fields$field_label[ROW])[[1]]
      levels <- NULL
      if (!is.na(class)) {
        if (class == "factor") {
          select_choices <- fields$select_choices_or_calculations[ROW]
          if (!is.na(select_choices)) {
            levels <- split_choices(select_choices)[[2]]
          } else {
            levels <- unique(DF[[COLUMN]]) %>% drop_nas()
          }
          if (any(duplicated(levels))) {
            DUPS <- levels %>%
              duplicated() %>%
              which()
            warning("You have a variable (", COLUMN, ") with dupplicate names (", levels[DUPS] %>% paste0(collapse = ", "), "). This is not great but for this proccess they will be merged and treated as identical responses.")
            levels <- levels %>% unique()
          }
          if (drop_blanks) {
            levels <- levels[which(levels %in% unique(DF[[COLUMN]]))]
          }
          if (!is.null(other_drops)) {
            if (length(other_drops) > 0) levels <- levels[which(!levels %in% other_drops)]
          }
        }
        if (class == "integer") {
          DF[[COLUMN]] <- as.integer(DF[[COLUMN]])
        }
        if (class == "numeric") {
          DF[[COLUMN]] <- as.numeric(DF[[COLUMN]])
        }
        DF
      }
      DF[[COLUMN]] <- DF[[COLUMN]] %>% clean_column_for_table(
        class = class,
        label = label,
        units = units,
        levels = levels
      )
    }
  }
  return(DF)
}
#' @noRd
clean_column_for_table <- function(col, class, label, units, levels) {
  if (!missing(class)) {
    if (!is.null(class)) {
      if (!is.na(class)) {
        if (class == "integer") {
          col <- col %>% as.integer()
        }
        if (class == "factor") {
          col <- col %>% factor(levels = levels, ordered = TRUE)
        }
        if (class == "numeric") {
          col <- col %>% as.numeric()
        }
      }
    }
  }
  if (!missing(label)) {
    if (!is.null(label)) {
      if (!is.na(label)) {
        attr(col, "label") <- label
      }
    }
  }
  if (!missing(units)) {
    if (!is.null(units)) {
      if (!is.na(units)) {
        attr(col, "units") <- units
      }
    }
  }
  col
}
#' @title Add a Subset to a REDCap Database
#' @description
#' Creates a subset of the main REDCap database (`project`) based on specific filter criteria
#' and saves it to a specified directory. The subset can be further customized with
#' additional forms, fields, and deidentification options.
#'
#' @inheritParams save_project
#' @param subset_name Character. The name of the subset to create.
#' @param filter_field Character. The name of the field in the database to filter on.
#' @param filter_choices Vector. The values of `filter_field` used to define the subset.
#' @param dir_other Character. The directory where the subset file will be saved.
#' Default is the `output` folder within the database directory.
#' @param file_name Character. The base name of the file where the subset will be saved.
#' Default is `<project$short_name>_<subset_name>`.
#' @param form_names Character vector. Names of forms to include in the subset. Default is `NULL`, which includes all forms.
#' @param field_names Character vector. Names of specific fields to include in the subset. Default is `NULL`, which includes all fields.
#' @param deidentify Logical. Whether to deidentify the data in the subset. Default is `TRUE`.
#' @param reset Logical. If `TRUE`, overwrite existing subset files with the same name. Default is `FALSE`.
#'
#' @return
#' A modified `project` object that includes the newly created subset.
#' The subset is also saved as a file in the specified directory.
#'
#' @details
#' This function filters the main REDCap database using the specified `filter_field`
#' and `filter_choices`, then creates a new subset with optional deidentification.
#' It can be customized to include only specific forms or fields. The resulting subset
#' is saved to a file for future use.
#'
#' @seealso
#' \code{\link{save_project}} for saving the main database or subsets.
#' @export
add_project_subset <- function(
    project,
    subset_name,
    filter_field,
    filter_choices,
    dir_other = file.path(project$dir_path, "output"),
    file_name = paste0(project$short_name, "_", subset_name),
    form_names = NULL,
    field_names = NULL,
    deidentify = TRUE,
    reset = FALSE) {
  if (is.null(project$summary$subsets[[subset_name]]) || reset) {
    project$summary$subsets[[subset_name]] <- list(
      subset_name = subset_name,
      filter_field = filter_field,
      filter_choices = filter_choices,
      form_names = form_names,
      field_names = field_names,
      subset_records = NULL,
      dir_other = dir_other,
      file_name = file_name,
      last_save_time = NULL,
      deidentify = deidentify,
      file_path = file.path(dir_other, paste0(file_name, ".xlsx"))
    )
  }
  return(project)
}
#' @noRd
generate_summary_save_list <- function(
    project,
    deidentify = TRUE,
    clean = TRUE,
    drop_blanks = TRUE,
    other_drops = NULL,
    include_metadata = TRUE,
    annotate_metadata = TRUE,
    include_record_summary = TRUE,
    include_users = TRUE,
    include_log = TRUE) {
  records <- sum_records(project)[[1]]
  if (deidentify) {
    project <- deidentify_project(project)
  }
  if (clean) {
    project <- project %>% clean_project(drop_blanks = drop_blanks, other_drops = other_drops) # problematic because setting numeric would delete missing codes
  }
  to_save_list <- project$data
  if (include_metadata) {
    if (annotate_metadata && is_something(project$data)) {
      to_save_list$forms <- annotate_forms(project)
      to_save_list$fields <- annotate_fields(project)
      to_save_list$choices <- annotate_choices(project)
    } else {
      to_save_list$forms <- project$metadata$forms
      to_save_list$fields <- project$metadata$fields
      to_save_list$choices <- project$metadata$choices
    }
    # if(project$internals$is_transformed){
    #   to_save_list$original_forms <- project$transformation$original_forms
    #   to_save_list$original_fields <- project$transformation$original_fields
    # }
  }
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
  # to_save_list$choices <- annotate_choices(project)
  # to_save_list$choices <- annotate_choices(project)
  return(to_save_list)
}
#' @noRd
save_REDCapSync_list <- function(
    project,
    to_save_list,
    dir_other = file.path(project$dir_path, "output"),
    file_name = paste0(project$short_name, "_REDCapSync"),
    separate = FALSE,
    with_links = TRUE) {
  link_col_list <- list()
  if (with_links) {
    if (project$internals$project_type == "redcap") {
      add_links <- which(names(to_save_list) %in% names(project$data))
      if (length(add_links) > 0) {
        to_save_list[add_links] <- to_save_list[add_links] %>% lapply(function(DF) {
          add_redcap_links_to_DF(DF, project)
        })
        link_col_list <- list(
          "redcap_link"
        )
        names(link_col_list) <- project$redcap$id_col
      }
    }
  }
  if (project$internals$use_csv) {
    to_save_list %>% list_to_csv(
      dir = dir_other,
      file_name = file_name,
      overwrite = TRUE
    )
  } else {
    to_save_list %>% list_to_excel(
      dir = dir_other,
      separate = separate,
      link_col_list = link_col_list,
      file_name = file_name,
      # str_trunc_length = 10000,
      header_df_list = to_save_list %>% construct_header_list(fields = project$metadata$fields) %>% process_df_list(silent = TRUE),
      key_cols_list = construct_key_col_list(project),
      overwrite = TRUE
    )
  }
}
#' @title Generate a Summary from a Subset Name
#' @description
#' Generates a summary from a predefined subset of data within a REDCap project. The summary can be customized based on various options, such as cleaning the data, including metadata, and annotating metadata.
#'
#' @inheritParams save_project
#' @param subset_name Character. The name of the subset from which to generate the summary.
#' @param clean Logical. If `TRUE`, the data will be cleaned before summarizing. Default is `TRUE`.
#' @param drop_blanks Logical. If `TRUE`, records with blank fields will be dropped. Default is `TRUE`.
#' @param include_metadata Logical. If `TRUE`, metadata will be included in the summary. Default is `TRUE`.
#' @param annotate_metadata Logical. If `TRUE`, metadata will be annotated in the summary. Default is `TRUE`.
#' @param include_record_summary Logical. If `TRUE`, a record summary will be included in the generated summary. Default is `TRUE`.
#' @param include_users Logical. If `TRUE`, user-related information will be included in the summary. Default is `TRUE`.
#' @param include_log Logical. If `TRUE`, the log of changes will be included in the summary. Default is `TRUE`.
#'
#' @return
#' A list containing the generated summary based on the specified options. The list includes filtered and cleaned data, metadata, and other summary details.
#'
#' @details
#' This function allows you to generate a summary of data from a specific subset of records within the REDCap project. The function provides flexible options for cleaning, annotating, and including metadata, as well as controlling whether to include record summaries, user information, and logs.
#' @export
generate_summary_from_subset_name <- function(
    project,
    subset_name,
    clean = TRUE,
    drop_blanks = TRUE,
    include_metadata = TRUE,
    annotate_metadata = TRUE,
    include_record_summary = TRUE,
    include_users = TRUE,
    include_log = TRUE) {
  subset_list <- project$summary$subsets[[subset_name]]
  if (subset_list$filter_field == project$redcap$id_col) {
    subset_list$filter_choices <- subset_list$subset_records[[project$redcap$id_col]]
  }
  project$data <- filter_project(
    project = project,
    field_names = subset_list$field_names,
    form_names = subset_list$form_names,
    filter_field = subset_list$filter_field,
    filter_choices = subset_list$filter_choices
  )
  to_save_list <- project %>% generate_summary_save_list(
    deidentify = subset_list$deidentify,
    clean = clean,
    drop_blanks = drop_blanks,
    include_metadata = include_metadata,
    annotate_metadata = annotate_metadata,
    include_record_summary = include_record_summary,
    include_users = include_users,
    include_log = include_log
  )
  return(to_save_list)
}
#' @title Summarize REDCap Database
#' @description
#' Summarizes the REDCap database (`project` object) by filtering and generating a summary list.
#'
#' @details
#' This function filters the REDCap database based on the provided parameters and generates a summary list. The summary can include metadata, record summaries, user information, and logs. The function also supports deidentification and cleaning of the data.
#'
#' @inheritParams save_project
#' @param with_links Logical (TRUE/FALSE). If TRUE, includes links in the summary. Default is `TRUE`.
#' @param deidentify Logical (TRUE/FALSE). If TRUE, deidentifies the summary data. Default is `TRUE`.
#' @param clean Logical (TRUE/FALSE). If TRUE, cleans the summary data. Default is `TRUE`.
#' @param drop_blanks Logical (TRUE/FALSE). If TRUE, drops blank entries from the summary. Default is `TRUE`.
#' @param include_metadata Logical (TRUE/FALSE). If TRUE, includes metadata in the summary. Default is `TRUE`.
#' @param annotate_metadata Logical (TRUE/FALSE). If TRUE, annotates metadata in the summary. Default is `TRUE`.
#' @param include_record_summary Logical (TRUE/FALSE). If TRUE, includes a summary of records in the summary. Default is `TRUE`.
#' @param include_users Logical (TRUE/FALSE). If TRUE, includes user information in the summary. Default is `TRUE`.
#' @param include_log Logical (TRUE/FALSE). If TRUE, includes logs in the summary. Default is `TRUE`.
#' @param separate Logical (TRUE/FALSE). If TRUE, separates the summary into different sections. Default is `FALSE`.
#' @param reset Logical (TRUE/FALSE). If TRUE, forces the summary generation even if there are issues. Default is `FALSE`.
#' @return List. Returns a list containing the summarized data, including records, metadata, users, logs, and any other specified data.
#' @seealso
#' \link{setup_project} for initializing the `project` object.
#' \link{sync_project} for updating the `project` object.
#' @family db_functions
#' @export
summarize_project <- function(
    project,
    with_links = TRUE,
    deidentify = TRUE,
    clean = TRUE,
    drop_blanks = TRUE,
    include_metadata = TRUE,
    annotate_metadata = TRUE,
    include_record_summary = TRUE,
    include_users = TRUE,
    include_log = TRUE,
    separate = FALSE,
    reset = FALSE) {
  project <- project %>% assert_blank_project()
  original_data <- project$data
  do_it <- is.null(project$internals$last_summary)
  last_data_update <- project$internals$last_data_update
  if (!do_it) {
    do_it <- project$internals$last_summary < last_data_update
  }
  if (reset || do_it) {
    to_save_list <- project %>% generate_summary_save_list(
      deidentify = deidentify,
      clean = clean,
      drop_blanks = drop_blanks,
      include_metadata = include_metadata,
      annotate_metadata = annotate_metadata,
      include_record_summary = include_record_summary,
      include_users = include_users,
      include_log = include_log
    )
    project %>% save_REDCapSync_list(
      to_save_list = to_save_list,
      separate = separate,
      with_links = with_links
    )
    project$internals$last_summary <- last_data_update
  }
  subset_names <- check_subsets(project)
  if (reset) subset_names <- project$summary$subsets %>% names()
  if (is_something(subset_names)) {
    for (subset_name in subset_names) {
      project$data <- original_data
      subset_list <- project$summary$subsets[[subset_name]]
      project$summary$subsets[[subset_name]]$subset_records <- get_subset_records(project = project, subset_name = subset_name)
      project$summary$subsets[[subset_name]]$last_save_time <- now_time()
      to_save_list <- project %>% generate_summary_from_subset_name(
        subset_name = subset_name,
        clean = clean,
        drop_blanks = drop_blanks,
        include_metadata = include_metadata,
        annotate_metadata = annotate_metadata,
        include_record_summary = include_record_summary,
        include_users = include_users,
        include_log = include_log
      )
      project %>% save_REDCapSync_list(
        to_save_list = to_save_list,
        dir_other = subset_list$dir_other,
        file_name = subset_list$file_name,
        separate = separate,
        with_links = with_links
      )
    }
  }
  project$data <- original_data
  return(invisible(project))
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
  return(project)
}
#' @noRd
sum_records <- function(project) {
  records <- NULL
  if (project$data %>% is_something()) {
    cols <- project$redcap$id_col
    if (is.data.frame(project$metadata$arms)) {
      if (nrow(project$metadata$arms) > 1) {
        cols <- project$redcap$id_col %>% append("arm_number")
      }
    }
    if (length(cols) == 1) {
      records <- data.frame(
        records = names(project$data) %>% lapply(function(IN) {
          project$data[[IN]][, cols]
        }) %>% unlist() %>% unique()
      )
      colnames(records) <- cols
    }
    if (length(cols) == 2) {
      records <- names(project$data) %>%
        lapply(function(IN) {
          project$data[[IN]][, cols]
        }) %>%
        dplyr::bind_rows() %>%
        unique()
      # records <- records[order(as.integer(records[[project$redcap$id_col]])),]
    }
    rownames(records) <- NULL
    if (records[[project$redcap$id_col]] %>% duplicated() %>% any()) stop("duplicate ", project$redcap$id_col, " in sum_records() function")
  }
  return(records)
}
#' @noRd
get_log <- function(project, records) {
  log <- project$redcap$log
  log <- log[which(!is.na(log$username)), ]
  log <- log[which(!is.na(log$record)), ]
  # if(drop_exports){
  #   log <- log[which(log$action_type != "Exports" | is.na(log$action_type)), ]
  # }
  if (!missing(records)) {
    if (!is.null(records)) {
      log <- log[which(log$record %in% records), ]
    }
  }
  return(log)
}
#' @noRd
summarize_users_from_log <- function(project, records) {
  log <- get_log(project, records)
  summary_users <- project$redcap$users %>% dplyr::select(c("username", "role_label", "email", "firstname", "lastname"))
  user_groups <- log %>% split(log$username)
  summary_users <- summary_users[which(summary_users$username %in% names(user_groups)), ]
  if (nrow(summary_users) == 0) {
    return(NULL)
  }
  user_groups <- user_groups[drop_nas(match(summary_users$username, names(user_groups)))]
  summary_users$last_timestamp <- user_groups %>%
    lapply(function(group) {
      group$timestamp[[1]]
    }) %>%
    unlist()
  summary_users$first_timestamp <- user_groups %>%
    lapply(function(group) {
      group$timestamp %>% dplyr::last()
    }) %>%
    unlist()
  summary_users$last_user <- user_groups %>% lapply(function(group) {
    group$username[[1]]
  })
  summary_users$unique_records_n <- user_groups %>%
    lapply(function(group) {
      ul(group$record)
    }) %>%
    unlist()
  return(summary_users)
}
#' @noRd
summarize_comments_from_log <- function(project, records) {
  log <- get_log(project, records)
  summary_users <- project$redcap$users %>% dplyr::select(c("username", "role_label", "email", "firstname", "lastname"))
  user_groups <- log %>% split(log$username)
  summary_users <- summary_users[which(summary_users$username %in% names(user_groups)), ]
  if (nrow(summary_users) == 0) {
    return(NULL)
  }
  user_groups <- user_groups[drop_nas(match(summary_users$username, names(user_groups)))]
  summary_users$last_timestamp <- user_groups %>%
    lapply(function(group) {
      group$timestamp[[1]]
    }) %>%
    unlist()
  summary_users$first_timestamp <- user_groups %>%
    lapply(function(group) {
      group$timestamp %>% dplyr::last()
    }) %>%
    unlist()
  summary_users$last_user <- user_groups %>% lapply(function(group) {
    group$username[[1]]
  })
  summary_users$unique_records_n <- user_groups %>%
    lapply(function(group) {
      ul(group$record)
    }) %>%
    unlist()
  return(summary_comments)
}
#' @noRd
summarize_records_from_log <- function(project, records) {
  log <- project$redcap$log
  log <- log[which(!is.na(log$username)), ]
  log <- log[which(!is.na(log$record)), ]
  if (!missing(records)) {
    if (!is.null(records)) {
      log <- log[which(log$record %in% records), ]
    }
  }
  # records -------------
  # all_records <- unique(log$record)
  summary_records <- project$summary$all_records
  record_groups <- log %>% split(log$record)
  summary_records <- summary_records[which(summary_records[[project$redcap$id_col]] %in% names(record_groups)), , drop = FALSE]
  # users_log_rows <- users %>% lapply(function(user){which(log$username==user)})
  # records_log_rows <- records %>% lapply(function(record){which(log$record==record)})
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
  summary_records$unique_users_n <- record_groups %>%
    lapply(function(group) {
      ul(group$username)
    }) %>%
    unlist()
  return(summary_records)
}
#' @noRd
get_subset_records <- function(project, subset_name) {
  subset_list <- project$summary$subsets[[subset_name]]
  filter_choices <- subset_list$filter_choices
  form_name <- field_names_to_form_names(project, field_names = subset_list$filter_field)
  records <- project$data[[form_name]][[project$redcap$id_col]][which(project$data[[form_name]][[subset_list$filter_field]] %in% filter_choices)] %>% unique()
  subset_records <- project$summary$all_records[which(project$summary$all_records[[project$redcap$id_col]] %in% records), ]
  return(subset_records)
}
#' @noRd
subset_records_due <- function(project, subset_name) {
  subset_list <- project$summary$subsets[[subset_name]]
  if (is.null(subset_list$last_save_time)) {
    return(TRUE)
  }
  if (!file.exists(subset_list$file_path)) {
    return(TRUE)
  }
  subset_records <- get_subset_records(
    project = project,
    subset_name = subset_name
  )
  return(!identical(unname(subset_list$subset_records), unname(subset_records)))
}
#' @noRd
check_subsets <- function(project, subset_names) {
  if (missing(subset_names)) subset_names <- project$summary$subsets %>% names()
  needs_refresh <- NULL
  if (is.null(subset_names)) bullet_in_console("There are no subsets at `project$summary$subsets` which can be added with `add_project_subset()`!")
  for (subset_name in subset_names) {
    if (subset_records_due(project = project, subset_name = subset_name)) needs_refresh <- needs_refresh %>% append(subset_name)
  }
  if (is.null(needs_refresh)) bullet_in_console("Refresh of subsets not needed!", bullet_type = "v")
  return(needs_refresh)
}
#' @title Select REDCap Records from project
#' @description
#' This function filters the records in the `project` object by specified
#' criteria, such as field names, form names, and optional filtering based on a
#' specific field and its values. It returns a modified `project` object
#' containing only the records that match the filter criteria.
#'
#' @inheritParams save_project
#' @param field_names A character vector of field names to be included in the filtered data. If missing, all fields are included.
#' @param form_names A character vector of form names to be included in the filtered data. If missing, all forms are included.
#' @param filter_field A character string representing an extra variable name to be filtered by. This field must be present in the data frame.
#' @param filter_choices A character vector of values to filter by for the `filter_field`. Only records with these values in the specified field will be included.
#' @param warn_only A logical flag (`TRUE` or `FALSE`). If `TRUE`, the function will issue a warning instead of stopping if the filtering criteria do not match any records. Defaults to `FALSE`.
#' @param no_duplicate_cols A logical flag (`TRUE` or `FALSE`). If `TRUE`, the function will avoid including duplicate columns in the output. Defaults to `FALSE`.
#'
#' @return A modified `project` object with filtered records and columns based on the provided criteria.
#'
#' @details
#' This function filters the data in the `project` object according to the specified form and field names and optional filter criteria. If no field names or form names are provided, it defaults to using all fields and forms in the database.
#' The function uses the helper `filter_DF_list` to apply the filtering logic to the `project$data` list.
#'
#' @export
filter_project <- function(project, filter_field, filter_choices, form_names, field_names, warn_only = FALSE, no_duplicate_cols = FALSE) { # , ignore_incomplete=FALSE, ignore_unverified = FALSE
  if (missing(field_names)) field_names <- project %>% get_all_field_names()
  if (is.null(field_names)) field_names <- project %>% get_all_field_names()
  if (missing(form_names)) form_names <- names(project$data)
  if (is.null(form_names)) form_names <- names(project$data)
  return(
    filter_DF_list(
      DF_list = project$data,
      project = project,
      filter_field = filter_field,
      filter_choices = filter_choices,
      form_names = form_names,
      field_names = field_names,
      warn_only = warn_only,
      no_duplicate_cols = no_duplicate_cols
    )
  )
}
#' @title rmarkdown_project
#' @description
#' Generates an RMarkdown report for the given REDCap database (`project` object).
#' This function creates an RMarkdown file in the specified directory or default directory,
#' allowing users to create custom reports based on the database content.
#'
#' @details
#' This function checks if a directory is specified, and if not, defaults to the `output` folder
#' within the project's directory. It generates the RMarkdown file that can then be used for further
#' processing or rendering into HTML, PDF, or other formats.
#'
#' @inheritParams save_project
#' @param dir_other Character string specifying the directory where the RMarkdown report will be saved.
#' If not provided, it defaults to the `output` directory inside the project's main directory.
#' @return A message indicating the creation of the RMarkdown report and the path to the generated file.
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
#' @param FORM data.frame of labelled REDCap to be converted to raw REDCap (for uploads)
#' @return project object that has been filtered to only include the specified records
#' @export
labelled_to_raw_form <- function(FORM, project) {
  use_missing_codes <- is.data.frame(project$metadata$missing_codes)
  fields <- filter_fields_from_form(FORM = FORM, project = project)
  for (i in seq_len(nrow(fields))) { # i <-  seq_len(nrow(fields) %>% sample(1)
    COL_NAME <- fields$field_name[i]
    has_choices <- fields$has_choices[i]
    if (has_choices) {
      z <- fields$select_choices_or_calculations[i] %>% split_choices()
      FORM[[COL_NAME]] <- FORM[[COL_NAME]] %>%
        lapply(function(C) {
          OUT <- NA
          if (!is.na(C)) {
            coded_redcap <- which(z$name == C)
            if (length(coded_redcap) > 0) {
              OUT <- z$code[coded_redcap]
            } else {
              if (use_missing_codes) {
                coded_redcap2 <- which(project$metadata$missing_codes$name == C)
                if (length(coded_redcap2) > 0) {
                  OUT <- project$metadata$missing_codes$code[coded_redcap2]
                } else {
                  stop("Mismatch in choices compared to REDCap (above)! Column: ", COL_NAME, ", Choice: ", C)
                }
              } else {
                stop("Mismatch in choices compared to REDCap (above)! Column: ", COL_NAME, ", Choice: ", C, ". Also not a missing code.")
              }
            }
          }
          OUT
        }) %>%
        unlist() %>%
        as.character()
    } else {
      if (use_missing_codes) {
        FORM[[COL_NAME]] <- FORM[[COL_NAME]] %>%
          lapply(function(C) {
            OUT <- C
            if (!is.na(C)) {
              D <- which(project$metadata$missing_codes$name == C)
              if (length(D) > 0) {
                OUT <- project$metadata$missing_codes$code[D]
              }
            }
            OUT
          }) %>%
          unlist() %>%
          as.character()
      }
    }
  }
  FORM
}
#' @title Raw to Labelled REDCap forms
#' @param FORM data.frame of raw REDCap to be converted to labelled REDCap
#' @inheritParams save_project
#' @return project object
#' @export
raw_to_labelled_form <- function(FORM, project) {
  if (nrow(FORM) > 0) {
    use_missing_codes <- is.data.frame(project$metadata$missing_codes)
    metadata <- filter_fields_from_form(FORM = FORM, project = project)
    for (i in seq_len(nrow(metadata))) { # i <-  seq_len(nrow(metadata)) %>% sample(1)
      COL_NAME <- metadata$field_name[i]
      has_choices <- metadata$has_choices[i]
      if (has_choices) {
        z <- metadata$select_choices_or_calculations[i] %>% split_choices()
        FORM[[COL_NAME]] <- FORM[[COL_NAME]] %>%
          lapply(function(C) {
            OUT <- NA
            if (!is.na(C)) {
              coded_redcap <- which(z$code == C)
              if (length(coded_redcap) > 0) {
                OUT <- z$name[coded_redcap]
              } else {
                if (use_missing_codes) {
                  coded_redcap2 <- which(project$metadata$missing_codes$code == C)
                  if (length(coded_redcap2) > 0) {
                    OUT <- project$metadata$missing_codes$name[coded_redcap2]
                  } else {
                    warning("Mismatch in choices compared to REDCap (above)! Column: ", COL_NAME, ", Choice: ", C, ". Also not a missing code.")
                  }
                } else {
                  warning("Mismatch in choices compared to REDCap (above)! Column: ", COL_NAME, ", Choice: ", C)
                }
              }
            }
            OUT
          }) %>%
          unlist() %>%
          as.character()
      } else {
        if (use_missing_codes) {
          z <- project$metadata$missing_codes
          FORM[[COL_NAME]] <- FORM[[COL_NAME]] %>%
            lapply(function(C) {
              OUT <- C
              if (!is.na(C)) {
                D <- which(z$code == C)
                if (length(D) > 0) {
                  OUT <- z$name[D]
                }
              }
              OUT
            }) %>%
            unlist() %>%
            as.character()
        }
      }
    }
  }
  FORM
}
#' @noRd
stack_vars <- function(project, vars, new_name, drop_na = TRUE) {
  project <- assert_blank_project(project)
  fields <- project$metadata$fields
  if (!all(vars %in% fields$field_name)) stop("all vars must be in metadata.")
  the_stack <- NULL
  for (var in vars) { # var <- vars %>% sample1()
    DF <- filter_project(project, field_names = var)[[1]]
    colnames(DF)[which(colnames(DF) == var)] <- new_name
    the_stack <- the_stack %>% dplyr::bind_rows(DF)
  }
  if (drop_na) {
    the_stack <- the_stack[which(!is.na(the_stack[[new_name]])), ]
  }
  return(the_stack)
}
#' @noRd
get_original_field_names <- function(project) {
  if (project$internals$is_transformed) {
    return(project$transformation$original_fields$field_name)
  }
  return(project$metadata$fields$field_name)
}
#' @noRd
get_all_field_names <- function(project) {
  return(project$data %>% lapply(colnames) %>% unlist() %>% unique())
}
#' @noRd
field_names_to_form_names <- function(project, field_names) {
  form_key_cols <- project$metadata$form_key_cols %>%
    unlist() %>%
    unique()
  field_names_keys <- field_names[which(field_names %in% form_key_cols)]
  form_names_keys <- field_names_keys %>%
    lapply(function(field_name) {
      project$metadata$form_key_cols %>%
        names() %>%
        lapply(function(FORM) {
          if (!field_name %in% project$metadata$form_key_cols[[FORM]]) {
            return(NULL)
          }
          return(FORM)
        }) %>%
        unlist()
    }) %>%
    unlist() %>%
    as.character() %>%
    unique()
  fields <- project$metadata$fields
  field_names_not_keys <- field_names[which(!field_names %in% form_key_cols)]
  form_names_not_keys <- fields$form_name[match(field_names_not_keys, fields$field_name)] %>% drop_nas()
  form_names <- c(form_names_not_keys, form_names_keys) %>% unique()
  return(form_names)
}
#' @noRd
form_names_to_field_names <- function(form_names, project, original_only = FALSE) {
  field_names <- NULL
  if (original_only) {
    fields <- get_original_fields(project)
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
  return(
    project$metadata$forms$form_label[
      match(
        x = form_names,
        table = project$metadata$forms$form_name
      )
    ]
  )
}
#' @noRd
form_labels_to_form_names <- function(form_labels, project) {
  return(
    project$metadata$forms$form_name[
      match(
        x = form_labels,
        table = project$metadata$forms$form_label
      )
    ]
  )
}
#' @noRd
field_names_to_field_labels <- function(field_names, project) {
  return(
    project$metadata$fields$field_label[
      match(
        x = field_names,
        table = project$metadata$fields$field_name
      )
    ]
  )
}
#' @noRd
construct_header_list <- function(DF_list, md_elements = c("form_name", "field_type", "field_label"), fields) {
  if (anyDuplicated(fields$field_name) > 0) stop("dup names not allowed in fields")
  df_col_list <- DF_list %>% lapply(colnames)
  header_df_list <- df_col_list %>% lapply(function(field_names) {
    x <- field_names %>%
      lapply(function(field_name) {
        row <- which(fields$field_name == field_name)
        if (length(row) > 0) {
          return(as.character(fields[md_elements][row, ]))
        } else {
          return(rep("", length(md_elements)))
        }
      }) %>%
      as.data.frame()
    colnames(x) <- field_names
    x <- x[which(apply(x, 1, function(row) {
      any(row != "")
    })), ]
    x
  })
  return(header_df_list)
}
#' @noRd
stripped_project <- function(project) {
  project$redcap$log <- list()
  project$data <- list()
  project$data_updates <- list()
  return(project)
}
#' @noRd
filter_DF_list <- function(DF_list, project, filter_field, filter_choices, form_names, field_names, warn_only = FALSE, no_duplicate_cols = FALSE) {
  if (missing(field_names)) field_names <- project %>% get_all_field_names()
  if (is.null(field_names)) field_names <- project %>% get_all_field_names()
  if (missing(form_names)) form_names <- names(DF_list)
  if (is.null(form_names)) form_names <- names(DF_list)
  out_list <- list()
  form_key_cols <- project$metadata$form_key_cols %>%
    unlist() %>%
    unique()
  is_key <- filter_field %in% form_key_cols
  if (!is_key) {
    form_name <- field_names_to_form_names(project, field_names = filter_field)
    is_repeating_filter <- project$metadata$forms$repeating[which(project$metadata$forms$form_name == form_name)]
  }
  for (FORM in form_names) {
    DF <- DF_list[[FORM]]
    is_repeating_form <- project$metadata$forms$repeating[which(project$metadata$forms$form_name == FORM)]
    if (is_something(DF)) {
      filter_field_final <- filter_field
      filter_choices_final <- filter_choices
      if (!is_key) {
        if (is_repeating_filter) {
          if (!is_repeating_form) {
            filter_field_final <- project$metadata$form_key_cols[[FORM]]
            filter_choices_final <- DF_list[[form_name]][[filter_field_final]][which(DF_list[[form_name]][[filter_field]] %in% filter_choices)] %>% unique()
          }
        }
      }
      rows <- which(DF_list[[FORM]][[filter_field_final]] %in% filter_choices_final)
      field_names_adj <- field_names
      if (no_duplicate_cols) field_names_adj <- field_names_adj %>% vec1_in_vec2(form_names_to_field_names(FORM, project, original_only = FALSE))
      cols <- colnames(DF)[which(colnames(DF) %in% field_names_adj)]
      if (length(rows) > 0 && length(cols) > 0) {
        cols <- colnames(DF)[which(colnames(DF) %in% unique(c(project$metadata$form_key_cols[[FORM]], field_names_adj)))]
        out_list[[FORM]] <- DF[rows, cols]
      }
    }
  }
  return(out_list)
}
#' @noRd
field_names_metadata <- function(project, field_names, col_names) {
  fields <- get_original_fields(project) # project$metadata$fields
  # if(!deparse(substitute(FORM))%in%project$metadata$forms$form_name)stop("To avoid potential issues the form name should match one of the instrument names" )
  BAD <- field_names[which(!field_names %in% c(project$metadata$fields$field_name, project$redcap$raw_structure_cols, "arm_number", "event_name"))]
  if (length(BAD) > 0) stop("All column names in your form must match items in your metadata, `project$metadata$fields$field_name`... ", paste0(BAD, collapse = ", "))
  # metadata <- project$metadata$fields[which(project$metadata$fields$form_name%in%instruments),]
  fields <- fields[which(fields$field_name %in% field_names), ]
  # metadata <- metadata[which(metadata$field_name%in%field_names),]
  if (!missing(col_names)) {
    if (is_something(col_names)) fields <- fields[[col_names]]
  }
  return(fields)
}
#' @noRd
filter_fields_from_form <- function(FORM, project) {
  forms <- project %>% field_names_to_form_names(field_names = colnames(FORM))
  if (any(forms %in% get_original_forms(project)$repeating)) stop("All column names in your form must match only one form in your metadata, `project$metadata$forms$form_name`, unless they are all non-repeating")
  fields <- project %>% field_names_metadata(field_names = colnames(FORM))
  fields <- fields[which(fields$field_type != "descriptive"), ]
  fields$has_choices <- !is.na(fields$select_choices_or_calculations)
  fields$has_choices[which(fields$field_type == "calc")] <- FALSE
  return(fields)
}
#' @noRd
labelled_to_raw_project <- function(project) {
  project <- assert_blank_project(project)
  if (!project$internals$labelled) stop("project is already raw/coded (not labelled values)")
  for (TABLE in names(project$data)) {
    project$data[[TABLE]] <- labelled_to_raw_form(FORM = project$data[[TABLE]], project = project)
  }
  project$internals$labelled <- FALSE
  project
}
#' @noRd
DF_list_to_text <- function(DF_list, project, drop_nas = TRUE, clean_names = TRUE) {
  output_list <- c()
  for (i in seq_along(DF_list)) {
    DF <- DF_list[[i]]
    the_raw_name <- names(DF_list)[[i]]
    the_name <- the_raw_name
    if (clean_names) the_name <- project$metadata$forms$form_label[which(project$metadata$forms$form_name == the_raw_name)]
    df_name <- paste0("----- ", the_name, " Table -----")
    output_list <- c(output_list, paste0("&nbsp;&nbsp;<strong>", df_name, "</strong><br>"))
    key_col_names <- project$metadata$form_key_cols[[the_raw_name]]
    for (j in seq_len(nrow(DF))) {
      for (col_name in colnames(DF)) {
        entry <- DF[j, col_name]
        if (!col_name %in% key_col_names) {
          if (!is.na(entry) || !drop_nas) {
            entry <- gsub("\\n", "<br>", entry)
            col_name_clean <- col_name
            if (clean_names) col_name_clean <- project$metadata$fields$field_label[which(project$metadata$fields$field_name == col_name)]
            output_list <- c(output_list, paste0("&nbsp;&nbsp;<strong>", col_name_clean, ":</strong> <br>&nbsp;&nbsp;&nbsp;&nbsp;", entry, "<br>"))
          }
        }
      }
      # output_list <- c(output_list, "<br>")
    }
    output_list <- c(output_list, "<br>")
  }
  return(output_list)
}
#' @noRd
check_project_for_IDs <- function(project, required_percent_filled = 0.7) {
  cols <- NULL
  if (is_something(project)) {
    if (is_something(project$data)) {
      DF <- project$data[[project$metadata$forms$form_name[which(!project$metadata$forms$repeating)][[1]]]]
      IN_length <- DF %>% nrow()
      cols <- colnames(DF)[DF %>%
        lapply(function(IN) {
          OUT <- FALSE
          x <- IN %>% drop_nas()
          if ((length(x) / IN_length) > required_percent_filled) {
            OUT <- anyDuplicated(x) == 0
          }
          return(OUT)
        }) %>%
        unlist() %>%
        which()]
    }
  }
  return(cols)
}
