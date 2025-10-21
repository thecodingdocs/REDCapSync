#' @noRd
default_project_transformation <- function(project) {
  lifecycle::signal_stage("experimental", "default_project_transformation()")
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
add_default_transformation <- function(project) {
  project$transformation$forms <- default_project_transformation(project = project)
  invisible(project)
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
#' @noRd
add_default_summaries <- function(project,
                                  exclude_identifiers = FALSE,
                                  exclude_free_text = FALSE,
                                  date_handling = "none") {
  assert_logical(exclude_identifiers)
  assert_logical(exclude_free_text)
  summary_name <- "REDCapSync_raw"
  project <- add_project_summary(
    project = project,
    summary_name = summary_name,
    transform = FALSE,
    filter_list = NULL,
    exclude_identifiers = exclude_identifiers,
    exclude_free_text = exclude_free_text,
    date_handling = "none",
    labelled = FALSE,
    clean = FALSE,
    drop_blanks = FALSE,
    drop_others = NULL,
    include_metadata = TRUE,
    annotate_metadata = FALSE,
    include_record_summary = FALSE,
    include_users = TRUE,
    include_log = FALSE,
    with_links = nrow(project$summary$all_records) <= 3000,
    separate = TRUE,
    use_csv = project$internals$use_csv,
    dir_other = file.path(project$dir_path, "REDCap", project$short_name),
    file_name = project$short_name
  )
  summary_name <- "REDCapSync"
  transform <- FALSE
  if(project$internals$is_transformed||
     is_something(project$transformation$forms)||
     is_something(project$transformation$fields)
  ){
    transform <- TRUE
  }
  !is_something(project$transformation$forms)
  project <- add_project_summary(
    project = project,
    summary_name = summary_name,
    transform = transform,
    filter_list = NULL,
    exclude_identifiers = exclude_identifiers,
    exclude_free_text = exclude_free_text,
    date_handling = "none",
    upload_compatible = TRUE,
    labelled = TRUE,
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
transform_project <- function(data_list,transformation_list) {
  has_data <- is_something(data_list$data)
  the_names <- transformation_list$fields$field_name
  has_fields <- !is.null(the_names)
  has_transformation <- is_something(transformation_list$forms)
  if (!has_data) {
    cli_alert_warning("No data... nothing to do!")
  }
  if (!has_fields) {
    cli_alert_danger("No additional fields. Use `add_project_field()`")
  }
  if (!has_transformation) {
    cli_alert_warning("No transformation. Use `add_project_transformation()`")
  }
  forms_transformation <- transformation_list$forms
  forms_transformation_original <- forms_transformation
  all_records <- data_list$summary$all_records
  check_logical <- !all_records$was_transformed
  id_col <- data_list$redcap$id_col
  needs_full <- all(check_logical)
  needs_nothing <- !any(check_logical)
  needs_partial <- !needs_full && !needs_nothing
  if (needs_nothing) {
    cli_alert_success("Everything transformed already.")
  }
  if (needs_nothing || !has_transformation || !has_data) {
    return(invisible(data_list))
  }
  named_df_list <- data_list$data
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
    if(needs_full || needs_partial){
      data_list$data <- form_list
    }
  }
  if (!is.null(data_list$metadata$form_key_cols)) {
    forms_transformation$key_cols <- forms_transformation$form_name %>%
      lapply(function(x) {
        data_list$metadata$form_key_cols[[x]] %>% paste0(collapse = "+")
      }) %>%
      unlist()
    forms_transformation$key_names <- forms_transformation$form_name %>%
      lapply(function(x) {
        row_match <- which(forms_transformation$form_name == x)
        if (!forms_transformation$repeating[row_match]) {
          return(data_list$metadata$form_key_cols[[x]])
        }
        paste0(forms_transformation$form_name[row_match], "_key")
      })
  }
  cli_alert_wrap(
    paste0(
      data_list$short_name,
      " transformed according to `transformation_list`"
    ),
    bullet_type = "v"
  )
  # forms ---------
  # new function RosyUtils
  # should separate out of this function
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
  data_list$metadata$forms <- forms_transformation
  # fields------------
  # fields <- combine_project_fields(data_list)
  # fields$original_form_name <- fields$form_name
  # fields$form_name <- forms_transformation_original$form_name_remap[match(fields$form_name, forms_transformation_original$form_name)]
  # fields <- fields[order(match(fields$form_name, forms_transformation$form_name)), ]
  # # new function RosyUtils
  # first <- 1:which(colnames(fields) == "form_name")
  # move <- which(colnames(fields) == "original_form_name")
  # last <- which(colnames(fields) != "original_form_name")[-first]
  # fields <- fields[, c(first, move, last)]
  # data_list$metadata$fields <- fields
  # cli_alert_wrap(
  #   paste0(
  #     "Added mod fields to ",
  #     data_list$short_name,
  #     " `data_list$transformation`"
  #   ),
  #   bullet_type = "v"
  # )
  # data_list$metadata$choices <- fields_to_choices(fields)
  # data_list$metadata$form_key_cols <-
  #   get_key_col_list(project = data_list, transform = TRUE)
  #end --------
  # row_match <- which(all_records[[id_col]]%in%new_records)
  #TODO
  # all_records$was_transformed <- TRUE
  # all_records$was_saved <- FALSE
  # data_list$summary$all_records <- all_records
  #TODO
  # if(data_list$internals$offload_transformation){
  #   saveRDS(
  #     data_list$transformation$data,
  #     file = get_project_path2(
  #       data_list = data_list,
  #       type = "transformation",
  #       check_dir = TRUE
  #     )
  #   )
  #   data_list$transformation$data <- NULL
  # }
  #TODO
  # data_list$internals$last_data_transformation <- now_time()
  invisible(data_list)
}
#' @noRd
missing_form_names <- function(project) {
  form_names <- names(project$data)
  form_names <- form_names[which(!form_names %in% project$metadata$forms$form_name)]
  form_names
}
