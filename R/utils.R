#' @noRd
add_redcap_links_to_form <- function(form, project) { # add instance links
  if (project$redcap$id_col %in% colnames(form)) {
    form_structure_cols <- project$redcap$raw_structure_cols[
      which(
        project$redcap$raw_structure_cols %in% colnames(form)
      )
    ]
    form_structure_cols <- project$redcap$raw_structure_cols[
      which(
        project$redcap$raw_structure_cols %in% colnames(form) &
          project$redcap$raw_structure_cols != project$redcap$id_col
      )
    ]
    link_head <- project$links$redcap_record_home
    link_tail <- paste0("&id=", form[[project$redcap$id_col]])
    if ("redcap_repeat_instrument" %in% form_structure_cols) {
      link_head <- project$links$redcap_record_subpage
      link_tail <- paste0(link_tail, "&page=", form[["redcap_repeat_instrument"]])
    }
    if ("redcap_repeat_instance" %in% form_structure_cols) {
      link_head <- project$links$redcap_record_subpage
      link_tail <- paste0(link_tail, "&instance=", form[["redcap_repeat_instance"]])
    }
    form$redcap_link <- paste0(link_head, link_tail)
    if ("arm_number" %in% colnames(form)) {
      form$redcap_link <- paste0(form$redcap_link, "&arm=", form[["arm_number"]])
    }
  }
  form
}
remove_from_form_list <- function(form_list,
                                  id_col,
                                  records = NULL
                                  ) {
  if (!is_something(form_list)) {
    return(form_list)
  }
  if (!is_df_list(form_list)) {
    stop("form_list is not a list of data.frames as expected.")
  }
  if(is.null(records)){
    return(form_list)
  }
  id_col <- project$redcap$id_col
  form_names <- names(form_list)[
    which(
      unlist(lapply(names(form_list), function(form_name) {
        nrow(form_list[[form_name]]) > 0L
      }))
    )
  ]
  for (form_name in form_names) {
    chosen_rows <- which(!form_list[[form_name]][[id_col]] %in% records)
    form_list[[form_name]] <- form_list[[form_name]][chosen_rows, ]
  }
  form_list
}
#' @noRd
remove_records_from_project <- function(project, records) {
  if (length(records) == 0L) {
    stop(
      "no records supplied to remove_records_from_project, but it's used in",
      "update which depends on records."
    )
  }
  id_col <- project$redcap$id_col
  project$summary$all_records <- project$summary$all_records[which(!project$summary$all_records[[id_col]] %in% records), ]
  project$data <- remove_from_form_list(
    form_list = project$data,
    records = records
    )
  project$transformation$data <- remove_from_form_list(
    form_list = project$transformation$data,
    records = records
  )
  invisible(project)
}
#' @noRd
ignore_redcap_log <- function(collapse = TRUE) {
  ignores <- c(
    "export",
    "download ",
    "edit report",
    "Switch DAG",
    "Copy report",
    "Multi-Language",
    "File Repository ",
    "custom record dashboard",
    "User regenerate own API token",
    "Create report",
    " external module"
  )
  if (collapse) {
    return(paste0(ignores, collapse = "|"))
  }
  return(ignores)
}
#' @noRd
split_choices <- function(x) {
  oops <- x
  # added this to account for redcap metadata output if not a number
  x <- gsub("\n", " | ", x)
  x <- x %>%
    strsplit(" [:|:] ") %>%
    unlist()
  check_length <- length(x)
  result <- stringr::str_match(string = x, pattern = "([^,]+), (.*)")
  x <- data.frame(
    code = result[, 2L],
    name = result[, 3L],
    stringsAsFactors = FALSE
  )
  rownames(x) <- NULL
  if (nrow(x) != check_length) stop("split choice error: ", oops)
  x
}
#' @noRd
redcap_field_types_not_in_data <- c(
  "descriptive", "checkbox"
)
