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
    link_tail <- "&id=" %>% paste0(form[[project$redcap$id_col]])
    if ("redcap_repeat_instrument" %in% form_structure_cols) {
      link_head <- project$links$redcap_record_subpage
      link_tail <- link_tail %>%
        paste0("&page=", form[["redcap_repeat_instrument"]])
    }
    if ("redcap_repeat_instance" %in% form_structure_cols) {
      link_head <- project$links$redcap_record_subpage
      link_tail <- link_tail %>%
        paste0("&instance=", form[["redcap_repeat_instance"]])
    }
    form$redcap_link <- paste0(link_head, link_tail)
    if ("arm_number" %in% colnames(form)) {
      form$redcap_link <- form$redcap_link %>%
        paste0("&arm=", form[["arm_number"]])
    }
  }
  form
}
#' @noRd
remove_records_from_list <- function(project, records, silent = FALSE) {
  form_list <- project$data
  if (!is_df_list(form_list)) {
    stop("form_list is not a list of data.frames as expected.")
  }
  if (length(records) == 0) {
    stop(
      paste0(
        "no records supplied to remove_records_from_list, but it's used in",
        "update which depends on records."
      )
    )
  }
  form_names <- names(form_list)[
    which(
      names(form_list) %>%
        lapply(function(form_name) {
          nrow(form_list[[form_name]]) > 0
        }) %>%
        unlist()
    )
  ]
  for (form_name in form_names) {
    rows <- which(!form_list[[form_name]][[project$redcap$id_col]] %in% records)
    form_list[[form_name]] <- form_list[[form_name]][rows, ]
  }
  if (!silent) message("Removed: ", paste0(records, collapse = ", "))
  form_list
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
  result <- x %>% stringr::str_match("([^,]+), (.*)")
  x <- data.frame(
    code = result[, 2],
    name = result[, 3]
  )
  rownames(x) <- NULL
  if (nrow(x) != check_length) stop("split choice error: ", oops)
  x
}
#' @noRd
redcap_field_types_not_in_data <- c(
  "descriptive", "checkbox"
)
