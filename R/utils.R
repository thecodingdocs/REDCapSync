#' @noRd
add_redcap_links_to_form <- function(form, project) { # add instance links
  if (project$redcap$id_col %in% colnames(form)) {
    form_structure_cols <- project$redcap$raw_structure_cols[which(project$redcap$raw_structure_cols %in% colnames(form))]
    form_structure_cols <- project$redcap$raw_structure_cols[which(project$redcap$raw_structure_cols %in% colnames(form) & project$redcap$raw_structure_cols != project$redcap$id_col)]
    link_head <- project$links$redcap_record_home
    link_tail <- "&id=" %>% paste0(form[[project$redcap$id_col]])
    if ("redcap_repeat_instrument" %in% form_structure_cols) {
      link_head <- project$links$redcap_record_subpage
      link_tail <- link_tail %>% paste0("&page=", form[["redcap_repeat_instrument"]])
    }
    if ("redcap_repeat_instance" %in% form_structure_cols) {
      link_head <- project$links$redcap_record_subpage
      link_tail <- link_tail %>% paste0("&instance=", form[["redcap_repeat_instance"]])
    }
    form$redcap_link <- paste0(link_head, link_tail)
    if ("arm_number" %in% colnames(form)) {
      form$redcap_link <- form$redcap_link %>% paste0("&arm=", form[["arm_number"]])
    }
  }
  return(form)
}
#' @noRd
count_project_upload_cells <- function(project) {
  project$data_updates %>%
    lapply(function(x) {
      nrow(x) * ncol(x)
    }) %>%
    unlist() %>%
    sum()
}
#' @noRd
husk_of_form <- function(project, form_name, field_names) {
  form <- project$data[[form_name]]
  cols <- colnames(form)[which(colnames(form) %in% project$redcap$raw_structure_cols)]
  form2 <- NULL
  for (col in cols) {
    form2[[col]] <- form[[col]]
  }
  form2 <- as.data.frame(form2)
  return(form2)
}
#' @noRd
all_project_to_char_cols <- function(project) {
  project$data <- project$data %>% all_character_cols_list()
  project$data_updates <- project$data_updates %>% all_character_cols_list()
  return(invisible(project))
}
#' @noRd
add_redcap_links_table <- function(form, project) {
  if (nrow(form) > 0) {
    form[[project$redcap$id_col]] <- paste0("<a href='", paste0("https://redcap.miami.edu/redcap_v", project$redcap$version, "/DataEntry/record_home.php?pid=", project$redcap$project_id, "&id=", form[[project$redcap$id_col]], "&arm=1"), "' target='_blank'>", form[[project$redcap$id_col]], "</a>")
  }
  form
}
#' @noRd
clean_RC_col_names <- function(form, project) {
  colnames(form) <- colnames(form) %>%
    lapply(function(col) {
      x <- project$metadata$fields$field_label[which(project$metadata$fields$field_name == col)]
      if (length(x) > 1) {
        x <- x[[1]]
      }
      ifelse(length(x) > 0, x, col)
    }) %>%
    unlist() %>%
    return()
  form
}
#' @noRd
clean_RC_df_for_DT <- function(form, project) {
  form %>%
    add_redcap_links_table(project) %>%
    clean_RC_col_names(project) %>%
    return()
}
#' @noRd
remove_records_from_list <- function(project, records, silent = FALSE) {
  data_list <- project$data
  if (!is_df_list(data_list)) {
    stop("data_list is not a list of data.frames as expected.")
  }
  if (length(records) == 0) {
    stop(
      paste0(
        "no records supplied to remove_records_from_list, but it's used in",
        "update which depends on records."
      )
    )
  }
  forms <- names(data_list)[
    which(
      names(data_list) %>%
        lapply(function(form) {
          nrow(data_list[[form]]) > 0
        }) %>%
        unlist()
    )
  ]
  for (form_name in forms) {
    rows <- which(!data_list[[form_name]][[project$redcap$id_col]] %in% records)
    data_list[[form_name]] <- data_list[[form_name]][rows, ]
  }
  if (!silent) message("Removed: ", paste0(records, collapse = ", "))
  return(data_list)
}
#' @noRd
other_drops <- function(ignore = FALSE) {
  if (ignore) {
    return(NULL)
  }
  c(
    "Not applicable",
    "No information",
    "Not asked",
    "Unknown",
    "Unencoded",
    "Unknown / Not Reported",
    "Missing Dates",
    "Pediatric"
  ) %>% return()
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
sidebar_choices <- function(project, n_threshold = 1) {
  choices <- annotate_choices(project)
  choices <- choices[which(choices$n >= n_threshold), ]
  sbc <- data.frame(
    form_name = choices$form_name,
    field_name = choices$field_name,
    name = choices$name,
    label = paste0(choices$label, " (n = ", clean_num(choices$n), ")")
  )
  return(sbc)
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
