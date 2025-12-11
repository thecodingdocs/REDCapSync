#' @noRd
add_redcap_links_to_form <- function(form, project) {
  # add instance links
  if (project$metadata$id_col %in% colnames(form)) {
    form_structure_cols <- project$metadata$raw_structure_cols[
      which(project$metadata$raw_structure_cols %in% colnames(form))]
    form_structure_cols <- project$metadata$raw_structure_cols[which(
      project$metadata$raw_structure_cols %in% colnames(form) &
        project$metadata$raw_structure_cols != project$metadata$id_col
    )]
    link_head <- project$links$redcap_record_home
    link_tail <- paste0("&id=", form[[project$metadata$id_col]])
    if ("redcap_repeat_instrument" %in% form_structure_cols) {
      link_head <- project$links$redcap_record_subpage
      link_tail <- link_tail |>
        paste0("&page=", form[["redcap_repeat_instrument"]])
    }
    if ("redcap_repeat_instance" %in% form_structure_cols) {
      link_head <- project$links$redcap_record_subpage
      link_tail <- link_tail |>
        paste0("&instance=", form[["redcap_repeat_instance"]])
    }
    form$redcap_link <- paste0(link_head, link_tail)
    if ("arm_number" %in% colnames(form)) {
      form$redcap_link <- form$redcap_link |>
        paste0("&arm=", form[["arm_number"]])
    }
  }
  form
}
#' @noRd
remove_from_form_list <- function(form_list, id_col, records = NULL) {
  if (!is_something(form_list)) {
    return(form_list)
  }
  if (!is_df_list(form_list)) {
    stop("form_list is not a list of data.frames as expected.")
  }
  if (is.null(records)) {
    return(form_list)
  }
  form_names <- names(form_list)[
    which(unlist(lapply(names(form_list), function(form_name) {
      nrow(form_list[[form_name]]) > 0L
  })))]
  for (form_name in form_names) {
    chosen_rows <- which(!form_list[[form_name]][[id_col]] %in% records)
    form_list[[form_name]] <- form_list[[form_name]][chosen_rows, ]
  }
  form_list
}
#' @noRd
remove_records_from_project <- function(project, records) {
  id_col <- project$metadata$id_col
  if (length(records) == 0L) {
    stop(
      "no records supplied to remove_records_from_project, but it's used in",
      "update which depends on records."
    )
  }
  project$data <- remove_from_form_list(form_list = project$data,
                                        id_col = id_col,
                                        records = records)
  project$transformation$data <- remove_from_form_list(
    form_list = project$transformation$data,
    id_col = id_col,
    records = records
  )
  invisible(project)
}
#' @noRd
split_choices <- function(x) {
  oops <- x
  # added this to account for redcap metadata output if not a number
  x <- gsub("\n", " | ", x)
  x <- x |>
    strsplit("[:|:]") |>
    trimws() |>
    unlist()
  check_length <- length(x)
  result <- stringr::str_match(string = x, pattern = "([^,]+),(.*)")
  x <- data.frame(
    code = result[, 2L] |> trimws(),
    name = result[, 3L] |> trimws(),
    stringsAsFactors = FALSE
  )
  rownames(x) <- NULL
  if (any(is.na(x$code)))
    stop("split choice error: ", oops)
  if (any(is.na(x$name)))
    stop("split choice error: ", oops)
  if (nrow(x) != check_length)
    stop("split choice error: ", oops)
  x
}
#' @noRd
.field_types_not_in_data <- c("descriptive", "checkbox")
#' @noRd
get_match <- function(df, return_field, match_field, match_text) {
  df[[return_field]][which(df[[match_field]] %in% match_text)]
}
#' @noRd
file_ext_alias <- function(x) {
  pos <- regexpr("\\.([[:alnum:]]+)$", x)
  ifelse(pos > -1L, substring(x, pos + 1L), "")
}
