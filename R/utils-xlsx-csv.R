#' @noRd
list_to_excel <- function(input_list,
                          dir,
                          file_name = NULL,
                          separate = FALSE,
                          overwrite = TRUE,
                          key_cols_list = list(),
                          derived_cols_list = list(),
                          link_col_list = list(),
                          str_trunc_length = 32000L,
                          header_df_list = NULL,
                          header_style = getOption("REDCapSync.header_style"),
                          body_style = getOption("REDCapSync.body_style"),
                          freeze_header = TRUE,
                          pad_rows = 0L,
                          pad_cols = 0L,
                          freeze_keys = TRUE,
                          drop_empty = TRUE) {
  if (is.null(key_cols_list)) {
    key_cols_list <- list()
  }
  if (is.null(link_col_list)) {
    link_col_list <- list()
  }
  if (is.null(derived_cols_list)) {
    derived_cols_list <- list()
  }
  input_list <- process_df_list(input_list, drop_empty = drop_empty)
  list_names <- names(input_list)
  if (length(input_list) == 0L) {
    return(warning("empty input_list cannot be saved", immediate. = TRUE))
  }
  if (separate) {
    for (i in seq_along(input_list)) {
      sub_list <- input_list[i]
      file_name2 <- names(sub_list)
      if (!is.null(file_name)) {
        file_name2 <- paste0(file_name, "_", file_name2)
      }
      keep_keys <- which(names(key_cols_list) == list_names[i])
      keep_deriveds <- which(names(derived_cols_list) == list_names[i])
      key_cols_list_i <- key_cols_list[keep_keys]
      derived_cols_list_i <- derived_cols_list[keep_deriveds]
      save_wb(
        wb = list_to_wb(
          input_list = sub_list,
          key_cols_list = key_cols_list_i,
          derived_cols_list = derived_cols_list_i,
          link_col_list = link_col_list,
          str_trunc_length = str_trunc_length,
          header_df_list = header_df_list,
          header_style = header_style,
          body_style = body_style,
          freeze_header = freeze_header,
          pad_rows = pad_rows,
          pad_cols = pad_cols,
          freeze_keys = freeze_keys,
          drop_empty = drop_empty
        ),
        dir = dir,
        file_name = file_name2,
        overwrite = overwrite
      )
    }
  } else {
    save_wb(
      wb = list_to_wb(
        input_list = input_list,
        key_cols_list = key_cols_list,
        derived_cols_list = derived_cols_list,
        link_col_list = link_col_list,
        str_trunc_length = str_trunc_length,
        header_df_list = header_df_list,
        header_style = header_style,
        body_style = body_style,
        freeze_header = freeze_header,
        pad_rows = pad_rows,
        pad_cols = pad_cols,
        freeze_keys = freeze_keys,
        drop_empty = drop_empty
      ),
      dir = dir,
      file_name = file_name,
      overwrite = overwrite
    )
  }
}
#' @noRd
save_wb <- function(wb, dir, file_name, overwrite = TRUE) {
  if (!dir.exists(dir))
    stop("dir doesn't exist")
  path <- file.path(dir, paste0(file_name, ".xlsx")) |> sanitize_path()
  openxlsx::saveWorkbook(wb = wb,
                         file = path,
                         overwrite = overwrite)
  cli_alert_wrap(paste0("Saved '", basename(path), "'!"), file = path)
}
#' @noRd
list_to_csv <- function(input_list,
                        dir,
                        file_name = NULL,
                        overwrite = TRUE,
                        drop_empty = TRUE) {
  input_list <- process_df_list(input_list, drop_empty = drop_empty)
  for (i in seq_along(input_list)) {
    sub_list <- input_list[i]
    file_name2 <- names(sub_list)
    if (!is.null(file_name)) {
      file_name2 <- paste0(file_name, "_", file_name2)
    }
    save_csv(
      form = sub_list[[1L]],
      dir = dir,
      file_name = file_name2,
      overwrite = overwrite
    )
  }
}
#' @noRd
save_csv <- function(form, dir, file_name, overwrite = TRUE) {
  if (!dir.exists(dir))
    stop("dir doesn't exist")
  path <- file.path(dir, paste0(file_name, ".csv")) |> sanitize_path()
  write_it <- TRUE
  if (!overwrite && file.exists(path)) {
    write_it <- FALSE
    cli_alert_wrap(paste0("Already a file!"), file = path)
  }
  if (write_it) {
    write.csv(x = form, file = path, row.names = FALSE)
    cli_alert_wrap(paste0("Saved '", basename(path), "'!"), file = path)
  }
}
#' @noRd
list_to_wb <- function(input_list,
                       key_cols_list = list(),
                       derived_cols_list = list(),
                       link_col_list = list(),
                       str_trunc_length = 32000L,
                       header_df_list = NULL,
                       header_style = getOption("REDCapSync.header_style"),
                       body_style = getOption("REDCapSync.body_style"),
                       freeze_header = TRUE,
                       pad_rows = 0L,
                       pad_cols = 0L,
                       freeze_keys = TRUE,
                       drop_empty = TRUE) {
  if (is.null(key_cols_list)) {
    key_cols_list <- list()
  }
  if (is.null(link_col_list)) {
    link_col_list <- list()
  }
  if (is.null(derived_cols_list)) {
    derived_cols_list <- list()
  }
  wb <- openxlsx::createWorkbook()
  input_list <- process_df_list(input_list, drop_empty = drop_empty)
  list_names <- names(input_list)
  list_link_names <- list()
  if (length(link_col_list) > 0L) {
    if (is_named_list(link_col_list)) {
      if (!all(names(link_col_list) %in% list_names)) {
        for (list_name in list_names) {
          list_link_names[[list_name]] <- link_col_list
        }
      }
    }
  }
  list_names_rename <- rename_list_names_excel(list_names = list_names)
  for (i in seq_along(list_names)) {
    wb <- form_to_wb(
      wb = wb,
      form = input_list[[list_names[i]]],
      form_name = list_names_rename[i],
      key_cols = key_cols_list[[list_names[i]]],
      derived_cols = NULL,
      link_col_list = list_link_names[[list_names[i]]],
      str_trunc_length = str_trunc_length,
      header_df = header_df_list[[list_names[i]]],
      header_style = header_style,
      body_style = body_style,
      freeze_header = freeze_header,
      pad_rows = pad_rows,
      pad_cols = pad_cols,
      freeze_keys = freeze_keys
    )
  }
  wb
}
#' @noRd
form_to_wb <- function(form,
                       form_name,
                       wb = openxlsx::createWorkbook(),
                       key_cols = NULL,
                       derived_cols = NULL,
                       link_col_list = list(),
                       str_trunc_length = 32000L,
                       header_df = NULL,
                       header_style = getOption("REDCapSync.header_style"),
                       body_style = getOption("REDCapSync.body_style"),
                       freeze_header = TRUE,
                       pad_rows = 0L,
                       pad_cols = 0L,
                       freeze_keys = TRUE) {
  if (nchar(form_name) > 31L) {
    stop(form_name, " is longer than 31 char")
  }
  form[] <- lapply(form, function(col) {
    out <- col
    if (is.character(col)) {
      out <- str_trunc(col, str_trunc_length, ellipsis = "")
    }
    out
  })
  hyperlink_col <- NULL
  if (is_something(header_df)) {
    names(header_df)[match(names(form), names(header_df))]
    missing_headers <- names(form) |>  vec1_not_in_vec2(names(header_df))
    if (length(missing_headers) > 0L) {
      for (missing_header in missing_headers){
        header_df[[missing_header]] <- ""
        #fix later
      }
    }
  }
  if (freeze_keys) {
    all_cols <- colnames(form)
    if (!all(key_cols %in% all_cols)) {
      stop("all key_cols must be in the forms")
    }
    freeze_key_cols <- which(all_cols %in% key_cols)
    if (length(freeze_key_cols) > 0L) {
      if (!is_consecutive_srt_1(freeze_key_cols)) {
        # warning?
        non_key_cols <- seq_len(ncol(form))
        non_key_cols <- non_key_cols[which(!non_key_cols %in% freeze_key_cols)]
        new_col_order <- c(freeze_key_cols, non_key_cols)
        if (is_something(header_df)) {
          header_df <- header_df[, new_col_order]
        }
        form <- form[, new_col_order]
      }
    }
  }
  openxlsx::addWorksheet(wb, form_name)
  start_row_header <- pad_rows + 1L
  start_row_table <- start_row_header
  start_col <- pad_cols + 1L
  if (is_something(header_df)) {
    openxlsx::writeData(
      wb,
      sheet = form_name,
      x = header_df,
      startRow = start_row_header,
      startCol = start_col,
      colNames = FALSE
    )
    start_row_table <- start_row_header + nrow(header_df)
  }
  if (length(link_col_list) > 0L) {
    has_names <- !is.null(names(link_col_list))
    for (i in seq_along(link_col_list)) {
      if (link_col_list[[i]] %in% colnames(form)) {
        class(form[[link_col_list[[i]]]]) <- "hyperlink"
      } # else warning?
      if (has_names) {
        if (names(link_col_list)[i] %in% colnames(form)) {
          hyperlink_col <- which(colnames(form) == names(link_col_list)[i])
          openxlsx::writeData(
            wb,
            sheet = form_name,
            x = form[[link_col_list[[i]]]],
            startRow = start_row_table + 1L,
            startCol = hyperlink_col + pad_cols
          )
          form[[link_col_list[[i]]]] <- NULL
        } # else warning?
      }
    }
  }
  openxlsx::writeDataTable(
    wb,
    sheet = form_name,
    x = form,
    startRow = start_row_table,
    startCol = start_col,
    tableStyle = "none"
  )
  # add derived style
  style_cols <- seq_len(ncol(form)) + pad_cols
  openxlsx::addStyle(
    wb,
    sheet = form_name,
    style = header_style,
    rows = seq(from = start_row_header, to = start_row_table),
    cols = style_cols,
    gridExpand = TRUE,
    stack = TRUE
  )
  openxlsx::addStyle(
    wb,
    sheet = form_name,
    style = body_style,
    rows = seq_len(nrow(form)) + start_row_table,
    cols = style_cols,
    gridExpand = TRUE,
    stack = TRUE
  )
  if (freeze_header || freeze_keys) {
    first_active_row <- NULL
    if (freeze_header) {
      first_active_row <- start_row_table + 1L
    }
    first_active_col <- NULL
    if (freeze_keys) {
      first_active_col <- start_col
      freeze_key_cols <- which(colnames(form) %in% key_cols)
      if (length(freeze_key_cols) > 0L) {
        if (is_consecutive_srt_1(freeze_key_cols)) {
          first_active_col <- first_active_col +
            freeze_key_cols[length(freeze_key_cols)]
        } else {
          the_warning <- "key_cols should be consecutive and start at the left."
          warning(the_warning, immediate. = TRUE)
        }
      }
      openxlsx::freezePane(wb,
                           form_name,
                           firstActiveRow = first_active_row,
                           firstActiveCol = first_active_col)
    }
  }
  wb
}
#' @noRd
rename_list_names_excel <- function(list_names) {
  list_names_rename <- str_trunc(list_names,
                                 width = 31L,
                                 side = "right",
                                 ellipsis = "")
  bad_names <- which_duplicated(list_names_rename)
  if (length(bad_names) > 0L) {
    cli_alert_danger(paste0(
      "Duplicated names when trimmed from right 31 max in Excel: ",
      toString(list_names[bad_names])
    ))
    cli_alert_info(
      paste0(
        "Use CSV or shorten the names and make sure they are unique if they",
        " are trimmed to 31 char. For now will make unique by adding number."
      )
    )
    list_names_rename <-
      unique_trimmed_strings(list_names_rename, max_length = 31L)
  }
  list_names_rename
}
#' @noRd
excel_to_list <- function(path) {
  sheets <- openxlsx::getSheetNames(path)
  names(sheets) <- seq_along(sheets)
  clean_sheets <- clean_env_names(sheets)
  out <- list()
  if ("summary_details" %in% sheets) {
    summary_details <- readxl::read_xlsx(
      path,
      col_types = "text",
      sheet = which(sheets == "summary_details")
    )
    if (all(c("paramater", "value") %in% colnames(summary_details))) {
      the_row <- which(summary_details$paramater == "raw_form_names")
      form_names <- summary_details$value[the_row] |>
        strsplit(" [|] ") |>
        unlist()
      the_row <- which(summary_details$paramater == "cols_start")
      cols_start <- as.integer(summary_details$value[the_row])
      if (cols_start > 1L) {
        for (i in as.integer(names(sheets)[match(form_names, sheets)])) {
          suppressMessages({
            out[[i]] <- readxl::read_xlsx(path,
                                          col_types = "text",
                                          sheet = i,
                                          col_names = FALSE)
          })
          final_nrow <- nrow(out[[i]])
          if (cols_start < final_nrow) {
            true_colnames <- out[[i]][cols_start, ] |> unlist() |> unname()
            out[[i]] <- out[[i]][(cols_start + 1L):final_nrow, ]
            colnames(out[[i]]) <- true_colnames
            sheets <- sheets[which(sheets != sheets[i])]
          }
        }
      }
    }
  }
  for (i in as.integer(names(sheets))) {
    out[[i]] <- readxl::read_xlsx(path, col_types = "text", sheet = i)
  }
  names(out) <- clean_sheets
  out
}
#' @noRd
.header_style <- openxlsx::createStyle(fgFill = "#74DFFF",
                                      halign = "center",
                                      valign = "center",
                                      textDecoration = "Bold",
                                      fontSize = 14L,
                                      fontColour = "black",
                                      border = "TopBottomLeftRight")
#' @noRd
.body_style <- openxlsx::createStyle(halign = "left",
                                    valign = "center",
                                    fontSize = 12L)
