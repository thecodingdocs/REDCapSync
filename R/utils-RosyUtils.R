bullet_in_console <- function(text = "",
                              url = NULL,
                              bullet_type = "i",
                              collapse = TRUE,
                              file = NULL,
                              silent = FALSE) {
  if (silent) {
    return(invisible())
  }
  url_if <- ""
  file_if <- ""
  if (length(url) > 0) {
    # url %>% lapply(function(x){assert_web_link(x)}) # doesnt work for /subheaders/
    # url_if <- " {.url {url}}"
    url_names <- names(url)
    if (is.list(url)) {
      url_names <- url %>% unlist()
      if (is_named_list(url)) url_names <- names(url)
      url <- unlist(url)
    }
    if (is.null(url_names)) url_names <- url
    if (collapse) url_if <- paste0(url_if, collapse = " and ")
    url_if <- paste0(
      " {cli::col_blue(cli::style_hyperlink('",
      url_names,
      "', '",
      url,
      "'))}"
    )
  }
  if (length(file) > 0) {
    file_names <- names(file)
    if (is.list(file)) {
      file_names <- file %>% unlist()
      if (is_named_list(file)) file_names <- names(file)
      file <- unlist(file)
    }
    if (is.null(file_names)) file_names <- file
    if (collapse) file_if <- paste0(file_if, collapse = " and ")
    file_if <- paste0(
      " {cli::col_blue(cli::style_hyperlink('",
      sanitize_path(file_names),
      "', '",
      sanitize_path(paste0("file://", file)),
      "'))}"
    )
  }
  for (i in seq_along(url_if)) text[i] <- paste0(text[i], url_if[i])
  for (i in seq_along(file_if)) text[i] <- paste0(text[i], file_if[i])
  names(text)[seq_along(text)] <- bullet_type
  cli::cli_bullets(text)
}
now_time <- function() {
  as.POSIXct(Sys.time(), tz = Sys.timezone())
}
process_df_list <- function(list, drop_empty = TRUE, silent = FALSE) {
  if (is_something(list)) {
    if (!is_df_list(list)) stop("list must be ...... a list :)")
    if (drop_empty) {
      is_a_df_with_rows <- list %>%
        lapply(function(x) {
          is_df <- is.data.frame(x)
          out <- FALSE
          if (is_df) {
            out <- nrow(x) > 0
          }
          out
        }) %>%
        unlist()
      keeps <- which(is_a_df_with_rows)
      drops <- which(!is_a_df_with_rows)
      if (length(drops) > 0) {
        if (!silent) {
          bullet_in_console(
            "Dropping non-data.frames and empties... ",
            paste0(names(drops), collapse = ", ")
          )
        }
      }
      list <- list[keeps]
    }
    if (length(list) > 0) {
      if (!is_named_df_list(list)) {
        names(list) <- paste0(seq_along(list))
      }
    }
  }
  list
}
is_something <- function(thing, row = 0) {
  out <- FALSE
  if (is.function(thing)) {
    return(TRUE)
  }
  if (!is.null(thing)) {
    if (is.data.frame(thing)) {
      if (nrow(thing) > row) {
        out <- TRUE
      }
    } else {
      if (length(thing) > 0) {
        if (is.list(thing)) {
          out <- TRUE
        } else {
          if (length(thing) == 1) {
            if (!is.na(thing)) {
              if (is.character(thing)) {
                if (thing != "") {
                  out <- TRUE
                }
              } else {
                out <- TRUE
              }
            }
          } else {
            out <- TRUE
          }
        }
      }
    }
  }
  out
}
sanitize_path <- function(path) {
  sanitized <- gsub("\\\\", "/", path)
  sanitized <- normalizePath(sanitized, winslash = "/", mustWork = FALSE)
  sanitized
}
find_form_diff <- function(new, old, ref_cols = NULL, message_pass = "") {
  new <- all_character_cols(new)
  old <- all_character_cols(old)
  if (!all(colnames(new) %in% colnames(old))) {
    stop("All new form columns must be included in old form")
  }
  if (!all(ref_cols %in% colnames(new)) || !all(ref_cols %in% colnames(old))) {
    stop("ref_cols must be included in both dfs")
  }
  if (length(ref_cols) > 1) {
    new$key <- apply(new[, ref_cols], 1, paste, collapse = "_")
    old$key <- apply(old[, ref_cols], 1, paste, collapse = "_")
  } else {
    new$key <- new[, ref_cols]
    old$key <- old[, ref_cols]
  }
  if (anyDuplicated(old$key) > 0) {
    stop("Keys must lead to unique rows! (old form)")
  }
  if (anyDuplicated(new$key) > 0) {
    stop("Keys must lead to unique rows! (new form)")
  }
  new_keys <- integer(0)
  if (any(!new$key %in% old$key)) {
    # warning("You have at least one new key compared to old form therefore all columns will be included by default",immediate. = TRUE)
    new_keys <- which(!new$key %in% old$key)
  }
  indices <- data.frame(
    row = integer(0),
    col = integer(0)
  )
  for (new_key in new_keys) {
    indices <- indices %>% dplyr::bind_rows(
      data.frame(
        row = new_key,
        col = which(!colnames(new) %in% c(ref_cols, "key"))
      )
    )
  }
  for (key in new$key[which(new$key %in% old$key)]) {
    row <- which(new$key == key)
    row_old <- which(old$key == key)
    for (col in colnames(new)[which(!colnames(new) %in% c(ref_cols, "key"))]) {
      col <- which(colnames(new) == col)
      if (!identical(new[row, col], old[row_old, col])) {
        indices <- indices %>% dplyr::bind_rows(
          data.frame(
            row = row,
            col = col
          )
        )
      }
    }
  }
  if (nrow(indices) > 0) {
    rows <- indices$row %>%
      unique() %>%
      sort()
    cols <- which(colnames(new) %in% ref_cols) %>% append(indices$col %>% unique() %>% sort())
    out_form <- new[rows, cols]
    message(message_pass, nrow(out_form), " rows have updates")
  } else {
    out_form <- NULL
    message(message_pass, "No changes!")
  }
  out_form
}
find_form_diff2 <- function(new,
                            old,
                            ref_cols = NULL,
                            message_pass = "",
                            view_old = TRUE,
                            n_row_view = 20) {
  new <- all_character_cols(new)
  old <- all_character_cols(old)
  if (!all(colnames(new) %in% colnames(old))) {
    stop("All new form columns must be included in old form")
  }
  if (!all(ref_cols %in% colnames(new)) || !all(ref_cols %in% colnames(old))) {
    stop("ref_cols must be included in both dfs")
  }
  if (length(ref_cols) > 1) {
    new_keys <- apply(new[, ref_cols], 1, paste, collapse = "_")
    old_keys <- apply(old[, ref_cols], 1, paste, collapse = "_")
  } else {
    new_keys <- new[, ref_cols]
    old_keys <- old[, ref_cols]
  }
  if (anyDuplicated(old_keys) > 0) {
    stop("Keys must lead to unique rows! (old form)")
  }
  if (anyDuplicated(new_keys) > 0) {
    stop("Keys must lead to unique rows! (new form)")
  }
  appended_old_col_suffix <- "__old"
  if (any(endsWith(unique(colnames(old), colnames(new)), appended_old_col_suffix))) {
    stop("colnames cant end with '", appended_old_col_suffix, "'")
  }
  merged_df <- merge(new, old, by = ref_cols, suffixes = c("", appended_old_col_suffix), all.x = TRUE)
  placeholder <- "NA_placeholder"
  rows_to_keep <- NULL
  cols_to_view <- cols_to_keep <- which(colnames(merged_df) %in% ref_cols)
  cols <- colnames(new)[which(!colnames(new) %in% ref_cols)]
  for (col in cols) {
    vector1 <- merged_df[[col]]
    compare_column <- paste0(col, appended_old_col_suffix)
    vector2 <- merged_df[[compare_column]]
    vector1_no_na <- ifelse(is.na(vector1), placeholder, vector1)
    vector2_no_na <- ifelse(is.na(vector2), placeholder, vector2)
    # Compare vectors element-wise
    are_not_equal <- which(vector1_no_na != vector2_no_na)
    if (length(are_not_equal) > 0) {
      rows_to_keep <- rows_to_keep %>% append(are_not_equal)
      additional_cols <- which(colnames(merged_df) == col)
      cols_to_keep <- cols_to_keep %>% append(additional_cols)
      if (view_old) {
        cols_to_view <- cols_to_view %>%
          append(additional_cols) %>%
          append(which(colnames(merged_df) == compare_column))
      }
    }
  }
  if (length(rows_to_keep) > 0) {
    rows_to_keep <- rows_to_keep %>% unique()
    cols_to_keep <- cols_to_keep %>% unique()
    if (view_old) {
      rows_to_keep2 <- rows_to_keep
      done <- FALSE
      while (!done) {
        length_of_rows_to_keep <- length(rows_to_keep2)
        if (length_of_rows_to_keep == 0) {
          done <- TRUE
        } else {
          indices <- 1:ifelse(length_of_rows_to_keep < n_row_view, length_of_rows_to_keep, n_row_view)
          rows_to_keep3 <- rows_to_keep2[indices]
          print.data.frame(merged_df[rows_to_keep3, unique(cols_to_view)])
          choice <- utils::menu(
            choices = c(
              "Check more rows",
              "Proceed with no more checking",
              "Stop the function"
            ),
            title = "What would you like to do?"
          )
          if (choice == 3) stop("Stopped as requested!")
          if (choice == 2) done <- TRUE
          if (choice == 1) rows_to_keep2 <- rows_to_keep2[-indices]
        }
      }
    }
    message(message_pass, length(rows_to_keep), " rows have updates")
    return(merged_df[rows_to_keep, cols_to_keep])
  } else {
    message(message_pass, "No changes!")
    return(NULL)
  }
}
find_df_list_diff <- function(new_list, old_list, ref_col_list, view_old = TRUE, n_row_view = 20) {
  if (!is_something(new_list)) {
    message("new_list is empty")
    return(list())
  }
  if (!is_something(old_list)) {
    message("old_list is empty")
    return(list())
  }
  if (!is_df_list(new_list)) stop("new_list must be a list of data.frames")
  if (!is_df_list(old_list)) stop("old_list must be a list of data.frames")
  if (any(!names(new_list) %in% names(old_list))) {
    stop("All new_list names must be included in the set of old_list names.")
  }
  if (!is.list(ref_col_list)) {
    ref_col_list <- names(new_list) %>% lapply(function(x) {
      ref_col_list
    })
    names(ref_col_list) <- names(new_list)
  }
  for (df_name in names(new_list)) {
    new_list[[df_name]] <- find_form_diff2(
      new = new_list[[df_name]],
      old = old_list[[df_name]],
      ref_cols = ref_col_list[[df_name]],
      message_pass = paste0(df_name, ": "),
      view_old = view_old,
      n_row_view = n_row_view
    )
  }
  new_list
}
all_character_cols <- function(form) {
  as.data.frame(lapply(form, as.character))
}
all_character_cols_list <- function(list) {
  lapply(list, all_character_cols)
}
as_comma_string <- function(vec) {
  paste0(vec, collapse = ", ")
}
vec1_in_vec2 <- function(vec1, vec2) {
  vec1[which(vec1 %in% vec2)]
}
vec1_not_in_vec2 <- function(vec1, vec2) {
  vec1[which(!vec1 %in% vec2)]
}
ul <- function(x) {
  length(unique(x))
}
wl <- function(x) {
  length(which(x))
}
drop_nas <- function(x) {
  if (length(x) == 0) {
    return(x)
  }
  x[!unlist(lapply(x, is.na))]
}
excel_to_list <- function(path) {
  sheets <- readxl::excel_sheets(path)
  clean_sheets <- clean_env_names(sheets)
  out <- list()
  for (i in seq_along(sheets)) {
    out[[i]] <- rio::import(path, col_types = "text", sheet = i)
  }
  names(out) <- clean_sheets
  out
}
csv_to_list <- function(paths) {
  paths <- sanitize_path(paths)
  form_list <- list()
  clean_names <- paths %>%
    basename() %>%
    tools::file_path_sans_ext() %>%
    clean_env_names()
  for (i in seq_along(paths)) {
    form_list[[i]] <- utils::read.csv(paths[i],
      stringsAsFactors = FALSE,
      na.strings = c("", "NA")
    )
  }
  names(form_list) <- clean_names
  form_list
}
csv_folder_to_list <- function(folder) {
  folder <- sanitize_path(folder)
  if (!dir.exists(folder)) stop("Folder does not exist: ", folder)
  paths <- list_files_real(folder)
  paths <- paths[which(paths %>% endsWith(".csv"))]
  csv_to_list(paths = paths)
}
is_named_df_list <- function(x, strict = FALSE) {
  is_named_list(x) && is_df_list(x, strict = strict)
}
is_named_list <- function(x, silent = TRUE, recursive = FALSE) {
  if (!is.list(x)) {
    return(FALSE)
  }
  if (is.null(names(x))) {
    return(FALSE)
  }
  named_all <- TRUE
  if (recursive) {
    for (n in names(x)) {
      element <- x[[n]]
      if (is.list(element)) {
        named_all <- named_all && is_named_list(element)
        if (!silent && !named_all) message("'", n, "' is not named")
      }
    }
  }
  named_all
}
wb_to_list <- function(wb) {
  # wb <- openxlsx::loadWorkbook(file = path)
  # test for if user does not have excel
  sheets <- openxlsx::sheets(wb)
  clean_sheets <- clean_env_names(sheets)
  out <- list()
  for (i in seq_along(sheets)) {
    col_row <- 1
    x <- openxlsx::getTables(wb, sheet = i)
    if (length(x) > 0) {
      # test for xlsx without letters for cols
      col_row <- gsub(
        "[A-Za-z]",
        "",
        unlist(x %>% attr("refs") %>% strsplit(":"))[[1]]
      ) %>%
        as.integer()
    }
    out[[i]] <- openxlsx::read.xlsx(wb, sheet = i, startRow = col_row)
  }
  names(out) <- clean_sheets
  out
}
form_to_wb <- function(
    form,
    form_name,
    wb = openxlsx::createWorkbook(),
    link_col_list = list(),
    str_trunc_length = 32000,
    header_df = NULL,
    tableStyle = "none",
    header_style = default_header_style,
    body_style = default_body_style,
    freeze_header = TRUE,
    pad_rows = 0,
    pad_cols = 0,
    freeze_keys = TRUE,
    key_cols = NULL) {
  if (nchar(form_name) > 31) stop(form_name, " is longer than 31 char")
  form <- form %>%
    lapply(stringr::str_trunc, str_trunc_length, ellipsis = "") %>%
    as.data.frame()
  hyperlink_col <- NULL
  if (freeze_keys) {
    all_cols <- colnames(form)
    if (any(!key_cols %in% all_cols)) stop("all key_cols must be in the forms")
    freeze_key_cols <- which(all_cols %in% key_cols)
    if (length(freeze_key_cols) > 0) {
      if (!is_consecutive_srt_1(freeze_key_cols)) {
        warning(
          "please keep your key cols on the left consecutively. Fixing ",
          form_name,
          ": ",
          paste0(key_cols, collapse = ", "),
          ".",
          immediate. = TRUE
        )
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
  if (nrow(form) > 0) {
    openxlsx::addWorksheet(wb, form_name)
    start_row_header <- pad_rows + 1
    start_row_table <- start_row_header
    startCol <- pad_cols + 1
    if (is_something(header_df)) {
      openxlsx::writeData(
        wb,
        sheet = form_name,
        x = header_df,
        startRow = start_row_header,
        startCol = startCol,
        colNames = FALSE
      )
      start_row_table <- start_row_header + nrow(header_df)
    }
    if (length(link_col_list) > 0) {
      has_names <- !is.null(names(link_col_list))
      for (i in seq_along(link_col_list)) {
        if (link_col_list[[i]] %in% colnames(form)) {
          class(form[[link_col_list[[i]]]]) <- "hyperlink"
        } else {
          # warning("",immediate. = TRUE)
        }
        if (has_names) {
          if (names(link_col_list)[i] %in% colnames(form)) {
            hyperlink_col <- which(colnames(form) == names(link_col_list)[i])
            openxlsx::writeData(
              wb,
              sheet = form_name,
              x = form[[link_col_list[[i]]]],
              startRow = start_row_table + 1,
              startCol = hyperlink_col + pad_cols
            )
            form[[link_col_list[[i]]]] <- NULL
          } else {
            # warning("",immediate. = TRUE)
          }
        }
      }
    }
    openxlsx::writeDataTable(
      wb,
      sheet = form_name,
      x = form,
      startRow = start_row_table,
      startCol = startCol,
      tableStyle = tableStyle
    )
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
      firstActiveRow <- NULL
      if (freeze_header) {
        firstActiveRow <- start_row_table + 1
      }
      firstActiveCol <- NULL
      if (freeze_keys) {
        firstActiveCol <- startCol
        freeze_key_cols <- which(colnames(form) %in% key_cols)
        if (length(freeze_key_cols) > 0) {
          if (is_consecutive_srt_1(freeze_key_cols)) {
            firstActiveCol <- firstActiveCol + freeze_key_cols[length(freeze_key_cols)]
          } else {
            warning("key_cols must be consecutive and start from the left most column.",
              immediate. = TRUE
            )
          }
        }
        openxlsx::freezePane(wb,
          form_name,
          firstActiveRow = firstActiveRow,
          firstActiveCol = firstActiveCol
        )
      }
    }
    return(wb)
  }
}
list_to_wb <- function(
    list,
    link_col_list = list(),
    str_trunc_length = 32000,
    header_df_list = NULL,
    tableStyle = "none",
    header_style = default_header_style,
    body_style = default_body_style,
    freeze_header = TRUE,
    pad_rows = 0,
    pad_cols = 0,
    freeze_keys = TRUE,
    key_cols_list = NULL,
    drop_empty = TRUE) {
  wb <- openxlsx::createWorkbook()
  list <- process_df_list(list, drop_empty = drop_empty)
  list_names <- names(list)
  list_link_names <- list()
  if (length(link_col_list) > 0) {
    if (is_named_list(link_col_list)) {
      if (!all(names(link_col_list) %in% list_names)) {
        for (list_name in list_names) {
          list_link_names[[list_name]] <- link_col_list
        }
      }
    }
  }
  list_names_rename <- stringr::str_trunc(list_names,
    width = 31,
    side = "right",
    ellipsis = ""
  )
  bad_names <- dw(list_names_rename)
  if (length(bad_names) > 0) {
    warning(
      "Duplicated names when trimmed from right 31 max in Excel: ",
      list_names[bad_names] %>% paste0(collapse = ", "),
      immediate. = TRUE
    )
    message(
      "Use CSV or shorten the names and make sure they are unique if they are trimmed to 31 char. For now will make unique by adding number."
    )
    list_names_rename <- unique_trimmed_strings(
      list_names_rename,
      max_length = 31
    )
  }
  for (i in seq_along(list_names)) {
    header_df <- header_df_list[[list_names[i]]]
    key_cols <- key_cols_list[[list_names[i]]]
    wb <- form_to_wb(
      form = list[[list_names[i]]],
      form_name = list_names_rename[i],
      wb = wb,
      link_col_list = list_link_names[[list_names[i]]],
      str_trunc_length = str_trunc_length,
      header_df = header_df,
      tableStyle = tableStyle,
      header_style = header_style,
      body_style = body_style,
      freeze_header = freeze_header,
      pad_rows = pad_rows,
      pad_cols = pad_cols,
      freeze_keys = freeze_keys,
      key_cols = key_cols
    )
  }
  wb
}
unique_trimmed_strings <- function(strings, max_length) {
  trim_string <- function(s, max_length) {
    substr(s, 1, max_length)
  }
  trimmed_strings <- lapply(strings, trim_string, max_length = max_length) %>%
    unlist()
  # Initialize a vector to store unique strings
  unique_strings <- character(length(trimmed_strings))
  # Initialize a counter to keep track of occurrences
  counts <- integer(length(trimmed_strings))
  for (i in seq_along(trimmed_strings)) {
    base_string <- trimmed_strings[i]
    new_string <- base_string
    counter <- 1
    # Keep adjusting the string until it's unique
    while (new_string %in% unique_strings) {
      new_string <- paste0(
        stringr::str_trunc(
          base_string,
          width = max_length - (counter),
          side = "right",
          ellipsis = ""
        ),
        counter
      )
      counter <- counter + 1
    }
    unique_strings[i] <- new_string
    counts[i] <- counter
  }
  unique_strings
}
list_to_excel <- function(
    list,
    dir,
    file_name = NULL,
    separate = FALSE,
    overwrite = TRUE,
    link_col_list = list(),
    str_trunc_length = 32000,
    header_df_list = NULL,
    tableStyle = "none",
    header_style = default_header_style,
    body_style = default_body_style,
    freeze_header = TRUE,
    pad_rows = 0,
    pad_cols = 0,
    freeze_keys = TRUE,
    key_cols_list = NULL,
    drop_empty = TRUE) {
  list <- process_df_list(list, drop_empty = drop_empty)
  list_names <- names(list)
  if (length(list) == 0) {
    return(warning("empty list cannot be saved", immediate. = TRUE))
  }
  if (separate) {
    for (i in seq_along(list)) {
      sub_list <- list[i]
      file_name2 <- names(sub_list)
      if (!is.null(file_name)) {
        file_name2 <- paste0(file_name, "_", file_name2)
      }
      save_wb(
        wb = list_to_wb(
          list = sub_list,
          link_col_list = link_col_list,
          str_trunc_length = str_trunc_length,
          header_df_list = header_df_list,
          tableStyle = tableStyle,
          header_style = header_style,
          body_style = body_style,
          freeze_header = freeze_header,
          pad_rows = pad_rows,
          pad_cols = pad_cols,
          freeze_keys = freeze_keys,
          key_cols_list = key_cols_list[[list_names[i]]],
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
        list = list,
        link_col_list = link_col_list,
        str_trunc_length = str_trunc_length,
        header_df_list = header_df_list,
        tableStyle = tableStyle,
        header_style = header_style,
        body_style = body_style,
        freeze_header = freeze_header,
        pad_rows = pad_rows,
        pad_cols = pad_cols,
        freeze_keys = freeze_keys,
        key_cols_list = key_cols_list,
        drop_empty = drop_empty
      ),
      dir = dir,
      file_name = file_name,
      overwrite = overwrite
    )
  }
}
list_to_csv <- function(list,
                        dir,
                        file_name = NULL,
                        overwrite = TRUE,
                        drop_empty = TRUE) {
  list <- process_df_list(list, drop_empty = drop_empty)
  for (i in seq_along(list)) {
    sub_list <- list[i]
    file_name2 <- names(sub_list)
    if (!is.null(file_name)) {
      file_name2 <- paste0(file_name, "_", file_name2)
    }
    save_csv(
      form = sub_list[[1]],
      dir = dir,
      file_name = file_name2,
      overwrite = overwrite
    )
  }
}
save_wb <- function(wb, dir, file_name, overwrite = TRUE) {
  if (!dir.exists(dir)) stop("dir doesn't exist")
  path <- file.path(dir, paste0(file_name, ".xlsx")) %>% sanitize_path()
  openxlsx::saveWorkbook(
    wb = wb,
    file = path,
    overwrite = overwrite
  )
  bullet_in_console(paste0("Saved '", basename(path), "'!"), file = path)
}
save_csv <- function(form, dir, file_name, overwrite = TRUE) {
  if (!dir.exists(dir)) stop("dir doesn't exist")
  path <- file.path(dir, paste0(file_name, ".csv")) %>% sanitize_path()
  write_it <- TRUE
  if (!overwrite) {
    if (file.exists(path)) {
      write_it <- FALSE
      bullet_in_console(paste0("Already a file!"), file = path)
    }
  }
  if (write_it) {
    utils::write.csv(
      x = form,
      file = path
    )
    bullet_in_console(paste0("Saved '", basename(path), "'!"), file = path)
  }
}
default_header_style <-
  openxlsx::createStyle(
    fgFill = "#74DFFF",
    halign = "center",
    valign = "center",
    textDecoration = "Bold",
    fontSize = 14,
    fontColour = "black",
    border = "TopBottomLeftRight",
  )
default_body_style <-
  openxlsx::createStyle(
    halign = "left",
    valign = "center",
    fontSize = 12
  )
dw <- function(x) {
  which(duplicated(x))
}
is_consecutive_srt_1 <- function(vec) {
  if (vec[1] != 1) {
    return(FALSE)
  }
  if (length(vec) > 1) {
    for (i in 2:length(vec)) {
      if (vec[i] != vec[i - 1] + 1) {
        return(FALSE)
      }
    }
  }
  TRUE
}
remove_html_tags <- function(text_vector) {
  # Regular expression to match HTML tags
  html_pattern <- "<[^>]+>"
  # Use gsub to remove the HTML tags from each element in the vector
  cleaned_vector <- gsub(html_pattern, "", text_vector)
  cleaned_vector
}
matches <- function(x, ref, count_only = FALSE) {
  final_match <- list()
  final_match[seq_along(x)] <- NA
  next_match <- match(x, ref)
  next_match_index <- which(!is.na(next_match))
  while (length(next_match_index) > 0) {
    final_match[next_match_index] <-
      next_match_index %>% lapply(function(index) {
        out <- NULL
        if (all(is.na(final_match[[index]]))) {
          out <- next_match[index]
        } else {
          out <- c(final_match[[index]], next_match[index])
        }
        out
      })
    ref[next_match[which(!is.na(next_match))]] <- NA
    next_match <- match(x, ref)
    next_match_index <- which(!is.na(next_match))
  }
  if (count_only) {
    final_match <- final_match %>%
      lapply(function(x) {
        if (is.na(x[1])) {
          return(NA)
        }
        length(x)
      }) %>%
      unlist()
  }
  final_match
}
choice_vector_string <- function(vec) {
  if (!is_something(vec)) {
    return(NA)
  }
  paste0(paste0(seq_along(vec), ", ", vec), collapse = " | ")
}
function_to_string <- function(func) {
  # Deparse the function and collapse into a single string using "\n"
  deparse(func) %>% paste(collapse = "\n")
}
clean_function <- function(func) {
  if (!is.function(func)) {
    stop("Input must be a function")
  }
  environment(func) <- emptyenv()
  func
}
object_size <- function(x) {
  format(utils::object.size(x), units = "auto")
}
file_size <- function(path) {
  format(structure(file.size(path), class = "object_size"), units = "auto")
}
drop_if <- function(x, drops) {
  x[which(!x %in% drops)]
}
sample1 <- function(x) {
  sample(x, 1)
}
list_files_real <- function(path, full_names = TRUE, recursive = FALSE) {
  grep(
    "~$",
    sanitize_path(list.files(
      path,
      full.names = full_names, recursive = recursive
    )),
    fixed = TRUE,
    value = TRUE,
    invert = TRUE
  )
}
wrap_text <- function(text, max_length = 40, spacer = "\n") {
  words <- unlist(strsplit(text, " "))
  current_line <- ""
  result <- ""
  for (word in words) {
    if (nchar(current_line) + nchar(word) + 1 > max_length) {
      result <- paste0(result, current_line, spacer)
      current_line <- word
    } else {
      if (nchar(current_line) == 0) {
        current_line <- word
      } else {
        current_line <- paste0(current_line, " ", word)
      }
    }
  }
  result <- paste0(result, current_line)
  return(result)
}
clean_env_names <- function(env_names, silent = FALSE, lowercase = TRUE) {
  cleaned_names <- character(length(env_names))
  for (i in seq_along(env_names)) {
    name <- env_names[i]
    is_valid <- is_env_name(name, silent = TRUE)
    if (is_valid) cleaned_names[i] <- name
    if (!is_valid) {
      if (!silent) message("Invalid environment name: '", name)
      cleaned_name <- gsub("__", "_", gsub(" ", "_", gsub("-", "", name)))
      if (lowercase) cleaned_name <- tolower(cleaned_name)
      if (cleaned_name %in% cleaned_names) {
        if (!silent) {
          message("Non-unique environment name: '", name, "', added numbers...")
        }
        cleaned_name <- cleaned_name %>%
          paste0("_", max(wl(cleaned_name %in% cleaned_names)) + 1)
      }
      cleaned_names[i] <- cleaned_name
    }
  }
  return(cleaned_names)
}
is_df_list <- function(x, strict = FALSE) {
  if (!is.list(x)) {
    return(FALSE)
  }
  if (length(x) == 0) {
    return(FALSE)
  }
  if (is_nested_list(x)) {
    return(FALSE)
  }
  out <- unlist(lapply(x, is.data.frame))
  if (strict) {
    return(all(out))
  }
  return(any(out))
}
check_match <- function(vec_list) {
  sorted_vecs <- lapply(vec_list, sort)
  all(
    unlist(
      lapply(sorted_vecs[-1], function(x) {
        identical(sorted_vecs[[1]], x)
      })
    )
  )
}
is_env_name <- function(env_name, silent = FALSE) {
  result <- tryCatch(
    {
      if (is.null(env_name)) stop("env_name is NULL")
      if (nchar(env_name) == 0) {
        stop("Short name cannot be empty.")
      }
      if (grepl("^\\d", env_name)) {
        stop("Short name cannot start with a number.")
      }
      if (grepl("[^A-Za-z0-9_]", env_name)) {
        stop("Short name can only contain letters, numbers, and underscores.")
      }
      return(TRUE)
    },
    error = function(e) {
      if (!silent) {
        message(e$message)
      }
      FALSE
    }
  )
  result
}
is_nested_list <- function(x) {
  if (!is.list(x)) {
    return(FALSE)
  }
  if (is.data.frame(x)) {
    return(FALSE)
  }
  outcome <- length(x) == 0
  for (i in seq_along(x)) {
    outcome <- outcome || is_nested_list(x[[i]])
  }
  outcome
}
clean_num <- function(num) {
  formatC(num, format = "d", big.mark = ",")
}
is_date <- function(date) {
  out <- grepl("^\\d{4}-\\d{2}-\\d{2}$|^\\d{4}-\\d{2}$|^\\d{4}$", date)
  if (out) {
    out2 <- date %>%
      strsplit(split = "-") %>%
      unlist()
    year <- out2[[1]]
    check_date <- year
    if (length(out2) == 1) {
      check_date <- check_date %>% paste0("-01")
      out2[[2]] <- "01"
    }
    if (length(out2) == 2) {
      check_date <- check_date %>% paste0("-01")
      out2[[3]] <- "01"
    }
    year <- year %>% as.integer()
    month <- out2[[2]] %>% as.integer()
    day <- out2[[3]] %>% as.integer()
    out <- month >= 1 &&
      month <= 12 &&
      day >= 1 &&
      day <= 31 && year >= 1900 && year <= lubridate::year(Sys.Date())
  }
  out
}
is_date_full <- function(date) {
  grepl("^\\d{4}-\\d{2}-\\d{2}$", date)
}
date_imputation <- function(dates_in, date_imputation) {
  # followup add min max
  z <- lapply(dates_in, is_date) %>% as.logical()
  x <- which(z & !is_date_full(dates_in))
  y <- which(!z)
  date_out <- dates_in
  if (length(y) > 0) {
    date_out[y] <- NA
  }
  if (length(x) > 0) {
    if (missing(date_imputation)) date_imputation <- NULL
    if (is.null(date_imputation)) {
      date_out[x] <- NA
    }
    if (!is.null(date_imputation)) {
      date_out[x] <- dates_in[x] %>%
        lapply(function(date) {
          admiral::impute_dtc_dt(
            date,
            highest_imputation = "M",
            date_imputation = date_imputation
          )
        }) %>%
        unlist()
    }
  }
  date_out
}
