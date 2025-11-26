#' @noRd
clean_for_cli <- function(path) {
  gsub("'", "\\\\'", path)
}
#' @noRd
cli_alert_wrap <- function(text = "",
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
    # url %>% lapply(function(x){assert_web_link(x)})
    # doesnt work for /subheaders/
    # url_if <- " {.url {url}}"
    url_names <- names(url)
    if (is.list(url)) {
      url_names <- unlist(url)
      if (is_named_list(url)) {
        url_names <- names(url)
      }
      url <- unlist(url)
    }
    if (is.null(url_names))
      url_names <- url
    if (collapse)
      url_if <- paste0(url_if, collapse = " and ")
    url_if <- paste0(
      " {cli::col_blue(cli::style_hyperlink('",
      url_names %>% clean_for_cli(),
      "', '",
      url %>% clean_for_cli(),
      "'))}"
    )
  }
  if (length(file) > 0) {
    file_names <- names(file)
    if (is.list(file)) {
      file_names <- unlist(file)
      if (is_named_list(file))
        file_names <- names(file)
      file <- unlist(file)
    }
    if (is.null(file_names))
      file_names <- file
    if (collapse)
      file_if <- paste0(file_if, collapse = " and ")
    file_if <- paste0(
      " {cli::col_blue(cli::style_hyperlink('",
      sanitize_path(file_names) %>% clean_for_cli(),
      "', '",
      sanitize_path(paste0("file://", file)) %>% clean_for_cli(),
      "'))}"
    )
  }
  for (i in seq_along(url_if))
    text[i] <- paste0(text[i], url_if[i])
  for (i in seq_along(file_if))
    text[i] <- paste0(text[i], file_if[i])
  names(text)[seq_along(text)] <- bullet_type
  cli::cli_bullets(text)
}
#' @noRd
now_time <- function() {
  as.POSIXct(Sys.time(), tz = Sys.timezone())
}
#' @noRd
process_df_list <- function(list,
                            drop_empty = TRUE,
                            silent = FALSE) {
  if (is_something(list)) {
    if (!is_df_list(list))
      stop("list must be ...... a list :)")
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
          cli_alert_wrap("Dropping non-data.frames and empties... ",
                         toString(names(drops)))
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
#' @noRd
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
#' @noRd
sanitize_path <- function(path) {
  sanitized <- gsub("\\\\", "/", path)
  sanitized <- normalizePath(sanitized, winslash = "/", mustWork = FALSE)
  sanitized
}
#' @noRd
all_character_cols <- function(form) {
  as.data.frame(lapply(form, as.character))
}
#' @noRd
all_character_cols_list <- function(list) {
  lapply(list, all_character_cols)
}
#' @noRd
vec1_in_vec2 <- function(vec1, vec2) {
  vec1[which(vec1 %in% vec2)]
}
#' @noRd
vec1_not_in_vec2 <- function(vec1, vec2) {
  vec1[which(!vec1 %in% vec2)]
}
#' @noRd
length_unique <- function(x) {
  length(unique(x))
}
#' @noRd
length_which <- function(x) {
  length(which(x))
}
#' @noRd
drop_nas <- function(x) {
  if (length(x) == 0) {
    return(x)
  }
  x[!unlist(lapply(x, is.na))]
}
#' @noRd
excel_to_list <- function(path) {
  sheets <- readxl::excel_sheets(path)
  names(sheets) <- seq_along(sheets)
  clean_sheets <- clean_env_names(sheets)
  out <- list()
  if("summary_details" %in% sheets){
    summary_details <- readxl::read_xlsx(
      path,
      col_types = "text",
      sheet = which(sheets == "summary_details")
    )
    if (all(c("paramater", "value") %in% colnames(summary_details))) {
      the_row <- which(summary_details$paramater == "raw_form_names")
      form_names <- summary_details$value[the_row] %>%
        strsplit(" [:|:] ") %>%
        unlist()
      the_row <- which(summary_details$paramater == "cols_start")
      cols_start <- summary_details$value[the_row] %>% as.integer()
      if(cols_start > 1){
        for(i in as.integer(names(sheets)[match(form_names,sheets)])){
          suppressMessages({
            out[[i]] <- readxl::read_xlsx(path,
                                          col_types = "text",
                                          sheet = i,
                                          col_names = FALSE)
          })
          final_nrow <- nrow(out[[i]])
          if(cols_start < final_nrow){
            true_colnames <- out[[i]][cols_start,] %>% unlist() %>% unname()
            out[[i]] <- out[[i]][(cols_start+1):final_nrow,]
            colnames(out[[i]])<- true_colnames
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
is_named_df_list <- function(x, strict = FALSE) {
  is_named_list(x) && is_df_list(x, strict = strict)
}
#' @noRd
is_named_list <- function(x,
                          silent = TRUE,
                          recursive = FALSE) {
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
        if (!silent && !named_all)
          message("'", n, "' is not named")
      }
    }
  }
  named_all
}
#' @noRd
wb_to_list <- function(wb) {
  # consider test for if user does not have excel
  sheets <- openxlsx::sheets(wb)
  clean_sheets <- clean_env_names(sheets)
  out <- list()
  for (i in seq_along(sheets)) {
    col_row <- 1
    x <- openxlsx::getTables(wb, sheet = i)
    if (length(x) > 0) {
      # test for xlsx without letters for cols
      col_row <- gsub("[A-Za-z]", "", unlist(x %>% attr("refs") %>% strsplit(":"))[[1]]) %>%
        as.integer()
    }
    out[[i]] <- openxlsx::read.xlsx(wb, sheet = i, startRow = col_row)
  }
  names(out) <- clean_sheets
  out
}
#' @noRd
form_to_wb <- function(form,
                       form_name,
                       wb = openxlsx::createWorkbook(),
                       key_cols = NULL,
                       derived_cols = NULL,
                       link_col_list = list(),
                       str_trunc_length = 32000,
                       header_df = NULL,
                       header_style = default_header_style,
                       body_style = default_body_style,
                       freeze_header = TRUE,
                       pad_rows = 0,
                       pad_cols = 0,
                       freeze_keys = TRUE) {
  if (nchar(form_name) > 31){
    stop(form_name, " is longer than 31 char")
  }
  form[] <- lapply(form, function(col) {
    out <- col
    if (is.character(col)) {
      out <- stringr::str_trunc(col, str_trunc_length, ellipsis = "")
    }
    out
  })
  hyperlink_col <- NULL
  if (is_something(header_df)) {
    names(header_df)[match(names(form),names(header_df))]
    missing_headers <- names(form) %>%  vec1_not_in_vec2(names(header_df))
    if(length(missing_headers)>0){
      for (missing_header in missing_headers){
        header_df[[missing_header]] <- ""
        #fix later
      }
    }
  }
  if (freeze_keys) {
    all_cols <- colnames(form)
    if (!all(key_cols %in% all_cols)){
      stop("all key_cols must be in the forms")
    }
    freeze_key_cols <- which(all_cols %in% key_cols)
    if (length(freeze_key_cols) > 0) {
      if (!is_consecutive_srt_1(freeze_key_cols)) {
        # warning(
        #   "please keep your key cols on the left consecutively. Fixing ",
        #   form_name,
        #   ": ",
        #   toString(key_cols),
        #   ".",
        #   immediate. = TRUE
        # ) # instead will fix add on problem
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
          firstActiveCol <- firstActiveCol +
            freeze_key_cols[length(freeze_key_cols)]
        } else {
          warning("key_cols must be consecutive and start from left most column.",
                  immediate. = TRUE)
        }
      }
      openxlsx::freezePane(wb,
                           form_name,
                           firstActiveRow = firstActiveRow,
                           firstActiveCol = firstActiveCol)
    }
  }
  return(wb)
}
#' @noRd
list_to_wb <- function(input_list,
                       key_cols_list = list(),
                       derived_cols_list = list(),
                       link_col_list = list(),
                       str_trunc_length = 32000,
                       header_df_list = NULL,
                       header_style = default_header_style,
                       body_style = default_body_style,
                       freeze_header = TRUE,
                       pad_rows = 0,
                       pad_cols = 0,
                       freeze_keys = TRUE,
                       drop_empty = TRUE) {
  if(is.null(key_cols_list)) key_cols_list <- list()
  if(is.null(link_col_list)) link_col_list <- list()
  if(is.null(derived_cols_list)) derived_cols_list <- list()
  wb <- openxlsx::createWorkbook()
  input_list <- process_df_list(input_list, drop_empty = drop_empty)
  list_names <- names(input_list)
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
rename_list_names_excel <- function(list_names) {
  list_names_rename <- stringr::str_trunc(list_names,
                                          width = 31,
                                          side = "right",
                                          ellipsis = "")
  bad_names <- which_duplicated(list_names_rename)
  if (length(bad_names) > 0) {
    cli_alert_danger("Duplicated names when trimmed from right 31 max in Excel: ",
                     toString(list_names[bad_names]))
    cli_alert_info(
      paste0(
        "Use CSV or shorten the names and make sure they are unique if they",
        " are trimmed to 31 char. For now will make unique by adding number."
      )
    )
    list_names_rename <- unique_trimmed_strings(list_names_rename, max_length = 31)
  }
  list_names_rename
}
#' @noRd
trim_string <- function(string, max_length) {
  substr(string, 1, max_length)
}
#' @noRd
unique_trimmed_strings <- function(strings, max_length) {
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
          width = max_length - stringr::str_length(counter),
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
#' @noRd
list_to_excel <- function(input_list,
                          dir,
                          file_name = NULL,
                          separate = FALSE,
                          overwrite = TRUE,
                          key_cols_list = list(),
                          derived_cols_list = list(),
                          link_col_list = list(),
                          str_trunc_length = 32000,
                          header_df_list = NULL,
                          header_style = default_header_style,
                          body_style = default_body_style,
                          freeze_header = TRUE,
                          pad_rows = 0,
                          pad_cols = 0,
                          freeze_keys = TRUE,
                          drop_empty = TRUE) {
  if(is.null(key_cols_list)) key_cols_list <- list()
  if(is.null(link_col_list)) link_col_list <- list()
  if(is.null(derived_cols_list)) derived_cols_list <- list()
  input_list <- process_df_list(input_list, drop_empty = drop_empty)
  list_names <- names(input_list)
  if (length(input_list) == 0) {
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
      keep_links <- which(names(link_col_list) == list_names[i])
      key_cols_list_i <- key_cols_list[keep_keys]
      derived_cols_list_i <- derived_cols_list[keep_deriveds]
      # link_col_list_i <- link_col_list[keep_links]
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
      form = sub_list[[1]],
      dir = dir,
      file_name = file_name2,
      overwrite = overwrite
    )
  }
}
#' @noRd
save_wb <- function(wb, dir, file_name, overwrite = TRUE) {
  if (!dir.exists(dir))
    stop("dir doesn't exist")
  path <- file.path(dir, paste0(file_name, ".xlsx")) %>% sanitize_path()
  openxlsx::saveWorkbook(wb = wb,
                         file = path,
                         overwrite = overwrite)
  cli_alert_wrap(paste0("Saved '", basename(path), "'!"), file = path)
}
#' @noRd
save_csv <- function(form, dir, file_name, overwrite = TRUE) {
  if (!dir.exists(dir))
    stop("dir doesn't exist")
  path <- file.path(dir, paste0(file_name, ".csv")) %>% sanitize_path()
  write_it <- TRUE
  if (!overwrite) {
    if (file.exists(path)) {
      write_it <- FALSE
      cli_alert_wrap(paste0("Already a file!"), file = path)
    }
  }
  if (write_it) {
    utils::write.csv(x = form, file = path,row.names = FALSE)
    cli_alert_wrap(paste0("Saved '", basename(path), "'!"), file = path)
  }
}
#' @noRd
default_header_style <-
  openxlsx::createStyle(
    fgFill = "#74DFFF",
    halign = "center",
    valign = "center",
    textDecoration = "Bold",
    fontSize = 14,
    fontColour = "black",
    border = "TopBottomLeftRight"
  )
#' @noRd
default_body_style <-
  openxlsx::createStyle(halign = "left",
                        valign = "center",
                        fontSize = 12)
#' @noRd
which_duplicated <- function(x) {
  which(duplicated(x))
}
#' @noRd
is_consecutive_srt_1 <- function(vec) {
  if (vec[1] != 1L) {
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
#' @noRd
remove_html_tags <- function(text_vector) {
  html_pattern <- "<[^>]+>"
  cleaned_vector <- gsub(html_pattern, "", text_vector)
  cleaned_vector
}
#' @noRd
object_size <- function(x) {
  format(utils::object.size(x), units = "auto")
}
#' @noRd
file_size <- function(path) {
  format(structure(file.size(path), class = "object_size"), units = "auto")
}
#' @noRd
drop_if <- function(x, drops) {
  x[which(!x %in% drops)]
}
#' @noRd
clean_env_names <- function(env_names,
                            silent = FALSE,
                            lowercase = TRUE) {
  cleaned_names <- character(length(env_names))
  for (i in seq_along(env_names)) {
    name <- env_names[i]
    is_valid <- is_env_name(name, silent = TRUE)
    if (is_valid) {
      cleaned_name <- name
    }
    if (!is_valid) {
      if (!silent){
        message("Invalid environment name: '", name)
      }
      cleaned_name <- gsub("__", "_", gsub(" ", "_", gsub("-", "", name)))
    }
    if (lowercase){
      cleaned_name <- tolower(cleaned_name)
    }
    if (cleaned_name %in% cleaned_names) {
      if (!silent) {
        message("Non-unique environment name: '",
                name,
                "', added numbers...")
      }
      cleaned_name <- cleaned_name %>%
        paste0("_", max(length_which(cleaned_name %in% cleaned_names)) + 1L)
    }
    cleaned_names[i] <- cleaned_name
  }
  cleaned_names
}
#' @noRd
is_df_list <- function(x, strict = FALSE) {
  if (!is.list(x)) {
    return(FALSE)
  }
  if (length(x) == 0L) {
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
#' @noRd
check_match <- function(vec_list) {
  sorted_vecs <- lapply(vec_list, sort)
  all(unlist(lapply(sorted_vecs[-1], function(x) {
    identical(sorted_vecs[[1]], x)
  })))
}
#' @noRd
is_env_name <- function(env_name, silent = FALSE) {
  result <- tryCatch({
    if (is.null(env_name)) {
      stop("env_name is NULL")
    }
    if (nchar(env_name) == 0L) {
      stop("Short name cannot be empty.")
    }
    if (grepl("^\\d", env_name)) {
      stop("Short name cannot start with a number.")
    }
    if (grepl("[^A-Za-z0-9_]", env_name)) {
      stop("Short name can only contain letters, numbers, and underscores.")
    }
    return(TRUE)
  }, error = function(e) {
    if (!silent) {
      message(e$message)
    }
    FALSE
  })
  result
}
#' @noRd
is_nested_list <- function(x) {
  if (!is.list(x)) {
    return(FALSE)
  }
  if (is.data.frame(x)) {
    return(FALSE)
  }
  outcome <- length(x) == 0L
  for (i in seq_along(x)) {
    outcome <- outcome || is_nested_list(x[[i]])
  }
  outcome
}
#' @noRd
generate_hex <- function(length = 32) {
  c(0:9, letters[1:6]) %>%
    sample(length, replace = TRUE) %>%
    paste0(collapse = "") %>%
    toupper()
}
