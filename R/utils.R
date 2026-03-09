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
  if (length(url) > 0L) {
    # assert?
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
    if (collapse) {
      url_if <- paste0(url_if, collapse = " and ")
    }
    url_if <- paste0(
      " {cli::col_blue(cli::style_hyperlink('",
      clean_for_cli(url_names),
      "', '",
      clean_for_cli(url),
      "'))}"
    )
  }
  if (length(file) > 0L) {
    file_names <- names(file)
    if (is.list(file)) {
      file_names <- unlist(file)
      if (is_named_list(file)) {
        file_names <- names(file)
      }
      file <- unlist(file)
    }
    if (is.null(file_names))
      file_names <- file
    if (collapse) {
      file_if <- paste0(file_if, collapse = " and ")
    }
    file_if <- paste0(
      " {cli::col_blue(cli::style_hyperlink('",
      sanitize_path(file_names) |> clean_for_cli(),
      "', '",
      sanitize_path(paste0("file://", file)) |> clean_for_cli(),
      "'))}"
    )
  }
  for (i in seq_along(url_if))
    text[i] <- paste0(text[i], url_if[i])
  for (i in seq_along(file_if))
    text[i] <- paste0(text[i], file_if[i])
  names(text)[seq_along(text)] <- bullet_type
  cli_bullets(text)
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
      is_a_df_with_rows <- list |>
        lapply(function(x) {
          is_df <- is.data.frame(x)
          out <- FALSE
          if (is_df) {
            out <- nrow(x) > 0L
          }
          out
        }) |>
        unlist()
      keeps <- which(is_a_df_with_rows)
      drops <- which(!is_a_df_with_rows)
      if (length(drops) > 0L) {
        if (!silent) {
          cli_alert_wrap("Dropping non-data.frames and empties... ",
                         toString(names(drops)))
        }
      }
      list <- list[keeps]
    }
    if (length(list) > 0L) {
      if (!is_named_df_list(list)) {
        names(list) <- paste0(seq_along(list))
      }
    }
  }
  list
}
#' @noRd
is_something <- function(thing, row_length = 0L) {
  out <- FALSE
  if (is.function(thing)) {
    return(TRUE)
  }
  if (!is.null(thing)) {
    if (is.data.frame(thing)) {
      if (nrow(thing) > row_length) {
        out <- TRUE
      }
    } else {
      if (length(thing) > 0L) {
        if (is.list(thing)) {
          out <- TRUE
        } else {
          if (length(thing) == 1L) {
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
drop_nas <- function(x) {
  if (length(x) == 0L) {
    return(x)
  }
  x[!unlist(lapply(x, is.na))]
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
trim_string <- function(string, max_length) {
  substr(string, 1L, max_length)
}
#' @noRd
unique_trimmed_strings <- function(strings, max_length) {
  trimmed_strings <- lapply(strings, trim_string, max_length = max_length) |>
    unlist()
  # Initialize a vector to store unique strings
  unique_strings <- character(length(trimmed_strings))
  # Initialize a counter to keep track of occurrences
  counts <- integer(length(trimmed_strings))
  for (i in seq_along(trimmed_strings)) {
    base_string <- trimmed_strings[i]
    new_string <- base_string
    counter <- 1L
    # Keep adjusting the string until it's unique
    while (new_string %in% unique_strings) {
      new_string <- paste0(
        str_trunc(
          base_string,
          width = max_length - str_length(counter),
          side = "right",
          ellipsis = ""
        ),
        counter
      )
      counter <- counter + 1L
    }
    unique_strings[i] <- new_string
    counts[i] <- counter
  }
  unique_strings
}
#' @noRd
which_duplicated <- function(x) {
  which(duplicated(x))
}
#' @noRd
is_consecutive_srt_1 <- function(vec) {
  if (vec[1L] != 1L) {
    return(FALSE)
  }
  if (length(vec) > 1L) {
    for (i in 2L:length(vec)) {
      if (vec[i] != vec[i - 1L] + 1L) {
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
  format(object.size(x), units = "auto")
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
      if (!silent) {
        message("Invalid environment name: '", name)
      }
      cleaned_name <- gsub("__", "_", gsub(" ", "_", gsub("-", "", name)))
    }
    if (lowercase) {
      cleaned_name <- tolower(cleaned_name)
    }
    if (cleaned_name %in% cleaned_names) {
      if (!silent) {
        message("Non-unique environment name: '",
                name,
                "', added numbers...")
      }
      cleaned_name <- cleaned_name |>
        paste0("_", max(length(which(cleaned_name %in% cleaned_names))) + 1L)
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
  any(out)
}
#' @noRd
check_match <- function(vec_list) {
  sorted_vecs <- lapply(vec_list, sort)
  all(unlist(lapply(sorted_vecs[-1L], function(x) {
    identical(sorted_vecs[[1L]], x)
  })))
}
#' @noRd
is_env_name <- function(env_name, silent = FALSE) {
  result <- tryCatch({
    if (is.null(env_name)) {
      stop("env_name is NULL")
    }
    if (is.na(env_name)) {
      stop("env_name is NA")
    }
    if (!nzchar(env_name)) {
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
      cli_alert_danger(e$message)
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
add_redcap_links_to_form <- function(form, project) {
  # add instance links
  if (project$metadata$id_col %in% colnames(form)) {
    rows_x <- which(
      project$metadata$raw_structure_cols %in% colnames(form) &
        project$metadata$raw_structure_cols != project$metadata$id_col
    )
    form_structure_cols <- project$metadata$raw_structure_cols[rows_x]
    link_head <- project$links$redcap_record_home
    link_tail <- paste0("&id=", form[[project$metadata$id_col]])
    if ("redcap_repeat_instrument" %in% form_structure_cols) {
      link_head <- project$links$redcap_record_subpage
      link_tail <-
        paste0(link_tail, "&page=", form[["redcap_repeat_instrument"]])
    }
    if ("redcap_repeat_instance" %in% form_structure_cols) {
      link_head <- project$links$redcap_record_subpage
      link_tail <-
        paste0(link_tail, "&instance=", form[["redcap_repeat_instance"]])
    }
    form$redcap_link <- paste0(link_head, link_tail)
    if ("arm_number" %in% colnames(form)) {
      form$redcap_link <-
        paste0(form$redcap_link, "&arm=", form[["arm_number"]])
    }
  }
  form
}
#' @noRd
split_choices <- function(x) {
  result <- x
  # added this to account for redcap metadata output if not a number
  result <- gsub("\n", " | ", result)
  result <- result |>
    strsplit("[|]") |>
    unlist() |>
    str_split_fixed(",", 2L)
  check_length <- length(result[, 1L])
  choices_data <- data.frame(
    code = trimws(result[, 1L]),
    name = trimws(result[, 2L]),
    stringsAsFactors = FALSE
  )
  rownames(choices_data) <- NULL
  if (anyNA(choices_data$code)) {
    stop("split choice error: ", x)
  }
  if (anyNA(choices_data$name)) {
    stop("split choice error: ", x)
  }
  if (nrow(choices_data) != check_length) {
    stop("split choice error: ", x)
  }
  if (any(!nzchar(choices_data$name))) {
    stop("split choice error: ", x)
  }
  choices_data
}
#' @noRd
get_match <- function(x, return_field, match_field, match_text) {
  x[[return_field]][which(x[[match_field]] %in% match_text)]
}
#' @noRd
file_ext_alias <- function(x) {
  pos <- regexpr("\\.([[:alnum:]]+)$", x)
  ifelse(pos > -1L, substring(x, pos + 1L), "")
}
#' @noRd
try_else_null <- function(expr) {
  tryCatch(
    expr = expr,
    error = function(e) {
      NULL
    }
  )
}
