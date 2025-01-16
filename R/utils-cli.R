#' @noRd
cli_message_maker <- function(collected, function_name, info, internal = TRUE) {
  assert_collection(collected)
  assert_character(function_name, any.missing = FALSE, len = 1)
  assert_logical(internal, any.missing = FALSE, len = 1)
  if (internal) {
    message <- c("i" = "This is an internal function. Something is very wrong!")
  } else {
    pkg_separator <- ifelse(internal, ":::", "::")
    pkg_ref <- paste0("{.fun REDCapSync", pkg_separator, function_name, "}")
    message <- c("i" = paste0("See ", pkg_ref, " or github page for help."))
  }
  if (!missing(info)) {
    assert_character(info, min.len = 1)
    names(info) <- rep_len("i", length.out = length(info))
    message <- message %>% append(info)
  }
  mistakes <- collected$getMessages()
  names(mistakes) <- rep_len(">", length.out = length(mistakes))
  message <- message %>% append(mistakes)
  return(message)
}
bullet_in_console <- function(text = "", url = NULL, bullet_type = "i", collapse = TRUE, file = NULL, silent = FALSE) {
  if (silent) {
    return(invisible())
  }
  url_if <- ""
  file_if <- ""
  if (length(url) > 0) {
    # url %>% lapply(function(IN){assert_web_link(IN)}) # doesnt work for /subheaders/
    # url_if <- " {.url {url}}"
    url_names <- names(url)
    if (is.list(url)) {
      url_names <- url %>% unlist()
      if (is_named_list(url)) url_names <- names(url)
      url <- unlist(url)
    }
    if (is.null(url_names)) url_names <- url
    if (collapse) url_if <- paste0(url_if, collapse = " and ")
    url_if <- paste0(" {cli::col_blue(cli::style_hyperlink('", url_names, "', '", url, "'))}")
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
  return(cli::cli_bullets(text))
  # * = • = bullet
  # > = → = arrow
  # v = ✔ = success
  # x = ✖ = danger
  # ! = ! = warning
  # i = ℹ = info
}
generate_hex <- function(length = 32) {
  toupper(paste0(sample(c(0:9, letters[1:6]), length, replace = TRUE), collapse = ""))
}
#' @noRd
is_exported <- function(func_name) {
  # # Check if the package namespace is loaded
  #
  # # Get the namespace environment for the package
  # ns_env <- asNamespace("REDCapSync")
  # "setup_project" %in% getNamespaceExports(ns_env)
  # RosyDev::get_external_functions()
  # getNamespaceImports()
}
