#' @noRd
cli_message_maker <- function(collected, function_name, info, internal = TRUE) {
  assert_collection(collected)
  assert_character(function_name, any.missing = FALSE, len = 1)
  assert_logical(internal, any.missing = FALSE, len = 1)
  if (internal) {
    message <- c(i = "This is an internal function. Something is very wrong!")
  } else {
    pkg_separator <- ifelse(internal, ":::", "::")
    pkg_ref <- paste0("{.fun REDCapSync", pkg_separator, function_name, "}")
    message <- c(i = paste0("See ", pkg_ref, " or github page for help."))
  }
  if (!missing(info)) {
    assert_character(info, min.len = 1)
    names(info) <- rep_len("i", length.out = length(info))
    message <- append(message, info)
  }
  mistakes <- gsub("\\}", "}}", gsub("\\{", "{{", collected$getMessages()))
  mistakes <- collected$getMessages()
  names(mistakes) <- rep_len(">", length.out = length(mistakes))
  message <- append(message, mistakes)
  return(message)
}
#' @noRd
generate_hex <- function(length = 32) {
  toupper(
    paste0(sample(c(0:9, letters[1:6]), length, replace = TRUE), collapse = "")
  )
}
