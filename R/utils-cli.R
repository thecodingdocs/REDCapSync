#' @noRd
cli_message_maker <- function(collected, function_name, info, internal = TRUE) {
  assert_collection(collected)
  assert_character(function_name, any.missing = FALSE, len = 1L)
  assert_logical(internal, any.missing = FALSE, len = 1L)
  if (internal) {
    message_text <-
      c(i = "This is an internal function. Something is very wrong!")
  } else {
    pkg_separator <- ifelse(internal, ":::", "::")
    pkg_ref <- paste0("{.fun REDCapSync", pkg_separator, function_name, "}")
    message_text <- c(i = paste0("See ", pkg_ref, " or github page for help."))
  }
  if (!missing(info)) {
    assert_character(info, min.len = 1L)
    names(info) <- rep_len("i", length.out = length(info))
    message_text <- append(message_text, info)
  }
  mistakes <- gsub("\\}", "}}", gsub("\\{", "{{", collected$getMessages()))
  mistakes <- collected$getMessages()
  names(mistakes) <- rep_len(">", length.out = length(mistakes))
  message_text <- append(message_text, mistakes)
  message_text
}
