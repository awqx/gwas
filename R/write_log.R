#' Write a log file from caught messages
#'
#' After messages have been caught by a `msg_catch` function, this function will
#' write warnings and errors into a text file.
#'
#' @param x the output from a `msg_catch_relist` call. Requires errors to be a character vector under `.$err` and warnings to be a vector under `.$warn`.
#' @param f character string or file location of the log file.
#'
#' @export
write_log <- function(x, f) {
  txt <- c(
    "--------",
    paste(length(x$err), "ERRORS"),
    x$err,
    "--------",
    paste(length(x$warn), "WARNINGS"),
    x$warn
  )
  writeLines(txt, f)
}
