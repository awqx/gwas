#' Message catching over a list
#'
#' Integration of [msg_catch()] and `lapply()`. Wraps together a `lapply()` that
#' takes a [msg_catch()] function and a [msg_catch_relist()].
#' Optional `logfile` argument can call [write_log()].
#'
#' Make sure functions passed to `fun` have an output that can be passed to `rbind()`,
#' such as a single row a `data.frame`.
#'
#' @param fun the function to be passed to [msg_catch()]
#' @param X the argument to `lapply()` over
#' @param logfile leave `NULL` to not log. Otherwise, a character string/connection to the log file.
#' @param ... additional arguments to be passed to `fun`
#'
#' @return list `fun_list(res, warn, err)` that contains a `data.frame` of results,
#' a character vector of warnings, and a chr vector of errors.
#'
#' @export
msg_catch_lapply <- function(fun, X, logfile = NULL, ...) {
  new_fun <- msg_catch(fun)

  fun_list <- lapply(X, FUN = new_fun, ...) %>%
    msg_catch_relist()

  temp <- names(fun_list$res)
  fun_list$res <- do.call(rbind, fun_list$res) %>%
    mutate(x = temp)

  if(!is.null(logfile)) write_log(fun_list, logfile)
  fun_list
}
