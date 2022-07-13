#' Message catching over a list
#'
#' Integration of [msg_catch()] and `lapply()`. Wraps together a `lapply()` that
#' takes a [msg_catch()] function and a [msg_catch_relist()].
#'
#' Make sure functions passed to `FUN` have an output that can be passed to `rbind()`,
#' such as a single row a `data.frame`.
#'
#' @param fun the function to be passed to [msg_catch()]
#' @param X the argument to `lapply()` over

#' @param ... additional arguments to be passed to `fun`
#'
#' @return list `fun_list(res, warn, err)` that contains a `data.frame` of results,
#' a character vector of warnings, and a chr vector of errors.
#'
#' @export
msg_catch_lapply <- function(X, FUN, ...) {
  fun_list <- lapply(X = X, FUN = msg_catch(FUN), ...) %>%
    msg_catch_relist()
  fun_list$res <- do.call(rbind, fun_list$res)
  fun_list
}
