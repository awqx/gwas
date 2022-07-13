#' Factory for tryCatch with message retention
#'
#' Converts a function into a tryCatch version that records status messages.
#' Taken from Martin Morgan's answer to https://stackoverflow.com/questions/4948361/.
#' `msg_catch_lapply` is identical except the tryCatch also applies a `lapply`.
#'
#' @param fun function to convert
#' @param ... additional arguments to pass to the function
#'
#' @export
msg_catch <- function(fun)
  function(...) {
  	warn <- err <- NULL
  	res <- withCallingHandlers(
  		tryCatch(
  			fun(...),
  			error = function(e) {
  				err <<- conditionMessage(e)
  				NULL
  			}),
  		warning = function(w) {
  			warn <<- append(warn, conditionMessage(w))
  			invokeRestart("muffleWarning")
  		}
  	)
  	list(res = res, warn = warn, err = err)
  }
