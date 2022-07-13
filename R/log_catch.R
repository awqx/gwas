#' Factory for logged tryCatch
#'
#' Converts a function into a tryCatch version that logs the result.
#' Taken from Martin Morgan's answer to https://stackoverflow.com/questions/4948361/.
#' `log_catch_lapply` is identical except the tryCatch also applies a `lapply`.
#'
#' @param fun function to convert
#' @param ... additional arguments to pass to the function
#'
#' @export
log_catch <- function(fun)
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
  	list(res, warn = warn, err = err)
  }

#' @describeIn log_catch An application of `log_catch` to lapply.
log_catch_lapply <- function(fun, X)
	function(...) {
		warn_list <- err_list <- res_list <- NULL
    dummy <- lapply(
    	X = X,
    	FUN = log_catch(fun),
    	...
    )
    lapply(
    	dummy,
    	function(x) {
    		append(warn_list, x$warm)
    		append(err_list, x$err)
    		append(res_list, x[[1]])
    	}
    )
    list(res_list, warn = warn_list, err = err_list)
	}
