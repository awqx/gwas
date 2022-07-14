#' Helper functions for message catching lists
#'
#' After a [msg_catch()] product function has been used with `lapply()`,
#' the results, warning, and errors can be resorted into separate lists.
#'
#' @param x the output from `lapply()` with [msg_catch()]. A list of lists of the form `list(res, warn = w, err = e)`
#'
#' @export
msg_catch_relist <- function(x) {
	if(sum(is.null(names(x))) > 0) {
	  message("ERROR - Not all elements named.")
	  NULL
	}

	res_list  <- lapply(x, function(z) z[[1]])
  warn_list <- relist_helper(x, "warn", "WARNING")
	err_list  <- relist_helper(x, "err", "ERROR")

	list(res = res_list, warn = warn_list, err = err_list)
}
