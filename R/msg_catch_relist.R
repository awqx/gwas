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
	 message("All elements need a name.")
	 NULL
	}

	res_list  <- lapply(x, function(z) z[[1]])
  warn_list <- relist_helper(x, "warn", "WARNING")
	err_list  <- relist_helper(x, "err", "ERROR")

	list(res = res_list, warn = warn_list, err = err_list)
}

#' @describeIn msg_catch_relist
#' Helper for relisting
#'
#' A helper function that remakes a list based on a given subset name. Only used for message (warnings and errors).
#'
#' @param x an output from using `lapply()` and a [msg_catch()] function
#' @param x_subset the name of the subset to make into a new list
#' @param msg_label an optional character string to start each entry
relist_helper <- function(x, x_subset, msg_label = NULL) {
  x_names <- names(x)
  if(is.null(msg_label)) msg_label <- x_subset
  sapply(
    1:length(x),
    function(z) {
      if(!is.null(x[[z]][[x_subset]])) {
        return(paste(msg_label, x_names[z], "--", x[[z]][[x_subset]]))
      } else {
        NULL
      }
    }
  ) %>%
   unlist() %>%
   .[!is.null(.)]
}
