#' Helper to label vector elements
#'
#' Helper for [msg_catch_lapply()]. Labels are
#' @param x a vector
#'
#' @return `x` with names that are the elements of `x`
make_temp_labels <- function(x) {
  if(!is.vector(x)) {
  	message("Temporary labels only work with vectors.")
  	NULL
  }
	names(x) <- x
	x
}
