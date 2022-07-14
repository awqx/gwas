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
