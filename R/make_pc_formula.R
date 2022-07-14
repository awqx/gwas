#' Make a formula with a specified number of principal components
#'
#' This function is a helper for [get_logreg()].
#'
#' @param x the name of the predictor
#' @param y the name of the response
#' @param n the number of principal components.
make_pc_formula <- function(x, y = "culture_2m", n = 0) {
  if (n == 0) {
    temp <- as.formula(paste0(y, " ~ ", x))
    return(temp)
  }
  as.formula(
    paste0(
      y, " ~ ",
      paste0(
        x, " + ",
        paste(paste0("pc", 1:n), collapse = " + "))
    )
  )
}
