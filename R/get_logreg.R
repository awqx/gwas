#' Calculate results from logistic regression
#'
#' This function returns the slope, standard deviation, and p-value
#' from logistic regression calculated using `glm()`.
#'
#' Logistic regression function: `glm(y ~ x, family = "binomial", data = df)`
#'
#' The formula for logistic regression uses
#'
#' @param x binary predictor data, e.g., for gene mutation or SNP
#' @param y binary response data
#' @param df `data.frame` containing `x` and `y`
#'
#' @return a single row `data.frame` containing slope, the standard dev., and p-value.
#' Suitable for usage with [msg_catch_lapply()].
#'
#' @export
get_lr_stats <- function(x, y, df, n = 0) {
  fit <- glm(
    make_pc_formula(y = y, x = x, n = n),
    family = "binomial",
    data = df,
  )
  temp <- data.frame(
    slope = coef(fit)[x],
    sd = coef(summary(fit))[x, "Std. Error"],
    p = coef(summary(fit))[x, "Pr(>|z|)"]
  )
  return(temp)
}
