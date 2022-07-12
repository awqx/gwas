#' Get odds ratio statistics
#'
#' Given a gene, wraps `epitools::oddsratio` to return the odds ratio
#' confidence interval, the Fisher exact result, the Chi-squared results,
#' and the p-value estimation.
#'
#' @param x the predictor vector, binary
#' @param y the outcome vector, binary
#'
#' @return Single row data frame (used for row binding)
#'
#' @export
or_stats <- function(x, y) {
  result <- tryCatch({
    or_list <- oddsratio(x, y)
    df <- data.frame(
      or = or_list$measure[2, "estimate"],
      or_lower = or_list$measure[2, "lower"],
      or_upper = or_list$measure[2, "upper"],
      fisher = or_list$p.value[2, "fisher.exact"],
      chisq = or_list$p.value[2, "chi.square"],
      midp = or_list$p.value[2, "midp.exact"]
    )
    return(df)
  },
  error = function(err) {
    message("Error reached. Gene skipped.")
    return(data.frame(
      or = NA,
      or_lower = NA,
      or_upper = NA,
      fisher = NA,
      chisq = NA,
      midp = NA
      )
    )
  })
  result
}
