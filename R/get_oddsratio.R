#' Get odds ratio statistics
#'
#' Given a gene, wraps `epitools::oddsratio` to return the odds ratio
#' confidence interval, the Fisher exact result, the Chi-squared results,
#' and the p-value estimation.
#'
#' @param x the predictor vector, binary
#' @param y the outcome vector, binary
#'
#' @return Single row data frame that can be used with [msg_catch_lapply()].
#'
#' @export
get_oddsratio <- function(x, y) {
  or_list <- oddsratio(x, y)
  data.frame(
    or = or_list$measure[2, "estimate"],
    or_lower = or_list$measure[2, "lower"],
    or_upper = or_list$measure[2, "upper"],
    fisher = or_list$p.value[2, "fisher.exact"],
    chisq = or_list$p.value[2, "chi.square"],
    midp = or_list$p.value[2, "midp.exact"]
  )
}
