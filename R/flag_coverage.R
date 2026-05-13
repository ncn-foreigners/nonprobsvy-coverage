#' Classify empirical coverage as green / amber / red
#'
#' Uses Monte Carlo SE of the coverage estimate as the yardstick:
#' - green: |empirical - nominal| <= mcse
#' - amber: within 2 * mcse
#' - red:   beyond 2 * mcse
#'
#' Vectorised over `coverage`, `nominal`, and `mcse`.
#'
#' @param coverage Empirical coverage (proportion in \[0, 1\]).
#' @param nominal Nominal level (e.g. 0.95).
#' @param mcse Monte Carlo SE of the coverage estimate (sqrt(p(1-p)/n_reps)).
#' @return Character vector in c("green", "amber", "red").
#' @export
flag_coverage <- function(coverage, nominal, mcse) {
  diff <- abs(coverage - nominal)
  out  <- rep("red", length(coverage))
  out[diff <= 2 * mcse] <- "amber"
  out[diff <=     mcse] <- "green"
  out[is.na(coverage)]  <- NA_character_
  out
}
