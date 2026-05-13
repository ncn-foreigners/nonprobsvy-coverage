#' Default coverage-spec DGP
#'
#' A small, deliberately simple data-generating process used for the spike
#' and for sanity-checking the harness. Three independent standard-normal
#' covariates, linear outcome, logistic selection biased by x1 and x2.
#' The probability sample is a simple random sample with equal weights.
#'
#' True target: population mean of y, equal to the intercept (1) in
#' expectation (covariates are zero-mean).
#'
#' @param n_pop Population size.
#' @param n_nonprob Expected non-probability sample size. The selection
#'   intercept is calibrated so E\[sum(R_i)\] = n_nonprob.
#' @param n_prob Probability sample size (SRS).
#' @param ... Ignored. Accepts extra arguments from generic callers so the
#'   dispatcher can pass the full parameter cell without erroring.
#' @return A list with `pop` (data.table), `nonprob_idx`, `prob_idx`,
#'   `prob_design` (svydesign), `true_target`.
#' @export
dgp_default <- function(n_pop = 10000, n_nonprob = 500, n_prob = 500, ...) {
  N <- as.integer(n_pop)
  x1 <- stats::rnorm(N)
  x2 <- stats::rnorm(N)
  x3 <- stats::rnorm(N)
  y  <- 1 + x1 + 0.5 * x2 - 0.3 * x3 + stats::rnorm(N)

  # selection: logit linear in x1, x2; intercept chosen so E[sum R] = n_nonprob
  lin_no_intercept <- 0.7 * x1 - 0.4 * x2
  theta0 <- stats::uniroot(
    function(a) sum(stats::plogis(a + lin_no_intercept)) - n_nonprob,
    interval = c(-15, 5)
  )$root
  p_sel <- stats::plogis(theta0 + lin_no_intercept)
  R <- stats::rbinom(N, 1, p_sel)
  nonprob_idx <- which(R == 1L)

  prob_idx <- sample.int(N, size = as.integer(n_prob))
  d <- rep(N / n_prob, length(prob_idx))

  pop <- data.table::data.table(x1 = x1, x2 = x2, x3 = x3, y = y)
  prob_df <- pop[prob_idx]
  prob_df[, d := d]
  prob_design <- survey::svydesign(ids = ~1, weights = ~d, data = prob_df)

  list(
    pop          = pop,
    nonprob_idx  = nonprob_idx,
    prob_idx     = prob_idx,
    prob_design  = prob_design,
    true_target  = mean(y)
  )
}
