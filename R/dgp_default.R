#' Default coverage-spec DGP — population generation step
#'
#' Design-based simulation convention: the finite population is generated
#' once per cell. Each Monte Carlo replicate then redraws *only* the
#' probability and non-probability samples (via [dgp_default_resample()]).
#'
#' Three independent standard-normal covariates, linear outcome
#' `y = 1 + x1 + 0.5 x2 - 0.3 x3 + N(0, 1)`. The true target is the
#' finite-population mean of `y` (or whichever column `target_var` names).
#'
#' @param n_pop Population size.
#' @param target_var Outcome column whose finite-population mean is the
#'   `true_target`. Only `"y"` is meaningful for this DGP.
#' @param ... Ignored; lets the generic dispatcher pass extra cell entries.
#' @return list(pop = data.table, true_target = numeric scalar).
#' @export
dgp_default_population <- function(n_pop = 10000, target_var = "y", ...) {
  N  <- as.integer(n_pop)
  x1 <- stats::rnorm(N)
  x2 <- stats::rnorm(N)
  x3 <- stats::rnorm(N)
  y  <- 1 + x1 + 0.5 * x2 - 0.3 * x3 + stats::rnorm(N)
  pop <- data.table::data.table(x1 = x1, x2 = x2, x3 = x3, y = y)
  list(pop = pop, true_target = mean(pop[[as.character(target_var)]]))
}

#' Default DGP — per-replicate sampling step
#'
#' Selection is logit linear in `x1` and `x2` with the intercept chosen so
#' `E[sum(R_i)] = n_nonprob`. The probability sample is a simple random
#' sample of size `n_prob` with equal design weights.
#'
#' @param pop data.table returned by [dgp_default_population()].
#' @param n_nonprob Expected non-probability sample size.
#' @param n_prob Probability sample size.
#' @param ... Ignored.
#' @return list(nonprob_idx, prob_idx, prob_design).
#' @export
dgp_default_resample <- function(pop, n_nonprob = 500, n_prob = 500, ...) {
  N <- nrow(pop)
  lin_no_intercept <- 0.7 * pop$x1 - 0.4 * pop$x2
  theta0 <- stats::uniroot(
    function(a) sum(stats::plogis(a + lin_no_intercept)) - n_nonprob,
    interval = c(-15, 5)
  )$root
  p_sel <- stats::plogis(theta0 + lin_no_intercept)
  R <- stats::rbinom(N, 1, p_sel)
  nonprob_idx <- which(R == 1L)

  prob_idx <- sample.int(N, size = as.integer(n_prob))
  prob_df  <- pop[prob_idx]
  prob_df[, d := N / n_prob]
  prob_design <- survey::svydesign(ids = ~1, weights = ~d, data = prob_df)

  list(nonprob_idx = nonprob_idx, prob_idx = prob_idx,
       prob_design = prob_design)
}

#' Default DGP — one-shot wrapper (population + samples in a single call)
#'
#' Convenience wrapper around [dgp_default_population()] +
#' [dgp_default_resample()]. Kept for ad-hoc use and for tests that need
#' both steps; the sim driver itself uses the two-step path so the
#' population stays fixed across replicates.
#' @export
dgp_default <- function(n_pop = 10000, n_nonprob = 500, n_prob = 500,
                        target_var = "y", ...) {
  pop_out  <- dgp_default_population(n_pop = n_pop, target_var = target_var)
  samp_out <- dgp_default_resample(pop_out$pop,
                                    n_nonprob = n_nonprob, n_prob = n_prob)
  list(pop = pop_out$pop,
       nonprob_idx = samp_out$nonprob_idx,
       prob_idx = samp_out$prob_idx,
       prob_design = samp_out$prob_design,
       true_target = pop_out$true_target)
}
