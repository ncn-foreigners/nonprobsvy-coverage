#' Kim, Park, Chen & Wu (2021) DGP — population generation
#'
#' Replicates the simulation setup of Kim et al. (2021), *JRSSA* 184(3).
#' Ported from `ncn-foreigners/software-tutorials/codes/2021-kim-et-al-jrssa.R`.
#'
#' Population: `x ~ N(2, 1)`, three outcomes
#'   - `y1 = 1 + 2 x + e`        with theoretical mean 5
#'   - `y2 = 3 + x + 2 e`        with theoretical mean 5
#'   - `y3 = 2.5 + 0.5 x^2 + e`  with theoretical mean 5
#' (`e ~ N(0, 1)` independent of `x`). Strata indicator `strata = (x <= 2)`.
#'
#' @param n_pop Population size (paper uses 100000).
#' @param target_var One of `"y1"`, `"y2"`, `"y3"`.
#' @param ... Ignored.
#' @return list(pop = data.table, true_target = numeric scalar).
#' @export
dgp_kim2021_population <- function(n_pop = 100000, target_var = "y1", ...) {
  N <- as.integer(n_pop)
  x  <- stats::rnorm(N, mean = 2, sd = 1)
  e  <- stats::rnorm(N)
  y1 <- 1   + 2 * x       + e
  y2 <- 3   +     x       + 2 * e
  y3 <- 2.5 + 0.5 * x^2   + e
  strata <- x <= 2
  pop <- data.table::data.table(x = x, y1 = y1, y2 = y2, y3 = y3,
                                strata = strata)

  target_var <- as.character(target_var)
  if (!target_var %in% c("y1", "y2", "y3")) {
    stop("dgp_kim2021: target_var must be one of y1, y2, y3 (got '",
         target_var, "')")
  }
  list(pop = pop, true_target = mean(pop[[target_var]]))
}

#' Kim 2021 DGP — per-replicate sampling
#'
#' Probability sample: simple random sample of size `n_prob` with weights
#' `N / n_prob`. Non-probability sample: stratified 70/30 — 70% of rows
#' come from `strata == TRUE`, 30% from `strata == FALSE`, oversampling
#' the lower tail of `x`.
#'
#' @param pop data.table returned by [dgp_kim2021_population()].
#' @param n_nonprob Total non-probability sample size.
#' @param n_prob Probability sample size.
#' @param ... Ignored.
#' @return list(nonprob_idx, prob_idx, prob_design).
#' @export
dgp_kim2021_resample <- function(pop, n_nonprob = 1000, n_prob = 500, ...) {
  N <- nrow(pop)

  prob_idx <- sample.int(N, size = as.integer(n_prob))
  prob_df  <- pop[prob_idx]
  prob_df[, d := N / n_prob]
  prob_design <- survey::svydesign(ids = ~1, weights = ~d, data = prob_df)

  n_b1 <- as.integer(round(0.7 * n_nonprob))
  n_b2 <- as.integer(n_nonprob) - n_b1
  idx_s1 <- which(pop$strata)
  idx_s2 <- which(!pop$strata)
  nonprob_idx <- c(
    sample(idx_s1, size = n_b1),
    sample(idx_s2, size = n_b2)
  )

  list(nonprob_idx = nonprob_idx, prob_idx = prob_idx,
       prob_design = prob_design)
}

#' Kim 2021 DGP — one-shot convenience wrapper
#'
#' Composes [dgp_kim2021_population()] and [dgp_kim2021_resample()]. The
#' sim driver uses the two-step path so the population stays fixed across
#' replicates; this wrapper exists for ad-hoc and test use.
#' @export
dgp_kim2021 <- function(n_pop = 100000, n_nonprob = 1000, n_prob = 500,
                        target_var = "y1", ...) {
  pop_out  <- dgp_kim2021_population(n_pop = n_pop, target_var = target_var)
  samp_out <- dgp_kim2021_resample(pop_out$pop,
                                    n_nonprob = n_nonprob, n_prob = n_prob)
  list(pop = pop_out$pop,
       nonprob_idx = samp_out$nonprob_idx,
       prob_idx = samp_out$prob_idx,
       prob_design = samp_out$prob_design,
       true_target = pop_out$true_target)
}
