#' Yang, Kim & Hwang (2021) DGP — population generation
#'
#' Replicates the simulation setup of Yang et al. (2021), *Survey
#' Methodology* 47(1). Ported from
#' `ncn-foreigners/software-tutorials/codes/2021-yang-et-al-survmeth.R`.
#'
#' Population: `x1 ~ N(1, 1)`, `x2 ~ Exp(1)`, `alpha ~ N(0, 1)`, `e ~ N(0, 1)`,
#' all independent. Four outcomes:
#'   - `y11 = 1 + x1 + x2 + alpha + e`               (continuous, linear)
#'   - `y12 = 0.5 (x1 - 1.5)^2 + x2^2 + alpha + e`   (continuous, nonlinear)
#'   - `y21 = Bernoulli(plogis(1 + x1 + x2 + alpha))`        (binary, linear)
#'   - `y22 = Bernoulli(plogis(0.5 (x1-1.5)^2 + x2^2 + alpha))` (binary, nonlinear)
#'
#' @param n_pop Population size (paper uses 100000).
#' @param target_var One of `"y11"`, `"y12"`, `"y21"`, `"y22"`.
#' @param ... Ignored.
#' @return list(pop = data.table, true_target = numeric scalar).
#' @export
dgp_yang2021_population <- function(n_pop = 100000, target_var = "y11", ...) {
  N  <- as.integer(n_pop)
  x1 <- stats::rnorm(N, mean = 1, sd = 1)
  x2 <- stats::rexp(N, rate = 1)
  alpha <- stats::rnorm(N)
  e     <- stats::rnorm(N)

  y11 <- 1 + x1 + x2 + alpha + e
  y12 <- 0.5 * (x1 - 1.5)^2 + x2^2 + alpha + e
  y21 <- stats::rbinom(N, 1, stats::plogis(1 + x1 + x2 + alpha))
  y22 <- stats::rbinom(N, 1, stats::plogis(0.5 * (x1 - 1.5)^2 + x2^2 + alpha))

  pop <- data.table::data.table(x1 = x1, x2 = x2,
                                y11 = y11, y12 = y12,
                                y21 = y21, y22 = y22)

  target_var <- as.character(target_var)
  if (!target_var %in% c("y11", "y12", "y21", "y22")) {
    stop("dgp_yang2021: target_var must be one of y11, y12, y21, y22 (got '",
         target_var, "')")
  }
  list(pop = pop, true_target = mean(pop[[target_var]]))
}

#' Yang 2021 DGP — per-replicate sampling
#'
#' Probability sample: SRS of size `n_prob`. Non-probability sample:
#' Bernoulli with inclusion probabilities given by `selection_mechanism`:
#'   - `"p1"`: `plogis(x2)` (mild bias)
#'   - `"p2"`: `plogis(-3 + (x1 - 1.5)^2 + (x2 - 2)^2)` (strong nonlinear)
#'
#' @param pop data.table returned by [dgp_yang2021_population()].
#' @param n_prob Probability sample size.
#' @param selection_mechanism `"p1"` or `"p2"`.
#' @param ... Ignored. `n_nonprob` is *not* a parameter — the sample size
#'   is random because selection is Bernoulli.
#' @return list(nonprob_idx, prob_idx, prob_design).
#' @export
dgp_yang2021_resample <- function(pop, n_prob = 1000,
                                  selection_mechanism = "p1", ...) {
  N <- nrow(pop)

  prob_idx <- sample.int(N, size = as.integer(n_prob))
  prob_df  <- pop[prob_idx]
  prob_df[, d := N / n_prob]
  prob_design <- survey::svydesign(ids = ~1, weights = ~d, data = prob_df)

  selection_mechanism <- as.character(selection_mechanism)
  p_sel <- switch(
    selection_mechanism,
    "p1" = stats::plogis(pop$x2),
    "p2" = stats::plogis(-3 + (pop$x1 - 1.5)^2 + (pop$x2 - 2)^2),
    stop("dgp_yang2021: selection_mechanism must be 'p1' or 'p2' (got '",
         selection_mechanism, "')")
  )
  R <- stats::rbinom(N, 1, p_sel)
  nonprob_idx <- which(R == 1L)

  list(nonprob_idx = nonprob_idx, prob_idx = prob_idx,
       prob_design = prob_design)
}

#' Yang 2021 DGP — one-shot convenience wrapper
#' @export
dgp_yang2021 <- function(n_pop = 100000, n_prob = 1000,
                         target_var = "y11",
                         selection_mechanism = "p1", ...) {
  pop_out  <- dgp_yang2021_population(n_pop = n_pop, target_var = target_var)
  samp_out <- dgp_yang2021_resample(pop_out$pop, n_prob = n_prob,
                                     selection_mechanism = selection_mechanism)
  list(pop = pop_out$pop,
       nonprob_idx = samp_out$nonprob_idx,
       prob_idx = samp_out$prob_idx,
       prob_design = samp_out$prob_design,
       true_target = pop_out$true_target)
}
