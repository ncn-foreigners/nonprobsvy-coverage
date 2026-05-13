#' DGP from Yang, Kim & Hwang (2021)
#'
#' Replicates the simulation setup of Yang, S., Kim, J. K., & Hwang, Y. (2021),
#' "Integration of Data from Probability Surveys and Big Found Data for Finite
#' Population Inference Using Mass Imputation", *Survey Methodology*, 47(1).
#' Ported from `ncn-foreigners/software-tutorials/codes/2021-yang-et-al-survmeth.R`.
#'
#' Population: `x1 ~ N(1, 1)`, `x2 ~ Exp(1)`, `alpha ~ N(0, 1)`, `e ~ N(0, 1)`,
#' all independent. Four outcomes:
#'   - `y11 = 1 + x1 + x2 + alpha + e`               (continuous, linear)
#'   - `y12 = 0.5 (x1 - 1.5)^2 + x2^2 + alpha + e`   (continuous, nonlinear)
#'   - `y21 = Bernoulli(plogis(1 + x1 + x2 + alpha))`        (binary, linear)
#'   - `y22 = Bernoulli(plogis(0.5 (x1-1.5)^2 + x2^2 + alpha))` (binary, nonlinear)
#'
#' Two selection mechanisms are defined in the paper:
#'   - `p1 = plogis(x2)`                                 (mild bias)
#'   - `p2 = plogis(-3 + (x1 - 1.5)^2 + (x2 - 2)^2)`     (strong nonlinear bias)
#' The non-probability sample is a Bernoulli sample from the population with
#' inclusion probabilities `p1` or `p2` (selectable via `selection_mechanism`).
#' The probability sample is a simple random sample of size `n_prob`.
#'
#' @param n_pop Population size (paper uses 100000).
#' @param n_prob Probability sample size (paper uses 1000).
#' @param target_var Which outcome to expose as the scalar `true_target`.
#'   One of `"y11"`, `"y12"`, `"y21"`, `"y22"`.
#' @param selection_mechanism Either `"p1"` (default, linear in x2) or `"p2"`
#'   (nonlinear in x1 and x2).
#' @param ... Ignored; accepts extra cell arguments from the generic dispatcher.
#'   Note: `n_nonprob` is *not* a parameter — the non-probability sample size
#'   is random because selection is Bernoulli. The DGP returns whatever was
#'   drawn.
#' @return A list with `pop` (data.table), `nonprob_idx`, `prob_idx`,
#'   `prob_design`, `true_target`.
#' @export
dgp_yang2021 <- function(n_pop = 100000, n_prob = 1000,
                         target_var = "y11",
                         selection_mechanism = "p1", ...) {
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

  prob_idx <- sample.int(N, size = as.integer(n_prob))
  prob_df  <- pop[prob_idx]
  prob_df[, d := N / n_prob]
  prob_design <- survey::svydesign(ids = ~1, weights = ~d, data = prob_df)

  selection_mechanism <- as.character(selection_mechanism)
  p_sel <- switch(
    selection_mechanism,
    "p1" = stats::plogis(x2),
    "p2" = stats::plogis(-3 + (x1 - 1.5)^2 + (x2 - 2)^2),
    stop("dgp_yang2021: selection_mechanism must be 'p1' or 'p2' (got '",
         selection_mechanism, "')")
  )
  R <- stats::rbinom(N, 1, p_sel)
  nonprob_idx <- which(R == 1L)

  target_var <- as.character(target_var)
  if (!target_var %in% c("y11", "y12", "y21", "y22")) {
    stop("dgp_yang2021: target_var must be one of y11, y12, y21, y22 (got '",
         target_var, "')")
  }

  list(
    pop          = pop,
    nonprob_idx  = nonprob_idx,
    prob_idx     = prob_idx,
    prob_design  = prob_design,
    true_target  = mean(pop[[target_var]])
  )
}
