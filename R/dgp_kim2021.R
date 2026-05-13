#' DGP from Kim, Park, Chen & Wu (2021)
#'
#' Replicates the simulation setup of Kim, J. K., Park, S., Chen, Y., & Wu, C.
#' (2021), "Combining Non-Probability and Probability Survey Samples Through
#' Mass Imputation", *Journal of the Royal Statistical Society A*, 184(3).
#' Ported from `ncn-foreigners/software-tutorials/codes/2021-kim-et-al-jrssa.R`.
#'
#' Population: `x ~ N(2, 1)`, three outcomes
#'  - `y1 = 1 + 2 x + e` with theoretical mean 5
#'  - `y2 = 3 + x + 2 e` with theoretical mean 5
#'  - `y3 = 2.5 + 0.5 x^2 + e` with theoretical mean 5
#' (`e ~ N(0, 1)` independent of `x`).
#'
#' Selection: a stratifying indicator `strata = (x <= 2)` is defined; the
#' non-probability sample takes 70% of its rows from `strata == TRUE` and 30%
#' from `strata == FALSE`, which oversamples the lower tail of `x` and
#' produces an estimable bias. The probability sample is a simple random
#' sample of size `n_prob` with equal design weights `N / n_prob`.
#'
#' @param n_pop Population size (paper uses 100000).
#' @param n_nonprob Total non-probability sample size (paper uses 1000).
#' @param n_prob Probability sample size (paper uses 500).
#' @param target_var Which outcome to expose as the scalar `true_target`.
#'   One of `"y1"`, `"y2"`, `"y3"`.
#' @param ... Ignored; accepts extra cell arguments from the generic dispatcher.
#' @return A list with `pop` (data.table with x, y1, y2, y3, strata),
#'   `nonprob_idx`, `prob_idx`, `prob_design`, `true_target`.
#' @export
dgp_kim2021 <- function(n_pop = 100000, n_nonprob = 1000, n_prob = 500,
                        target_var = "y1", ...) {
  N <- as.integer(n_pop)
  x  <- stats::rnorm(N, mean = 2, sd = 1)
  e  <- stats::rnorm(N)
  y1 <- 1   + 2 * x       + e
  y2 <- 3   +     x       + 2 * e
  y3 <- 2.5 + 0.5 * x^2   + e
  strata <- x <= 2
  pop <- data.table::data.table(x = x, y1 = y1, y2 = y2, y3 = y3,
                                strata = strata)

  prob_idx <- sample.int(N, size = as.integer(n_prob))
  prob_df  <- pop[prob_idx]
  prob_df[, d := N / n_prob]
  prob_design <- survey::svydesign(ids = ~1, weights = ~d, data = prob_df)

  n_b1 <- as.integer(round(0.7 * n_nonprob))
  n_b2 <- as.integer(n_nonprob) - n_b1
  idx_s1 <- which(strata)
  idx_s2 <- which(!strata)
  nonprob_idx <- c(
    sample(idx_s1, size = n_b1),
    sample(idx_s2, size = n_b2)
  )

  target_var <- as.character(target_var)
  if (!target_var %in% c("y1", "y2", "y3")) {
    stop("dgp_kim2021: target_var must be one of y1, y2, y3 (got '",
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
