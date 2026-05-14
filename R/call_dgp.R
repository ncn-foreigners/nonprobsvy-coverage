#' Dispatch a DGP id to its population-generation function
#'
#' Design-based simulation: the population is generated once per cell
#' and held fixed across Monte Carlo replicates. The companion
#' [call_dgp_resample()] draws fresh samples from that fixed population.
#'
#' YAML configs declare `dgp: <id>`. This dispatcher maps the id to the
#' `dgp_<id>_population()` function. To add a new DGP, write
#' `R/dgp_<id>.R` exporting `dgp_<id>_population()` and
#' `dgp_<id>_resample()` and add it to both switches below.
#'
#' @param dgp_id Character; the DGP identifier from YAML.
#' @param cell A named list of the current parameter cell.
#' @return list(pop = data.table, true_target = numeric).
#' @export
call_dgp_population <- function(dgp_id, cell) {
  fn <- switch(
    dgp_id,
    "default"  = dgp_default_population,
    "kim2021"  = dgp_kim2021_population,
    "yang2021" = dgp_yang2021_population,
    stop("Unknown DGP id: '", dgp_id, "'. ",
         "Add it to R/call_dgp.R::call_dgp_population().")
  )
  args <- cell[intersect(names(cell), names(formals(fn)))]
  do.call(fn, args)
}

#' Dispatch a DGP id to its resampling function
#'
#' Companion to [call_dgp_population()]. Draws the probability and
#' non-probability samples from a pre-generated population.
#'
#' @param dgp_id Character; the DGP identifier from YAML.
#' @param pop data.table from the population step.
#' @param cell A named list of the current parameter cell.
#' @return list(nonprob_idx, prob_idx, prob_design).
#' @export
call_dgp_resample <- function(dgp_id, pop, cell) {
  fn <- switch(
    dgp_id,
    "default"  = dgp_default_resample,
    "kim2021"  = dgp_kim2021_resample,
    "yang2021" = dgp_yang2021_resample,
    stop("Unknown DGP id: '", dgp_id, "'. ",
         "Add it to R/call_dgp.R::call_dgp_resample().")
  )
  args <- cell[intersect(names(cell), names(formals(fn)))]
  do.call(fn, c(list(pop = pop), args))
}

#' Backward-compat dispatcher (population + resample in one shot)
#'
#' Kept for ad-hoc calls and tests. The sim driver uses the split path
#' directly so the population stays fixed across replicates.
#' @export
call_dgp <- function(dgp_id, cell) {
  pop_out  <- call_dgp_population(dgp_id, cell)
  samp_out <- call_dgp_resample(dgp_id, pop_out$pop, cell)
  list(pop = pop_out$pop,
       nonprob_idx = samp_out$nonprob_idx,
       prob_idx = samp_out$prob_idx,
       prob_design = samp_out$prob_design,
       true_target = pop_out$true_target)
}
