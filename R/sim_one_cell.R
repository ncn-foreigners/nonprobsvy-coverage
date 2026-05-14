`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

#' Run all Monte Carlo replicates for a single parameter cell
#'
#' Design-based convention: the finite population is generated ONCE per
#' cell (via [call_dgp_population()] with `cfg$seed`). Each replicate
#' only redraws the probability and non-probability samples via
#' [call_dgp_resample()] under a per-rep seed `cfg$seed + r`.
#'
#' Failures are recorded (not raised) so a few bad reps don't kill the
#' cell. The replicate-level results are passed to [compute_metrics()] to
#' produce one row per `alpha`.
#'
#' @param cell Named list — one row of the expanded parameter grid.
#' @param cfg Parsed config.
#' @return A data.table with one row per `cfg$alpha`.
#' @export
sim_one_cell <- function(cell, cfg) {
  t0 <- Sys.time()

  # 1. Generate the finite population ONCE per cell.
  set.seed(cfg$seed)
  pop_out     <- call_dgp_population(cfg$dgp, cell)
  pop         <- pop_out$pop
  true_target <- pop_out$true_target

  # 2. Redraw samples per rep; the fit consumes pop + fresh indices.
  reps <- vector("list", cfg$n_reps)
  for (r in seq_len(cfg$n_reps)) {
    set.seed(cfg$seed + r)
    samp <- call_dgp_resample(cfg$dgp, pop, cell)
    dgp_out <- list(
      pop          = pop,
      nonprob_idx  = samp$nonprob_idx,
      prob_idx     = samp$prob_idx,
      prob_design  = samp$prob_design,
      true_target  = true_target
    )
    fit_res <- fit_nonprob(dgp_out, cell, cfg)
    reps[[r]] <- data.table::data.table(
      point   = fit_res$point,
      se      = fit_res$se,
      true    = true_target,
      success = fit_res$success
    )
  }

  reps_dt <- data.table::rbindlist(reps, fill = TRUE)
  runtime <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  compute_metrics(reps_dt, cell, cfg, runtime)
}
