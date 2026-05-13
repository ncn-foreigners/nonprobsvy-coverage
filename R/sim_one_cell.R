`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

#' Run all Monte Carlo replicates for a single parameter cell
#'
#' For each replicate, sets the per-rep seed, draws from the DGP, fits the
#' estimator, and stores point + SE + truth. Failures are recorded (not
#' raised) so a few bad reps don't kill the cell. The replicate-level results
#' are passed to [compute_metrics()] to produce one row per `alpha`.
#'
#' @param cell Named list — one row of the expanded parameter grid.
#' @param cfg Parsed config.
#' @return A data.table with one row per `cfg$alpha`.
#' @export
sim_one_cell <- function(cell, cfg) {
  t0   <- Sys.time()
  reps <- vector("list", cfg$n_reps)

  for (r in seq_len(cfg$n_reps)) {
    set.seed(cfg$seed + r)
    dgp_out <- call_dgp(cfg$dgp, cell)
    fit_res <- fit_nonprob(dgp_out, cell, cfg)
    reps[[r]] <- data.table::data.table(
      point   = fit_res$point,
      se      = fit_res$se,
      true    = dgp_out$true_target,
      success = fit_res$success
    )
  }

  reps_dt <- data.table::rbindlist(reps, fill = TRUE)
  runtime <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  compute_metrics(reps_dt, cell, cfg, runtime)
}
