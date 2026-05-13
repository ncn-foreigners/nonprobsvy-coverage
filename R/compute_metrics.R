#' Aggregate replicate-level estimates into coverage metrics
#'
#' Builds confidence intervals for each requested alpha from the same point
#' and SE pairs (no refitting). Emits one row per alpha. Provenance columns
#' (`nonprobsvy_version`, `r_version`, `commit_sha`, `run_date`) are filled
#' here so they travel with every row.
#'
#' @param reps_dt data.table with columns point, se, true, success.
#' @param cell Named list — the parameter cell that produced these reps.
#' @param cfg Parsed config.
#' @param runtime Wall-clock seconds for this cell.
#' @return data.table, one row per `cfg$alpha`.
#' @export
compute_metrics <- function(reps_dt, cell, cfg, runtime) {
  ok       <- reps_dt[reps_dt$success == TRUE, ]
  n_reps   <- nrow(ok)
  n_failed <- nrow(reps_dt) - n_reps

  rows <- lapply(cfg$alpha, function(a) {
    if (n_reps == 0L) {
      cov <- bias <- rmse <- mc_se <- mean_se <- ci_w <- true_t <- NA_real_
      cov_mcse <- NA_real_
    } else {
      z       <- stats::qnorm(1 - a / 2)
      lower   <- ok$point - z * ok$se
      upper   <- ok$point + z * ok$se
      covered <- ok$true >= lower & ok$true <= upper
      cov     <- mean(covered, na.rm = TRUE)
      bias    <- mean(ok$point - ok$true)
      rmse    <- sqrt(mean((ok$point - ok$true)^2))
      mc_se   <- stats::sd(ok$point) / sqrt(n_reps)
      mean_se <- mean(ok$se, na.rm = TRUE)
      ci_w    <- mean(upper - lower, na.rm = TRUE)
      true_t  <- mean(ok$true)
      cov_mcse <- sqrt(cov * (1 - cov) / n_reps)
    }

    data.table::data.table(
      script             = cfg$script,
      estimator          = cfg$estimator,
      dgp                = cfg$dgp,
      n_pop              = as.integer(cell$n_pop %||% NA),
      n_nonprob          = as.integer(cell$n_nonprob %||% NA),
      n_prob             = as.integer(cell$n_prob %||% NA),
      method_selection   = as.character(cell$method_selection %||% NA),
      method_outcome     = as.character(cell$method_outcome %||% NA),
      family_outcome     = as.character(cell$family_outcome %||% NA),
      vars_selection     = as.logical(cell$vars_selection %||% NA),
      bias_correction    = as.logical(cell$bias_correction %||% NA),
      var_method         = as.character(cell$var_method %||% NA),
      rep_type           = as.character(cell$rep_type %||% NA),
      num_boot           = as.integer(
        if (!is.null(cfg$num_boot) && identical(as.character(cell$var_method), "bootstrap"))
          cfg$num_boot else NA),
      alpha              = a,
      n_reps             = n_reps,
      n_failed           = n_failed,
      true_target        = true_t,
      bias               = bias,
      rmse               = rmse,
      mc_se              = mc_se,
      mean_se            = mean_se,
      coverage           = cov,
      coverage_mcse      = cov_mcse,
      ci_width           = ci_w,
      runtime_sec        = runtime,
      nonprobsvy_version = as.character(utils::packageVersion("nonprobsvy")),
      r_version          = paste(R.version$major, R.version$minor, sep = "."),
      commit_sha         = get_commit_sha(),
      run_date           = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    )
  })

  data.table::rbindlist(rows, fill = TRUE)
}

#' @keywords internal
get_commit_sha <- function() {
  tryCatch({
    out <- suppressWarnings(system2(
      "git", c("rev-parse", "--short", "HEAD"),
      stdout = TRUE, stderr = FALSE
    ))
    if (length(out)) as.character(out[1]) else NA_character_
  }, error = function(e) NA_character_)
}
