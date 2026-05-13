#' Fit `nonprob()` for a single Monte Carlo replicate
#'
#' Dispatches on `cfg$estimator` (`"ipw"`, `"mi"`, `"dr"`). Returns the point
#' estimate and SE on a fixed, easily-rbinded shape. Any error inside the fit
#' is caught — the replicate is marked `success = FALSE` rather than crashing
#' the whole cell.
#'
#' @param dgp_out Output of a DGP (see [dgp_default()]).
#' @param cell Named list of the current parameter cell.
#' @param cfg Parsed config (output of [load_config()]).
#' @return list(point, se, success).
#' @keywords internal
fit_nonprob <- function(dgp_out, cell, cfg) {
  switch(
    cfg$estimator,
    "ipw" = fit_ipw(dgp_out, cell, cfg),
    stop("Unknown estimator: '", cfg$estimator, "'")
  )
}

#' @keywords internal
fit_ipw <- function(dgp_out, cell, cfg) {
  nonprob_df <- dgp_out$pop[dgp_out$nonprob_idx]
  control <- make_control_inf(cell, cfg)

  fit <- tryCatch(
    nonprobsvy::nonprob(
      data             = nonprob_df,
      selection        = stats::as.formula(cfg$selection_formula),
      target           = stats::as.formula(cfg$target_formula),
      svydesign        = dgp_out$prob_design,
      method_selection = as.character(cell$method_selection),
      control_inference = control,
      se               = TRUE
    ),
    error = function(e) {
      message("[ipw] fit failed: ", conditionMessage(e))
      NULL
    }
  )

  if (is.null(fit)) {
    return(list(point = NA_real_, se = NA_real_, success = FALSE))
  }
  list(
    point   = unname(fit$output$mean[1]),
    se      = unname(fit$output$SE[1]),
    success = TRUE
  )
}

#' @keywords internal
make_control_inf <- function(cell, cfg) {
  args <- list(var_method = as.character(cell$var_method))
  if (identical(args$var_method, "bootstrap")) {
    if (!is.null(cell$rep_type) && !is.na(cell$rep_type)) {
      args$rep_type <- as.character(cell$rep_type)
    }
    if (!is.null(cfg$num_boot)) {
      args$num_boot <- as.integer(cfg$num_boot)
    }
  }
  do.call(nonprobsvy::control_inf, args)
}
