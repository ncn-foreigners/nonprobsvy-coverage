#' Fit `nonprob()` for a single Monte Carlo replicate
#'
#' Dispatches on `cfg$estimator` (currently `"ipw"`; `"mi"` and `"dr"` will
#' arrive in later phases). Returns the point estimate and SE on a fixed,
#' easily-rbinded shape. Any error inside the fit is caught — the replicate
#' is marked `success = FALSE` rather than crashing the whole cell.
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
    "mi"  = fit_mi(dgp_out,  cell, cfg),
    "dr"  = fit_dr(dgp_out,  cell, cfg),
    stop("Unknown estimator: '", cfg$estimator, "'")
  )
}

#' @keywords internal
fit_ipw <- function(dgp_out, cell, cfg) {
  nonprob_df     <- dgp_out$pop[dgp_out$nonprob_idx]
  selection_form <- stats::as.formula(as.character(cfg$selection_formula))
  target_form    <- stats::as.formula(paste("~", as.character(cell$target_var)))

  fit <- tryCatch(
    nonprobsvy::nonprob(
      data              = nonprob_df,
      selection         = selection_form,
      target            = target_form,
      svydesign         = dgp_out$prob_design,
      method_selection  = as.character(cell$method_selection),
      control_selection = make_control_sel(cell),
      control_inference = make_control_inf(cell, cfg),
      se                = TRUE
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
fit_mi <- function(dgp_out, cell, cfg) {
  nonprob_df  <- dgp_out$pop[dgp_out$nonprob_idx]
  outcome_rhs <- as.character(cfg$outcome_rhs %||% cell$outcome_rhs)
  if (length(outcome_rhs) == 0L || is.na(outcome_rhs)) {
    stop("fit_mi: config is missing 'outcome_rhs'")
  }
  outcome_form <- stats::as.formula(
    paste(as.character(cell$target_var), "~", outcome_rhs)
  )

  fit <- tryCatch(
    nonprobsvy::nonprob(
      data              = nonprob_df,
      outcome           = outcome_form,
      svydesign         = dgp_out$prob_design,
      method_outcome    = as.character(cell$method_outcome),
      family_outcome    = as.character(cell$family_outcome %||% "gaussian"),
      control_outcome   = make_control_out(cell),
      control_inference = make_control_inf(cell, cfg),
      se                = TRUE
    ),
    error = function(e) {
      message("[mi] fit failed: ", conditionMessage(e))
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
fit_dr <- function(dgp_out, cell, cfg) {
  nonprob_df  <- dgp_out$pop[dgp_out$nonprob_idx]
  outcome_rhs <- as.character(cfg$outcome_rhs %||% cell$outcome_rhs)
  if (length(outcome_rhs) == 0L || is.na(outcome_rhs)) {
    stop("fit_dr: config is missing 'outcome_rhs'")
  }
  selection_form <- stats::as.formula(as.character(cfg$selection_formula))
  outcome_form   <- stats::as.formula(
    paste(as.character(cell$target_var), "~", outcome_rhs)
  )

  fit <- tryCatch(
    nonprobsvy::nonprob(
      data              = nonprob_df,
      selection         = selection_form,
      outcome           = outcome_form,
      svydesign         = dgp_out$prob_design,
      method_selection  = as.character(cell$method_selection),
      method_outcome    = as.character(cell$method_outcome %||% "glm"),
      family_outcome    = as.character(cell$family_outcome %||% "gaussian"),
      control_selection = make_control_sel(cell),
      control_outcome   = make_control_out(cell),
      control_inference = make_control_inf(cell, cfg),
      se                = TRUE
    ),
    error = function(e) {
      message("[dr] fit failed: ", conditionMessage(e))
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

#' Build a `control_out` object from the current cell
#'
#' Mirrors [make_control_sel()]. Only sets arguments that are present and
#' non-NA in the cell, so cells that don't mention (say) `pmm_match_type`
#' get nonprobsvy's defaults.
#' @keywords internal
make_control_out <- function(cell) {
  args <- list()
  set <- function(name, cast) {
    v <- cell[[name]]
    if (length(v) && !is.na(v)) args[[name]] <<- cast(v)
  }
  set("k",               as.integer)
  set("penalty",         as.character)
  set("nfolds",          as.integer)
  set("nlambda",         as.integer)
  set("lambda_min",      as.numeric)
  set("pmm_match_type",  as.integer)
  set("pmm_weights",     as.character)
  set("pmm_reg_engine",  as.character)
  do.call(nonprobsvy::control_out, args)
}

#' Build a `control_sel` object from the current cell
#'
#' Only sets arguments that are present and non-NA in the cell, so cells
#' that don't mention (say) `penalty` get nonprobsvy's defaults.
#' @keywords internal
make_control_sel <- function(cell) {
  args <- list()
  set <- function(name, cast) {
    v <- cell[[name]]
    if (length(v) && !is.na(v)) args[[name]] <<- cast(v)
  }
  set("est_method", as.character)
  set("gee_h_fun",  as.integer)
  set("penalty",    as.character)
  set("nfolds",     as.integer)
  set("nlambda",    as.integer)
  set("lambda_min", as.numeric)
  do.call(nonprobsvy::control_sel, args)
}

#' Build a `control_inf` object from the current cell + config
#'
#' Var method always set. Bootstrap-only parameters (`rep_type`, `num_boot`)
#' are passed only when `var_method == "bootstrap"`. `vars_selection` is
#' passed through when explicitly true.
#' @keywords internal
make_control_inf <- function(cell, cfg) {
  args <- list(var_method = as.character(cell$var_method))
  if (identical(args$var_method, "bootstrap")) {
    if (length(cell$rep_type) && !is.na(cell$rep_type)) {
      args$rep_type <- as.character(cell$rep_type)
    }
    if (!is.null(cfg$num_boot)) {
      args$num_boot <- as.integer(cfg$num_boot)
    }
  }
  if (length(cell$vars_selection) && !is.na(cell$vars_selection)) {
    args$vars_selection <- as.logical(cell$vars_selection)
  }
  do.call(nonprobsvy::control_inf, args)
}
