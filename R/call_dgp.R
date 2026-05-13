#' Dispatch a DGP id to its function
#'
#' YAML configs declare `dgp: <id>`. This dispatcher maps the id to the
#' corresponding function in the `nonprobcover` package. To add a new DGP,
#' write `R/dgps/dgp_<id>.R` exporting `dgp_<id>(...)` and add it to the
#' dispatch list below.
#'
#' @param dgp_id Character; the DGP identifier from YAML.
#' @param cell A named list of the current parameter cell.
#' @return Whatever the chosen DGP returns — usually a list with elements
#'   `pop`, `nonprob_idx`, `prob_idx`, `prob_design`, `true_target`.
#' @export
call_dgp <- function(dgp_id, cell) {
  fn <- switch(
    dgp_id,
    "default"  = dgp_default,
    "kim2021"  = dgp_kim2021,
    "yang2021" = dgp_yang2021,
    stop("Unknown DGP id: '", dgp_id, "'. ",
         "Add it to R/call_dgp.R::call_dgp().")
  )
  args <- cell[intersect(names(cell), names(formals(fn)))]
  do.call(fn, args)
}
