#' Read a YAML simulation config file
#'
#' The YAML may contain scalar values, vectors (interpreted as grid axes to
#' expand), and nested lists. `alpha` is treated as a vector but is NOT part of
#' the simulation grid — confidence intervals are computed analytically from
#' point + SE for each alpha after the simulation runs.
#'
#' @param path Path to a YAML file.
#' @return A list with the parsed config plus a `config_path` element.
#' @export
load_config <- function(path) {
  cfg <- yaml::read_yaml(path)
  cfg$config_path <- path
  required <- c("script", "estimator", "dgp", "n_reps", "alpha", "seed")
  missing <- setdiff(required, names(cfg))
  if (length(missing)) {
    stop("Config is missing required keys: ",
         paste(missing, collapse = ", "))
  }
  cfg
}

#' Expand a config into a parameter grid (one row per simulation cell)
#'
#' Vector-valued keys become grid axes. `alpha`, `n_reps`, `seed`,
#' `script`, `estimator`, `dgp`, `num_boot`, `config_path` are excluded —
#' they are either single values or post-processed.
#'
#' @param cfg Output of [load_config()].
#' @return A data.table with one row per parameter combination.
#' @export
expand_grid_from_config <- function(cfg) {
  exclude <- c("alpha", "n_reps", "seed", "script", "estimator", "dgp",
               "num_boot", "config_path")
  grid_keys <- setdiff(names(cfg), exclude)
  grid_vals <- cfg[grid_keys]
  grid_vals <- lapply(grid_vals, function(x) if (is.null(x)) NA else x)
  grid <- do.call(data.table::CJ,
                  c(grid_vals, list(sorted = FALSE, unique = TRUE)))
  if (nrow(grid) == 0) {
    grid <- data.table::data.table(.placeholder = 1)
    grid[, .placeholder := NULL]
    grid <- grid[1]
  }
  grid
}
