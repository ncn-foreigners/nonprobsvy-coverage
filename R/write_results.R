#' Write a coverage CSV + sibling metadata + a copy of the input config
#'
#' @param rows data.table returned by [run_coverage_sim()].
#' @param cfg Parsed config.
#' @param output_dir Output directory (e.g. `"results/ipw"`).
#' @param total_runtime Wall-clock seconds for the whole sim.
#' @return Invisibly, the list of paths written.
#' @export
write_results <- function(rows, cfg, output_dir, total_runtime) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  base <- cfg$script

  cov_path  <- file.path(output_dir, paste0(base, "_coverage.csv"))
  meta_path <- file.path(output_dir, paste0(base, "_metadata.csv"))
  cfg_path  <- file.path(output_dir, paste0(base, "_config.yml"))

  data.table::fwrite(rows, cov_path)

  workers <- suppressWarnings(as.integer(
    Sys.getenv("NPC_WORKERS", unset = parallel::detectCores())
  ))

  meta <- data.table::data.table(
    script             = cfg$script,
    estimator          = cfg$estimator,
    dgp                = cfg$dgp,
    n_cells            = nrow(rows),
    total_runtime_sec  = total_runtime,
    workers            = workers,
    nonprobsvy_version = as.character(utils::packageVersion("nonprobsvy")),
    r_version          = paste(R.version$major, R.version$minor, sep = "."),
    platform           = R.version$platform,
    commit_sha         = get_commit_sha(),
    run_date           = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )
  data.table::fwrite(meta, meta_path)

  if (!is.null(cfg$config_path) && file.exists(cfg$config_path)) {
    file.copy(cfg$config_path, cfg_path, overwrite = TRUE)
  }

  invisible(list(coverage = cov_path, metadata = meta_path, config = cfg_path))
}
