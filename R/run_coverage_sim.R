#' Run a YAML-driven coverage simulation end to end
#'
#' Reads the config, expands the parameter grid, runs each cell in parallel
#' (or sequentially if `NPC_WORKERS=1` or on Windows), aggregates metrics, and
#' writes three files into `output_dir`:
#'
#' - `<script>_coverage.csv`  — one row per parameter cell × alpha
#' - `<script>_metadata.csv`  — one provenance row
#' - `<script>_config.yml`    — verbatim copy of the input config
#'
#' Parallelism is controlled by env var `NPC_WORKERS`. Defaults to the number
#' of available cores. macOS / Linux only — Windows falls back to sequential.
#'
#' @param config_path Path to the YAML config.
#' @param output_dir Where to write the CSVs.
#' @return Invisibly, the coverage data.table.
#' @export
run_coverage_sim <- function(config_path, output_dir) {
  cfg  <- load_config(config_path)
  grid <- expand_grid_from_config(cfg)

  message(sprintf(
    "[nonprobcover] %s: %d cell(s) x %d reps each",
    cfg$script, nrow(grid), cfg$n_reps
  ))

  workers <- suppressWarnings(as.integer(
    Sys.getenv("NPC_WORKERS", unset = parallel::detectCores())
  ))
  if (is.na(workers) || workers < 1L) workers <- 1L
  workers <- min(workers, nrow(grid))
  if (.Platform$OS.type == "windows") workers <- 1L

  t0 <- Sys.time()
  if (workers <= 1L) {
    rows_list <- lapply(
      seq_len(nrow(grid)),
      function(i) sim_one_cell(as.list(grid[i, , drop = FALSE]), cfg)
    )
  } else {
    rows_list <- parallel::mclapply(
      seq_len(nrow(grid)),
      function(i) sim_one_cell(as.list(grid[i, , drop = FALSE]), cfg),
      mc.cores     = workers,
      mc.set.seed  = TRUE
    )
  }
  rows <- data.table::rbindlist(rows_list, fill = TRUE)
  total_runtime <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

  write_results(rows, cfg, output_dir, total_runtime)
  message(sprintf(
    "[nonprobcover] %s done in %.1fs (%d rows written)",
    cfg$script, total_runtime, nrow(rows)
  ))
  invisible(rows)
}
