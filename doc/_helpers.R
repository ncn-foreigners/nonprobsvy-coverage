# Shared helpers for Quarto pages. Each doc/<family>/<script>.qmd sources
# this file from a setup chunk, then calls `render_coverage_page()` with
# the path to its CSVs. Keeps qmd boilerplate tiny.

suppressPackageStartupMessages({
  library(data.table)
  library(knitr)
})

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

# Inline-HTML color wrapper for coverage cells.
style_coverage <- function(coverage, nominal, mcse) {
  flag <- nonprobcover::flag_coverage(coverage, nominal, mcse)
  bg   <- c(green = "#cfeacf", amber = "#ffe6b3", red = "#f4c4c4")[flag]
  bg[is.na(bg)] <- "#eeeeee"
  sprintf(
    '<span style="background:%s;padding:2px 6px;border-radius:3px;">%.3f</span>',
    bg, coverage
  )
}

# Read coverage + metadata for one script. Returns a list(cov_dt, meta_dt).
load_results <- function(family, script) {
  base    <- file.path("..", "..", "results", family)
  cov_dt  <- data.table::fread(file.path(base, paste0(script, "_coverage.csv")))
  meta_dt <- data.table::fread(file.path(base, paste0(script, "_metadata.csv")))
  list(cov = cov_dt, meta = meta_dt)
}

# Drop columns that are entirely NA or constant — keeps tables readable.
drop_uninformative <- function(dt, keep) {
  for (col in setdiff(names(dt), keep)) {
    v <- dt[[col]]
    if (all(is.na(v)) || length(unique(v)) == 1L) {
      dt[, (col) := NULL]
    }
  }
  dt
}

# Build the coverage table for a single script's CSV.
render_coverage_page <- function(family, script,
                                 id_cols = c("n_pop", "n_nonprob", "n_prob",
                                             "method_selection", "method_outcome",
                                             "family_outcome", "vars_selection",
                                             "bias_correction", "var_method",
                                             "rep_type", "num_boot", "alpha")) {
  res  <- load_results(family, script)
  cov  <- res$cov
  meta <- res$meta

  display <- copy(cov)
  display[, coverage_html := style_coverage(coverage, 1 - alpha, coverage_mcse)]

  out_cols <- c(
    intersect(id_cols, names(display)),
    "n_reps", "n_failed", "bias", "rmse", "mc_se", "mean_se",
    "coverage_html", "ci_width"
  )
  out <- display[, ..out_cols]
  setnames(out, "coverage_html", "coverage")

  # Round numeric cols for display.
  num_cols <- c("bias", "rmse", "mc_se", "mean_se", "ci_width")
  for (col in intersect(num_cols, names(out))) {
    out[, (col) := sprintf("%.3f", get(col))]
  }

  out <- drop_uninformative(out, keep = c("alpha", "coverage", "n_reps"))

  cat(sprintf(
    paste0("**nonprobsvy version:** `%s` &nbsp;|&nbsp; **R:** `%s` &nbsp;|&nbsp;",
           " **run:** `%s` &nbsp;|&nbsp; **commit:** `%s`\n\n"),
    meta$nonprobsvy_version, meta$r_version, meta$run_date, meta$commit_sha
  ))

  print(knitr::kable(out, format = "html", escape = FALSE,
                     align = "c", table.attr = 'class="table table-sm"'))
}
