#!/usr/bin/env Rscript
# Bootstrap-only counterpart of ipw_scad. Lower budget than the analytic
# script (CV inside each fit makes the inner bootstrap loop expensive).
# Run from the repo root: Rscript scripts/ipw/ipw_scad_boot.R

suppressPackageStartupMessages(library(nonprobcover))

nonprobcover::run_coverage_sim(
  config_path = "scripts/ipw/ipw_scad_boot_config.yml",
  output_dir  = "results/ipw"
)
