#!/usr/bin/env Rscript
# IPW with GEE-based calibration constraints (default DGP).
# Run from the repo root: Rscript scripts/ipw/ipw_gee.R

suppressPackageStartupMessages(library(nonprobcover))

nonprobcover::run_coverage_sim(
  config_path = "scripts/ipw/ipw_gee_config.yml",
  output_dir  = "results/ipw"
)
