#!/usr/bin/env Rscript
# IPW with penalised variable selection (SCAD / lasso / MCP) on the default DGP.
# Run from the repo root: Rscript scripts/ipw/ipw_scad.R

suppressPackageStartupMessages(library(nonprobcover))

nonprobcover::run_coverage_sim(
  config_path = "scripts/ipw/ipw_scad_config.yml",
  output_dir  = "results/ipw"
)
