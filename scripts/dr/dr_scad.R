#!/usr/bin/env Rscript
# Doubly-robust with penalised variable selection (SCAD / lasso / MCP) on
# both selection and outcome models. Default DGP includes a noise variable
# x3 that each penalty should learn to drop on both sides.
# Run from the repo root: Rscript scripts/dr/dr_scad.R

suppressPackageStartupMessages(library(nonprobcover))

nonprobcover::run_coverage_sim(
  config_path = "scripts/dr/dr_scad_config.yml",
  output_dir  = "results/dr"
)
