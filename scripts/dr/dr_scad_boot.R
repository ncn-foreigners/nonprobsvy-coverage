#!/usr/bin/env Rscript
# Bootstrap-only counterpart of dr_scad. Reduced budget.
# Run from the repo root: Rscript scripts/dr/dr_scad_boot.R

suppressPackageStartupMessages(library(nonprobcover))

nonprobcover::run_coverage_sim(
  config_path = "scripts/dr/dr_scad_boot_config.yml",
  output_dir  = "results/dr"
)
