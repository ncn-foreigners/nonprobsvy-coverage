#!/usr/bin/env Rscript
# Bootstrap-only counterpart of mi_nn. Reduced budget.
# Run from the repo root: Rscript scripts/mi/mi_nn_boot.R

suppressPackageStartupMessages(library(nonprobcover))

nonprobcover::run_coverage_sim(
  config_path = "scripts/mi/mi_nn_boot_config.yml",
  output_dir  = "results/mi"
)
