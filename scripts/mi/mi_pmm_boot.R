#!/usr/bin/env Rscript
# Bootstrap-only counterpart of mi_pmm. Reduced budget.
# Run from the repo root: Rscript scripts/mi/mi_pmm_boot.R

suppressPackageStartupMessages(library(nonprobcover))

nonprobcover::run_coverage_sim(
  config_path = "scripts/mi/mi_pmm_boot_config.yml",
  output_dir  = "results/mi"
)
