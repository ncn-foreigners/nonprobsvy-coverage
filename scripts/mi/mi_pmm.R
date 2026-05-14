#!/usr/bin/env Rscript
# Mass imputation with predictive mean matching (default DGP).
# Sweeps pmm_match_type = {1, 2}: matching donors by predicted y vs by
# observed y in the donor pool.
# Run from the repo root: Rscript scripts/mi/mi_pmm.R

suppressPackageStartupMessages(library(nonprobcover))

nonprobcover::run_coverage_sim(
  config_path = "scripts/mi/mi_pmm_config.yml",
  output_dir  = "results/mi"
)
