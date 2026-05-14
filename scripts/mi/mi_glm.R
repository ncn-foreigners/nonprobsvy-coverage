#!/usr/bin/env Rscript
# Mass imputation with GLM outcome model (default DGP).
# Run from the repo root: Rscript scripts/mi/mi_glm.R

suppressPackageStartupMessages(library(nonprobcover))

nonprobcover::run_coverage_sim(
  config_path = "scripts/mi/mi_glm_config.yml",
  output_dir  = "results/mi"
)
