#!/usr/bin/env Rscript
# Mass imputation with nonparametric (loess) outcome model (default DGP).
# Run from the repo root: Rscript scripts/mi/mi_npar.R

suppressPackageStartupMessages(library(nonprobcover))

nonprobcover::run_coverage_sim(
  config_path = "scripts/mi/mi_npar_config.yml",
  output_dir  = "results/mi"
)
