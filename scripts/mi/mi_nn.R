#!/usr/bin/env Rscript
# Mass imputation with nearest-neighbour outcome model (default DGP).
# Sweeps k = {1, 5} as in Yang, Kim & Hwang (2021).
# Run from the repo root: Rscript scripts/mi/mi_nn.R

suppressPackageStartupMessages(library(nonprobcover))

nonprobcover::run_coverage_sim(
  config_path = "scripts/mi/mi_nn_config.yml",
  output_dir  = "results/mi"
)
