#!/usr/bin/env Rscript
# Doubly-robust IPW + GLM outcome on the default DGP.
# Run from the repo root: Rscript scripts/dr/dr_glm.R

suppressPackageStartupMessages(library(nonprobcover))

nonprobcover::run_coverage_sim(
  config_path = "scripts/dr/dr_glm_config.yml",
  output_dir  = "results/dr"
)
