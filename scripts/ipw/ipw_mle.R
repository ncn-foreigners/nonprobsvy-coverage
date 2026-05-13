#!/usr/bin/env Rscript
# Spike: IPW (MLE) coverage simulation with the default DGP.
# Run from the repo root: Rscript scripts/ipw/ipw_mle.R

suppressPackageStartupMessages(library(nonprobcover))

cfg_path <- "scripts/ipw/ipw_mle_config.yml"
nonprobcover::run_coverage_sim(
  config_path = cfg_path,
  output_dir  = "results/ipw"
)
