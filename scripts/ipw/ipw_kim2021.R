#!/usr/bin/env Rscript
# IPW (MLE) coverage on Kim, Park, Chen & Wu (2021) JRSSA DGP.
# Runs separately for each of y1, y2, y3 — the paper's three outcomes.
# Run from the repo root: Rscript scripts/ipw/ipw_kim2021.R

suppressPackageStartupMessages(library(nonprobcover))

nonprobcover::run_coverage_sim(
  config_path = "scripts/ipw/ipw_kim2021_config.yml",
  output_dir  = "results/ipw"
)
