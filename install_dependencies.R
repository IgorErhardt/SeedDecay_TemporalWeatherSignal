#!/usr/bin/env Rscript

# Install the packages required by the current analysis and figures.
# Run this once from the project root on a machine that does not already have
# the bundled project library. Package versions used for the frozen analysis
# remain recorded in each analysis directory's provenance files.

cran_packages <- c(
  "digest",
  "ggplot2",
  "ggrepel",
  "lmtest",
  "patchwork",
  "readxl",
  "r4pde",
  "rnaturalearth",
  "sf"
)

missing_cran <- cran_packages[!vapply(cran_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_cran)) {
  install.packages(missing_cran, repos = "https://cloud.r-project.org")
}

cat("Required R packages are available.\n")
