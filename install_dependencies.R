#!/usr/bin/env Rscript

# Install the packages required by the current analysis and figures.
# Run this once from the project root on a machine that does not already have
# the bundled project library. Package versions used for the frozen analysis
# remain recorded in each analysis directory's provenance files.

project_library <- file.path(getwd(), "R-library")
dir.create(project_library, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(project_library, .libPaths()))

cran_packages <- c(
  "clubSandwich",
  "digest",
  "ggplot2",
  "ggrepel",
  "httr",
  "lmtest",
  "patchwork",
  "readxl",
  "refund",
  "r4pde",
  "rnaturalearth",
  "sf"
)

missing_cran <- cran_packages[!vapply(cran_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_cran)) {
  install.packages(
    missing_cran,
    lib = project_library,
    repos = "https://cloud.r-project.org",
    dependencies = c("Depends", "Imports", "LinkingTo")
  )
}

cat("Required R packages are available.\n")
