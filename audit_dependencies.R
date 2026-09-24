#!/usr/bin/env Rscript

# Record the dependency closure for the current analysis. This is intentionally
# separate from model fitting and does not modify any analysis object. It uses
# the active R libraries so the project does not require a machine-specific
# package-library directory to be committed.
project_library <- file.path(getwd(), "R-library")
if (dir.exists(project_library)) .libPaths(c(project_library, .libPaths()))

roots <- c(
  "digest", "ggplot2", "ggrepel", "lmtest", "patchwork", "readxl",
  "r4pde", "rnaturalearth", "sf"
)
installed <- as.data.frame(installed.packages(), stringsAsFactors = FALSE)
installed <- installed[!duplicated(installed$Package), , drop = FALSE]
dependency_map <- tools::package_dependencies(
  packages = roots,
  db = installed,
  which = c("Depends", "Imports", "LinkingTo"),
  recursive = TRUE
)
needed <- sort(unique(c(roots, unlist(dependency_map, use.names = FALSE))))
needed <- intersect(needed, installed$Package)

manifest <- installed[match(needed, installed$Package), c("Package", "Version")]
manifest$role <- ifelse(manifest$Package %in% roots, "direct", "dependency")
utils::write.csv(manifest, "required_package_manifest.csv", row.names = FALSE)

cat(length(needed), "packages recorded for the reproducible workflow.\n")
