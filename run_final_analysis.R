#!/usr/bin/env Rscript

# One-command final workflow. Legacy modular scripts remain recoverable but are
# not executed and therefore cannot generate competing figure versions.
scripts <- c(
  "analysis/scripts/01_prepare_data_weather.R",
  "analysis/scripts/02_fit_consolidated_analysis.R",
  "analysis/scripts/03_render_figures.R",
  "analysis/scripts/04_verify_analysis.R"
)
for (script in scripts) {
  message("Running ", script)
  status <- system2(file.path(R.home("bin"), "Rscript.exe"), script)
  if (!identical(status, 0L)) stop("Final workflow stage failed: ", script)
}
message("Final corrected-date analysis, figures, and verification completed.")
