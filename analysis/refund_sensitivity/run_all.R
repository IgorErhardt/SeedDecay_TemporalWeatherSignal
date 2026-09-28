#!/usr/bin/env Rscript

.libPaths(c(file.path(getwd(), "R-library"), .libPaths()))

scripts <- c(
  "analysis/refund_sensitivity/scripts/01_fit_refund_sensitivity.R",
  "analysis/refund_sensitivity/scripts/02_build_comparison_report.R",
  "analysis/refund_sensitivity/scripts/03_verify_refund_sensitivity.R"
)

for (script in scripts) {
  message("Running ", script)
  status <- system2(file.path(R.home("bin"), "Rscript.exe"), c("--vanilla", shQuote(script)))
  if (!identical(status, 0L)) stop("Sensitivity workflow failed at ", script)
}

message("refund sensitivity workflow completed.")
