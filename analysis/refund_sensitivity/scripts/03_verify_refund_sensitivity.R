#!/usr/bin/env Rscript

# Verify the active primary-versus-penalized refund sensitivity workflow.
.libPaths(c(file.path(getwd(), "R-library"), .libPaths()))
source("analysis/R/v4_utils.R")
root <- file.path("analysis", "refund_sensitivity")
tab <- file.path(root, "outputs", "tables")
required <- c(
  file.path(tab, "refund_functional_curves.csv"),
  file.path(tab, "refund_interval_contrasts.csv"),
  file.path(tab, "refund_global_tests.csv"),
  file.path(tab, "refund_loso_curves.csv"),
  file.path(tab, "refund_loso_global_tests.csv"),
  file.path(tab, "global_and_curve_comparison.csv"),
  file.path(tab, "penalized_association_survival.csv"),
  file.path(root, "models", "refund_sensitivity_models.rds"),
  file.path(root, "config", "frozen_refund_specification.csv"),
  file.path(root, "outputs", "figures", "penalized_comparison", "figure1_penalized_curve_comparison.png"),
  file.path(root, "outputs", "figures", "penalized_comparison", "figure2_penalized_interval_comparison.png"),
  file.path(root, "outputs", "figures", "penalized_comparison", "figure3_penalized_loso.png"),
  file.path(root, "report", "refund_comparison_report.qmd"),
  file.path(root, "report", "refund_comparison_report.html")
)
if (!all(file.exists(required))) {
  stop("Missing penalized refund outputs: ", paste(required[!file.exists(required)], collapse = ", "))
}
curves <- utils::read.csv(required[1])
contrasts <- utils::read.csv(required[2])
global <- utils::read.csv(required[3])
loso_curves <- utils::read.csv(required[4])
loso_global <- utils::read.csv(required[5])
comparison <- utils::read.csv(required[6])
survival <- utils::read.csv(required[7])
qmd_text <- paste(readLines(required[13], warn = FALSE), collapse = "\n")
processes <- c("Tmin", "LogRain", "Tmax", "RH")
checks <- c(
  identical(sort(unique(curves$process)), sort(processes)),
  all(table(curves$process) == 80L),
  identical(range(curves$lag_day), c(-80L, -1L)),
  all(curves$bootstrap_reps >= 0.8 * 999),
  all(is.finite(curves$beta) & is.finite(curves$conf_low) & is.finite(curves$conf_high)),
  all(table(contrasts$process) == 8L),
  nrow(global) == 4L && all(global$converged),
  all(global$edf <= 4 + 1e-8 & global$edf >= 1),
  all(table(loso_curves$process) == 4L * 80L),
  all(table(loso_global$process) == 4L) && all(loso_global$converged),
  nrow(comparison) == 4L && all(is.finite(comparison$curve_correlation)),
  nrow(survival) == 4L,
  grepl("Log rainfall amount clearly survived penalization", qmd_text, fixed = TRUE),
  !grepl("Parameterization-matched refund sensitivity analysis", qmd_text, fixed = TRUE),
  file.info(required[14])$size > 10000
)
audit <- data.frame(
  check = c("four processes", "80 lags per process", "domain -80:-1",
            "bootstrap complete", "finite curves and bands", "eight contrasts",
            "four converged penalized fits", "valid penalized EDF",
            "four LOSO curves per process", "four converged LOSO fits",
            "four finite curve comparisons", "four survival classifications",
            "report states rainfall survival", "report excludes matched-only framing",
            "rendered HTML nonempty"),
  passed = checks
)
utils::write.csv(audit, file.path(root, "provenance", "verification_audit.csv"), row.names = FALSE)
if (!all(checks)) stop("Penalized refund sensitivity verification failed.")
manifest_files <- list.files(root, recursive = TRUE, full.names = TRUE)
manifest_files <- manifest_files[file.info(manifest_files)$isdir %in% FALSE]
manifest_files <- manifest_files[basename(manifest_files) != "manifest.csv"]
v4_write_csv(v4_manifest(manifest_files, root), file.path(root, "provenance", "manifest.csv"))
message("Penalized refund verification passed: ", sum(checks), "/", length(checks), " checks.")
