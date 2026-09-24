#!/usr/bin/env Rscript

# Structural, semantic, and numerical audit for the consolidated workflow.
.libPaths(c(file.path(getwd(), "R-library"), .libPaths()))
source("analysis/R/consolidated_analysis_utils.R")
p <- v4_paths()
ft <- file.path(p$tables, "final")
ff <- file.path(p$freeze, "final")
fig_dir <- "figures"
checks <- list()
add <- function(name, pass, detail) {
  checks[[length(checks) + 1L]] <<- data.frame(check = name, pass = isTRUE(pass), detail = detail)
}

trials <- readRDS(file.path(p$processed, "trial_cohort_v4.rds"))
weather <- readRDS(file.path(p$processed, "weather_era5_absolute_days.rds"))
registry <- consolidated_registry()
analysis_days <- -80:-1
add("Cohort size", nrow(trials) == 72L, paste(nrow(trials), "trials"))
add("Meteorological units", length(unique(trials$met_unit)) >= 1L,
    paste(length(unique(trials$met_unit)), "units"))
add("Source date preserved", all(c("recorded_evaluation_date", "grain_evaluation_date") %in% names(trials)),
    paste(intersect(c("recorded_evaluation_date", "grain_evaluation_date"), names(trials)), collapse = "|"))
add("Twenty-day correction", all(trials$grain_evaluation_date == trials$recorded_evaluation_date + 20L),
    "grain_evaluation_date = recorded_evaluation_date + 20")
add("Corrected cycle duration", all(trials$cycle_days == as.integer(trials$grain_evaluation_date - trials$sowing_date)),
    "cycle_days uses corrected grain-evaluation date")
per_trial <- table(weather$trial_id)
add("Weather domain", nrow(weather) == 72L * length(analysis_days) && all(per_trial == length(analysis_days)) &&
      identical(range(weather$lag_day), range(analysis_days)) && !any(weather$lag_day == 0),
    paste(nrow(weather), "rows; lags", paste(range(weather$lag_day), collapse = ":")))

complete <- utils::read.csv(file.path(ft, "analysis_completeness_audit.csv"))
n_processes <- nrow(registry)
add("Retained-process coverage", nrow(complete) == n_processes * 2L * 3L && all(complete$complete),
    paste(sum(complete$complete), "of", nrow(complete), "families complete"))
add("Primary broad rows", nrow(utils::read.csv(file.path(ft, "primary_broad_coefficients.csv"))) == n_processes * 8L,
    paste(n_processes, "processes x 8 intervals"))
add("Primary functional rows", nrow(utils::read.csv(file.path(ft, "primary_functional_curves.csv"))) == n_processes * 80L,
    paste(n_processes, "processes x 80 lags"))

# The active audit is self-contained and does not depend on archived modular
# outputs. Verify that the cached object and exported tables cover the same
# registry and scenario structure.
models <- readRDS(file.path(p$models, "final", "consolidated_models.rds"))
model_processes <- sort(names(models$primary))
add("Cached-model process registry", identical(model_processes, sort(registry$process)),
    paste(length(model_processes), "primary process objects"))
add("Cached-model scenario coverage",
    length(models$sensitivities) == 3L && length(models$loso) == 4L &&
      all(vapply(models$sensitivities, length, integer(1)) == n_processes) &&
      all(vapply(models$loso, length, integer(1)) == n_processes),
    paste("3 sensitivities and 4 season omissions, each with", n_processes, "processes"))

expected_figures <- c(
  "figure1_trial_locations_map.png", "figure2_all_process_broad_intervals.png",
  "figure3_all_process_functional_curves.png", "figure4_all_process_shared_sensitivities.png",
  "figure5_all_process_loso_stability.png", "figure6_days_after_sowing.png",
  "figureS1_process_correlations.png", "figureS2_fit_and_stability.png",
  "figureS3_functional_coefficient_heatmap.png",
  "figureS4_broad_residual_vs_fitted.png", "figureS5_broad_normal_qq.png",
  "figureS6_functional_residual_vs_fitted.png", "figureS7_functional_normal_qq.png",
  "figure4b_functional_shared_sensitivities.png", "figure5b_functional_loso_stability.png",
  "figure8_ga_distributions.png", "figure3b_functional_pointwise_bootstrap.png"
)
actual_figures <- sort(list.files(fig_dir, pattern = "\\.png$"))
add("Final-only figure directory", identical(actual_figures, sort(expected_figures)),
    paste(length(actual_figures), "PNG files; expected", length(expected_figures)))
add("Final figure manifest", file.exists(file.path(ff, "final_figure_manifest.csv")),
    "final figure SHA-256 manifest exists")

out <- consolidated_bind(checks)
v4_write_csv(out, file.path(p$diagnostics, "final", "consolidated_verification.csv"))
if (!all(out$pass)) {
  print(out)
  stop("Consolidated verification failed.")
}
cat("Consolidated verification passed:", nrow(out), "checks.\n")
