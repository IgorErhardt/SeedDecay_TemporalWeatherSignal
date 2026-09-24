#!/usr/bin/env Rscript

# Single statistical engine for every retained process.
# This script writes tables/models only; final figures have one separate owner.
.libPaths(c(file.path(getwd(), "R-library"), .libPaths()))
source("analysis/R/consolidated_analysis_utils.R")
p <- v4_init_dirs()
v4_assert_freeze(file.path(p$freeze, "preanalysis_freeze.csv"))

final_tables <- file.path(p$tables, "final")
final_models <- file.path(p$models, "final")
final_diagnostics <- file.path(p$diagnostics, "final")
final_freeze <- file.path(p$freeze, "final")
invisible(lapply(c(final_tables, final_models, final_diagnostics, final_freeze),
                 dir.create, recursive = TRUE, showWarnings = FALSE))

registry <- consolidated_registry()
analysis_days <- -80:-1
trials <- readRDS(file.path(p$processed, "trial_cohort_v4.rds"))
weather <- consolidated_add_processes(readRDS(file.path(p$processed, "weather_era5_absolute_days.rds")))
unit_trials <- readRDS(file.path(p$processed, "meteorological_unit_cohort.rds"))
unit_weather <- consolidated_add_processes(readRDS(file.path(p$processed, "meteorological_unit_weather.rds")))
# Outcome-only influence flags are defined before any outcome-weather fit.
q <- stats::quantile(trials$ga, c(0.25, 0.75), type = 8)
fence <- c(q[1] - 1.5 * diff(q), q[2] + 1.5 * diff(q))
trials$outcome_influence_flag <- trials$ga < fence[1] | trials$ga > fence[2]
saveRDS(trials, file.path(p$processed, "trial_cohort_v4_with_outcome_flags.rds"))
v4_write_csv(trials, file.path(p$processed, "trial_cohort_v4_with_outcome_flags.csv"))
trials$log_ga <- log1p(trials$ga)
unit_trials$log_ga <- log1p(unit_trials$ga)
flagged_ids <- trials$trial_id[trials$outcome_influence_flag]
unit_trials$outcome_influence_flag <- vapply(strsplit(unit_trials$member_trials, "\\|"),
                                             function(x) any(x %in% flagged_ids), logical(1))

stopifnot(nrow(trials) == 72L, nrow(unit_trials) == length(unique(trials$met_unit)),
          all(table(weather$trial_id) == length(analysis_days)),
          identical(range(weather$lag_day), range(analysis_days)),
          all(trials$evaluation_date == trials$recorded_evaluation_date + 20L),
          all(trials$evaluation_date == trials$grain_evaluation_date),
          all(trials$cycle_days == as.integer(trials$grain_evaluation_date - trials$sowing_date)))
if (!all(registry$process %in% names(weather))) stop("Registry/weather mismatch.")

spec <- data.frame(
  item = c("analysis_anchor", "source_date_correction_days", "primary_domain",
           "broad_intervals", "functional_basis",
           "functional_df", "primary_bootstrap_reps", "sensitivity_bootstrap_reps",
           "bootstrap_unit", "bootstrap_stratification", "adjustment",
           "shared_sensitivities", "processes", "primary_seed", "sensitivity_seed"),
  value = c("grain evaluation date", "20", "-80:-1",
            paste(levels(v4_interval_map(analysis_days)), collapse = "|"),
            "cubic B-spline", "4", "999", "499",
            "meteorological exposure unit cluster", "season", "season fixed effect",
            "log1p(GA)|meteorological-unit aggregation|exclude outcome-only high GA|leave one season out",
            paste(registry$process, collapse = "|"), "24041987", "24044001")
)
v4_write_csv(spec, file.path(p$config, "consolidated_analysis_specification.csv"))
v4_write_csv(registry, file.path(final_tables, "process_registry.csv"))

message("Fitting consolidated primary models...")
primary <- consolidated_tag(consolidated_fit_set(
  trials, weather, registry, days = analysis_days, response = "ga",
  bootstrap_reps = 999L, seed_base = 24041987L), "Primary")

message("Fitting shared sensitivity models...")
keep <- !trials$outcome_influence_flag
sensitivities <- list(
  consolidated_tag(consolidated_fit_set(trials, weather, registry, analysis_days, "log_ga",
    bootstrap_reps = 499L, seed_base = 24044101L), "log1p(GA)"),
  consolidated_tag(consolidated_fit_set(unit_trials, unit_weather, registry, analysis_days, "ga",
    bootstrap_reps = 499L, seed_base = 24044301L), "Meteorological units"),
  consolidated_tag(consolidated_fit_set(trials[keep, ],
    weather[weather$trial_id %in% trials$trial_id[keep], ], registry, analysis_days, "ga",
    bootstrap_reps = 499L, seed_base = 24044401L), "Exclude high GA")
)

message("Fitting leave-one-season-out stress tests...")
loso <- lapply(seq_along(unique(trials$season)), function(j) {
  omitted <- unique(trials$season)[j]
  use <- trials$season != omitted
  consolidated_tag(consolidated_fit_set(
    trials[use, ], weather[weather$trial_id %in% trials$trial_id[use], ], registry,
    analysis_days, "ga", bootstrap = FALSE, bootstrap_reps = 0L,
    seed_base = 24044501L + j * 20L), paste("Omit", omitted))
})
names(loso) <- paste("Omit", unique(trials$season))

write_component <- function(x, prefix) {
  v4_write_csv(x$broad_coefficients, file.path(final_tables, paste0(prefix, "_broad_coefficients.csv")))
  v4_write_csv(x$broad_global, file.path(final_tables, paste0(prefix, "_broad_global_tests.csv")))
  v4_write_csv(x$functional_curves, file.path(final_tables, paste0(prefix, "_functional_curves.csv")))
  v4_write_csv(x$functional_contrasts, file.path(final_tables, paste0(prefix, "_functional_contrasts.csv")))
  v4_write_csv(x$functional_global, file.path(final_tables, paste0(prefix, "_functional_global_tests.csv")))
}
write_component(primary, "primary")
sens <- list(
  broad_coefficients = consolidated_bind_component(sensitivities, "broad_coefficients"),
  broad_global = consolidated_bind_component(sensitivities, "broad_global"),
  functional_curves = consolidated_bind_component(sensitivities, "functional_curves"),
  functional_contrasts = consolidated_bind_component(sensitivities, "functional_contrasts"),
  functional_global = consolidated_bind_component(sensitivities, "functional_global")
)
write_component(sens, "sensitivity")
loso_out <- list(
  broad_coefficients = consolidated_bind_component(loso, "broad_coefficients"),
  broad_global = consolidated_bind_component(loso, "broad_global"),
  functional_curves = consolidated_bind_component(loso, "functional_curves"),
  functional_contrasts = consolidated_bind_component(loso, "functional_contrasts"),
  functional_global = consolidated_bind_component(loso, "functional_global")
)
write_component(loso_out, "loso")
saveRDS(list(primary = primary$objects,
             sensitivities = lapply(sensitivities, `[[`, "objects"),
             loso = lapply(loso, `[[`, "objects")),
        file.path(final_models, "consolidated_models.rds"))

# Residual records are generated from the same final primary objects.
residual_rows <- list()
adequacy <- list()
for (proc in registry$process) {
  obj <- primary$objects[[proc]]
  for (method in c("Broad intervals", "Scalar on function")) {
    fit <- if (method == "Broad intervals") obj$broad_standardized else obj$functional$full
    dat <- data.frame(process = proc, method = method, trial_id = trials$trial_id,
                      fitted = stats::fitted(fit), residual = stats::residuals(fit),
                      standardized_residual = stats::rstandard(fit),
                      leverage = stats::hatvalues(fit), cooks_distance = stats::cooks.distance(fit))
    residual_rows[[paste(proc, method)]] <- dat
    sw <- if (nrow(dat) >= 3L && nrow(dat) <= 5000L) stats::shapiro.test(dat$residual) else NULL
    bp <- if (requireNamespace("lmtest", quietly = TRUE)) lmtest::bptest(fit) else NULL
    adequacy[[paste(proc, method)]] <- data.frame(
      process = proc, method = method, n = stats::nobs(fit), residual_df = stats::df.residual(fit),
      shapiro_w = if (is.null(sw)) NA_real_ else unname(sw$statistic),
      shapiro_p = if (is.null(sw)) NA_real_ else sw$p.value,
      breusch_pagan_statistic = if (is.null(bp)) NA_real_ else unname(bp$statistic),
      breusch_pagan_p = if (is.null(bp)) NA_real_ else bp$p.value,
      max_abs_standardized_residual = max(abs(dat$standardized_residual)),
      max_cooks_distance = max(dat$cooks_distance))
  }
}
v4_write_csv(consolidated_bind(residual_rows), file.path(final_diagnostics, "primary_model_residuals.csv"))
v4_write_csv(consolidated_bind(adequacy), file.path(final_diagnostics, "primary_model_adequacy.csv"))

# Cross-analysis support is descriptive and not a multiple-testing decision rule.
support <- consolidated_bind(lapply(registry$process, function(proc) {
  for_method <- function(global_primary, global_sens, global_loso, method) {
    p0 <- global_primary$p_value[global_primary$process == proc]
    ps <- global_sens$p_value[global_sens$process == proc]
    pl <- global_loso$p_value[global_loso$process == proc]
    data.frame(process = proc, method = method, primary_p = p0,
               sensitivity_support_fraction_p_le_0_10 = mean(ps <= 0.10),
               loso_support_fraction_p_le_0_10 = mean(pl <= 0.10),
               sensitivity_fits = length(ps), loso_fits = length(pl))
  }
  rbind(for_method(primary$broad_global, sens$broad_global, loso_out$broad_global, "Broad intervals"),
        for_method(primary$functional_global, sens$functional_global, loso_out$functional_global, "Scalar on function"))
}))
v4_write_csv(support, file.path(final_tables, "process_support_summary.csv"))

expected <- expand.grid(process = registry$process,
                        method = c("Broad intervals", "Scalar on function"),
                        family = c("Primary", "Shared sensitivities", "Season omissions"),
                        stringsAsFactors = FALSE)
expected$expected_fits <- ifelse(
  expected$family == "Primary", 1L,
  ifelse(expected$family == "Shared sensitivities", 3L, 4L)
)
expected$observed_fits <- mapply(function(proc, method, family) {
  tab <- if (method == "Broad intervals") {
    if (family == "Primary") primary$broad_global else if (family == "Shared sensitivities") sens$broad_global else loso_out$broad_global
  } else {
    if (family == "Primary") primary$functional_global else if (family == "Shared sensitivities") sens$functional_global else loso_out$functional_global
  }
  sum(tab$process == proc)
}, expected$process, expected$method, expected$family)
expected$complete <- expected$observed_fits == expected$expected_fits
v4_write_csv(expected, file.path(final_tables, "analysis_completeness_audit.csv"))
if (!all(expected$complete)) stop("Consolidated process coverage is incomplete.")

final_files <- c(list.files(final_tables, full.names = TRUE),
                 list.files(final_models, full.names = TRUE),
                 list.files(final_diagnostics, full.names = TRUE),
                 file.path(p$config, c("process_registry.csv", "consolidated_analysis_specification.csv")))
v4_write_csv(v4_manifest(final_files, p$root), file.path(final_freeze, "consolidated_results_manifest.csv"))
capture.output(sessionInfo(), file = file.path(p$provenance, "sessionInfo_consolidated_analysis.txt"))
message("Consolidated analysis completed: ", nrow(registry),
        " retained processes, one engine, no figures generated.")
