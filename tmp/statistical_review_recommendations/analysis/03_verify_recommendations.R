#!/usr/bin/env Rscript

.libPaths(c(file.path(getwd(), "R-library"), .libPaths()))
source("analysis/R/v4_utils.R")

root <- file.path("tmp", "statistical_review_recommendations")
tables <- file.path(root, "outputs", "tables")
figures <- file.path(root, "outputs", "figures")
models <- file.path(root, "outputs", "models")
report <- file.path(root, "report", "statistical_review_followup.html")

expected_tables <- c(
  "cohort_summary.csv", "cell_season_cluster_counts.csv",
  "cluster_robust_broad_coefficients.csv", "cluster_robust_broad_global_tests.csv",
  "cluster_robust_functional_global_tests.csv",
  "cell_season_bootstrap_functional_curves.csv",
  "cell_season_bootstrap_functional_contrasts.csv",
  "incremental_r2_permutation_null.csv",
  "leave_one_season_out_prediction_metrics.csv",
  "penalized_k10_global_tests.csv", "penalized_k10_functional_curves.csv",
  "within_process_interval_correlations.csv",
  "cross_process_correlations_by_interval.csv",
  "influence_diagnostics.csv", "quasibinomial_logit_cluster_robust_global_tests.csv",
  "two_process_cluster_robust_tests.csv", "recommendation_feasibility.csv"
)
expected_figures <- sprintf("figure%d_%s.png", 1:8,
  c("global_inference_comparison", "cluster_robust_broad_coefficients",
    "cell_season_bootstrap_functional_curves", "covariate_adjustment_sensitivity",
    "loso_predictions", "penalized_k10_curves", "cross_process_correlations",
    "lograin_influence"))

checks <- data.frame(check = character(), passed = logical(), detail = character())
add <- function(name, passed, detail = "") {
  checks <<- rbind(checks, data.frame(check = name, passed = isTRUE(passed), detail = detail))
}

add("Expected tables", all(file.exists(file.path(tables, expected_tables))),
    paste(sum(file.exists(file.path(tables, expected_tables))), "of", length(expected_tables)))
add("Expected figures", all(file.exists(file.path(figures, expected_figures))),
    paste(sum(file.exists(file.path(figures, expected_figures))), "of", length(expected_figures)))
add("Model cache", file.exists(file.path(models, "statistical_review_models.rds")))
add("Rendered HTML report", file.exists(report) && file.info(report)$size > 100000)

clusters <- read.csv(file.path(tables, "cell_season_cluster_counts.csv"))
add("36 cell-season clusters", length(unique(clusters$cell_season)) == 36L)
add("72 trials represented", sum(clusters$n_trials) == 72L)

bg <- subset(read.csv(file.path(tables, "cluster_robust_broad_global_tests.csv")),
             scenario == "Season only")
fg <- subset(read.csv(file.path(tables, "cluster_robust_functional_global_tests.csv")),
             scenario == "Season only")
add("Four clustered broad tests", nrow(bg) == 4L && all(is.finite(bg$p_value_CR2)))
add("Four clustered functional tests", nrow(fg) == 4L && all(is.finite(fg$p_value_CR2)))

fb <- read.csv(file.path(tables, "cell_season_bootstrap_functional_curves.csv"))
add("Functional cluster bootstrap complete",
    all(tapply(fb$successful_reps, fb$process, unique) == 999L))
pk <- read.csv(file.path(tables, "penalized_k10_functional_curves.csv"))
add("Penalized cluster bootstrap adequate",
    all(tapply(pk$successful_reps, pk$process, unique) >= 0.95 * 499L))

loso <- read.csv(file.path(tables, "leave_one_season_out_prediction_metrics.csv"))
add("Eight LOSO metrics", nrow(loso) == 8L && all(is.finite(loso$out_of_season_R2)))
perm <- read.csv(file.path(tables, "incremental_r2_permutation_null.csv"))
add("Eight permutation tests", nrow(perm) == 8L && all(perm$permutation_p > 0 & perm$permutation_p <= 1))

utils::write.csv(checks, file.path(root, "outputs", "verification_checks.csv"), row.names = FALSE)
if (!all(checks$passed)) stop("One or more recommendation-analysis checks failed.")

files <- list.files(root, recursive = TRUE, full.names = TRUE)
files <- files[!file.info(files)$isdir]
manifest_path <- file.path(root, "outputs", "output_manifest.csv")
files <- files[normalizePath(files, winslash = "/", mustWork = FALSE) !=
                 normalizePath(manifest_path, winslash = "/", mustWork = FALSE)]
manifest <- v4_manifest(files, root)
utils::write.csv(manifest, manifest_path, row.names = FALSE)
message("Statistical-review follow-up verification passed: ", nrow(checks), " checks.")
