#!/usr/bin/env Rscript

# Parameterization-matched refund sensitivity. This uses the same unpenalized
# four-dimensional cubic spline space as the frozen primary implementation.
.libPaths(c(file.path(getwd(), "R-library"), .libPaths()))
if (!requireNamespace("refund", quietly = TRUE)) stop("Package 'refund' is required.")
source("analysis/R/consolidated_analysis_utils.R")

root <- file.path("analysis", "refund_sensitivity")
table_dir <- file.path(root, "outputs", "tables")
model_dir <- file.path(root, "models")
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(model_dir, recursive = TRUE, showWarnings = FALSE)

p <- v4_init_dirs()
days <- -80:-1
registry <- consolidated_registry()
trials <- readRDS(file.path(p$processed, "trial_cohort_v4.rds"))
weather <- consolidated_add_processes(
  readRDS(file.path(p$processed, "weather_era5_absolute_days.rds"))
)
primary_curves <- utils::read.csv(
  file.path(p$tables, "final", "primary_functional_curves.csv"), check.names = FALSE
)
primary_contrasts <- utils::read.csv(
  file.path(p$tables, "final", "primary_functional_contrasts.csv"), check.names = FALSE
)
primary_global <- utils::read.csv(
  file.path(p$tables, "final", "primary_functional_global_tests.csv"), check.names = FALSE
)

fit_matched <- function(trials, mat, days, center = NULL) {
  if (is.null(center)) center <- colMeans(mat)
  Xc <- sweep(mat, 2, center, "-")
  dat <- data.frame(outcome = trials$ga, season = factor(trials$season))
  dat$X <- I(Xc)
  lf <- refund::lf
  fit <- refund::pfr(
    outcome ~ season + lf(
      X, argvals = days, integration = "riemann", bs = "ps", k = 4,
      m = 2, fx = TRUE
    ),
    data = dat, fitter = "gam", method = "REML", family = stats::gaussian()
  )
  cf <- stats::coef(fit, n = length(days))
  lag_col <- grep("\\.argvals$", names(cf), value = TRUE)
  if (length(lag_col) != 1L) stop("Unexpected refund coefficient output.")
  ord <- match(days, cf[[lag_col]])
  sm <- summary(fit)
  st <- as.data.frame(sm$s.table)
  p_col <- grep("p", names(st), ignore.case = TRUE, value = TRUE)[1]
  list(
    fit = fit, beta = as.numeric(cf$value[ord]), model_se = as.numeric(cf$se[ord]),
    center = center, edf = unname(st[1, "edf"]), ref_df = unname(st[1, "Ref.df"]),
    f_statistic = unname(st[1, "F"]), p_value = unname(st[1, p_col]),
    r_squared = 1 - sum(stats::residuals(fit)^2) /
      sum((trials$ga - mean(trials$ga))^2),
    residual_sd = sqrt(unname(sm$scale)), converged = isTRUE(fit$converged)
  )
}

bootstrap_matched <- function(trials, mat, fit_obj, days, reps, seed) {
  set.seed(seed)
  units <- unique(trials[c("weather_cluster_id", "season")])
  if (anyDuplicated(units$weather_cluster_id)) stop("Weather clusters cross seasons.")
  strata <- split(units$weather_cluster_id, units$season)
  curves <- matrix(NA_real_, reps, length(days))
  imap <- v4_interval_map(days)
  contrasts <- matrix(NA_real_, reps, nlevels(imap),
                      dimnames = list(NULL, levels(imap)))
  failures <- character()
  for (b in seq_len(reps)) {
    sampled <- unlist(lapply(strata, function(u) sample(u, length(u), replace = TRUE)),
                      use.names = FALSE)
    idx <- unlist(lapply(sampled, function(u) which(trials$weather_cluster_id == u)),
                  use.names = FALSE)
    tryCatch({
      fb <- fit_matched(trials[idx, , drop = FALSE], mat[idx, , drop = FALSE],
                        days, fit_obj$center)
      curves[b, ] <- fb$beta
      contrasts[b, ] <- vapply(levels(imap), function(z) sum(fb$beta[imap == z]),
                               numeric(1))
    }, error = function(e) failures <<- c(failures, conditionMessage(e)))
    if (b %% 100L == 0L) message("  matched bootstrap ", b, "/", reps)
  }
  keep <- stats::complete.cases(curves)
  if (sum(keep) < max(100L, floor(0.8 * reps))) stop("Too many bootstrap failures.")
  curves <- curves[keep, , drop = FALSE]
  contrasts <- contrasts[keep, , drop = FALSE]
  se <- apply(curves, 2, stats::sd)
  zmax <- apply(abs(sweep(curves, 2, fit_obj$beta, "-") /
                      matrix(se, nrow(curves), length(se), byrow = TRUE)),
                1, max, na.rm = TRUE)
  crit <- unname(stats::quantile(zmax[is.finite(zmax)], 0.95, type = 8))
  interval_hat <- vapply(levels(imap), function(z) sum(fit_obj$beta[imap == z]),
                         numeric(1))
  list(
    curves = curves, contrasts = contrasts,
    pointwise_low = apply(curves, 2, stats::quantile, 0.025, type = 8),
    pointwise_high = apply(curves, 2, stats::quantile, 0.975, type = 8),
    simultaneous_critical = crit,
    simultaneous_low = fit_obj$beta - crit * se,
    simultaneous_high = fit_obj$beta + crit * se,
    interval_summary = data.frame(
      interval = levels(imap), estimate = interval_hat,
      conf_low = apply(contrasts, 2, stats::quantile, 0.025, type = 8),
      conf_high = apply(contrasts, 2, stats::quantile, 0.975, type = 8)
    ),
    successful_reps = nrow(curves), requested_reps = reps,
    failure_messages = unique(failures)
  )
}

curves_out <- contrasts_out <- globals_out <- comparisons_out <- list()
loso_curves_out <- loso_global_out <- objects <- list()
reps <- 999L
seed_base <- 24041987L # identical to the frozen primary bootstrap seeds

for (i in seq_len(nrow(registry))) {
  meta <- registry[i, ]
  proc <- meta$process
  message("Fitting parameterization-matched refund model for ", proc)
  mat <- v4_process_matrix(weather, trials, proc, days)
  ff <- fit_matched(trials, mat, days)
  fb <- bootstrap_matched(trials, mat, ff, days, reps,
                          seed_base + meta$seed_offset)
  process_sd <- stats::sd(as.vector(mat))
  curve <- data.frame(
    process = proc, process_label = meta$label, lag_day = days,
    beta = ff$beta, model_se = ff$model_se,
    pointwise_conf_low = fb$pointwise_low,
    pointwise_conf_high = fb$pointwise_high,
    conf_low = fb$simultaneous_low, conf_high = fb$simultaneous_high,
    beta_per_process_sd = ff$beta * process_sd,
    conf_low_per_process_sd = fb$simultaneous_low * process_sd,
    conf_high_per_process_sd = fb$simultaneous_high * process_sd,
    bootstrap_reps = fb$successful_reps
  )
  cont <- fb$interval_summary
  cont$process <- proc
  cont$process_label <- meta$label
  reduced <- stats::lm(trials$ga ~ factor(trials$season))
  global <- data.frame(
    process = proc, process_label = meta$label, n = nrow(trials),
    edf = ff$edf, ref_df = ff$ref_df, f_statistic = ff$f_statistic,
    p_value = ff$p_value, r_squared = ff$r_squared,
    incremental_r_squared = ff$r_squared - summary(reduced)$r.squared,
    residual_sd = ff$residual_sd, converged = ff$converged,
    method = "refund::pfr; ps k=4 m=2; fx=TRUE"
  )
  pc <- primary_curves[primary_curves$process == proc, ]
  pc <- pc[match(days, pc$lag_day), ]
  primary_obj <- v4_fit_functional(trials, mat, days, "ga", "season", 4L)
  comparisons_out[[proc]] <- data.frame(
    process = proc,
    curve_correlation = stats::cor(pc$beta, ff$beta),
    max_abs_beta_difference = max(abs(pc$beta - ff$beta)),
    max_abs_fitted_difference = max(abs(stats::fitted(primary_obj$full) -
                                        stats::fitted(ff$fit))),
    max_abs_simultaneous_low_difference = max(abs(pc$conf_low - curve$conf_low)),
    max_abs_simultaneous_high_difference = max(abs(pc$conf_high - curve$conf_high))
  )

  proc_loso_curves <- proc_loso_global <- list()
  for (j in seq_along(unique(trials$season))) {
    omitted <- unique(trials$season)[j]
    use <- trials$season != omitted
    lf <- fit_matched(trials[use, , drop = FALSE], mat[use, , drop = FALSE], days)
    proc_loso_curves[[j]] <- data.frame(
      process = proc, process_label = meta$label, omitted_season = omitted,
      lag_day = days, beta = lf$beta, beta_per_process_sd = lf$beta * process_sd
    )
    proc_loso_global[[j]] <- data.frame(
      process = proc, process_label = meta$label, omitted_season = omitted,
      n = sum(use), edf = lf$edf, f_statistic = lf$f_statistic,
      p_value = lf$p_value, converged = lf$converged
    )
  }
  curves_out[[proc]] <- curve
  contrasts_out[[proc]] <- cont
  globals_out[[proc]] <- global
  loso_curves_out[[proc]] <- do.call(rbind, proc_loso_curves)
  loso_global_out[[proc]] <- do.call(rbind, proc_loso_global)
  objects[[proc]] <- list(fit = ff$fit, center = ff$center, bootstrap = fb)
}

bind <- function(x) { z <- do.call(rbind, x); rownames(z) <- NULL; z }
matched_curves <- bind(curves_out)
matched_contrasts <- bind(contrasts_out)
matched_global <- bind(globals_out)
matched_comparison <- bind(comparisons_out)
matched_loso_curves <- bind(loso_curves_out)
matched_loso_global <- bind(loso_global_out)

pg <- primary_global[c("process", "p_value", "incremental_r_squared")]
names(pg)[2:3] <- c("primary_p_value", "primary_incremental_r_squared")
mg <- matched_global[c("process", "p_value", "incremental_r_squared", "edf")]
names(mg)[2:4] <- c("matched_p_value", "matched_incremental_r_squared", "matched_edf")
matched_comparison <- merge(merge(pg, mg, by = "process"), matched_comparison,
                            by = "process")

v4_write_csv(matched_curves, file.path(table_dir, "matched_refund_functional_curves.csv"))
v4_write_csv(matched_contrasts, file.path(table_dir, "matched_refund_interval_contrasts.csv"))
v4_write_csv(matched_global, file.path(table_dir, "matched_refund_global_tests.csv"))
v4_write_csv(matched_loso_curves, file.path(table_dir, "matched_refund_loso_curves.csv"))
v4_write_csv(matched_loso_global, file.path(table_dir, "matched_refund_loso_global_tests.csv"))
v4_write_csv(matched_comparison, file.path(table_dir, "matched_refund_comparison.csv"))
saveRDS(objects, file.path(model_dir, "matched_refund_models.rds"))

spec <- data.frame(
  item = c("analysis_role", "domain", "functional_term", "basis",
           "basis_dimension", "penalization", "integration", "adjustment",
           "bootstrap", "bootstrap_reps", "seed_base", "refund_version"),
  value = c("parameterization-matched software sensitivity", "-80:-1",
            "refund::lf() in refund::pfr()", "cubic P-spline", "4",
            "none (fx=TRUE)", "Riemann", "season fixed effect",
            "season-stratified ERA5 cell-by-season cluster bootstrap", reps,
            seed_base, as.character(utils::packageVersion("refund")))
)
v4_write_csv(spec, file.path(root, "config", "matched_refund_specification.csv"))
message("Parameterization-matched refund analysis completed.")
