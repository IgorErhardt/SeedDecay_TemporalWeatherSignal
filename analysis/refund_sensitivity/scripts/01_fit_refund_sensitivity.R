#!/usr/bin/env Rscript

# Independent sensitivity implementation using refund::pfr(). Primary model
# objects and tables are read-only and are never overwritten by this script.
.libPaths(c(file.path(getwd(), "R-library"), .libPaths()))
if (!requireNamespace("refund", quietly = TRUE)) {
  stop("Package 'refund' is required. Run install_dependencies.R first.")
}

source("analysis/R/consolidated_analysis_utils.R")

root <- file.path("analysis", "refund_sensitivity")
dirs <- file.path(root, c("config", "models", "outputs/tables", "outputs/figures",
                          "provenance", "report"))
invisible(lapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE))
table_dir <- file.path(root, "outputs", "tables")
model_dir <- file.path(root, "models")
prov_dir <- file.path(root, "provenance")

p <- v4_init_dirs()
days <- -80:-1
registry <- consolidated_registry()
trials <- readRDS(file.path(p$processed, "trial_cohort_v4.rds"))
weather <- consolidated_add_processes(
  readRDS(file.path(p$processed, "weather_era5_absolute_days.rds"))
)
primary_curves <- utils::read.csv(
  file.path(p$tables, "final", "primary_functional_curves.csv"),
  check.names = FALSE
)
primary_contrasts <- utils::read.csv(
  file.path(p$tables, "final", "primary_functional_contrasts.csv"),
  check.names = FALSE
)
primary_global <- utils::read.csv(
  file.path(p$tables, "final", "primary_functional_global_tests.csv"),
  check.names = FALSE
)

stopifnot(
  nrow(trials) == 72L,
  identical(range(days), c(-80L, -1L)),
  all(registry$process %in% names(weather)),
  all(registry$process %in% unique(primary_curves$process))
)

fit_refund <- function(trials, mat, days, response = "ga", center = NULL) {
  if (is.null(center)) center <- colMeans(mat)
  Xc <- sweep(mat, 2, center, "-")
  dat <- data.frame(
    outcome = trials[[response]],
    season = factor(trials$season)
  )
  dat$X <- I(Xc)
  lf <- refund::lf
  fit <- refund::pfr(
    outcome ~ season + lf(
      X, argvals = days, integration = "riemann", bs = "ps", k = 4, m = 2
    ),
    data = dat, fitter = "gam", method = "REML", family = stats::gaussian()
  )
  cf <- stats::coef(fit, n = length(days))
  if (!all(c("value", "se") %in% names(cf))) {
    stop("Unexpected coefficient output from refund::coef.pfr().")
  }
  lag_col <- grep("\\.argvals$", names(cf), value = TRUE)
  if (length(lag_col) != 1L) stop("Could not identify refund coefficient lag column.")
  ord <- match(days, cf[[lag_col]])
  if (anyNA(ord)) stop("refund coefficient grid does not match the frozen lag domain.")
  sm <- summary(fit)
  st <- as.data.frame(sm$s.table)
  p_col <- grep("p-value", names(st), fixed = TRUE, value = TRUE)
  if (!length(p_col)) p_col <- grep("p", names(st), ignore.case = TRUE, value = TRUE)[1]
  list(
    fit = fit,
    beta = as.numeric(cf$value[ord]),
    model_se = as.numeric(cf$se[ord]),
    center = center,
    edf = unname(st[1, "edf"]),
    ref_df = unname(st[1, "Ref.df"]),
    f_statistic = unname(st[1, "F"]),
    p_value = unname(st[1, p_col]),
    r_squared = unname(sm$r.sq),
    adjusted_r_squared = unname(sm$r.sq),
    deviance_explained = unname(sm$dev.expl),
    residual_sd = sqrt(unname(sm$scale)),
    converged = isTRUE(fit$converged)
  )
}

bootstrap_refund <- function(trials, mat, fit_obj, days, reps, seed,
                             unit_col = "met_unit", strata_col = "season") {
  set.seed(seed)
  units <- unique(trials[c(unit_col, strata_col)])
  if (anyDuplicated(units[[unit_col]])) {
    stop("Meteorological units cross seasons; stratified cluster bootstrap is invalid.")
  }
  strata <- split(units[[unit_col]], units[[strata_col]])
  curves <- matrix(NA_real_, reps, length(days))
  imap <- v4_interval_map(days)
  contrasts <- matrix(NA_real_, reps, nlevels(imap),
                      dimnames = list(NULL, levels(imap)))
  failures <- character()
  for (b in seq_len(reps)) {
    sampled <- unlist(lapply(strata, function(u) sample(u, length(u), replace = TRUE)),
                      use.names = FALSE)
    idx <- unlist(lapply(sampled, function(u) which(trials[[unit_col]] == u)),
                  use.names = FALSE)
    ok <- tryCatch({
      fb <- fit_refund(trials[idx, , drop = FALSE], mat[idx, , drop = FALSE],
                       days, center = fit_obj$center)
      curves[b, ] <- fb$beta
      contrasts[b, ] <- vapply(levels(imap), function(z) sum(fb$beta[imap == z]),
                               numeric(1))
      TRUE
    }, error = function(e) {
      failures <<- c(failures, conditionMessage(e))
      FALSE
    })
    if (b %% 100L == 0L) message("  bootstrap ", b, "/", reps)
  }
  keep <- stats::complete.cases(curves)
  if (sum(keep) < max(100L, floor(0.8 * reps))) {
    stop("Too many refund bootstrap failures: ", sum(!keep))
  }
  curves <- curves[keep, , drop = FALSE]
  contrasts <- contrasts[keep, , drop = FALSE]
  se <- apply(curves, 2, stats::sd)
  safe_se <- ifelse(se > 0, se, NA_real_)
  zmax <- apply(
    abs(sweep(curves, 2, fit_obj$beta, "-") /
          matrix(safe_se, nrow(curves), length(safe_se), byrow = TRUE)),
    1, max, na.rm = TRUE
  )
  crit <- unname(stats::quantile(zmax[is.finite(zmax)], 0.95, type = 8))
  interval_hat <- vapply(levels(imap), function(z) sum(fit_obj$beta[imap == z]),
                         numeric(1))
  list(
    curves = curves,
    contrasts = contrasts,
    pointwise_se = se,
    pointwise_low = apply(curves, 2, stats::quantile, 0.025, type = 8, na.rm = TRUE),
    pointwise_high = apply(curves, 2, stats::quantile, 0.975, type = 8, na.rm = TRUE),
    simultaneous_critical = crit,
    simultaneous_low = fit_obj$beta - crit * se,
    simultaneous_high = fit_obj$beta + crit * se,
    interval_summary = data.frame(
      interval = levels(imap),
      estimate = interval_hat,
      conf_low = apply(contrasts, 2, stats::quantile, 0.025, type = 8, na.rm = TRUE),
      conf_high = apply(contrasts, 2, stats::quantile, 0.975, type = 8, na.rm = TRUE)
    ),
    requested_reps = reps,
    successful_reps = nrow(curves),
    failure_messages = unique(failures)
  )
}

curves_out <- contrasts_out <- globals_out <- agreement_out <- list()
loso_curves_out <- loso_global_out <- list()
objects <- list()
bootstrap_reps <- 999L
seed_base <- 25092026L

for (i in seq_len(nrow(registry))) {
  meta <- registry[i, ]
  proc <- meta$process
  message("Fitting refund sensitivity for ", proc)
  mat <- v4_process_matrix(weather, trials, proc, days)
  ff <- fit_refund(trials, mat, days)
  fb <- bootstrap_refund(trials, mat, ff, days, bootstrap_reps,
                         seed_base + meta$seed_offset)
  process_sd <- stats::sd(as.vector(mat))
  curve <- data.frame(
    process = proc, process_label = meta$label, lag_day = days,
    beta = ff$beta, model_se = ff$model_se,
    pointwise_conf_low = fb$pointwise_low,
    pointwise_conf_high = fb$pointwise_high,
    conf_low = fb$simultaneous_low,
    conf_high = fb$simultaneous_high,
    beta_per_process_sd = ff$beta * process_sd,
    conf_low_per_process_sd = fb$simultaneous_low * process_sd,
    conf_high_per_process_sd = fb$simultaneous_high * process_sd,
    bootstrap_reps = fb$successful_reps
  )
  cont <- fb$interval_summary
  cont$process <- proc
  cont$process_label <- meta$label
  cont$estimate_per_process_sd <- cont$estimate * process_sd
  cont$conf_low_per_process_sd <- cont$conf_low * process_sd
  cont$conf_high_per_process_sd <- cont$conf_high * process_sd
  reduced <- stats::lm(trials$ga ~ factor(trials$season))
  full_r2 <- 1 - sum(stats::residuals(ff$fit)^2) /
    sum((trials$ga - mean(trials$ga))^2)
  global <- data.frame(
    process = proc, process_label = meta$label, n = nrow(trials),
    edf = ff$edf, ref_df = ff$ref_df, f_statistic = ff$f_statistic,
    p_value = ff$p_value, r_squared = full_r2,
    incremental_r_squared = full_r2 - summary(reduced)$r.squared,
    deviance_explained = ff$deviance_explained,
    residual_sd = ff$residual_sd, converged = ff$converged,
    method = "refund::pfr; ps k=4 m=2; REML"
  )
  pc <- primary_curves[primary_curves$process == proc, ]
  pc <- pc[match(days, pc$lag_day), ]
  agreement <- data.frame(
    process = proc,
    curve_correlation = stats::cor(pc$beta, ff$beta),
    curve_rmse = sqrt(mean((pc$beta - ff$beta)^2)),
    sign_agreement_fraction = mean(sign(pc$beta) == sign(ff$beta)),
    primary_nonzero_days_simultaneous = sum(pc$conf_low > 0 | pc$conf_high < 0),
    refund_nonzero_days_simultaneous = sum(curve$conf_low > 0 | curve$conf_high < 0)
  )

  seasons <- unique(trials$season)
  proc_loso_curves <- proc_loso_global <- list()
  for (j in seq_along(seasons)) {
    omitted <- seasons[j]
    use <- trials$season != omitted
    lf <- fit_refund(
      trials[use, , drop = FALSE], mat[use, , drop = FALSE], days
    )
    proc_loso_curves[[j]] <- data.frame(
      process = proc, process_label = meta$label, omitted_season = omitted,
      lag_day = days, beta = lf$beta,
      beta_per_process_sd = lf$beta * process_sd
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
  agreement_out[[proc]] <- agreement
  loso_curves_out[[proc]] <- do.call(rbind, proc_loso_curves)
  loso_global_out[[proc]] <- do.call(rbind, proc_loso_global)
  objects[[proc]] <- list(fit = ff$fit, center = ff$center, bootstrap = fb,
                          process_sd = process_sd)
}

bind <- function(x) { out <- do.call(rbind, x); rownames(out) <- NULL; out }
refund_curves <- bind(curves_out)
refund_contrasts <- bind(contrasts_out)
refund_global <- bind(globals_out)
agreement <- bind(agreement_out)
refund_loso_curves <- bind(loso_curves_out)
refund_loso_global <- bind(loso_global_out)

# Direct interval-level comparison in identical units.
pc_keep <- primary_contrasts[c("process", "interval", "estimate",
                               "conf_low", "conf_high")]
names(pc_keep)[3:5] <- paste0("primary_", names(pc_keep)[3:5])
rc_keep <- refund_contrasts[c("process", "interval", "estimate",
                              "conf_low", "conf_high")]
names(rc_keep)[3:5] <- paste0("refund_", names(rc_keep)[3:5])
contrast_comparison <- merge(pc_keep, rc_keep, by = c("process", "interval"), all = TRUE)
contrast_comparison$direction_agrees <-
  sign(contrast_comparison$primary_estimate) == sign(contrast_comparison$refund_estimate)

pg_keep <- primary_global[c("process", "p_value", "incremental_r_squared")]
names(pg_keep)[2:3] <- c("primary_p_value", "primary_incremental_r_squared")
rg_keep <- refund_global[c("process", "p_value", "incremental_r_squared", "edf")]
names(rg_keep)[2:4] <- c("refund_p_value", "refund_incremental_r_squared", "refund_edf")
global_comparison <- merge(pg_keep, rg_keep, by = "process", all = TRUE)
global_comparison <- merge(global_comparison, agreement, by = "process", all = TRUE)

v4_write_csv(refund_curves, file.path(table_dir, "refund_functional_curves.csv"))
v4_write_csv(refund_contrasts, file.path(table_dir, "refund_interval_contrasts.csv"))
v4_write_csv(refund_global, file.path(table_dir, "refund_global_tests.csv"))
v4_write_csv(refund_loso_curves, file.path(table_dir, "refund_loso_curves.csv"))
v4_write_csv(refund_loso_global, file.path(table_dir, "refund_loso_global_tests.csv"))
v4_write_csv(contrast_comparison, file.path(table_dir, "interval_contrast_comparison.csv"))
v4_write_csv(global_comparison, file.path(table_dir, "global_and_curve_comparison.csv"))
saveRDS(objects, file.path(model_dir, "refund_sensitivity_models.rds"))

spec <- data.frame(
  item = c("analysis_role", "domain", "processes", "response", "adjustment",
           "functional_term", "basis", "maximum_basis_dimension", "penalty",
           "smoothing_selection", "integration", "bootstrap", "bootstrap_reps",
           "seed_base", "refund_version", "R_version"),
  value = c("methodological sensitivity", "-80:-1",
            paste(registry$process, collapse = "|"), "GA percentage points",
            "season fixed effect", "refund::lf() in refund::pfr()",
            "cubic P-spline", "4", "second-order difference", "REML", "Riemann",
            "season-stratified meteorological-unit cluster bootstrap", bootstrap_reps,
            seed_base, as.character(utils::packageVersion("refund")), R.version.string)
)
v4_write_csv(spec, file.path(root, "config", "frozen_refund_specification.csv"))
capture.output(sessionInfo(), file = file.path(prov_dir, "sessionInfo.txt"))

manifest_files <- c(
  list.files(table_dir, full.names = TRUE),
  list.files(model_dir, full.names = TRUE),
  file.path(root, "config", "frozen_refund_specification.csv"),
  file.path(prov_dir, "sessionInfo.txt")
)
v4_write_csv(v4_manifest(manifest_files, root), file.path(prov_dir, "manifest.csv"))
message("refund sensitivity fits and bootstrap completed.")
