#!/usr/bin/env Rscript

# Independent implementation of the statistical-review recommendations.
# This script reads the frozen analysis inputs but writes only under tmp/.

.libPaths(c(file.path(getwd(), "R-library"), .libPaths()))
suppressPackageStartupMessages({
  library(clubSandwich)
  library(ggplot2)
  library(MASS)
  library(refund)
})
source("analysis/R/consolidated_analysis_utils.R")

root <- file.path("tmp", "statistical_review_recommendations")
out_tables <- file.path(root, "outputs", "tables")
out_figures <- file.path(root, "outputs", "figures")
out_models <- file.path(root, "outputs", "models")
out_prov <- file.path(root, "outputs", "provenance")
invisible(lapply(c(out_tables, out_figures, out_models, out_prov), dir.create,
                 recursive = TRUE, showWarnings = FALSE))

write_csv <- function(x, name) {
  utils::write.csv(x, file.path(out_tables, name), row.names = FALSE, na = "")
}

bind_fill <- function(xs) {
  xs <- xs[lengths(xs) > 0]
  if (!length(xs)) return(data.frame())
  nms <- unique(unlist(lapply(xs, names), use.names = FALSE))
  xs <- lapply(xs, function(x) {
    for (nm in setdiff(nms, names(x))) x[[nm]] <- NA
    x[nms]
  })
  out <- do.call(rbind, xs)
  rownames(out) <- NULL
  out
}

days <- -80:-1
interval_map <- v4_interval_map(days)
intervals <- levels(interval_map)
registry <- consolidated_registry()
registry <- registry[match(c("LogRain", "RH", "Tmax", "Tmin"), registry$process), ]
trials <- readRDS("analysis/data/processed/trial_cohort_v4.rds")
weather <- consolidated_add_processes(
  readRDS("analysis/data/processed/weather_era5_absolute_days.rds")
)

# Establish ERA5 grid cells directly from the weather object. These are not
# field coordinates and are kept distinct from the meteorological-history ID.
grid <- unique(weather[c("trial_id", "longitude", "latitude")])
names(grid)[2:3] <- c("era5_longitude", "era5_latitude")
trials <- merge(trials, grid, by = "trial_id", all.x = TRUE, sort = FALSE)
trials <- trials[match(unique(weather$trial_id), trials$trial_id), ]
stopifnot(nrow(trials) == 72L, !anyNA(trials$era5_longitude), !anyNA(trials$era5_latitude))
trials$era5_cell <- paste(format(round(trials$era5_longitude, 5), trim = TRUE),
                          format(round(trials$era5_latitude, 5), trim = TRUE), sep = "_")
trials$cell_season <- interaction(trials$era5_cell, trials$season, drop = TRUE)
trials$season <- factor(trials$season)
trials$cultivar <- factor(trials$cultivar)

cluster_counts <- aggregate(trial_id ~ season + era5_cell + cell_season,
                            data = trials, FUN = length)
names(cluster_counts)[names(cluster_counts) == "trial_id"] <- "n_trials"
write_csv(cluster_counts, "cell_season_cluster_counts.csv")

cohort_summary <- data.frame(
  item = c("Trials", "Growing seasons", "Field-coordinate combinations",
           "Cities", "ERA5 cells", "ERA5 cell-by-season clusters",
           "Meteorological exposure units", "Cultivars",
           "Cycle-duration minimum", "Cycle-duration maximum"),
  value = c(nrow(trials), nlevels(trials$season),
            length(unique(paste(trials$latitude, trials$longitude))),
            length(unique(paste(trials$city, trials$state))),
            length(unique(trials$era5_cell)), nlevels(trials$cell_season),
            length(unique(trials$met_unit)), nlevels(trials$cultivar),
            min(trials$cycle_days), max(trials$cycle_days))
)
write_csv(cohort_summary, "cohort_summary.csv")

season_desc <- do.call(rbind, lapply(split(trials, trials$season), function(d) {
  data.frame(season = as.character(d$season[1]), n_trials = nrow(d),
             n_cities = length(unique(paste(d$city, d$state))),
             n_field_coordinates = length(unique(paste(d$latitude, d$longitude))),
             n_era5_cells = length(unique(d$era5_cell)),
             n_cell_season_clusters = length(unique(d$cell_season)),
             ga_mean = mean(d$ga), ga_sd = sd(d$ga), ga_median = median(d$ga),
             ga_min = min(d$ga), ga_max = max(d$ga))
}))
write_csv(season_desc, "descriptive_by_season.csv")
write_csv(as.data.frame(table(cultivar = trials$cultivar)), "cultivar_counts.csv")

# Pre-compute matrices, interval summaries, standardization parameters, and
# four-score functional designs. The original frozen objects are not altered.
proc <- list()
for (i in seq_len(nrow(registry))) {
  meta <- registry[i, ]
  mat <- v4_process_matrix(weather, trials, meta$process, days)
  feats <- consolidated_interval_features(mat, days, meta$process, meta$interval_summary)
  sds <- vapply(feats, sd, numeric(1))
  zfeats <- as.data.frame(Map(function(x, s) (x - mean(x)) / s, feats, sds),
                              check.names = FALSE)
  names(zfeats) <- names(feats)
  fdes <- v4_functional_design(mat, days, df = 4L)
  proc[[meta$process]] <- list(meta = meta, mat = mat, features = feats,
                              zfeatures = zfeats, feature_sd = sds,
                              fdesign = fdes, process_sd = sd(as.vector(mat)))
}

# Weather descriptives by interval.
weather_desc <- list()
for (nm in names(proc)) {
  d <- proc[[nm]]$features
  weather_desc[[nm]] <- do.call(rbind, lapply(seq_along(d), function(k) {
    x <- d[[k]]
    data.frame(process = nm, interval = intervals[k], summary = proc[[nm]]$meta$interval_summary,
               mean = mean(x), sd = sd(x), median = median(x), min = min(x), max = max(x))
  }))
}
write_csv(bind_fill(weather_desc), "weather_interval_descriptives.csv")

fit_cluster_model <- function(y, X, covariates, feature_names, process, scenario,
                              method = "Broad intervals") {
  d <- cbind(trials, as.data.frame(X, check.names = FALSE))
  d$outcome <- y
  rhs <- c(covariates, feature_names)
  fit <- lm(reformulate(rhs, response = "outcome"), data = d)
  reduced <- lm(reformulate(covariates, response = "outcome"), data = d)
  V <- vcovCR(fit, cluster = d$cell_season, type = "CR2")
  ct <- as.data.frame(coef_test(fit, vcov = V, test = "Satterthwaite"))
  ct <- ct[match(feature_names, ct$Coef), ]
  crit <- qt(0.975, ct$df_Satt)
  n_feature <- length(feature_names)
  interval_label <- if (n_feature == length(intervals)) intervals else rep(NA_character_, n_feature)
  coefs <- data.frame(
    process = process, method = method, scenario = scenario,
    feature = feature_names, interval = interval_label,
    estimate = ct$beta, std_error_CR2 = ct$SE, df_Satterthwaite = ct$df_Satt,
    conf_low_CR2 = ct$beta - crit * ct$SE,
    conf_high_CR2 = ct$beta + crit * ct$SE,
    p_value_CR2 = ct$p_Satt
  )
  wt <- as.data.frame(Wald_test(fit, constraints = constrain_zero(feature_names),
                                vcov = V, test = "HTZ"))
  sm <- summary(fit); sr <- summary(reduced)
  global <- data.frame(
    process = process, method = method, scenario = scenario,
    n = nrow(d), n_clusters = nlevels(d$cell_season),
    F_HTZ = wt$Fstat, df_num = wt$df_num, df_denom = wt$df_denom,
    p_value_CR2 = wt$p_val, r_squared = sm$r.squared,
    adjusted_r_squared = sm$adj.r.squared,
    reduced_adjusted_r_squared = sr$adj.r.squared,
    change_adjusted_r_squared = sm$adj.r.squared - sr$adj.r.squared,
    incremental_r_squared = sm$r.squared - sr$r.squared
  )
  list(fit = fit, reduced = reduced, vcov = V, coefficients = coefs, global = global, data = d)
}

covariate_sets <- list(
  "Season only" = "season",
  "Season + sowing DOY + cultivar" = c("season", "sowing_doy", "cultivar"),
  "Season + cycle duration + cultivar" = c("season", "cycle_days", "cultivar")
)

broad_fits <- list(); broad_coef <- list(); broad_global <- list()
functional_fits <- list(); functional_coef <- list(); functional_global <- list()
for (nm in names(proc)) {
  for (scenario in names(covariate_sets)) {
    covs <- covariate_sets[[scenario]]
    bf <- fit_cluster_model(trials$ga, proc[[nm]]$zfeatures, covs,
                            names(proc[[nm]]$zfeatures), nm, scenario, "Broad intervals")
    key <- paste(nm, scenario, sep = "__")
    broad_fits[[key]] <- bf
    broad_coef[[key]] <- bf$coefficients
    broad_global[[key]] <- bf$global

    scores <- as.data.frame(proc[[nm]]$fdesign$scores)
    ff <- fit_cluster_model(trials$ga, scores, covs, names(scores), nm, scenario,
                            "Scalar-on-function scores")
    theta <- coef(ff$fit)[names(scores)]
    beta <- as.vector(proc[[nm]]$fdesign$basis %*% theta)
    Vtheta <- as.matrix(ff$vcov)[names(scores), names(scores), drop = FALSE]
    curve_se <- sqrt(pmax(0, diag(proc[[nm]]$fdesign$basis %*% Vtheta %*%
                                    t(proc[[nm]]$fdesign$basis))))
    functional_fits[[key]] <- c(ff, list(beta = beta, curve_se_CR2 = curve_se))
    functional_global[[key]] <- ff$global
    functional_coef[[key]] <- data.frame(
      process = nm, scenario = scenario, lag_day = days,
      beta = beta, se_CR2 = curve_se,
      conf_low_CR2_pointwise = beta - 1.96 * curve_se,
      conf_high_CR2_pointwise = beta + 1.96 * curve_se
    )
  }
}
write_csv(bind_fill(broad_coef), "cluster_robust_broad_coefficients.csv")
write_csv(bind_fill(broad_global), "cluster_robust_broad_global_tests.csv")
write_csv(bind_fill(functional_global), "cluster_robust_functional_global_tests.csv")
write_csv(bind_fill(functional_coef), "cluster_robust_functional_curves_pointwise.csv")

# Cell-by-season cluster bootstrap for the primary four-df functional model.
sample_cluster_rows <- function(seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  ids <- split(unique(data.frame(season = trials$season,
                                 cluster = trials$cell_season)), trials$season)
  idx <- integer(0)
  for (s in names(ids)) {
    u <- as.character(ids[[s]]$cluster)
    sampled <- sample(u, length(u), replace = TRUE)
    idx <- c(idx, unlist(lapply(sampled, function(cl)
      which(as.character(trials$cell_season) == cl)), use.names = FALSE))
  }
  idx
}

cluster_bootstrap_functional <- function(nm, reps = 999L, seed = 28092026L) {
  obj <- proc[[nm]]
  base <- v4_fit_functional(trials, obj$mat, days, "ga", "season", 4L)
  curves <- matrix(NA_real_, reps, length(days))
  contrasts <- matrix(NA_real_, reps, length(intervals), dimnames = list(NULL, intervals))
  set.seed(seed)
  for (b in seq_len(reps)) {
    idx <- sample_cluster_rows()
    ok <- tryCatch({
      fb <- v4_fit_functional(trials[idx, ], obj$mat[idx, , drop = FALSE], days,
                              "ga", "season", 4L, base$center)
      curves[b, ] <- fb$beta
      contrasts[b, ] <- vapply(intervals, function(iv)
        sum(fb$beta[interval_map == iv]), numeric(1))
      TRUE
    }, error = function(e) FALSE)
  }
  keep <- complete.cases(curves)
  if (sum(keep) < 0.8 * reps) stop("Excessive cluster-bootstrap failures for ", nm)
  curves <- curves[keep, , drop = FALSE]
  contrasts <- contrasts[keep, , drop = FALSE]
  se <- apply(curves, 2, sd)
  zmax <- apply(abs(sweep(curves, 2, base$beta, "-") /
                      matrix(ifelse(se > 0, se, NA_real_), nrow(curves), length(se), byrow = TRUE)),
                1, max, na.rm = TRUE)
  critical <- unname(quantile(zmax[is.finite(zmax)], .95, type = 8))
  curve <- data.frame(
    process = nm, lag_day = days, beta = base$beta,
    conf_low_simultaneous = base$beta - critical * se,
    conf_high_simultaneous = base$beta + critical * se,
    conf_low_pointwise = apply(curves, 2, quantile, .025, type = 8),
    conf_high_pointwise = apply(curves, 2, quantile, .975, type = 8),
    simultaneous_critical = critical, successful_reps = nrow(curves)
  )
  ihat <- vapply(intervals, function(iv) sum(base$beta[interval_map == iv]), numeric(1))
  ints <- data.frame(
    process = nm, interval = intervals, estimate = ihat,
    conf_low = apply(contrasts, 2, quantile, .025, type = 8),
    conf_high = apply(contrasts, 2, quantile, .975, type = 8),
    successful_reps = nrow(curves)
  )
  list(base = base, curves = curves, contrasts = contrasts, curve = curve, intervals = ints)
}

cluster_boot <- list()
for (i in seq_along(proc)) {
  nm <- names(proc)[i]
  message("Cell-season cluster bootstrap: ", nm)
  cluster_boot[[nm]] <- cluster_bootstrap_functional(nm, 999L, 28092026L + i * 1000L)
}
write_csv(bind_fill(lapply(cluster_boot, `[[`, "curve")),
          "cell_season_bootstrap_functional_curves.csv")
write_csv(bind_fill(lapply(cluster_boot, `[[`, "intervals")),
          "cell_season_bootstrap_functional_contrasts.csv")

# Adjacent two-interval contrasts using cluster-robust covariance. These are
# evaluated as descriptive stability estimands, not as additional selection.
adjacent <- list()
for (nm in names(proc)) {
  bf <- broad_fits[[paste(nm, "Season only", sep = "__")]]
  fns <- names(proc[[nm]]$zfeatures)
  for (k in seq_len(length(fns) - 1L)) {
    L <- setNames(rep(0, length(coef(bf$fit))), names(coef(bf$fit)))
    L[fns[c(k, k + 1L)]] <- 1
    est <- sum(L * coef(bf$fit))
    se <- sqrt(drop(t(L) %*% as.matrix(bf$vcov) %*% L))
    # Conservative normal-reference interval; the individual CR2 tests use
    # Satterthwaite/HTZ small-sample corrections above.
    adjacent[[paste(nm, k)]] <- data.frame(
      process = nm, intervals = paste(intervals[k], intervals[k + 1L], sep = " + "),
      estimate_standardized_joint = est, se_CR2 = se,
      conf_low_normal = est - qnorm(.975) * se,
      conf_high_normal = est + qnorm(.975) * se
    )
  }
}
write_csv(bind_fill(adjacent), "adjacent_interval_cluster_robust_contrasts.csv")

# Within-process interval correlations and VIFs after residualizing season.
within_corr <- list(); vif_rows <- list()
for (nm in names(proc)) {
  x <- as.matrix(proc[[nm]]$zfeatures)
  C <- cor(x)
  within_corr[[nm]] <- data.frame(
    process = nm,
    interval_1 = rep(intervals, each = length(intervals)),
    interval_2 = rep(intervals, times = length(intervals)),
    correlation = as.vector(C)
  )
  resid_x <- apply(x, 2, function(v) residuals(lm(v ~ trials$season)))
  R <- cor(resid_x)
  invR <- tryCatch(solve(R), error = function(e) MASS::ginv(R))
  vif_rows[[nm]] <- data.frame(process = nm, interval = intervals,
                               season_residualized_VIF = diag(invR))
}
write_csv(bind_fill(within_corr), "within_process_interval_correlations.csv")
write_csv(bind_fill(vif_rows), "within_process_interval_vif.csv")

cross_corr <- list()
for (k in seq_along(intervals)) {
  x <- do.call(cbind, lapply(proc, function(z) z$zfeatures[[k]]))
  colnames(x) <- names(proc)
  C <- cor(x)
  cross_corr[[k]] <- data.frame(
    interval = intervals[k], process_1 = rep(names(proc), each = length(proc)),
    process_2 = rep(names(proc), times = length(proc)), correlation = as.vector(C)
  )
}
write_csv(bind_fill(cross_corr), "cross_process_correlations_by_interval.csv")

# Permutation null for in-sample incremental R2. The complete vector of eight
# interval summaries or four functional scores is permuted within season.
permutation_test <- function(y, X, covariates, reps = 999L, seed = 1L) {
  d <- cbind(trials, as.data.frame(X)); d$outcome <- y
  xnames <- names(as.data.frame(X))
  reduced <- lm(reformulate(covariates, response = "outcome"), data = d)
  full <- lm(reformulate(c(covariates, xnames), response = "outcome"), data = d)
  obs <- summary(full)$r.squared - summary(reduced)$r.squared
  null <- numeric(reps)
  set.seed(seed)
  season_rows <- split(seq_len(nrow(d)), d$season)
  for (b in seq_len(reps)) {
    perm <- seq_len(nrow(d))
    for (idx in season_rows) perm[idx] <- sample(idx)
    dp <- d
    dp[xnames] <- d[perm, xnames, drop = FALSE]
    fp <- lm(reformulate(c(covariates, xnames), response = "outcome"), data = dp)
    null[b] <- summary(fp)$r.squared - summary(reduced)$r.squared
  }
  c(observed = obs, null_median = median(null), null_95th = quantile(null, .95, names = FALSE),
    permutation_p = (1 + sum(null >= obs)) / (reps + 1))
}

perm_rows <- list()
for (i in seq_along(proc)) {
  nm <- names(proc)[i]
  pb <- permutation_test(trials$ga, proc[[nm]]$features, "season", 999L, 29000000L + i)
  pf <- permutation_test(trials$ga, as.data.frame(proc[[nm]]$fdesign$scores), "season",
                         999L, 29100000L + i)
  perm_rows[[paste0(nm, "b")]] <- data.frame(process = nm, method = "Broad intervals",
                                              t(pb), check.names = FALSE)
  perm_rows[[paste0(nm, "f")]] <- data.frame(process = nm, method = "Scalar-on-function",
                                              t(pf), check.names = FALSE)
}
write_csv(bind_fill(perm_rows), "incremental_r2_permutation_null.csv")

# Leave-one-season-out prediction of season-centred DG from season-centred
# weather summaries. This is an internal transportability stress test, not
# external validation and not a prediction of the held-out season mean.
center_within <- function(x, group) {
  out <- x
  for (g in unique(group)) {
    idx <- which(group == g)
    out[idx, ] <- sweep(x[idx, , drop = FALSE], 2, colMeans(x[idx, , drop = FALSE]), "-")
  }
  out
}

loso_predict <- function(y, X, process, method) {
  seasons <- levels(trials$season)
  yc <- ave(y, trials$season, FUN = function(z) z - mean(z))
  Xc <- center_within(as.matrix(X), trials$season)
  pred_rows <- list()
  for (s in seasons) {
    train <- trials$season != s; test <- !train
    fit <- lm.fit(cbind(1, Xc[train, , drop = FALSE]), yc[train])
    pred <- drop(cbind(1, Xc[test, , drop = FALSE]) %*% fit$coefficients)
    pred_rows[[s]] <- data.frame(process = process, method = method,
                                 omitted_season = s, trial_id = trials$trial_id[test],
                                 observed_centered_ga = yc[test], predicted_centered_ga = pred)
  }
  p <- bind_fill(pred_rows)
  data.frame(process = process, method = method, n = nrow(p),
             RMSE = sqrt(mean((p$observed_centered_ga - p$predicted_centered_ga)^2)),
             MAE = mean(abs(p$observed_centered_ga - p$predicted_centered_ga)),
             out_of_season_R2 = 1 - sum((p$observed_centered_ga - p$predicted_centered_ga)^2) /
               sum(p$observed_centered_ga^2),
             Spearman = cor(p$observed_centered_ga, p$predicted_centered_ga,
                            method = "spearman"), predictions = I(list(p)))
}

loso_metrics <- list(); loso_predictions <- list()
for (nm in names(proc)) {
  for (method in c("Broad intervals", "Scalar-on-function")) {
    X <- if (method == "Broad intervals") proc[[nm]]$features else proc[[nm]]$fdesign$scores
    z <- loso_predict(trials$ga, X, nm, method)
    key <- paste(nm, method)
    loso_predictions[[key]] <- z$predictions[[1]]
    z$predictions <- NULL
    loso_metrics[[key]] <- z
  }
}
write_csv(bind_fill(loso_metrics), "leave_one_season_out_prediction_metrics.csv")
write_csv(bind_fill(loso_predictions), "leave_one_season_out_predictions.csv")

# Influence diagnostics and robust broad-regression comparison.
influence_rows <- list(); robust_rows <- list()
for (nm in names(proc)) {
  bf <- broad_fits[[paste(nm, "Season only", sep = "__")]]
  cd <- cooks.distance(bf$fit)
  dfb <- dfbetas(bf$fit)[, names(proc[[nm]]$zfeatures), drop = FALSE]
  influence_rows[[nm]] <- data.frame(
    process = nm, trial_id = trials$trial_id, season = trials$season,
    cultivar = trials$cultivar, city = trials$city, ga = trials$ga,
    cooks_distance = cd, leverage = hatvalues(bf$fit),
    studentized_residual = rstudent(bf$fit),
    max_abs_weather_DFBETA = apply(abs(dfb), 1, max),
    high_ga_top3 = rank(-trials$ga, ties.method = "first") <= 3
  )
  rfit <- MASS::rlm(formula(bf$fit), data = bf$data, maxit = 200)
  fcoef <- coef(bf$fit)[names(proc[[nm]]$zfeatures)]
  rcoef <- coef(rfit)[names(proc[[nm]]$zfeatures)]
  robust_rows[[nm]] <- data.frame(process = nm, interval = intervals,
                                  OLS_estimate = fcoef, robust_RLM_estimate = rcoef,
                                  same_direction = sign(fcoef) == sign(rcoef))
}
write_csv(bind_fill(influence_rows), "influence_diagnostics.csv")
write_csv(bind_fill(robust_rows), "robust_regression_comparison.csv")

# Quasibinomial-logit response sensitivity with cluster-robust inference. This
# accommodates the observed zero-DG trials and the bounded percentage scale;
# it is a mean-model sensitivity rather than a binomial count-data model.
quasi_rows <- list(); quasi_global <- list()
for (nm in names(proc)) {
  X <- proc[[nm]]$zfeatures
  d <- cbind(trials, X); d$outcome <- d$ga / 100
  fns <- names(X)
  fit <- glm(reformulate(c("season", fns), response = "outcome"), data = d,
             family = quasibinomial(link = "logit"))
  V <- vcovCR(fit, cluster = d$cell_season, type = "CR2")
  ct <- as.data.frame(coef_test(fit, vcov = V, test = "Satterthwaite"))
  ct <- ct[match(fns, ct$Coef), ]
  crit <- qt(.975, ct$df_Satt)
  quasi_rows[[nm]] <- data.frame(
    process = nm, interval = intervals, log_odds_ratio = ct$beta,
    odds_ratio = exp(ct$beta),
    odds_ratio_conf_low = exp(ct$beta - crit * ct$SE),
    odds_ratio_conf_high = exp(ct$beta + crit * ct$SE), p_value_CR2 = ct$p_Satt
  )
  wt <- as.data.frame(Wald_test(fit, constraints = constrain_zero(fns),
                                vcov = V, test = "HTZ"))
  quasi_global[[nm]] <- data.frame(process = nm, F_HTZ = wt$Fstat,
                                   df_num = wt$df_num, df_denom = wt$df_denom,
                                   p_value_CR2 = wt$p_val)
}
write_csv(bind_fill(quasi_rows), "quasibinomial_logit_cluster_robust_coefficients.csv")
write_csv(bind_fill(quasi_global), "quasibinomial_logit_cluster_robust_global_tests.csv")

# A richer REML-penalized distributed-lag sensitivity (k=10). The curve is
# regularized, so k is a maximum basis dimension rather than the fitted EDF.
fit_penalized <- function(tr, mat, center, k = 10L) {
  Xc <- sweep(mat, 2, center, "-")
  dat <- data.frame(outcome = tr$ga, season = factor(tr$season))
  dat$X <- I(Xc)
  lf <- refund::lf
  fit <- refund::pfr(
    outcome ~ season + lf(X, argvals = days, integration = "riemann",
                          bs = "ps", k = k, m = 2),
    data = dat, fitter = "gam", method = "REML", family = gaussian()
  )
  cf <- coef(fit, n = length(days))
  lag_col <- grep("\\.argvals$", names(cf), value = TRUE)
  ord <- match(days, cf[[lag_col]])
  st <- as.data.frame(summary(fit)$s.table)
  pcol <- grep("p-value", names(st), fixed = TRUE, value = TRUE)
  list(fit = fit, beta = as.numeric(cf$value[ord]), model_se = as.numeric(cf$se[ord]),
       edf = st$edf[1], ref_df = st$Ref.df[1], F = st$F[1], p_value = st[[pcol]][1],
       r_squared = summary(fit)$r.sq)
}

penalized <- list(); penalized_curves <- list(); penalized_ints <- list(); penalized_global <- list()
for (i in seq_along(proc)) {
  nm <- names(proc)[i]; obj <- proc[[nm]]
  message("Richer penalized distributed lag and cluster bootstrap: ", nm)
  center <- colMeans(obj$mat)
  base <- fit_penalized(trials, obj$mat, center, 10L)
  reps <- 499L
  curves <- matrix(NA_real_, reps, length(days))
  ints <- matrix(NA_real_, reps, length(intervals), dimnames = list(NULL, intervals))
  set.seed(30000000L + i)
  for (b in seq_len(reps)) {
    idx <- sample_cluster_rows()
    ok <- tryCatch({
      fb <- fit_penalized(trials[idx, ], obj$mat[idx, , drop = FALSE], center, 10L)
      curves[b, ] <- fb$beta
      ints[b, ] <- vapply(intervals, function(iv) sum(fb$beta[interval_map == iv]), numeric(1))
      TRUE
    }, error = function(e) FALSE)
  }
  keep <- complete.cases(curves)
  if (sum(keep) < .75 * reps) stop("Excessive penalized-bootstrap failures for ", nm)
  curves <- curves[keep, , drop = FALSE]; ints <- ints[keep, , drop = FALSE]
  se <- apply(curves, 2, sd)
  zmax <- apply(abs(sweep(curves, 2, base$beta, "-") /
                      matrix(ifelse(se > 0, se, NA_real_), nrow(curves), length(se), byrow = TRUE)),
                1, max, na.rm = TRUE)
  crit <- unname(quantile(zmax[is.finite(zmax)], .95, type = 8))
  penalized_curves[[nm]] <- data.frame(
    process = nm, lag_day = days, beta = base$beta,
    conf_low_simultaneous = base$beta - crit * se,
    conf_high_simultaneous = base$beta + crit * se,
    successful_reps = nrow(curves), maximum_basis_dimension = 10L,
    fitted_edf = base$edf
  )
  ihat <- vapply(intervals, function(iv) sum(base$beta[interval_map == iv]), numeric(1))
  penalized_ints[[nm]] <- data.frame(
    process = nm, interval = intervals, estimate = ihat,
    conf_low = apply(ints, 2, quantile, .025, type = 8),
    conf_high = apply(ints, 2, quantile, .975, type = 8),
    successful_reps = nrow(curves)
  )
  penalized_global[[nm]] <- data.frame(
    process = nm, edf = base$edf, reference_df = base$ref_df,
    F_model_based = base$F, p_value_model_based = base$p_value,
    r_squared = base$r_squared, maximum_basis_dimension = 10L,
    bootstrap_clusters = nlevels(trials$cell_season), bootstrap_reps = nrow(curves)
  )
  penalized[[nm]] <- list(base = base, curves = curves, contrasts = ints)
}
write_csv(bind_fill(penalized_curves), "penalized_k10_functional_curves.csv")
write_csv(bind_fill(penalized_ints), "penalized_k10_interval_contrasts.csv")
write_csv(bind_fill(penalized_global), "penalized_k10_global_tests.csv")

# Two-process models for LogRain and Tmin, the two processes with functional
# evidence in the primary analysis. These are sensitivities, not selected final
# models, and no interaction is introduced.
two_broad_X <- cbind(proc$LogRain$zfeatures, proc$Tmin$zfeatures)
fit_two_process <- function(X) {
  X <- as.data.frame(X, check.names = FALSE)
  d <- cbind(trials, X)
  d$outcome <- d$ga
  fit <- lm(reformulate(c("season", names(X)), response = "outcome"), data = d)
  V <- vcovCR(fit, cluster = d$cell_season, type = "CR2")
  list(fit = fit, vcov = V, data = d)
}
two_broad <- fit_two_process(two_broad_X)
two_broad_tests <- list()
for (nm in c("LogRain", "Tmin")) {
  fns <- grep(paste0("^", nm, "_B"), names(coef(two_broad$fit)), value = TRUE)
  if (!length(fns)) stop("Could not identify two-process broad terms for ", nm)
  wt <- as.data.frame(Wald_test(two_broad$fit, constraints = constrain_zero(fns),
                                vcov = two_broad$vcov, test = "HTZ"))
  two_broad_tests[[nm]] <- data.frame(model = "Broad intervals", process = nm,
                                      F_HTZ = wt$Fstat, df_num = wt$df_num,
                                      df_denom = wt$df_denom, p_value_CR2 = wt$p_val)
}
two_scores <- cbind(proc$LogRain$fdesign$scores, proc$Tmin$fdesign$scores)
colnames(two_scores) <- c(paste0("LogRain_score", 1:4), paste0("Tmin_score", 1:4))
two_func <- fit_two_process(two_scores)
for (nm in c("LogRain", "Tmin")) {
  fns <- grep(paste0("^", nm, "_score"), names(coef(two_func$fit)), value = TRUE)
  if (!length(fns)) stop("Could not identify two-process functional terms for ", nm)
  wt <- as.data.frame(Wald_test(two_func$fit, constraints = constrain_zero(fns),
                                vcov = two_func$vcov, test = "HTZ"))
  two_broad_tests[[paste0(nm, "f")]] <- data.frame(model = "Scalar-on-function",
                                                    process = nm, F_HTZ = wt$Fstat,
                                                    df_num = wt$df_num, df_denom = wt$df_denom,
                                                    p_value_CR2 = wt$p_val)
}
write_csv(bind_fill(two_broad_tests), "two_process_cluster_robust_tests.csv")

# Feasibility audit for recommendations requiring unavailable variables.
feasibility <- data.frame(
  recommendation = c("Cell-by-season clustered inference", "Sowing-date adjustment",
                     "Cycle-duration adjustment", "Cultivar adjustment",
                     "Phenology-anchored R5/R7 analysis", "Sample-mass weighting",
                     "Trial-type stratification", "Hourly wetness summaries"),
  status = c("Implemented", "Implemented", "Implemented", "Implemented",
             "Not estimable from frozen source", "Not estimable from frozen source",
             "Not estimable from frozen source", "Not estimable from daily frozen weather"),
  reason = c("36 ERA5 cell-by-season clusters reconstructed from pinned weather coordinates",
             "Complete sowing dates and sowing day-of-year available",
             "Complete sowing-to-grain-evaluation duration available",
             "Cultivar complete with two observed levels",
             "No R5 or R7 date field exists in the raw trial source or frozen cohort",
             "No sample-mass field exists in the raw trial source or frozen cohort",
             "No fungicide-versus-planting-date trial-type field exists in the source",
             "Frozen ERA5 analytical cache contains daily rather than hourly fields")
)
write_csv(feasibility, "recommendation_feasibility.csv")

# Save all fitted objects for audit and exact reproduction.
saveRDS(list(broad_fits = broad_fits, functional_fits = functional_fits,
             functional_cell_season_bootstrap = cluster_boot,
             penalized_k10 = penalized, two_process_broad = two_broad,
             two_process_functional = two_func),
        file.path(out_models, "statistical_review_models.rds"), compress = "xz")

writeLines(capture.output(sessionInfo()), file.path(out_prov, "sessionInfo.txt"))
inputs <- c("analysis/data/processed/trial_cohort_v4.rds",
            "analysis/data/processed/weather_era5_absolute_days.rds",
            "analysis/config/process_registry.csv",
            "analysis/R/consolidated_analysis_utils.R",
            "analysis/R/v4_utils.R")
manifest <- data.frame(path = normalizePath(inputs, winslash = "/", mustWork = TRUE),
                       sha256 = v4_sha256(inputs))
write_csv(manifest, "input_manifest.csv")

message("Recommended statistical analyses completed in: ", normalizePath(root, winslash = "/"))
