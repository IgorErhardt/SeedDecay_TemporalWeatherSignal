# Shared implementation for the consolidated analysis.

source("analysis/R/v4_utils.R")

consolidated_registry <- function(path = "analysis/config/process_registry.csv") {
  x <- utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
  required <- c("process", "label", "domain", "role", "interval_summary",
                "raw_unit", "display_order", "color", "seed_offset")
  if (!all(required %in% names(x))) stop("Process registry is incomplete.")
  x <- x[order(x$display_order), ]
  if (anyDuplicated(x$process) || !all(x$interval_summary %in% c("sum", "mean")))
    stop("Invalid process registry.")
  x
}

consolidated_add_processes <- function(weather) {
  required <- c("T2M_MIN", "Tmax", "RH", "Rain")
  if (!all(required %in% names(weather)))
    stop("Weather data lack fields needed by the consolidated process registry.")
  weather$Tmin <- weather$T2M_MIN
  weather$LogRain <- log1p(weather$Rain)
  weather
}

consolidated_interval_features <- function(mat, days, process, summary_type) {
  imap <- v4_interval_map(days)
  fun <- if (identical(summary_type, "sum")) rowSums else rowMeans
  vals <- lapply(levels(imap), function(b) fun(mat[, imap == b, drop = FALSE]))
  out <- as.data.frame(vals, check.names = FALSE)
  names(out) <- paste(process, seq_along(vals), sep = "_B")
  out
}

consolidated_bind <- function(x) {
  if (!length(x)) return(data.frame())
  cols <- unique(unlist(lapply(x, names), use.names = FALSE))
  x <- lapply(x, function(z) {
    for (nm in setdiff(cols, names(z))) z[[nm]] <- NA
    z[cols]
  })
  out <- do.call(rbind, x)
  rownames(out) <- NULL
  out
}

consolidated_fit_set <- function(trials, weather, registry, days = -80:-1,
                                 response = "ga", covariates = "season",
                                 cluster_col = "weather_cluster_id",
                                 bootstrap_reps = 999L, bootstrap = TRUE,
                                 seed_base = 24041987L) {
  broad_coef <- broad_global <- curves <- contrasts <- functional_global <- objects <- list()
  intervals <- levels(v4_interval_map(days))
  for (i in seq_len(nrow(registry))) {
    meta <- registry[i, ]
    proc <- meta$process
    mat <- v4_process_matrix(weather, trials, proc, days)
    feats <- consolidated_interval_features(mat, days, proc, meta$interval_summary)
    cols <- names(feats)
    feature_sd <- vapply(feats, stats::sd, numeric(1))
    if (any(!is.finite(feature_sd) | feature_sd == 0))
      stop("Non-variable broad feature for ", proc, " in ", paste(range(days), collapse = ":"))
    zfeats <- as.data.frame(Map(function(x, s) (x - mean(x)) / s, feats, feature_sd),
                                check.names = FALSE)
    names(zfeats) <- cols
    fit_z <- v4_global_lm(cbind(trials, zfeats), response, cols, covariates, cluster_col)
    fit_raw <- v4_global_lm(cbind(trials, feats), response, cols, covariates, cluster_col)
    bc <- v4_tidy_lm(fit_z$full, cols, intervals,
                     coefficient_test = fit_z$coefficient_test)
    raw <- v4_tidy_lm(fit_raw$full, cols, intervals,
                      coefficient_test = fit_raw$coefficient_test)
    bc$raw_estimate <- raw$estimate
    bc$raw_conf_low <- raw$conf_low
    bc$raw_conf_high <- raw$conf_high
    bc$feature_sd <- as.numeric(feature_sd)
    bc$process <- proc
    bc$process_label <- meta$label
    bc$domain <- meta$domain
    bc$role <- meta$role
    bc$raw_unit <- meta$raw_unit
    bc$response <- response
    bc$n <- nrow(trials)
    bc$days <- paste(range(days), collapse = ":")
    bc$covariates <- paste(covariates, collapse = "|")
    broad_coef[[proc]] <- bc
    bg <- fit_z$global
    bg$process <- proc; bg$process_label <- meta$label; bg$domain <- meta$domain
    bg$role <- meta$role; bg$response <- response; bg$n <- nrow(trials)
    bg$days <- paste(range(days), collapse = ":")
    bg$covariates <- paste(covariates, collapse = "|")
    broad_global[[proc]] <- bg

    ff <- v4_fit_functional(trials, mat, days, response, covariates, 4L,
                            cluster_col = cluster_col)
    process_sd <- stats::sd(as.vector(mat))
    if (bootstrap) {
      fb <- v4_cluster_bootstrap_curves(trials, mat, ff, reps = bootstrap_reps,
                                        unit_col = cluster_col,
                                        seed = seed_base + meta$seed_offset)
      fc <- data.frame(lag_day = days, beta = ff$beta,
                       conf_low = fb$conf_low, conf_high = fb$conf_high,
                       pointwise_conf_low = fb$pointwise_conf_low,
                       pointwise_conf_high = fb$pointwise_conf_high,
                       beta_per_process_sd = ff$beta * process_sd,
                       conf_low_per_process_sd = fb$conf_low * process_sd,
                       conf_high_per_process_sd = fb$conf_high * process_sd,
                       pointwise_conf_low_per_process_sd = fb$pointwise_conf_low * process_sd,
                       pointwise_conf_high_per_process_sd = fb$pointwise_conf_high * process_sd,
                       bootstrap_reps = fb$successful_reps)
      fi <- fb$interval_summary
    } else {
      imap <- v4_interval_map(days)
      fi <- data.frame(interval = levels(imap),
                       estimate = vapply(levels(imap), function(z) sum(ff$beta[imap == z]), numeric(1)),
                       conf_low = NA_real_, conf_high = NA_real_)
      fc <- data.frame(lag_day = days, beta = ff$beta, conf_low = NA_real_,
                       conf_high = NA_real_, pointwise_conf_low = NA_real_,
                       pointwise_conf_high = NA_real_, beta_per_process_sd = ff$beta * process_sd,
                       conf_low_per_process_sd = NA_real_, conf_high_per_process_sd = NA_real_,
                       pointwise_conf_low_per_process_sd = NA_real_,
                       pointwise_conf_high_per_process_sd = NA_real_,
                       bootstrap_reps = 0L)
      fb <- NULL
    }
    n_days <- as.numeric(table(v4_interval_map(days))[fi$interval])
    # Integrated contrasts represent raising every day in an interval by one
    # unit. Division by interval length gives the average contrast per day.
    fi$estimate_per_day <- fi$estimate / n_days
    fi$conf_low_per_day <- fi$conf_low / n_days
    fi$conf_high_per_day <- fi$conf_high / n_days
    fi$estimate_per_process_sd <- fi$estimate * process_sd
    fi$conf_low_per_process_sd <- fi$conf_low * process_sd
    fi$conf_high_per_process_sd <- fi$conf_high * process_sd
    for (z in list(fc, fi)) invisible(z)
    fc$process <- fi$process <- proc
    fc$process_label <- fi$process_label <- meta$label
    fc$domain <- fi$domain <- meta$domain
    fc$role <- fi$role <- meta$role
    fc$response <- fi$response <- response
    fc$n <- fi$n <- nrow(trials)
    fc$days <- fi$days <- paste(range(days), collapse = ":")
    curves[[proc]] <- fc
    contrasts[[proc]] <- fi
    fg <- ff$global
    fg$process <- proc; fg$process_label <- meta$label; fg$domain <- meta$domain
    fg$role <- meta$role; fg$response <- response; fg$n <- nrow(trials)
    fg$days <- paste(range(days), collapse = ":"); fg$functional_df <- 4L
    fg$covariates <- paste(covariates, collapse = "|")
    functional_global[[proc]] <- fg
    objects[[proc]] <- list(broad_standardized = fit_z$full, broad_raw = fit_raw$full,
                            functional = ff, bootstrap = fb, feature_sd = feature_sd,
                            process_sd = process_sd, summary_type = meta$interval_summary)
  }
  list(broad_coefficients = consolidated_bind(broad_coef),
       broad_global = consolidated_bind(broad_global),
       functional_curves = consolidated_bind(curves),
       functional_contrasts = consolidated_bind(contrasts),
       functional_global = consolidated_bind(functional_global), objects = objects)
}

consolidated_tag <- function(result, scenario) {
  for (nm in setdiff(names(result), "objects")) result[[nm]]$scenario <- scenario
  result
}

consolidated_bind_component <- function(results, component) {
  consolidated_bind(lapply(results, `[[`, component))
}
