options(stringsAsFactors = FALSE)

v4_root <- function() {
  normalizePath("analysis", winslash = "/", mustWork = FALSE)
}

v4_paths <- function() {
  root <- v4_root()
  list(
    root = root,
    cache = file.path(root, "data", "weather_cache_era5"),
    processed = file.path(root, "data", "processed"),
    config = file.path(root, "config"),
    models = file.path(root, "models"),
    tables = file.path(root, "outputs", "tables"),
    diagnostics = file.path(root, "outputs", "diagnostics"),
    provenance = file.path(root, "provenance"),
    freeze = file.path(root, "freeze")
  )
}

v4_init_dirs <- function() {
  p <- v4_paths()
  invisible(lapply(unique(unlist(p)), dir.create, recursive = TRUE, showWarnings = FALSE))
  p
}

v4_sha256 <- function(path) {
  if (!requireNamespace("digest", quietly = TRUE)) stop("Package 'digest' is required.")
  unname(vapply(path, digest::digest, character(1), file = TRUE, algo = "sha256"))
}

v4_write_csv <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(x, path, row.names = FALSE, na = "")
  invisible(path)
}

v4_write_lines <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(x, path, useBytes = TRUE)
  invisible(path)
}

v4_manifest <- function(paths, root = getwd()) {
  paths <- unique(normalizePath(paths, winslash = "/", mustWork = TRUE))
  info <- file.info(paths)
  data.frame(
    path = paths,
    relative_path = sub(paste0("^", gsub("([\\.\\+\\*\\?\\[\\^\\]\\$\\(\\)\\{\\}=!<>|:\\-])", "\\\\\\1", normalizePath(root, winslash = "/")), "/?"), "", paths),
    bytes = info$size,
    modified_utc = format(info$mtime, tz = "UTC", usetz = TRUE),
    sha256 = v4_sha256(paths),
    stringsAsFactors = FALSE
  )
}

v4_parse_mdy <- function(x) {
  as.Date(x, format = "%m/%d/%Y")
}

v4_skewness <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) < 3L) return(NA_real_)
  z <- (x - mean(x)) / stats::sd(x)
  mean(z^3)
}

v4_safe_cor <- function(x, y) {
  ok <- is.finite(x) & is.finite(y)
  if (sum(ok) < 3L || stats::sd(x[ok]) == 0 || stats::sd(y[ok]) == 0) return(NA_real_)
  stats::cor(x[ok], y[ok])
}

v4_srmse <- function(x, y) {
  ok <- is.finite(x) & is.finite(y)
  if (!any(ok)) return(NA_real_)
  denom <- stats::sd(c(x[ok], y[ok]))
  if (!is.finite(denom) || denom == 0) return(ifelse(all(x[ok] == y[ok]), 0, Inf))
  sqrt(mean((x[ok] - y[ok])^2)) / denom
}

v4_union_components <- function(ids, edges) {
  parent <- setNames(ids, ids)
  find_root <- function(x) {
    while (parent[[x]] != x) x <- parent[[x]]
    x
  }
  if (nrow(edges)) {
    for (i in seq_len(nrow(edges))) {
      a <- find_root(edges$id1[i]); b <- find_root(edges$id2[i])
      if (a != b) parent[[b]] <- a
    }
  }
  roots <- vapply(ids, find_root, character(1))
  u <- unique(roots)
  setNames(sprintf("MEU%03d", match(roots, u)), ids)
}

v4_interval_map <- function(days) {
  if (identical(range(days), c(-100L, -1L)) && length(days) == 100L) {
    lev <- c("-100:-81", "-80:-61", "-60:-41", "-40:-21", "-20:-1")
    breaks <- c(-101L, -81L, -61L, -41L, -21L, -1L)
  } else if (identical(range(days), c(-80L, -1L)) && length(days) == 80L) {
    lev <- c("-80:-71", "-70:-61", "-60:-51", "-50:-41",
             "-40:-31", "-30:-21", "-20:-11", "-10:-1")
    breaks <- c(-81L, -71L, -61L, -51L, -41L, -31L, -21L, -11L, -1L)
  } else if (identical(range(days), c(-70L, -1L)) && length(days) == 70L) {
    lev <- c("-70:-61", "-60:-51", "-50:-41", "-40:-31", "-30:-21", "-20:-11", "-10:-1")
    breaks <- c(min(days) - 1L, seq(min(days) + 9L, -1L, by = 10L))
  } else if (identical(range(days), c(-60L, -1L)) && length(days) == 60L) {
    lev <- c("-60:-51", "-50:-41", "-40:-31", "-30:-21", "-20:-11", "-10:-1")
    breaks <- c(min(days) - 1L, seq(min(days) + 9L, -1L, by = 10L))
  } else stop("Unsupported temporal domain for fixed intervals.")
  cut(days, breaks = breaks, labels = lev, include.lowest = TRUE)
}

v4_process_matrix <- function(weather, trials, process, days) {
  w <- weather[weather$lag_day %in% days, , drop = FALSE]
  if (!process %in% names(w)) stop("Missing weather process: ", process)
  mat <- matrix(NA_real_, nrow = nrow(trials), ncol = length(days), dimnames = list(trials$trial_id, as.character(days)))
  for (i in seq_len(nrow(trials))) {
    z <- w[w$trial_id == trials$trial_id[i], c("lag_day", process), drop = FALSE]
    mat[i, match(z$lag_day, days)] <- z[[process]]
  }
  if (anyNA(mat)) stop("Incomplete process matrix for ", process, " in domain ", paste(range(days), collapse = ":"))
  mat
}

v4_interval_features <- function(mat, days, process) {
  imap <- v4_interval_map(days)
  vals <- lapply(levels(imap), function(b) rowMeans(mat[, imap == b, drop = FALSE]))
  out <- as.data.frame(vals, check.names = FALSE)
  names(out) <- paste(process, seq_along(vals), sep = "_B")
  out
}

v4_global_lm <- function(data, response, feature_cols, covariates = "season",
                         cluster_col = NULL) {
  terms0 <- covariates[covariates %in% names(data)]
  rhs0 <- if (length(terms0)) paste(terms0, collapse = " + ") else "1"
  reduced <- stats::lm(stats::as.formula(paste(response, "~", rhs0)), data = data)
  full <- stats::lm(stats::as.formula(paste(response, "~", paste(c(rhs0, feature_cols), collapse = " + "))), data = data)
  av <- stats::anova(reduced, full)
  sm <- summary(full)
  naive_global <- data.frame(
    df_num = av$Df[2], df_den = stats::df.residual(full),
    f_statistic = av$F[2], p_value = av$`Pr(>F)`[2]
  )
  global <- data.frame(
    df_num = naive_global$df_num, df_den = naive_global$df_den,
    f_statistic = naive_global$f_statistic, p_value = naive_global$p_value,
    r_squared = sm$r.squared, adjusted_r_squared = sm$adj.r.squared,
    incremental_r_squared = sm$r.squared - summary(reduced)$r.squared,
    residual_sd = sm$sigma,
    naive_df_num = naive_global$df_num, naive_df_den = naive_global$df_den,
    naive_f_statistic = naive_global$f_statistic,
    naive_p_value = naive_global$p_value,
    n_clusters = NA_integer_, inference = "ordinary OLS"
  )
  vcov_cr2 <- NULL
  coefficient_test <- NULL
  if (!is.null(cluster_col)) {
    if (!cluster_col %in% names(data)) stop("Cluster column not found: ", cluster_col)
    if (!requireNamespace("clubSandwich", quietly = TRUE))
      stop("Package 'clubSandwich' is required for CR2 cluster-robust inference.")
    cluster <- data[[cluster_col]]
    if (anyNA(cluster)) stop("Missing values in cluster column: ", cluster_col)
    vcov_cr2 <- clubSandwich::vcovCR(full, cluster = cluster, type = "CR2")
    coefficient_test <- as.data.frame(clubSandwich::coef_test(
      full, vcov = vcov_cr2, test = "Satterthwaite"
    ))
    wald <- as.data.frame(clubSandwich::Wald_test(
      full,
      constraints = clubSandwich::constrain_zero(feature_cols),
      vcov = vcov_cr2,
      test = "HTZ"
    ))
    global$df_num <- wald$df_num[1]
    global$df_den <- wald$df_denom[1]
    global$f_statistic <- wald$Fstat[1]
    global$p_value <- wald$p_val[1]
    global$n_clusters <- length(unique(cluster))
    global$inference <- paste0("CR2/HTZ clustered by ", cluster_col)
  }
  list(
    reduced = reduced,
    full = full,
    global = global,
    vcov_cr2 = vcov_cr2,
    coefficient_test = coefficient_test
  )
}

v4_tidy_lm <- function(fit, feature_cols, interval_labels, scales = NULL,
                       coefficient_test = NULL) {
  cf <- summary(fit)$coefficients
  keep <- match(feature_cols, rownames(cf))
  naive_critical <- stats::qt(0.975, stats::df.residual(fit))
  out <- data.frame(
    feature = feature_cols,
    interval = interval_labels,
    estimate = cf[keep, 1],
    std_error = cf[keep, 2],
    conf_low = cf[keep, 1] - naive_critical * cf[keep, 2],
    conf_high = cf[keep, 1] + naive_critical * cf[keep, 2],
    p_value = cf[keep, 4],
    df = stats::df.residual(fit),
    naive_std_error = cf[keep, 2],
    naive_conf_low = cf[keep, 1] - naive_critical * cf[keep, 2],
    naive_conf_high = cf[keep, 1] + naive_critical * cf[keep, 2],
    naive_p_value = cf[keep, 4],
    inference = "ordinary OLS"
  )
  if (!is.null(coefficient_test)) {
    term <- if ("Coef" %in% names(coefficient_test)) {
      as.character(coefficient_test$Coef)
    } else rownames(coefficient_test)
    robust_keep <- match(feature_cols, term)
    if (anyNA(robust_keep)) stop("CR2 coefficient test did not contain every weather feature.")
    robust <- coefficient_test[robust_keep, , drop = FALSE]
    critical <- stats::qt(0.975, robust$df_Satt)
    out$std_error <- robust$SE
    out$conf_low <- out$estimate - critical * robust$SE
    out$conf_high <- out$estimate + critical * robust$SE
    out$p_value <- robust$p_Satt
    out$df <- robust$df_Satt
    out$inference <- "CR2 with Satterthwaite degrees of freedom"
  }
  if (!is.null(scales)) {
    out$feature_sd <- as.numeric(scales[feature_cols])
    out$estimate_unstandardized <- out$estimate / out$feature_sd
    out$conf_low_unstandardized <- out$conf_low / out$feature_sd
    out$conf_high_unstandardized <- out$conf_high / out$feature_sd
  }
  out
}

v4_bs_basis <- function(days, df = 4L) {
  B <- splines::bs(days, df = df, degree = 3, intercept = TRUE,
                   Boundary.knots = range(days))
  colnames(B) <- paste0("fb", seq_len(ncol(B)))
  B
}

v4_functional_design <- function(mat, days, df = 4L, center = NULL) {
  if (is.null(center)) center <- colMeans(mat)
  Xc <- sweep(mat, 2, center, "-")
  B <- v4_bs_basis(days, df)
  scores <- Xc %*% B
  colnames(scores) <- paste0("score", seq_len(ncol(scores)))
  list(scores = scores, basis = B, center = center)
}

v4_fit_functional <- function(trials, mat, days, response = "ga", covariates = "season",
                              df = 4L, center = NULL, cluster_col = NULL) {
  des <- v4_functional_design(mat, days, df, center)
  dat <- cbind(trials, as.data.frame(des$scores))
  score_cols <- colnames(des$scores)
  fit <- v4_global_lm(dat, response, score_cols, covariates, cluster_col)
  theta <- stats::coef(fit$full)[score_cols]
  beta <- as.vector(des$basis %*% theta)
  list(full = fit$full, reduced = fit$reduced, global = fit$global, beta = beta,
       basis = des$basis, center = des$center, scores = des$scores,
       score_cols = score_cols, days = days, response = response,
       covariates = covariates, cluster_col = cluster_col,
       vcov_cr2 = fit$vcov_cr2, coefficient_test = fit$coefficient_test)
}

v4_cluster_bootstrap_curves <- function(trials, mat, fit_obj, unit_col = "weather_cluster_id",
                                        strata_col = "season", reps = 999L, seed = 24041987L) {
  set.seed(seed)
  units <- unique(trials[c(unit_col, strata_col)])
  if (anyDuplicated(units[[unit_col]])) stop("Bootstrap clusters cross strata; cannot use stratified cluster bootstrap.")
  strata <- split(units[[unit_col]], units[[strata_col]])
  curves <- matrix(NA_real_, nrow = reps, ncol = length(fit_obj$days))
  int_map <- v4_interval_map(fit_obj$days)
  contrasts <- matrix(NA_real_, nrow = reps, ncol = nlevels(int_map))
  colnames(contrasts) <- levels(int_map)
  failures <- character(0)
  for (b in seq_len(reps)) {
    sampled <- unlist(lapply(strata, function(u) sample(u, length(u), replace = TRUE)), use.names = FALSE)
    idx <- integer(0)
    for (j in seq_along(sampled)) {
      rows <- which(trials[[unit_col]] == sampled[j])
      idx <- c(idx, rows)
    }
    trb <- trials[idx, , drop = FALSE]
    mb <- mat[idx, , drop = FALSE]
    ok <- tryCatch({
      fb <- v4_fit_functional(trb, mb, fit_obj$days, fit_obj$response,
                              fit_obj$covariates, ncol(fit_obj$basis), fit_obj$center,
                              cluster_col = NULL)
      curves[b, ] <- fb$beta
      contrasts[b, ] <- vapply(levels(int_map), function(z) sum(fb$beta[int_map == z]), numeric(1))
      TRUE
    }, error = function(e) { failures <<- c(failures, conditionMessage(e)); FALSE })
  }
  keep <- stats::complete.cases(curves)
  if (sum(keep) < max(100L, floor(0.8 * reps))) stop("Too many functional bootstrap failures: ", sum(!keep))
  curves <- curves[keep, , drop = FALSE]
  contrasts <- contrasts[keep, , drop = FALSE]
  se <- apply(curves, 2, stats::sd)
  safe_se <- ifelse(se > 0, se, NA_real_)
  zmax <- apply(abs(sweep(curves, 2, fit_obj$beta, "-") / matrix(safe_se, nrow(curves), length(safe_se), byrow = TRUE)), 1, max, na.rm = TRUE)
  crit <- unname(stats::quantile(zmax[is.finite(zmax)], 0.95, type = 8))
  interval_hat <- vapply(levels(int_map), function(z) sum(fit_obj$beta[int_map == z]), numeric(1))
  list(
    curves = curves,
    contrasts = contrasts,
    pointwise_se = se,
    pointwise_conf_low = apply(curves, 2, stats::quantile, 0.025, type = 8, na.rm = TRUE),
    pointwise_conf_high = apply(curves, 2, stats::quantile, 0.975, type = 8, na.rm = TRUE),
    simultaneous_critical = crit,
    conf_low = fit_obj$beta - crit * se,
    conf_high = fit_obj$beta + crit * se,
    interval_summary = data.frame(
      interval = levels(int_map), estimate = interval_hat,
      conf_low = apply(contrasts, 2, stats::quantile, 0.025, type = 8, na.rm = TRUE),
      conf_high = apply(contrasts, 2, stats::quantile, 0.975, type = 8, na.rm = TRUE)
    ),
    successful_reps = nrow(curves), requested_reps = reps,
    failure_messages = unique(failures)
  )
}

v4_theme <- function() {
  ggplot2::theme_minimal(base_size = 10.5) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      plot.title.position = "plot",
      plot.title = ggplot2::element_text(face = "bold", size = 12),
      plot.subtitle = ggplot2::element_text(color = "#475569"),
      axis.title = ggplot2::element_text(face = "bold"),
      strip.text = ggplot2::element_text(face = "bold"),
      legend.position = "bottom"
    )
}

v4_save_plot <- function(plot, filename, width = 8, height = 5.2) {
  p <- v4_paths()
  path <- file.path(p$figures, filename)
  ggplot2::ggsave(path, plot, width = width, height = height, dpi = 320, bg = "white")
  path
}

v4_freeze_manifest <- function(directory, output_path) {
  files <- list.files(directory, recursive = TRUE, full.names = TRUE)
  files <- files[file.info(files)$isdir %in% FALSE]
  files <- files[normalizePath(files, winslash = "/", mustWork = FALSE) != normalizePath(output_path, winslash = "/", mustWork = FALSE)]
  v4_write_csv(v4_manifest(files, v4_root()), output_path)
  invisible(output_path)
}

v4_assert_freeze <- function(path) {
  if (!file.exists(path)) stop("Required freeze gate is absent: ", path)
  invisible(TRUE)
}
