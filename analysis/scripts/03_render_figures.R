#!/usr/bin/env Rscript

# Sole owner of the final analysis figures. It reads frozen consolidated
# tables and does not fit or alter statistical models.
.libPaths(c(file.path(getwd(), "R-library"), .libPaths()))
suppressPackageStartupMessages({
  library(ggplot2)
  library(patchwork)
})
source("analysis/R/consolidated_analysis_utils.R")
source("analysis/R/panel_labels.R")

p <- v4_paths()
tables <- file.path(p$tables, "final")
diagnostics <- file.path(p$diagnostics, "final")
freeze <- file.path(p$freeze, "final", "consolidated_results_manifest.csv")
if (!file.exists(freeze)) stop("Consolidated results are not frozen.")
out <- "figures"
dir.create(out, recursive = TRUE, showWarnings = FALSE)

# Remove only files owned by this renderer so obsolete variants cannot remain
# mixed with current figures.
owned <- c(
  "figure1_trial_locations_map.png", "figure2_all_process_broad_intervals.png",
  "figure3_all_process_functional_curves.png", "figure4_all_process_shared_sensitivities.png",
  "figure5_all_process_loso_stability.png", "figure6_days_after_sowing.png",
  "figureS1_process_correlations.png", "figureS2_fit_and_stability.png",
  "figureS3_functional_coefficient_heatmap.png",
  "figureS4_broad_residual_vs_fitted.png", "figureS5_broad_normal_qq.png",
  "figureS6_functional_residual_vs_fitted.png", "figureS7_functional_normal_qq.png",
  "figureS8_refund_penalized_curves.png", "figureS9_refund_penalized_contrasts.png",
  "figure4b_functional_shared_sensitivities.png", "figure5b_functional_loso_stability.png",
  "figure8_ga_distributions.png", "figure3b_functional_pointwise_bootstrap.png"
)
obsolete_owned <- c("figureS4_broad_residual_diagnostics.png",
                    "figureS5_functional_residual_diagnostics.png",
                    "figure2_ga_distributions.png",
                    "figure3_all_process_broad_intervals.png",
                    "figure6_all_process_functional_curves.png",
                    "figure7b_functional_shared_sensitivities.png",
                    "figure8_functional_loso_stability.png",
                    "figureS1_functional_coefficient_heatmap.png",
                    "figureS2_broad_residual_vs_fitted.png",
                    "figureS3_broad_normal_qq.png",
                    "figureS4_functional_residual_vs_fitted.png",
                    "figureS5_functional_normal_qq.png",
                    "figureS6_refund_penalized_curves.png",
                    "figureS7_refund_penalized_contrasts.png")
to_remove <- c(owned, obsolete_owned)
unlink(file.path(out, to_remove[file.exists(file.path(out, to_remove))]))

read_final <- function(name) utils::read.csv(file.path(tables, name), check.names = FALSE,
                                             stringsAsFactors = FALSE)
registry <- read_final("process_registry.csv")

# The public registry contains only the four retained processes.
keep_processes <- c("LogRain", "RH", "Tmax", "Tmin")
registry$include_analysis <- registry$process %in% keep_processes

missing_keep <- setdiff(keep_processes, registry$process)
if (length(missing_keep) > 0) {
  stop("These requested processes were not found in process_registry.csv: ",
       paste(missing_keep, collapse = ", "))
}

registry <- registry[registry$include_analysis, , drop = FALSE]
registry <- registry[order(registry$display_order), ]
proc_order <- registry$process
label_lookup <- setNames(registry$label, registry$process)

# Use process colors directly from process_registry.csv.
if (any(is.na(registry$color) | registry$color == "")) {
  stop("Every included process must have a non-missing color in process_registry.csv.")
}
display_color_lookup <- setNames(registry$color, registry$label)

analysis_days <- -80:-1
interval_primary <- levels(v4_interval_map(analysis_days))
# Display the retrospective domain at regular 10-day intervals while retaining
# the final modeled day (-1), which does not fall on the 10-day sequence.
functional_day_breaks <- c(seq(-80, -10, by = 10), -1)

theme_paper <- function(base_size = 9.2) {
  theme_minimal(base_size = base_size, base_family = "sans") +
    theme(panel.grid.minor = element_blank(), panel.grid.major = element_line(color = "#E5E7EB", linewidth = .35),
          strip.text = element_text(face = "bold", color = "#111111"),
          axis.title = element_text(face = "bold"), axis.text = element_text(color = "#3F3F46"),
          legend.position = "bottom", legend.title = element_blank(),
          plot.margin = margin(7, 8, 7, 7))
}
save_final <- function(x, name, width, height) {
  ggsave(file.path(out, name), x, width = width, height = height, dpi = 320, bg = "white")
}
facet_process <- function(x) {
  x <- x[x$process %in% proc_order, , drop = FALSE]
  x$process <- factor(x$process, levels = proc_order, labels = label_lookup[proc_order])
  x
}

# Highlight contiguous lag regions where the primary 95% simultaneous
# confidence band for the daily coefficient function excludes zero.  The
# shading therefore corresponds exactly to the colored ribbon, rather than to
# the separate integrated 10-day contrasts.
functional_band_days <- read_final("primary_functional_curves.csv")
functional_band_days <- functional_band_days[
  functional_band_days$process %in% proc_order,
  c("process", "lag_day", "conf_low_per_process_sd", "conf_high_per_process_sd"),
  drop = FALSE
]
functional_band_days$direction <- ifelse(
  functional_band_days$conf_low_per_process_sd > 0, "positive",
  ifelse(functional_band_days$conf_high_per_process_sd < 0, "negative", NA_character_)
)
band_regions <- lapply(split(functional_band_days, functional_band_days$process), function(z) {
  z <- z[order(z$lag_day), , drop = FALSE]
  active <- !is.na(z$direction)
  boundary <- c(TRUE,
                diff(z$lag_day) != 1L |
                  active[-1L] != active[-length(active)] |
                  ifelse(active[-1L] & active[-length(active)],
                         z$direction[-1L] != z$direction[-nrow(z)], FALSE))
  z$region <- cumsum(boundary)
  z <- z[active, , drop = FALSE]
  if (!nrow(z)) return(NULL)
  do.call(rbind, lapply(split(z, z$region), function(r) {
    data.frame(process = r$process[1], xmin = min(r$lag_day) - 0.5,
               xmax = max(r$lag_day) + 0.5, direction = r$direction[1])
  }))
})
band_regions <- Filter(Negate(is.null), band_regions)
functional_band_regions <- consolidated_bind(band_regions)
functional_band_regions$process <- factor(
  functional_band_regions$process,
  levels = proc_order,
  labels = label_lookup[proc_order]
)
functional_band_shadow <- function() {
  geom_rect(
    data = functional_band_regions,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
    inherit.aes = FALSE, fill = "#D9D9D9", alpha = 0.42, linewidth = 0
  )
}

# Figure 1 is generated by the spatial script directly into the final folder.
Sys.setenv(FINAL_FIGURE_DIR = out)
source("analysis/scripts/10_trial_location_map.R", local = new.env(parent = globalenv()))

primary_b <- facet_process(read_final("primary_broad_coefficients.csv"))
primary_b$interval <- factor(primary_b$interval, levels = interval_primary)
g2 <- ggplot(primary_b, aes(interval, estimate, group = process, color = process)) +
  geom_hline(yintercept = 0, color = "#8798AA", linewidth = .45) +
  geom_errorbar(aes(ymin = conf_low, ymax = conf_high), width = 0, linewidth = .55) +
  geom_point(size = 1.9) +
  scale_color_manual(values = display_color_lookup, guide = "none") +
  facet_wrap(~process, ncol = 2, scales = "free_y") + panel_letter_layer(primary_b, "process") +
  labs(x = "Days before grain evaluation",
       y = "DG percentage points per 1-SD higher interval summary") +
  theme_paper() + theme(axis.text.x = element_text(angle = 35, hjust = 1))
save_final(g2, owned[2], 8.1, 8.3)

primary_f <- facet_process(read_final("primary_functional_curves.csv"))
g3 <- ggplot(primary_f, aes(lag_day, beta_per_process_sd, color = process, fill = process)) +
  functional_band_shadow() +
  geom_hline(yintercept = 0, color = "#8798AA", linewidth = .45) +
  geom_ribbon(aes(ymin = conf_low_per_process_sd, ymax = conf_high_per_process_sd),
              alpha = .24, linewidth = 0) +
  geom_line(linewidth = .8) +
  scale_fill_manual(values = display_color_lookup, guide = "none") +
  scale_color_manual(values = display_color_lookup, guide = "none") +
  facet_wrap(~process, ncol = 2, scales = "free_y") + panel_letter_layer(primary_f, "process") +
  labs(x = "Days before grain evaluation",
       y = "Daily association with DG per 1-SD higher process") +
  scale_x_continuous(limits = range(analysis_days), breaks = functional_day_breaks) + theme_paper()
save_final(g3, owned[3], 8.1, 8.3)

# Additional descriptive uncertainty view. Unlike Figure 3's simultaneous
# band, this ribbon is formed directly from the 2.5th and 97.5th percentiles
# of the fitted coefficient at each lag across the cluster-bootstrap samples.
g3b <- ggplot(primary_f, aes(lag_day, beta_per_process_sd, color = process, fill = process)) +
  geom_hline(yintercept = 0, color = "#8798AA", linewidth = .45) +
  geom_ribbon(aes(ymin = pointwise_conf_low_per_process_sd,
                  ymax = pointwise_conf_high_per_process_sd),
              alpha = .24, linewidth = 0) +
  geom_line(linewidth = .8) +
  scale_fill_manual(values = display_color_lookup, guide = "none") +
  scale_color_manual(values = display_color_lookup, guide = "none") +
  facet_wrap(~process, ncol = 2, scales = "free_y") + panel_letter_layer(primary_f, "process") +
  labs(x = "Days before grain evaluation",
       y = "Daily association with DG per 1-SD higher process") +
  scale_x_continuous(limits = range(analysis_days), breaks = functional_day_breaks) + theme_paper()
save_final(g3b, "figure3b_functional_pointwise_bootstrap.png", 8.1, 8.3)

trials <- readRDS(file.path(p$processed, "trial_cohort_v4_with_outcome_flags.rds"))
trials$log_ga <- log1p(trials$ga)

# Four-panel outcome description: overall distributions on the original and
# transformed scales, followed by original-scale comparisons by season and
# cultivar. Histograms show distribution shape; boxplots retain trial points.
trials$season_plot <- factor(
  trials$season,
  levels = c("2022/2023", "2023/2024", "2024/2025", "2025/2026"),
  labels = c("22/23", "23/24", "24/25", "25/26")
)
trials$cultivar_plot <- factor(
  trials$cultivar,
  levels = c("Desafio RR", "Olimpo"),
  labels = c("Brasmax Desafio RR", "Brasmax Olimpo IPRO")
)
outcome_hist_theme <- theme_paper() + theme(legend.position = "none")
g8a <- ggplot(trials, aes(ga)) +
  geom_histogram(bins = 12, boundary = 0, closed = "left",
                 fill = "#4682B4", color = "white", linewidth = .35) +
  geom_vline(xintercept = median(trials$ga), linetype = "dashed",
             color = "#243B53", linewidth = .65) +
  labs(x = "Damaged grain (%)", y = "Number of trials") +
  outcome_hist_theme + single_panel_letter("A")
g8b <- ggplot(trials, aes(log_ga)) +
  geom_histogram(bins = 12, fill = "#4682B4", color = "white", linewidth = .35) +
  geom_vline(xintercept = median(trials$log_ga), linetype = "dashed",
             color = "#243B53", linewidth = .65) +
  labs(x = "log(1 + damaged grain)", y = "Number of trials") +
  outcome_hist_theme + single_panel_letter("B")
g8c <- ggplot(trials, aes(season_plot, ga, fill = season_plot)) +
  geom_boxplot(width = .62, outlier.shape = NA, color = "#374151", linewidth = .55) +
  geom_jitter(width = .12, size = 1.05, alpha = .55, color = "#1F2937") +
  scale_fill_manual(values = c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3")) +
  labs(x = "Growing season", y = "Damaged grain (%)") +
  outcome_hist_theme + single_panel_letter("C")
g8d <- ggplot(trials, aes(cultivar_plot, ga, fill = cultivar_plot)) +
  geom_boxplot(width = .56, outlier.shape = NA, color = "#374151", linewidth = .55) +
  geom_jitter(width = .11, size = 1.05, alpha = .55, color = "#1F2937") +
  scale_fill_manual(values = c("#4682B4", "#9CC3D5")) +
  labs(x = "Cultivar", y = "Damaged grain (%)") +
  outcome_hist_theme + single_panel_letter("D")
save_final((g8a | g8b) / (g8c | g8d), "figure8_ga_distributions.png", 8.4, 7.3)

response_sd <- c("Primary" = sd(trials$ga), "log1p(GA)" = sd(trials$log_ga),
                 "Exclude high GA" = sd(trials$ga[!trials$outcome_influence_flag]),
                 "Sowing day + cultivar" = sd(trials$ga),
                 "Cycle duration + cultivar" = sd(trials$ga))
sens_b <- rbind(read_final("primary_broad_coefficients.csv"),
                read_final("sensitivity_broad_coefficients.csv"))
sens_b$outcome_sd <- unname(response_sd[sens_b$scenario])
sens_b$scenario <- factor(sens_b$scenario,
                          levels = c("Primary", "log1p(GA)",
                                     "Exclude high GA",
                                     "Sowing day + cultivar", "Cycle duration + cultivar"))
sens_b$std_estimate <- sens_b$estimate / sens_b$outcome_sd
sens_b$std_low <- sens_b$conf_low / sens_b$outcome_sd
sens_b$std_high <- sens_b$conf_high / sens_b$outcome_sd
sens_b <- facet_process(sens_b)
sens_b$interval <- factor(sens_b$interval, levels = interval_primary)
g4 <- ggplot(sens_b, aes(interval, std_estimate, color = scenario, group = scenario)) +
  geom_hline(yintercept = 0, color = "#8798AA", linewidth = .4) +
  geom_errorbar(aes(ymin = std_low, ymax = std_high), width = 0,
                position = position_dodge(width = .62), alpha = .35, linewidth = .55) +
  geom_point(position = position_dodge(width = .62), size = 1.25) +
  facet_wrap(~process, ncol = 2, scales = "free_y") + panel_letter_layer(sens_b, "process") +
  labs(x = "Days before grain evaluation", y = "Outcome SD per exposure-summary SD") +
  theme_paper(8.6) + theme(axis.text.x = element_text(angle = 40, hjust = 1),
                           legend.text = element_text(size = 7.5)) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))
save_final(g4, owned[4], 9.0, 9.0)

loso_b <- read_final("loso_broad_coefficients.csv")
season_sd <- setNames(vapply(unique(trials$season), function(s)
  sd(trials$ga[trials$season != s]), numeric(1)), paste("Omit", unique(trials$season)))
loso_b$outcome_sd <- unname(season_sd[loso_b$scenario])
loso_b$std_estimate <- loso_b$estimate / loso_b$outcome_sd
loso_b$std_low <- loso_b$conf_low / loso_b$outcome_sd
loso_b$std_high <- loso_b$conf_high / loso_b$outcome_sd
loso_b <- facet_process(loso_b)
loso_b$interval <- factor(loso_b$interval, levels = interval_primary)
g5 <- ggplot(loso_b, aes(interval, std_estimate, color = scenario, group = scenario)) +
  geom_hline(yintercept = 0, color = "#8798AA", linewidth = .4) +
  geom_errorbar(aes(ymin = std_low, ymax = std_high), width = 0,
                position = position_dodge(width = .62), alpha = .35, linewidth = .55) +
  geom_point(position = position_dodge(width = .62), size = 1.3) +
  facet_wrap(~process, ncol = 2, scales = "free_y") + panel_letter_layer(loso_b, "process") +
  labs(x = "Days before grain evaluation", y = "Outcome SD per exposure-summary SD") +
  theme_paper(8.6) + theme(axis.text.x = element_text(angle = 40, hjust = 1),
                           legend.text = element_text(size = 7.5))
save_final(g5, owned[5], 9.0, 9.0)

# Continuous scalar-on-function stability views corresponding to Figures 4 and 5.
sens_f <- rbind(read_final("primary_functional_curves.csv"),
                read_final("sensitivity_functional_curves.csv"))
sens_f$outcome_sd <- unname(response_sd[sens_f$scenario])
sens_f$std_beta <- sens_f$beta_per_process_sd / sens_f$outcome_sd
sens_f$scenario <- factor(sens_f$scenario,
                          levels = c("Primary", "log1p(GA)",
                                     "Exclude high GA",
                                     "Sowing day + cultivar", "Cycle duration + cultivar"))
sens_f <- facet_process(sens_f)
g4b <- ggplot(sens_f, aes(lag_day, std_beta, color = scenario, group = scenario)) +
  functional_band_shadow() +
  geom_hline(yintercept = 0, color = "#8798AA", linewidth = .4) +
  geom_line(linewidth = .65) + facet_wrap(~process, ncol = 2, scales = "free_y") +
  panel_letter_layer(sens_f, "process") +
  scale_x_continuous(limits = range(analysis_days), breaks = functional_day_breaks) +
  labs(x = "Days before grain evaluation", y = "Outcome SD per 1-SD higher daily process") +
  theme_paper(8.6) + theme(legend.text = element_text(size = 7.5)) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))
save_final(g4b, "figure4b_functional_shared_sensitivities.png", 9.0, 9.0)

loso_f <- read_final("loso_functional_curves.csv")
loso_f$outcome_sd <- unname(season_sd[loso_f$scenario])
loso_f$std_beta <- loso_f$beta_per_process_sd / loso_f$outcome_sd
loso_f <- facet_process(loso_f)
g5b <- ggplot(loso_f, aes(lag_day, std_beta, color = scenario, group = scenario)) +
  functional_band_shadow() +
  geom_hline(yintercept = 0, color = "#8798AA", linewidth = .4) +
  geom_line(linewidth = .65) + facet_wrap(~process, ncol = 2, scales = "free_y") +
  panel_letter_layer(loso_f, "process") +
  scale_x_continuous(limits = range(analysis_days), breaks = functional_day_breaks) +
  labs(x = "Days before grain evaluation", y = "Outcome SD per 1-SD higher daily process") +
  theme_paper(8.6) + theme(legend.text = element_text(size = 7.5))
save_final(g5b, "figure5b_functional_loso_stability.png", 9.0, 9.0)

# Crop-age diagnostic uses the corrected grain-evaluation duration.
imap <- v4_interval_map(analysis_days)
das <- do.call(rbind, lapply(seq_len(nrow(trials)), function(i) do.call(rbind,
                                                                        lapply(levels(imap), function(int) {
                                                                          lags <- analysis_days[imap == int]
                                                                          data.frame(trial_id = trials$trial_id[i], interval = int,
                                                                                     midpoint = mean(trials$cycle_days[i] + lags))
                                                                        }))))
das$interval <- factor(das$interval, levels = interval_primary)
g6 <- ggplot(das, aes(interval, midpoint)) + geom_boxplot(fill = "#9CC3D5", color = "#375A6D", width = .65) +
  geom_jitter(width = .12, alpha = .32, size = .8, color = "#264653") +
  labs(x = "Days before grain evaluation", y = "Days after sowing at interval midpoint") +
  theme_paper() + theme(axis.text.x = element_text(angle = 35, hjust = 1)) + single_panel_letter("A")
save_final(g6, owned[6], 7.2, 4.8)

# S1: correlations of matching broad-interval summaries.
weather <- consolidated_add_processes(readRDS(file.path(p$processed, "weather_era5_absolute_days.rds")))
features <- list()
for (i in seq_len(nrow(registry))) {
  proc <- registry$process[i]
  mat <- v4_process_matrix(weather, trials, proc, analysis_days)
  f <- consolidated_interval_features(mat, analysis_days, proc, registry$interval_summary[i])
  names(f) <- interval_primary
  features[[proc]] <- f
}
corr <- Reduce(`+`, lapply(interval_primary, function(int) {
  x <- as.data.frame(lapply(proc_order, function(proc) features[[proc]][[int]]))
  names(x) <- proc_order
  cor(x)
})) / length(interval_primary)
corr_long <- as.data.frame(as.table(corr), stringsAsFactors = FALSE)
names(corr_long) <- c("process_1", "process_2", "correlation")
corr_long$process_1 <- factor(corr_long$process_1, proc_order, label_lookup[proc_order])
corr_long$process_2 <- factor(corr_long$process_2, rev(proc_order), rev(label_lookup[proc_order]))
gs1 <- ggplot(corr_long, aes(process_1, process_2, fill = correlation)) + geom_tile() +
  scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B", midpoint = 0,
                       limits = c(-1, 1), name = "Mean r") +
  coord_equal() + labs(x = NULL, y = NULL) + theme_paper() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.grid = element_blank()) + single_panel_letter("A")
save_final(gs1, owned[7], 7.2, 6.3)

# S2: in-sample contribution and descriptive support across stress tests.
bg <- read_final("primary_broad_global_tests.csv")
fg <- read_final("primary_functional_global_tests.csv")
fit <- rbind(data.frame(process = bg$process, method = "Broad intervals", value = bg$incremental_r_squared),
             data.frame(process = fg$process, method = "Scalar on function", value = fg$incremental_r_squared))
fit <- fit[fit$process %in% proc_order, , drop = FALSE]
fit$process <- factor(fit$process, proc_order, label_lookup[proc_order])
support <- read_final("process_support_summary.csv")
support_long <- rbind(data.frame(process = support$process, method = support$method,
                                 set = "Shared sensitivities", value = support$sensitivity_support_fraction_p_le_0_10),
                      data.frame(process = support$process, method = support$method,
                                 set = "Season omissions", value = support$loso_support_fraction_p_le_0_10))
support_long <- support_long[support_long$process %in% proc_order, , drop = FALSE]
support_long$process <- factor(support_long$process, proc_order, label_lookup[proc_order])
pa <- ggplot(fit, aes(process, value, fill = method)) + geom_col(position = position_dodge(.7), width = .65) +
  labs(x = NULL, y = expression("Incremental "*R^2)) + theme_paper() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + single_panel_letter("A")
pb <- ggplot(support_long, aes(process, value, fill = set)) + geom_col(position = position_dodge(.7), width = .65) +
  facet_wrap(~method) + scale_y_continuous(limits = c(0, 1)) +
  labs(x = NULL, y = "Fraction with process-level P <= 0.10") + theme_paper() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + single_panel_letter("B")
save_final(pa / pb, owned[8], 8.3, 8.0)

# S3: compact view of the fitted functional coefficients.
heat <- primary_f
heat$process <- factor(heat$process, levels = rev(levels(heat$process)))
gs3 <- ggplot(heat, aes(lag_day, process, fill = beta_per_process_sd)) + geom_tile() +
  scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B", midpoint = 0,
                       name = "Association\nwith DG") +
  scale_x_continuous(limits = c(-80.5, -0.5), breaks = functional_day_breaks,
                     expand = expansion(mult = 0)) +
  geom_vline(xintercept = seq(-70.5, -10.5, 10), color = "white", linewidth = .35) +
  labs(x = "Days before grain evaluation", y = NULL) + theme_paper() + theme(panel.grid = element_blank())
save_final(gs3, owned[9], 8.1, 4.7)

# S4-S7: residual-versus-fitted and normal Q-Q displays remain separate for
# each model family so the two diagnostics can be inspected independently.
resid <- utils::read.csv(file.path(diagnostics, "primary_model_residuals.csv"), stringsAsFactors = FALSE)
resid <- resid[resid$process %in% proc_order, , drop = FALSE]
resid$process <- factor(resid$process, proc_order, label_lookup[proc_order])
for (method in c("Broad intervals", "Scalar on function")) {
  z <- resid[resid$method == method, ]
  a <- ggplot(z, aes(fitted, standardized_residual)) + geom_hline(yintercept = 0, color = "#8798AA") +
    geom_point(color = "#2C6E9F", alpha = .7, size = 1.15) + geom_smooth(method = "loess", se = FALSE,
                                                                         color = "#B23A48", linewidth = .65) + facet_wrap(~process, ncol = 2, scales = "free_x") +
    labs(x = "Fitted DG", y = "Standardized residual") + theme_paper(8.5) + panel_letter_layer(z, "process")
  b <- ggplot(z, aes(sample = standardized_residual)) + stat_qq(color = "#2C6E9F", size = 1.05) +
    stat_qq_line(color = "#475569", linewidth = .55) + facet_wrap(~process, ncol = 2) +
    labs(x = "Theoretical quantile", y = "Observed quantile") + theme_paper(8.5) + panel_letter_layer(z, "process")
  if (method == "Broad intervals") {
    save_final(a, owned[10], 9.0, 7.5)
    save_final(b, owned[11], 9.0, 7.5)
  } else {
    save_final(a, owned[12], 9.0, 7.5)
    save_final(b, owned[13], 9.0, 7.5)
  }
}

# S8-S9: REML-penalized refund sensitivity. These are the only final figures
# promoted from the sensitivity branch: daily coefficient functions with
# simultaneous bands and integrated 10-day contrasts.
refund_tables <- file.path("analysis", "refund_sensitivity", "outputs", "tables")
refund_required <- file.path(refund_tables, c(
  "refund_functional_curves.csv", "refund_interval_contrasts.csv"
))
if (!all(file.exists(refund_required))) {
  stop("Missing REML-penalized refund sensitivity tables. Run the refund fitting stage first.")
}

refund_f <- utils::read.csv(refund_required[1], check.names = FALSE,
                            stringsAsFactors = FALSE)
primary_refund_f <- read_final("primary_functional_curves.csv")
primary_refund_f$method <- "Primary fixed 4-df spline"
refund_f$method <- "REML-penalized refund"
refund_curve <- rbind(
  primary_refund_f[c("process", "process_label", "lag_day", "beta_per_process_sd",
                     "conf_low_per_process_sd", "conf_high_per_process_sd", "method")],
  refund_f[c("process", "process_label", "lag_day", "beta_per_process_sd",
             "conf_low_per_process_sd", "conf_high_per_process_sd", "method")]
)
refund_curve <- facet_process(refund_curve)
refund_curve$method <- factor(refund_curve$method,
                              levels = c("Primary fixed 4-df spline",
                                         "REML-penalized refund"))
refund_method_colors <- c("Primary fixed 4-df spline" = "#2C7FB8",
                          "REML-penalized refund" = "#D95F0E")
gs8 <- ggplot(refund_curve,
              aes(lag_day, beta_per_process_sd, color = method, fill = method)) +
  geom_hline(yintercept = 0, color = "#8798AA", linewidth = .45) +
  geom_ribbon(aes(ymin = conf_low_per_process_sd,
                  ymax = conf_high_per_process_sd),
              alpha = .12, linewidth = 0) +
  geom_line(aes(linetype = method), linewidth = .8) +
  scale_color_manual(values = refund_method_colors) +
  scale_fill_manual(values = refund_method_colors) +
  scale_linetype_manual(values = c("solid", "22")) +
  facet_wrap(~process, ncol = 2, scales = "free_y") +
  panel_letter_layer(refund_curve, "process") +
  scale_x_continuous(limits = range(analysis_days),
                     breaks = functional_day_breaks) +
  labs(x = "Days before grain evaluation",
       y = "Daily association with DG per 1-SD higher process") +
  theme_paper(8.7) + theme(legend.text = element_text(size = 7.5))
save_final(gs8, "figureS8_refund_penalized_curves.png", 8.5, 7.0)

refund_c <- utils::read.csv(refund_required[2], check.names = FALSE,
                            stringsAsFactors = FALSE)
primary_refund_c <- read_final("primary_functional_contrasts.csv")
primary_refund_c$method <- "Primary fixed 4-df spline"
refund_c$method <- "REML-penalized refund"
refund_contrast <- rbind(
  primary_refund_c[c("process", "process_label", "interval", "estimate",
                     "conf_low", "conf_high", "method")],
  refund_c[c("process", "process_label", "interval", "estimate",
             "conf_low", "conf_high", "method")]
)
refund_contrast <- facet_process(refund_contrast)
refund_contrast$interval <- factor(refund_contrast$interval, levels = interval_primary)
refund_contrast$method <- factor(refund_contrast$method,
                                 levels = c("Primary fixed 4-df spline",
                                            "REML-penalized refund"))
gs9 <- ggplot(refund_contrast,
              aes(interval, estimate, color = method, group = method)) +
  geom_hline(yintercept = 0, color = "#8798AA", linewidth = .45) +
  geom_errorbar(aes(ymin = conf_low, ymax = conf_high), width = 0,
                position = position_dodge(width = .5), linewidth = .45) +
  geom_point(position = position_dodge(width = .5), size = 1.65) +
  scale_color_manual(values = refund_method_colors) +
  facet_wrap(~process, ncol = 2, scales = "free_y") +
  panel_letter_layer(refund_contrast, "process") +
  labs(x = "Days before grain evaluation", y = "Integrated DG contrast") +
  theme_paper(8.7) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1),
        legend.text = element_text(size = 7.5))
save_final(gs9, "figureS9_refund_penalized_contrasts.png", 8.7, 7.2)

manifest <- v4_manifest(file.path(out, owned), getwd())
v4_write_csv(manifest, file.path(p$freeze, "final", "final_figure_manifest.csv"))
capture.output(sessionInfo(), file = file.path(p$provenance, "sessionInfo_final_figures.txt"))
message("Rendered the final analysis figure set only: ", length(owned), " files.")
