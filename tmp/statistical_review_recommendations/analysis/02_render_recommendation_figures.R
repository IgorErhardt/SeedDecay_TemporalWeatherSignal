#!/usr/bin/env Rscript

.libPaths(c(file.path(getwd(), "R-library"), .libPaths()))
suppressPackageStartupMessages({
  library(ggplot2)
  library(patchwork)
})

root <- file.path("tmp", "statistical_review_recommendations")
tab <- file.path(root, "outputs", "tables")
fig <- file.path(root, "outputs", "figures")
dir.create(fig, recursive = TRUE, showWarnings = FALSE)

read_t <- function(x) read.csv(file.path(tab, x), check.names = FALSE)
order_proc <- c("LogRain", "RH", "Tmax", "Tmin")
labels <- c(LogRain = "Log rainfall amount", RH = "Relative humidity",
            Tmax = "Maximum temperature", Tmin = "Minimum temperature")
intervals <- c("-80:-71", "-70:-61", "-60:-51", "-50:-41",
               "-40:-31", "-30:-21", "-20:-11", "-10:-1")

theme_review <- function(base_size = 10) {
  theme_minimal(base_size = base_size) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_line(color = "#E5E7EB", linewidth = .3),
          strip.text = element_text(face = "bold"),
          axis.title = element_text(face = "bold"),
          legend.position = "bottom",
          plot.title = element_text(face = "bold"))
}

save_plot <- function(p, name, w = 9, h = 6.5) {
  ggsave(file.path(fig, name), p, width = w, height = h, dpi = 320, bg = "white")
}

# Global process inference before and after cell-by-season cluster correction.
naive_b <- read.csv("analysis/outputs/tables/final/primary_broad_global_tests.csv")
naive_f <- read.csv("analysis/outputs/tables/final/primary_functional_global_tests.csv")
cr_b <- subset(read_t("cluster_robust_broad_global_tests.csv"), scenario == "Season only")
cr_f <- subset(read_t("cluster_robust_functional_global_tests.csv"), scenario == "Season only")
global <- rbind(
  data.frame(process = naive_b$process, method = "Broad intervals", inference = "Naive", p = naive_b$p_value),
  data.frame(process = cr_b$process, method = "Broad intervals", inference = "Cell-season CR2", p = cr_b$p_value_CR2),
  data.frame(process = naive_f$process, method = "Scalar-on-function", inference = "Naive", p = naive_f$p_value),
  data.frame(process = cr_f$process, method = "Scalar-on-function", inference = "Cell-season CR2", p = cr_f$p_value_CR2)
)
global$process <- factor(global$process, order_proc, labels[order_proc])
p1 <- ggplot(global, aes(p, process, color = inference, shape = inference)) +
  geom_vline(xintercept = .05, linetype = 2, color = "#64748B") +
  geom_point(size = 2.8, position = position_dodge(width = .45)) +
  scale_x_log10(limits = c(.005, .7), breaks = c(.01, .025, .05, .1, .25, .5)) +
  facet_wrap(~method, ncol = 1) +
  labs(x = "Global process P value (log scale)", y = NULL,
       title = "Global inference before and after cell-by-season clustering") +
  theme_review()
save_plot(p1, "figure1_global_inference_comparison.png", 7.6, 6.2)

# Cluster-robust broad estimates.
bcoef <- subset(read_t("cluster_robust_broad_coefficients.csv"), scenario == "Season only")
bcoef$process <- factor(bcoef$process, order_proc, labels[order_proc])
bcoef$interval <- factor(bcoef$interval, intervals)
p2 <- ggplot(bcoef, aes(interval, estimate)) +
  geom_hline(yintercept = 0, color = "#64748B", linewidth = .4) +
  geom_errorbar(aes(ymin = conf_low_CR2, ymax = conf_high_CR2), width = .12,
                linewidth = .65, color = "#4682B4") +
  geom_point(size = 2, color = "#1D4E73") +
  facet_wrap(~process, ncol = 2, scales = "free_y") +
  labs(x = "Days before grain evaluation", y = "DG percentage points per interval-summary SD",
       title = "Broad-interval estimates with cell-season CR2 intervals") +
  theme_review() + theme(axis.text.x = element_text(angle = 40, hjust = 1))
save_plot(p2, "figure2_cluster_robust_broad_coefficients.png", 9, 7.4)

# Cell-season cluster-bootstrap scalar-on-function curves.
fcurve <- read_t("cell_season_bootstrap_functional_curves.csv")
proc_sd <- setNames(vapply(order_proc, function(nm) {
  wx <- readRDS("analysis/data/processed/weather_era5_absolute_days.rds")
  if (nm == "LogRain") x <- log1p(wx$PRECTOTCORR)
  else if (nm == "Tmin") x <- wx$T2M_MIN
  else if (nm == "Tmax") x <- wx$T2M_MAX
  else x <- wx$RH2M
  sd(x)
}, numeric(1)), order_proc)
fcurve$scale <- proc_sd[fcurve$process]
for (nm in c("beta", "conf_low_simultaneous", "conf_high_simultaneous"))
  fcurve[[nm]] <- fcurve[[nm]] * fcurve$scale
fcurve$process <- factor(fcurve$process, order_proc, labels[order_proc])
p3 <- ggplot(fcurve, aes(lag_day, beta)) +
  geom_hline(yintercept = 0, color = "#64748B", linewidth = .4) +
  geom_ribbon(aes(ymin = conf_low_simultaneous, ymax = conf_high_simultaneous),
              fill = "#4682B4", alpha = .20) +
  geom_line(color = "#1D5F9E", linewidth = .85) +
  facet_wrap(~process, ncol = 2, scales = "free_y") +
  scale_x_continuous(breaks = c(seq(-80, -10, 10), -1)) +
  labs(x = "Days before grain evaluation",
       y = "Daily association per process SD",
       title = "Scalar-on-function estimates with cell-season cluster-bootstrap bands") +
  theme_review()
save_plot(p3, "figure3_cell_season_bootstrap_functional_curves.png", 9, 7.2)

# Adjustment sensitivity under the same CR2 estimator.
cov_b <- read_t("cluster_robust_broad_global_tests.csv")
cov_f <- read_t("cluster_robust_functional_global_tests.csv")
cov <- rbind(data.frame(cov_b, method = "Broad intervals"),
             data.frame(cov_f, method = "Scalar-on-function"))
cov$process <- factor(cov$process, order_proc, labels[order_proc])
p4 <- ggplot(cov, aes(p_value_CR2, scenario, color = method, shape = method)) +
  geom_vline(xintercept = .05, linetype = 2, color = "#64748B") +
  geom_point(size = 2.4, position = position_dodge(width = .45)) +
  scale_x_log10(limits = c(.005, 1), breaks = c(.01, .025, .05, .1, .25, .5, 1)) +
  facet_wrap(~process, ncol = 2) +
  labs(x = "Cell-season CR2 global P value (log scale)", y = NULL,
       title = "Sensitivity to sowing date, cycle duration, and cultivar adjustment") +
  theme_review(9)
save_plot(p4, "figure4_covariate_adjustment_sensitivity.png", 9.2, 7.2)

# LOSO prediction. Each point is a season-centred held-out observation.
lp <- read_t("leave_one_season_out_predictions.csv")
lp$process <- factor(lp$process, order_proc, labels[order_proc])
p5 <- ggplot(lp, aes(observed_centered_ga, predicted_centered_ga, color = omitted_season)) +
  geom_abline(slope = 1, intercept = 0, color = "#334155", linewidth = .45) +
  geom_point(alpha = .72, size = 1.6) +
  facet_grid(method ~ process) +
  coord_equal() +
  labs(x = "Observed season-centred DG", y = "LOSO predicted season-centred DG",
       color = "Held-out season", title = "Leave-one-season-out prediction") +
  theme_review(8.5)
save_plot(p5, "figure5_loso_predictions.png", 11, 6.3)

# Richer penalized k=10 curves.
pk <- read_t("penalized_k10_functional_curves.csv")
pk$scale <- proc_sd[pk$process]
for (nm in c("beta", "conf_low_simultaneous", "conf_high_simultaneous"))
  pk[[nm]] <- pk[[nm]] * pk$scale
pk$process <- factor(pk$process, order_proc, labels[order_proc])
p6 <- ggplot(pk, aes(lag_day, beta)) +
  geom_hline(yintercept = 0, color = "#64748B", linewidth = .4) +
  geom_ribbon(aes(ymin = conf_low_simultaneous, ymax = conf_high_simultaneous),
              fill = "#E69F00", alpha = .18) +
  geom_line(color = "#B45F06", linewidth = .85) +
  facet_wrap(~process, ncol = 2, scales = "free_y") +
  scale_x_continuous(breaks = c(seq(-80, -10, 10), -1)) +
  labs(x = "Days before grain evaluation", y = "Daily association per process SD",
       title = "Richer REML-penalized distributed-lag sensitivity") +
  theme_review()
save_plot(p6, "figure6_penalized_k10_curves.png", 9, 7.2)

# Cross-process correlations at matched intervals.
cc <- read_t("cross_process_correlations_by_interval.csv")
cc$process_1 <- factor(cc$process_1, order_proc, labels[order_proc])
cc$process_2 <- factor(cc$process_2, rev(order_proc), labels[rev(order_proc)])
cc$interval <- factor(cc$interval, intervals)
p7 <- ggplot(cc, aes(process_1, process_2, fill = correlation)) +
  geom_tile(color = "white", linewidth = .35) +
  geom_text(aes(label = sprintf("%.2f", correlation)), size = 2.3) +
  scale_fill_gradient2(low = "#B2182B", mid = "white", high = "#2166AC",
                       midpoint = 0, limits = c(-1, 1)) +
  facet_wrap(~interval, ncol = 4) +
  labs(x = NULL, y = NULL, fill = "Correlation",
       title = "Cross-process correlation within each 10-day interval") +
  theme_review(8) + theme(axis.text.x = element_text(angle = 35, hjust = 1),
                          legend.position = "right")
save_plot(p7, "figure7_cross_process_correlations.png", 11, 6.5)

# Influence diagnostic for the rainfall broad model.
infl <- subset(read_t("influence_diagnostics.csv"), process == "LogRain")
infl$label <- ifelse(infl$cooks_distance > 4 / nrow(infl) | infl$high_ga_top3, infl$trial_id, "")
p8 <- ggplot(infl, aes(reorder(trial_id, cooks_distance), cooks_distance,
                       color = high_ga_top3)) +
  geom_hline(yintercept = 4 / nrow(infl), linetype = 2, color = "#64748B") +
  geom_point(size = 1.8) +
  geom_text(aes(label = label), hjust = -.15, size = 2.7, check_overlap = TRUE) +
  coord_flip(clip = "off") +
  scale_color_manual(values = c(`FALSE` = "#4682B4", `TRUE` = "#B2182B")) +
  labs(x = NULL, y = "Cook's distance", color = "Three highest DG",
       title = "Influence in the log-rainfall broad-interval model") +
  theme_review(8.5)
save_plot(p8, "figure8_lograin_influence.png", 7.8, 8.5)

message("Created 8 recommendation-review figures in ", normalizePath(fig, winslash = "/"))
