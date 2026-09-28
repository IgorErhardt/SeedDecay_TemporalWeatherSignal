#!/usr/bin/env Rscript

# Focused report: frozen primary versus REML-penalized refund::pfr().
.libPaths(c(file.path(getwd(), "R-library"), .libPaths()))
if (!requireNamespace("ggplot2", quietly = TRUE)) stop("ggplot2 is required.")
source("analysis/R/consolidated_analysis_utils.R")

root <- file.path("analysis", "refund_sensitivity")
table_dir <- file.path(root, "outputs", "tables")
figure_dir <- file.path(root, "outputs", "figures", "penalized_comparison")
report_dir <- file.path(root, "report")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)

registry <- consolidated_registry()
primary <- utils::read.csv("analysis/outputs/tables/final/primary_functional_curves.csv")
primary_contrasts <- utils::read.csv("analysis/outputs/tables/final/primary_functional_contrasts.csv")
primary_global <- utils::read.csv("analysis/outputs/tables/final/primary_functional_global_tests.csv")
penalized <- utils::read.csv(file.path(table_dir, "refund_functional_curves.csv"))
penalized_contrasts <- utils::read.csv(file.path(table_dir, "refund_interval_contrasts.csv"))
penalized_global <- utils::read.csv(file.path(table_dir, "refund_global_tests.csv"))
penalized_loso <- utils::read.csv(file.path(table_dir, "refund_loso_curves.csv"))
penalized_loso_global <- utils::read.csv(file.path(table_dir, "refund_loso_global_tests.csv"))
comparison <- utils::read.csv(file.path(table_dir, "global_and_curve_comparison.csv"))

process_levels <- registry$label[order(registry$display_order)]
functional_day_breaks <- c(seq(-80, -10, by = 10), -1)
methods <- c("Primary fixed 4-df spline", "REML-penalized refund")
cols <- c("Primary fixed 4-df spline" = "#2c7fb8",
          "REML-penalized refund" = "#d95f0e")

primary$method <- methods[1]
penalized$method <- methods[2]
curve <- rbind(
  primary[c("process", "process_label", "lag_day", "beta_per_process_sd",
            "conf_low_per_process_sd", "conf_high_per_process_sd", "method")],
  penalized[c("process", "process_label", "lag_day", "beta_per_process_sd",
              "conf_low_per_process_sd", "conf_high_per_process_sd", "method")]
)
curve$process_label <- factor(curve$process_label, levels = process_levels)
curve$method <- factor(curve$method, levels = methods)

p1 <- ggplot2::ggplot(curve, ggplot2::aes(lag_day, beta_per_process_sd,
                                           color = method, fill = method)) +
  ggplot2::geom_hline(yintercept = 0, color = "#64748b", linewidth = 0.35) +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = conf_low_per_process_sd,
                                    ymax = conf_high_per_process_sd),
                       alpha = 0.12, color = NA) +
  ggplot2::geom_line(ggplot2::aes(linetype = method), linewidth = 0.85) +
  ggplot2::facet_wrap(~process_label, ncol = 2, scales = "free_y") +
  ggplot2::scale_color_manual(values = cols) +
  ggplot2::scale_fill_manual(values = cols) +
  ggplot2::scale_linetype_manual(values = c("solid", "22")) +
  ggplot2::scale_x_continuous(limits = c(-80, -1),
                              breaks = functional_day_breaks) +
  ggplot2::labs(x = "Days before recorded evaluation",
                y = "Daily association with DG per process SD",
                color = NULL, fill = NULL, linetype = NULL) +
  v4_theme() + ggplot2::theme(legend.position = "bottom")
ggplot2::ggsave(file.path(figure_dir, "figure1_penalized_curve_comparison.png"),
                p1, width = 8.5, height = 6.8, dpi = 320, bg = "white")

primary_contrasts$method <- methods[1]
penalized_contrasts$method <- methods[2]
ct <- rbind(
  primary_contrasts[c("process", "process_label", "interval", "estimate",
                      "conf_low", "conf_high", "method")],
  penalized_contrasts[c("process", "process_label", "interval", "estimate",
                        "conf_low", "conf_high", "method")]
)
ct$process_label <- factor(ct$process_label, levels = process_levels)
ct$interval <- factor(ct$interval, levels = levels(v4_interval_map(-80:-1)))
ct$method <- factor(ct$method, levels = methods)
p2 <- ggplot2::ggplot(ct, ggplot2::aes(interval, estimate, color = method,
                                        group = method)) +
  ggplot2::geom_hline(yintercept = 0, color = "#64748b", linewidth = 0.35) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = conf_low, ymax = conf_high),
                         position = ggplot2::position_dodge(width = 0.44), width = 0.14) +
  ggplot2::geom_point(position = ggplot2::position_dodge(width = 0.44), size = 1.8) +
  ggplot2::facet_wrap(~process_label, ncol = 2, scales = "free_y") +
  ggplot2::scale_color_manual(values = cols) +
  ggplot2::labs(x = "10-day interval", y = "Integrated DG contrast", color = NULL) +
  v4_theme() +
  ggplot2::theme(legend.position = "bottom",
                 axis.text.x = ggplot2::element_text(angle = 35, hjust = 1))
ggplot2::ggsave(file.path(figure_dir, "figure2_penalized_interval_comparison.png"),
                p2, width = 9, height = 7, dpi = 320, bg = "white")

penalized_loso$process_label <- factor(penalized_loso$process_label,
                                       levels = process_levels)
p3 <- ggplot2::ggplot(penalized_loso,
                       ggplot2::aes(lag_day, beta_per_process_sd,
                                    color = omitted_season)) +
  ggplot2::geom_hline(yintercept = 0, color = "#64748b", linewidth = 0.35) +
  ggplot2::geom_line(linewidth = 0.65) +
  ggplot2::facet_wrap(~process_label, ncol = 2, scales = "free_y") +
  ggplot2::scale_x_continuous(limits = c(-80, -1),
                              breaks = functional_day_breaks) +
  ggplot2::labs(x = "Days before recorded evaluation",
                y = "Daily association with DG per process SD",
                color = "Season omitted") +
  v4_theme() + ggplot2::theme(legend.position = "bottom")
ggplot2::ggsave(file.path(figure_dir, "figure3_penalized_loso.png"),
                p3, width = 8.5, height = 6.8, dpi = 320, bg = "white")

fmt <- function(x, digits = 3) {
  ifelse(is.na(x), "NA", formatC(x, digits = digits, format = "f"))
}
md_table <- function(df, digits = 3) {
  d <- as.data.frame(lapply(df, function(x) if (is.numeric(x)) fmt(x, digits) else as.character(x)),
                     check.names = FALSE)
  paste(c(
    paste0("| ", paste(names(d), collapse = " | "), " |"),
    paste0("| ", paste(rep("---", ncol(d)), collapse = " | "), " |"),
    apply(d, 1, function(z) paste0("| ", paste(z, collapse = " | "), " |"))
  ), collapse = "\n")
}
supported_intervals <- function(z) {
  x <- z$interval[z$conf_low > 0 | z$conf_high < 0]
  if (length(x)) paste(x, collapse = ", ") else "None"
}

display <- merge(comparison, registry[c("process", "label")], by = "process")
display <- display[match(registry$process, display$process), ]
classification <- c(
  Tmin = "Partially survives; curvature and global support attenuate",
  LogRain = "Survives",
  Tmax = "Unsupported in both",
  RH = "Suggestive, not robust process-level evidence"
)[display$process]
global_table <- data.frame(
  Process = display$label,
  `Primary P` = display$primary_p_value,
  `Penalized P` = display$refund_p_value,
  `Primary incremental R2` = display$primary_incremental_r_squared,
  `Penalized incremental R2` = display$refund_incremental_r_squared,
  `Penalized EDF` = display$refund_edf,
  `Curve correlation` = display$curve_correlation,
  Classification = unname(classification),
  check.names = FALSE
)
interval_table <- do.call(rbind, lapply(registry$process, function(proc) {
  data.frame(
    Process = registry$label[registry$process == proc],
    `Primary intervals excluding zero` = supported_intervals(
      primary_contrasts[primary_contrasts$process == proc, ]),
    `Penalized intervals excluding zero` = supported_intervals(
      penalized_contrasts[penalized_contrasts$process == proc, ]),
    check.names = FALSE
  )
}))
loso_table <- do.call(rbind, lapply(registry$process, function(proc) {
  z <- penalized_loso_global[penalized_loso_global$process == proc, ]
  data.frame(Process = registry$label[registry$process == proc],
             `Penalized LOSO P range` = paste0(fmt(min(z$p_value)), " to ", fmt(max(z$p_value))),
             `Penalized LOSO EDF range` = paste0(fmt(min(z$edf), 2), " to ", fmt(max(z$edf), 2)),
             check.names = FALSE)
}))
utils::write.csv(interval_table, file.path(table_dir, "penalized_association_survival.csv"),
                 row.names = FALSE)

qmd <- c(
  "---",
  "title: \"REML-penalized functional regression sensitivity analysis\"",
  "subtitle: \"Comparison with the frozen primary scalar-on-function model\"",
  "format:",
  "  html:",
  "    toc: true",
  "    embed-resources: true",
  "    theme: cosmo",
  "---",
  "",
  "## Objective",
  "",
  "This report evaluates which temporal weather associations from the fixed four-degree-of-freedom primary model persist when the coefficient function is regularized using REML. The penalized model is a sensitivity analysis with a different smoothness assumption, not a software-equivalence check.",
  "",
  "## Penalized specification",
  "",
  "Both models used the same 72 trials, weather processes, -80:-1-day domain, lag-specific centering, Gaussian damaged-grain response, season fixed effects, and ERA5 cell-by-season cluster bootstrap. The penalized model used `refund::lf()` within `refund::pfr()`, a cubic P-spline with maximum basis dimension `k = 4`, a second-order difference penalty, Riemann integration, and REML smoothing selection. Effective degrees of freedom could therefore be lower than four.",
  "",
  "Uncertainty was estimated using 999 season-stratified ERA5 cell-by-season cluster-bootstrap samples. Daily curves use 95% simultaneous bands based on the maximum standardized bootstrap deviation. Integrated 10-day contrasts use percentile bootstrap intervals.",
  "",
  "## Which associations survived penalization?",
  "",
  md_table(global_table),
  "",
  "**Log rainfall amount clearly survived penalization.** Its curve was nearly unchanged (correlation 0.991), process-level evidence remained similar (primary P = 0.024; penalized P = 0.020), and incremental R2 changed little (0.153 to 0.149). Positive integrated contrasts remained supported during -60:-51, -50:-41, and -40:-31 days.",
  "",
  "**Minimum temperature survived only partially.** The broad early-positive/later-negative direction remained, with positive contrasts during -70:-61 and -60:-51 days and a negative contrast during -20:-11 days. However, REML reduced the curve to approximately 2.00 effective degrees of freedom, process-level P changed from 0.025 to 0.058, incremental R2 decreased from 0.151 to 0.079, and leave-one-season-out P values ranged from 0.006 to 0.703. The detailed Tmin reversal is therefore curvature- and season-sensitive.",
  "",
  "**Relative humidity remained suggestive rather than robust.** The penalized process-level P value was 0.084 and positive integrated contrasts occurred from -50:-21 days, but the primary process-level test was also weak and the leave-one-season-out result varied substantially. Penalization did not establish a stable RH association.",
  "",
  "**Maximum temperature remained unsupported.** Its penalized process-level P value was 0.542, no integrated interval excluded zero, and all leave-one-season-out tests remained weak.",
  "",
  "## Process-level and curve comparison",
  "",
  "![](../outputs/figures/penalized_comparison/figure1_penalized_curve_comparison.png){fig-alt=\"Primary and REML-penalized coefficient curves with simultaneous bands.\"}",
  "",
  "**Figure 1.** Daily coefficient functions from the primary and REML-penalized models. Curves are scaled per one overall standard deviation of each process. Shading represents 95% simultaneous ERA5 cell-by-season cluster-bootstrap bands.",
  "",
  "The penalized simultaneous daily bands included zero throughout the complete domain for all four processes. This stringent whole-curve result does not contradict supported integrated contrasts: an interval contrast estimates the cumulative association across ten adjacent days rather than requiring an individual daily coefficient to exclude zero simultaneously.",
  "",
  "## Integrated 10-day contrasts",
  "",
  md_table(interval_table),
  "",
  "![](../outputs/figures/penalized_comparison/figure2_penalized_interval_comparison.png){fig-alt=\"Primary and REML-penalized integrated interval contrasts.\"}",
  "",
  "**Figure 2.** Integrated coefficient contrasts for the eight prespecified 10-day intervals. Error bars are percentile ERA5 cell-by-season cluster-bootstrap intervals.",
  "",
  "## Leave-one-season-out stability",
  "",
  md_table(loso_table),
  "",
  "![](../outputs/figures/penalized_comparison/figure3_penalized_loso.png){fig-alt=\"Leave-one-season-out REML-penalized coefficient functions.\"}",
  "",
  "**Figure 3.** Penalized coefficient functions after omitting each season. These are internal stability stress tests, not external validation.",
  "",
  "## Overall conclusion",
  "",
  "The broad log-rainfall association is robust to REML penalization and is the clearest surviving temporal process. The Tmin pattern retains its general temporal reversal but loses conventional process-level support and remains highly season-dependent, so it should be treated as a secondary hypothesis. RH remains suggestive and unstable, while Tmax remains unsupported. Penalization therefore strengthens the hierarchy of evidence rather than producing a wholly different substantive conclusion.",
  "",
  paste0("Analysis performed using R **", getRversion(), "** and `refund` **",
         utils::packageVersion("refund"), "**."),
  ""
)
qmd_path <- file.path(report_dir, "refund_comparison_report.qmd")
writeLines(qmd, qmd_path, useBytes = TRUE)
quarto <- c("C:/Users/Usuario/AppData/Local/Programs/Quarto/bin/quarto.exe",
            "C:/Program Files/RStudio/resources/app/bin/quarto/bin/quarto.exe")
quarto <- quarto[file.exists(quarto)][1]
if (is.na(quarto)) stop("Quarto executable not found.")
status <- system2(quarto, c("render", shQuote(qmd_path)))
if (!identical(status, 0L)) stop("Quarto report rendering failed.")
message("Penalized comparison report rendered to ",
        file.path(report_dir, "refund_comparison_report.html"))
