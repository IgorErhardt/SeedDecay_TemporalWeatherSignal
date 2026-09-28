# Repository cleanup

The Git-shareable project contains only the current consolidated analysis.
Exploratory model branches, superseded temporal-domain
diagnostics, temporary document renders, and machine-specific package files
were moved to the Git-ignored `_local_archive/` after a separate project backup
was created.

The retained workflow is:

1. `analysis/scripts/01_prepare_data_weather.R`
2. `analysis/scripts/02_fit_consolidated_analysis.R`
3. `analysis/scripts/03_render_figures.R`
4. `analysis/scripts/04_verify_analysis.R`

Run all four stages with `run_final_analysis.R`. The repository retains the raw
inputs, pinned ERA5 cache needed by the workflow, processed analysis datasets,
frozen consolidated model, final statistical tables, diagnostics, figures, and
provenance records. It does not generate a report or manuscript.

Archived material included:

- temporary and QA directories;
- 60- and 70-day weather caches superseded by the current cache;
- pre-consolidation threshold, interaction, and alternative-domain results;
- superseded analysis reports and figure variants;
- the local `R-library/` directory and RStudio session state.

Package versions remain recorded in `required_package_manifest.csv`; use
`install_dependencies.R` to prepare a fresh R installation.
