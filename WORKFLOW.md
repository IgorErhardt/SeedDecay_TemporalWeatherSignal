# Consolidated corrected-date workflow

The active analysis has one statistical engine and one final-figure renderer.
Historical modular implementations are retained only in the recoverable legacy
archive documented in `ARCHIVE_LOCATION.txt`; they are not executed.

## Scientific specification

- Cohort: 72 trials; meteorological exposure units are re-audited over the
  complete 80-day weather history on every run.
- Outcome: damaged grain (DG/GA) in percentage points.
- Source date: retained as `recorded_evaluation_date`.
- Operational grain-evaluation date: source `evaluation_date + 20 days`, stored
  as `grain_evaluation_date` and used as `evaluation_date` by model code.
- Primary weather domain: days -80 through -1 before grain evaluation; day 0 is
  excluded.
- Weather: pinned ERA5 through `r4pde::get_era5(models = "era5")`.
- Broad models: eight fixed 10-day intervals, season adjustment, one process
  per model.
- Functional models: four cubic B-spline basis functions, season adjustment,
  and season-stratified meteorological-unit cluster bootstrap bands.

## Active execution path

```text
raw inputs + validated ERA5 cache
        |
        v
01_prepare_data_weather.R
  cohort, +20-day correction, weather QA, exposure units, input freeze
        |
        v
02_fit_consolidated_analysis.R
  one registry-driven engine for the four retained processes; primary fits,
  three shared sensitivities, four season omissions, residual diagnostics
        |
        v
03_render_figures.R
  one renderer; exactly 17 final analysis figures
        |
        v
04_verify_analysis.R
  date/domain checks, process coverage, numerical parity, manifests
```

Run from the project root:

```powershell
& 'C:\Program Files\R\R-4.5.0\bin\Rscript.exe' 'run_final_analysis.R'
```

`analysis/run_all.R` is a backward-compatible wrapper for the same command.

## Single owners

| Responsibility | Active owner |
|---|---|
| Cohort, date correction, ERA5 cache, weather QA, exposure units | `analysis/scripts/01_prepare_data_weather.R` |
| Process definitions and display metadata | `analysis/config/process_registry.csv` |
| Shared fitting functions | `analysis/R/consolidated_analysis_utils.R` |
| All primary, sensitivity and LOSO models | `analysis/scripts/02_fit_consolidated_analysis.R` |
| All final figures | `analysis/scripts/03_render_figures.R` |
| Final audit | `analysis/scripts/04_verify_analysis.R` |

No active fitting script generates figures, and no active figure script refits
a model. This removes the former dependency cycle and prevents alternative
figure versions from being recreated.

## Active outputs

- Tables: `analysis/outputs/tables/final/`
- Models: `analysis/models/final/`
- Diagnostics: `analysis/outputs/diagnostics/final/`
- Figures: `figures/`
- Manifests: `analysis/freeze/final/`
- Verification: `analysis/outputs/diagnostics/final/consolidated_verification.csv`

The figure directory is an exact whitelist: 17 analysis figures, including
separate broad-interval and continuous scalar-on-function stability views.
Obsolete split and intermediate variants are not generated.

## Required verification

- 72 trials and the meteorological exposure units reconstructed from the
  complete 80-day histories.
- Uniform 20-day correction and cycle duration recomputed to the corrected date.
- Exactly 80 weather days per trial, spanning -80:-1 with no day 0.
- A pre-sowing exposure audit for the earliest interval; these antecedent days
  are not interpreted as direct crop exposure.
- Four retained processes represented in both model types, three shared sensitivities,
  and four season omissions.
- Agreement between the cached consolidated model object, process registry,
  exported tables, sensitivities, and season-omission scenarios.
- Exactly the expected final figure set and a valid SHA-256 manifest.
