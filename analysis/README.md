# Consolidated absolute-time weather analysis

This directory contains the reproducible analysis of damaged-grain percentage
(GA) using explicitly pinned ERA5 weather. The operational grain-evaluation
date is the originally recorded field date plus 20 days, following clarification
from the evaluators. Both dates are preserved. The primary weather domain is
days -80 through -1 before the corrected grain-evaluation date. The earliest
interval is documented as antecedent weather because it can precede sowing in
some short-cycle trials.

## Analytical views

Four weather processes are defined once in `config/process_registry.csv`:
Tmin, LogRain, Tmax, and RH. Each is analyzed separately using:

1. eight fixed 10-day interval regressions adjusted for season; and
2. four-degree-of-freedom scalar-on-function regression with
   meteorological-unit cluster-bootstrap uncertainty.

No LASSO, automated interval selection, daily search, interaction search, or
primary multivariable weather model is used.

## Run and verify

From the project root:

```powershell
& 'C:\Program Files\R\R-4.5.0\bin\Rscript.exe' 'run_final_analysis.R'
```

The command runs preparation, the shared statistical engine, the sole final
figure renderer, and the consolidated audit. `analysis/run_all.R` is a
backward-compatible wrapper for the same workflow.

The active dependency map and output ownership are documented in
[`../WORKFLOW.md`](../WORKFLOW.md).

## Active outputs

- Analysis data and weather cache: `data/`
- Final fitted objects: `models/final/`
- Final tables: `outputs/tables/final/`
- Final diagnostics: `outputs/diagnostics/final/`
- Final figures: `../figures/`
- Specifications and manifests: `config/` and `freeze/final/`

All estimates are observational associations. The corrected date is not labeled
R7 because a common phenological stage could not be independently verified. A
period before grain evaluation is not automatically an infection window.
