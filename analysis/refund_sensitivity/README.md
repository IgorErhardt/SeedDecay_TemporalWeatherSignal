# `refund` scalar-on-function sensitivity analysis

The active workflow compares the frozen primary scalar-on-function models with
a REML-penalized functional model fitted by `refund::pfr()`. Its purpose is to
determine which temporal associations persist under stronger smoothness
regularization.

The sensitivity analysis uses the same 72-trial cohort, pinned ERA5 weather,
corrected recorded evaluation date, -80:-1-day domain, four retained weather
processes, lag-specific centering, season fixed effect, and
season-stratified meteorological-unit cluster bootstrap as the primary
analysis. It does not modify the primary fitted objects or outputs.

The penalized model uses a linear functional term, cubic P-spline basis with
maximum dimension `k = 4`, a second-order difference penalty, and REML
smoothing selection. Effective degrees of freedom may therefore fall below
four. Parameterization-matched objects remain preserved for provenance but are
not used by the active workflow or comparison report.

Run from the project root:

```powershell
& 'C:\Program Files\R\R-4.5.0\bin\Rscript.exe' 'analysis/refund_sensitivity/run_all.R'
```

The rendered comparison report is written to
`analysis/refund_sensitivity/report/refund_comparison_report.html`.
