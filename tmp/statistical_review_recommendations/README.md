# Statistical review follow-up analyses

This directory contains an isolated implementation of the feasible statistical
recommendations in the external review. The frozen primary analysis under
`analysis/` is read-only and is not modified by this workflow.

## Implemented analyses

- CR2 cluster-robust broad-interval and functional-score inference using 36
  ERA5 cell-by-season clusters.
- Season-stratified cell-by-season cluster bootstrap for the primary
  scalar-on-function coefficient curves.
- Adjustment sensitivities for sowing day-of-year plus cultivar and cycle
  duration plus cultivar.
- Adjusted R-squared, within-season permutation nulls, and leave-one-season-out
  prediction of season-centred damaged-grain percentage.
- A richer REML-penalized distributed-lag sensitivity with maximum basis
  dimension 10 and cell-by-season bootstrap uncertainty.
- Adjacent-interval contrasts, interval VIFs, within-process and cross-process
  correlations, two-process models, influence diagnostics, robust regression,
  and a quasibinomial-logit response sensitivity.

Recommendations requiring unavailable R5/R7 dates, sample mass, explicit trial
type, or hourly weather were documented but not approximated.

## Reproduce

From the project root, run:

```powershell
& 'C:\Program Files\R\R-4.5.0\bin\Rscript.exe' 'tmp\statistical_review_recommendations\analysis\01_run_recommended_analyses.R'
& 'C:\Program Files\R\R-4.5.0\bin\Rscript.exe' 'tmp\statistical_review_recommendations\analysis\02_render_recommendation_figures.R'
& 'C:\Users\Usuario\AppData\Local\Programs\Quarto\bin\quarto.cmd' render 'tmp\statistical_review_recommendations\report\statistical_review_followup.qmd'
& 'C:\Program Files\R\R-4.5.0\bin\Rscript.exe' 'tmp\statistical_review_recommendations\analysis\03_verify_recommendations.R'
```

The main output is `report/statistical_review_followup.html`. Tables, figures,
fitted objects, session information, hashes, and verification results are under
`outputs/`.
