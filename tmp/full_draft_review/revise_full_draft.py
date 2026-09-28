from pathlib import Path
from zipfile import ZipFile, ZIP_DEFLATED
import shutil
import tempfile
import os

from docx import Document
from docx.shared import RGBColor, Inches, Pt


SOURCE = Path(r"C:\Users\Usuario\Downloads\Modelo_GA_SOJA.docx")
ROOT = Path(r"D:\Igor_Masters\Proj_PodAndGrainRot")
OUTDIR = ROOT / "tmp" / "full_draft_review"
OUTPUT = OUTDIR / "Modelo_GA_SOJA_reviewed_current_analysis.docx"
RED = RGBColor(255, 0, 0)


def replace_paragraph(paragraph, text):
    """Replace a prose paragraph and mark the complete altered text in red."""
    for child in list(paragraph._p):
        if child.tag.endswith("}pPr"):
            continue
        paragraph._p.remove(child)
    run = paragraph.add_run(text)
    run.font.color.rgb = RED


def replace_cell(cell, text):
    """Replace a table cell and mark the updated entry in red."""
    cell.text = text
    for paragraph in cell.paragraphs:
        for run in paragraph.runs:
            run.font.color.rgb = RED


doc = Document(SOURCE)
p = doc.paragraphs

replacements = {
    14: (
        "In recent years, soybean seed decay has led to substantial yield and quality losses in tropical Brazil, "
        "yet the timing of weather conditions associated with damaged grains (DG) remains poorly understood. We "
        "analyzed DG percentages from 72 field trials conducted during four growing seasons (2022/2023 to 2025/2026) "
        "in Mato Grosso and Rondônia using 80-day weather trajectories preceding the corrected grain-evaluation date. "
        "Daily maximum and minimum temperature, relative humidity, and rainfall were obtained from an explicitly "
        "pinned ERA5 product. Each weather variable was analyzed separately using season-adjusted linear regression "
        "across eight prespecified 10-day intervals and scalar-on-function regression. Inference accounted for "
        "dependence among trials sharing an ERA5 grid cell within the same season. Log-transformed rainfall showed "
        "the strongest cross-method concordance. The interval model identified its largest positive estimate during "
        "−50 to −41 days, whereas the scalar-on-function analysis supported a broader positive region extending "
        "approximately from −60 to −31 days. Minimum temperature showed secondary positive estimates earlier and "
        "negative estimates later in the domain, but process-level evidence was weaker and season-dependent. Relative "
        "humidity and maximum temperature were not reproduced as coherent smooth temporal patterns. The rainfall "
        "association weakened after exclusion of the highest-DG trials and varied across leave-one-season-out analyses. "
        "Overall, the most consistent finding was a broad rainfall-associated retrospective period approximately 60 "
        "to 40 days before grain evaluation. This pattern should be considered an observational hypothesis for "
        "prospective microclimate studies and validation in larger independent datasets."
    ),
    24: (
        "In this study, we examined weather conditions associated with soybean grain damage in 72 field experiments "
        "conducted in Mato Grosso and Rondônia, Brazil. We hypothesized that final grain damage may reflect temporally "
        "structured environmental processes occurring before grain evaluation. Because pathogen infection, canopy "
        "microclimate, and intermediate phenological stages were not observed directly, the analysis was designed to "
        "identify retrospective weather-associated patterns rather than infection windows or causal mechanisms."
    ),
    25: (
        "We evaluated 80-day meteorological trajectories preceding the corrected grain-evaluation date using "
        "complementary prespecified 10-day interval and scalar-on-function regression analyses. The objective was to "
        "identify broad temporal patterns in rainfall, relative humidity, and temperature associated with final DG. "
        "Given the modest number of trials, model complexity was deliberately restricted, uncertainty accounted for "
        "clustering by ERA5 grid cell within season, and interpretation emphasized effect magnitude, temporal "
        "concordance, and stability rather than isolated P-value thresholds."
    ),
    30: (
        "The dataset comprised 72 field trials conducted across four growing seasons (2022/2023 to 2025/2026) in "
        "commercial soybean-producing municipalities in Mato Grosso (MT) and Rondônia (RO), Brazil (Fig. 1). These "
        "trials evaluated fungicides or planting dates (Kudlawiec et al. 2023; Belufi et al. 2024; Belufi et al. 2025; "
        "Belufi et al. 2026; Farias-Neto et al. 2023; Farias-Neto et al. 2024). We used data from two major commercial "
        "cultivars considered susceptible to seed decay: Brasmax Desafio RR (8473RSF; n = 52) and Brasmax Olimpo IPRO "
        "(80I82RSF IPRO; n = 20). The interval from sowing to the corrected grain-evaluation date ranged from 93 to 137 "
        "days, with a median of 118 days. The primary response was the mean percentage of damaged grains (DG) from "
        "non-fungicide-treated plots. Samples of 25–125 g of grain per plot were collected after harvest and reduced "
        "using a sample divider or homogenizer. DG was determined according to Brazilian official grain-classification "
        "standards and included burned, moldy, fermented, damaged, immature, shriveled, sprouted, and heat-damaged grains."
    ),
    33: (
        "Daily meteorological series for the coordinates of each trial were retrieved with r4pde::get_era5() using "
        "models = \"era5\", thereby pinning temperature, relative humidity, and precipitation to one named weather "
        "product. Field collaborators clarified that grain evaluation occurred 20 days after the date recorded in the "
        "source evaluation_date field. The operational anchor was therefore defined as the recorded date plus 20 "
        "calendar days (t = 0), and the primary exposure window comprised t = −80 to −1 days; day 0 was excluded. "
        "Because observed intermediate phenological stages were unavailable and sowing-to-evaluation duration varied "
        "among trials, retrospective lags were not assigned automatically to specific crop stages."
    ),
    35: (
        "Figure 1. Geographical distribution of the meteorological exposure locations. Points represent ERA5 grid-cell "
        "centroids assigned to the 72 soybean trials conducted in Mato Grosso (MT) and Rondônia (RO), Brazil, across "
        "the 2022/2023 to 2025/2026 growing seasons. Labels indicate the number of trial records assigned to each grid cell."
    ),
    42: (
        "The 80-day period preceding the corrected grain-evaluation date was divided a priori into eight non-overlapping "
        "10-day intervals: −80 to −71, −70 to −61, −60 to −51, −50 to −41, −40 to −31, −30 to −21, −20 to −11, and "
        "−10 to −1 days. Day 0 was excluded. Each meteorological process was analyzed separately using a Gaussian "
        "multiple linear regression. The eight standardized interval summaries for a process were entered simultaneously, "
        "with growing season included as a fixed effect. The process-specific models can be represented as:"
    ),
    44: (
        "where βpk represents the difference in DG percentage points associated with a one-standard-deviation increase "
        "in process p during interval k, conditional on growing season and the other seven intervals of the same process, "
        "with"
    ),
    46: (
        "Coefficients were estimated using all 72 trials. Statistical uncertainty was calculated with a CR2 "
        "cluster-robust covariance estimator using ERA5 grid-cell-by-season combinations as clusters (36 clusters). "
        "Trials remained separate observations, while residuals were allowed to be correlated and heteroscedastic "
        "within a cluster. For each process, a cluster-robust Wald-type F test with an approximate Hotelling T² "
        "small-sample correction jointly tested whether all eight interval coefficients were zero. This provided the "
        "process-level assessment beyond growing-season differences while accounting for shared weather support within "
        "the same grid cell and season."
    ),
    47: (
        "Incremental R² was calculated as the difference between the ordinary in-sample R² values of the full and "
        "season-only models. It represents the additional proportion of observed DG variation accounted for by the "
        "eight interval summaries beyond growing season. Because it was calculated from the fitting data, incremental "
        "R² was interpreted as an in-sample association measure rather than validated predictive performance."
    ),
    48: (
        "Interval-specific coefficients were reported with Wald-type 95% confidence intervals calculated from CR2 "
        "cluster-robust standard errors and Satterthwaite-adjusted degrees of freedom. Estimates were interpreted with "
        "the process-level test, their uncertainty, concordance with scalar-on-function regression, and stability across "
        "the prespecified sensitivity analyses."
    ),
    51: (
        "Each daily meteorological process was also evaluated separately using scalar-on-function regression. Unlike "
        "the 10-day interval analysis, this approach allowed the association to vary continuously across the 80 days "
        "preceding the corrected grain-evaluation date. For trial i, the model was:"
    ),
    55: (
        "where DGi is the damaged-grain percentage for trial i; Xi(t) is the value of the meteorological process on lag "
        "day t; X̄(t) is the across-trial mean exposure at the same lag; βp(t) is the process-specific time-varying "
        "coefficient function; Seasonis denotes the growing-season indicators; and δs denotes differences in mean DG "
        "among seasons."
    ),
    56: (
        "The coefficient function βp(t) represents the estimated difference in DG percentage points associated with a "
        "one-unit increase in exposure at lag t, conditional on growing season and the smooth temporal structure. It was "
        "represented by a cubic B-spline basis with four fixed degrees of freedom across the full 80-day domain, following "
        "Ramsay and Silverman (2005). Daily exposures were centered at each lag, and the same deliberately low basis "
        "dimension was used for every process. Because adjacent coefficients are mathematically linked through the basis, "
        "interpretation focused on direction, magnitude, and broad temporal location rather than isolated days."
    ),
    57: (
        "The fixed, unpenalized four-degree-of-freedom specification was primary because its low, prespecified complexity "
        "provided a transparent and directly comparable representation across processes without selecting smoothing "
        "separately according to the response. As a sensitivity analysis, models were refitted using refund::pfr() with "
        "a cubic P-spline functional term, maximum basis dimension k = 4, a second-order difference penalty, and smoothing "
        "selected by restricted maximum likelihood. This allowed the effective degrees of freedom to decrease below four "
        "when supported by the data and assessed dependence of the temporal patterns on the absence of penalization."
    ),
    58: (
        "For each process, the joint null hypothesis that all four spline-score coefficients were zero was evaluated using "
        "a cluster-robust Wald-type F test. The CR2 covariance estimator used the 36 ERA5 grid-cell-by-season combinations "
        "as clusters, and an approximate Hotelling T² correction accommodated the limited number of clusters. The test "
        "therefore assessed whether the complete functional trajectory contributed information about DG beyond growing "
        "season while allowing dependence among trials sharing a grid cell within a season."
    ),
    59: (
        "Uncertainty in the coefficient function was estimated using 999 season-stratified cluster-bootstrap samples. "
        "ERA5 grid-cell-by-season clusters were sampled with replacement within season; whenever a cluster was selected, "
        "all trial records belonging to it were included together, without averaging their outcomes. The full functional "
        "model was refitted in each bootstrap sample. A 95% simultaneous confidence band was constructed from the 95th "
        "percentile of the maximum standardized bootstrap deviation across the entire 80-day domain:"
    ),
    61: (
        "where SEboot,p(t) is the bootstrap standard error at lag t and cmaxp,0.95 is the 95th percentile of the maximum "
        "standardized deviations. The band reflects uncertainty in the coefficient function as a whole and controls "
        "simultaneous coverage across the complete lag domain rather than providing separate pointwise intervals."
    ),
    62: (
        "For each prespecified 10-day interval Bk, an interval-integrated contrast Cpk was calculated by summing the "
        "estimated daily coefficient function over the interval:"
    ),
    64: (
        "For every bootstrap sample, coefficients were summed over the same interval. The 95% confidence interval for "
        "each integrated contrast was defined by the 2.5th and 97.5th percentiles of that cluster-bootstrap distribution. "
        "These percentile intervals differ from the simultaneous ribbon: the ribbon evaluates the entire daily curve, "
        "whereas an integrated contrast evaluates the cumulative association within one prespecified interval."
    ),
    68: (
        "Stability analyses assessed whether the estimated associations depended materially on outcome scale, the sparse "
        "upper tail of DG, covariate adjustment, or individual growing seasons. Both primary model types were refitted "
        "using log(1 + DG); after excluding the three outcome-only high-DG observations; with sowing day of year and "
        "cultivar added to growing season; and with sowing-to-evaluation duration and cultivar added to growing season. "
        "Leave-one-season-out refits were conducted separately. Stability was judged by process-level evidence, effect "
        "magnitude and uncertainty, preservation of direction and "
        "broad temporal location, and concordance between the two primary analytical views."
    ),
    82: (
        "Internal season stability was evaluated by fitting four additional models, each omitting one season. Both the "
        "broad-interval and scalar-on-function models were refitted using the remaining three seasons with the same "
        "cell-by-season clustered inference. Because each omission removed a substantial and potentially distinctive "
        "portion of a four-season dataset, these analyses were treated as internal stress tests rather than external "
        "validation. Particular concern was assigned to major reversals, disappearance of the temporal pattern, or "
        "movement to a biologically different part of the retrospective domain."
    ),
    86: (
        "All analyses were conducted in R 4.5.0 (R Core Team 2025). Primary broad-interval and scalar-on-function models "
        "were fitted as Gaussian linear models using base R and cubic B-spline basis functions from splines. Cluster-robust "
        "covariance estimation, Satterthwaite degrees of freedom, and approximate Hotelling T² joint tests were implemented "
        "with clubSandwich using ERA5 grid-cell-by-season clusters. Functional simultaneous bands and integrated-contrast "
        "intervals were obtained from 999 season-stratified cluster-bootstrap samples. The penalized sensitivity used "
        "refund::pfr() with REML-selected smoothing. Data preparation, model fitting, sensitivities, leave-one-season-out "
        "analyses, figures, and verification were implemented in reproducible R scripts. Data and code are available at "
        "https://github.com/IgorErhardt/SeedDecay_TemporalWeatherSignal."
    ),
    94: (
        "Log-transformed rainfall provided the clearest broad-interval pattern, although the CR2 process-level evidence "
        "was modest (global P = 0.083; incremental R² = 0.257) (Table 1). The largest estimate occurred during −50 to "
        "−41 days (+3.40 DG percentage points per one-standard-deviation increase; 95% CI: 1.71 to 5.10), while the "
        "adjacent −60 to −51-day estimate was positive but its interval narrowly included zero (Fig. 3A). Process-level "
        "evidence was similar after log outcome transformation (P = 0.050) and adjustment for sowing day and cultivar "
        "(P = 0.056), but weakened after exclusion of the three high-DG observations (P = 0.313) and with cycle duration "
        "and cultivar adjustment (P = 0.110) (Fig. 4A). Leave-one-season-out global P values ranged from 0.224 to 0.560, "
        "indicating appreciable season dependence (Fig. 5A)."
    ),
    95: (
        "Relative humidity showed modest process-level evidence (global P = 0.074; incremental R² = 0.270) (Table 1). "
        "Positive estimates were supported during −50 to −41 days (+2.55; 95% CI: 0.14 to 4.96) and −20 to −11 days "
        "(+4.22; 95% CI: 1.79 to 6.65) (Fig. 3B), but the separated intervals did not define a single continuous temporal "
        "region. Global P values ranged from 0.040 to 0.241 across the shared sensitivities and from 0.249 to 0.457 after "
        "omitting individual seasons (Figs. 4B and 5B), supporting cautious interpretation."
    ),
    96: (
        "Maximum temperature also showed a heterogeneous broad-interval pattern (global P = 0.070; incremental R² = "
        "0.250). Higher Tmax was associated with greater DG during −40 to −31 days (+5.16; 95% CI: 1.58 to 8.74) and "
        "lower DG during −20 to −11 days (−3.94; 95% CI: −6.79 to −1.08) (Fig. 3C; Table 1). The process-level result "
        "was stronger after excluding the three high-DG observations (P = 0.037) but otherwise remained near P = "
        "0.061–0.104 across the prespecified sensitivities; leave-one-season-out values ranged from 0.057 to 0.354. The "
        "directional reversal and season dependence limit a simple interpretation."
    ),
    97: (
        "Minimum temperature provided weaker broad process-level evidence (global P = 0.309; incremental R² = 0.185). "
        "Supported interval estimates were positive during −70 to −61 days (+6.27; 95% CI: 1.85 to 10.68) and negative "
        "during −30 to −21 days (−3.70; 95% CI: −7.35 to −0.04) (Fig. 3D; Table 1). Global P values ranged from 0.103 "
        "to 0.330 across shared sensitivities and from 0.423 to 0.704 after individual season omissions, providing limited "
        "support for a stable broad-interval Tmin process."
    ),
    98: (
        "Table 1. Summary of the primary 10-day interval regression models. Global P values are from CR2 "
        "cluster-robust Wald-type F tests with approximate Hotelling T² correction using 36 ERA5 grid-cell-by-season "
        "clusters. Interval effects are DG percentage-point differences per one-standard-deviation increase in the "
        "corresponding weather summary, with CR2/Satterthwaite 95% confidence intervals. Incremental R² is the in-sample "
        "increase over the season-only model."
    ),
    101: (
        "Figure 3. Associations between weather conditions during eight prespecified 10-day intervals and damaged-grain "
        "percentage (DG). Points are interval-specific coefficients and vertical lines are CR2/Satterthwaite 95% confidence "
        "intervals from separate season-adjusted models for (A) log-transformed rainfall, (B) relative humidity, (C) "
        "maximum temperature, and (D) minimum temperature. All eight standardized interval summaries for a process were "
        "entered simultaneously. Coefficients are DG percentage-point differences per one-standard-deviation increase, "
        "conditional on season and the other seven intervals of the same process."
    ),
    105: (
        "Figure 4. Sensitivity of the eight 10-day interval estimates to alternative specifications. Points are "
        "standardized coefficients and lines are CR2/Satterthwaite 95% confidence intervals for log(1 + DG), exclusion "
        "of the three high-DG observations, sowing-day-plus-cultivar adjustment, and cycle-duration-plus-cultivar adjustment. "
        "Coefficients are expressed as outcome standard deviations per one-standard-deviation increase in the interval summary."
    ),
    108: (
        "Figure 5. Leave-one-season-out stability of the eight 10-day interval estimates. Points are standardized "
        "coefficients and lines are CR2/Satterthwaite 95% confidence intervals after omitting the indicated season. "
        "Coefficients are outcome standard deviations per one-standard-deviation increase in the interval weather summary."
    ),
    110: (
        "The scalar-on-function analysis provided the strongest process-level evidence for log-transformed rainfall "
        "(global P = 0.011; incremental R² = 0.153) (Fig. 6A; Table 2). The simultaneous confidence band excluded zero "
        "approximately from −59 to −38 days. Positive integrated contrasts occurred during −60 to −51 days (+4.08; 95% "
        "bootstrap CI: 1.44 to 5.72), −50 to −41 days (+4.81; 2.54 to 6.88), and −40 to −31 days (+4.45; 1.19 to 8.21). "
        "Process-level evidence remained under outcome transformation (P = 0.045), sowing-day-plus-cultivar adjustment "
        "(P = 0.043), and cycle-duration-plus-cultivar adjustment (P = 0.026), but weakened after excluding high-DG "
        "observations (P = 0.122). Leave-one-season-out P values ranged from 0.005 to 0.116, showing variable strength "
        "despite a recognizable broad coefficient shape (Figs. 7A and 8A)."
    ),
    111: (
        "Relative humidity showed suggestive functional evidence (global P = 0.057; incremental R² = 0.103) (Fig. 6B; "
        "Table 2). Its integrated contrast for −50 to −41 days was positive (+2.13; 95% bootstrap CI: 0.36 to 3.51), "
        "but the simultaneous confidence band included zero throughout the domain. Global P values ranged from 0.135 to "
        "0.276 across shared sensitivities except in the primary analysis, and from 0.037 to 0.392 across season omissions "
        "(Figs. 7B and 8B). The evidence therefore did not support a consistently resolved smooth RH trajectory."
    ),
    112: (
        "Maximum temperature showed little evidence of a coherent functional association (global P = 0.463; incremental "
        "R² = 0.057) (Fig. 6C; Table 2). Neither the simultaneous band nor any integrated 10-day contrast excluded zero. "
        "Global P values ranged from 0.341 to 0.829 across shared sensitivities and from 0.307 to 0.780 across season "
        "omissions (Figs. 7C and 8C). Thus, the opposing Tmax estimates in the interval model were not reproduced as a "
        "supported smooth temporal pattern."
    ),
    113: (
        "Minimum temperature showed positive functional regions earlier and negative regions later in the domain, but "
        "the process-level evidence was uncertain (global P = 0.112; incremental R² = 0.151) (Fig. 6D; Table 2). Positive "
        "integrated contrasts occurred during −70 to −61 (+4.43; 95% bootstrap CI: 2.13 to 6.83) and −60 to −51 days "
        "(+3.91; 1.32 to 6.69), with negative contrasts during −30 to −21 (−3.66; −5.90 to −1.51) and −20 to −11 days "
        "(−3.36; −5.48 to −1.61). The simultaneous band excluded zero approximately during −69 to −56 and −27 to −14 "
        "days. Nevertheless, global evidence weakened across sensitivities (P = 0.070–0.259) and season omissions "
        "(P = 0.160–0.331), so the temporal reversal remains a secondary, unstable hypothesis."
    ),
    114: (
        "Table 2. Summary of the primary scalar-on-function models. Global P values are from CR2 cluster-robust "
        "Wald-type F tests with approximate Hotelling T² correction using 36 ERA5 grid-cell-by-season clusters. "
        "Integrated contrasts are DG percentage-point differences for a one-standard-deviation higher process across "
        "the complete 10-day interval; 95% intervals are percentile cell-by-season cluster-bootstrap intervals. "
        "Incremental R² is the in-sample increase over the season-only model."
    ),
    117: (
        "Figure 6. Primary low-dimensional scalar-on-function associations. Curves are scaled per one overall standard "
        "deviation of the daily process. Grey ribbons are 95% simultaneous confidence bands obtained from 999 "
        "season-stratified ERA5 grid-cell-by-season cluster-bootstrap samples."
    ),
    119: (
        "Figure 7. Continuous scalar-on-function sensitivity analyses. The red line is the primary fit and its grey "
        "ribbon is the primary 95% simultaneous cell-by-season cluster-bootstrap band. Colored lines show refits using "
        "log(1 + DG), exclusion of the three high-DG observations, sowing-day-plus-cultivar adjustment, or "
        "cycle-duration-plus-cultivar adjustment. Coefficients are standardized within each analysis."
    ),
    121: (
        "Figure 8. Continuous scalar-on-function leave-one-season-out analysis. Colored lines show the coefficient "
        "function after omitting one growing season. The grey ribbon is the primary 95% simultaneous confidence band "
        "obtained from 999 season-stratified ERA5 grid-cell-by-season cluster-bootstrap samples."
    ),
    123: (
        "Log-transformed rainfall showed the strongest concordance between the broad-interval and scalar-on-function "
        "analyses. Both approaches identified positive estimates centered approximately 60 to 40 days before grain "
        "evaluation, while functional integrated contrasts extended through −31 days. The largest estimate in each "
        "analysis occurred during −50 to −41 days: +3.40 DG percentage points in the broad model and +4.81 for the "
        "functional integrated contrast, after their analysis-specific standardization. The values are not identical "
        "estimands, but they agree in direction and temporal location. The REML-penalized sensitivity retained similar "
        "process-level evidence (P = 0.020; incremental R² = 0.149) and positive integrated contrasts from −60 to −31 "
        "days (Figs. S6 and S7), indicating that the rainfall pattern was not dependent on the absence of penalization."
    ),
    124: (
        "Minimum temperature showed secondary cross-method agreement in the direction and timing of selected interval "
        "contrasts: positive estimates earlier and negative estimates later in the domain. The penalized functional model "
        "retained positive contrasts during −70 to −51 days and a negative contrast during −20 to −11 days (Fig. S7). "
        "However, broad and functional CR2 process-level tests were uncertain (P = 0.309 and 0.112), the penalized model "
        "reduced the effective degrees of freedom and had a simultaneous band including zero throughout the domain, and "
        "leave-one-season-out evidence was weak. Tmin should therefore be regarded as a secondary temporal hypothesis, "
        "not a reproducible process-level association."
    ),
    130: (
        "The most consistent result was a broad rainfall-associated retrospective period centered approximately 60 to "
        "40 days before grain evaluation. The temporal location agreed between the prespecified interval and "
        "scalar-on-function analyses, although the clustered global test was stronger for the functional model than for "
        "the interval model. Minimum temperature showed a secondary reversal from positive estimates earlier to negative "
        "estimates later in the domain, but global and leave-one-season-out evidence was weaker. Relative humidity and "
        "maximum temperature were not reproduced as coherent smooth trajectories."
    ),
    131: (
        "The rainfall pattern is consistent with previous observations from tropical Brazil. Lehner et al. (2025) reported "
        "approximately 1.7-fold greater cumulative precipitation during the reproductive stage in a late-planted soybean "
        "trial that also developed substantially more damaged grains. Our analysis adds temporal resolution by locating "
        "the strongest association approximately 60 to 40 days before grain evaluation. Rainfall can promote splash "
        "dispersal, prolong surface wetness, and favor fungal infection or colonization; work with the Diaporthe complex "
        "has shown dispersal from infected debris and latent infection of immature soybean tissues (Kmetz et al. 1979), "
        "and field studies have linked moisture during pod filling and maturation with later seed infection and decay "
        "(Shortt et al. 1981; TeKrony et al. 1983). Nevertheless, intermediate phenology, canopy wetness, pathogen abundance, "
        "and infection were not measured here. The retrospective lags therefore cannot be labeled as a specific crop stage "
        "or infection window, and the association should be interpreted as a broader moisture-related hypothesis."
    ),
    132: (
        "The Tmin contrasts suggest a possible time-dependent thermal pattern, but the evidence is less secure than for "
        "rainfall. Warm nights can reduce soybean seed germination and vigor and alter seed composition during development "
        "(Gibson and Mullen 1996; Keigley and Mullen 1986; Chebrolu et al. 2016; Krishnan et al. 2020), while Diaporthe "
        "infection is temperature dependent and may interact with moisture availability (Rupe 1990; Rupe and Ferriss "
        "1987). These mechanisms provide biological context but do not validate the observed lag pattern. The uncertain "
        "process-level tests, attenuation under penalization, and season dependence mean that the early positive and later "
        "negative Tmin contrasts should be treated as hypotheses for prospective evaluation rather than as evidence of a "
        "demonstrated stage-dependent sequence."
    ),
    133: (
        "A strength of this study was the use of a continuous DG response, preserving quantitative variation and allowing "
        "associations to be expressed in percentage points. Combining eight prespecified 10-day intervals with a "
        "low-dimensional scalar-on-function model constrained temporal complexity while providing complementary temporal "
        "views. Dependence among trials sharing an ERA5 grid cell within a season was addressed through CR2 clustered "
        "inference and cluster bootstrap rather than by treating all trial records as meteorologically independent."
    ),
    134: (
        "Several limitations remain. The 72 trials represented only four growing seasons and 36 ERA5 grid-cell-by-season "
        "clusters, limiting precision for resolving subtle nonlinearities, interactions, or cultivar-specific patterns. "
        "Gridded ERA5 weather may not reproduce canopy-level temperature, humidity, rainfall exposure, or surface wetness. "
        "The operational date anchor required a uniform 20-day correction based on field clarification, and intermediate "
        "phenological observations were unavailable; the same retrospective lag may therefore correspond to different "
        "crop ages among trials. The sparse upper tail of DG also materially informed the rainfall association, as shown "
        "by attenuation after exclusion of the three high-DG observations. Finally, DG is a syndromic endpoint that may "
        "combine pathogen infection, latent colonization, physiological deterioration, and other grain defects. Although "
        "Diaporthe ueckeri and D. longicolla can reproduce pod and grain rot symptoms (Nascimento et al. 2025), diverse "
        "fungal taxa occur in symptomatic and asymptomatic tissues (Ramos et al. 2026); the present associations cannot be "
        "attributed to a single causal agent."
    ),
    135: (
        "Overall, the data support a temporally structured rainfall-associated pattern rather than a sharply defined "
        "infection window or causal mechanism. The most defensible candidate region extends approximately 60 to 40 days "
        "before corrected grain evaluation, with functional evidence extending through −31 days. Temperature and relative "
        "humidity patterns were less stable, and the Tmin reversal remains secondary. Prospective field studies with "
        "measured phenology, canopy microclimate, pathogen dynamics, and a larger number of independent seasons are needed "
        "to test whether the rainfall-associated temporal region is reproducible and epidemiologically informative."
    ),
    184: (
        "Figure S1. Temporal heatmap of primary scalar-on-function coefficient estimates. Rows are separate season-adjusted "
        "models and columns are days from 80 to 1 before the corrected grain-evaluation date. Coefficients are DG percentage "
        "points per one overall standard deviation of the corresponding daily process."
    ),
    194: (
        "Figure S6. Primary unpenalized cubic B-spline coefficient functions with four fixed degrees of freedom (solid blue) "
        "and refund::pfr() REML-penalized cubic P-spline sensitivity functions with maximum basis dimension k = 4 and a "
        "second-order difference penalty (dashed orange). Ribbons are separate 95% simultaneous bands from 999 "
        "season-stratified ERA5 grid-cell-by-season cluster-bootstrap samples. Coefficients are scaled per one overall "
        "standard deviation. Panels show (A) log-transformed rainfall, (B) relative humidity, (C) maximum temperature, "
        "and (D) minimum temperature."
    ),
    197: (
        "Figure S7. Integrated 10-day contrasts from the primary and REML-penalized scalar-on-function models. Points are "
        "coefficient functions integrated over each of the eight prespecified intervals; error bars are 95% percentile "
        "intervals from season-stratified ERA5 grid-cell-by-season cluster-bootstrap samples. Contrasts are DG percentage "
        "points for a one-standard-deviation higher process throughout the interval. Panels show (A) log-transformed "
        "rainfall, (B) relative humidity, (C) maximum temperature, and (D) minimum temperature."
    ),
}

for idx, text in replacements.items():
    replace_paragraph(p[idx], text)


table1 = [
    ["Variable", "Primary global P; incremental R²", "Supported interval(s) and effect on DG (95% CR2 CI)",
     "Shared sensitivity global P", "LOSO global P range"],
    ["Log rainfall", "0.083; 0.257", "−50 to −41: +3.40 (1.71, 5.10)",
     "log(DG): 0.050\nExclude high DG: 0.313\nSowing day + cultivar: 0.056\nCycle duration + cultivar: 0.110", "0.224–0.560"],
    ["Relative humidity", "0.074; 0.270",
     "−50 to −41: +2.55 (0.14, 4.96)\n−20 to −11: +4.22 (1.79, 6.65)",
     "log(DG): 0.040\nExclude high DG: 0.240\nSowing day + cultivar: 0.074\nCycle duration + cultivar: 0.133", "0.249–0.457"],
    ["Maximum temperature", "0.070; 0.250",
     "−40 to −31: +5.16 (1.58, 8.74)\n−20 to −11: −3.94 (−6.79, −1.08)",
     "log(DG): 0.061\nExclude high DG: 0.037\nSowing day + cultivar: 0.070\nCycle duration + cultivar: 0.104", "0.057–0.354"],
    ["Minimum temperature", "0.309; 0.185",
     "−70 to −61: +6.27 (1.85, 10.68)\n−30 to −21: −3.70 (−7.35, −0.04)",
     "log(DG): 0.103\nExclude high DG: 0.330\nSowing day + cultivar: 0.228\nCycle duration + cultivar: 0.326", "0.423–0.704"],
]

table2 = [
    ["Variable", "Primary global P; incremental R²", "Supported integrated interval(s) and contrast (95% bootstrap CI)",
     "Shared sensitivity global P", "LOSO global P range"],
    ["Log rainfall", "0.011; 0.153",
     "−60 to −51: +4.08 (1.44, 5.72)\n−50 to −41: +4.81 (2.54, 6.88)\n−40 to −31: +4.45 (1.19, 8.21)",
     "log(DG): 0.045\nExclude high DG: 0.122\nSowing day + cultivar: 0.043\nCycle duration + cultivar: 0.026", "0.005–0.116"],
    ["Relative humidity", "0.057; 0.103", "−50 to −41: +2.13 (0.36, 3.51)",
     "log(DG): 0.250\nExclude high DG: 0.276\nSowing day + cultivar: 0.147\nCycle duration + cultivar: 0.135", "0.037–0.392"],
    ["Maximum temperature", "0.463; 0.057", "No integrated contrast with 95% CI excluding zero",
     "log(DG): 0.341\nExclude high DG: 0.368\nSowing day + cultivar: 0.829\nCycle duration + cultivar: 0.476", "0.307–0.780"],
    ["Minimum temperature", "0.112; 0.151",
     "−70 to −61: +4.43 (2.13, 6.83)\n−60 to −51: +3.91 (1.32, 6.69)\n−30 to −21: −3.66 (−5.90, −1.51)\n−20 to −11: −3.36 (−5.48, −1.61)",
     "log(DG): 0.070\nExclude high DG: 0.259\nSowing day + cultivar: 0.183\nCycle duration + cultivar: 0.113", "0.160–0.331"],
]

for table, values in zip(doc.tables, (table1, table2)):
    table.autofit = False
    for row, row_values in zip(table.rows, values):
        merged = row.cells[4].merge(row.cells[6])
        visible_cells = [row.cells[0], row.cells[1], row.cells[2], row.cells[3], merged]
        widths = [Inches(0.85), Inches(0.90), Inches(2.00), Inches(1.85), Inches(0.80)]
        for cell, width in zip(visible_cells, widths):
            cell.width = width
        for cell, value in zip(visible_cells, row_values):
            replace_cell(cell, value)
            for paragraph in cell.paragraphs:
                for run in paragraph.runs:
                    run.font.size = Pt(8)

OUTDIR.mkdir(parents=True, exist_ok=True)
doc.save(OUTPUT)

# Replace the embedded figures with the current finalized project figures while
# preserving the document's existing drawing dimensions and placement.
media_replacements = {
    "word/media/image9.png": ROOT / "figures" / "figure1_trial_locations_map.png",
    "word/media/image14.png": ROOT / "figures" / "figure8_ga_distributions.png",
    "word/media/image7.png": ROOT / "figures" / "figure2_all_process_broad_intervals.png",
    "word/media/image5.png": ROOT / "figures" / "figure4_all_process_shared_sensitivities.png",
    "word/media/image13.png": ROOT / "figures" / "figure5_all_process_loso_stability.png",
    "word/media/image11.png": ROOT / "figures" / "figure3_all_process_functional_curves.png",
    "word/media/image4.png": ROOT / "figures" / "figure4b_functional_shared_sensitivities.png",
    "word/media/image3.png": ROOT / "figures" / "figure5b_functional_loso_stability.png",
    "word/media/image2.png": ROOT / "figures" / "figureS3_functional_coefficient_heatmap.png",
    "word/media/image8.png": ROOT / "figures" / "figureS4_broad_residual_vs_fitted.png",
    "word/media/image6.png": ROOT / "figures" / "figureS5_broad_normal_qq.png",
    "word/media/image15.png": ROOT / "figures" / "figureS6_functional_residual_vs_fitted.png",
    "word/media/image12.png": ROOT / "figures" / "figureS7_functional_normal_qq.png",
    "word/media/image10.png": ROOT / "figures" / "figureS8_refund_penalized_curves.png",
    "word/media/image1.png": ROOT / "figures" / "figureS9_refund_penalized_contrasts.png",
}
missing = [str(v) for v in media_replacements.values() if not v.exists()]
if missing:
    raise FileNotFoundError("Missing replacement figures:\n" + "\n".join(missing))

tmp_fd, tmp_name = tempfile.mkstemp(suffix=".docx", dir=OUTDIR)
os.close(tmp_fd)
tmp_docx = Path(tmp_name)
with ZipFile(OUTPUT, "r") as zin, ZipFile(tmp_docx, "w", ZIP_DEFLATED) as zout:
    for info in zin.infolist():
        if info.filename in media_replacements:
            zout.writestr(info, media_replacements[info.filename].read_bytes())
        else:
            zout.writestr(info, zin.read(info.filename))
shutil.move(tmp_docx, OUTPUT)

print(OUTPUT)
