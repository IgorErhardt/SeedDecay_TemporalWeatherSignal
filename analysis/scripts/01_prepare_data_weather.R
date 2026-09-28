#!/usr/bin/env Rscript

.libPaths(c(file.path(getwd(), "R-library"), .libPaths()))
source("analysis/R/v4_utils.R")
p <- v4_init_dirs()

seed_primary <- 24041987L
set.seed(seed_primary)
primary_days <- -80:-1
# Retain the validated 100-day retrieval cache and subset it to the official
# 80-day analysis domain. This avoids unnecessary network calls while keeping
# the meteorological product and provenance unchanged.
retrieval_days_back <- max(100L, abs(min(primary_days)))

# The two raw sources are read directly; no analysis-ready object is inherited.
source_original <- normalizePath("data/raw/trial_data_source.csv", winslash = "/", mustWork = TRUE)
source_validation <- normalizePath("data/raw/ValidationTrials.xlsx", winslash = "/", mustWork = TRUE)

input_manifest <- v4_manifest(c(source_original, source_validation), getwd())
v4_write_csv(input_manifest, file.path(p$provenance, "input_manifest.csv"))

original <- utils::read.csv(source_original, check.names = FALSE,
                            na.strings = c("", "NA"))
original$source_row <- seq_len(nrow(original))
original$sowing_date <- v4_parse_mdy(original[["sowing(mdy)"]])
original$evaluation_date <- v4_parse_mdy(original[["evaluation(mdy)"]])
clean_numeric <- function(x) suppressWarnings(as.numeric(gsub("[^0-9.\\-]", "", as.character(x), perl = TRUE)))
original$ga <- clean_numeric(original$ga)
original$inc_vag <- clean_numeric(original$inc_vag)
original$latitude <- clean_numeric(original$latitude)
original$longitude <- clean_numeric(original$longitude)

required_original <- c("responsible_company", "year", "cultivar", "city", "state",
                       "sowing_date", "evaluation_date", "ga", "latitude", "longitude")
eligible <- stats::complete.cases(original[required_original])
excl_elig <- original[!eligible, , drop = FALSE]
excl_elig$reason <- "Missing required identity, date, outcome, or coordinate field"

orig_ok <- original[eligible, , drop = FALSE]
orig_ok$city_key <- trimws(tools::toTitleCase(tolower(orig_ok$city)))
orig_ok$city_key[orig_ok$city_key == "Planaldo Da Serra"] <- "Planalto Da Serra"
orig_ok$source_key <- paste(orig_ok$year, orig_ok$state, orig_ok$city_key,
                            orig_ok$sowing_date, orig_ok$cultivar,
                            orig_ok$responsible_company, sep = "|")
# Stable identifiers are assigned from outcome-independent descriptors. This
# independently reproduces the documented T036 label without using a processed table.
ord <- with(orig_ok, order(year, state, city_key, sowing_date, cultivar,
                           responsible_company, source_key))
orig_ok <- orig_ok[ord, , drop = FALSE]
orig_ok$trial_id <- sprintf("T%03d", seq_len(nrow(orig_ok)))
err <- orig_ok$trial_id == "T036" & abs(orig_ok$ga - 54.14) < 1e-8
if (sum(err) != 1L) {
  cand <- orig_ok[abs(orig_ok$ga - 54.14) < 1e-8, c("trial_id", "source_row", "source_key", "ga"), drop = FALSE]
  print(cand)
  print(excl_elig[c("source_row", required_original)])
  stop("The prespecified T036 experimental-error record was not uniquely recovered.")
}
excl_err <- orig_ok[err, , drop = FALSE]
excl_err$reason <- "Documented experimental error: GA 54.14% is not a valid biological observation"
orig_keep <- orig_ok[!err, , drop = FALSE]

if (!requireNamespace("readxl", quietly = TRUE)) stop("Package 'readxl' is required.")
val <- as.data.frame(readxl::read_excel(source_validation))
val$source_row <- seq_len(nrow(val))
coord <- strsplit(gsub("\\s+", "", val$Latlon), ",", fixed = FALSE)
val$latitude <- as.numeric(vapply(coord, `[`, character(1), 1))
val$longitude <- as.numeric(vapply(coord, `[`, character(1), 2))
val$sowing_date <- as.Date(val$sowing_date, tz = "UTC")
val$evaluation_date <- as.Date(val$evaluation_date, tz = "UTC")
val$year <- paste(format(val$sowing_date, "%Y"), format(val$evaluation_date, "%Y"), sep = "/")
val$state <- "MT"
val$trial_id <- sprintf("V%03d", seq_len(nrow(val)))
val$inc_vag <- NA_real_

canon <- c("trial_id", "responsible_company", "year", "cultivar", "city", "state",
           "sowing_date", "evaluation_date", "inc_vag", "ga", "latitude", "longitude", "source_row")
original_canon <- orig_keep
names(original_canon)[names(original_canon) == "evaluation(mdy)"] <- "evaluation_mdy_source"
names(original_canon)[names(original_canon) == "sowing(mdy)"] <- "sowing_mdy_source"
original_canon$cohort_source <- "original_csv"
val$cohort_source <- "later_validation_xlsx"

trials <- rbind(
  original_canon[c(canon, "cohort_source")],
  val[c(canon, "cohort_source")]
)
trials$city <- trimws(tools::toTitleCase(tolower(trials$city)))
trials$season <- trials$year
trials$recorded_evaluation_date <- trials$evaluation_date
trials$grain_evaluation_date <- trials$recorded_evaluation_date + 20L
trials$evaluation_date_offset_days <- 20L
# Keep evaluation_date as the operational model anchor for compatibility with
# downstream code, while preserving the source date explicitly.
trials$evaluation_date <- trials$grain_evaluation_date
trials$recorded_cycle_days <- as.integer(trials$recorded_evaluation_date - trials$sowing_date)
trials$cycle_days <- as.integer(trials$grain_evaluation_date - trials$sowing_date)
trials$sowing_doy <- as.integer(format(trials$sowing_date, "%j"))
trials$ga <- as.numeric(trials$ga)

if (nrow(trials) != 72L) stop("Clean-room reconstruction did not recover 72 trials; found ", nrow(trials), ".")
if ("T036" %in% trials$trial_id) stop("T036 was not excluded.")
if (anyDuplicated(trials$trial_id)) stop("Duplicate trial identifiers.")
if (any(trials$evaluation_date <= trials$sowing_date)) stop("Evaluation must follow sowing for every retained trial.")
if (any(!is.finite(trials$ga)) || any(trials$ga < 0 | trials$ga > 100)) stop("Invalid retained GA values.")
if (any(trials$latitude > 6 | trials$latitude < -35 | trials$longitude > -30 | trials$longitude < -75)) stop("Coordinates outside broad Brazil bounds.")

exclusions <- rbind(
  data.frame(source = "original_csv", source_row = excl_elig$source_row, trial_id = NA_character_, ga = excl_elig$ga, reason = excl_elig$reason),
  data.frame(source = "original_csv", source_row = excl_err$source_row, trial_id = excl_err$trial_id, ga = excl_err$ga, reason = excl_err$reason)
)
v4_write_csv(exclusions, file.path(p$tables, "cohort_exclusions.csv"))

# Search project documentation without opening outputs from previous analyses.
doc_candidates <- c("README.md", list.files("docs", pattern = "\\.(md|qmd|txt)$", recursive = TRUE, full.names = TRUE),
                    list.files("data", pattern = "\\.(md|txt)$", recursive = TRUE, full.names = TRUE))
doc_candidates <- unique(doc_candidates[file.exists(doc_candidates)])
hits <- list()
for (f in doc_candidates) {
  txt <- tryCatch(readLines(f, warn = FALSE, encoding = "UTF-8"), error = function(e) character())
  which_hit <- grep("\\bR7\\b|evaluation date|data de avalia|avalia.{0,30}R7", txt, ignore.case = TRUE, perl = TRUE)
  if (length(which_hit)) hits[[length(hits) + 1L]] <- data.frame(file = f, line = which_hit, text = txt[which_hit])
}
anchor_audit <- if (length(hits)) do.call(rbind, hits) else data.frame(file = character(), line = integer(), text = character())
anchor_verified <- any(grepl("R7", anchor_audit$text, ignore.case = TRUE) & grepl("protocol|performed|avaliad|evaluation", anchor_audit$text, ignore.case = TRUE))
v4_write_csv(anchor_audit, file.path(p$tables, "temporal_anchor_document_audit.csv"))
v4_write_csv(data.frame(
  operational_anchor = "grain evaluation date (source evaluation_date + 20 days)",
  source_date_preserved_as = "recorded_evaluation_date",
  correction_days = 20L,
  R7_verified = anchor_verified,
  conclusion = "Data providers clarified that grain evaluation occurred 20 days after the source evaluation_date; the corrected date is used as the weather anchor"
), file.path(p$tables, "temporal_anchor_conclusion.csv"))

# Retrieve one trial at a time so interrupted runs resume from validated fragments.
# r4pde is required only when a validated pinned cache fragment is unavailable.
weather_parts <- vector("list", nrow(trials))
retrieval_log <- vector("list", nrow(trials))
for (i in seq_len(nrow(trials))) {
  id <- trials$trial_id[i]
  cache_file <- file.path(p$cache, paste0(id, "_era5_100back.rds"))
  use_cache <- FALSE
  if (file.exists(cache_file)) {
    z <- tryCatch(readRDS(cache_file), error = function(e) NULL)
    use_cache <- !is.null(z) && identical(attr(z, "requested_model"), "era5") &&
      length(unique(as.Date(z$date))) == retrieval_days_back + 1L &&
      isTRUE(all.equal(sort(as.Date(z$date)),
                       seq(trials$evaluation_date[i] - retrieval_days_back, trials$evaluation_date[i], by = "day"))) &&
      all(c("T2M", "T2M_MAX", "T2M_MIN", "RH2M", "PRECTOTCORR") %in% names(z))
  }
  if (!use_cache) {
    if (!requireNamespace("r4pde", quietly = TRUE))
      stop("Package 'r4pde' and its imports are required to refresh missing weather cache files.")
    request <- data.frame(study = id, latitude = trials$latitude[i], longitude = trials$longitude[i],
                          evaluation_date = trials$evaluation_date[i])
    z <- r4pde::get_era5(
      data = request, days_around = retrieval_days_back, date_col = "evaluation_date", study_col = "study",
      pars = c("temperature_2m", "relative_humidity_2m", "precipitation", "dewpoint_2m"),
      models = "era5", direction = "back"
    )
    z <- as.data.frame(z)
    if (nrow(z) != retrieval_days_back + 1L) stop("ERA5 retrieval incomplete for ", id, ": ", nrow(z), " rows.")
    attr(z, "requested_model") <- "era5"
    attr(z, "retrieved_utc") <- format(Sys.time(), tz = "UTC", usetz = TRUE)
    attr(z, "r4pde_version") <- as.character(utils::packageVersion("r4pde"))
    saveRDS(z, cache_file)
  }
  z <- as.data.frame(z)
  z$trial_id <- id
  z$requested_latitude <- trials$latitude[i]
  z$requested_longitude <- trials$longitude[i]
  z$evaluation_date <- trials$evaluation_date[i]
  weather_parts[[i]] <- z
  retrieval_log[[i]] <- data.frame(
    trial_id = id, requested_model = "era5",
    api_endpoint = "https://archive-api.open-meteo.com/v1/archive",
    pars = "temperature_2m|relative_humidity_2m|precipitation|dewpoint_2m",
    start_date = min(z$date), end_date = max(z$date), rows = nrow(z),
    returned_grid_latitude = unique(z$latitude)[1], returned_grid_longitude = unique(z$longitude)[1],
    cache_file = normalizePath(cache_file, winslash = "/", mustWork = TRUE),
    cache_sha256 = v4_sha256(cache_file), reused_valid_cache = use_cache
  )
}
weather_all <- do.call(rbind, weather_parts)
retrieval_log <- do.call(rbind, retrieval_log)
weather_all$date <- as.Date(weather_all$date)
weather_all$lag_day <- as.integer(weather_all$date - weather_all$evaluation_date)
weather <- weather_all[weather_all$lag_day %in% primary_days, , drop = FALSE]
weather$Tmax <- weather$T2M_MAX
weather$RH <- weather$RH2M
weather$Rain <- weather$PRECTOTCORR
weather$WetDay <- as.numeric(weather$Rain >= 1)
weather$LogRain <- log1p(weather$Rain)

qa_by_trial <- do.call(rbind, lapply(split(weather, weather$trial_id), function(z) data.frame(
  trial_id = z$trial_id[1], rows = nrow(z), unique_dates = length(unique(z$date)),
  min_lag = min(z$lag_day), max_lag = max(z$lag_day),
  missing_Tmax = sum(!is.finite(z$Tmax)), missing_RH = sum(!is.finite(z$RH)), missing_Rain = sum(!is.finite(z$Rain)),
  invalid_RH = sum(z$RH < 0 | z$RH > 100, na.rm = TRUE),
  negative_rain = sum(z$Rain < 0, na.rm = TRUE),
  temperature_order_failures = sum(z$T2M_MIN > z$T2M | z$T2M > z$T2M_MAX, na.rm = TRUE),
  calendar_complete = identical(sort(z$lag_day), primary_days)
)))
if (any(qa_by_trial$rows != length(primary_days) | !qa_by_trial$calendar_complete |
        qa_by_trial$missing_Tmax > 0 | qa_by_trial$missing_RH > 0 | qa_by_trial$missing_Rain > 0 |
        qa_by_trial$invalid_RH > 0 | qa_by_trial$negative_rain > 0 | qa_by_trial$temperature_order_failures > 0)) {
  v4_write_csv(qa_by_trial, file.path(p$tables, "weather_quality_audit.csv"))
  stop("Pinned ERA5 quality audit failed; inspect weather_quality_audit.csv.")
}
v4_write_csv(qa_by_trial, file.path(p$tables, "weather_quality_audit.csv"))
v4_write_csv(retrieval_log, file.path(p$provenance, "era5_retrieval_manifest.csv"))

# Define the inferential clustering unit from the actual ERA5 grid coordinates
# returned for each trial. Cell-by-season clusters allow outcomes sharing the
# same gridded weather source within a growing season to have correlated errors.
grid_by_trial <- unique(weather[c("trial_id", "longitude", "latitude")])
if (anyDuplicated(grid_by_trial$trial_id) || nrow(grid_by_trial) != nrow(trials))
  stop("Each trial must map to exactly one ERA5 grid cell.")
grid_by_trial$cell_key <- sprintf("%.6f|%.6f", grid_by_trial$longitude,
                                  grid_by_trial$latitude)
cell_keys <- sort(unique(grid_by_trial$cell_key))
cell_ids <- setNames(sprintf("ERA5C%02d", seq_along(cell_keys)), cell_keys)
grid_by_trial$era5_cell_id <- unname(cell_ids[grid_by_trial$cell_key])
trials$era5_cell_id <- grid_by_trial$era5_cell_id[
  match(trials$trial_id, grid_by_trial$trial_id)
]
trials$weather_cluster_id <- paste(trials$era5_cell_id, trials$season, sep = "__")
weather$era5_cell_id <- trials$era5_cell_id[match(weather$trial_id, trials$trial_id)]
weather$weather_cluster_id <- trials$weather_cluster_id[
  match(weather$trial_id, trials$trial_id)
]
weather_cluster_map <- trials[c("trial_id", "season", "era5_cell_id",
                                "weather_cluster_id")]
weather_cluster_map$era5_longitude <- grid_by_trial$longitude[
  match(weather_cluster_map$trial_id, grid_by_trial$trial_id)
]
weather_cluster_map$era5_latitude <- grid_by_trial$latitude[
  match(weather_cluster_map$trial_id, grid_by_trial$trial_id)
]
cluster_sizes <- table(weather_cluster_map$weather_cluster_id)
weather_cluster_map$cluster_n_trials <- as.integer(
  cluster_sizes[weather_cluster_map$weather_cluster_id]
)
v4_write_csv(weather_cluster_map,
             file.path(p$tables, "weather_cell_season_cluster_map.csv"))
saveRDS(weather, file.path(p$processed, "weather_era5_absolute_days.rds"))
v4_write_csv(weather, file.path(p$processed, "weather_era5_absolute_days.csv"))

# Pairwise audit uses identical absolute dates and both levels and shapes across
# the complete 80-day history; duplicated histories are therefore re-evaluated.
w80 <- weather[weather$lag_day %in% primary_days, , drop = FALSE]
ids <- trials$trial_id
pairs <- vector("list", choose(length(ids), 2))
k <- 0L
for (i in seq_len(length(ids) - 1L)) for (j in (i + 1L):length(ids)) {
  k <- k + 1L
  a <- w80[w80$trial_id == ids[i], ]; b <- w80[w80$trial_id == ids[j], ]
  z <- merge(a[c("date", "Tmax", "RH", "WetDay")], b[c("date", "Tmax", "RH", "WetDay")], by = "date", suffixes = c("_1", "_2"))
  overlap <- nrow(z) / length(primary_days)
  same_grid <- abs(a$latitude[1] - b$latitude[1]) < 1e-8 && abs(a$longitude[1] - b$longitude[1]) < 1e-8
  tc <- v4_safe_cor(z$Tmax_1, z$Tmax_2); rc <- v4_safe_cor(z$RH_1, z$RH_2)
  tr <- v4_srmse(z$Tmax_1, z$Tmax_2); rr <- v4_srmse(z$RH_1, z$RH_2)
  wa <- if (nrow(z)) mean(z$WetDay_1 == z$WetDay_2) else NA_real_
  exact <- same_grid && overlap == 1 && tr < 1e-10 && rr < 1e-10 && wa == 1
  near <- same_grid && overlap >= 0.98 && tc >= 0.999 && rc >= 0.999 && tr <= 0.05 && rr <= 0.05 && wa >= 0.98
  correlated <- !near && overlap >= 0.8 && tc >= 0.95 && rc >= 0.95
  composite <- mean(c((tc + 1) / 2, (rc + 1) / 2, exp(-tr), exp(-rr), wa, overlap), na.rm = TRUE)
  pairs[[k]] <- data.frame(id1 = ids[i], id2 = ids[j], same_grid = same_grid,
                           date_overlap = overlap, tmax_correlation = tc, rh_correlation = rc,
                           tmax_srmse = tr, rh_srmse = rr, wetday_agreement = wa,
                           exact_duplicate = exact, near_duplicate = near,
                           merely_correlated = correlated, composite_similarity = composite)
}
pair_audit <- do.call(rbind, pairs)
near_edges <- pair_audit[pair_audit$near_duplicate, c("id1", "id2"), drop = FALSE]
units <- v4_union_components(ids, near_edges)
trials$met_unit <- unname(units[trials$trial_id])

v4_write_csv(pair_audit, file.path(p$tables, "meteorological_similarity_pairs.csv"))
unit_map <- trials[c("trial_id", "met_unit", "season", "evaluation_date", "latitude", "longitude")]
unit_sizes <- table(trials$met_unit)
unit_map$unit_size <- as.integer(unit_sizes[unit_map$met_unit])
v4_write_csv(unit_map, file.path(p$tables, "meteorological_unit_map.csv"))

# The −80-day domain can precede sowing in short-cycle trials. This audit
# documents that coverage rather than treating these lags as crop-stage data.
imap <- v4_interval_map(primary_days)
pre_sowing_audit <- do.call(rbind, lapply(levels(imap), function(int) {
  lags <- primary_days[imap == int]
  n_pre <- vapply(trials$cycle_days, function(cycle) sum(cycle + lags < 0L), integer(1))
  data.frame(interval = int, n_trials_with_any_pre_sowing_day = sum(n_pre > 0L),
             total_pre_sowing_trial_days = sum(n_pre),
             min_days_after_sowing = min(outer(trials$cycle_days, lags, `+`)),
             max_days_after_sowing = max(outer(trials$cycle_days, lags, `+`)))
}))
v4_write_csv(pre_sowing_audit, file.path(p$tables, "pre_sowing_exposure_audit.csv"))

saveRDS(trials, file.path(p$processed, "trial_cohort_v4.rds"))
v4_write_csv(trials, file.path(p$processed, "trial_cohort_v4.csv"))

# Frozen prespecified rules. This file is written before outcome-weather models are fitted.
spec <- data.frame(
  item = c("analysis_name", "primary_outcome", "sensitivity_outcome", "anchor_label",
           "source_anchor_field", "anchor_correction_days", "day_zero_excluded",
           "primary_days", "weather_product", "weather_function", "weather_model_argument",
           "primary_processes", "wetday_threshold_mm", "broad_intervals", "functional_basis", "functional_df",
           "bootstrap_reps_primary", "bootstrap_reps_sensitivity", "bootstrap_unit", "bootstrap_stratification",
           "primary_inference", "n_era5_cells", "n_weather_clusters",
           "simultaneous_band", "primary_adjustment", "excluded_primary_covariates", "influence_rule",
           "duplicate_rule", "seed_primary", "seed_sensitivity", "n_trials", "n_duplicate_history_groups",
           "postfreeze_gate"),
  value = c("Version 4 clean absolute-time analysis", "GA percentage points", "log1p(GA)",
            "grain evaluation date (source evaluation_date + 20 days)",
            "recorded_evaluation_date", "20", "TRUE", "-80:-1", "ERA5",
            "r4pde::get_era5", "models='era5'", "Tmin|LogRain|Tmax|RH", "1",
            "eight fixed 10-day intervals", "cubic B-spline", "4", "999", "499",
            "ERA5 cell-by-season cluster", "season",
            "CR2 covariance; Satterthwaite coefficient tests; HTZ process tests",
            as.character(length(unique(trials$era5_cell_id))),
            as.character(length(unique(trials$weather_cluster_id))),
            "bootstrap maximum standardized deviation",
            "season", "state", "GA outside Tukey 1.5-IQR fences computed before weather modeling",
            "same ERA5 grid; date overlap>=0.98; Tmax/RH correlations>=0.999; standardized RMSE<=0.05; WetDay agreement>=0.98",
            as.character(seed_primary), "24041988", as.character(nrow(trials)), as.character(length(unique(trials$met_unit))),
            "No prior-analysis result may be read until primary and sensitivity freeze manifests exist")
)
v4_write_csv(spec, file.path(p$config, "frozen_primary_specification.csv"))
v4_write_csv(data.frame(
  threshold = c("date_overlap", "Tmax_correlation", "RH_correlation", "Tmax_sRMSE", "RH_sRMSE", "WetDay_agreement"),
  rule = c(">=", ">=", ">=", "<=", "<=", ">="), value = c(0.98, 0.999, 0.999, 0.05, 0.05, 0.98)
), file.path(p$config, "frozen_duplicate_rule.csv"))

cohort_hash <- v4_sha256(file.path(p$processed, "trial_cohort_v4.csv"))
weather_hash <- v4_sha256(file.path(p$processed, "weather_era5_absolute_days.csv"))
freeze_paths <- c(file.path(p$processed, "trial_cohort_v4.csv"),
                  file.path(p$processed, "weather_era5_absolute_days.csv"),
                  file.path(p$config, "frozen_primary_specification.csv"))
v4_write_csv(data.frame(object = c("cohort", "weather", "primary_specification"),
                        path = normalizePath(freeze_paths, winslash = "/", mustWork = TRUE),
                        relative_path = sub(paste0("^", normalizePath(p$root, winslash = "/"), "/"), "",
                                            normalizePath(freeze_paths, winslash = "/", mustWork = TRUE)),
                        sha256 = c(cohort_hash, weather_hash, v4_sha256(file.path(p$config, "frozen_primary_specification.csv")))),
             file.path(p$freeze, "preanalysis_freeze.csv"))

capture.output(sessionInfo(), file = file.path(p$provenance, "sessionInfo_prepare.txt"))
cat("Prepared and frozen Version 4 cohort/weather inputs.\n")
cat("Trials:", nrow(trials),
    " ERA5 cells:", length(unique(trials$era5_cell_id)),
    " Cell-season clusters:", length(unique(trials$weather_cluster_id)),
    " Near-duplicate history groups:", length(unique(trials$met_unit)), "\n")
