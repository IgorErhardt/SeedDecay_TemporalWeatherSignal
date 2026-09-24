#!/usr/bin/env Rscript

# Publication-ready map of the Version 4 trial cohort.
#
# The map displays the ERA5 cell centroid assigned to each trial rather than the
# recorded field coordinates. Trials sharing a weather cell are aggregated, and
# every displayed centroid is annotated with its assigned number of trials.

.libPaths(c(file.path(getwd(), "R-library"), .libPaths()))
source("analysis/R/panel_labels.R")

required_packages <- c("ggplot2", "sf", "ggrepel", "patchwork", "rnaturalearth")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0L) {
  stop("Missing required package(s): ", paste(missing_packages, collapse = ", "))
}

trial_file <- file.path(
  "analysis", "data", "processed", "trial_cohort_v4_with_outcome_flags.csv"
)
weather_manifest_file <- file.path(
  "analysis", "provenance", "era5_retrieval_manifest.csv"
)
state_cache_file <- file.path(
  "analysis", "data", "spatial", "brazil_states_all_2020.rds"
)
table_file <- file.path(
  "analysis", "outputs", "tables", "trial_counts_by_field_coordinate.csv"
)
era5_count_file <- file.path(
  "analysis", "outputs", "tables", "trial_counts_by_era5_location.csv"
)
provenance_file <- file.path(
  "analysis", "provenance", "trial_location_map_provenance.csv"
)
session_file <- file.path(
  "analysis", "provenance", "sessionInfo_trial_location_map.txt"
)
figure_dir <- Sys.getenv("FINAL_FIGURE_DIR", unset = "figures")
figure_file <- file.path(figure_dir, "figure1_trial_locations_map.png")
gpkg_file <- file.path(
  "analysis", "outputs", "spatial", "trial_field_locations.gpkg"
)

for (path in unique(dirname(c(
  state_cache_file, table_file, era5_count_file, provenance_file, session_file,
  figure_file, gpkg_file
)))) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
}

if (!file.exists(trial_file)) stop("Trial cohort not found: ", trial_file)
if (!file.exists(weather_manifest_file)) {
  stop("ERA5 retrieval manifest not found: ", weather_manifest_file)
}
if (!file.exists(state_cache_file)) {
  stop(
    "Cached state boundaries not found. Retrieve all Brazilian states with ",
    "geobr::read_state(year = 2020, code_state = 'all') first."
  )
}

trials <- utils::read.csv(trial_file, stringsAsFactors = FALSE)
weather_manifest <- utils::read.csv(weather_manifest_file, stringsAsFactors = FALSE)
states <- readRDS(state_cache_file)

# Fail early if the map is no longer based on a valid frozen trial cohort.
if (anyDuplicated(trials$trial_id)) stop("Duplicated trial_id in trial cohort.")
if (anyDuplicated(weather_manifest$trial_id)) {
  stop("Duplicated trial_id in ERA5 retrieval manifest.")
}
if (!setequal(trials$trial_id, weather_manifest$trial_id)) {
  stop("Trial IDs differ between the cohort and ERA5 retrieval manifest.")
}
if (any(!is.finite(trials$longitude)) || any(!is.finite(trials$latitude))) {
  stop("Missing or invalid recorded trial coordinates.")
}

# Counts are calculated only for exactly matching recorded field coordinates.
# No rounding, spatial tolerance, municipality correction, or outcome-dependent
# grouping is used.
field_counts <- stats::aggregate(
  trial_id ~ longitude + latitude,
  data = trials,
  FUN = length
)
names(field_counts)[names(field_counts) == "trial_id"] <- "n_trials"
field_counts <- field_counts[order(-field_counts$n_trials), ]
field_counts$location_id <- sprintf("FIELD_%02d", seq_len(nrow(field_counts)))
field_counts$n_trials_factor <- factor(
  field_counts$n_trials,
  levels = sort(unique(field_counts$n_trials))
)

# The displayed map locations are the exact ERA5 cell centers returned for each
# trial. Counts are aggregated at those coordinates without spatial tolerance.
era5_counts <- stats::aggregate(
  trial_id ~ returned_grid_longitude + returned_grid_latitude,
  data = weather_manifest,
  FUN = length
)
names(era5_counts)[names(era5_counts) == "trial_id"] <- "n_trials"
era5_counts <- era5_counts[order(-era5_counts$n_trials), ]
era5_counts$location_id <- sprintf("ERA5_%02d", seq_len(nrow(era5_counts)))
era5_counts$label <- paste0("n = ", era5_counts$n_trials)
era5_counts$n_trials_factor <- factor(
  era5_counts$n_trials,
  levels = sort(unique(era5_counts$n_trials))
)
if (sum(era5_counts$n_trials) != nrow(trials)) {
  stop("ERA5-location counts do not sum to the trial cohort size.")
}

# Zoom to the observed trial longitude extent with a small fixed margin. Expand
# latitude symmetrically so the geographic view is approximately square after
# accounting for longitude convergence at the mean study latitude.
longitude_offset <- 0.75
map_xlim <- range(field_counts$longitude) + c(-longitude_offset, longitude_offset)
raw_latitude_range <- range(field_counts$latitude)
mean_latitude_radians <- mean(raw_latitude_range) * pi / 180
target_latitude_span <- diff(map_xlim) * cos(mean_latitude_radians)
latitude_offset <- max(
  0.60,
  (target_latitude_span - diff(raw_latitude_range)) / 2
)
map_ylim <- range(field_counts$latitude) + c(-latitude_offset, latitude_offset)

if (sum(field_counts$n_trials) != nrow(trials)) {
  stop("Field-coordinate counts do not sum to the trial cohort size.")
}

utils::write.csv(field_counts, table_file, row.names = FALSE, na = "")
utils::write.csv(era5_counts, era5_count_file, row.names = FALSE, na = "")

# Export the plotted unique locations and the underlying trial-level records to
# one GeoPackage. Both layers retain the source longitude/latitude in WGS84.
plotted_field_locations_sf <- sf::st_as_sf(
  field_counts[c("location_id", "n_trials", "longitude", "latitude")],
  coords = c("longitude", "latitude"),
  crs = 4326,
  remove = FALSE
)
trial_records_sf <- sf::st_as_sf(
  trials,
  coords = c("longitude", "latitude"),
  crs = 4326,
  remove = FALSE
)
if (!file.exists(gpkg_file)) {
  sf::st_write(
    plotted_field_locations_sf,
    dsn = gpkg_file,
    layer = "plotted_field_locations",
    quiet = TRUE
  )
  sf::st_write(
    trial_records_sf,
    dsn = gpkg_file,
    layer = "trial_records",
    append = TRUE,
    quiet = TRUE
  )
}

# Keep neighboring states visible while highlighting the two states containing
# the study trials. Only the focal states are labeled because labels calculated
# from partly cropped neighboring polygons can be clipped at the map margins.
focus_states <- states[states$abbrev_state %in% c("RO", "MT"), ]
visible_bbox <- sf::st_bbox(c(
  xmin = map_xlim[1], ymin = map_ylim[1],
  xmax = map_xlim[2], ymax = map_ylim[2]
), crs = sf::st_crs(states))
visible_focus_states <- suppressWarnings(sf::st_crop(focus_states, visible_bbox))
state_label_points <- suppressWarnings(sf::st_point_on_surface(visible_focus_states))
state_label_xy <- cbind(
  sf::st_drop_geometry(state_label_points),
  sf::st_coordinates(state_label_points)
)

map_plot <- ggplot2::ggplot() +
  ggplot2::geom_sf(
    data = states,
    fill = "#FAFAFA",
    color = "#66717E",
    linewidth = 0.45
  ) +
  ggplot2::geom_sf(
    data = focus_states,
    fill = "#EDF1F3",
    color = "#53606C",
    linewidth = 0.65
  ) +
  ggplot2::geom_point(
    data = era5_counts,
    ggplot2::aes(
      x = returned_grid_longitude,
      y = returned_grid_latitude
    ),
    shape = 21,
    size = 2.7,
    fill = "steelblue",
    color = "white",
    stroke = 0.65,
    alpha = 0.92
  ) +
  ggrepel::geom_label_repel(
    data = era5_counts,
    ggplot2::aes(
      x = returned_grid_longitude,
      y = returned_grid_latitude,
      label = label
    ),
    seed = 20260914,
    min.segment.length = 0,
    box.padding = 0.22,
    point.padding = 0.28,
    segment.color = "#78828C",
    segment.size = 0.32,
    label.size = 0.18,
    label.padding = grid::unit(0.10, "lines"),
    fill = ggplot2::alpha("white", 0.92),
    color = "#263238",
    size = 2.8,
    max.overlaps = Inf
  ) +
  ggplot2::geom_text(
    data = state_label_xy,
    ggplot2::aes(x = X, y = Y, label = abbrev_state),
    color = "#7A848D",
    fontface = "bold",
    size = 5.2
  ) +
  single_panel_letter("A") +
  ggplot2::coord_sf(
    xlim = map_xlim,
    ylim = map_ylim,
    expand = FALSE
  ) +
  ggplot2::labs(x = "Longitude", y = "Latitude") +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(
    panel.grid.major = ggplot2::element_line(color = "#D9DEE2", linewidth = 0.35),
    panel.grid.minor = ggplot2::element_blank(),
    axis.title = ggplot2::element_text(face = "bold", color = "#263238"),
    axis.text = ggplot2::element_text(color = "#455A64"),
    legend.position = "none",
    plot.margin = ggplot2::margin(8, 10, 8, 8)
  )

# Regional locator inset. Latin America is defined here as South America,
# Central America, the Caribbean, and Mexico. The red rectangle is the exact
# longitude-latitude extent displayed in the main map.
countries <- rnaturalearth::ne_countries(scale = 110, returnclass = "sf")
latin_america <- countries[
  countries$continent == "South America" |
    countries$subregion %in% c("Central America", "Caribbean") |
    countries$admin == "Mexico",
]
main_view <- sf::st_as_sfc(sf::st_bbox(c(
  xmin = map_xlim[1], ymin = map_ylim[1],
  xmax = map_xlim[2], ymax = map_ylim[2]
), crs = sf::st_crs(4326)))
main_view <- sf::st_sf(view = "main map extent", geometry = main_view)

inset_plot <- ggplot2::ggplot() +
  ggplot2::geom_sf(
    data = latin_america,
    fill = "#F7F7F7",
    color = "#727C84",
    linewidth = 0.20
  ) +
  ggplot2::geom_sf(
    data = main_view,
    fill = ggplot2::alpha("red", 0.5),
    color = "#B2182B",
    linewidth = 0.55
  ) +
  single_panel_letter("B") +
  ggplot2::coord_sf(
    xlim = c(-120, -30),
    ylim = c(-56, 33),
    expand = FALSE
  ) +
  ggplot2::theme_void() +
  ggplot2::theme(
    panel.background = ggplot2::element_rect(fill = "white", color = NA),
    panel.border = ggplot2::element_rect(
      fill = NA,
      color = "#4F5962",
      linewidth = 0.45
    ),
    plot.margin = ggplot2::margin(2, 2, 2, 2)
  )

map_with_inset <- map_plot + patchwork::inset_element(
  inset_plot,
  left = 0.015,
  bottom = 0.02,
  right = 0.34,
  top = 0.38,
  align_to = "panel",
  clip = TRUE
)

ggplot2::ggsave(
  filename = figure_file,
  plot = map_with_inset,
  width = 6.8,
  height = 6.8,
  units = "in",
  dpi = 450,
  bg = "white"
)

provenance <- data.frame(
  item = c(
    "trial_source", "weather_grid_source", "boundary_source",
    "boundary_year", "highlighted_states", "trial_count",
    "unique_field_coordinate_count", "era5_location_count", "figure_seed", "figure_file",
    "spatial_file", "spatial_crs", "spatial_layers"
  ),
  value = c(
    normalizePath(trial_file, winslash = "/"),
    normalizePath(weather_manifest_file, winslash = "/"),
    "IBGE state boundaries distributed by geobr",
    "2020",
    "Rondonia (RO); Mato Grosso (MT); surrounding states retained",
    nrow(trials),
    nrow(field_counts),
    nrow(era5_counts),
    "20260914",
    normalizePath(figure_file, winslash = "/", mustWork = FALSE),
    normalizePath(gpkg_file, winslash = "/", mustWork = FALSE),
    "EPSG:4326 (WGS 84)",
    "plotted_field_locations; trial_records"
  ),
  stringsAsFactors = FALSE
)
utils::write.csv(provenance, provenance_file, row.names = FALSE, na = "")
writeLines(capture.output(sessionInfo()), session_file)

message("Created: ", figure_file)
message("Created: ", gpkg_file)
message(
  "Mapped ", nrow(trials), " trials across ", nrow(era5_counts),
  " ERA5 cell centroids."
)
