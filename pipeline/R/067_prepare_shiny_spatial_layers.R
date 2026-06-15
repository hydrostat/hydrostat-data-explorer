# ============================================================
# 067_prepare_shiny_spatial_layers.R
#
# Purpose:
# Prepare optional spatial layers for the Shiny app and create
# a combined preview map with station points from the local
# Shiny export.
#
# This script uses only local files.
# It does not call the ANA API.
# It does not use credentials, tokens, or passwords.
# Run from the project root.
# ============================================================

library(DBI)
library(duckdb)
library(dplyr)
library(sf)
library(leaflet)
library(htmlwidgets)
library(rmapshaper)

# ------------------------------------------------------------
# Paths
# ------------------------------------------------------------

shiny_db <- file.path("exports", "shiny_minimal.duckdb")

spatial_input_dir <- file.path("data", "raw", "spatial")
spatial_output_dir <- file.path("exports", "spatial_layers")
preview_output_dir <- file.path("outputs", "map_previews")

dir.create(spatial_output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(preview_output_dir, recursive = TRUE, showWarnings = FALSE)

output_rds <- file.path(spatial_output_dir, "shiny_spatial_layers.rds")
output_gpkg <- file.path(spatial_output_dir, "shiny_spatial_layers.gpkg")
output_manifest <- file.path(spatial_output_dir, "spatial_layers_manifest.csv")
output_missing <- file.path(spatial_output_dir, "spatial_layers_missing_inputs.csv")
output_errors <- file.path(spatial_output_dir, "spatial_layers_read_errors.csv")
output_preview_html <- file.path(preview_output_dir, "067_spatial_layers_preview.html")
output_preview_csv <- file.path(preview_output_dir, "067_spatial_layers_preview_points.csv")

# ------------------------------------------------------------
# Station-group priority
# ------------------------------------------------------------

station_status_priority <- function(map_status) {
  dplyr::case_when(
    map_status == "measurements_and_rating_curves" ~ 1L,
    map_status == "measurements_only" ~ 2L,
    map_status == "registration_only" ~ 3L,
    TRUE ~ 9L
  )
}

# ------------------------------------------------------------
# Preview options
# ------------------------------------------------------------
# South America is intentionally not included in this preview.
# Keep the preview focused on Brazil, ANA stations, and river layers.

sample_registration_only <- TRUE
max_registration_only_preview <- 8000
hide_registration_only_by_default <- TRUE

# Spatial layers are simplified for Shiny/Leaflet performance.
# Values were selected from GeoJSON size diagnostics.
simplify_spatial_layers <- TRUE

spatial_simplification_keep <- c(
  brazil_boundary = 0.20,
  states = 0.02,
  basins = 0.02,
  rivers_large = 0.50,
  rivers_medium = 0.30,
  rivers_small = 0.10
)

# Allow GDAL to reconstruct missing .shx files when possible.
Sys.setenv(SHAPE_RESTORE_SHX = "YES")

# ------------------------------------------------------------
# Expected local spatial inputs
# ------------------------------------------------------------

expected_layers <- data.frame(
  layer_key = c(
    "brazil_boundary",
    "states",
    "basins",
    "rivers_large",
    "rivers_medium",
    "rivers_small"
  ),
  file_name = c(
    "brasil_contorno.shp",
    "GEOFT_UNIDADE_FEDERACAO_2022.shp",
    "Regiões_Hidrográficas_(CNRH).shp",
    "hidro_lvl2_EPSG5880.shp",
    "hidro_lvl3_EPSG5880.shp",
    "hidro_lvl4_EPSG5880.shp"
  ),
  layer_label = c(
    "Brazil boundary",
    "State boundaries",
    "Macro basins",
    "Large rivers",
    "Medium rivers",
    "Small rivers"
  ),
  geometry_type_expected = c(
    "polygon",
    "polygon",
    "polygon",
    "line",
    "line",
    "line"
  ),
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------
# Helper functions
# ------------------------------------------------------------

locate_input_file <- function(file_name, input_dir) {
  if (!dir.exists(input_dir)) {
    return(NA_character_)
  }
  
  files <- list.files(
    input_dir,
    recursive = TRUE,
    full.names = TRUE,
    all.files = FALSE,
    no.. = TRUE
  )
  
  if (length(files) == 0) {
    return(NA_character_)
  }
  
  hit <- files[tolower(basename(files)) == tolower(file_name)]
  
  if (length(hit) == 0) {
    return(NA_character_)
  }
  
  hit[1]
}

list_shapefile_sidecars <- function(shp_path) {
  if (is.na(shp_path) || !file.exists(shp_path)) {
    return(character(0))
  }
  
  layer_dir <- dirname(shp_path)
  base_no_ext <- tools::file_path_sans_ext(basename(shp_path))
  
  all_files <- list.files(layer_dir, full.names = TRUE)
  all_files[tools::file_path_sans_ext(basename(all_files)) == base_no_ext]
}

has_sidecar_ext <- function(sidecars, ext) {
  any(tolower(tools::file_ext(sidecars)) == tolower(ext))
}

bbox_looks_lonlat <- function(x) {
  bb <- sf::st_bbox(x)
  
  isTRUE(
    is.finite(bb[["xmin"]]) &&
      is.finite(bb[["xmax"]]) &&
      is.finite(bb[["ymin"]]) &&
      is.finite(bb[["ymax"]]) &&
      bb[["xmin"]] >= -180 && bb[["xmax"]] <= 180 &&
      bb[["ymin"]] >= -90 && bb[["ymax"]] <= 90
  )
}

infer_or_set_crs <- function(x, layer_key, file_path) {
  if (!is.na(sf::st_crs(x))) {
    return(x)
  }
  
  # River files are explicitly identified as EPSG5880 in their filenames.
  if (
    grepl("EPSG5880", basename(file_path), ignore.case = TRUE) ||
    grepl("^rivers_", layer_key)
  ) {
    sf::st_crs(x) <- 5880
    return(x)
  }
  
  # Some local boundary shapefiles may already be lon/lat but lack a .prj.
  # Only infer EPSG:4326 when the bounding box clearly looks like lon/lat.
  if (bbox_looks_lonlat(x)) {
    sf::st_crs(x) <- 4326
    return(x)
  }
  
  x
}

simplify_for_shiny_spatial_layer <- function(x, layer_key) {
  if (!isTRUE(simplify_spatial_layers)) {
    return(x)
  }
  
  keep_value <- spatial_simplification_keep[layer_key]
  
  if (length(keep_value) == 0 || is.na(keep_value)) {
    return(x)
  }
  
  suppressWarnings(
    rmapshaper::ms_simplify(
      x,
      keep = unname(keep_value),
      keep_shapes = TRUE
    )
  )
}

make_basin_fill_colors <- function(n) {
  grDevices::hcl.colors(n, palette = "Set 3")
}

compact_spatial_layer_for_shiny <- function(x, layer_key) {
  if (identical(layer_key, "basins")) {
    x <- x %>%
      dplyr::mutate(
        basin_fill_color = make_basin_fill_colors(dplyr::n())
      ) %>%
      dplyr::select(basin_fill_color, geometry)
    
    return(x)
  }
  
  x %>%
    dplyr::select(geometry)
}

standardize_sf <- function(x, layer_key, file_path, target_crs = 4326) {
  if (is.null(x) || nrow(x) == 0) {
    return(x)
  }
  
  x <- sf::st_zm(x, drop = TRUE, what = "ZM")
  x <- infer_or_set_crs(x, layer_key = layer_key, file_path = file_path)
  
  if (is.na(sf::st_crs(x))) {
    stop("Spatial layer has no CRS and CRS could not be inferred.")
  }
  
  x <- suppressWarnings(sf::st_make_valid(x))
  x <- sf::st_transform(x, target_crs)
  
  x <- simplify_for_shiny_spatial_layer(
    x = x,
    layer_key = layer_key
  )
  
  x <- suppressWarnings(sf::st_make_valid(x))
  
  x <- compact_spatial_layer_for_shiny(
    x = x,
    layer_key = layer_key
  )
  
  x
}

safe_write_geojson <- function(x, file_path) {
  if (!is.null(x) && nrow(x) > 0) {
    suppressWarnings(
      sf::write_sf(x, file_path, delete_dsn = TRUE, quiet = TRUE)
    )
  }
}

safe_write_gpkg_layer <- function(x, dsn, layer_name, append = TRUE) {
  if (!is.null(x) && nrow(x) > 0) {
    suppressWarnings(
      sf::write_sf(
        x,
        dsn = dsn,
        layer = layer_name,
        append = append,
        quiet = TRUE
      )
    )
  }
}

add_poly_layer_if_present <- function(map, layers, layer_key, group, color, weight,
                                      opacity = 0.8, dash_array = NULL,
                                      fill = FALSE, fill_color = color,
                                      fill_opacity = 0) {
  if (layer_key %in% names(layers) && nrow(layers[[layer_key]]) > 0) {
    map <- map %>%
      addPolygons(
        data = layers[[layer_key]],
        group = group,
        color = color,
        weight = weight,
        fill = fill,
        fillColor = fill_color,
        fillOpacity = fill_opacity,
        opacity = opacity,
        dashArray = dash_array
      )
  }
  
  map
}

add_line_layer_if_present <- function(map, layers, layer_key, group, color, weight,
                                      opacity = 0.8) {
  if (layer_key %in% names(layers) && nrow(layers[[layer_key]]) > 0) {
    map <- map %>%
      addPolylines(
        data = layers[[layer_key]],
        group = group,
        color = color,
        weight = weight,
        opacity = opacity
      )
  }
  
  map
}

# ------------------------------------------------------------
# Locate and read available spatial layers
# ------------------------------------------------------------

input_files <- expected_layers %>%
  rowwise() %>%
  mutate(
    file_path = locate_input_file(file_name, spatial_input_dir),
    exists = !is.na(file_path) && file.exists(file_path)
  ) %>%
  ungroup()

# Check shapefile sidecars. This is diagnostic only; the script will still try
# to read the layer, because GDAL may handle some missing auxiliary files.
sidecar_status <- lapply(seq_len(nrow(input_files)), function(i) {
  row <- input_files[i, ]
  
  if (!isTRUE(row$exists) || tools::file_ext(row$file_name) != "shp") {
    return(data.frame(
      layer_key = row$layer_key,
      has_shp = isTRUE(row$exists),
      has_shx = NA,
      has_dbf = NA,
      has_prj = NA,
      stringsAsFactors = FALSE
    ))
  }
  
  sidecars <- list_shapefile_sidecars(row$file_path)
  
  data.frame(
    layer_key = row$layer_key,
    has_shp = has_sidecar_ext(sidecars, "shp"),
    has_shx = has_sidecar_ext(sidecars, "shx"),
    has_dbf = has_sidecar_ext(sidecars, "dbf"),
    has_prj = has_sidecar_ext(sidecars, "prj"),
    stringsAsFactors = FALSE
  )
}) %>%
  bind_rows()

input_files <- input_files %>%
  left_join(sidecar_status, by = "layer_key")

write.csv(
  input_files,
  output_manifest,
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

write.csv(
  input_files %>% filter(!exists),
  output_missing,
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

available_layers <- list()
read_errors <- data.frame(
  layer_key = character(),
  file_name = character(),
  file_path = character(),
  error_message = character(),
  stringsAsFactors = FALSE
)

for (i in seq_len(nrow(input_files))) {
  row <- input_files[i, ]
  
  if (!isTRUE(row$exists)) {
    message("Missing spatial layer: ", row$layer_key, " | expected file: ", row$file_name)
    next
  }
  
  message("Reading spatial layer: ", row$layer_key, " | ", row$file_path)
  
  layer_result <- tryCatch({
    layer_sf <- sf::read_sf(row$file_path, quiet = TRUE)
    layer_sf <- standardize_sf(
      layer_sf,
      layer_key = row$layer_key,
      file_path = row$file_path,
      target_crs = 4326
    )
    layer_sf
  }, error = function(e) {
    read_errors <<- bind_rows(
      read_errors,
      data.frame(
        layer_key = row$layer_key,
        file_name = row$file_name,
        file_path = row$file_path,
        error_message = conditionMessage(e),
        stringsAsFactors = FALSE
      )
    )
    NULL
  })
  
  if (!is.null(layer_result) && nrow(layer_result) > 0) {
    available_layers[[row$layer_key]] <- layer_result
    message("  loaded features: ", nrow(layer_result))
  } else {
    message("  layer not loaded: ", row$layer_key)
  }
}

write.csv(
  read_errors,
  output_errors,
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# ------------------------------------------------------------
# Save prepared spatial layers
# ------------------------------------------------------------

saveRDS(available_layers, output_rds)

if (file.exists(output_gpkg)) {
  file.remove(output_gpkg)
}

for (nm in names(available_layers)) {
  safe_write_gpkg_layer(
    x = available_layers[[nm]],
    dsn = output_gpkg,
    layer_name = nm,
    append = TRUE
  )
}

for (nm in names(available_layers)) {
  safe_write_geojson(
    x = available_layers[[nm]],
    file_path = file.path(spatial_output_dir, paste0(nm, ".geojson"))
  )
}

# ------------------------------------------------------------
# Read station data from local Shiny export
# ------------------------------------------------------------

con <- dbConnect(duckdb::duckdb(), shiny_db, read_only = TRUE)
on.exit(dbDisconnect(con), add = TRUE)

tables_available <- dbListTables(con)

stations <- dbReadTable(con, "stations_minimal") %>%
  mutate(station_code = as.character(station_code))

map_status <- dbReadTable(con, "station_map_status") %>%
  mutate(station_code = as.character(station_code)) %>%
  select(
    station_code,
    any_of(c(
      "map_status",
      "map_status_label"
    ))
  )

if ("station_diagnostic_summary" %in% tables_available) {
  station_diagnostic_summary <- dbReadTable(con, "station_diagnostic_summary") %>%
    mutate(station_code = as.character(station_code)) %>%
    select(
      station_code,
      any_of(c(
        "diagnostic_attention_class",
        "diagnostic_attention_score"
      ))
    )
} else {
  station_diagnostic_summary <- data.frame(
    station_code = character(),
    diagnostic_attention_class = character(),
    diagnostic_attention_score = numeric(),
    stringsAsFactors = FALSE
  )
}

required_station_cols <- c(
  "station_code",
  "station_name",
  "uf",
  "municipality",
  "basin_name",
  "latitude",
  "longitude"
)

for (nm in required_station_cols) {
  if (!nm %in% names(stations)) {
    stations[[nm]] <- NA
  }
}

map_data <- stations %>%
  left_join(map_status, by = "station_code") %>%
  left_join(station_diagnostic_summary, by = "station_code") %>%
  mutate(
    latitude = as.numeric(latitude),
    longitude = as.numeric(longitude)
  ) %>%
  filter(!is.na(latitude), !is.na(longitude)) %>%
  mutate(
    map_status = ifelse(is.na(map_status), "registration_only", map_status),
    map_status_label = case_when(
      map_status == "measurements_and_rating_curves" ~ "Discharge measurements and rating curves",
      map_status == "measurements_only" ~ "Discharge measurements only",
      map_status == "registration_only" ~ "Station registration only",
      TRUE ~ ifelse(is.na(map_status_label), "Station registration only", map_status_label)
    ),
    diagnostic_attention_class = ifelse(
      is.na(diagnostic_attention_class),
      "not available",
      diagnostic_attention_class
    ),
    station_name = ifelse(is.na(station_name), "", station_name),
    uf = ifelse(is.na(uf), "", uf),
    municipality = ifelse(is.na(municipality), "", municipality),
    basin_name = ifelse(is.na(basin_name), "", basin_name),
    popup = paste0(
      "<b>", station_code, " - ", station_name, "</b><br>",
      "UF: ", uf, "<br>",
      "Municipality: ", municipality, "<br>",
      "Basin: ", basin_name, "<br>",
      "Status: ", map_status_label, "<br>",
      "Diagnostic attention: ", diagnostic_attention_class
    )
  ) %>%
  dplyr::arrange(station_status_priority(map_status), station_code) %>%
  dplyr::distinct(station_code, .keep_all = TRUE)

# ------------------------------------------------------------
# Optional sampling for static HTML preview
# ------------------------------------------------------------

data_stations <- map_data %>%
  filter(map_status != "registration_only")

registration_stations <- map_data %>%
  filter(map_status == "registration_only")

if (
  sample_registration_only &&
  nrow(registration_stations) > max_registration_only_preview
) {
  set.seed(67)
  
  registration_stations <- registration_stations %>%
    slice_sample(n = max_registration_only_preview)
}

map_preview_data <- bind_rows(
  registration_stations,
  data_stations
)

write.csv(map_preview_data, output_preview_csv, row.names = FALSE, fileEncoding = "UTF-8")

# ------------------------------------------------------------
# Station symbology
# ------------------------------------------------------------

status_palette <- c(
  "registration_only" = "#bdbdbd",
  "measurements_only" = "#f28e2b",
  "measurements_and_rating_curves" = "#1f78b4"
)

status_radius <- c(
  "registration_only" = 2,
  "measurements_only" = 4,
  "measurements_and_rating_curves" = 5
)

status_opacity <- c(
  "registration_only" = 0.25,
  "measurements_only" = 0.75,
  "measurements_and_rating_curves" = 0.90
)

status_groups <- c(
  "registration_only" = "Registration only",
  "measurements_only" = "Stations with measurements only",
  "measurements_and_rating_curves" = "Stations with measurements and rating curves"
)

measurements_and_curves_layer <- map_preview_data %>%
  filter(map_status == "measurements_and_rating_curves") %>%
  distinct(station_code, .keep_all = TRUE)

measurements_only_layer <- map_preview_data %>%
  filter(
    map_status == "measurements_only",
    !(station_code %in% measurements_and_curves_layer$station_code)
  ) %>%
  distinct(station_code, .keep_all = TRUE)

registration_layer <- map_preview_data %>%
  filter(
    map_status == "registration_only",
    !(station_code %in% c(measurements_and_curves_layer$station_code, measurements_only_layer$station_code))
  ) %>%
  distinct(station_code, .keep_all = TRUE)

# ------------------------------------------------------------
# Spatial symbology
# ------------------------------------------------------------

spatial_groups <- c(
  "brazil_boundary" = "Brazil boundary",
  "states" = "State boundaries",
  "basins" = "Macro basins",
  "rivers_large" = "Large rivers",
  "rivers_medium" = "Medium rivers",
  "rivers_small" = "Small rivers"
)

river_colors <- c(
  "rivers_large" = "#08519c",
  "rivers_medium" = "#3182bd",
  "rivers_small" = "#9ecae1"
)

river_weights <- c(
  "rivers_large" = 1.6,
  "rivers_medium" = 0.9,
  "rivers_small" = 0.45
)

river_opacity <- c(
  "rivers_large" = 0.85,
  "rivers_medium" = 0.65,
  "rivers_small" = 0.45
)

# ------------------------------------------------------------
# Create combined map
# ------------------------------------------------------------

m <- leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
  addProviderTiles(providers$CartoDB.Positron)

# Polygon/context layers
m <- add_poly_layer_if_present(
  m, available_layers,
  layer_key = "brazil_boundary",
  group = spatial_groups[["brazil_boundary"]],
  color = "#000000",
  weight = 2.2,
  opacity = 0.95
)

m <- add_poly_layer_if_present(
  m, available_layers,
  layer_key = "states",
  group = spatial_groups[["states"]],
  color = "#000000",
  weight = 0.6,
  opacity = 0.65
)

m <- add_poly_layer_if_present(
  m, available_layers,
  layer_key = "basins",
  group = spatial_groups[["basins"]],
  color = "#737373",
  weight = 0.6,
  opacity = 0.65,
  fill = TRUE,
  fill_color = ~basin_fill_color,
  fill_opacity = 0.05
)

# River layers: add small first, then medium, then large on top.
m <- add_line_layer_if_present(
  m, available_layers,
  layer_key = "rivers_small",
  group = spatial_groups[["rivers_small"]],
  color = river_colors[["rivers_small"]],
  weight = river_weights[["rivers_small"]],
  opacity = river_opacity[["rivers_small"]]
)

m <- add_line_layer_if_present(
  m, available_layers,
  layer_key = "rivers_medium",
  group = spatial_groups[["rivers_medium"]],
  color = river_colors[["rivers_medium"]],
  weight = river_weights[["rivers_medium"]],
  opacity = river_opacity[["rivers_medium"]]
)

m <- add_line_layer_if_present(
  m, available_layers,
  layer_key = "rivers_large",
  group = spatial_groups[["rivers_large"]],
  color = river_colors[["rivers_large"]],
  weight = river_weights[["rivers_large"]],
  opacity = river_opacity[["rivers_large"]]
)

# Station layers: registration first, data stations on top.
if (nrow(registration_layer) > 0) {
  m <- m %>%
    addCircleMarkers(
      data = registration_layer,
      lng = ~longitude,
      lat = ~latitude,
      group = status_groups[["registration_only"]],
      radius = status_radius[["registration_only"]],
      stroke = FALSE,
      fillColor = status_palette[["registration_only"]],
      fillOpacity = status_opacity[["registration_only"]],
      popup = ~popup
    )
}

if (nrow(measurements_only_layer) > 0) {
  m <- m %>%
    addCircleMarkers(
      data = measurements_only_layer,
      lng = ~longitude,
      lat = ~latitude,
      group = status_groups[["measurements_only"]],
      radius = status_radius[["measurements_only"]],
      stroke = TRUE,
      color = "#ffffff",
      weight = 0.5,
      fillColor = status_palette[["measurements_only"]],
      fillOpacity = status_opacity[["measurements_only"]],
      popup = ~popup
    )
}

if (nrow(measurements_and_curves_layer) > 0) {
  m <- m %>%
    addCircleMarkers(
      data = measurements_and_curves_layer,
      lng = ~longitude,
      lat = ~latitude,
      group = status_groups[["measurements_and_rating_curves"]],
      radius = status_radius[["measurements_and_rating_curves"]],
      stroke = TRUE,
      color = "#ffffff",
      weight = 0.7,
      fillColor = status_palette[["measurements_and_rating_curves"]],
      fillOpacity = status_opacity[["measurements_and_rating_curves"]],
      popup = ~popup
    )
}

# ------------------------------------------------------------
# Layer control and legends
# ------------------------------------------------------------

loaded_spatial_groups <- unname(spatial_groups[names(spatial_groups) %in% names(available_layers)])
station_groups <- unname(status_groups)

overlay_groups <- c(
  loaded_spatial_groups,
  station_groups
)

m <- m %>%
  addLayersControl(
    overlayGroups = overlay_groups,
    options = layersControlOptions(collapsed = FALSE)
  )

# Separate station legend.
m <- m %>%
  addLegend(
    position = "bottomright",
    colors = status_palette[c(
      "measurements_and_rating_curves",
      "measurements_only",
      "registration_only"
    )],
    labels = c(
      "Measurements and rating curves",
      "Measurements only",
      "Registration only"
    ),
    title = "Station status",
    opacity = 0.85
  )

# Separate river legend only for river layers that were actually loaded.
loaded_river_keys <- c("rivers_large", "rivers_medium", "rivers_small")
loaded_river_keys <- loaded_river_keys[loaded_river_keys %in% names(available_layers)]

if (length(loaded_river_keys) > 0) {
  m <- m %>%
    addLegend(
      position = "bottomleft",
      colors = river_colors[loaded_river_keys],
      labels = c(
        "rivers_large" = "Large rivers",
        "rivers_medium" = "Medium rivers",
        "rivers_small" = "Small rivers"
      )[loaded_river_keys],
      title = "River layers",
      opacity = 0.85
    )
}

m <- m %>%
  addControl(
    html = paste0(
      "<b>Spatial layers preview</b><br>",
      "Local data only. No live ANA API request.<br>",
      "Station points: ", nrow(map_preview_data), " / ", nrow(map_data), "<br>",
      "Loaded spatial layers: ", paste(names(available_layers), collapse = ", "), "<br>",
      "Read errors: ", nrow(read_errors)
    ),
    position = "topright"
  )

if (hide_registration_only_by_default) {
  m <- m %>%
    hideGroup(status_groups[["registration_only"]])
}

# Optional default visibility for context layers.
# State boundaries and macro basins start hidden when available, because they
# are context layers and can be turned on for inspection.
if ("states" %in% names(available_layers)) {
  m <- m %>% hideGroup(spatial_groups[["states"]])
}

if ("basins" %in% names(available_layers)) {
  m <- m %>% hideGroup(spatial_groups[["basins"]])
}

if (nrow(map_preview_data) > 0) {
  m <- m %>%
    fitBounds(
      lng1 = min(map_preview_data$longitude, na.rm = TRUE),
      lat1 = min(map_preview_data$latitude, na.rm = TRUE),
      lng2 = max(map_preview_data$longitude, na.rm = TRUE),
      lat2 = max(map_preview_data$latitude, na.rm = TRUE)
    )
}

saveWidget(
  widget = m,
  file = output_preview_html,
  selfcontained = TRUE
)

message("Finished preparing spatial layers.")
message("Prepared RDS: ", output_rds)
message("Prepared GPKG: ", output_gpkg)
message("Preview HTML: ", output_preview_html)
message("Preview CSV: ", output_preview_csv)
message("Manifest: ", output_manifest)
message("Missing inputs: ", output_missing)
message("Read errors: ", output_errors)
message("Available layers:")
print(names(available_layers))
message("Read errors table:")
print(read_errors)
