# ============================================================
# pipeline/R/062_calculate_station_quality_indices.R
#
# Purpose:
# Create station-assessment and preliminary quality-index tables
# for the Shiny app using only the local Shiny export database.
#
# Input:
#   exports/shiny_minimal.duckdb
#
# Outputs written to the same local DuckDB database:
#   station_assessment_summary
#   station_data_availability
#   station_measurement_indices
#   station_rating_curve_indices
#   station_cross_section_indices
#   station_quality_indices
#   station_map_status
#
# This script does not call ANA APIs, does not use credentials,
# and does not source acquisition/download scripts.
# ============================================================

library(DBI)
library(duckdb)
library(dplyr)

# ------------------------------------------------------------
# Paths
# ------------------------------------------------------------

shiny_db <- file.path("exports", "shiny_minimal.duckdb")
output_dir <- file.path("outputs", "station_assessment")

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(shiny_db)) {
  stop("Missing Shiny export database: ", shiny_db)
}

message("============================================================")
message("062_calculate_station_quality_indices")
message("============================================================")
message("Input database: ", shiny_db)
message("This script uses local DuckDB tables only.")

# ------------------------------------------------------------
# Helper functions
# ------------------------------------------------------------

safe_divide <- function(x, y) {
  x <- as.numeric(x)
  y <- as.numeric(y)
  ifelse(is.na(y) | y == 0, NA_real_, x / y)
}

safe_min_date <- function(x) {
  x <- as.Date(x)
  if (all(is.na(x))) return(as.Date(NA))
  min(x, na.rm = TRUE)
}

safe_max_date <- function(x) {
  x <- as.Date(x)
  if (all(is.na(x))) return(as.Date(NA))
  max(x, na.rm = TRUE)
}

safe_min_num <- function(x) {
  x <- as.numeric(x)
  if (all(is.na(x))) return(NA_real_)
  min(x, na.rm = TRUE)
}

safe_max_num <- function(x) {
  x <- as.numeric(x)
  if (all(is.na(x))) return(NA_real_)
  max(x, na.rm = TRUE)
}

class_pct <- function(x) {
  case_when(
    is.na(x) ~ "not_available",
    x >= 0.95 ~ "high",
    x >= 0.80 ~ "moderate",
    x > 0 ~ "low",
    TRUE ~ "none"
  )
}

class_overlap <- function(x) {
  case_when(
    is.na(x) ~ "not_available",
    x == 0 ~ "none_detected",
    x <= 5 ~ "low",
    x <= 20 ~ "moderate",
    TRUE ~ "high"
  )
}

get_optional_column <- function(df, column_name, default = NA) {
  if (column_name %in% names(df)) {
    df[[column_name]]
  } else {
    rep(default, nrow(df))
  }
}

add_missing_columns <- function(df, columns, default = NA) {
  for (col in columns) {
    if (!col %in% names(df)) {
      df[[col]] <- default
    }
  }
  df
}

count_overlaps <- function(df) {
  if (nrow(df) < 2) return(0L)
  
  df <- df %>%
    mutate(
      valid_from_date = as.Date(valid_from),
      valid_to_date = as.Date(valid_to),
      valid_to_for_overlap = ifelse(
        is.na(valid_to_date),
        as.Date("9999-12-31"),
        valid_to_date
      ),
      valid_to_for_overlap = as.Date(valid_to_for_overlap, origin = "1970-01-01")
    ) %>%
    filter(!is.na(valid_from_date)) %>%
    distinct(rating_curve_id, valid_from_date, valid_to_for_overlap) %>%
    arrange(valid_from_date, valid_to_for_overlap)
  
  if (nrow(df) < 2) return(0L)
  
  n_overlap <- 0L
  
  for (i in seq_len(nrow(df) - 1L)) {
    for (j in seq.int(i + 1L, nrow(df))) {
      if (
        df$valid_from_date[j] <= df$valid_to_for_overlap[i] &&
        df$valid_to_for_overlap[j] >= df$valid_from_date[i]
      ) {
        n_overlap <- n_overlap + 1L
      }
    }
  }
  
  n_overlap
}

make_index <- function(
    data,
    group,
    name,
    numeric_value,
    text_value,
    unit,
    class,
    direction,
    description,
    requirement,
    available_now,
    order
) {
  data.frame(
    station_code = data$station_code,
    index_group = group,
    index_name = name,
    index_value_numeric = numeric_value,
    index_value_text = text_value,
    index_unit = unit,
    index_class = class,
    index_direction = direction,
    index_description = description,
    data_requirement = requirement,
    can_be_calculated_now = available_now,
    display_order = order,
    stringsAsFactors = FALSE
  )
}

# ------------------------------------------------------------
# Connect and read local data
# ------------------------------------------------------------

con <- dbConnect(duckdb::duckdb(), shiny_db, read_only = FALSE)
on.exit(dbDisconnect(con), add = TRUE)

required_tables <- c(
  "stations_minimal",
  "station_discharge_products_summary",
  "discharge_measurements_summary_by_station",
  "rating_curve_summary",
  "rating_curves",
  "cross_sections",
  "cross_section_vertices",
  "cross_section_summary"
)

missing_tables <- setdiff(required_tables, dbListTables(con))

if (length(missing_tables) > 0) {
  stop("Missing required local table(s): ", paste(missing_tables, collapse = ", "))
}

stations <- dbReadTable(con, "stations_minimal") %>%
  mutate(station_code = as.character(station_code))

products <- dbReadTable(con, "station_discharge_products_summary") %>%
  mutate(station_code = as.character(station_code))

measurements <- dbReadTable(con, "discharge_measurements_summary_by_station") %>%
  mutate(station_code = as.character(station_code))

rating_summary <- dbReadTable(con, "rating_curve_summary") %>%
  mutate(
    station_code = as.character(station_code),
    rating_curve_id = as.character(rating_curve_id)
  )

rating_segments <- dbReadTable(con, "rating_curves") %>%
  mutate(
    station_code = as.character(station_code),
    rating_curve_id = as.character(rating_curve_id),
    rating_curve_segment_id = as.character(rating_curve_segment_id)
  )

cross_sections <- dbReadTable(con, "cross_sections") %>%
  mutate(
    station_code = as.character(station_code),
    cross_section_id = as.character(cross_section_id)
  )

cross_section_summary <- dbReadTable(con, "cross_section_summary") %>%
  mutate(station_code = as.character(station_code))

rating_summary <- add_missing_columns(
  rating_summary,
  c(
    "consistency_level",
    "n_segments",
    "valid_from",
    "valid_to",
    "stage_min_cm",
    "stage_max_cm"
  )
)

rating_segments <- add_missing_columns(
  rating_segments,
  c(
    "segment_number",
    "curve_type",
    "equation_type",
    "stage_min_cm",
    "stage_max_cm",
    "coefficient_a",
    "coefficient_h0",
    "coefficient_n"
  )
)

cross_sections <- add_missing_columns(
  cross_sections,
  c(
    "measurement_datetime",
    "consistency_level",
    "survey_number",
    "section_type",
    "n_vertices",
    "n_vertices_reported",
    "distance_pipf_m",
    "x_distance_min_m",
    "x_distance_max_m",
    "y_stage_min_cm",
    "y_stage_max_cm",
    "geometry_stage_step_cm",
    "vertex_distance_min_m",
    "vertex_distance_max_m",
    "vertex_stage_min_cm",
    "vertex_stage_max_cm",
    "n_missing_vertex_distance",
    "n_missing_vertex_stage"
  )
)

# ------------------------------------------------------------
# Discharge-measurement indices
# ------------------------------------------------------------

station_measurement_indices <- measurements %>%
  mutate(
    first_measurement_date = as.Date(first_measurement_datetime),
    last_measurement_date = as.Date(last_measurement_datetime),
    measurement_period_days = as.numeric(last_measurement_date - first_measurement_date),
    measurement_period_years = measurement_period_days / 365.25,
    pct_stage_values = safe_divide(n_stage_values, n_measurements),
    pct_discharge_values = safe_divide(n_discharge_values, n_measurements),
    pct_consistency_level_1 = safe_divide(n_consistency_level_1, n_measurements),
    pct_consistency_level_2 = safe_divide(n_consistency_level_2, n_measurements),
    stage_range_cm = stage_max_cm - stage_min_cm,
    discharge_range_m3s = discharge_max_m3s - discharge_min_m3s,
    has_discharge_measurements_processed = !is.na(n_measurements) & n_measurements > 0,
    measurement_record_class = case_when(
      is.na(n_measurements) | n_measurements <= 0 ~ "none",
      n_measurements < 5 ~ "very_limited",
      n_measurements < 20 ~ "limited",
      n_measurements < 50 ~ "moderate",
      TRUE ~ "substantial"
    ),
    measurement_temporal_class = case_when(
      is.na(n_years_with_measurements) | n_years_with_measurements <= 0 ~ "none",
      n_years_with_measurements < 3 ~ "very_short",
      n_years_with_measurements < 10 ~ "short",
      n_years_with_measurements < 20 ~ "moderate",
      TRUE ~ "long"
    )
  )

# ------------------------------------------------------------
# Rating-curve indices
# ------------------------------------------------------------

rating_curve_station_summary <- rating_summary %>%
  group_by(station_code) %>%
  summarise(
    n_rating_curves = n_distinct(rating_curve_id, na.rm = TRUE),
    n_rating_curves_summary_rows = n(),
    n_consistency_levels_rating_curves = n_distinct(consistency_level, na.rm = TRUE),
    n_rating_curve_segments_from_summary = sum(n_segments, na.rm = TRUE),
    n_curves_missing_valid_from = sum(is.na(valid_from)),
    n_curves_missing_valid_to = sum(is.na(valid_to)),
    n_curves_missing_stage_bounds = sum(is.na(stage_min_cm) | is.na(stage_max_cm)),
    n_curves_with_invalid_stage_range = sum(
      !is.na(stage_min_cm) & !is.na(stage_max_cm) & stage_max_cm < stage_min_cm
    ),
    n_curves_with_invalid_validity_dates = sum(
      !is.na(valid_from) & !is.na(valid_to) & as.Date(valid_to) < as.Date(valid_from)
    ),
    first_rating_curve_valid_from = safe_min_date(valid_from),
    last_rating_curve_valid_from = safe_max_date(valid_from),
    first_rating_curve_valid_to = safe_min_date(valid_to),
    last_rating_curve_valid_to = safe_max_date(valid_to),
    rating_curve_stage_min_cm = safe_min_num(stage_min_cm),
    rating_curve_stage_max_cm = safe_max_num(stage_max_cm),
    .groups = "drop"
  ) %>%
  mutate(
    rating_curve_validity_period_days = as.numeric(last_rating_curve_valid_to - first_rating_curve_valid_from),
    rating_curve_validity_period_years = rating_curve_validity_period_days / 365.25,
    rating_curve_stage_range_cm = rating_curve_stage_max_cm - rating_curve_stage_min_cm
  )

rating_curve_segment_summary <- rating_segments %>%
  group_by(station_code) %>%
  summarise(
    n_rating_curve_segments = n(),
    n_rating_curve_ids_from_segments = n_distinct(rating_curve_id, na.rm = TRUE),
    n_segment_numbers = n_distinct(segment_number, na.rm = TRUE),
    n_curve_types = n_distinct(curve_type, na.rm = TRUE),
    n_equation_types = n_distinct(equation_type, na.rm = TRUE),
    n_segments_missing_stage_bounds = sum(is.na(stage_min_cm) | is.na(stage_max_cm)),
    n_segments_with_invalid_stage_range = sum(
      !is.na(stage_min_cm) & !is.na(stage_max_cm) & stage_max_cm < stage_min_cm
    ),
    n_segments_missing_any_coefficient = sum(
      is.na(coefficient_a) | is.na(coefficient_h0) | is.na(coefficient_n)
    ),
    segment_stage_min_cm = safe_min_num(stage_min_cm),
    segment_stage_max_cm = safe_max_num(stage_max_cm),
    .groups = "drop"
  ) %>%
  mutate(segment_stage_range_cm = segment_stage_max_cm - segment_stage_min_cm)

rating_overlap_indices <- bind_rows(
  lapply(
    split(rating_summary, rating_summary$station_code),
    function(x) {
      data.frame(
        station_code = x$station_code[1],
        n_overlapping_rating_curve_pairs = count_overlaps(x),
        stringsAsFactors = FALSE
      )
    }
  )
)

station_rating_curve_indices <- rating_curve_station_summary %>%
  full_join(rating_curve_segment_summary, by = "station_code") %>%
  full_join(rating_overlap_indices, by = "station_code") %>%
  mutate(
    n_rating_curves = coalesce(n_rating_curves, n_rating_curve_ids_from_segments),
    n_rating_curve_segments = coalesce(
      n_rating_curve_segments,
      n_rating_curve_segments_from_summary,
      0L
    ),
    n_overlapping_rating_curve_pairs = coalesce(n_overlapping_rating_curve_pairs, 0L),
    has_rating_curves_processed = !is.na(n_rating_curves) & n_rating_curves > 0,
    rating_curve_record_class = case_when(
      is.na(n_rating_curves) | n_rating_curves <= 0 ~ "none",
      n_rating_curves == 1 ~ "single_curve",
      n_rating_curves < 5 ~ "few_curves",
      n_rating_curves < 20 ~ "multiple_curves",
      TRUE ~ "many_curves"
    ),
    rating_curve_overlap_class = class_overlap(n_overlapping_rating_curve_pairs),
    rating_curve_stage_range_class = case_when(
      is.na(rating_curve_stage_range_cm) | rating_curve_stage_range_cm <= 0 ~ "not_available",
      rating_curve_stage_range_cm < 100 ~ "narrow",
      rating_curve_stage_range_cm < 500 ~ "moderate",
      TRUE ~ "wide"
    )
  )

# ------------------------------------------------------------
# Cross-section indices
# ------------------------------------------------------------

cross_section_survey_summary <- cross_sections %>%
  group_by(station_code) %>%
  summarise(
    n_cross_sections = n_distinct(cross_section_id, na.rm = TRUE),
    n_cross_section_rows = n(),
    n_cross_section_surveys = n_distinct(survey_number, na.rm = TRUE),
    n_cross_section_consistency_levels = n_distinct(consistency_level, na.rm = TRUE),
    first_cross_section_datetime = safe_min_date(measurement_datetime),
    last_cross_section_datetime = safe_max_date(measurement_datetime),
    n_cross_sections_missing_datetime = sum(is.na(measurement_datetime)),
    n_cross_sections_missing_vertex_count = sum(is.na(n_vertices)),
    n_cross_sections_missing_distance = sum(is.na(vertex_distance_min_m) | is.na(vertex_distance_max_m)),
    n_cross_sections_missing_stage = sum(is.na(vertex_stage_min_cm) | is.na(vertex_stage_max_cm)),
    n_cross_sections_with_missing_vertex_distance = sum(coalesce(n_missing_vertex_distance, 0L) > 0),
    n_cross_sections_with_missing_vertex_stage = sum(coalesce(n_missing_vertex_stage, 0L) > 0),
    n_cross_section_vertices_from_sections = sum(n_vertices, na.rm = TRUE),
    cross_section_distance_min_m = safe_min_num(vertex_distance_min_m),
    cross_section_distance_max_m = safe_max_num(vertex_distance_max_m),
    cross_section_stage_min_cm = safe_min_num(vertex_stage_min_cm),
    cross_section_stage_max_cm = safe_max_num(vertex_stage_max_cm),
    .groups = "drop"
  ) %>%
  mutate(
    cross_section_period_days = as.numeric(last_cross_section_datetime - first_cross_section_datetime),
    cross_section_period_years = cross_section_period_days / 365.25,
    cross_section_distance_span_m = cross_section_distance_max_m - cross_section_distance_min_m,
    cross_section_stage_range_cm = cross_section_stage_max_cm - cross_section_stage_min_cm
  )

cross_section_vertex_summary <- dbGetQuery(
  con,
  "SELECT
     station_code,
     COUNT(*) AS n_cross_section_vertices,
     COUNT(DISTINCT cross_section_id) AS n_cross_section_ids_from_vertices,
     MIN(vertex_distance_m) AS vertex_distance_min_m,
     MAX(vertex_distance_m) AS vertex_distance_max_m,
     MIN(vertex_stage_cm) AS vertex_stage_min_cm,
     MAX(vertex_stage_cm) AS vertex_stage_max_cm,
     SUM(CASE WHEN vertex_distance_m IS NULL THEN 1 ELSE 0 END) AS n_vertices_missing_distance,
     SUM(CASE WHEN vertex_stage_cm IS NULL THEN 1 ELSE 0 END) AS n_vertices_missing_stage
   FROM cross_section_vertices
   GROUP BY station_code"
) %>%
  mutate(station_code = as.character(station_code))

cross_section_summary_rows <- cross_section_summary %>%
  group_by(station_code) %>%
  summarise(
    has_cross_section_summary_row = TRUE,
    n_cross_section_summary_rows = n(),
    .groups = "drop"
  )

station_cross_section_indices <- cross_section_survey_summary %>%
  full_join(cross_section_vertex_summary, by = "station_code") %>%
  full_join(cross_section_summary_rows, by = "station_code") %>%
  mutate(
    n_cross_sections = coalesce(n_cross_sections, n_cross_section_ids_from_vertices, 0L),
    n_cross_section_vertices = coalesce(n_cross_section_vertices, n_cross_section_vertices_from_sections, 0L),
    n_cross_section_summary_rows = coalesce(n_cross_section_summary_rows, 0L),
    has_cross_section_summary_row = coalesce(has_cross_section_summary_row, FALSE),
    has_cross_sections_processed = !is.na(n_cross_sections) & n_cross_sections > 0,
    has_cross_section_vertices_processed = !is.na(n_cross_section_vertices) & n_cross_section_vertices > 0,
    pct_vertices_with_distance = 1 - safe_divide(n_vertices_missing_distance, n_cross_section_vertices),
    pct_vertices_with_stage = 1 - safe_divide(n_vertices_missing_stage, n_cross_section_vertices),
    cross_section_record_class = case_when(
      is.na(n_cross_sections) | n_cross_sections <= 0 ~ "none",
      n_cross_sections == 1 ~ "single_profile",
      n_cross_sections < 5 ~ "few_profiles",
      n_cross_sections < 20 ~ "multiple_profiles",
      TRUE ~ "many_profiles"
    ),
    cross_section_vertex_class = case_when(
      is.na(n_cross_section_vertices) | n_cross_section_vertices <= 0 ~ "none",
      n_cross_section_vertices < 20 ~ "very_limited",
      n_cross_section_vertices < 100 ~ "limited",
      n_cross_section_vertices < 500 ~ "moderate",
      TRUE ~ "substantial"
    ),
    cross_section_temporal_class = case_when(
      is.na(cross_section_period_years) ~ "not_available",
      cross_section_period_years <= 0 ~ "single_date_or_unknown_span",
      cross_section_period_years < 3 ~ "very_short",
      cross_section_period_years < 10 ~ "short",
      cross_section_period_years < 20 ~ "moderate",
      TRUE ~ "long"
    ),
    cross_section_geometry_class = case_when(
      !has_cross_section_vertices_processed ~ "not_available",
      is.na(cross_section_distance_span_m) | is.na(cross_section_stage_range_cm) ~ "incomplete_geometry",
      cross_section_distance_span_m <= 0 | cross_section_stage_range_cm <= 0 ~ "invalid_or_flat_geometry",
      TRUE ~ "geometry_available"
    )
  )

# ------------------------------------------------------------
# Station availability and summary table
# ------------------------------------------------------------

product_summary <- data.frame(
  station_code = products$station_code,
  n_discharge_measurements_processed_product = get_optional_column(products, "n_discharge_measurements"),
  n_rating_curves_product = get_optional_column(products, "n_rating_curves"),
  n_rating_curve_segments_product = get_optional_column(products, "n_rating_curve_segments"),
  has_discharge_measurements_processed_product = as.logical(
    get_optional_column(products, "has_discharge_measurements_processed", FALSE)
  ),
  has_rating_curves_processed_product = as.logical(
    get_optional_column(products, "has_rating_curves_processed", FALSE)
  ),
  stringsAsFactors = FALSE
)

station_data_availability <- stations %>%
  left_join(product_summary, by = "station_code") %>%
  left_join(
    station_measurement_indices %>%
      select(
        station_code,
        n_measurements,
        n_years_with_measurements,
        pct_stage_values,
        pct_discharge_values,
        measurement_record_class,
        measurement_temporal_class
      ),
    by = "station_code"
  ) %>%
  left_join(
    station_rating_curve_indices %>%
      select(
        station_code,
        n_rating_curves,
        n_rating_curve_segments,
        n_overlapping_rating_curve_pairs,
        rating_curve_record_class,
        rating_curve_overlap_class,
        rating_curve_stage_range_class
      ),
    by = "station_code"
  ) %>%
  left_join(
    station_cross_section_indices %>%
      select(
        station_code,
        n_cross_sections,
        n_cross_section_vertices,
        first_cross_section_datetime,
        last_cross_section_datetime,
        cross_section_period_years,
        cross_section_distance_span_m,
        cross_section_stage_range_cm,
        has_cross_sections_processed,
        has_cross_section_vertices_processed,
        cross_section_record_class,
        cross_section_vertex_class,
        cross_section_temporal_class,
        cross_section_geometry_class
      ),
    by = "station_code"
  ) %>%
  mutate(
    has_coordinates = !is.na(latitude) & !is.na(longitude),
    has_station_name = !is.na(station_name) & station_name != "",
    has_basin_info = !is.na(basin_code) | !is.na(basin_name),
    has_municipality_info = !is.na(municipality) & municipality != "",
    has_drainage_area = !is.na(drainage_area) & drainage_area > 0,
    has_registered_discharge_period = !is.na(discharge_start_date) | !is.na(discharge_end_date),
    has_registered_stage_period = !is.na(stage_start_date) | !is.na(stage_end_date),
    has_registered_rainfall_period = !is.na(rainfall_start_date) | !is.na(rainfall_end_date),
    has_registered_telemetric_period = !is.na(telemetric_start_date) | !is.na(telemetric_end_date),
    has_discharge_measurements_processed = coalesce(
      has_discharge_measurements_processed_product,
      !is.na(n_measurements) & n_measurements > 0,
      FALSE
    ),
    has_rating_curves_processed = coalesce(
      has_rating_curves_processed_product,
      !is.na(n_rating_curves) & n_rating_curves > 0,
      FALSE
    ),
    has_cross_sections_processed = coalesce(has_cross_sections_processed, FALSE),
    has_cross_section_vertices_processed = coalesce(has_cross_section_vertices_processed, FALSE),
    has_basic_station_assessment_data = has_discharge_measurements_processed & has_rating_curves_processed,
    
    # Future local derived products, not available in the current export.
    has_daily_stage_series_processed = FALSE,
    has_daily_discharge_series_processed = FALSE
  )

station_assessment_summary <- station_data_availability %>%
  mutate(
    registration_score =
      ifelse(has_coordinates, 10, 0) +
      ifelse(has_station_name, 5, 0) +
      ifelse(has_basin_info, 5, 0) +
      ifelse(has_municipality_info, 5, 0) +
      ifelse(has_drainage_area, 10, 0),
    
    measurement_amount_score = case_when(
      is.na(n_measurements) | n_measurements <= 0 ~ 0,
      n_measurements < 5 ~ 4,
      n_measurements < 20 ~ 8,
      n_measurements < 50 ~ 12,
      TRUE ~ 15
    ),
    
    measurement_temporal_score = case_when(
      is.na(n_years_with_measurements) | n_years_with_measurements <= 0 ~ 0,
      n_years_with_measurements < 3 ~ 3,
      n_years_with_measurements < 10 ~ 6,
      n_years_with_measurements < 20 ~ 8,
      TRUE ~ 10
    ),
    
    measurement_completeness_score = case_when(
      is.na(pct_stage_values) & is.na(pct_discharge_values) ~ 0,
      coalesce(pct_stage_values, 0) >= 0.95 & coalesce(pct_discharge_values, 0) >= 0.95 ~ 10,
      coalesce(pct_stage_values, 0) >= 0.80 & coalesce(pct_discharge_values, 0) >= 0.80 ~ 7,
      coalesce(pct_stage_values, 0) > 0 & coalesce(pct_discharge_values, 0) > 0 ~ 4,
      TRUE ~ 0
    ),
    
    rating_curve_presence_score = case_when(
      is.na(n_rating_curves) | n_rating_curves <= 0 ~ 0,
      n_rating_curves == 1 ~ 8,
      n_rating_curves < 5 ~ 10,
      TRUE ~ 12
    ),
    
    rating_curve_segment_score = case_when(
      is.na(n_rating_curve_segments) | n_rating_curve_segments <= 0 ~ 0,
      n_rating_curve_segments < 5 ~ 4,
      n_rating_curve_segments < 20 ~ 6,
      TRUE ~ 8
    ),
    
    rating_curve_overlap_score = case_when(
      is.na(n_rating_curves) | n_rating_curves <= 0 ~ 0,
      is.na(n_overlapping_rating_curve_pairs) ~ 0,
      n_overlapping_rating_curve_pairs == 0 ~ 5,
      n_overlapping_rating_curve_pairs <= 5 ~ 3,
      TRUE ~ 1
    ),
    
    cross_section_presence_score = case_when(
      is.na(n_cross_sections) | n_cross_sections <= 0 ~ 0,
      n_cross_sections == 1 ~ 3,
      TRUE ~ 5
    ),
    
    preliminary_information_score =
      registration_score +
      measurement_amount_score +
      measurement_temporal_score +
      measurement_completeness_score +
      rating_curve_presence_score +
      rating_curve_segment_score +
      rating_curve_overlap_score +
      cross_section_presence_score,
    
    preliminary_information_class = case_when(
      preliminary_information_score >= 80 ~ "high_information",
      preliminary_information_score >= 60 ~ "moderate_information",
      preliminary_information_score >= 40 ~ "limited_information",
      preliminary_information_score >= 20 ~ "basic_registration_or_sparse_products",
      TRUE ~ "registration_only_or_incomplete"
    ),
    
    station_assessment_status = case_when(
      !has_coordinates ~ "missing_coordinates",
      has_discharge_measurements_processed & has_rating_curves_processed ~ "measurements_and_rating_curves",
      has_discharge_measurements_processed & !has_rating_curves_processed ~ "measurements_only",
      !has_discharge_measurements_processed & has_rating_curves_processed ~ "rating_curves_only",
      TRUE ~ "registration_only"
    ),
    
    station_assessment_status_label = case_when(
      station_assessment_status == "missing_coordinates" ~ "Missing coordinates",
      station_assessment_status == "measurements_and_rating_curves" ~ "Discharge measurements and rating curves",
      station_assessment_status == "measurements_only" ~ "Discharge measurements only",
      station_assessment_status == "rating_curves_only" ~ "Rating curves only",
      TRUE ~ "Station registration only"
    )
  )

station_map_status <- station_assessment_summary %>%
  mutate(
    map_status = station_assessment_status,
    map_status_label = station_assessment_status_label,
    map_priority = case_when(
      map_status == "measurements_and_rating_curves" ~ 1L,
      map_status == "measurements_only" ~ 2L,
      map_status == "rating_curves_only" ~ 3L,
      map_status == "registration_only" ~ 4L,
      TRUE ~ 5L
    ),
    station_label = paste0(
      station_code,
      " - ",
      ifelse(is.na(station_name), "Unnamed station", station_name)
    )
  ) %>%
  select(
    station_code,
    station_label,
    station_name,
    station_type,
    uf,
    municipality,
    basin_code,
    basin_name,
    latitude,
    longitude,
    drainage_area,
    has_coordinates,
    has_discharge_measurements_processed,
    has_rating_curves_processed,
    has_cross_sections_processed,
    n_cross_sections,
    n_cross_section_vertices,
    preliminary_information_score,
    preliminary_information_class,
    map_status,
    map_status_label,
    map_priority
  )

# ------------------------------------------------------------
# Long index table for Shiny
# ------------------------------------------------------------

station_quality_indices <- bind_rows(
  make_index(
    station_assessment_summary,
    "Station registration",
    "Coordinates available",
    as.numeric(station_assessment_summary$has_coordinates),
    ifelse(station_assessment_summary$has_coordinates, "Yes", "No"),
    NA_character_,
    ifelse(station_assessment_summary$has_coordinates, "available", "missing"),
    "presence",
    "Indicates whether latitude and longitude are available for mapping.",
    "stations_minimal.latitude and stations_minimal.longitude",
    TRUE,
    10
  ),
  
  make_index(
    station_assessment_summary,
    "Station registration",
    "Drainage area available",
    as.numeric(station_assessment_summary$has_drainage_area),
    ifelse(station_assessment_summary$has_drainage_area, "Yes", "No"),
    NA_character_,
    ifelse(station_assessment_summary$has_drainage_area, "available", "missing"),
    "presence",
    "Indicates whether drainage area is available and positive.",
    "stations_minimal.drainage_area",
    TRUE,
    20
  ),
  
  make_index(
    station_assessment_summary,
    "Station registration",
    "Registered discharge period available",
    as.numeric(station_assessment_summary$has_registered_discharge_period),
    ifelse(station_assessment_summary$has_registered_discharge_period, "Yes", "No"),
    NA_character_,
    ifelse(station_assessment_summary$has_registered_discharge_period, "available", "missing"),
    "presence",
    "Indicates whether station metadata reports a discharge-measurement availability period.",
    "stations_minimal.discharge_start_date and discharge_end_date",
    TRUE,
    30
  ),
  
  make_index(
    station_assessment_summary,
    "Discharge measurements",
    "Processed discharge measurements available",
    as.numeric(station_assessment_summary$has_discharge_measurements_processed),
    ifelse(station_assessment_summary$has_discharge_measurements_processed, "Yes", "No"),
    NA_character_,
    ifelse(station_assessment_summary$has_discharge_measurements_processed, "available", "missing"),
    "presence",
    "Indicates whether cleaned discharge-measurement records are available.",
    "discharge_measurements_summary_by_station",
    TRUE,
    100
  ),
  
  make_index(
    station_assessment_summary,
    "Discharge measurements",
    "Number of discharge measurements",
    station_assessment_summary$n_measurements,
    as.character(station_assessment_summary$n_measurements),
    "records",
    station_assessment_summary$measurement_record_class,
    "higher_is_more_informative",
    "Number of cleaned discharge-measurement records available for the station.",
    "discharge_measurements_summary_by_station.n_measurements",
    TRUE,
    110
  ),
  
  make_index(
    station_assessment_summary,
    "Discharge measurements",
    "Years with discharge measurements",
    station_assessment_summary$n_years_with_measurements,
    as.character(station_assessment_summary$n_years_with_measurements),
    "years",
    station_assessment_summary$measurement_temporal_class,
    "higher_is_more_informative",
    "Number of calendar years with at least one discharge measurement.",
    "discharge_measurements_summary_by_station.n_years_with_measurements",
    TRUE,
    120
  ),
  
  make_index(
    station_assessment_summary,
    "Discharge measurements",
    "Stage availability in discharge measurements",
    station_assessment_summary$pct_stage_values,
    ifelse(
      is.na(station_assessment_summary$pct_stage_values),
      NA_character_,
      paste0(round(100 * station_assessment_summary$pct_stage_values, 1), "%")
    ),
    "%",
    class_pct(station_assessment_summary$pct_stage_values),
    "higher_is_more_complete",
    "Percentage of discharge-measurement records with stage values.",
    "discharge_measurements_summary_by_station.n_stage_values and n_measurements",
    TRUE,
    130
  ),
  
  make_index(
    station_assessment_summary,
    "Discharge measurements",
    "Discharge availability in discharge measurements",
    station_assessment_summary$pct_discharge_values,
    ifelse(
      is.na(station_assessment_summary$pct_discharge_values),
      NA_character_,
      paste0(round(100 * station_assessment_summary$pct_discharge_values, 1), "%")
    ),
    "%",
    class_pct(station_assessment_summary$pct_discharge_values),
    "higher_is_more_complete",
    "Percentage of discharge-measurement records with discharge values.",
    "discharge_measurements_summary_by_station.n_discharge_values and n_measurements",
    TRUE,
    140
  ),
  
  make_index(
    station_assessment_summary,
    "Rating curves",
    "Processed rating curves available",
    as.numeric(station_assessment_summary$has_rating_curves_processed),
    ifelse(station_assessment_summary$has_rating_curves_processed, "Yes", "No"),
    NA_character_,
    ifelse(station_assessment_summary$has_rating_curves_processed, "available", "missing"),
    "presence",
    "Indicates whether cleaned rating-curve records are available.",
    "rating_curve_summary and rating_curves",
    TRUE,
    200
  ),
  
  make_index(
    station_assessment_summary,
    "Rating curves",
    "Number of rating curves",
    station_assessment_summary$n_rating_curves,
    as.character(station_assessment_summary$n_rating_curves),
    "curves",
    station_assessment_summary$rating_curve_record_class,
    "context_dependent",
    "Number of distinct rating curves available for the station.",
    "rating_curve_summary.rating_curve_id",
    TRUE,
    210
  ),
  
  make_index(
    station_assessment_summary,
    "Rating curves",
    "Number of rating-curve segments",
    station_assessment_summary$n_rating_curve_segments,
    as.character(station_assessment_summary$n_rating_curve_segments),
    "segments",
    case_when(
      is.na(station_assessment_summary$n_rating_curve_segments) |
        station_assessment_summary$n_rating_curve_segments <= 0 ~ "none",
      station_assessment_summary$n_rating_curve_segments == 1 ~ "single_segment",
      station_assessment_summary$n_rating_curve_segments < 5 ~ "few_segments",
      station_assessment_summary$n_rating_curve_segments < 20 ~ "multiple_segments",
      TRUE ~ "many_segments"
    ),
    "context_dependent",
    "Number of rating-curve segments available for the station.",
    "rating_curves.rating_curve_segment_id",
    TRUE,
    220
  ),
  
  make_index(
    station_assessment_summary,
    "Rating curves",
    "Overlapping rating-curve validity pairs",
    station_assessment_summary$n_overlapping_rating_curve_pairs,
    as.character(station_assessment_summary$n_overlapping_rating_curve_pairs),
    "pairs",
    station_assessment_summary$rating_curve_overlap_class,
    "lower_is_preferable_for_screening",
    "Number of rating-curve pairs with overlapping validity periods. This is a screening indicator only.",
    "rating_curve_summary.valid_from and valid_to",
    TRUE,
    230
  ),
  
  make_index(
    station_assessment_summary,
    "Cross sections",
    "Processed cross-section profiles available",
    as.numeric(station_assessment_summary$has_cross_sections_processed),
    ifelse(station_assessment_summary$has_cross_sections_processed, "Yes", "No"),
    NA_character_,
    ifelse(station_assessment_summary$has_cross_sections_processed, "available", "missing"),
    "presence",
    "Indicates whether cleaned cross-section profile records are available.",
    "cross_sections and cross_section_summary",
    TRUE,
    300
  ),
  
  make_index(
    station_assessment_summary,
    "Cross sections",
    "Number of cross-section profiles",
    station_assessment_summary$n_cross_sections,
    as.character(station_assessment_summary$n_cross_sections),
    "profiles",
    station_assessment_summary$cross_section_record_class,
    "higher_is_more_informative",
    "Number of cross-section profile records available for the station.",
    "cross_sections.cross_section_id",
    TRUE,
    310
  ),
  
  make_index(
    station_assessment_summary,
    "Cross sections",
    "Number of cross-section vertices",
    station_assessment_summary$n_cross_section_vertices,
    as.character(station_assessment_summary$n_cross_section_vertices),
    "vertices",
    station_assessment_summary$cross_section_vertex_class,
    "higher_is_more_informative",
    "Number of vertex records available for plotting cross-section profiles.",
    "cross_section_vertices.cross_section_vertex_id",
    TRUE,
    320
  ),
  
  make_index(
    station_assessment_summary,
    "Cross sections",
    "Cross-section distance span",
    station_assessment_summary$cross_section_distance_span_m,
    ifelse(
      is.na(station_assessment_summary$cross_section_distance_span_m),
      NA_character_,
      as.character(round(station_assessment_summary$cross_section_distance_span_m, 2))
    ),
    "m",
    station_assessment_summary$cross_section_geometry_class,
    "context_dependent",
    "Approximate maximum horizontal span covered by available cross-section vertices.",
    "cross_sections.vertex_distance_min_m and vertex_distance_max_m",
    TRUE,
    330
  ),
  
  make_index(
    station_assessment_summary,
    "Station assessment",
    "Preliminary information score",
    station_assessment_summary$preliminary_information_score,
    as.character(station_assessment_summary$preliminary_information_score),
    "0-100",
    station_assessment_summary$preliminary_information_class,
    "higher_is_more_informative",
    "Completeness-oriented screening score based only on the current local export. It is not a final hydrological quality score.",
    "station metadata, discharge-measurement summaries, rating-curve summaries, and cross-section availability",
    TRUE,
    400
  ),
  
  make_index(
    station_assessment_summary,
    "Future indices",
    "Daily stage/discharge gap indicators",
    NA_real_,
    "Not available in current Shiny export",
    NA_character_,
    "not_calculable_current_export",
    "lower_is_preferable",
    "Future index. Requires complete or compacted daily stage/discharge gap summaries.",
    "future daily stage/discharge tables or gap-summary table",
    FALSE,
    500
  ),
  
  make_index(
    station_assessment_summary,
    "Future indices",
    "Discharge without valid rating curve",
    NA_real_,
    "Not available in current Shiny export",
    NA_character_,
    "not_calculable_current_export",
    "lower_is_preferable",
    "Future index. Requires comparison between discharge records and rating-curve validity periods.",
    "future daily discharge/stage summaries and rating-curve validity matching",
    FALSE,
    510
  )
) %>%
  arrange(station_code, display_order)

# ------------------------------------------------------------
# Write tables
# ------------------------------------------------------------

new_tables <- c(
  "station_assessment_summary",
  "station_data_availability",
  "station_measurement_indices",
  "station_rating_curve_indices",
  "station_cross_section_indices",
  "station_quality_indices",
  "station_map_status"
)

dbWriteTable(con, "station_assessment_summary", station_assessment_summary, overwrite = TRUE)
dbWriteTable(con, "station_data_availability", station_data_availability, overwrite = TRUE)
dbWriteTable(con, "station_measurement_indices", station_measurement_indices, overwrite = TRUE)
dbWriteTable(con, "station_rating_curve_indices", station_rating_curve_indices, overwrite = TRUE)
dbWriteTable(con, "station_cross_section_indices", station_cross_section_indices, overwrite = TRUE)
dbWriteTable(con, "station_quality_indices", station_quality_indices, overwrite = TRUE)
dbWriteTable(con, "station_map_status", station_map_status, overwrite = TRUE)

# ------------------------------------------------------------
# Update metadata
# ------------------------------------------------------------

metadata_update <- data.frame(
  key = c(
    "stage_09b_station_assessment_processed_at",
    "stage_09b_station_assessment_script",
    "stage_09b_station_assessment_source",
    "stage_09b_station_assessment_note",
    "stage_09b_station_assessment_cross_sections"
  ),
  value = c(
    as.character(Sys.time()),
    "pipeline/R/062_calculate_station_quality_indices.R",
    "exports/shiny_minimal.duckdb",
    "Preliminary indices are local screening/completeness indicators, not final hydrological quality decisions.",
    "Cross-section availability is now calculated from cross_sections, cross_section_vertices, and cross_section_summary."
  ),
  stringsAsFactors = FALSE
)

if ("metadata" %in% dbListTables(con)) {
  metadata_existing <- dbReadTable(con, "metadata") %>%
    filter(!key %in% metadata_update$key)
  
  dbWriteTable(
    con,
    "metadata",
    bind_rows(metadata_existing, metadata_update),
    overwrite = TRUE
  )
}

# ------------------------------------------------------------
# Checks and supporting CSVs
# ------------------------------------------------------------

row_counts <- data.frame(
  table_name = new_tables,
  n_rows = as.numeric(
    sapply(
      new_tables,
      function(x) dbGetQuery(con, paste0("SELECT COUNT(*) AS n FROM ", x))$n
    )
  ),
  stringsAsFactors = FALSE
)

map_status_counts <- station_map_status %>%
  count(map_status, map_status_label, sort = TRUE)

cross_section_availability_counts <- station_assessment_summary %>%
  count(has_cross_sections_processed, cross_section_record_class, sort = TRUE)

write.csv(
  row_counts,
  file.path(output_dir, "062_station_assessment_row_counts.csv"),
  row.names = FALSE
)

write.csv(
  map_status_counts,
  file.path(output_dir, "062_station_map_status_counts.csv"),
  row.names = FALSE
)

write.csv(
  cross_section_availability_counts,
  file.path(output_dir, "062_cross_section_availability_counts.csv"),
  row.names = FALSE
)

dbExecute(con, "CHECKPOINT")

message("Finished writing station assessment tables.")
message("Output database: ", shiny_db)
message("Output folder: ", output_dir)
message("Row counts:")
print(row_counts)
message("Map status counts:")
print(map_status_counts)
message("Cross-section availability counts:")
print(cross_section_availability_counts)
message("Important: preliminary_information_score is a screening/completeness indicator, not a final hydrological quality score.")
