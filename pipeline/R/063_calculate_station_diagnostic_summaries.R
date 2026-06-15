# ============================================================
# pipeline/R/063_calculate_station_diagnostic_summaries.R
#
# Purpose:
# Calculate lightweight station diagnostic summaries for Shiny
# filtering, tables, and map support.
#
# Input:
#   exports/shiny_minimal.duckdb
#
# Required previous step:
#   pipeline/R/062_calculate_station_quality_indices.R
#
# Outputs written to the same local DuckDB database:
#   station_diagnostic_summary
#   station_diagnostic_indices
#
# This script does not call ANA APIs, does not use credentials,
# and does not source acquisition/download scripts.
#
# Detailed point-level diagnostics are still calculated on demand
# by R/station_diagnostic_functions.R for the selected station.
# ============================================================

# Load packages
library(DBI)
library(duckdb)
library(dplyr)

# Load shared pipeline helpers
source(file.path("pipeline", "helpers", "duckdb_helpers.R"), local = TRUE)

source(file.path("R", "station_diagnostic_functions.R"), local = TRUE)

# ------------------------------------------------------------
# Paths and parameters
# ------------------------------------------------------------

shiny_db <- file.path("exports", "shiny_minimal.duckdb")
output_dir <- file.path("outputs", "station_assessment")

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

params <- station_diagnostic_default_params()

if (!file.exists(shiny_db)) {
  stop("Missing local Shiny export database: ", shiny_db)
}

message("============================================================")
message("063_calculate_station_diagnostic_summaries")
message("============================================================")
message("Input database: ", shiny_db)
message("This script uses local DuckDB tables only.")

# ------------------------------------------------------------
# Helper functions
# ------------------------------------------------------------

count_rows <- function(con, table_name) {
  dbGetQuery(
    con,
    paste0("SELECT COUNT(*) AS n_rows FROM ", quote_ident(table_name))
  )$n_rows[1]
}

default_diagnostic_summary <- function(station_code, diagnostic_class = "not_available", diagnostic_score = 0) {
  data.frame(
    station_code = station_code,
    n_measurements = 0,
    n_valid_measurements = 0,
    n_stage_zero_or_negative = 0,
    pct_stage_zero_or_negative = NA_real_,
    n_discharge_zero_or_negative = 0,
    pct_discharge_zero_or_negative = NA_real_,
    n_repeated_stage_variable_discharge_points = 0,
    pct_repeated_stage_variable_discharge_points = NA_real_,
    n_repeated_discharge_variable_stage_points = 0,
    pct_repeated_discharge_variable_stage_points = NA_real_,
    n_rating_curves = 0,
    n_rating_curve_segments = 0,
    rating_match_fraction = NA_real_,
    median_abs_rating_log_residual = NA_real_,
    outside_residual_envelope_fraction = NA_real_,
    n_temporal_regimes = NA_integer_,
    temporal_regime_evidence_class = NA_character_,
    baseline_power_equation = NA_character_,
    baseline_power_h0_m = NA_real_,
    baseline_power_a = NA_real_,
    baseline_power_b = NA_real_,
    diagnostic_attention_score = diagnostic_score,
    diagnostic_attention_class = diagnostic_class,
    diagnostic_detail_level = "light_station_summary",
    stringsAsFactors = FALSE
  )
}

safe_make_indices <- function(summary_row) {
  tryCatch(
    make_diagnostic_indices(summary_row),
    error = function(e) {
      data.frame(
        station_code = summary_row$station_code,
        index_group = "Diagnostic calculation",
        index_name = "Diagnostic index calculation",
        index_value_numeric = NA_real_,
        index_value_text = paste("Failed:", conditionMessage(e)),
        index_unit = NA_character_,
        index_class = "calculation_failed",
        index_direction = "screening",
        index_description = "The diagnostic index table could not be created for this station.",
        data_requirement = "station_diagnostic_functions.R",
        can_be_calculated_now = FALSE,
        display_order = 999L,
        stringsAsFactors = FALSE
      )
    }
  )
}

# ------------------------------------------------------------
# Connect and check local database
# ------------------------------------------------------------

con <- dbConnect(duckdb::duckdb(), shiny_db, read_only = FALSE)
on.exit(dbDisconnect(con), add = TRUE)

tables <- dbListTables(con)

required_tables <- c(
  "stations_minimal",
  "station_discharge_products_summary",
  "discharge_measurements",
  "rating_curves",
  "rating_curve_summary",
  "station_assessment_summary",
  "station_data_availability",
  "station_cross_section_indices",
  "station_map_status"
)

missing_tables <- setdiff(required_tables, tables)

if (length(missing_tables) > 0) {
  stop(
    "Missing required table(s) in local Shiny export: ",
    paste(missing_tables, collapse = ", "),
    ". Run pipeline/R/062_calculate_station_quality_indices.R before this script."
  )
}

# ------------------------------------------------------------
# Read local tables
# ------------------------------------------------------------

stations <- dbReadTable(con, "stations_minimal") %>%
  mutate(station_code = as.character(station_code)) %>%
  select(station_code)

station_data_availability <- dbReadTable(con, "station_data_availability") %>%
  mutate(station_code = as.character(station_code))

station_cross_section_indices <- dbReadTable(con, "station_cross_section_indices") %>%
  mutate(station_code = as.character(station_code))

station_assessment_summary <- dbReadTable(con, "station_assessment_summary") %>%
  mutate(station_code = as.character(station_code))

# These point/segment tables are moderate in the current compact export.
# They are read once and split by station to avoid repeated filtering.
discharge_measurements <- dbReadTable(con, "discharge_measurements") %>%
  mutate(station_code = as.character(station_code))

rating_curves <- dbReadTable(con, "rating_curves") %>%
  mutate(station_code = as.character(station_code))

rating_curve_summary <- dbReadTable(con, "rating_curve_summary") %>%
  mutate(station_code = as.character(station_code))

# ------------------------------------------------------------
# Prepare station lists and split tables by station
# ------------------------------------------------------------

station_codes <- stations$station_code

measurement_codes <- unique(discharge_measurements$station_code)
rating_curve_codes <- unique(rating_curves$station_code)

diagnostic_station_codes <- intersect(
  station_codes,
  union(measurement_codes, rating_curve_codes)
)

message("Total stations in export: ", length(station_codes))
message("Stations with measurements or rating curves for diagnostic summaries: ", length(diagnostic_station_codes))

measurement_split <- split(discharge_measurements, discharge_measurements$station_code)
rating_curve_split <- split(rating_curves, rating_curves$station_code)
rating_curve_summary_split <- split(rating_curve_summary, rating_curve_summary$station_code)

summary_list <- vector("list", length(station_codes))
indices_list <- vector("list", length(station_codes))

failed_stations <- data.frame(
  station_code = character(),
  error_message = character(),
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------
# Calculate lightweight summaries
# ------------------------------------------------------------

for (i in seq_along(station_codes)) {
  code <- station_codes[i]
  
  if (i == 1 || i %% 1000 == 0 || i == length(station_codes)) {
    message("Diagnostic summaries: ", i, " / ", length(station_codes), " | station ", code)
  }
  
  m <- measurement_split[[code]]
  rc <- rating_curve_split[[code]]
  rcs <- rating_curve_summary_split[[code]]
  
  if (is.null(m)) {
    m <- discharge_measurements[0, , drop = FALSE]
  }
  
  if (is.null(rc)) {
    rc <- rating_curves[0, , drop = FALSE]
  }
  
  if (is.null(rcs)) {
    rcs <- rating_curve_summary[0, , drop = FALSE]
  }
  
  if (nrow(m) == 0 && nrow(rc) == 0) {
    summary_row <- default_diagnostic_summary(code)
    summary_list[[i]] <- summary_row
    indices_list[[i]] <- safe_make_indices(summary_row)
  } else {
    diag <- tryCatch(
      calculate_station_diagnostics(
        measurements = m,
        rating_curves = rc,
        rating_curve_summary = rcs,
        params = params,
        detailed = FALSE
      ),
      error = function(e) {
        failed_stations <<- rbind(
          failed_stations,
          data.frame(
            station_code = code,
            error_message = conditionMessage(e),
            stringsAsFactors = FALSE
          )
        )
        
        summary_row <- default_diagnostic_summary(
          station_code = code,
          diagnostic_class = "calculation_failed",
          diagnostic_score = NA_real_
        )
        
        list(
          summary = summary_row,
          indices = safe_make_indices(summary_row)
        )
      }
    )
    
    summary_list[[i]] <- diag$summary
    indices_list[[i]] <- diag$indices
  }
}

station_diagnostic_summary_base <- bind_rows(summary_list) %>%
  mutate(station_code = as.character(station_code))

station_diagnostic_indices <- bind_rows(indices_list) %>%
  mutate(station_code = as.character(station_code))

# ------------------------------------------------------------
# Add station-level context from 062 outputs
# ------------------------------------------------------------

availability_context <- station_data_availability %>%
  select(
    station_code,
    any_of(c(
      "has_discharge_measurements_processed",
      "has_rating_curves_processed",
      "has_cross_sections_processed",
      "has_cross_section_vertices_processed",
      "n_cross_sections",
      "n_cross_section_vertices",
      "first_cross_section_datetime",
      "last_cross_section_datetime",
      "cross_section_period_years",
      "cross_section_distance_span_m",
      "cross_section_stage_range_cm",
      "cross_section_record_class",
      "cross_section_vertex_class",
      "cross_section_temporal_class",
      "cross_section_geometry_class"
    ))
  )

assessment_context <- station_assessment_summary %>%
  select(
    station_code,
    any_of(c(
      "preliminary_information_score",
      "preliminary_information_class",
      "station_assessment_status",
      "station_assessment_status_label"
    ))
  )

station_diagnostic_summary <- stations %>%
  left_join(station_diagnostic_summary_base, by = "station_code") %>%
  left_join(availability_context, by = "station_code") %>%
  left_join(assessment_context, by = "station_code")

# Fill any unexpected missing diagnostic rows with default values.
missing_diagnostic_rows <- which(is.na(station_diagnostic_summary$diagnostic_attention_class))

if (length(missing_diagnostic_rows) > 0) {
  for (row_id in missing_diagnostic_rows) {
    code <- station_diagnostic_summary$station_code[row_id]
    default_row <- default_diagnostic_summary(code)
    
    for (col in names(default_row)) {
      if (col %in% names(station_diagnostic_summary)) {
        station_diagnostic_summary[[col]][row_id] <- default_row[[col]]
      }
    }
  }
}

# ------------------------------------------------------------
# Critical checks
# ------------------------------------------------------------

duplicate_summary_codes <- station_diagnostic_summary %>%
  count(station_code, name = "n") %>%
  filter(n > 1)

if (nrow(duplicate_summary_codes) > 0) {
  stop("station_diagnostic_summary has duplicated station_code values.")
}

missing_summary_codes <- setdiff(station_codes, station_diagnostic_summary$station_code)

if (length(missing_summary_codes) > 0) {
  stop("station_diagnostic_summary is missing station_code values.")
}

if (nrow(station_diagnostic_summary) != length(station_codes)) {
  stop("station_diagnostic_summary row count does not match stations_minimal.")
}

if (nrow(station_diagnostic_indices) == 0) {
  stop("station_diagnostic_indices is empty.")
}

# ------------------------------------------------------------
# Write lightweight outputs
# ------------------------------------------------------------

dbWriteTable(con, "station_diagnostic_summary", station_diagnostic_summary, overwrite = TRUE)
dbWriteTable(con, "station_diagnostic_indices", station_diagnostic_indices, overwrite = TRUE)

# ------------------------------------------------------------
# Update metadata
# ------------------------------------------------------------

metadata_update <- data.frame(
  key = c(
    "stage_09c_station_diagnostic_processed_at",
    "stage_09c_station_diagnostic_script",
    "stage_09c_station_diagnostic_source",
    "stage_09c_station_diagnostic_note",
    "stage_09c_station_diagnostic_cross_section_context"
  ),
  value = c(
    as.character(Sys.time()),
    "pipeline/R/063_calculate_station_diagnostic_summaries.R",
    "exports/shiny_minimal.duckdb",
    "Lightweight station diagnostic summaries were calculated for Shiny filtering and display. Detailed point-level diagnostics remain on-demand.",
    "Cross-section availability and geometry context from R/062 outputs are preserved in station_diagnostic_summary, but cross-section hydraulic diagnostics are not calculated in this script."
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
  table_name = c("station_diagnostic_summary", "station_diagnostic_indices"),
  n_rows = c(
    count_rows(con, "station_diagnostic_summary"),
    count_rows(con, "station_diagnostic_indices")
  ),
  stringsAsFactors = FALSE
)

class_counts <- station_diagnostic_summary %>%
  count(diagnostic_attention_class, name = "n") %>%
  arrange(desc(n))

cross_section_context_counts <- station_diagnostic_summary %>%
  count(has_cross_sections_processed, cross_section_record_class, name = "n") %>%
  arrange(desc(n))

diagnostic_failure_counts <- data.frame(
  n_failed_stations = nrow(failed_stations),
  stringsAsFactors = FALSE
)

write.csv(
  row_counts,
  file.path(output_dir, "063_station_diagnostic_row_counts.csv"),
  row.names = FALSE
)

write.csv(
  class_counts,
  file.path(output_dir, "063_station_diagnostic_attention_class_counts.csv"),
  row.names = FALSE
)

write.csv(
  cross_section_context_counts,
  file.path(output_dir, "063_cross_section_context_counts.csv"),
  row.names = FALSE
)

write.csv(
  failed_stations,
  file.path(output_dir, "063_station_diagnostic_failed_stations.csv"),
  row.names = FALSE
)

dbExecute(con, "CHECKPOINT")

# ------------------------------------------------------------
# Console summary
# ------------------------------------------------------------

message("Finished writing lightweight station diagnostic summaries.")
message("Output database: ", shiny_db)
message("Output folder: ", output_dir)

message("Row counts:")
print(row_counts)

message("Diagnostic attention class counts:")
print(class_counts)

message("Cross-section context counts:")
print(cross_section_context_counts)

message("Diagnostic calculation failures:")
print(diagnostic_failure_counts)

if (nrow(failed_stations) > 0) {
  message("Some station diagnostic calculations failed. See:")
  message(file.path(output_dir, "063_station_diagnostic_failed_stations.csv"))
}

message("Detailed point-level diagnostics are calculated on demand for the selected station.")
