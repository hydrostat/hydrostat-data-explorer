# ============================================================
# pipeline/R/053_write_discharge_products_to_duckdb.R
#
# Write validated discharge products to the local DuckDB database.
#
# Inputs:
# - data/processed/discharge_measurements.parquet
# - data/processed/rating_curves.parquet
# - data/processed/rating_curve_summary.parquet
# - data/processed/discharge_measurements_qc_summary.csv
# - data/processed/rating_curves_qc_summary.csv
#
# Outputs:
# - data/ana_hidro.duckdb
#   tables:
#     discharge_measurements
#     rating_curves
#     rating_curve_summary
#   views:
#     v_discharge_measurements_with_station
#     v_rating_curves_with_station
#     v_rating_curve_summary_with_station
#     v_station_discharge_products_summary
#
# - data/processed/duckdb_discharge_products_summary.csv
# ============================================================

# Load packages
library(DBI)
library(duckdb)
library(arrow)
library(dplyr)
library(readr)

# Define paths
db_file <- file.path("data", "ana_hidro.duckdb")

discharge_file <- file.path("data", "processed", "discharge_measurements.parquet")
rating_curves_file <- file.path("data", "processed", "rating_curves.parquet")
rating_curve_summary_file <- file.path("data", "processed", "rating_curve_summary.parquet")

discharge_qc_file <- file.path("data", "processed", "discharge_measurements_qc_summary.csv")
rating_curves_qc_file <- file.path("data", "processed", "rating_curves_qc_summary.csv")

output_summary_file <- file.path("data", "processed", "duckdb_discharge_products_summary.csv")

processed_at_value <- Sys.time()

# Critical input checks
if (!file.exists(db_file)) {
  stop("Missing DuckDB database: ", db_file)
}

if (!file.exists(discharge_file)) {
  stop("Missing processed discharge measurements file: ", discharge_file)
}

if (!file.exists(rating_curves_file)) {
  stop("Missing processed rating-curve segments file: ", rating_curves_file)
}

if (!file.exists(rating_curve_summary_file)) {
  stop("Missing processed rating-curve summary file: ", rating_curve_summary_file)
}

if (!file.exists(discharge_qc_file)) {
  stop("Missing discharge measurements QC summary file: ", discharge_qc_file)
}

if (!file.exists(rating_curves_qc_file)) {
  stop("Missing rating curves QC summary file: ", rating_curves_qc_file)
}

# Read validated products
discharge_measurements <- arrow::read_parquet(discharge_file)
rating_curves <- arrow::read_parquet(rating_curves_file)
rating_curve_summary <- arrow::read_parquet(rating_curve_summary_file)

discharge_qc <- readr::read_csv(
  discharge_qc_file,
  col_types = readr::cols(.default = readr::col_character()),
  show_col_types = FALSE
)

rating_curves_qc <- readr::read_csv(
  rating_curves_qc_file,
  col_types = readr::cols(.default = readr::col_character()),
  show_col_types = FALSE
)

# Critical table checks
required_discharge_fields <- c(
  "station_code",
  "measurement_datetime",
  "consistency_level",
  "last_update",
  "stage_cm",
  "discharge_m3s",
  "wetted_area_m2",
  "width_m",
  "mean_depth_m",
  "mean_velocity_ms",
  "source_route",
  "raw_file",
  "downloaded_at",
  "processed_at"
)

required_rating_curve_fields <- c(
  "rating_curve_id",
  "rating_curve_segment_id",
  "station_code",
  "measurement_datetime",
  "valid_from",
  "valid_to",
  "consistency_level",
  "last_update",
  "segment_number_raw",
  "segment_number",
  "n_segments_reported",
  "curve_type",
  "equation_type",
  "stage_min_cm",
  "stage_max_cm",
  "table_stage_step_cm",
  "coefficient_a",
  "coefficient_h0",
  "coefficient_n",
  "source_route",
  "raw_file",
  "downloaded_at",
  "processed_at"
)

required_rating_curve_summary_fields <- c(
  "rating_curve_id",
  "station_code",
  "valid_from",
  "valid_to",
  "consistency_level",
  "n_segments",
  "n_distinct_segment_numbers",
  "n_segments_reported_max",
  "stage_min_cm",
  "stage_max_cm",
  "first_last_update",
  "last_last_update",
  "first_downloaded_at",
  "last_downloaded_at",
  "source_route",
  "processed_at"
)

missing_discharge_fields <- setdiff(required_discharge_fields, names(discharge_measurements))
missing_rating_curve_fields <- setdiff(required_rating_curve_fields, names(rating_curves))
missing_rating_curve_summary_fields <- setdiff(required_rating_curve_summary_fields, names(rating_curve_summary))

if (length(missing_discharge_fields) > 0) {
  stop("Missing fields in discharge_measurements: ", paste(missing_discharge_fields, collapse = ", "))
}

if (length(missing_rating_curve_fields) > 0) {
  stop("Missing fields in rating_curves: ", paste(missing_rating_curve_fields, collapse = ", "))
}

if (length(missing_rating_curve_summary_fields) > 0) {
  stop("Missing fields in rating_curve_summary: ", paste(missing_rating_curve_summary_fields, collapse = ", "))
}

if (nrow(discharge_measurements) == 0) {
  stop("discharge_measurements is empty.")
}

if (nrow(rating_curves) == 0) {
  stop("rating_curves is empty.")
}

if (nrow(rating_curve_summary) == 0) {
  stop("rating_curve_summary is empty.")
}

# Discharge measurements key check
discharge_duplicates <- discharge_measurements %>%
  count(station_code, measurement_datetime, consistency_level, name = "n") %>%
  filter(n > 1)

if (nrow(discharge_duplicates) > 0) {
  stop("Duplicated keys found in discharge_measurements.")
}

# Rating-curve segment key check
rating_curve_segment_duplicates <- rating_curves %>%
  count(rating_curve_segment_id, name = "n") %>%
  filter(n > 1)

if (nrow(rating_curve_segment_duplicates) > 0) {
  stop("Duplicated rating_curve_segment_id values found in rating_curves.")
}

# Rating-curve summary key check
rating_curve_summary_duplicates <- rating_curve_summary %>%
  count(rating_curve_id, name = "n") %>%
  filter(n > 1)

if (nrow(rating_curve_summary_duplicates) > 0) {
  stop("Duplicated rating_curve_id values found in rating_curve_summary.")
}

# Check that all curve segments have a summary row
missing_summary_ids <- setdiff(
  unique(rating_curves$rating_curve_id),
  unique(rating_curve_summary$rating_curve_id)
)

if (length(missing_summary_ids) > 0) {
  stop("Some rating_curve_id values in rating_curves are missing from rating_curve_summary.")
}

# Connect to DuckDB
con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_file, read_only = FALSE)

on.exit({
  DBI::dbDisconnect(con)
}, add = TRUE)

# Check existing station table
tables_available <- DBI::dbGetQuery(
  con,
  "
  SELECT table_name
  FROM information_schema.tables
  WHERE table_schema = 'main'
  "
)

if (!"stations" %in% tables_available$table_name) {
  stop("The DuckDB database does not contain the required table: stations")
}

stations <- DBI::dbGetQuery(
  con,
  "
  SELECT station_code
  FROM stations
  "
)

# Check station-code linkage
missing_discharge_station_codes <- setdiff(
  unique(discharge_measurements$station_code),
  unique(stations$station_code)
)

missing_rating_curve_station_codes <- setdiff(
  unique(rating_curves$station_code),
  unique(stations$station_code)
)

if (length(missing_discharge_station_codes) > 0) {
  stop(
    "Some station_code values in discharge_measurements are absent from stations. First values: ",
    paste(head(missing_discharge_station_codes, 10), collapse = ", ")
  )
}

if (length(missing_rating_curve_station_codes) > 0) {
  stop(
    "Some station_code values in rating_curves are absent from stations. First values: ",
    paste(head(missing_rating_curve_station_codes, 10), collapse = ", ")
  )
}

# Prepare metadata update
metadata_update <- tibble::tibble(
  key = c(
    "schema_stage",
    "schema_version",
    "discharge_measurements_source_file",
    "rating_curves_source_file",
    "rating_curve_summary_source_file",
    "discharge_measurements_qc_file",
    "rating_curves_qc_file",
    "discharge_products_written_at",
    "n_discharge_measurements",
    "n_discharge_measurement_stations",
    "n_rating_curve_segments",
    "n_rating_curves",
    "n_rating_curve_stations",
    "discharge_measurements_key",
    "rating_curve_key",
    "rating_curve_segment_key",
    "consistency_level_interpretation",
    "hydro_dictionary_reference",
    "stores_full_time_series"
  ),
  value = c(
    "06_data_cleaning_qc_053_write_discharge_products_to_duckdb",
    "0.3",
    discharge_file,
    rating_curves_file,
    rating_curve_summary_file,
    discharge_qc_file,
    rating_curves_qc_file,
    as.character(processed_at_value),
    as.character(nrow(discharge_measurements)),
    as.character(dplyr::n_distinct(discharge_measurements$station_code)),
    as.character(nrow(rating_curves)),
    as.character(nrow(rating_curve_summary)),
    as.character(dplyr::n_distinct(rating_curves$station_code)),
    "station_code + measurement_datetime + consistency_level",
    "rating_curve_id = station_code + valid_from + valid_to + consistency_level",
    "rating_curve_segment_id",
    "1 = raw/bruto; 2 = consisted/consistido",
    "Hidro 1.4 data dictionary; API field names may differ from the legacy Hidro database",
    "no"
  )
)

# Write tables and views
DBI::dbBegin(con)

tryCatch(
  {
    # Drop dependent views before replacing tables
    DBI::dbExecute(con, "DROP VIEW IF EXISTS v_station_discharge_products_summary")
    DBI::dbExecute(con, "DROP VIEW IF EXISTS v_rating_curve_summary_with_station")
    DBI::dbExecute(con, "DROP VIEW IF EXISTS v_rating_curves_with_station")
    DBI::dbExecute(con, "DROP VIEW IF EXISTS v_discharge_measurements_with_station")
    
    # Replace analytical tables
    DBI::dbWriteTable(
      con,
      "discharge_measurements",
      discharge_measurements,
      overwrite = TRUE
    )
    
    DBI::dbWriteTable(
      con,
      "rating_curves",
      rating_curves,
      overwrite = TRUE
    )
    
    DBI::dbWriteTable(
      con,
      "rating_curve_summary",
      rating_curve_summary,
      overwrite = TRUE
    )
    
    # Keep compact QC summaries in the database
    DBI::dbWriteTable(
      con,
      "discharge_measurements_qc_summary",
      discharge_qc,
      overwrite = TRUE
    )
    
    DBI::dbWriteTable(
      con,
      "rating_curves_qc_summary",
      rating_curves_qc,
      overwrite = TRUE
    )
    
    # Metadata table, if absent
    DBI::dbExecute(
      con,
      "
      CREATE TABLE IF NOT EXISTS metadata (
        key VARCHAR,
        value VARCHAR
      )
      "
    )
    
    DBI::dbWriteTable(
      con,
      "metadata_update",
      metadata_update,
      overwrite = TRUE
    )
    
    DBI::dbExecute(
      con,
      "
      DELETE FROM metadata
      WHERE key IN (
        SELECT key
        FROM metadata_update
      )
      "
    )
    
    DBI::dbExecute(
      con,
      "
      INSERT INTO metadata
      SELECT key, value
      FROM metadata_update
      "
    )
    
    DBI::dbExecute(con, "DROP TABLE metadata_update")
    
    # Indexes for common joins and filters
    DBI::dbExecute(
      con,
      "
      CREATE INDEX IF NOT EXISTS idx_discharge_measurements_station_datetime
      ON discharge_measurements(station_code, measurement_datetime)
      "
    )
    
    DBI::dbExecute(
      con,
      "
      CREATE INDEX IF NOT EXISTS idx_discharge_measurements_station_consistency
      ON discharge_measurements(station_code, consistency_level)
      "
    )
    
    DBI::dbExecute(
      con,
      "
      CREATE INDEX IF NOT EXISTS idx_rating_curves_segment_id
      ON rating_curves(rating_curve_segment_id)
      "
    )
    
    DBI::dbExecute(
      con,
      "
      CREATE INDEX IF NOT EXISTS idx_rating_curves_station_validity
      ON rating_curves(station_code, valid_from, valid_to)
      "
    )
    
    DBI::dbExecute(
      con,
      "
      CREATE INDEX IF NOT EXISTS idx_rating_curve_summary_id
      ON rating_curve_summary(rating_curve_id)
      "
    )
    
    DBI::dbExecute(
      con,
      "
      CREATE INDEX IF NOT EXISTS idx_rating_curve_summary_station_validity
      ON rating_curve_summary(station_code, valid_from, valid_to)
      "
    )
    
    # Views with station metadata
    DBI::dbExecute(
      con,
      "
      CREATE VIEW v_discharge_measurements_with_station AS
      SELECT
        dm.*,
        s.station_name,
        s.uf,
        s.uf_name,
        s.municipality,
        s.basin_code,
        s.basin_name,
        s.sub_basin_code,
        s.latitude,
        s.longitude,
        s.drainage_area,
        s.operator,
        s.responsible_agency
      FROM discharge_measurements dm
      LEFT JOIN stations s
        ON dm.station_code = s.station_code
      "
    )
    
    DBI::dbExecute(
      con,
      "
      CREATE VIEW v_rating_curves_with_station AS
      SELECT
        rc.*,
        s.station_name,
        s.uf,
        s.uf_name,
        s.municipality,
        s.basin_code,
        s.basin_name,
        s.sub_basin_code,
        s.latitude,
        s.longitude,
        s.drainage_area,
        s.operator,
        s.responsible_agency
      FROM rating_curves rc
      LEFT JOIN stations s
        ON rc.station_code = s.station_code
      "
    )
    
    DBI::dbExecute(
      con,
      "
      CREATE VIEW v_rating_curve_summary_with_station AS
      SELECT
        rcs.*,
        s.station_name,
        s.uf,
        s.uf_name,
        s.municipality,
        s.basin_code,
        s.basin_name,
        s.sub_basin_code,
        s.latitude,
        s.longitude,
        s.drainage_area,
        s.operator,
        s.responsible_agency
      FROM rating_curve_summary rcs
      LEFT JOIN stations s
        ON rcs.station_code = s.station_code
      "
    )
    
    DBI::dbExecute(
      con,
      "
      CREATE VIEW v_station_discharge_products_summary AS
      WITH discharge_by_station AS (
        SELECT
          station_code,
          COUNT(*) AS n_discharge_measurements,
          MIN(measurement_datetime) AS first_measurement_datetime,
          MAX(measurement_datetime) AS last_measurement_datetime,
          MIN(discharge_m3s) AS min_discharge_m3s,
          MAX(discharge_m3s) AS max_discharge_m3s,
          AVG(discharge_m3s) AS mean_discharge_m3s,
          MIN(stage_cm) AS min_stage_cm,
          MAX(stage_cm) AS max_stage_cm
        FROM discharge_measurements
        GROUP BY station_code
      ),
      rating_curves_by_station AS (
        SELECT
          station_code,
          COUNT(*) AS n_rating_curves,
          MIN(valid_from) AS first_rating_curve_valid_from,
          MAX(valid_from) AS last_rating_curve_valid_from,
          MIN(valid_to) AS first_rating_curve_valid_to,
          MAX(valid_to) AS last_rating_curve_valid_to
        FROM rating_curve_summary
        GROUP BY station_code
      ),
      rating_curve_segments_by_station AS (
        SELECT
          station_code,
          COUNT(*) AS n_rating_curve_segments
        FROM rating_curves
        GROUP BY station_code
      )
      SELECT
        s.station_code,
        s.station_name,
        s.uf,
        s.uf_name,
        s.municipality,
        s.basin_code,
        s.basin_name,
        s.sub_basin_code,
        s.latitude,
        s.longitude,
        s.drainage_area,
        s.operator,
        s.responsible_agency,
        COALESCE(d.n_discharge_measurements, 0) AS n_discharge_measurements,
        d.first_measurement_datetime,
        d.last_measurement_datetime,
        d.min_discharge_m3s,
        d.max_discharge_m3s,
        d.mean_discharge_m3s,
        d.min_stage_cm,
        d.max_stage_cm,
        COALESCE(rc.n_rating_curves, 0) AS n_rating_curves,
        COALESCE(rcs.n_rating_curve_segments, 0) AS n_rating_curve_segments,
        rc.first_rating_curve_valid_from,
        rc.last_rating_curve_valid_from,
        rc.first_rating_curve_valid_to,
        rc.last_rating_curve_valid_to,
        CASE
          WHEN COALESCE(d.n_discharge_measurements, 0) > 0 THEN TRUE
          ELSE FALSE
        END AS has_discharge_measurements_processed,
        CASE
          WHEN COALESCE(rc.n_rating_curves, 0) > 0 THEN TRUE
          ELSE FALSE
        END AS has_rating_curves_processed
      FROM stations s
      LEFT JOIN discharge_by_station d
        ON s.station_code = d.station_code
      LEFT JOIN rating_curves_by_station rc
        ON s.station_code = rc.station_code
      LEFT JOIN rating_curve_segments_by_station rcs
        ON s.station_code = rcs.station_code
      "
    )
    
    DBI::dbCommit(con)
  },
  error = function(e) {
    DBI::dbRollback(con)
    stop(e)
  }
)

# Final database checks
table_counts <- DBI::dbGetQuery(
  con,
  "
  SELECT 'stations' AS table_name, COUNT(*) AS n_rows FROM stations
  UNION ALL
  SELECT 'discharge_measurements' AS table_name, COUNT(*) AS n_rows FROM discharge_measurements
  UNION ALL
  SELECT 'rating_curves' AS table_name, COUNT(*) AS n_rows FROM rating_curves
  UNION ALL
  SELECT 'rating_curve_summary' AS table_name, COUNT(*) AS n_rows FROM rating_curve_summary
  UNION ALL
  SELECT 'discharge_measurements_qc_summary' AS table_name, COUNT(*) AS n_rows FROM discharge_measurements_qc_summary
  UNION ALL
  SELECT 'rating_curves_qc_summary' AS table_name, COUNT(*) AS n_rows FROM rating_curves_qc_summary
  "
)

view_counts <- DBI::dbGetQuery(
  con,
  "
  SELECT 'v_station_discharge_products_summary' AS view_name, COUNT(*) AS n_rows
  FROM v_station_discharge_products_summary
  UNION ALL
  SELECT 'v_discharge_measurements_with_station' AS view_name, COUNT(*) AS n_rows
  FROM v_discharge_measurements_with_station
  UNION ALL
  SELECT 'v_rating_curves_with_station' AS view_name, COUNT(*) AS n_rows
  FROM v_rating_curves_with_station
  UNION ALL
  SELECT 'v_rating_curve_summary_with_station' AS view_name, COUNT(*) AS n_rows
  FROM v_rating_curve_summary_with_station
  "
)

key_checks <- tibble::tibble(
  check_name = c(
    "duplicate_discharge_measurement_keys",
    "duplicate_rating_curve_segment_ids",
    "duplicate_rating_curve_summary_ids",
    "missing_discharge_station_codes",
    "missing_rating_curve_station_codes"
  ),
  value = c(
    nrow(discharge_duplicates),
    nrow(rating_curve_segment_duplicates),
    nrow(rating_curve_summary_duplicates),
    length(missing_discharge_station_codes),
    length(missing_rating_curve_station_codes)
  )
)

output_summary <- bind_rows(
  table_counts %>%
    mutate(object_type = "table") %>%
    rename(object_name = table_name),
  view_counts %>%
    mutate(object_type = "view") %>%
    rename(object_name = view_name)
) %>%
  select(object_type, object_name, n_rows)

readr::write_csv(output_summary, output_summary_file)

# Console summary
message("Finished writing discharge products to DuckDB.")
message("Database: ", db_file)
message("Discharge measurements: ", nrow(discharge_measurements))
message("Rating-curve segments: ", nrow(rating_curves))
message("Rating curves: ", nrow(rating_curve_summary))
message("Discharge measurement stations: ", dplyr::n_distinct(discharge_measurements$station_code))
message("Rating-curve stations: ", dplyr::n_distinct(rating_curves$station_code))
message("Summary file: ", output_summary_file)

print(table_counts)
print(view_counts)
print(key_checks)
