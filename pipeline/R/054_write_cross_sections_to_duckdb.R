# ============================================================
# pipeline/R/054_write_cross_sections_to_duckdb.R
#
# Write validated cross-section products to the local DuckDB database.
#
# Inputs:
# - data/processed/cross_sections.parquet
# - data/processed/cross_section_vertices.parquet
# - data/processed/cross_section_summary.parquet
# - data/processed/cross_sections_qc_summary.csv
#
# Outputs:
# - data/ana_hidro.duckdb
#   tables:
#     cross_sections
#     cross_section_vertices
#     cross_section_summary
#     cross_sections_qc_summary
#   views:
#     v_cross_sections_with_station
#     v_cross_section_vertices_with_station
#     v_cross_section_summary_with_station
#     v_station_discharge_products_summary
#
# - data/processed/duckdb_cross_section_products_summary.csv
#
# Notes:
# - This script assumes pipeline/R/053_write_discharge_products_to_duckdb.R
#   has already been executed.
# - It keeps R/053 stable and only adds the cross-section products.
# ============================================================

# Load packages
library(DBI)
library(duckdb)
library(arrow)
library(dplyr)
library(readr)

# Define paths
db_file <- file.path("data", "ana_hidro.duckdb")

cross_sections_file <- file.path("data", "processed", "cross_sections.parquet")
cross_section_vertices_file <- file.path("data", "processed", "cross_section_vertices.parquet")
cross_section_summary_file <- file.path("data", "processed", "cross_section_summary.parquet")
cross_sections_qc_file <- file.path("data", "processed", "cross_sections_qc_summary.csv")

output_summary_file <- file.path("data", "processed", "duckdb_cross_section_products_summary.csv")

processed_at_value <- Sys.time()

# Critical input checks
if (!file.exists(db_file)) {
  stop("Missing DuckDB database: ", db_file)
}

if (!file.exists(cross_sections_file)) {
  stop("Missing processed cross sections file: ", cross_sections_file)
}

if (!file.exists(cross_section_vertices_file)) {
  stop("Missing processed cross-section vertices file: ", cross_section_vertices_file)
}

if (!file.exists(cross_section_summary_file)) {
  stop("Missing processed cross-section summary file: ", cross_section_summary_file)
}

if (!file.exists(cross_sections_qc_file)) {
  stop("Missing cross sections QC summary file: ", cross_sections_qc_file)
}

# Read validated products
cross_sections <- arrow::read_parquet(cross_sections_file)
cross_section_vertices <- arrow::read_parquet(cross_section_vertices_file)
cross_section_summary <- arrow::read_parquet(cross_section_summary_file)

cross_sections_qc <- readr::read_csv(
  cross_sections_qc_file,
  col_types = readr::cols(.default = readr::col_character()),
  show_col_types = FALSE
)

# Critical field checks
required_cross_sections_fields <- c(
  "cross_section_id",
  "station_code",
  "measurement_datetime",
  "consistency_level",
  "survey_number",
  "section_type",
  "source_record_id",
  "last_update",
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
  "n_missing_vertex_stage",
  "observation",
  "source_route",
  "first_raw_file",
  "first_downloaded_at",
  "last_downloaded_at",
  "processed_at"
)

required_cross_section_vertices_fields <- c(
  "cross_section_id",
  "cross_section_vertex_id",
  "station_code",
  "measurement_datetime",
  "consistency_level",
  "last_update",
  "survey_number",
  "section_type",
  "source_record_id",
  "vertex_order",
  "vertex_distance_m",
  "vertex_stage_cm",
  "n_vertices_reported",
  "distance_pipf_m",
  "x_distance_max_m",
  "x_distance_min_m",
  "y_stage_max_cm",
  "y_stage_min_cm",
  "geometry_stage_step_cm",
  "observation",
  "raw_verticais",
  "source_item_index",
  "source_route",
  "raw_file",
  "downloaded_at",
  "processed_at"
)

required_cross_section_summary_fields <- c(
  "station_code",
  "n_cross_sections",
  "n_cross_section_vertices",
  "first_cross_section_datetime",
  "last_cross_section_datetime",
  "n_consistency_level_1",
  "n_consistency_level_2",
  "n_section_type_1",
  "n_section_type_2",
  "min_vertex_distance_m",
  "max_vertex_distance_m",
  "min_vertex_stage_cm",
  "max_vertex_stage_cm",
  "first_downloaded_at",
  "last_downloaded_at",
  "processed_at"
)

missing_cross_sections_fields <- setdiff(required_cross_sections_fields, names(cross_sections))
missing_cross_section_vertices_fields <- setdiff(required_cross_section_vertices_fields, names(cross_section_vertices))
missing_cross_section_summary_fields <- setdiff(required_cross_section_summary_fields, names(cross_section_summary))

if (length(missing_cross_sections_fields) > 0) {
  stop("Missing fields in cross_sections: ", paste(missing_cross_sections_fields, collapse = ", "))
}

if (length(missing_cross_section_vertices_fields) > 0) {
  stop("Missing fields in cross_section_vertices: ", paste(missing_cross_section_vertices_fields, collapse = ", "))
}

if (length(missing_cross_section_summary_fields) > 0) {
  stop("Missing fields in cross_section_summary: ", paste(missing_cross_section_summary_fields, collapse = ", "))
}

if (nrow(cross_sections) == 0) {
  stop("cross_sections is empty.")
}

if (nrow(cross_section_vertices) == 0) {
  stop("cross_section_vertices is empty.")
}

if (nrow(cross_section_summary) == 0) {
  stop("cross_section_summary is empty.")
}

# Key checks before writing
cross_section_duplicates <- cross_sections %>%
  count(cross_section_id, name = "n") %>%
  filter(n > 1)

if (nrow(cross_section_duplicates) > 0) {
  stop("Duplicated cross_section_id values found in cross_sections.")
}

cross_section_vertex_duplicates <- cross_section_vertices %>%
  count(cross_section_vertex_id, name = "n") %>%
  filter(n > 1)

if (nrow(cross_section_vertex_duplicates) > 0) {
  stop("Duplicated cross_section_vertex_id values found in cross_section_vertices.")
}

cross_section_summary_duplicates <- cross_section_summary %>%
  count(station_code, name = "n") %>%
  filter(n > 1)

if (nrow(cross_section_summary_duplicates) > 0) {
  stop("Duplicated station_code values found in cross_section_summary.")
}

# Check that all vertices have a profile row
missing_cross_section_ids_in_sections <- setdiff(
  unique(cross_section_vertices$cross_section_id),
  unique(cross_sections$cross_section_id)
)

if (length(missing_cross_section_ids_in_sections) > 0) {
  stop("Some cross_section_id values in cross_section_vertices are missing from cross_sections.")
}

# Check that all profile stations have a station summary row
missing_cross_section_summary_station_codes <- setdiff(
  unique(cross_sections$station_code),
  unique(cross_section_summary$station_code)
)

if (length(missing_cross_section_summary_station_codes) > 0) {
  stop("Some station_code values in cross_sections are missing from cross_section_summary.")
}

# Connect to DuckDB
con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_file, read_only = FALSE)

on.exit({
  DBI::dbDisconnect(con)
}, add = TRUE)

# Check required existing tables from previous stages
tables_available <- DBI::dbGetQuery(
  con,
  "
  SELECT table_name
  FROM information_schema.tables
  WHERE table_schema = 'main'
  "
)

required_existing_tables <- c(
  "stations",
  "discharge_measurements",
  "rating_curves",
  "rating_curve_summary"
)

missing_existing_tables <- setdiff(required_existing_tables, tables_available$table_name)

if (length(missing_existing_tables) > 0) {
  stop(
    "The DuckDB database is missing required tables from previous stages: ",
    paste(missing_existing_tables, collapse = ", ")
  )
}

stations <- DBI::dbGetQuery(
  con,
  "
  SELECT station_code
  FROM stations
  "
)

# Check station-code linkage
missing_cross_section_station_codes <- setdiff(
  unique(cross_sections$station_code),
  unique(stations$station_code)
)

missing_cross_section_vertex_station_codes <- setdiff(
  unique(cross_section_vertices$station_code),
  unique(stations$station_code)
)

missing_cross_section_summary_station_codes_from_stations <- setdiff(
  unique(cross_section_summary$station_code),
  unique(stations$station_code)
)

if (length(missing_cross_section_station_codes) > 0) {
  stop(
    "Some station_code values in cross_sections are absent from stations. First values: ",
    paste(head(missing_cross_section_station_codes, 10), collapse = ", ")
  )
}

if (length(missing_cross_section_vertex_station_codes) > 0) {
  stop(
    "Some station_code values in cross_section_vertices are absent from stations. First values: ",
    paste(head(missing_cross_section_vertex_station_codes, 10), collapse = ", ")
  )
}

if (length(missing_cross_section_summary_station_codes_from_stations) > 0) {
  stop(
    "Some station_code values in cross_section_summary are absent from stations. First values: ",
    paste(head(missing_cross_section_summary_station_codes_from_stations, 10), collapse = ", ")
  )
}

# Prepare metadata update
metadata_update <- tibble::tibble(
  key = c(
    "schema_stage",
    "schema_version",
    "cross_sections_source_file",
    "cross_section_vertices_source_file",
    "cross_section_summary_source_file",
    "cross_sections_qc_file",
    "cross_section_products_written_at",
    "n_cross_sections",
    "n_cross_section_vertices",
    "n_cross_section_stations",
    "cross_section_key",
    "cross_section_vertex_key",
    "cross_section_summary_key",
    "cross_section_interpretation",
    "stores_full_time_series"
  ),
  value = c(
    "07_duckdb_database_054_write_cross_sections_to_duckdb",
    "0.4",
    cross_sections_file,
    cross_section_vertices_file,
    cross_section_summary_file,
    cross_sections_qc_file,
    as.character(processed_at_value),
    as.character(nrow(cross_sections)),
    as.character(nrow(cross_section_vertices)),
    as.character(dplyr::n_distinct(cross_sections$station_code)),
    "cross_section_id",
    "cross_section_vertex_id",
    "station_code",
    "one row in cross_sections = one survey/profile; one row in cross_section_vertices = one vertical/profile point",
    "no"
  )
)

# Write tables and views
DBI::dbBegin(con)

tryCatch(
  {
    # Drop dependent views before replacing cross-section tables
    DBI::dbExecute(con, "DROP VIEW IF EXISTS v_station_discharge_products_summary")
    DBI::dbExecute(con, "DROP VIEW IF EXISTS v_cross_section_summary_with_station")
    DBI::dbExecute(con, "DROP VIEW IF EXISTS v_cross_section_vertices_with_station")
    DBI::dbExecute(con, "DROP VIEW IF EXISTS v_cross_sections_with_station")

    # Replace cross-section analytical tables
    DBI::dbWriteTable(
      con,
      "cross_sections",
      cross_sections,
      overwrite = TRUE
    )

    DBI::dbWriteTable(
      con,
      "cross_section_vertices",
      cross_section_vertices,
      overwrite = TRUE
    )

    DBI::dbWriteTable(
      con,
      "cross_section_summary",
      cross_section_summary,
      overwrite = TRUE
    )

    DBI::dbWriteTable(
      con,
      "cross_sections_qc_summary",
      cross_sections_qc,
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
      CREATE INDEX IF NOT EXISTS idx_cross_sections_id
      ON cross_sections(cross_section_id)
      "
    )

    DBI::dbExecute(
      con,
      "
      CREATE INDEX IF NOT EXISTS idx_cross_sections_station_datetime
      ON cross_sections(station_code, measurement_datetime)
      "
    )

    DBI::dbExecute(
      con,
      "
      CREATE INDEX IF NOT EXISTS idx_cross_section_vertices_id
      ON cross_section_vertices(cross_section_vertex_id)
      "
    )

    DBI::dbExecute(
      con,
      "
      CREATE INDEX IF NOT EXISTS idx_cross_section_vertices_section_id
      ON cross_section_vertices(cross_section_id)
      "
    )

    DBI::dbExecute(
      con,
      "
      CREATE INDEX IF NOT EXISTS idx_cross_section_vertices_station_datetime
      ON cross_section_vertices(station_code, measurement_datetime)
      "
    )

    DBI::dbExecute(
      con,
      "
      CREATE INDEX IF NOT EXISTS idx_cross_section_summary_station
      ON cross_section_summary(station_code)
      "
    )

    # Views with station metadata
    DBI::dbExecute(
      con,
      "
      CREATE VIEW v_cross_sections_with_station AS
      SELECT
        cs.*,
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
      FROM cross_sections cs
      LEFT JOIN stations s
        ON cs.station_code = s.station_code
      "
    )

    DBI::dbExecute(
      con,
      "
      CREATE VIEW v_cross_section_vertices_with_station AS
      SELECT
        csv.*,
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
      FROM cross_section_vertices csv
      LEFT JOIN stations s
        ON csv.station_code = s.station_code
      "
    )

    DBI::dbExecute(
      con,
      "
      CREATE VIEW v_cross_section_summary_with_station AS
      SELECT
        css.*,
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
      FROM cross_section_summary css
      LEFT JOIN stations s
        ON css.station_code = s.station_code
      "
    )

    # Recreate the station-level product summary with cross sections included.
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
      ),
      cross_sections_by_station AS (
        SELECT
          station_code,
          n_cross_sections,
          n_cross_section_vertices,
          first_cross_section_datetime,
          last_cross_section_datetime,
          min_vertex_distance_m,
          max_vertex_distance_m,
          min_vertex_stage_cm,
          max_vertex_stage_cm
        FROM cross_section_summary
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
        COALESCE(cs.n_cross_sections, 0) AS n_cross_sections,
        COALESCE(cs.n_cross_section_vertices, 0) AS n_cross_section_vertices,
        cs.first_cross_section_datetime,
        cs.last_cross_section_datetime,
        cs.min_vertex_distance_m,
        cs.max_vertex_distance_m,
        cs.min_vertex_stage_cm,
        cs.max_vertex_stage_cm,
        CASE
          WHEN COALESCE(d.n_discharge_measurements, 0) > 0 THEN TRUE
          ELSE FALSE
        END AS has_discharge_measurements_processed,
        CASE
          WHEN COALESCE(rc.n_rating_curves, 0) > 0 THEN TRUE
          ELSE FALSE
        END AS has_rating_curves_processed,
        CASE
          WHEN COALESCE(cs.n_cross_sections, 0) > 0 THEN TRUE
          ELSE FALSE
        END AS has_cross_sections_processed
      FROM stations s
      LEFT JOIN discharge_by_station d
        ON s.station_code = d.station_code
      LEFT JOIN rating_curves_by_station rc
        ON s.station_code = rc.station_code
      LEFT JOIN rating_curve_segments_by_station rcs
        ON s.station_code = rcs.station_code
      LEFT JOIN cross_sections_by_station cs
        ON s.station_code = cs.station_code
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
  SELECT 'cross_sections' AS table_name, COUNT(*) AS n_rows FROM cross_sections
  UNION ALL
  SELECT 'cross_section_vertices' AS table_name, COUNT(*) AS n_rows FROM cross_section_vertices
  UNION ALL
  SELECT 'cross_section_summary' AS table_name, COUNT(*) AS n_rows FROM cross_section_summary
  UNION ALL
  SELECT 'discharge_measurements_qc_summary' AS table_name, COUNT(*) AS n_rows FROM discharge_measurements_qc_summary
  UNION ALL
  SELECT 'rating_curves_qc_summary' AS table_name, COUNT(*) AS n_rows FROM rating_curves_qc_summary
  UNION ALL
  SELECT 'cross_sections_qc_summary' AS table_name, COUNT(*) AS n_rows FROM cross_sections_qc_summary
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
  UNION ALL
  SELECT 'v_cross_sections_with_station' AS view_name, COUNT(*) AS n_rows
  FROM v_cross_sections_with_station
  UNION ALL
  SELECT 'v_cross_section_vertices_with_station' AS view_name, COUNT(*) AS n_rows
  FROM v_cross_section_vertices_with_station
  UNION ALL
  SELECT 'v_cross_section_summary_with_station' AS view_name, COUNT(*) AS n_rows
  FROM v_cross_section_summary_with_station
  "
)

key_checks <- tibble::tibble(
  check_name = c(
    "duplicate_cross_section_ids",
    "duplicate_cross_section_vertex_ids",
    "duplicate_cross_section_summary_station_codes",
    "missing_cross_section_ids_in_sections",
    "missing_cross_section_summary_station_codes",
    "missing_cross_section_station_codes",
    "missing_cross_section_vertex_station_codes",
    "missing_cross_section_summary_station_codes_from_stations"
  ),
  value = c(
    nrow(cross_section_duplicates),
    nrow(cross_section_vertex_duplicates),
    nrow(cross_section_summary_duplicates),
    length(missing_cross_section_ids_in_sections),
    length(missing_cross_section_summary_station_codes),
    length(missing_cross_section_station_codes),
    length(missing_cross_section_vertex_station_codes),
    length(missing_cross_section_summary_station_codes_from_stations)
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
message("Finished writing cross-section products to DuckDB.")
message("Database: ", db_file)
message("Cross sections: ", nrow(cross_sections))
message("Cross-section vertices: ", nrow(cross_section_vertices))
message("Cross-section stations: ", dplyr::n_distinct(cross_sections$station_code))
message("Summary file: ", output_summary_file)

print(table_counts)
print(view_counts)
print(key_checks)
