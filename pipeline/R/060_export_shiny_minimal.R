# ============================================================
# pipeline/R/060_export_shiny_minimal.R
# Create compact Shiny-ready exports from the local ANA DuckDB
# ============================================================

# Load packages
library(DBI)
library(duckdb)
library(jsonlite)

# Load shared pipeline helpers
source(file.path("pipeline", "helpers", "duckdb_helpers.R"), local = TRUE)

# Define paths
source_db <- file.path("data", "ana_hidro.duckdb")
export_dir <- "exports"
export_db <- file.path(export_dir, "shiny_minimal.duckdb")
metadata_json <- file.path(export_dir, "metadata.json")
export_row_counts_csv <- file.path(export_dir, "export_row_counts.csv")
data_dictionary_csv <- file.path(export_dir, "data_dictionary.csv")

# Critical input checks
if (!file.exists(source_db)) {
  stop("Input DuckDB database was not found: ", source_db)
}

if (!dir.exists(export_dir)) {
  dir.create(export_dir, recursive = TRUE)
}

if (file.exists(export_db)) {
  file.remove(export_db)
}

# Connect to export database and attach source database as read-only
con <- dbConnect(duckdb::duckdb(), dbdir = export_db, read_only = FALSE)
on.exit(dbDisconnect(con), add = TRUE)

source_db_abs <- normalizePath(source_db, winslash = "/", mustWork = TRUE)

dbExecute(
  con,
  paste0(
    "ATTACH ",
    as.character(dbQuoteString(con, source_db_abs)),
    " AS src (READ_ONLY)"
  )
)

# ------------------------------------------------------------
# Helper functions
# ------------------------------------------------------------

sql_string <- function(x) {
  as.character(dbQuoteString(con, x))
}

source_table <- function(x) {
  paste0("src.main.", quote_ident(x))
}

export_table <- function(x) {
  quote_ident(x)
}

get_source_objects <- function() {
  source_tables <- dbGetQuery(
    con,
    "SELECT table_name
     FROM information_schema.tables
     WHERE table_catalog = 'src'
       AND table_schema = 'main'"
  )$table_name
  
  source_views <- tryCatch(
    {
      dbGetQuery(
        con,
        "SELECT table_name
         FROM information_schema.views
         WHERE table_catalog = 'src'
           AND table_schema = 'main'"
      )$table_name
    },
    error = function(e) character()
  )
  
  unique(c(source_tables, source_views))
}

get_source_columns <- function(object_name) {
  dbGetQuery(
    con,
    paste0(
      "SELECT column_name ",
      "FROM information_schema.columns ",
      "WHERE table_catalog = 'src' ",
      "AND table_schema = 'main' ",
      "AND table_name = ", sql_string(object_name), " ",
      "ORDER BY ordinal_position"
    )
  )$column_name
}

first_existing <- function(columns, candidates, label, required = TRUE) {
  found <- candidates[candidates %in% columns]
  
  if (length(found) > 0) {
    return(found[1])
  }
  
  if (required) {
    stop(
      "Could not find a required column for ", label,
      ". Tried: ", paste(candidates, collapse = ", ")
    )
  }
  
  NA_character_
}

count_rows <- function(table_name) {
  dbGetQuery(
    con,
    paste0("SELECT COUNT(*) AS n_rows FROM ", export_table(table_name))
  )$n_rows[1]
}

duplicate_key_count <- function(table_name, key_columns) {
  key_sql <- paste(quote_ident(key_columns), collapse = ", ")
  
  dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(*) AS n_duplicate_keys ",
      "FROM (",
      "  SELECT ", key_sql, ", COUNT(*) AS n ",
      "  FROM ", export_table(table_name), " ",
      "  GROUP BY ", key_sql, " ",
      "  HAVING COUNT(*) > 1",
      ") x"
    )
  )$n_duplicate_keys[1]
}

missing_station_links <- function(table_name) {
  dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(*) AS n_missing ",
      "FROM (",
      "  SELECT DISTINCT station_code ",
      "  FROM ", export_table(table_name),
      ") p ",
      "LEFT JOIN stations_minimal s USING (station_code) ",
      "WHERE p.station_code IS NULL OR s.station_code IS NULL"
    )
  )$n_missing[1]
}

missing_cross_section_links <- function(vertex_table, section_table) {
  dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(*) AS n_missing ",
      "FROM (",
      "  SELECT DISTINCT cross_section_id ",
      "  FROM ", export_table(vertex_table),
      ") v ",
      "LEFT JOIN ", export_table(section_table), " s USING (cross_section_id) ",
      "WHERE v.cross_section_id IS NULL OR s.cross_section_id IS NULL"
    )
  )$n_missing[1]
}

safe_export_columns <- function(columns) {
  lower_columns <- tolower(columns)
  
  exact_exclude <- lower_columns %in% c(
    "raw_file",
    "raw_file_path",
    "first_raw_file",
    "source_file",
    "source_path",
    "local_file",
    "request_headers",
    "response_body",
    "raw_json",
    "json_response",
    "raw_verticais"
  )
  
  sensitive_pattern <- grepl(
    "token|senha|password|cpf|cnpj|identificador|credential|secret",
    lower_columns
  )
  
  columns[!(exact_exclude | sensitive_pattern)]
}

create_export_table <- function(source_object, target_table, order_by = NULL) {
  source_columns <- get_source_columns(source_object)
  selected_columns <- safe_export_columns(source_columns)
  
  if (length(selected_columns) == 0) {
    stop("No safe export columns were found for source object: ", source_object)
  }
  
  select_sql <- paste(quote_ident(selected_columns), collapse = ", ")
  
  order_sql <- ""
  if (!is.null(order_by)) {
    valid_order <- order_by[order_by %in% selected_columns]
    if (length(valid_order) > 0) {
      order_sql <- paste0(
        " ORDER BY ",
        paste(quote_ident(valid_order), collapse = ", ")
      )
    }
  }
  
  dbExecute(
    con,
    paste0(
      "CREATE TABLE ", export_table(target_table), " AS ",
      "SELECT ", select_sql, " ",
      "FROM ", source_table(source_object),
      order_sql
    )
  )
}

create_export_table_selected <- function(source_object, target_table, selected_columns, order_by = NULL) {
  source_columns <- get_source_columns(source_object)
  
  selected_columns <- selected_columns[selected_columns %in% source_columns]
  selected_columns <- safe_export_columns(selected_columns)
  
  if (length(selected_columns) == 0) {
    stop("No selected safe export columns were found for source object: ", source_object)
  }
  
  select_sql <- paste(quote_ident(selected_columns), collapse = ", ")
  
  order_sql <- ""
  if (!is.null(order_by)) {
    valid_order <- order_by[order_by %in% selected_columns]
    if (length(valid_order) > 0) {
      order_sql <- paste0(
        " ORDER BY ",
        paste(quote_ident(valid_order), collapse = ", ")
      )
    }
  }
  
  dbExecute(
    con,
    paste0(
      "CREATE TABLE ", export_table(target_table), " AS ",
      "SELECT ", select_sql, " ",
      "FROM ", source_table(source_object),
      order_sql
    )
  )
}

numeric_summary_sql <- function(column_name, prefix, unit_suffix) {
  if (is.na(column_name)) {
    return(c(
      paste0("CAST(0 AS BIGINT) AS n_", prefix, "_values"),
      paste0("CAST(NULL AS DOUBLE) AS ", prefix, "_min_", unit_suffix),
      paste0("CAST(NULL AS DOUBLE) AS ", prefix, "_max_", unit_suffix),
      paste0("CAST(NULL AS DOUBLE) AS ", prefix, "_mean_", unit_suffix)
    ))
  }
  
  value_sql <- paste0("TRY_CAST(", quote_ident(column_name), " AS DOUBLE)")
  
  c(
    paste0("COUNT(", value_sql, ") AS n_", prefix, "_values"),
    paste0("MIN(", value_sql, ") AS ", prefix, "_min_", unit_suffix),
    paste0("MAX(", value_sql, ") AS ", prefix, "_max_", unit_suffix),
    paste0("AVG(", value_sql, ") AS ", prefix, "_mean_", unit_suffix)
  )
}

# ------------------------------------------------------------
# 1. Check required source objects
# ------------------------------------------------------------

required_source_objects <- c(
  "stations",
  "metadata",
  "discharge_measurements",
  "rating_curves",
  "rating_curve_summary",
  "cross_sections",
  "cross_section_vertices",
  "cross_section_summary",
  "v_station_discharge_products_summary",
  "v_discharge_measurements_with_station",
  "v_rating_curves_with_station",
  "v_rating_curve_summary_with_station",
  "v_cross_sections_with_station",
  "v_cross_section_vertices_with_station",
  "v_cross_section_summary_with_station"
)

source_objects <- get_source_objects()
missing_objects <- setdiff(required_source_objects, source_objects)

if (length(missing_objects) > 0) {
  stop(
    "The following required source tables/views are missing from ",
    source_db,
    ": ",
    paste(missing_objects, collapse = ", ")
  )
}

# ------------------------------------------------------------
# 2. stations_minimal
# ------------------------------------------------------------

station_columns <- get_source_columns("stations")

required_station_columns <- c(
  "station_code",
  "station_name",
  "uf",
  "latitude",
  "longitude"
)

missing_station_columns <- setdiff(required_station_columns, station_columns)

if (length(missing_station_columns) > 0) {
  stop(
    "The stations table is missing required columns: ",
    paste(missing_station_columns, collapse = ", ")
  )
}

desired_station_columns <- c(
  "station_code",
  "station_name",
  "station_type",
  "uf",
  "municipality",
  "basin_code",
  "basin_name",
  "river_name",
  "latitude",
  "longitude",
  "altitude",
  "drainage_area",
  "operator",
  "responsible_agency",
  "is_operating",
  "discharge_start_date",
  "discharge_end_date",
  "telemetric_start_date",
  "telemetric_end_date",
  "stage_start_date",
  "stage_end_date",
  "rainfall_start_date",
  "rainfall_end_date",
  "has_discharge_measurements",
  "has_telemetry",
  "has_stage_data",
  "has_rainfall_data",
  "last_update"
)

station_export_columns <- desired_station_columns[desired_station_columns %in% station_columns]
station_export_columns <- safe_export_columns(station_export_columns)

station_select_sql <- paste(quote_ident(station_export_columns), collapse = ", ")

dbExecute(
  con,
  paste0(
    "CREATE TABLE stations_minimal AS ",
    "SELECT ", station_select_sql, " ",
    "FROM ", source_table("stations"), " ",
    "ORDER BY station_code"
  )
)

# ------------------------------------------------------------
# 3. Station-level product summary
# ------------------------------------------------------------

create_export_table(
  source_object = "v_station_discharge_products_summary",
  target_table = "station_discharge_products_summary",
  order_by = c("station_code")
)

# ------------------------------------------------------------
# 4. Discharge measurements and compact summaries
# ------------------------------------------------------------

create_export_table(
  source_object = "discharge_measurements",
  target_table = "discharge_measurements",
  order_by = c("station_code", "measurement_datetime", "consistency_level")
)

discharge_columns <- get_source_columns("discharge_measurements")

measurement_datetime_col <- first_existing(
  discharge_columns,
  c("measurement_datetime", "datetime", "date_time", "data_hora_medicao"),
  label = "discharge measurement datetime"
)

stage_col <- first_existing(
  discharge_columns,
  c("stage_cm", "water_level_cm", "level_cm", "cota_cm", "stage", "water_level", "cota"),
  label = "stage/water level",
  required = FALSE
)

discharge_col <- first_existing(
  discharge_columns,
  c("discharge_m3s", "flow_m3s", "vazao_m3s", "discharge", "flow", "vazao"),
  label = "discharge",
  required = FALSE
)

consistency_col <- first_existing(
  discharge_columns,
  c("consistency_level", "nivel_consistencia"),
  label = "consistency level",
  required = FALSE
)

datetime_sql <- paste0("TRY_CAST(", quote_ident(measurement_datetime_col), " AS TIMESTAMP)")
year_sql <- paste0("CAST(EXTRACT(YEAR FROM ", datetime_sql, ") AS INTEGER)")

if (!is.na(consistency_col)) {
  consistency_sql <- paste0("TRY_CAST(", quote_ident(consistency_col), " AS INTEGER)")
  
  consistency_summary_sql <- c(
    paste0("COUNT(DISTINCT ", consistency_sql, ") AS n_consistency_levels"),
    paste0("COUNT(*) FILTER (WHERE ", consistency_sql, " = 1) AS n_consistency_level_1"),
    paste0("COUNT(*) FILTER (WHERE ", consistency_sql, " = 2) AS n_consistency_level_2")
  )
} else {
  consistency_summary_sql <- c(
    "CAST(NULL AS BIGINT) AS n_consistency_levels",
    "CAST(NULL AS BIGINT) AS n_consistency_level_1",
    "CAST(NULL AS BIGINT) AS n_consistency_level_2"
  )
}

measurement_numeric_summary_sql <- c(
  numeric_summary_sql(stage_col, "stage", "cm"),
  numeric_summary_sql(discharge_col, "discharge", "m3s")
)

station_summary_fields <- c(
  "station_code",
  "COUNT(*) AS n_measurements",
  paste0("MIN(", datetime_sql, ") AS first_measurement_datetime"),
  paste0("MAX(", datetime_sql, ") AS last_measurement_datetime"),
  paste0("COUNT(DISTINCT ", year_sql, ") AS n_years_with_measurements"),
  consistency_summary_sql,
  measurement_numeric_summary_sql
)

dbExecute(
  con,
  paste0(
    "CREATE TABLE discharge_measurements_summary_by_station AS ",
    "SELECT ", paste(station_summary_fields, collapse = ",\n       "), " ",
    "FROM discharge_measurements ",
    "GROUP BY station_code ",
    "ORDER BY station_code"
  )
)

year_summary_fields <- c(
  "station_code",
  paste0(year_sql, " AS measurement_year"),
  "COUNT(*) AS n_measurements",
  paste0("MIN(", datetime_sql, ") AS first_measurement_datetime"),
  paste0("MAX(", datetime_sql, ") AS last_measurement_datetime"),
  consistency_summary_sql,
  measurement_numeric_summary_sql
)

dbExecute(
  con,
  paste0(
    "CREATE TABLE discharge_measurements_summary_by_year AS ",
    "SELECT ", paste(year_summary_fields, collapse = ",\n       "), " ",
    "FROM discharge_measurements ",
    "WHERE ", datetime_sql, " IS NOT NULL ",
    "GROUP BY station_code, ", year_sql, " ",
    "ORDER BY station_code, measurement_year"
  )
)

# ------------------------------------------------------------
# 5. Rating-curve products
# ------------------------------------------------------------

create_export_table(
  source_object = "rating_curve_summary",
  target_table = "rating_curve_summary",
  order_by = c("station_code", "valid_from", "valid_to", "consistency_level", "rating_curve_id")
)

create_export_table(
  source_object = "rating_curves",
  target_table = "rating_curves",
  order_by = c("station_code", "valid_from", "valid_to", "consistency_level", "rating_curve_id", "segment_number")
)

# ------------------------------------------------------------
# 6. Cross-section products
# ------------------------------------------------------------

# Cross-section survey-level table.
# This table is small enough to preserve useful metadata, but raw file paths are
# excluded by safe_export_columns().
create_export_table(
  source_object = "cross_sections",
  target_table = "cross_sections",
  order_by = c("station_code", "cross_section_id")
)

# Compact vertex-level table.
# Do not export raw_verticais or repeated observation fields at vertex level.
# They are very large and not needed for Shiny profile plots.
cross_section_vertex_columns <- c(
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
  "x_distance_min_m",
  "x_distance_max_m",
  "y_stage_min_cm",
  "y_stage_max_cm",
  "geometry_stage_step_cm",
  "source_route",
  "downloaded_at",
  "processed_at"
)

create_export_table_selected(
  source_object = "cross_section_vertices",
  target_table = "cross_section_vertices",
  selected_columns = cross_section_vertex_columns,
  order_by = c("station_code", "cross_section_id", "vertex_order", "cross_section_vertex_id")
)

create_export_table(
  source_object = "cross_section_summary",
  target_table = "cross_section_summary",
  order_by = c("station_code")
)

# ------------------------------------------------------------
# 7. Shiny-friendly views
# ------------------------------------------------------------

station_view_columns <- c(
  "station_name",
  "uf",
  "municipality",
  "latitude",
  "longitude"
)

station_view_columns <- station_view_columns[station_view_columns %in% station_export_columns]

station_join_sql <- ""
if (length(station_view_columns) > 0) {
  station_join_sql <- paste0(
    ", ",
    paste0("s.", quote_ident(station_view_columns), collapse = ", ")
  )
}

dbExecute(
  con,
  "CREATE VIEW v_station_discharge_products_summary AS
   SELECT *
   FROM station_discharge_products_summary"
)

dbExecute(
  con,
  paste0(
    "CREATE VIEW v_discharge_measurements_with_station AS ",
    "SELECT d.*", station_join_sql, " ",
    "FROM discharge_measurements d ",
    "LEFT JOIN stations_minimal s USING (station_code)"
  )
)

dbExecute(
  con,
  paste0(
    "CREATE VIEW v_rating_curves_with_station AS ",
    "SELECT r.*", station_join_sql, " ",
    "FROM rating_curves r ",
    "LEFT JOIN stations_minimal s USING (station_code)"
  )
)

dbExecute(
  con,
  paste0(
    "CREATE VIEW v_rating_curve_summary_with_station AS ",
    "SELECT r.*", station_join_sql, " ",
    "FROM rating_curve_summary r ",
    "LEFT JOIN stations_minimal s USING (station_code)"
  )
)

dbExecute(
  con,
  paste0(
    "CREATE VIEW v_cross_sections_with_station AS ",
    "SELECT c.*", station_join_sql, " ",
    "FROM cross_sections c ",
    "LEFT JOIN stations_minimal s USING (station_code)"
  )
)

dbExecute(
  con,
  paste0(
    "CREATE VIEW v_cross_section_vertices_with_station AS ",
    "SELECT v.*", station_join_sql, " ",
    "FROM cross_section_vertices v ",
    "LEFT JOIN stations_minimal s USING (station_code)"
  )
)

dbExecute(
  con,
  paste0(
    "CREATE VIEW v_cross_section_summary_with_station AS ",
    "SELECT c.*", station_join_sql, " ",
    "FROM cross_section_summary c ",
    "LEFT JOIN stations_minimal s USING (station_code)"
  )
)

# ------------------------------------------------------------
# 8. Data dictionary
# ------------------------------------------------------------

data_dictionary <- data.frame(
  table_name = c(
    "stations_minimal",
    "station_discharge_products_summary",
    "discharge_measurements",
    "discharge_measurements_summary_by_station",
    "discharge_measurements_summary_by_year",
    "rating_curve_summary",
    "rating_curves",
    "cross_sections",
    "cross_section_vertices",
    "cross_section_summary",
    "metadata",
    "data_dictionary",
    "export_row_counts"
  ),
  description = c(
    "Compact station metadata table for Shiny use.",
    "Station-level summary of available discharge-measurement, rating-curve, and related products.",
    "Cleaned point-level discharge measurements from the resumo_descarga product; useful for station-level inspection and stage-discharge plots.",
    "Compact station-level summary of cleaned discharge measurements.",
    "Compact station-year summary of cleaned discharge measurements.",
    "One row per rating curve, preserving curve-level identifiers and validity information.",
    "One row per rating-curve segment, preserving multi-segment curves.",
    "One row per cross-section survey/profile record.",
    "One row per cross-section profile vertex; included for selected-station profile plots.",
    "One row per station with cross-section summary information.",
    "Export-level metadata, limitations, source information, and security/API statements.",
    "Table-level data dictionary for the Shiny minimal export.",
    "Final row counts for all exported tables."
  ),
  source_object = c(
    "src.main.stations",
    "src.main.v_station_discharge_products_summary",
    "src.main.discharge_measurements",
    "derived from discharge_measurements",
    "derived from discharge_measurements",
    "src.main.rating_curve_summary",
    "src.main.rating_curves",
    "src.main.cross_sections",
    "src.main.cross_section_vertices",
    "src.main.cross_section_summary",
    "derived during export",
    "derived during export",
    "derived during export"
  ),
  notes = c(
    "station_code is the integration key.",
    "Materialized from the source DuckDB view for faster Shiny access.",
    "This is not a complete continuous discharge time series.",
    "Designed for compact Shiny summaries and filters.",
    "Designed for compact temporal summaries without exporting continuous time series.",
    "Use station_code and rating_curve_id for joins.",
    "Use rating_curve_id and rating_curve_segment_id for joins.",
    "Use station_code and cross_section_id for joins.",
    "Use cross_section_id to link vertices to cross_sections.",
    "station_code should be unique in this table.",
    "Does not contain ANA credentials, CPF/CNPJ, passwords, or tokens.",
    "Can be extended as the Shiny app becomes more detailed.",
    "Used for deployment checks and reproducibility."
  ),
  stringsAsFactors = FALSE
)

dbWriteTable(con, "data_dictionary", data_dictionary, overwrite = TRUE)

# Create placeholder metadata and row-count tables before final checks
placeholder_metadata <- data.frame(
  key = "export_status",
  value = "metadata will be finalized at the end of the export",
  stringsAsFactors = FALSE
)

dbWriteTable(con, "metadata", placeholder_metadata, overwrite = TRUE)

placeholder_row_counts <- data.frame(
  table_name = "placeholder",
  n_rows = 0,
  stringsAsFactors = FALSE
)

dbWriteTable(con, "export_row_counts", placeholder_row_counts, overwrite = TRUE)

# ------------------------------------------------------------
# 9. Critical key checks
# ------------------------------------------------------------

export_discharge_columns <- dbListFields(con, "discharge_measurements")
export_rating_curve_columns <- dbListFields(con, "rating_curves")
export_rating_curve_summary_columns <- dbListFields(con, "rating_curve_summary")
export_cross_section_columns <- dbListFields(con, "cross_sections")
export_cross_section_vertex_columns <- dbListFields(con, "cross_section_vertices")
export_cross_section_summary_columns <- dbListFields(con, "cross_section_summary")

measurement_datetime_export_col <- first_existing(
  export_discharge_columns,
  c("measurement_datetime", "datetime", "date_time", "data_hora_medicao"),
  label = "exported discharge measurement datetime"
)

consistency_export_col <- first_existing(
  export_discharge_columns,
  c("consistency_level", "nivel_consistencia"),
  label = "exported consistency level"
)

rating_curve_segment_id_col <- first_existing(
  export_rating_curve_columns,
  c("rating_curve_segment_id"),
  label = "rating curve segment id"
)

rating_curve_id_summary_col <- first_existing(
  export_rating_curve_summary_columns,
  c("rating_curve_id"),
  label = "rating curve id in rating_curve_summary"
)

cross_section_id_col <- first_existing(
  export_cross_section_columns,
  c("cross_section_id"),
  label = "cross-section id"
)

cross_section_vertex_id_col <- first_existing(
  export_cross_section_vertex_columns,
  c("cross_section_vertex_id"),
  label = "cross-section vertex id"
)

cross_section_summary_station_col <- first_existing(
  export_cross_section_summary_columns,
  c("station_code"),
  label = "station code in cross_section_summary"
)

export_key_checks <- data.frame(
  check_name = c(
    "duplicate_station_codes",
    "duplicate_station_discharge_summary_station_codes",
    "duplicate_discharge_measurement_keys",
    "duplicate_rating_curve_segment_ids",
    "duplicate_rating_curve_summary_ids",
    "duplicate_cross_section_ids",
    "duplicate_cross_section_vertex_ids",
    "duplicate_cross_section_summary_station_codes"
  ),
  value = c(
    duplicate_key_count("stations_minimal", c("station_code")),
    duplicate_key_count("station_discharge_products_summary", c("station_code")),
    duplicate_key_count(
      "discharge_measurements",
      c("station_code", measurement_datetime_export_col, consistency_export_col)
    ),
    duplicate_key_count("rating_curves", c(rating_curve_segment_id_col)),
    duplicate_key_count("rating_curve_summary", c(rating_curve_id_summary_col)),
    duplicate_key_count("cross_sections", c(cross_section_id_col)),
    duplicate_key_count("cross_section_vertices", c(cross_section_vertex_id_col)),
    duplicate_key_count("cross_section_summary", c(cross_section_summary_station_col))
  ),
  stringsAsFactors = FALSE
)

failed_key_checks <- export_key_checks[export_key_checks$value > 0, ]

if (nrow(failed_key_checks) > 0) {
  stop(
    "Critical duplicate-key checks failed: ",
    paste(
      paste0(failed_key_checks$check_name, " = ", failed_key_checks$value),
      collapse = "; "
    )
  )
}

# ------------------------------------------------------------
# 10. Critical linkage checks
# ------------------------------------------------------------

export_linkage_checks <- data.frame(
  check_name = c(
    "missing_station_links_in_station_discharge_products_summary",
    "missing_station_links_in_discharge_measurements",
    "missing_station_links_in_discharge_measurements_summary_by_station",
    "missing_station_links_in_discharge_measurements_summary_by_year",
    "missing_station_links_in_rating_curve_summary",
    "missing_station_links_in_rating_curves",
    "missing_station_links_in_cross_sections",
    "missing_station_links_in_cross_section_vertices",
    "missing_station_links_in_cross_section_summary",
    "missing_cross_section_id_links_from_vertices_to_cross_sections"
  ),
  value = c(
    missing_station_links("station_discharge_products_summary"),
    missing_station_links("discharge_measurements"),
    missing_station_links("discharge_measurements_summary_by_station"),
    missing_station_links("discharge_measurements_summary_by_year"),
    missing_station_links("rating_curve_summary"),
    missing_station_links("rating_curves"),
    missing_station_links("cross_sections"),
    missing_station_links("cross_section_vertices"),
    missing_station_links("cross_section_summary"),
    missing_cross_section_links("cross_section_vertices", "cross_sections")
  ),
  stringsAsFactors = FALSE
)

failed_linkage_checks <- export_linkage_checks[export_linkage_checks$value > 0, ]

if (nrow(failed_linkage_checks) > 0) {
  stop(
    "Critical linkage checks failed: ",
    paste(
      paste0(failed_linkage_checks$check_name, " = ", failed_linkage_checks$value),
      collapse = "; "
    )
  )
}

# ------------------------------------------------------------
# 11. Final row counts
# ------------------------------------------------------------

final_tables <- c(
  "stations_minimal",
  "station_discharge_products_summary",
  "discharge_measurements",
  "discharge_measurements_summary_by_station",
  "discharge_measurements_summary_by_year",
  "rating_curve_summary",
  "rating_curves",
  "cross_sections",
  "cross_section_vertices",
  "cross_section_summary",
  "metadata",
  "data_dictionary",
  "export_row_counts"
)

export_table_counts <- data.frame(
  table_name = final_tables,
  n_rows = as.numeric(vapply(final_tables, count_rows, numeric(1))),
  stringsAsFactors = FALSE
)

empty_tables <- export_table_counts$table_name[export_table_counts$n_rows == 0]

if (length(empty_tables) > 0) {
  stop("The following exported tables are empty: ", paste(empty_tables, collapse = ", "))
}

# ------------------------------------------------------------
# 12. Final metadata
# ------------------------------------------------------------

source_metadata_columns <- get_source_columns("metadata")

if (all(c("key", "value") %in% source_metadata_columns)) {
  source_metadata <- dbGetQuery(
    con,
    paste0("SELECT key, value FROM ", source_table("metadata"))
  )
  
  source_metadata$key <- as.character(source_metadata$key)
  source_metadata$value <- as.character(source_metadata$value)
  
  keep_source_metadata <- !grepl(
    "token|senha|password|cpf|cnpj|identificador|credential|secret",
    tolower(source_metadata$key)
  )
  
  source_metadata <- source_metadata[keep_source_metadata, , drop = FALSE]
  
  source_metadata_rows <- data.frame(
    key = paste0("source_metadata.", source_metadata$key),
    value = source_metadata$value,
    stringsAsFactors = FALSE
  )
} else {
  source_metadata_rows <- data.frame(
    key = character(),
    value = character(),
    stringsAsFactors = FALSE
  )
}

metadata_rows <- data.frame(
  key = c(
    "export_name",
    "export_database_path",
    "source_database_path",
    "export_datetime",
    "source_tables_views",
    "included_point_level_discharge_measurements",
    "included_cross_section_vertices",
    "security_statement",
    "api_statement",
    "privacy_statement",
    "raw_data_statement",
    "limitations",
    "export_table_counts",
    "export_key_checks",
    "export_linkage_checks"
  ),
  value = c(
    "shiny_minimal",
    export_db,
    source_db,
    format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    paste(required_source_objects, collapse = "; "),
    "yes; cleaned point-level discharge measurements are included for selected-station inspection and stage-discharge plots",
    "yes; full cross_section_vertices is included because it is useful for selected-station cross-section profile plots and remains acceptable for a local first deployment",
    "This export does not contain ANA credentials, CPF/CNPJ, passwords, or authentication tokens.",
    "This bundled export contains no credentials or authentication tokens. The Shiny application may perform user-initiated authenticated ANA API requests using session-only credentials and token state.",
    "Uploaded and downloaded daily series, partial download state, authentication tokens, and derived session analyses remain in memory for the active session and are not written by the application to DuckDB, project files, or persistent caches.",
    "This export does not include raw JSON files. Local raw-file path fields and sensitive fields are excluded when detected.",
    paste(
      c(
        "Complete fluviometric, pluviometric, rainfall, stage, and telemetric time series are not included.",
        "Discharge measurements are point measurements from the cleaned resumo_descarga product, not a continuous discharge time series.",
        "Cross-section vertices are included for profile plots, but they may be simplified in a future deployment if file size becomes limiting.",
        "The export is intended as a compact Shiny-ready data product and may evolve with future modules."
      ),
      collapse = " "
    ),
    as.character(toJSON(export_table_counts, dataframe = "rows", auto_unbox = TRUE)),
    as.character(toJSON(export_key_checks, dataframe = "rows", auto_unbox = TRUE)),
    as.character(toJSON(export_linkage_checks, dataframe = "rows", auto_unbox = TRUE))
  ),
  stringsAsFactors = FALSE
)

metadata_rows <- rbind(metadata_rows, source_metadata_rows)

dbWriteTable(con, "metadata", metadata_rows, overwrite = TRUE)

# Recompute row counts after final metadata has been written
export_table_counts <- data.frame(
  table_name = final_tables,
  n_rows = as.numeric(vapply(final_tables, count_rows, numeric(1))),
  stringsAsFactors = FALSE
)

dbWriteTable(con, "export_row_counts", export_table_counts, overwrite = TRUE)

# Recompute once more so export_row_counts contains its own final row count
export_table_counts <- data.frame(
  table_name = final_tables,
  n_rows = as.numeric(vapply(final_tables, count_rows, numeric(1))),
  stringsAsFactors = FALSE
)

dbWriteTable(con, "export_row_counts", export_table_counts, overwrite = TRUE)

# ------------------------------------------------------------
# 13. Supporting files
# ------------------------------------------------------------

metadata_json_content <- list(
  export_name = "shiny_minimal",
  export_database_path = export_db,
  source_database_path = source_db,
  export_datetime = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
  source_tables_views = required_source_objects,
  exported_tables = export_table_counts,
  export_key_checks = export_key_checks,
  export_linkage_checks = export_linkage_checks,
  included_point_level_discharge_measurements = TRUE,
  included_cross_section_vertices = TRUE,
  security_statement = "This export does not contain ANA credentials, CPF/CNPJ, passwords, or authentication tokens.",
  api_statement = "This bundled export contains no credentials or authentication tokens. The Shiny application may perform user-initiated authenticated ANA API requests using session-only credentials and token state.",
  privacy_statement = "Uploaded and downloaded daily series, partial download state, authentication tokens, and derived session analyses remain in memory for the active session and are not written by the application to DuckDB, project files, or persistent caches.",
  raw_data_statement = "This export does not include raw JSON files. Local raw-file path fields and sensitive fields are excluded when detected.",
  limitations = c(
    "Complete fluviometric, pluviometric, rainfall, stage, and telemetric time series are not included.",
    "Discharge measurements are point measurements from the cleaned resumo_descarga product, not a continuous discharge time series.",
    "Cross-section vertices are included for profile plots, but they may be simplified in a future deployment if file size becomes limiting.",
    "The export is intended as a compact Shiny-ready data product and may evolve with future modules."
  )
)

write_json(
  metadata_json_content,
  path = metadata_json,
  pretty = TRUE,
  auto_unbox = TRUE,
  na = "null"
)

write.csv(
  export_table_counts,
  file = export_row_counts_csv,
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

write.csv(
  data_dictionary,
  file = data_dictionary_csv,
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# Flush database changes before reporting file size
dbExecute(con, "CHECKPOINT")

export_db_size_mb <- round(file.info(export_db)$size / 1024^2, 2)

# ------------------------------------------------------------
# 14. Console summary
# ------------------------------------------------------------

message("Finished writing Shiny minimal exports.")
message("Export database: ", export_db)
message("Export database size: ", export_db_size_mb, " MB")
message("Metadata JSON: ", metadata_json)
message("Export row counts CSV: ", export_row_counts_csv)
message("Data dictionary CSV: ", data_dictionary_csv)

message("Exported table counts:")
print(export_table_counts)

message("Export key checks:")
print(export_key_checks)

message("Export linkage checks:")
print(export_linkage_checks)
