# ============================================================
# pipeline/R/050_process_discharge_measurements.R
#
# Process raw ANA HidroWebService discharge measurement JSONs
# from the resumo_descarga route.
#
# Outputs:
# - data/processed/discharge_measurements.parquet
# - data/processed/discharge_measurements_duplicates_removed.csv
# - data/processed/discharge_measurements_qc_summary.csv
#
# Notes:
# - This script does not write to DuckDB yet.
# - It uses only successful requests from discharge_request_log.csv.
# - It removes records without measurement_datetime.
# - If measurement_datetime exists but consistency_level is missing, it assigns consistency_level = 1.
# - If records share station_code + measurement_datetime and at least one is consistency_level = 2,
#   only consistency_level = 2 is kept.
# - Other suspicious values are summarized but not removed.
# ============================================================

# Load packages
library(jsonlite)
library(dplyr)
library(readr)
library(arrow)
library(stringr)
library(lubridate)

# Load shared pipeline helpers
source(file.path("pipeline", "helpers", "ana_parse_helpers.R"), local = TRUE)

# Define paths and parameters
request_log_file <- file.path("data", "processed", "discharge_request_log.csv")
raw_route_dir <- file.path("data", "raw", "discharge_routes", "route=resumo_descarga")

output_file <- file.path("data", "processed", "discharge_measurements.parquet")
duplicates_file <- file.path("data", "processed", "discharge_measurements_duplicates_removed.csv")
qc_summary_file <- file.path("data", "processed", "discharge_measurements_qc_summary.csv")

route_filter <- "resumo_descarga"
source_route_value <- "/EstacoesTelemetricas/HidroSerieResumoDescarga/v1"

processed_at_value <- Sys.time()
project_root <- normalizePath(".", winslash = "/", mustWork = TRUE)

# Critical input checks
if (!file.exists(request_log_file)) {
  stop("Missing request log file: ", request_log_file)
}

if (!dir.exists(raw_route_dir)) {
  stop("Missing raw route directory: ", raw_route_dir)
}

# Helper functions
# Build an index of raw JSON files
raw_json_index <- list.files(
  raw_route_dir,
  pattern = "\\.json$",
  recursive = TRUE,
  full.names = TRUE
)

if (length(raw_json_index) == 0) {
  stop("No JSON files found in: ", raw_route_dir)
}

raw_json_index <- normalizePath(raw_json_index, winslash = "/", mustWork = TRUE)

resolve_file_path <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  
  if (is.na(x) || x == "") {
    return(NA_character_)
  }
  
  candidates <- c(
    x,
    file.path(raw_route_dir, x),
    file.path("data", "raw", "discharge_routes", x)
  )
  
  candidates <- normalizePath(candidates, winslash = "/", mustWork = FALSE)
  existing <- candidates[file.exists(candidates)]
  
  if (length(existing) > 0) {
    return(existing[1])
  }
  
  basename_match <- raw_json_index[basename(raw_json_index) == basename(x)]
  
  if (length(basename_match) > 0) {
    return(basename_match[1])
  }
  
  NA_character_
}

read_resumo_file <- function(file_path, downloaded_at_value) {
  response <- tryCatch(
    jsonlite::fromJSON(file_path, flatten = TRUE),
    error = function(e) {
      stop("Failed to parse JSON file: ", file_path, "\n", e$message)
    }
  )
  
  if (!"items" %in% names(response) || is.null(response$items) || length(response$items) == 0) {
    return(tibble::tibble())
  }
  
  items <- response$items
  
  if (is.data.frame(items)) {
    out <- tibble::as_tibble(items)
  } else if (is.list(items)) {
    out <- dplyr::bind_rows(items)
  } else {
    stop("JSON file has a non-tabular 'items' field: ", file_path)
  }
  
  if (nrow(out) == 0) {
    return(tibble::tibble())
  }
  
  out$raw_file <- to_project_path(file_path)
  out$downloaded_at <- downloaded_at_value
  
  out
}

# Read and standardize request log
request_log <- readr::read_csv(
  request_log_file,
  col_types = readr::cols(.default = readr::col_character()),
  show_col_types = FALSE
)

route_column <- find_column(request_log, c("route_name", "route", "source_route"))
success_column <- find_column(request_log, c("success", "request_success", "ok"))
n_items_column <- find_column(request_log, c("n_items", "items_count", "number_of_items"))
raw_file_column <- find_column(request_log, c("raw_file", "raw_file_path", "file_path", "output_file", "response_file", "json_file"))
downloaded_at_column <- find_column(request_log, c("downloaded_at", "datetime_request", "request_datetime", "requested_at", "finished_at", "timestamp"))

if (is.na(route_column)) {
  stop("Could not identify a route column in the request log.")
}

if (is.na(success_column)) {
  stop("Could not identify a success column in the request log.")
}

if (is.na(n_items_column)) {
  stop("Could not identify an n_items column in the request log.")
}

request_log_std <- request_log %>%
  mutate(
    route_text = tolower(as.character(.data[[route_column]])),
    route_text = if_else(is.na(route_text), "", route_text),
    success_text = tolower(as.character(.data[[success_column]])),
    success_flag = success_text %in% c("true", "t", "1", "yes", "y", "sim"),
    n_items = parse_decimal(.data[[n_items_column]])
  )

requests_positive <- request_log_std %>%
  filter(
    success_flag,
    n_items > 0,
    route_text == route_filter |
      stringr::str_detect(route_text, route_filter) |
      stringr::str_detect(route_text, "hidroserieresumodescarga")
  )

if (nrow(requests_positive) == 0) {
  stop("No successful resumo_descarga requests with n_items > 0 were found in the request log.")
}

# Locate JSON files
if (!is.na(raw_file_column)) {
  requests_positive$raw_file_path <- vapply(
    requests_positive[[raw_file_column]],
    resolve_file_path,
    character(1)
  )
  
  unresolved_files <- requests_positive %>%
    filter(is.na(raw_file_path))
  
  if (nrow(unresolved_files) > 0) {
    stop(
      "Some positive request-log entries could not be linked to JSON files. ",
      "First unresolved entry: ",
      unresolved_files[[raw_file_column]][1]
    )
  }
  
  if (!is.na(downloaded_at_column)) {
    requests_positive$downloaded_at_raw <- requests_positive[[downloaded_at_column]]
  } else {
    requests_positive$downloaded_at_raw <- NA_character_
  }
  
  requests_positive <- requests_positive %>%
    distinct(raw_file_path, .keep_all = TRUE)
} else {
  message("No raw-file column found in the request log. Using all JSON files in the resumo_descarga directory.")
  
  requests_positive <- tibble::tibble(
    raw_file_path = raw_json_index,
    downloaded_at_raw = NA_character_
  )
}

if (nrow(requests_positive) == 0) {
  stop("No JSON files selected for processing.")
}

# Read raw JSON records
raw_tables <- vector("list", nrow(requests_positive))

for (i in seq_len(nrow(requests_positive))) {
  raw_tables[[i]] <- read_resumo_file(
    file_path = requests_positive$raw_file_path[i],
    downloaded_at_value = requests_positive$downloaded_at_raw[i]
  )
  print(paste0(i,'/', nrow(requests_positive)))
}

raw_items <- dplyr::bind_rows(raw_tables)

if (nrow(raw_items) == 0) {
  stop("No discharge measurement records were read from the selected JSON files.")
}

# Standardize fields
discharge_raw <- tibble::tibble(
  station_code = standardize_station_code(
    pick_column(raw_items, c("codigoestacao", "CodigoEstacao", "Codigo_Estacao", "CodEstacao"))
  ),
  measurement_datetime = parse_datetime_api(
    pick_column(raw_items, c("Data_Hora_Dado", "DataHoraDado", "Data_Hora_Medicao"))
  ),
  consistency_level = parse_integer_simple(
    pick_column(raw_items, c("Nivel_Consistencia", "NivelConsistencia", "Consistencia"))
  ),
  last_update = parse_datetime_api(
    pick_column(raw_items, c("Data_Ultima_Alteracao", "DataUltimaAlteracao", "Data_Atualizacao", "Data_Ultima_Atualizacao"))
  ),
  stage_cm = parse_decimal(
    pick_column(raw_items, c("Cota", "Cota (cm)", "Cota_cm"))
  ),
  discharge_m3s = parse_decimal(
    pick_column(raw_items, c("Vazao", "Vazão", "Vazao (m3/s)", "Vazao_m3s", "Vazao_m3_s"))
  ),
  wetted_area_m2 = parse_decimal(
    pick_column(raw_items, c("Area_Molhada", "AreaMolhada", "Area_Molhada (m2)", "Area_Molhada_m2"))
  ),
  width_m = parse_decimal(
    pick_column(raw_items, c("Largura", "Largura (m)", "Largura_m"))
  ),
  mean_depth_m = parse_decimal(
    pick_column(raw_items, c("Profundidade", "Profundidade (m)", "Profundidade_m"))
  ),
  mean_velocity_ms = parse_decimal(
    pick_column(raw_items, c("Vel_Media", "VelMedia", "Vel_Media (m/s)", "Vel_Media_ms"))
  ),
  source_route = source_route_value,
  raw_file = as.character(pick_column(raw_items, c("raw_file"))),
  downloaded_at = parse_datetime_api(
    pick_column(raw_items, c("downloaded_at"))
  ),
  processed_at = processed_at_value
)

# Apply cleaning rules and deduplicate
#
# Rules:
# 1. Remove records without measurement_datetime.
# 2. If measurement_datetime exists but consistency_level is missing,
#    assign consistency_level = 1 and keep the record.
# 3. If records share station_code + measurement_datetime and at least
#    one record has consistency_level == 2, keep only consistency_level == 2.
# 4. Then, within the standard logical key
#    station_code + measurement_datetime + consistency_level,
#    keep the most recent record by last_update.

n_missing_consistency_level_assigned_to_1 <- sum(
  is.na(discharge_raw$consistency_level) &
    !is.na(discharge_raw$measurement_datetime)
)

discharge_cleaning_input <- discharge_raw %>%
  mutate(
    consistency_level = if_else(
      !is.na(measurement_datetime) & is.na(consistency_level),
      1L,
      consistency_level
    )
  )

missing_measurement_datetime_removed <- discharge_cleaning_input %>%
  filter(is.na(measurement_datetime)) %>%
  mutate(
    removal_reason = "missing_measurement_datetime",
    duplicate_group_size = NA_integer_,
    duplicate_rank = NA_integer_
  ) %>%
  select(
    station_code,
    measurement_datetime,
    consistency_level,
    last_update,
    stage_cm,
    discharge_m3s,
    wetted_area_m2,
    width_m,
    mean_depth_m,
    mean_velocity_ms,
    source_route,
    raw_file,
    downloaded_at,
    processed_at,
    duplicate_group_size,
    duplicate_rank,
    removal_reason
  )

records_with_measurement_datetime <- discharge_cleaning_input %>%
  filter(!is.na(measurement_datetime))

datetime_key_records <- records_with_measurement_datetime %>%
  mutate(
    datetime_key_complete = !is.na(station_code) &
      !is.na(measurement_datetime)
  )

complete_datetime_records <- datetime_key_records %>%
  filter(datetime_key_complete) %>%
  group_by(station_code, measurement_datetime) %>%
  mutate(
    datetime_group_size = n(),
    has_consisted_record = any(consistency_level == 2, na.rm = TRUE),
    consistency_preference_remove = has_consisted_record &
      (is.na(consistency_level) | consistency_level != 2)
  ) %>%
  ungroup()

consistency_preference_removed <- complete_datetime_records %>%
  filter(consistency_preference_remove) %>%
  mutate(
    removal_reason = "non_consisted_removed_because_consisted_exists",
    duplicate_group_size = datetime_group_size,
    duplicate_rank = NA_integer_
  ) %>%
  select(
    station_code,
    measurement_datetime,
    consistency_level,
    last_update,
    stage_cm,
    discharge_m3s,
    wetted_area_m2,
    width_m,
    mean_depth_m,
    mean_velocity_ms,
    source_route,
    raw_file,
    downloaded_at,
    processed_at,
    duplicate_group_size,
    duplicate_rank,
    removal_reason
  )

records_after_consistency_preference <- bind_rows(
  complete_datetime_records %>%
    filter(!consistency_preference_remove) %>%
    select(
      -datetime_key_complete,
      -datetime_group_size,
      -has_consisted_record,
      -consistency_preference_remove
    ),
  datetime_key_records %>%
    filter(!datetime_key_complete) %>%
    select(-datetime_key_complete)
)

key_complete <- records_after_consistency_preference %>%
  mutate(
    key_complete = !is.na(station_code) &
      !is.na(measurement_datetime) &
      !is.na(consistency_level)
  )

complete_key_records <- key_complete %>%
  filter(key_complete) %>%
  arrange(
    station_code,
    measurement_datetime,
    consistency_level,
    desc(last_update),
    desc(downloaded_at),
    raw_file
  ) %>%
  group_by(station_code, measurement_datetime, consistency_level) %>%
  mutate(
    duplicate_group_size = n(),
    duplicate_rank = row_number()
  ) %>%
  ungroup()

records_without_complete_key <- key_complete %>%
  filter(!key_complete) %>%
  mutate(
    duplicate_group_size = NA_integer_,
    duplicate_rank = NA_integer_
  )

duplicates_removed_key <- complete_key_records %>%
  filter(duplicate_group_size > 1, duplicate_rank > 1) %>%
  mutate(
    removal_reason = "duplicate_logical_key_keep_latest_last_update"
  ) %>%
  select(
    station_code,
    measurement_datetime,
    consistency_level,
    last_update,
    stage_cm,
    discharge_m3s,
    wetted_area_m2,
    width_m,
    mean_depth_m,
    mean_velocity_ms,
    source_route,
    raw_file,
    downloaded_at,
    processed_at,
    duplicate_group_size,
    duplicate_rank,
    removal_reason
  )

duplicates_removed <- bind_rows(
  missing_measurement_datetime_removed,
  consistency_preference_removed,
  duplicates_removed_key
) %>%
  arrange(
    station_code,
    measurement_datetime,
    consistency_level,
    removal_reason
  )

discharge_measurements <- bind_rows(
  complete_key_records %>% filter(duplicate_rank == 1),
  records_without_complete_key
) %>%
  select(
    station_code,
    measurement_datetime,
    consistency_level,
    last_update,
    stage_cm,
    discharge_m3s,
    wetted_area_m2,
    width_m,
    mean_depth_m,
    mean_velocity_ms,
    source_route,
    raw_file,
    downloaded_at,
    processed_at
  ) %>%
  arrange(station_code, measurement_datetime, consistency_level)

# Critical post-deduplication check
remaining_duplicates <- discharge_measurements %>%
  filter(
    !is.na(station_code),
    !is.na(measurement_datetime),
    !is.na(consistency_level)
  ) %>%
  count(station_code, measurement_datetime, consistency_level, name = "n") %>%
  filter(n > 1)

if (nrow(remaining_duplicates) > 0) {
  stop("Duplicated logical keys remain after deduplication.")
}

# Critical consistency-preference check
remaining_consistency_conflicts <- discharge_measurements %>%
  filter(
    !is.na(station_code),
    !is.na(measurement_datetime)
  ) %>%
  group_by(station_code, measurement_datetime) %>%
  summarise(
    has_consisted_record = any(consistency_level == 2, na.rm = TRUE),
    has_non_consisted_record = any(is.na(consistency_level) | consistency_level != 2),
    .groups = "drop"
  ) %>%
  filter(has_consisted_record, has_non_consisted_record)

if (nrow(remaining_consistency_conflicts) > 0) {
  stop("Non-consisted records remain where a consistency_level = 2 record exists.")
}

# Critical date and consistency checks
remaining_missing_measurement_datetime <- discharge_measurements %>%
  filter(is.na(measurement_datetime))

if (nrow(remaining_missing_measurement_datetime) > 0) {
  stop("Records without measurement_datetime remain after cleaning.")
}

remaining_missing_consistency_level <- discharge_measurements %>%
  filter(is.na(consistency_level))

if (nrow(remaining_missing_consistency_level) > 0) {
  stop("Records without consistency_level remain after cleaning.")
}

# QC summary
qc_summary <- tibble::tibble(
  processed_at = processed_at_value,
  n_json_files = nrow(requests_positive),
  n_raw_records = nrow(discharge_raw),
  n_final_records = nrow(discharge_measurements),
  n_missing_measurement_datetime_removed = nrow(missing_measurement_datetime_removed),
  n_missing_consistency_level_assigned_to_1 = n_missing_consistency_level_assigned_to_1,
  n_non_consisted_removed = nrow(consistency_preference_removed),
  n_duplicates_removed = nrow(duplicates_removed_key),
  n_total_records_removed = nrow(duplicates_removed),
  n_stations = n_distinct(discharge_measurements$station_code, na.rm = TRUE),
  first_measurement_datetime = safe_min_datetime(discharge_measurements$measurement_datetime),
  last_measurement_datetime = safe_max_datetime(discharge_measurements$measurement_datetime),
  n_missing_station_code = sum(is.na(discharge_measurements$station_code)),
  n_missing_measurement_datetime = sum(is.na(discharge_measurements$measurement_datetime)),
  n_missing_consistency_level = sum(is.na(discharge_measurements$consistency_level)),
  n_missing_discharge = sum(is.na(discharge_measurements$discharge_m3s)),
  n_negative_discharge = sum(discharge_measurements$discharge_m3s < 0, na.rm = TRUE),
  n_negative_stage = sum(discharge_measurements$stage_cm < 0, na.rm = TRUE),
  n_negative_width = sum(discharge_measurements$width_m < 0, na.rm = TRUE),
  n_negative_wetted_area = sum(discharge_measurements$wetted_area_m2 < 0, na.rm = TRUE),
  n_negative_mean_depth = sum(discharge_measurements$mean_depth_m < 0, na.rm = TRUE),
  n_negative_mean_velocity = sum(discharge_measurements$mean_velocity_ms < 0, na.rm = TRUE)
)

# Save outputs
arrow::write_parquet(discharge_measurements, output_file)

readr::write_csv(duplicates_removed, duplicates_file)
readr::write_csv(qc_summary, qc_summary_file)

# Console summary
message("Finished processing resumo_descarga discharge measurements.")
message("JSON files processed: ", nrow(requests_positive))
message("Raw records: ", nrow(discharge_raw))
message("Final records: ", nrow(discharge_measurements))
message("Records without measurement_datetime removed: ", nrow(missing_measurement_datetime_removed))
message("Records with missing consistency_level assigned to 1: ", n_missing_consistency_level_assigned_to_1)
message("Non-consisted records removed because a consistency_level = 2 record exists: ", nrow(consistency_preference_removed))
message("Duplicate logical-key records removed: ", nrow(duplicates_removed_key))
message("Total records removed: ", nrow(duplicates_removed))
message("Stations: ", qc_summary$n_stations)
message("Output: ", output_file)
message("Removed records file: ", duplicates_file)
message("QC summary file: ", qc_summary_file)
