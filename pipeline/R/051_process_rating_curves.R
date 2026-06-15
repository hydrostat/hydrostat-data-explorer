# ============================================================
# pipeline/R/051_process_rating_curves.R
#
# Process raw ANA HidroWebService rating-curve JSONs
# from the curva_descarga route.
#
# Output logic:
# - One row = one rating-curve segment.
# - A rating curve is a group of segments sharing:
#   station_code + valid_from + valid_to + consistency_level.
#
# Outputs:
# - data/processed/rating_curves.parquet
# - data/processed/rating_curve_summary.parquet
# - data/processed/rating_curves_duplicates_removed.csv
# - data/processed/rating_curves_qc_summary.csv
#
# Cleaning logic:
# - Remove records without measurement_datetime / valid_from.
# - If measurement_datetime exists but consistency_level is missing,
#   assign consistency_level = 1.
# - If records share station_code + measurement_datetime and at least
#   one record has consistency_level == 2, keep only consistency_level == 2.
# - Deduplicate only identical/equivalent curve segments, not whole curves.
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
raw_route_dir <- file.path("data", "raw", "discharge_routes", "route=curva_descarga")

output_file <- file.path("data", "processed", "rating_curves.parquet")
summary_file <- file.path("data", "processed", "rating_curve_summary.parquet")
duplicates_file <- file.path("data", "processed", "rating_curves_duplicates_removed.csv")
qc_summary_file <- file.path("data", "processed", "rating_curves_qc_summary.csv")

route_filter <- "curva_descarga"
source_route_value <- "/EstacoesTelemetricas/HidroSerieCurvaDescarga/v1"

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
parse_segment_number <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x[x %in% c("", "NA", "NaN", "null", "NULL")] <- NA_character_
  
  first_part <- sub("/.*$", "", x)
  suppressWarnings(as.integer(first_part))
}

parse_segments_reported <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x[x %in% c("", "NA", "NaN", "null", "NULL")] <- NA_character_
  
  has_slash <- grepl("/", x)
  out <- rep(NA_integer_, length(x))
  out[has_slash] <- suppressWarnings(as.integer(sub("^.*/", "", x[has_slash])))
  
  out
}

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

read_curva_file <- function(file_path, downloaded_at_value) {
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
      stringr::str_detect(route_text, "hidroseriecurvadescarga")
  )

if (nrow(requests_positive) == 0) {
  stop("No successful curva_descarga requests with n_items > 0 were found in the request log.")
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
  message("No raw-file column found in the request log. Using all JSON files in the curva_descarga directory.")
  
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
  raw_tables[[i]] <- read_curva_file(
    file_path = requests_positive$raw_file_path[i],
    downloaded_at_value = requests_positive$downloaded_at_raw[i]
  )
}

raw_items <- dplyr::bind_rows(raw_tables)

if (nrow(raw_items) == 0) {
  stop("No rating-curve records were read from the selected JSON files.")
}

# Standardize fields
raw_numero_curva <- as.character(
  pick_column(
    raw_items,
    c(
      "Numero_Curva",
      "NumeroCurva",
      "Num_Curva",
      "Curva_Numero",
      "Curva"
    )
  )
)

raw_tipo_curva <- as.character(
  pick_column(
    raw_items,
    c(
      "Tipo_Curva",
      "TipoCurva",
      "Tipo"
    )
  )
)

raw_tipo_equacao <- as.character(
  pick_column(
    raw_items,
    c(
      "Tipo_Equacao",
      "TipoEquacao",
      "Equacao_Tipo",
      "Tipo_Formula",
      "Formula"
    )
  )
)

rating_curves_raw <- tibble::tibble(
  station_code = standardize_station_code(
    pick_column(raw_items, c("codigoestacao", "CodigoEstacao", "Codigo_Estacao", "CodEstacao"))
  ),
  measurement_datetime = parse_datetime_api(
    pick_column(
      raw_items,
      c(
        "Periodo_Validade_Inicio",
        "PeriodoValidadeInicio",
        "Data_Validade_Inicio",
        "Data_Inicio",
        "Data_Hora_Dado",
        "Data_Hora_Medicao"
      )
    )
  ),
  valid_from = parse_datetime_api(
    pick_column(
      raw_items,
      c(
        "Periodo_Validade_Inicio",
        "PeriodoValidadeInicio",
        "Data_Validade_Inicio",
        "Data_Inicio",
        "Data_Hora_Dado",
        "Data_Hora_Medicao"
      )
    )
  ),
  valid_to = parse_datetime_api(
    pick_column(
      raw_items,
      c(
        "Periodo_Validade_Fim",
        "PeriodoValidadeFim",
        "Data_Validade_Fim",
        "Data_Fim",
        "Data_Final"
      )
    )
  ),
  consistency_level = parse_integer_simple(
    pick_column(raw_items, c("Nivel_Consistencia", "NivelConsistencia", "Consistencia"))
  ),
  last_update = parse_datetime_api(
    pick_column(
      raw_items,
      c(
        "Data_Ultima_Alteracao",
        "DataUltimaAlteracao",
        "Data_Atualizacao",
        "Data_Ultima_Atualizacao"
      )
    )
  ),
  segment_number_raw = raw_numero_curva,
  segment_number = parse_segment_number(raw_numero_curva),
  n_segments_reported = parse_segments_reported(raw_numero_curva),
  curve_type = raw_tipo_curva,
  equation_type = raw_tipo_equacao,
  stage_min_cm = parse_decimal(
    pick_column(
      raw_items,
      c(
        "Cota_Minima",
        "CotaMinima",
        "Limite_Inferior",
        "LimiteInferior",
        "Cota_Inferior",
        "CotaInicial",
        "Cota_Inicial",
        "Cota_De"
      )
    )
  ),
  stage_max_cm = parse_decimal(
    pick_column(
      raw_items,
      c(
        "Cota_Maxima",
        "CotaMaxima",
        "Limite_Superior",
        "LimiteSuperior",
        "Cota_Superior",
        "CotaFinal",
        "Cota_Final",
        "Cota_Ate"
      )
    )
  ),
  table_stage_step_cm = parse_decimal(
    pick_column(
      raw_items,
      c(
        "Intervalo_Cota",
        "IntervaloCota",
        "Intervalo_Cota_Tabela",
        "Incremento_Cota",
        "Passo_Cota"
      )
    )
  ),
  coefficient_a = parse_decimal(
    pick_column(
      raw_items,
      c(
        "Coeficiente_A",
        "CoeficienteA",
        "Coef_A",
        "CoefA",
        "A"
      )
    )
  ),
  coefficient_h0 = parse_decimal(
    pick_column(
      raw_items,
      c(
        "Coeficiente_H0",
        "CoeficienteH0",
        "Coef_H0",
        "CoefH0",
        "H0",
        "h0"
      )
    )
  ),
  coefficient_n = parse_decimal(
    pick_column(
      raw_items,
      c(
        "Coeficiente_N",
        "CoeficienteN",
        "Coef_N",
        "CoefN",
        "N",
        "n"
      )
    )
  ),
  source_route = source_route_value,
  raw_file = as.character(pick_column(raw_items, c("raw_file"))),
  downloaded_at = parse_datetime_api(
    pick_column(raw_items, c("downloaded_at"))
  ),
  processed_at = processed_at_value
)

# Remove records without measurement_datetime / valid_from
missing_measurement_datetime_removed <- rating_curves_raw %>%
  filter(is.na(measurement_datetime)) %>%
  mutate(
    removal_reason = "missing_measurement_datetime",
    duplicate_group_size = NA_integer_,
    duplicate_rank = NA_integer_
  )

rating_curves_step1 <- rating_curves_raw %>%
  filter(!is.na(measurement_datetime))

n_missing_consistency_level_assigned_to_1 <- sum(is.na(rating_curves_step1$consistency_level))

rating_curves_step1 <- rating_curves_step1 %>%
  mutate(
    consistency_level = if_else(is.na(consistency_level), 1L, consistency_level)
  )

# Apply consistency preference at curve-date level
#
# If records share station_code + measurement_datetime and at least one
# record is consisted, keep only the consisted records.
complete_datetime_records <- rating_curves_step1 %>%
  filter(!is.na(station_code), !is.na(measurement_datetime)) %>%
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
    valid_from,
    valid_to,
    consistency_level,
    last_update,
    segment_number_raw,
    segment_number,
    n_segments_reported,
    curve_type,
    equation_type,
    stage_min_cm,
    stage_max_cm,
    table_stage_step_cm,
    coefficient_a,
    coefficient_h0,
    coefficient_n,
    source_route,
    raw_file,
    downloaded_at,
    processed_at,
    duplicate_group_size,
    duplicate_rank,
    removal_reason
  )

records_without_complete_datetime_key <- rating_curves_step1 %>%
  filter(is.na(station_code) | is.na(measurement_datetime))

records_after_consistency_preference <- bind_rows(
  complete_datetime_records %>%
    filter(!consistency_preference_remove) %>%
    select(
      -datetime_group_size,
      -has_consisted_record,
      -consistency_preference_remove
    ),
  records_without_complete_datetime_key
)

# Create curve and segment IDs
#
# rating_curve_id groups all segments from the same rating curve.
# rating_curve_segment_id identifies each curve segment.
rating_curves_with_ids <- records_after_consistency_preference %>%
  mutate(
    rating_curve_id = make_id(
      station_code,
      valid_from,
      valid_to,
      consistency_level
    ),
    rating_curve_segment_id = make_id(
      station_code,
      valid_from,
      valid_to,
      consistency_level,
      segment_number_raw,
      stage_min_cm,
      stage_max_cm,
      curve_type,
      equation_type,
      table_stage_step_cm,
      coefficient_a,
      coefficient_h0,
      coefficient_n
    )
  )

# Deduplicate at segment level
#
# This preserves multiple segments from the same curve, such as:
# 01/03, 02/03, 03/03.
segment_records <- rating_curves_with_ids %>%
  arrange(
    rating_curve_segment_id,
    desc(last_update),
    desc(downloaded_at),
    raw_file
  ) %>%
  group_by(rating_curve_segment_id) %>%
  mutate(
    duplicate_group_size = n(),
    duplicate_rank = row_number()
  ) %>%
  ungroup()

duplicate_segments_removed <- segment_records %>%
  filter(duplicate_group_size > 1, duplicate_rank > 1) %>%
  mutate(
    removal_reason = "duplicate_rating_curve_segment_keep_latest_last_update"
  ) %>%
  select(
    rating_curve_id,
    rating_curve_segment_id,
    station_code,
    measurement_datetime,
    valid_from,
    valid_to,
    consistency_level,
    last_update,
    segment_number_raw,
    segment_number,
    n_segments_reported,
    curve_type,
    equation_type,
    stage_min_cm,
    stage_max_cm,
    table_stage_step_cm,
    coefficient_a,
    coefficient_h0,
    coefficient_n,
    source_route,
    raw_file,
    downloaded_at,
    processed_at,
    duplicate_group_size,
    duplicate_rank,
    removal_reason
  )

rating_curves <- segment_records %>%
  filter(duplicate_rank == 1) %>%
  select(
    rating_curve_id,
    rating_curve_segment_id,
    station_code,
    measurement_datetime,
    valid_from,
    valid_to,
    consistency_level,
    last_update,
    segment_number_raw,
    segment_number,
    n_segments_reported,
    curve_type,
    equation_type,
    stage_min_cm,
    stage_max_cm,
    table_stage_step_cm,
    coefficient_a,
    coefficient_h0,
    coefficient_n,
    source_route,
    raw_file,
    downloaded_at,
    processed_at
  ) %>%
  arrange(station_code, valid_from, valid_to, consistency_level, segment_number, stage_min_cm)

# Combine all removed records
rating_curves_removed <- bind_rows(
  missing_measurement_datetime_removed %>%
    mutate(
      rating_curve_id = NA_character_,
      rating_curve_segment_id = NA_character_
    ) %>%
    select(
      rating_curve_id,
      rating_curve_segment_id,
      station_code,
      measurement_datetime,
      valid_from,
      valid_to,
      consistency_level,
      last_update,
      segment_number_raw,
      segment_number,
      n_segments_reported,
      curve_type,
      equation_type,
      stage_min_cm,
      stage_max_cm,
      table_stage_step_cm,
      coefficient_a,
      coefficient_h0,
      coefficient_n,
      source_route,
      raw_file,
      downloaded_at,
      processed_at,
      duplicate_group_size,
      duplicate_rank,
      removal_reason
    ),
  consistency_preference_removed %>%
    mutate(
      rating_curve_id = make_id(
        station_code,
        valid_from,
        valid_to,
        consistency_level
      ),
      rating_curve_segment_id = make_id(
        station_code,
        valid_from,
        valid_to,
        consistency_level,
        segment_number_raw,
        stage_min_cm,
        stage_max_cm,
        curve_type,
        equation_type,
        table_stage_step_cm,
        coefficient_a,
        coefficient_h0,
        coefficient_n
      )
    ) %>%
    select(
      rating_curve_id,
      rating_curve_segment_id,
      station_code,
      measurement_datetime,
      valid_from,
      valid_to,
      consistency_level,
      last_update,
      segment_number_raw,
      segment_number,
      n_segments_reported,
      curve_type,
      equation_type,
      stage_min_cm,
      stage_max_cm,
      table_stage_step_cm,
      coefficient_a,
      coefficient_h0,
      coefficient_n,
      source_route,
      raw_file,
      downloaded_at,
      processed_at,
      duplicate_group_size,
      duplicate_rank,
      removal_reason
    ),
  duplicate_segments_removed
) %>%
  arrange(station_code, measurement_datetime, valid_to, consistency_level, segment_number, removal_reason)

# Critical checks
if (any(is.na(rating_curves$station_code))) {
  stop("Missing station_code remains in rating_curves.")
}

if (any(is.na(rating_curves$measurement_datetime))) {
  stop("Missing measurement_datetime remains in rating_curves.")
}

if (any(is.na(rating_curves$consistency_level))) {
  stop("Missing consistency_level remains in rating_curves.")
}

remaining_segment_duplicates <- rating_curves %>%
  count(rating_curve_segment_id, name = "n") %>%
  filter(n > 1)

if (nrow(remaining_segment_duplicates) > 0) {
  stop("Duplicated rating_curve_segment_id values remain after deduplication.")
}

remaining_consistency_conflicts <- rating_curves %>%
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
  stop("Non-consisted rating-curve segments remain where a consistency_level = 2 curve exists.")
}

# Rating-curve summary
rating_curve_summary <- rating_curves %>%
  group_by(
    rating_curve_id,
    station_code,
    valid_from,
    valid_to,
    consistency_level
  ) %>%
  summarise(
    n_segments = n(),
    n_distinct_segment_numbers = n_distinct(segment_number, na.rm = TRUE),
    n_segments_reported_max = suppressWarnings(max(n_segments_reported, na.rm = TRUE)),
    stage_min_cm = suppressWarnings(min(stage_min_cm, na.rm = TRUE)),
    stage_max_cm = suppressWarnings(max(stage_max_cm, na.rm = TRUE)),
    first_last_update = safe_min_datetime(last_update),
    last_last_update = safe_max_datetime(last_update),
    first_downloaded_at = safe_min_datetime(downloaded_at),
    last_downloaded_at = safe_max_datetime(downloaded_at),
    source_route = first(source_route),
    processed_at = first(processed_at),
    .groups = "drop"
  ) %>%
  mutate(
    n_segments_reported_max = replace(
      n_segments_reported_max,
      !is.finite(n_segments_reported_max),
      NA_real_
    ),
    n_segments_reported_max = as.integer(n_segments_reported_max),
    stage_min_cm = if_else(is.infinite(stage_min_cm), NA_real_, stage_min_cm),
    stage_max_cm = if_else(is.infinite(stage_max_cm), NA_real_, stage_max_cm)
  ) %>%
  arrange(station_code, valid_from, valid_to, consistency_level)

# QC summary
qc_summary <- tibble::tibble(
  processed_at = processed_at_value,
  n_json_files = nrow(requests_positive),
  n_raw_records = nrow(rating_curves_raw),
  n_missing_measurement_datetime_removed = nrow(missing_measurement_datetime_removed),
  n_missing_consistency_level_assigned_to_1 = n_missing_consistency_level_assigned_to_1,
  n_non_consisted_removed = nrow(consistency_preference_removed),
  n_duplicate_segments_removed = nrow(duplicate_segments_removed),
  n_total_records_removed = nrow(rating_curves_removed),
  n_final_curve_segments = nrow(rating_curves),
  n_final_rating_curves = nrow(rating_curve_summary),
  n_stations = n_distinct(rating_curves$station_code, na.rm = TRUE),
  first_valid_from = safe_min_datetime(rating_curves$valid_from),
  last_valid_from = safe_max_datetime(rating_curves$valid_from),
  n_missing_station_code = sum(is.na(rating_curves$station_code)),
  n_missing_measurement_datetime = sum(is.na(rating_curves$measurement_datetime)),
  n_missing_valid_to = sum(is.na(rating_curves$valid_to)),
  n_missing_consistency_level = sum(is.na(rating_curves$consistency_level)),
  n_missing_segment_number = sum(is.na(rating_curves$segment_number)),
  n_missing_stage_min_cm = sum(is.na(rating_curves$stage_min_cm)),
  n_missing_stage_max_cm = sum(is.na(rating_curves$stage_max_cm)),
  n_negative_stage_min_cm = sum(rating_curves$stage_min_cm < 0, na.rm = TRUE),
  n_negative_stage_max_cm = sum(rating_curves$stage_max_cm < 0, na.rm = TRUE),
  n_missing_coefficient_a = sum(is.na(rating_curves$coefficient_a)),
  n_missing_coefficient_h0 = sum(is.na(rating_curves$coefficient_h0)),
  n_missing_coefficient_n = sum(is.na(rating_curves$coefficient_n)),
  n_remaining_segment_duplicates = nrow(remaining_segment_duplicates),
  n_remaining_consistency_conflicts = nrow(remaining_consistency_conflicts)
)

# Save outputs
arrow::write_parquet(rating_curves, output_file)
arrow::write_parquet(rating_curve_summary, summary_file)

readr::write_csv(rating_curves_removed, duplicates_file)
readr::write_csv(qc_summary, qc_summary_file)

# Console summary
message("Finished processing curva_descarga rating curves.")
message("JSON files processed: ", nrow(requests_positive))
message("Raw records: ", nrow(rating_curves_raw))
message("Final curve segments: ", nrow(rating_curves))
message("Final rating curves: ", nrow(rating_curve_summary))
message("Missing measurement_datetime records removed: ", nrow(missing_measurement_datetime_removed))
message("Non-consisted records removed because a consistency_level = 2 curve exists: ", nrow(consistency_preference_removed))
message("Duplicate curve segments removed: ", nrow(duplicate_segments_removed))
message("Total records removed: ", nrow(rating_curves_removed))
message("Output: ", output_file)
message("Summary output: ", summary_file)
message("Removed records file: ", duplicates_file)
message("QC summary file: ", qc_summary_file)
