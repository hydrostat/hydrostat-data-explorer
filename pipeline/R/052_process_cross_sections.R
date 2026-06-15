# ============================================================
# pipeline/R/052_process_cross_sections.R
#
# Process raw ANA HidroWebService cross-section JSONs
# from the perfil_transversal route.
#
# Output logic:
# - One row in cross_section_vertices = one vertical/profile point.
# - One row in cross_sections = one cross-section survey/profile.
# - One row in cross_section_summary = one station-level cross-section summary.
#
# Outputs:
# - data/processed/cross_sections.parquet
# - data/processed/cross_section_vertices.parquet
# - data/processed/cross_section_summary.parquet
# - data/processed/cross_sections_duplicates_removed.csv
# - data/processed/cross_sections_qc_summary.csv
#
# Cleaning logic:
# - Use successful request-log entries for route = perfil_transversal and n_items > 0.
# - Remove records without measurement_datetime.
# - If measurement_datetime exists but consistency_level is missing,
#   assign consistency_level = 1.
# - If records share station_code + measurement_datetime and at least
#   one record has consistency_level == 2, keep only consistency_level == 2.
# - Deduplicate only identical/equivalent cross-section vertices, not whole sections.
#
# Notes:
# - This script does not write to DuckDB yet.
# - Cross-section cota values may be negative and are not removed.
# - Cross-section distances may be negative in some source records and are not removed.
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
request_log_file <- file.path("data", "processed", "cross_section_request_log.csv")

# Fallback for projects that keep all route requests in one common log.
if (!file.exists(request_log_file)) {
  request_log_file <- file.path("data", "processed", "discharge_request_log.csv")
}

raw_route_dir <- file.path("data", "raw", "discharge_cross_sections", "route=perfil_transversal")

# Fallback for older folder convention.
if (!dir.exists(raw_route_dir)) {
  raw_route_dir <- file.path("data", "raw", "discharge_routes", "route=perfil_transversal")
}

cross_sections_file <- file.path("data", "processed", "cross_sections.parquet")
cross_section_vertices_file <- file.path("data", "processed", "cross_section_vertices.parquet")
cross_section_summary_file <- file.path("data", "processed", "cross_section_summary.parquet")
duplicates_file <- file.path("data", "processed", "cross_sections_duplicates_removed.csv")
qc_summary_file <- file.path("data", "processed", "cross_sections_qc_summary.csv")

route_filter <- "perfil_transversal"
source_route_value <- "/EstacoesTelemetricas/HidroSeriePerfilTransversal/v1"

processed_at_value <- Sys.time()
project_root <- normalizePath(".", winslash = "/", mustWork = TRUE)

# If TRUE, positive log entries whose files are not available locally are skipped.
# This supports partial/incremental cross-section downloads.
allow_missing_positive_files <- TRUE

# Critical input checks
if (!file.exists(request_log_file)) {
  stop("Missing cross-section request log file: ", request_log_file)
}

if (!dir.exists(raw_route_dir)) {
  stop("Missing raw cross-section route directory: ", raw_route_dir)
}

# Helper functions
safe_min_numeric <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }

  min(x, na.rm = TRUE)
}

safe_max_numeric <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }

  max(x, na.rm = TRUE)
}

safe_max_integer <- function(x) {
  if (all(is.na(x))) {
    return(NA_integer_)
  }

  as.integer(max(x, na.rm = TRUE))
}

first_non_missing <- function(x) {
  y <- x[!is.na(x)]

  if (length(y) == 0) {
    return(NA)
  }

  y[[1]]
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
    file.path("data", "raw", "discharge_cross_sections", x),
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

read_cross_section_file <- function(file_path, downloaded_at_value) {
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
  out$source_item_index <- seq_len(nrow(out))

  out
}

# Read and standardize request log
request_log <- readr::read_csv(
  request_log_file,
  col_types = readr::cols(.default = readr::col_character()),
  show_col_types = FALSE
)

route_column <- find_column(request_log, c("route_name", "route", "source_route"))
endpoint_column <- find_column(request_log, c("endpoint", "api_endpoint"))
success_column <- find_column(request_log, c("success", "request_success", "ok"))
n_items_column <- find_column(request_log, c("n_items", "items_count", "number_of_items"))
raw_file_column <- find_column(request_log, c("raw_file", "raw_file_path", "file_path", "output_file", "response_file", "json_file"))
downloaded_at_column <- find_column(request_log, c("downloaded_at", "datetime_request", "request_datetime", "requested_at", "finished_at", "timestamp"))

if (is.na(success_column)) {
  stop("Could not identify a success column in the request log.")
}

if (is.na(n_items_column)) {
  stop("Could not identify an n_items column in the request log.")
}

request_log_std <- request_log %>%
  mutate(
    route_text = if (!is.na(route_column)) tolower(as.character(.data[[route_column]])) else "",
    endpoint_text = if (!is.na(endpoint_column)) tolower(as.character(.data[[endpoint_column]])) else "",
    route_text = if_else(is.na(route_text), "", route_text),
    endpoint_text = if_else(is.na(endpoint_text), "", endpoint_text),
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
      stringr::str_detect(endpoint_text, "hidroserieperfiltransversal") |
      stringr::str_detect(endpoint_text, "perfiltransversal")
  )

if (nrow(requests_positive) == 0) {
  stop("No successful perfil_transversal requests with n_items > 0 were found in the request log.")
}

# Locate JSON files
n_positive_log_entries <- nrow(requests_positive)
n_positive_files_unresolved <- 0L

if (!is.na(raw_file_column)) {
  requests_positive$raw_file_path <- vapply(
    requests_positive[[raw_file_column]],
    resolve_file_path,
    character(1)
  )

  unresolved_files <- requests_positive %>%
    filter(is.na(raw_file_path))

  n_positive_files_unresolved <- nrow(unresolved_files)

  if (n_positive_files_unresolved > 0 && !allow_missing_positive_files) {
    stop(
      "Some positive request-log entries could not be linked to JSON files. ",
      "First unresolved entry: ",
      unresolved_files[[raw_file_column]][1]
    )
  }

  if (n_positive_files_unresolved == nrow(requests_positive)) {
    stop("No positive request-log entries could be linked to existing JSON files.")
  }

  if (n_positive_files_unresolved > 0) {
    warning(
      n_positive_files_unresolved,
      " positive request-log entries could not be linked to existing JSON files and will be skipped."
    )
  }

  if (!is.na(downloaded_at_column)) {
    requests_positive$downloaded_at_raw <- requests_positive[[downloaded_at_column]]
  } else {
    requests_positive$downloaded_at_raw <- NA_character_
  }

  requests_positive <- requests_positive %>%
    filter(!is.na(raw_file_path)) %>%
    distinct(raw_file_path, .keep_all = TRUE)
} else {
  message("No raw-file column found in the request log. Using all JSON files in the perfil_transversal directory.")

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
  raw_tables[[i]] <- read_cross_section_file(
    file_path = requests_positive$raw_file_path[i],
    downloaded_at_value = requests_positive$downloaded_at_raw[i]
  )

  if (i %% 500 == 0 || i == nrow(requests_positive)) {
    message("Read cross-section JSON files: ", i, "/", nrow(requests_positive))
  }
}

raw_items <- dplyr::bind_rows(raw_tables)

if (nrow(raw_items) == 0) {
  stop("No cross-section records were read from the selected JSON files.")
}

# Standardize fields
cross_section_raw <- tibble::tibble(
  station_code = standardize_station_code(
    pick_column(raw_items, c("codigoestacao", "CodigoEstacao", "Codigo_Estacao", "CodEstacao", "EstacaoCodigo"))
  ),
  measurement_datetime = parse_datetime_api(
    pick_column(raw_items, c("Data_Hora_Medicao", "DataHoraMedicao", "Data_Hora_Dado", "Data"))
  ),
  consistency_level = parse_integer_simple(
    pick_column(raw_items, c("Nivel_Consistencia", "NivelConsistencia", "Consistencia"))
  ),
  last_update = parse_datetime_api(
    pick_column(raw_items, c("Data_Ultima_Alteracao", "DataUltimaAlteracao", "Data_Atualizacao", "Data_Ultima_Atualizacao"))
  ),
  survey_number = parse_integer_simple(
    pick_column(raw_items, c("Num_Levantamento", "NumLevantamento", "Numero_Levantamento", "NumeroLevantamento"))
  ),
  section_type = parse_integer_simple(
    pick_column(raw_items, c("Tipo_Secao", "TipoSecao", "Tipo_Seção"))
  ),
  source_record_id = as.character(
    pick_column(raw_items, c("Registro_ID", "RegistroID", "Registro_Id"))
  ),
  n_vertices_reported = parse_integer_simple(
    pick_column(raw_items, c("Num_Verticais", "NumVerticais", "Numero_Verticais", "NumeroVerticais"))
  ),
  distance_pipf_m = parse_decimal(
    pick_column(raw_items, c("Distancia_pipf", "DistanciaPIPF", "Distancia_PI_PF", "DistanciaPIPF_m"))
  ),
  x_distance_max_m = parse_decimal(
    pick_column(raw_items, c("Eixo_X_Dist_Maxima", "EixoXDistMaxima", "Eixo_X_Distancia_Maxima"))
  ),
  x_distance_min_m = parse_decimal(
    pick_column(raw_items, c("Eixo_X_Dist_Minima", "EixoXDistMinima", "Eixo_X_Distancia_Minima"))
  ),
  y_stage_max_cm = parse_decimal(
    pick_column(raw_items, c("Eixo_Y_Cota_Maxima", "EixoYCotaMaxima"))
  ),
  y_stage_min_cm = parse_decimal(
    pick_column(raw_items, c("Eixo_Y_Cota_Minima", "EixoYCotaMinima"))
  ),
  geometry_stage_step_cm = parse_decimal(
    pick_column(raw_items, c("Elm_Geom_Passo_Cota", "ElmGeomPassoCota", "Passo_Cota", "TabelaPassoCota"))
  ),
  observation = as.character(
    pick_column(raw_items, c("Observacoes", "Observações", "Observacao", "Observação"))
  ),
  vertex_distance_m = parse_decimal(
    pick_column(raw_items, c("Distancia", "Distância", "Distancia_m"))
  ),
  vertex_stage_cm = parse_decimal(
    pick_column(raw_items, c("Cota", "Cota_cm"))
  ),
  raw_verticais = as.character(
    pick_column(raw_items, c("verticais", "Verticais"))
  ),
  source_item_index = parse_integer_simple(
    pick_column(raw_items, c("source_item_index"))
  ),
  source_route = source_route_value,
  raw_file = as.character(pick_column(raw_items, c("raw_file"))),
  downloaded_at = parse_datetime_api(
    pick_column(raw_items, c("downloaded_at"))
  ),
  processed_at = processed_at_value
)

# Apply cleaning rules
#
# Rules:
# 1. Remove records without measurement_datetime.
# 2. If measurement_datetime exists but consistency_level is missing,
#    assign consistency_level = 1 and keep the record.
# 3. If records share station_code + measurement_datetime and at least
#    one record has consistency_level == 2, keep only consistency_level == 2.
# 4. Deduplicate vertices by cross_section_vertex_id.

n_missing_consistency_level_assigned_to_1 <- sum(
  is.na(cross_section_raw$consistency_level) &
    !is.na(cross_section_raw$measurement_datetime)
)

cross_section_cleaning_input <- cross_section_raw %>%
  mutate(
    consistency_level = if_else(
      !is.na(measurement_datetime) & is.na(consistency_level),
      1L,
      consistency_level
    )
  )

missing_measurement_datetime_removed <- cross_section_cleaning_input %>%
  filter(is.na(measurement_datetime)) %>%
  mutate(
    cross_section_id = NA_character_,
    cross_section_vertex_id = NA_character_,
    removal_reason = "missing_measurement_datetime",
    duplicate_group_size = NA_integer_,
    duplicate_rank = NA_integer_
  )

records_with_measurement_datetime <- cross_section_cleaning_input %>%
  filter(!is.na(measurement_datetime))

complete_datetime_records <- records_with_measurement_datetime %>%
  filter(!is.na(station_code), !is.na(measurement_datetime)) %>%
  group_by(station_code, measurement_datetime) %>%
  mutate(
    datetime_group_size = n(),
    has_consisted_record = any(consistency_level == 2, na.rm = TRUE),
    consistency_preference_remove = has_consisted_record &
      (is.na(consistency_level) | consistency_level != 2)
  ) %>%
  ungroup()

records_without_complete_datetime_key <- records_with_measurement_datetime %>%
  filter(is.na(station_code) | is.na(measurement_datetime))

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
    survey_number,
    section_type,
    source_record_id,
    n_vertices_reported,
    distance_pipf_m,
    x_distance_max_m,
    x_distance_min_m,
    y_stage_max_cm,
    y_stage_min_cm,
    geometry_stage_step_cm,
    observation,
    vertex_distance_m,
    vertex_stage_cm,
    raw_verticais,
    source_item_index,
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
      -datetime_group_size,
      -has_consisted_record,
      -consistency_preference_remove
    ),
  records_without_complete_datetime_key
)

# Create cross-section and vertex IDs
cross_section_with_ids <- records_after_consistency_preference %>%
  mutate(
    cross_section_id = make_id(
      station_code,
      measurement_datetime,
      consistency_level,
      survey_number,
      section_type,
      source_record_id
    ),
    vertex_geometry_key = if_else(
      !is.na(vertex_distance_m) | !is.na(vertex_stage_cm),
      make_id(vertex_distance_m, vertex_stage_cm),
      make_id("source_item", source_item_index)
    ),
    cross_section_vertex_id = make_id(
      cross_section_id,
      vertex_geometry_key
    )
  )

# Deduplicate at vertex level
vertex_records <- cross_section_with_ids %>%
  arrange(
    cross_section_vertex_id,
    desc(last_update),
    desc(downloaded_at),
    raw_file,
    source_item_index
  ) %>%
  group_by(cross_section_vertex_id) %>%
  mutate(
    duplicate_group_size = n(),
    duplicate_rank = row_number()
  ) %>%
  ungroup()

duplicate_vertices_removed <- vertex_records %>%
  filter(duplicate_group_size > 1, duplicate_rank > 1) %>%
  mutate(
    removal_reason = "duplicate_cross_section_vertex_keep_latest_last_update"
  ) %>%
  select(
    cross_section_id,
    cross_section_vertex_id,
    station_code,
    measurement_datetime,
    consistency_level,
    last_update,
    survey_number,
    section_type,
    source_record_id,
    n_vertices_reported,
    distance_pipf_m,
    x_distance_max_m,
    x_distance_min_m,
    y_stage_max_cm,
    y_stage_min_cm,
    geometry_stage_step_cm,
    observation,
    vertex_distance_m,
    vertex_stage_cm,
    raw_verticais,
    source_item_index,
    source_route,
    raw_file,
    downloaded_at,
    processed_at,
    duplicate_group_size,
    duplicate_rank,
    removal_reason
  )

cross_section_vertices <- vertex_records %>%
  filter(duplicate_rank == 1) %>%
  group_by(cross_section_id) %>%
  arrange(vertex_distance_m, vertex_stage_cm, source_item_index, .by_group = TRUE) %>%
  mutate(
    vertex_order = row_number()
  ) %>%
  ungroup() %>%
  select(
    cross_section_id,
    cross_section_vertex_id,
    station_code,
    measurement_datetime,
    consistency_level,
    last_update,
    survey_number,
    section_type,
    source_record_id,
    vertex_order,
    vertex_distance_m,
    vertex_stage_cm,
    n_vertices_reported,
    distance_pipf_m,
    x_distance_max_m,
    x_distance_min_m,
    y_stage_max_cm,
    y_stage_min_cm,
    geometry_stage_step_cm,
    observation,
    raw_verticais,
    source_item_index,
    source_route,
    raw_file,
    downloaded_at,
    processed_at
  ) %>%
  arrange(station_code, measurement_datetime, consistency_level, survey_number, section_type, vertex_order)

# Removed records
cross_sections_removed <- bind_rows(
  missing_measurement_datetime_removed %>%
    select(
      cross_section_id,
      cross_section_vertex_id,
      station_code,
      measurement_datetime,
      consistency_level,
      last_update,
      survey_number,
      section_type,
      source_record_id,
      n_vertices_reported,
      distance_pipf_m,
      x_distance_max_m,
      x_distance_min_m,
      y_stage_max_cm,
      y_stage_min_cm,
      geometry_stage_step_cm,
      observation,
      vertex_distance_m,
      vertex_stage_cm,
      raw_verticais,
      source_item_index,
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
      cross_section_id = make_id(
        station_code,
        measurement_datetime,
        consistency_level,
        survey_number,
        section_type,
        source_record_id
      ),
      vertex_geometry_key = if_else(
        !is.na(vertex_distance_m) | !is.na(vertex_stage_cm),
        make_id(vertex_distance_m, vertex_stage_cm),
        make_id("source_item", source_item_index)
      ),
      cross_section_vertex_id = make_id(
        cross_section_id,
        vertex_geometry_key
      )
    ) %>%
    select(
      cross_section_id,
      cross_section_vertex_id,
      station_code,
      measurement_datetime,
      consistency_level,
      last_update,
      survey_number,
      section_type,
      source_record_id,
      n_vertices_reported,
      distance_pipf_m,
      x_distance_max_m,
      x_distance_min_m,
      y_stage_max_cm,
      y_stage_min_cm,
      geometry_stage_step_cm,
      observation,
      vertex_distance_m,
      vertex_stage_cm,
      raw_verticais,
      source_item_index,
      source_route,
      raw_file,
      downloaded_at,
      processed_at,
      duplicate_group_size,
      duplicate_rank,
      removal_reason
    ),
  duplicate_vertices_removed
) %>%
  arrange(station_code, measurement_datetime, consistency_level, survey_number, section_type, vertex_order = source_item_index)

# Cross-section profile table
cross_sections <- cross_section_vertices %>%
  group_by(
    cross_section_id,
    station_code,
    measurement_datetime,
    consistency_level,
    survey_number,
    section_type,
    source_record_id
  ) %>%
  summarise(
    last_update = safe_max_datetime(last_update),
    n_vertices = n(),
    n_vertices_reported = safe_max_integer(n_vertices_reported),
    distance_pipf_m = first_non_missing(distance_pipf_m),
    x_distance_min_m = first_non_missing(x_distance_min_m),
    x_distance_max_m = first_non_missing(x_distance_max_m),
    y_stage_min_cm = first_non_missing(y_stage_min_cm),
    y_stage_max_cm = first_non_missing(y_stage_max_cm),
    geometry_stage_step_cm = first_non_missing(geometry_stage_step_cm),
    vertex_distance_min_m = safe_min_numeric(vertex_distance_m),
    vertex_distance_max_m = safe_max_numeric(vertex_distance_m),
    vertex_stage_min_cm = safe_min_numeric(vertex_stage_cm),
    vertex_stage_max_cm = safe_max_numeric(vertex_stage_cm),
    n_missing_vertex_distance = sum(is.na(vertex_distance_m)),
    n_missing_vertex_stage = sum(is.na(vertex_stage_cm)),
    observation = first_non_missing(observation),
    source_route = first(source_route),
    first_raw_file = first(raw_file),
    first_downloaded_at = safe_min_datetime(downloaded_at),
    last_downloaded_at = safe_max_datetime(downloaded_at),
    processed_at = first(processed_at),
    .groups = "drop"
  ) %>%
  arrange(station_code, measurement_datetime, consistency_level, survey_number, section_type)

# Station-level cross-section summary
cross_section_summary <- cross_sections %>%
  group_by(station_code) %>%
  summarise(
    n_cross_sections = n(),
    n_cross_section_vertices = sum(n_vertices, na.rm = TRUE),
    first_cross_section_datetime = safe_min_datetime(measurement_datetime),
    last_cross_section_datetime = safe_max_datetime(measurement_datetime),
    n_consistency_level_1 = sum(consistency_level == 1, na.rm = TRUE),
    n_consistency_level_2 = sum(consistency_level == 2, na.rm = TRUE),
    n_section_type_1 = sum(section_type == 1, na.rm = TRUE),
    n_section_type_2 = sum(section_type == 2, na.rm = TRUE),
    min_vertex_distance_m = safe_min_numeric(vertex_distance_min_m),
    max_vertex_distance_m = safe_max_numeric(vertex_distance_max_m),
    min_vertex_stage_cm = safe_min_numeric(vertex_stage_min_cm),
    max_vertex_stage_cm = safe_max_numeric(vertex_stage_max_cm),
    first_downloaded_at = safe_min_datetime(first_downloaded_at),
    last_downloaded_at = safe_max_datetime(last_downloaded_at),
    processed_at = first(processed_at),
    .groups = "drop"
  ) %>%
  arrange(station_code)

# Critical checks
if (any(is.na(cross_section_vertices$station_code))) {
  stop("Missing station_code remains in cross_section_vertices.")
}

if (any(is.na(cross_section_vertices$measurement_datetime))) {
  stop("Missing measurement_datetime remains in cross_section_vertices.")
}

if (any(is.na(cross_section_vertices$consistency_level))) {
  stop("Missing consistency_level remains in cross_section_vertices.")
}

remaining_vertex_duplicates <- cross_section_vertices %>%
  count(cross_section_vertex_id, name = "n") %>%
  filter(n > 1)

if (nrow(remaining_vertex_duplicates) > 0) {
  stop("Duplicated cross_section_vertex_id values remain after deduplication.")
}

remaining_section_duplicates <- cross_sections %>%
  count(cross_section_id, name = "n") %>%
  filter(n > 1)

if (nrow(remaining_section_duplicates) > 0) {
  stop("Duplicated cross_section_id values remain after profile summarization.")
}

missing_section_ids <- setdiff(
  unique(cross_section_vertices$cross_section_id),
  unique(cross_sections$cross_section_id)
)

if (length(missing_section_ids) > 0) {
  stop("Some cross_section_id values in cross_section_vertices are missing from cross_sections.")
}

remaining_consistency_conflicts <- cross_section_vertices %>%
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
  stop("Non-consisted cross-section vertices remain where a consistency_level = 2 section exists.")
}

# QC summary
qc_summary <- tibble::tibble(
  processed_at = processed_at_value,
  request_log_file = request_log_file,
  raw_route_dir = raw_route_dir,
  n_positive_log_entries = n_positive_log_entries,
  n_positive_files_selected = nrow(requests_positive),
  n_positive_files_unresolved = n_positive_files_unresolved,
  n_raw_vertex_records = nrow(cross_section_raw),
  n_missing_measurement_datetime_removed = nrow(missing_measurement_datetime_removed),
  n_missing_consistency_level_assigned_to_1 = n_missing_consistency_level_assigned_to_1,
  n_non_consisted_removed = nrow(consistency_preference_removed),
  n_duplicate_vertices_removed = nrow(duplicate_vertices_removed),
  n_total_records_removed = nrow(cross_sections_removed),
  n_final_cross_section_vertices = nrow(cross_section_vertices),
  n_final_cross_sections = nrow(cross_sections),
  n_final_station_summaries = nrow(cross_section_summary),
  n_stations = n_distinct(cross_section_vertices$station_code, na.rm = TRUE),
  first_measurement_datetime = safe_min_datetime(cross_section_vertices$measurement_datetime),
  last_measurement_datetime = safe_max_datetime(cross_section_vertices$measurement_datetime),
  n_missing_station_code = sum(is.na(cross_section_vertices$station_code)),
  n_missing_measurement_datetime = sum(is.na(cross_section_vertices$measurement_datetime)),
  n_missing_consistency_level = sum(is.na(cross_section_vertices$consistency_level)),
  n_missing_survey_number = sum(is.na(cross_section_vertices$survey_number)),
  n_missing_section_type = sum(is.na(cross_section_vertices$section_type)),
  n_missing_source_record_id = sum(is.na(cross_section_vertices$source_record_id)),
  n_missing_vertex_distance_m = sum(is.na(cross_section_vertices$vertex_distance_m)),
  n_missing_vertex_stage_cm = sum(is.na(cross_section_vertices$vertex_stage_cm)),
  n_negative_vertex_distance_m = sum(cross_section_vertices$vertex_distance_m < 0, na.rm = TRUE),
  n_negative_vertex_stage_cm = sum(cross_section_vertices$vertex_stage_cm < 0, na.rm = TRUE),
  n_missing_distance_pipf_m = sum(is.na(cross_section_vertices$distance_pipf_m)),
  n_missing_n_vertices_reported = sum(is.na(cross_section_vertices$n_vertices_reported)),
  n_remaining_vertex_duplicates = nrow(remaining_vertex_duplicates),
  n_remaining_section_duplicates = nrow(remaining_section_duplicates),
  n_remaining_consistency_conflicts = nrow(remaining_consistency_conflicts)
)

# Save outputs
arrow::write_parquet(cross_sections, cross_sections_file)
arrow::write_parquet(cross_section_vertices, cross_section_vertices_file)
arrow::write_parquet(cross_section_summary, cross_section_summary_file)

readr::write_csv(cross_sections_removed, duplicates_file)
readr::write_csv(qc_summary, qc_summary_file)

# Console summary
message("Finished processing perfil_transversal cross sections.")
message("Request log: ", request_log_file)
message("Raw route directory: ", raw_route_dir)
message("Positive request-log entries: ", n_positive_log_entries)
message("Positive JSON files selected: ", nrow(requests_positive))
message("Positive JSON files unresolved/skipped: ", n_positive_files_unresolved)
message("Raw vertex records: ", nrow(cross_section_raw))
message("Final cross-section vertices: ", nrow(cross_section_vertices))
message("Final cross sections: ", nrow(cross_sections))
message("Station summaries: ", nrow(cross_section_summary))
message("Missing measurement_datetime records removed: ", nrow(missing_measurement_datetime_removed))
message("Non-consisted records removed because a consistency_level = 2 section exists: ", nrow(consistency_preference_removed))
message("Duplicate vertices removed: ", nrow(duplicate_vertices_removed))
message("Total records removed: ", nrow(cross_sections_removed))
message("Cross sections output: ", cross_sections_file)
message("Cross-section vertices output: ", cross_section_vertices_file)
message("Cross-section summary output: ", cross_section_summary_file)
message("Removed records file: ", duplicates_file)
message("QC summary file: ", qc_summary_file)
