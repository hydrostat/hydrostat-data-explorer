# ============================================================
# 020_get_inventory_API.R
# Purpose: Download station inventory from ANA HidroWebService API
# Output: db_inventory_api
# ============================================================

source(file.path("pipeline", "R", "010_auth.R"), local = TRUE)

# ------------------------------------------------------------
# Paths
# ------------------------------------------------------------

raw_dir <- file.path("data", "raw")
processed_dir <- file.path("data", "processed")
log_dir <- "logs"

dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)

timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

raw_uf_path <- file.path(raw_dir, paste0("020_hidro_uf_", timestamp, ".json"))
raw_inventory_path <- file.path(raw_dir, paste0("020_inventory_api_all_ufs_", timestamp, ".json"))

db_inventory_api_path <- file.path(processed_dir, "db_inventory_api.parquet")
api_log_path <- file.path(log_dir, "api_log.csv")

# ------------------------------------------------------------
# Routes
# ------------------------------------------------------------

uf_route <- "/HidroUF/v1"
inventory_route <- "/HidroInventarioEstacoes/v1"

uf_url <- paste0(ana_base_url, uf_route)
inventory_url <- paste0(ana_base_url, inventory_route)

# ------------------------------------------------------------
# Get token
# ------------------------------------------------------------

token <- ana_get_token()

# ------------------------------------------------------------
# Download UF list from API
# ------------------------------------------------------------

uf_request_start <- Sys.time()

uf_resp <- httr2::request(uf_url) |>
  httr2::req_headers(
    accept = "*/*",
    Authorization = paste("Bearer", token)
  ) |>
  httr2::req_error(is_error = function(resp) FALSE) |>
  httr2::req_perform()

uf_request_end <- Sys.time()
uf_http_code <- httr2::resp_status(uf_resp)
uf_body_raw <- httr2::resp_body_string(uf_resp)

writeLines(uf_body_raw, raw_uf_path, useBytes = TRUE)

uf_body <- jsonlite::fromJSON(uf_body_raw, flatten = TRUE)

uf_api_code <- if (!is.null(uf_body$code)) uf_body$code else NA_integer_
uf_api_message <- if (!is.null(uf_body$message)) uf_body$message else NA_character_

uf_n_items <- if (!is.null(uf_body$items) && is.data.frame(uf_body$items)) {
  nrow(uf_body$items)
} else {
  0L
}

uf_success <- uf_http_code >= 200 && uf_http_code < 300 &&
  !is.null(uf_body$items) &&
  is.data.frame(uf_body$items) &&
  uf_n_items > 0

uf_error_message <- NA_character_

if (uf_http_code < 200 || uf_http_code >= 300) {
  uf_error_message <- paste("HTTP request failed with status", uf_http_code, "-", uf_api_message)
} else if (is.null(uf_body$items)) {
  uf_error_message <- "UF response does not contain items."
} else if (!is.data.frame(uf_body$items) || uf_n_items == 0) {
  uf_error_message <- "UF list is empty."
}

uf_log_row <- tibble::tibble(
  datetime_request = format(uf_request_start, "%Y-%m-%d %H:%M:%S"),
  route = uf_route,
  station_code = NA_character_,
  parameters = as.character("{}"),
  http_code = uf_http_code,
  api_code = uf_api_code,
  api_message = uf_api_message,
  n_items = uf_n_items,
  elapsed_seconds = as.numeric(difftime(uf_request_end, uf_request_start, units = "secs")),
  success = uf_success,
  error_message = uf_error_message
)

readr::write_csv(
  uf_log_row,
  api_log_path,
  append = file.exists(api_log_path)
)

if (!uf_success) {
  stop(uf_error_message)
}

ufs <- tibble::as_tibble(uf_body$items) |>
  dplyr::filter(!is.na(Estado_Codigo_IBGE)) |>
  dplyr::transmute(
    uf = Estado_Sigla,
    uf_name = Estado_Nome,
    uf_ibge_code = as.character(Estado_Codigo_IBGE),
    uf_ana_code = as.character(codigouf)
  ) |>
  dplyr::arrange(uf)

# ------------------------------------------------------------
# Download station inventory by UF
# ------------------------------------------------------------

inventory_raw_list <- list()
inventory_items_list <- list()
inventory_log_rows <- list()

for (i in seq_len(nrow(ufs))) {
  current_uf <- ufs$uf[i]

  message("Downloading API station inventory for UF: ", current_uf)

  inventory_params <- list(
    `Unidade Federativa` = current_uf
  )

  request_start <- Sys.time()

  req <- httr2::request(inventory_url) |>
    httr2::req_headers(
      accept = "*/*",
      Authorization = paste("Bearer", token)
    ) |>
    httr2::req_error(is_error = function(resp) FALSE)

  req <- do.call(
    httr2::req_url_query,
    c(list(req), inventory_params)
  )

  resp <- httr2::req_perform(req)

  request_end <- Sys.time()
  http_code <- httr2::resp_status(resp)
  body_raw <- httr2::resp_body_string(resp)

  body <- jsonlite::fromJSON(body_raw, flatten = TRUE)

  api_code <- if (!is.null(body$code)) body$code else NA_integer_
  api_message <- if (!is.null(body$message)) body$message else NA_character_

  n_items <- if (!is.null(body$items) && is.data.frame(body$items)) {
    nrow(body$items)
  } else {
    0L
  }

  success <- http_code >= 200 && http_code < 300 &&
    !is.null(body$items) &&
    is.data.frame(body$items) &&
    n_items > 0

  error_message <- NA_character_

  if (http_code < 200 || http_code >= 300) {
    error_message <- paste("HTTP request failed with status", http_code, "-", api_message)
  } else if (is.null(body$items)) {
    error_message <- "Response does not contain items."
  } else if (!is.data.frame(body$items) || n_items == 0) {
    error_message <- "Station inventory is empty."
  }

  inventory_log_rows[[i]] <- tibble::tibble(
    datetime_request = format(request_start, "%Y-%m-%d %H:%M:%S"),
    route = inventory_route,
    station_code = NA_character_,
    parameters = as.character(jsonlite::toJSON(inventory_params, auto_unbox = TRUE)),
    http_code = http_code,
    api_code = api_code,
    api_message = api_message,
    n_items = n_items,
    elapsed_seconds = as.numeric(difftime(request_end, request_start, units = "secs")),
    success = success,
    error_message = error_message
  )

  inventory_raw_list[[current_uf]] <- body

  if (http_code < 200 || http_code >= 300) {
    stop(error_message)
  }

  if (is.null(body$items)) {
    stop(error_message)
  }

  if (!is.data.frame(body$items) || nrow(body$items) == 0) {
    warning(error_message, " UF: ", current_uf)
    next
  }

  inventory_items_list[[current_uf]] <- tibble::as_tibble(body$items)
}

inventory_log <- dplyr::bind_rows(inventory_log_rows)

readr::write_csv(
  inventory_log,
  api_log_path,
  append = file.exists(api_log_path)
)

jsonlite::write_json(
  inventory_raw_list,
  raw_inventory_path,
  auto_unbox = TRUE,
  pretty = TRUE,
  null = "null"
)

inventory_items <- dplyr::bind_rows(
  inventory_items_list,
  .id = "download_uf"
)

if (nrow(inventory_items) == 0) {
  stop("No API station inventory records were returned.")
}

# ------------------------------------------------------------
# Parse API inventory
# ------------------------------------------------------------

db_inventory_api <- inventory_items |>
  dplyr::transmute(
    station_code = stringr::str_pad(trimws(as.character(codigoestacao)), width = 8, side = "left", pad = "0"),
    station_name = Estacao_Nome,
    station_type = Tipo_Estacao,
    uf = UF_Estacao,
    uf_name = UF_Nome_Estacao,
    municipality_code = as.character(Municipio_Codigo),
    municipality = Municipio_Nome,
    basin_code = as.character(codigobacia),
    basin_name = Bacia_Nome,
    latitude = readr::parse_number(Latitude),
    longitude = readr::parse_number(Longitude),
    altitude = readr::parse_number(Altitude),
    drainage_area = readr::parse_number(Area_Drenagem),
    operator_code = as.character(Operadora_Codigo),
    operator = Operadora_Sigla,
    responsible_agency = Responsavel_Sigla,
    is_operating = Operando == "1",
    telemetric_start_date = as.POSIXct(
      Data_Periodo_Telemetrica_Inicio,
      format = "%Y-%m-%d %H:%M:%OS",
      tz = "UTC"
    ),
    last_update = as.POSIXct(
      Data_Ultima_Atualizacao,
      format = "%Y-%m-%d %H:%M:%OS",
      tz = "UTC"
    ),
    download_uf = download_uf,
    source_api = TRUE,
    source_ws_telemetric = FALSE,
    source_ws_conventional = FALSE,
    source_priority = "api",
    source_route = inventory_route,
    downloaded_at = Sys.time()
  ) |>
  dplyr::filter(!is.na(station_code), station_code != "") |>
  dplyr::distinct(station_code, .keep_all = TRUE) |>
  dplyr::arrange(uf, station_code)

# ------------------------------------------------------------
# Save output
# ------------------------------------------------------------

arrow::write_parquet(db_inventory_api, db_inventory_api_path)

message("API inventory downloaded successfully.")
message("Raw UF JSON saved to: ", raw_uf_path)
message("Raw API inventory JSON saved to: ", raw_inventory_path)
message("Processed API inventory saved to: ", db_inventory_api_path)
message("Number of UFs requested: ", nrow(ufs))
message("Number of API stations: ", nrow(db_inventory_api))
