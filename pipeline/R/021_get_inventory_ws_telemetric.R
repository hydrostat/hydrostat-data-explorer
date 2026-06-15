# ============================================================
# 021_get_inventory_ws_telemetric.R
# Purpose: Download telemetric station inventory from old ANA webservice
# Output: db_inventory_ws_tel
# ============================================================

source(file.path("pipeline", "R", "000_setup.R"), local = TRUE)

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

raw_xml_path <- file.path(raw_dir, paste0("021_ws_telemetric_inventory_", timestamp, ".xml"))
db_inventory_ws_tel_path <- file.path(processed_dir, "db_inventory_ws_tel.parquet")
ws_log_path <- file.path(log_dir, "ws_inventory_log.csv")

# ------------------------------------------------------------
# Endpoint
# ------------------------------------------------------------

ws_route <- "/ServiceANA.asmx/ListaEstacoesTelemetricas"

ws_url <- paste0(
  "https://telemetriaws1.ana.gov.br",
  ws_route,
  "?statusEstacoes=&origem="
)

# ------------------------------------------------------------
# Download XML
# ------------------------------------------------------------

message("Downloading old webservice telemetric inventory...")

request_start <- Sys.time()

resp <- httr2::request(ws_url) |>
  httr2::req_headers(accept = "text/xml") |>
  httr2::req_timeout(600) |>
  httr2::req_error(is_error = function(resp) FALSE) |>
  httr2::req_perform()

request_end <- Sys.time()

http_code <- httr2::resp_status(resp)
xml_raw <- httr2::resp_body_string(resp)

writeLines(xml_raw, raw_xml_path, useBytes = TRUE)

success <- http_code >= 200 && http_code < 300
error_message <- if (success) NA_character_ else paste0("HTTP status ", http_code, ". ", substr(xml_raw, 1, 300))

log_row <- tibble::tibble(
  datetime_request = format(request_start, "%Y-%m-%d %H:%M:%S"),
  route = ws_route,
  parameters = "statusEstacoes=&origem=",
  http_code = http_code,
  elapsed_seconds = as.numeric(difftime(request_end, request_start, units = "secs")),
  response_chars = nchar(xml_raw),
  success = success,
  error_message = error_message,
  raw_xml_path = raw_xml_path
)

readr::write_csv(
  log_row,
  ws_log_path,
  append = file.exists(ws_log_path)
)

if (!success) {
  stop(error_message)
}

# ------------------------------------------------------------
# Parse XML
# ------------------------------------------------------------

doc <- XML::xmlParse(
  raw_xml_path,
  useInternalNodes = TRUE,
  encoding = "UTF-8"
)

table_nodes <- XML::getNodeSet(
  doc,
  "//*[local-name()='Table']"
)

if (length(table_nodes) == 0) {
  stop("No Table nodes found in telemetric XML.")
}

get_xml_value <- function(node, field_name) {
  value <- XML::getNodeSet(
    node,
    paste0("./*[local-name()='", field_name, "']")
  )
  
  if (length(value) == 0) {
    return(NA_character_)
  }
  
  trimws(XML::xmlValue(value[[1]]))
}

extract_field <- function(field_name) {
  vapply(
    table_nodes,
    get_xml_value,
    field_name = field_name,
    FUN.VALUE = character(1)
  )
}

db_inventory_ws_tel <- tibble::tibble(
  station_code = extract_field("CodEstacao"),
  station_name = extract_field("NomeEstacao"),
  basin_name = extract_field("Bacia"),
  sub_basin_code = extract_field("SubBacia"),
  operator = extract_field("Operadora"),
  responsible_agency = extract_field("Responsavel"),
  municipality_uf = extract_field("Municipio-UF"),
  latitude = extract_field("Latitude"),
  longitude = extract_field("Longitude"),
  altitude = extract_field("Altitude"),
  river_code = extract_field("CodRio"),
  river_name = extract_field("NomeRio"),
  origin = extract_field("Origem"),
  station_status = extract_field("StatusEstacao")
) |>
  dplyr::mutate(
    station_code = stringr::str_pad(
      trimws(as.character(station_code)),
      width = 8,
      side = "left",
      pad = "0"
    ),
    latitude = readr::parse_number(as.character(latitude)),
    longitude = readr::parse_number(as.character(longitude)),
    altitude = readr::parse_number(as.character(altitude)),
    source_api = FALSE,
    source_ws_telemetric = TRUE,
    source_ws_conventional = FALSE,
    source_priority = "ws_telemetric",
    source_route = ws_route,
    downloaded_at = Sys.time()
  ) |>
  dplyr::filter(!is.na(station_code), station_code != "") |>
  dplyr::distinct(station_code, .keep_all = TRUE) |>
  dplyr::arrange(station_code)

# ------------------------------------------------------------
# Save output
# ------------------------------------------------------------

arrow::write_parquet(db_inventory_ws_tel, db_inventory_ws_tel_path)

message("Old webservice telemetric inventory downloaded successfully.")
message("Raw XML saved to: ", raw_xml_path)
message("Processed telemetric inventory saved to: ", db_inventory_ws_tel_path)
message("Number of telemetric stations: ", nrow(db_inventory_ws_tel))
