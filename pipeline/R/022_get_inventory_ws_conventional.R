# ============================================================
# 022_get_inventory_ws_conventional.R
# Purpose: Download conventional station inventory from old ANA webservice by state
# Output: db_inventory_ws_con
# ============================================================

source(file.path("pipeline", "R", "000_setup.R"), local = TRUE)

# ------------------------------------------------------------
# Paths
# ------------------------------------------------------------

raw_dir <- file.path("data", "raw", "022_ws_conventional_by_state")
processed_dir <- file.path("data", "processed")
log_dir <- "logs"

dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)

timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

db_inventory_ws_con_path <- file.path(processed_dir, "db_inventory_ws_con.parquet")
ws_con_log_path <- file.path(log_dir, paste0("022_ws_conventional_download_log_", timestamp, ".csv"))

# ------------------------------------------------------------
# Endpoint and state list
# ------------------------------------------------------------

ws_route <- "/ServiceANA.asmx/HidroInventario"

ws_url <- paste0(
  "https://telemetriaws1.ana.gov.br",
  ws_route
)

states <- tibble::tribble(
  ~uf, ~state_name,
  "AC", "ACRE",
  "AL", "ALAGOAS",
  "AP", "AMAPÁ",
  "AM", "AMAZONAS",
  "BA", "BAHIA",
  "CE", "CEARÁ",
  "DF", "DISTRITO FEDERAL",
  "ES", "ESPÍRITO SANTO",
  "GO", "GOIÁS",
  "MA", "MARANHÃO",
  "MT", "MATO GROSSO",
  "MS", "MATO GROSSO DO SUL",
  "MG", "MINAS GERAIS",
  "PA", "PARÁ",
  "PB", "PARAÍBA",
  "PR", "PARANÁ",
  "PE", "PERNAMBUCO",
  "PI", "PIAUÍ",
  "RJ", "RIO DE JANEIRO",
  "RN", "RIO GRANDE DO NORTE",
  "RS", "RIO GRANDE DO SUL",
  "RO", "RONDÔNIA",
  "RR", "RORAIMA",
  "SC", "SANTA CATARINA",
  "SP", "SÃO PAULO",
  "SE", "SERGIPE",
  "TO", "TOCANTINS"
)

# ------------------------------------------------------------
# Small XML helper
# ------------------------------------------------------------

get_first_value <- function(node, field_name) {
  value <- XML::xpathSApply(
    node,
    paste0("./*[local-name()='", field_name, "']"),
    XML::xmlValue
  )

  if (length(value) == 0) {
    return(NA_character_)
  }

  value[1]
}

# ------------------------------------------------------------
# Download and parse inventory by state
# ------------------------------------------------------------

inventory_list <- list()
log_list <- list()

for (i in seq_len(nrow(states))) {
  current_uf <- states$uf[i]
  current_state <- states$state_name[i]

  message("Requesting old HidroInventario for ", current_uf, " - ", current_state)

  raw_xml_path <- file.path(
    raw_dir,
    paste0("old_ws_hidro_inventario_", current_uf, "_", timestamp, ".xml")
  )

  request_start <- Sys.time()

  resp <- httr2::request(ws_url) |>
    httr2::req_method("POST") |>
    httr2::req_headers(
      `Content-Type` = "application/x-www-form-urlencoded",
      accept = "text/xml"
    ) |>
    httr2::req_body_form(
      codEstDE = "",
      codEstATE = "",
      tpEst = "",
      nmEst = "",
      nmRio = "",
      codSubBacia = "",
      codBacia = "",
      nmMunicipio = "",
      nmEstado = current_state,
      sgResp = "",
      sgOper = "",
      telemetrica = ""
    ) |>
    httr2::req_timeout(900) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform()

  request_end <- Sys.time()

  http_code <- httr2::resp_status(resp)
  xml_raw <- httr2::resp_body_string(resp)

  writeLines(xml_raw, raw_xml_path, useBytes = TRUE)

  n_table_nodes <- NA_integer_
  n_items <- 0L
  success <- FALSE
  error_message <- NA_character_

  if (http_code >= 200 && http_code < 300) {
    doc <- XML::xmlParse(
      raw_xml_path,
      useInternalNodes = TRUE,
      encoding = "UTF-8"
    )

    table_nodes <- XML::getNodeSet(
      doc,
      "//*[local-name()='Table']"
    )

    n_table_nodes <- length(table_nodes)

    if (n_table_nodes > 0) {
      current_inventory <- dplyr::bind_rows(lapply(table_nodes, function(node) {
        tibble::tibble(
          station_code = get_first_value(node, "Codigo"),
          station_name = get_first_value(node, "Nome"),
          station_type = get_first_value(node, "TipoEstacao"),
          uf = current_uf,
          uf_name = get_first_value(node, "nmEstado"),
          municipality_code = get_first_value(node, "MunicipioCodigo"),
          municipality = get_first_value(node, "nmMunicipio"),
          basin_code = get_first_value(node, "BaciaCodigo"),
          sub_basin_code = get_first_value(node, "SubBaciaCodigo"),
          river_code = get_first_value(node, "RioCodigo"),
          river_name = get_first_value(node, "RioNome"),
          latitude = get_first_value(node, "Latitude"),
          longitude = get_first_value(node, "Longitude"),
          altitude = get_first_value(node, "Altitude"),
          drainage_area = get_first_value(node, "AreaDrenagem"),
          operator_code = get_first_value(node, "OperadoraCodigo"),
          operator = get_first_value(node, "OperadoraSigla"),
          responsible_agency = get_first_value(node, "ResponsavelSigla"),
          is_operating_raw = get_first_value(node, "Operando"),
          telemetric_raw = get_first_value(node, "TipoEstacaoTelemetrica"),
          
          discharge_start_date_raw = get_first_value(node, "PeriodoDescLiquidaInicio"),
          discharge_end_date_raw = get_first_value(node, "PeriodoDescLiquidaFim"),
          telemetric_start_date_raw = get_first_value(node, "PeriodoTelemetricaInicio"),
          telemetric_end_date_raw = get_first_value(node, "PeriodoTelemetricaFim"),
          stage_start_date_raw = get_first_value(node, "PeriodoEscalaInicio"),
          stage_end_date_raw = get_first_value(node, "PeriodoEscalaFim"),
          rainfall_start_date_raw = get_first_value(node, "PeriodoPluviometroInicio"),
          rainfall_end_date_raw = get_first_value(node, "PeriodoPluviometroFim"),
          
          last_update_raw = get_first_value(node, "UltimaAtualizacao")
        )
      })) |>
        dplyr::mutate(
          station_code = stringr::str_pad(trimws(as.character(station_code)), width = 8, side = "left", pad = "0"),
          latitude = readr::parse_number(as.character(latitude)),
          longitude = readr::parse_number(as.character(longitude)),
          altitude = readr::parse_number(as.character(altitude)),
          drainage_area = readr::parse_number(as.character(drainage_area)),
          is_operating = is_operating_raw %in% c("1", "Sim", "SIM", "S"),
          source_api = FALSE,
          source_ws_telemetric = FALSE,
          source_ws_conventional = TRUE,
          source_priority = "ws_conventional",
          source_route = ws_route,
          downloaded_at = Sys.time()
        ) |>
        dplyr::filter(!is.na(station_code), station_code != "") |>
        dplyr::distinct(station_code, .keep_all = TRUE)

      n_items <- nrow(current_inventory)
      inventory_list[[current_uf]] <- current_inventory
    }

    success <- TRUE

  } else {
    error_message <- paste0(
      "HTTP status ",
      http_code,
      ". First response characters: ",
      substr(xml_raw, 1, 300)
    )

    warning("Request failed for ", current_uf, ": ", error_message)
  }

  log_list[[current_uf]] <- tibble::tibble(
    datetime_request = format(request_start, "%Y-%m-%d %H:%M:%S"),
    route = ws_route,
    uf = current_uf,
    state_name = current_state,
    parameters = paste0("nmEstado=", current_state),
    http_code = http_code,
    elapsed_seconds = as.numeric(difftime(request_end, request_start, units = "secs")),
    response_chars = nchar(xml_raw),
    n_table_nodes = n_table_nodes,
    n_items = n_items,
    success = success,
    error_message = error_message,
    raw_xml_path = raw_xml_path
  )

  message("  HTTP: ", http_code, " | items: ", n_items)
}

# ------------------------------------------------------------
# Save output
# ------------------------------------------------------------

db_inventory_ws_con <- dplyr::bind_rows(inventory_list) |>
  dplyr::distinct(station_code, .keep_all = TRUE) |>
  dplyr::arrange(uf, station_code)

download_log <- dplyr::bind_rows(log_list)

if (nrow(db_inventory_ws_con) == 0) {
  stop("No conventional inventory records were returned.")
}

arrow::write_parquet(db_inventory_ws_con, db_inventory_ws_con_path)
readr::write_csv(download_log, ws_con_log_path)

message("Old webservice conventional inventory downloaded successfully.")
message("Processed conventional inventory saved to: ", db_inventory_ws_con_path)
message("Download log saved to: ", ws_con_log_path)
message("Number of conventional stations: ", nrow(db_inventory_ws_con))

print(download_log |>
  dplyr::select(uf, http_code, elapsed_seconds, n_items, success))
