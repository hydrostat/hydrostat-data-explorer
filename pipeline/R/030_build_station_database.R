# ============================================================
# pipeline/R/030_build_station_database.R
# Build the stable stations table and write it to DuckDB
# ============================================================

# Load packages and project setup
source(file.path("pipeline", "R", "000_setup.R"), local = TRUE)

library(arrow)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(DBI)
library(duckdb)

# Define paths
inventory_file <- file.path("data", "processed", "db_inventory_all.parquet")
inventory_summary_file <- file.path("data", "processed", "db_inventory_all_summary.csv")
inventory_added_file <- file.path("data", "processed", "db_inventory_added_from_ws.csv")

duckdb_file <- file.path("data", "ana_hidro.duckdb")

stations_summary_file <- file.path("data", "processed", "stations_summary.csv")
stations_by_uf_file <- file.path("data", "processed", "stations_by_uf.csv")
stations_source_summary_file <- file.path("data", "processed", "stations_source_summary.csv")
stations_temporal_availability_file <- file.path("data", "processed", "stations_temporal_availability_summary.csv")

# Critical input checks
input_files <- c(
  inventory_file,
  inventory_summary_file,
  inventory_added_file
)

missing_files <- input_files[!file.exists(input_files)]

if (length(missing_files) > 0) {
  stop(
    "Missing required input file(s):\n",
    paste(missing_files, collapse = "\n")
  )
}

# Read inputs
inventory_all <- arrow::read_parquet(inventory_file)
inventory_summary <- readr::read_csv(inventory_summary_file, show_col_types = FALSE)
inventory_added_from_ws <- readr::read_csv(inventory_added_file, show_col_types = FALSE)

# Check that the updated consolidated inventory contains the expected temporal fields
required_temporal_fields <- c(
  "discharge_start_date",
  "discharge_end_date",
  "telemetric_start_date",
  "telemetric_end_date",
  "stage_start_date",
  "stage_end_date",
  "rainfall_start_date",
  "rainfall_end_date"
)

missing_temporal_fields <- required_temporal_fields[
  !required_temporal_fields %in% names(inventory_all)
]

if (length(missing_temporal_fields) > 0) {
  stop(
    "Missing expected temporal availability field(s) in db_inventory_all.parquet:\n",
    paste(missing_temporal_fields, collapse = "\n")
  )
}

# Small helpers to handle column-name differences across sources
pick_chr <- function(data, candidate_names) {
  found <- candidate_names[candidate_names %in% names(data)]
  
  if (length(found) == 0) {
    return(rep(NA_character_, nrow(data)))
  }
  
  as.character(data[[found[1]]])
}

pick_num <- function(data, candidate_names) {
  found <- candidate_names[candidate_names %in% names(data)]
  
  if (length(found) == 0) {
    return(rep(NA_real_, nrow(data)))
  }
  
  value <- as.character(data[[found[1]]])
  value <- stringr::str_replace_all(value, ",", ".")
  suppressWarnings(as.numeric(value))
}

pick_lgl <- function(data, candidate_names) {
  found <- candidate_names[candidate_names %in% names(data)]
  
  if (length(found) == 0) {
    return(rep(FALSE, nrow(data)))
  }
  
  value <- data[[found[1]]]
  
  if (is.logical(value)) {
    return(value)
  }
  
  value_chr <- stringr::str_to_lower(as.character(value))
  
  value_chr %in% c("true", "t", "1", "yes", "sim", "s")
}

pick_date <- function(data, candidate_names) {
  found <- candidate_names[candidate_names %in% names(data)]
  
  if (length(found) == 0) {
    return(as.Date(rep(NA_character_, nrow(data))))
  }
  
  value <- as.character(data[[found[1]]])
  value <- stringr::str_squish(value)
  value[value %in% c("", "NA", "NaN", "NULL", "null")] <- NA_character_
  
  parsed <- suppressWarnings(
    lubridate::parse_date_time(
      value,
      orders = c("ymd HMS", "ymd HM", "ymd", "dmy HMS", "dmy HM", "dmy"),
      quiet = TRUE
    )
  )
  
  as.Date(parsed)
}

# Build stable stations table
stations <- tibble::tibble(
  station_code = pick_chr(inventory_all, c(
    "station_code",
    "codigoestacao",
    "Codigo",
    "CodEstacao",
    "Codigo_Estacao"
  )),
  station_name = pick_chr(inventory_all, c(
    "station_name",
    "Estacao_Nome",
    "NomeEstacao",
    "Nome",
    "Nome_Estacao"
  )),
  station_type = pick_chr(inventory_all, c(
    "station_type",
    "Tipo_Estacao",
    "TipoEstacao",
    "Tipo"
  )),
  uf = pick_chr(inventory_all, c(
    "uf",
    "UF_Estacao",
    "UF",
    "Estado_Sigla"
  )),
  uf_name = pick_chr(inventory_all, c(
    "uf_name",
    "UF_Nome_Estacao",
    "Estado_Nome",
    "NomeEstado"
  )),
  municipality_code = pick_chr(inventory_all, c(
    "municipality_code",
    "Municipio_Codigo",
    "CodigoMunicipio"
  )),
  municipality = pick_chr(inventory_all, c(
    "municipality",
    "Municipio_Nome",
    "Municipio",
    "municipality_uf"
  )),
  basin_code = pick_chr(inventory_all, c(
    "basin_code",
    "codigobacia",
    "Codigo_Bacia",
    "Bacia_Codigo"
  )),
  basin_name = pick_chr(inventory_all, c(
    "basin_name",
    "Bacia_Nome",
    "Bacia"
  )),
  sub_basin_code = pick_chr(inventory_all, c(
    "sub_basin_code",
    "SubBacia",
    "Sub_Bacia_Codigo"
  )),
  latitude = pick_num(inventory_all, c(
    "latitude",
    "Latitude",
    "lat"
  )),
  longitude = pick_num(inventory_all, c(
    "longitude",
    "Longitude",
    "lon",
    "long"
  )),
  altitude = pick_num(inventory_all, c(
    "altitude",
    "Altitude"
  )),
  drainage_area = pick_num(inventory_all, c(
    "drainage_area",
    "Area_Drenagem",
    "AreaDrenagem"
  )),
  operator_code = pick_chr(inventory_all, c(
    "operator_code",
    "Operadora_Codigo",
    "Codigo_Operadora_Unidade_UF"
  )),
  operator = pick_chr(inventory_all, c(
    "operator",
    "Operadora_Sigla",
    "Operadora"
  )),
  responsible_agency = pick_chr(inventory_all, c(
    "responsible_agency",
    "Responsavel_Sigla",
    "Responsavel"
  )),
  is_operating = pick_lgl(inventory_all, c(
    "is_operating",
    "Operando"
  )),
  discharge_start_date = pick_date(inventory_all, c(
    "discharge_start_date",
    "Data_Periodo_Desc_liquida_Inicio",
    "Data_Periodo_Desc_Liquida_Inicio",
    "PeriodoDescLiquidaInicio"
  )),
  discharge_end_date = pick_date(inventory_all, c(
    "discharge_end_date",
    "Data_Periodo_Desc_Liquida_Fim",
    "Data_Periodo_Desc_liquida_Fim",
    "PeriodoDescLiquidaFim"
  )),
  telemetric_start_date = pick_date(inventory_all, c(
    "telemetric_start_date",
    "Data_Periodo_Telemetrica_Inicio",
    "PeriodoTelemetricaInicio"
  )),
  telemetric_end_date = pick_date(inventory_all, c(
    "telemetric_end_date",
    "Data_Periodo_Telemetrica_Fim",
    "PeriodoTelemetricaFim"
  )),
  stage_start_date = pick_date(inventory_all, c(
    "stage_start_date",
    "Data_Periodo_Escala_Inicio",
    "PeriodoEscalaInicio"
  )),
  stage_end_date = pick_date(inventory_all, c(
    "stage_end_date",
    "Data_Periodo_Escala_Fim",
    "PeriodoEscalaFim"
  )),
  rainfall_start_date = pick_date(inventory_all, c(
    "rainfall_start_date",
    "Data_Periodo_Pluviometro_Inicio",
    "PeriodoPluviometroInicio"
  )),
  rainfall_end_date = pick_date(inventory_all, c(
    "rainfall_end_date",
    "Data_Periodo_Pluviometro_Fim",
    "PeriodoPluviometroFim"
  )),
  last_update = pick_date(inventory_all, c(
    "last_update",
    "Data_Ultima_Atualizacao"
  )),
  source_api = pick_lgl(inventory_all, c(
    "source_api"
  )),
  source_ws_telemetric = pick_lgl(inventory_all, c(
    "source_ws_telemetric"
  )),
  source_ws_conventional = pick_lgl(inventory_all, c(
    "source_ws_conventional"
  )),
  source_priority = pick_chr(inventory_all, c(
    "source_priority"
  )),
  source_route = pick_chr(inventory_all, c(
    "source_route"
  )),
  downloaded_at = pick_chr(inventory_all, c(
    "downloaded_at"
  ))
) |>
  mutate(
    station_code = stringr::str_trim(station_code),
    station_code = stringr::str_pad(station_code, width = 8, side = "left", pad = "0"),
    station_name = stringr::str_squish(station_name),
    station_type = stringr::str_squish(station_type),
    uf = stringr::str_to_upper(stringr::str_squish(uf)),
    uf_name = stringr::str_squish(uf_name),
    municipality = stringr::str_squish(municipality),
    basin_name = stringr::str_squish(basin_name),
    operator = stringr::str_squish(operator),
    responsible_agency = stringr::str_squish(responsible_agency),
    source_priority = stringr::str_squish(source_priority),
    has_discharge_measurements = !is.na(discharge_start_date),
    has_telemetry = !is.na(telemetric_start_date),
    has_stage_data = !is.na(stage_start_date),
    has_rainfall_data = !is.na(rainfall_start_date),
    missing_coordinates = is.na(latitude) | is.na(longitude)
  ) |>
  arrange(station_code)

# Critical station table checks
if (nrow(stations) == 0) {
  stop("The station table is empty.")
}

duplicate_station_codes <- stations |>
  count(station_code, name = "n") |>
  filter(n > 1)

if (nrow(duplicate_station_codes) > 0) {
  print(duplicate_station_codes)
  stop("Duplicated station_code values found. Fix duplicates before writing to DuckDB.")
}

missing_coordinate_rows <- stations |>
  filter(missing_coordinates)

if (nrow(missing_coordinate_rows) > 0) {
  print(missing_coordinate_rows |> select(station_code, station_name, uf, latitude, longitude) |> head(20))
  warning("Some stations have missing coordinates. Check stations_summary.csv.")
}

# Build supporting check outputs
stations_summary <- tibble::tibble(
  metric = c(
    "number_of_stations",
    "number_of_ufs",
    "number_missing_coordinates",
    "number_duplicate_station_codes",
    "number_added_from_legacy_webservices",
    "number_has_discharge_measurements",
    "number_has_telemetry",
    "number_has_stage_data",
    "number_has_rainfall_data"
  ),
  value = c(
    nrow(stations),
    stations |> filter(!is.na(uf), uf != "") |> distinct(uf) |> nrow(),
    sum(stations$missing_coordinates, na.rm = TRUE),
    nrow(duplicate_station_codes),
    nrow(inventory_added_from_ws),
    sum(stations$has_discharge_measurements, na.rm = TRUE),
    sum(stations$has_telemetry, na.rm = TRUE),
    sum(stations$has_stage_data, na.rm = TRUE),
    sum(stations$has_rainfall_data, na.rm = TRUE)
  )
)

stations_by_uf <- stations |>
  count(uf, name = "n_stations") |>
  arrange(uf)

stations_source_summary <- stations |>
  summarise(
    n_stations = n(),
    n_source_api = sum(source_api, na.rm = TRUE),
    n_source_ws_telemetric = sum(source_ws_telemetric, na.rm = TRUE),
    n_source_ws_conventional = sum(source_ws_conventional, na.rm = TRUE),
    n_source_priority_api = sum(source_priority == "api", na.rm = TRUE),
    n_source_priority_ws_telemetric = sum(source_priority == "ws_telemetric", na.rm = TRUE),
    n_source_priority_ws_conventional = sum(source_priority == "ws_conventional", na.rm = TRUE)
  )

stations_temporal_availability_summary <- tibble::tibble(
  data_type = c(
    "discharge_measurements",
    "telemetry",
    "stage",
    "rainfall"
  ),
  start_date_field = c(
    "discharge_start_date",
    "telemetric_start_date",
    "stage_start_date",
    "rainfall_start_date"
  ),
  end_date_field = c(
    "discharge_end_date",
    "telemetric_end_date",
    "stage_end_date",
    "rainfall_end_date"
  ),
  flag_field = c(
    "has_discharge_measurements",
    "has_telemetry",
    "has_stage_data",
    "has_rainfall_data"
  ),
  n_stations_with_start_date = c(
    sum(!is.na(stations$discharge_start_date)),
    sum(!is.na(stations$telemetric_start_date)),
    sum(!is.na(stations$stage_start_date)),
    sum(!is.na(stations$rainfall_start_date))
  ),
  n_stations_with_end_date = c(
    sum(!is.na(stations$discharge_end_date)),
    sum(!is.na(stations$telemetric_end_date)),
    sum(!is.na(stations$stage_end_date)),
    sum(!is.na(stations$rainfall_end_date))
  ),
  first_start_date = as.character(c(
    min(stations$discharge_start_date, na.rm = TRUE),
    min(stations$telemetric_start_date, na.rm = TRUE),
    min(stations$stage_start_date, na.rm = TRUE),
    min(stations$rainfall_start_date, na.rm = TRUE)
  )),
  last_end_date = as.character(c(
    max(stations$discharge_end_date, na.rm = TRUE),
    max(stations$telemetric_end_date, na.rm = TRUE),
    max(stations$stage_end_date, na.rm = TRUE),
    max(stations$rainfall_end_date, na.rm = TRUE)
  ))
) |>
  mutate(
    first_start_date = if_else(first_start_date == "Inf", NA_character_, first_start_date),
    last_end_date = if_else(last_end_date == "-Inf", NA_character_, last_end_date)
  )

# Build database metadata table
metadata <- tibble::tibble(
  key = c(
    "database_path",
    "database_created_or_updated_at",
    "schema_stage",
    "schema_version",
    "stations_source_file",
    "inventory_summary_file",
    "inventory_added_file",
    "n_stations",
    "n_ufs",
    "n_missing_coordinates",
    "n_duplicate_station_codes",
    "n_added_from_legacy_webservices",
    "station_integration_key",
    "stations_includes_temporal_availability_fields",
    "temporal_availability_fields",
    "temporal_availability_flags",
    "n_has_discharge_measurements",
    "n_has_telemetry",
    "n_has_stage_data",
    "n_has_rainfall_data",
    "stores_full_time_series",
    "planned_future_tables",
    "shiny_authenticated_download_model"
  ),
  value = c(
    duckdb_file,
    as.character(Sys.time()),
    "04_station_database",
    "0.2.0",
    inventory_file,
    inventory_summary_file,
    inventory_added_file,
    as.character(nrow(stations)),
    as.character(stations |> filter(!is.na(uf), uf != "") |> distinct(uf) |> nrow()),
    as.character(sum(stations$missing_coordinates, na.rm = TRUE)),
    as.character(nrow(duplicate_station_codes)),
    as.character(nrow(inventory_added_from_ws)),
    "station_code",
    "yes",
    paste(required_temporal_fields, collapse = "; "),
    "has_discharge_measurements; has_telemetry; has_stage_data; has_rainfall_data",
    as.character(sum(stations$has_discharge_measurements, na.rm = TRUE)),
    as.character(sum(stations$has_telemetry, na.rm = TRUE)),
    as.character(sum(stations$has_stage_data, na.rm = TRUE)),
    as.character(sum(stations$has_rainfall_data, na.rm = TRUE)),
    "no",
    "discharge_measurements_summary; rating_curves; rating_curve_points_or_parameters",
    "Authenticated ANA API downloads use credentials supplied by the user only for the active Shiny session; credentials, tokens, downloaded series, partial data, and reports are not persisted by the application."
  )
)

# Save supporting checks
readr::write_csv(stations_summary, stations_summary_file)
readr::write_csv(stations_by_uf, stations_by_uf_file)
readr::write_csv(stations_source_summary, stations_source_summary_file)
readr::write_csv(stations_temporal_availability_summary, stations_temporal_availability_file)

# Write stations and metadata to DuckDB
con <- NULL

tryCatch(
  {
    con <- DBI::dbConnect(duckdb::duckdb(), dbdir = duckdb_file, read_only = FALSE)
    
    DBI::dbWriteTable(
      conn = con,
      name = "stations",
      value = stations,
      overwrite = TRUE
    )
    
    DBI::dbWriteTable(
      conn = con,
      name = "metadata",
      value = metadata,
      overwrite = TRUE
    )
    
    db_n <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM stations")$n
    
    if (db_n != nrow(stations)) {
      stop("DuckDB write check failed: row count differs from station table.")
    }
  },
  error = function(e) {
    stop("Failed to connect/write to DuckDB: ", conditionMessage(e))
  },
  finally = {
    if (!is.null(con)) {
      DBI::dbDisconnect(con)
    }
  }
)

# Print final summary
print(stations_summary)
print(stations_source_summary)
print(stations_temporal_availability_summary)

message("Station database updated successfully.")
message("DuckDB file: ", duckdb_file)
message("DuckDB tables: stations, metadata")
message("Supporting files written to data/processed/")
