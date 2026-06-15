# ============================================================
# 023_consolidate_inventory.R
# Purpose: Consolidate API and old webservice inventories
# Output: db_inventory_all
# ============================================================

source(file.path("pipeline", "R", "000_setup.R"), local = TRUE)

# ------------------------------------------------------------
# Paths
# ------------------------------------------------------------

processed_dir <- file.path("data", "processed")
dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)

db_inventory_api_path <- file.path(processed_dir, "db_inventory_api.parquet")
db_inventory_ws_tel_path <- file.path(processed_dir, "db_inventory_ws_tel.parquet")
db_inventory_ws_con_path <- file.path(processed_dir, "db_inventory_ws_con.parquet")

db_inventory_all_path <- file.path(processed_dir, "db_inventory_all.parquet")
inventory_summary_path <- file.path(processed_dir, "db_inventory_all_summary.csv")
inventory_added_from_ws_path <- file.path(processed_dir, "db_inventory_added_from_ws.csv")
inventory_temporal_summary_path <- file.path(processed_dir, "db_inventory_temporal_availability_summary.csv")

# ------------------------------------------------------------
# Helper functions
# ------------------------------------------------------------

parse_inventory_datetime <- function(x) {
  if (inherits(x, "POSIXct")) {
    return(x)
  }
  
  x <- trimws(as.character(x))
  x[x == ""] <- NA_character_
  x <- sub("\\.0$", "", x)
  
  out <- as.POSIXct(x, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  missing_full_datetime <- is.na(out) & !is.na(x)
  
  if (any(missing_full_datetime)) {
    out[missing_full_datetime] <- as.POSIXct(
      as.Date(x[missing_full_datetime], format = "%Y-%m-%d"),
      tz = "UTC"
    )
  }
  
  out
}

get_first_existing_column <- function(data, candidates) {
  existing <- candidates[candidates %in% names(data)]
  
  if (length(existing) == 0) {
    return(rep(NA_character_, nrow(data)))
  }
  
  data[[existing[1]]]
}

get_column_or_na <- function(data, column_name, type = "character") {
  if (column_name %in% names(data)) {
    return(data[[column_name]])
  }
  
  if (type == "numeric") {
    return(rep(NA_real_, nrow(data)))
  }
  
  if (type == "logical") {
    return(rep(NA, nrow(data)))
  }
  
  if (type == "datetime") {
    return(as.POSIXct(rep(NA_character_, nrow(data)), tz = "UTC"))
  }
  
  rep(NA_character_, nrow(data))
}

add_missing_columns <- function(data, target_columns) {
  missing_columns <- setdiff(target_columns, names(data))
  
  for (column_name in missing_columns) {
    data[[column_name]] <- NA
  }
  
  data |>
    dplyr::select(dplyr::all_of(target_columns))
}

add_temporal_availability_fields <- function(data) {
  data |>
    dplyr::mutate(
      discharge_start_date = parse_inventory_datetime(
        get_first_existing_column(
          dplyr::pick(dplyr::everything()),
          c(
            "discharge_start_date",
            "discharge_start_date_raw",
            "PeriodoDescLiquidaInicio",
            "Data_Periodo_Desc_Liquida_Inicio",
            "Data_Periodo_Desc_liquida_Inicio"
          )
        )
      ),
      discharge_end_date = parse_inventory_datetime(
        get_first_existing_column(
          dplyr::pick(dplyr::everything()),
          c(
            "discharge_end_date",
            "discharge_end_date_raw",
            "PeriodoDescLiquidaFim",
            "Data_Periodo_Desc_Liquida_Fim",
            "Data_Periodo_Desc_liquida_Fim"
          )
        )
      ),
      telemetric_start_date = parse_inventory_datetime(
        get_first_existing_column(
          dplyr::pick(dplyr::everything()),
          c(
            "telemetric_start_date",
            "telemetric_start_date_raw",
            "PeriodoTelemetricaInicio",
            "Data_Periodo_Telemetrica_Inicio"
          )
        )
      ),
      telemetric_end_date = parse_inventory_datetime(
        get_first_existing_column(
          dplyr::pick(dplyr::everything()),
          c(
            "telemetric_end_date",
            "telemetric_end_date_raw",
            "PeriodoTelemetricaFim",
            "Data_Periodo_Telemetrica_Fim"
          )
        )
      ),
      stage_start_date = parse_inventory_datetime(
        get_first_existing_column(
          dplyr::pick(dplyr::everything()),
          c(
            "stage_start_date",
            "stage_start_date_raw",
            "PeriodoEscalaInicio",
            "Data_Periodo_Escala_Inicio"
          )
        )
      ),
      stage_end_date = parse_inventory_datetime(
        get_first_existing_column(
          dplyr::pick(dplyr::everything()),
          c(
            "stage_end_date",
            "stage_end_date_raw",
            "PeriodoEscalaFim",
            "Data_Periodo_Escala_Fim"
          )
        )
      ),
      rainfall_start_date = parse_inventory_datetime(
        get_first_existing_column(
          dplyr::pick(dplyr::everything()),
          c(
            "rainfall_start_date",
            "rainfall_start_date_raw",
            "PeriodoPluviometroInicio",
            "Data_Periodo_Pluviometro_Inicio"
          )
        )
      ),
      rainfall_end_date = parse_inventory_datetime(
        get_first_existing_column(
          dplyr::pick(dplyr::everything()),
          c(
            "rainfall_end_date",
            "rainfall_end_date_raw",
            "PeriodoPluviometroFim",
            "Data_Periodo_Pluviometro_Fim"
          )
        )
      )
    )
}

# ------------------------------------------------------------
# Read source inventories
# ------------------------------------------------------------

if (!file.exists(db_inventory_api_path)) {
  stop("Missing file: ", db_inventory_api_path)
}

if (!file.exists(db_inventory_ws_tel_path)) {
  stop("Missing file: ", db_inventory_ws_tel_path)
}

if (!file.exists(db_inventory_ws_con_path)) {
  stop("Missing file: ", db_inventory_ws_con_path)
}

db_inventory_api <- arrow::read_parquet(db_inventory_api_path)
db_inventory_ws_tel <- arrow::read_parquet(db_inventory_ws_tel_path)
db_inventory_ws_con <- arrow::read_parquet(db_inventory_ws_con_path)

# ------------------------------------------------------------
# Standardize station code and temporal availability fields
# ------------------------------------------------------------

db_inventory_api <- db_inventory_api |>
  dplyr::mutate(
    station_code = stringr::str_pad(trimws(as.character(station_code)), width = 8, side = "left", pad = "0")
  ) |>
  add_temporal_availability_fields() |>
  dplyr::distinct(station_code, .keep_all = TRUE)

db_inventory_ws_tel <- db_inventory_ws_tel |>
  dplyr::mutate(
    station_code = stringr::str_pad(trimws(as.character(station_code)), width = 8, side = "left", pad = "0")
  ) |>
  add_temporal_availability_fields() |>
  dplyr::distinct(station_code, .keep_all = TRUE)

db_inventory_ws_con <- db_inventory_ws_con |>
  dplyr::mutate(
    station_code = stringr::str_pad(trimws(as.character(station_code)), width = 8, side = "left", pad = "0")
  ) |>
  add_temporal_availability_fields() |>
  dplyr::distinct(station_code, .keep_all = TRUE)

# ------------------------------------------------------------
# Find additional stations from webservices
# ------------------------------------------------------------

ws_tel_extra <- db_inventory_ws_tel |>
  dplyr::anti_join(
    db_inventory_api |> dplyr::select(station_code),
    by = "station_code"
  )

ws_con_extra <- db_inventory_ws_con |>
  dplyr::anti_join(
    db_inventory_api |> dplyr::select(station_code),
    by = "station_code"
  ) |>
  dplyr::anti_join(
    ws_tel_extra |> dplyr::select(station_code),
    by = "station_code"
  )

# ------------------------------------------------------------
# Convert webservice-only records to API-like structure
# ------------------------------------------------------------

api_columns <- names(db_inventory_api)

ws_tel_extra_standard <- tibble::tibble(
  station_code = get_column_or_na(ws_tel_extra, "station_code"),
  station_name = get_column_or_na(ws_tel_extra, "station_name"),
  station_type = rep("Telemetrica", nrow(ws_tel_extra)),
  uf = rep(NA_character_, nrow(ws_tel_extra)),
  uf_name = rep(NA_character_, nrow(ws_tel_extra)),
  municipality_code = rep(NA_character_, nrow(ws_tel_extra)),
  municipality = get_column_or_na(ws_tel_extra, "municipality_uf"),
  basin_code = rep(NA_character_, nrow(ws_tel_extra)),
  basin_name = get_column_or_na(ws_tel_extra, "basin_name"),
  latitude = get_column_or_na(ws_tel_extra, "latitude", type = "numeric"),
  longitude = get_column_or_na(ws_tel_extra, "longitude", type = "numeric"),
  altitude = get_column_or_na(ws_tel_extra, "altitude", type = "numeric"),
  drainage_area = rep(NA_real_, nrow(ws_tel_extra)),
  operator_code = rep(NA_character_, nrow(ws_tel_extra)),
  operator = get_column_or_na(ws_tel_extra, "operator"),
  responsible_agency = get_column_or_na(ws_tel_extra, "responsible_agency"),
  is_operating = get_column_or_na(ws_tel_extra, "station_status") %in% c("0", "Ativo", "ATIVO", "A"),
  discharge_start_date = as.POSIXct(rep(NA_character_, nrow(ws_tel_extra)), tz = "UTC"),
  discharge_end_date = as.POSIXct(rep(NA_character_, nrow(ws_tel_extra)), tz = "UTC"),
  telemetric_start_date = get_column_or_na(ws_tel_extra, "telemetric_start_date", type = "datetime"),
  telemetric_end_date = get_column_or_na(ws_tel_extra, "telemetric_end_date", type = "datetime"),
  stage_start_date = as.POSIXct(rep(NA_character_, nrow(ws_tel_extra)), tz = "UTC"),
  stage_end_date = as.POSIXct(rep(NA_character_, nrow(ws_tel_extra)), tz = "UTC"),
  rainfall_start_date = as.POSIXct(rep(NA_character_, nrow(ws_tel_extra)), tz = "UTC"),
  rainfall_end_date = as.POSIXct(rep(NA_character_, nrow(ws_tel_extra)), tz = "UTC"),
  last_update = as.POSIXct(rep(NA_character_, nrow(ws_tel_extra)), tz = "UTC"),
  download_uf = rep(NA_character_, nrow(ws_tel_extra)),
  source_api = rep(FALSE, nrow(ws_tel_extra)),
  source_ws_telemetric = rep(TRUE, nrow(ws_tel_extra)),
  source_ws_conventional = rep(FALSE, nrow(ws_tel_extra)),
  source_priority = rep("ws_telemetric", nrow(ws_tel_extra)),
  source_route = get_column_or_na(ws_tel_extra, "source_route"),
  downloaded_at = rep(Sys.time(), nrow(ws_tel_extra))
)

ws_con_extra_standard <- tibble::tibble(
  station_code = get_column_or_na(ws_con_extra, "station_code"),
  station_name = get_column_or_na(ws_con_extra, "station_name"),
  station_type = get_column_or_na(ws_con_extra, "station_type"),
  uf = get_column_or_na(ws_con_extra, "uf"),
  uf_name = get_column_or_na(ws_con_extra, "uf_name"),
  municipality_code = get_column_or_na(ws_con_extra, "municipality_code"),
  municipality = get_column_or_na(ws_con_extra, "municipality"),
  basin_code = get_column_or_na(ws_con_extra, "basin_code"),
  basin_name = rep(NA_character_, nrow(ws_con_extra)),
  latitude = get_column_or_na(ws_con_extra, "latitude", type = "numeric"),
  longitude = get_column_or_na(ws_con_extra, "longitude", type = "numeric"),
  altitude = get_column_or_na(ws_con_extra, "altitude", type = "numeric"),
  drainage_area = get_column_or_na(ws_con_extra, "drainage_area", type = "numeric"),
  operator_code = get_column_or_na(ws_con_extra, "operator_code"),
  operator = get_column_or_na(ws_con_extra, "operator"),
  responsible_agency = get_column_or_na(ws_con_extra, "responsible_agency"),
  is_operating = get_column_or_na(ws_con_extra, "is_operating", type = "logical"),
  discharge_start_date = get_column_or_na(ws_con_extra, "discharge_start_date", type = "datetime"),
  discharge_end_date = get_column_or_na(ws_con_extra, "discharge_end_date", type = "datetime"),
  telemetric_start_date = get_column_or_na(ws_con_extra, "telemetric_start_date", type = "datetime"),
  telemetric_end_date = get_column_or_na(ws_con_extra, "telemetric_end_date", type = "datetime"),
  stage_start_date = get_column_or_na(ws_con_extra, "stage_start_date", type = "datetime"),
  stage_end_date = get_column_or_na(ws_con_extra, "stage_end_date", type = "datetime"),
  rainfall_start_date = get_column_or_na(ws_con_extra, "rainfall_start_date", type = "datetime"),
  rainfall_end_date = get_column_or_na(ws_con_extra, "rainfall_end_date", type = "datetime"),
  last_update = as.POSIXct(rep(NA_character_, nrow(ws_con_extra)), tz = "UTC"),
  download_uf = get_column_or_na(ws_con_extra, "uf"),
  source_api = rep(FALSE, nrow(ws_con_extra)),
  source_ws_telemetric = rep(FALSE, nrow(ws_con_extra)),
  source_ws_conventional = rep(TRUE, nrow(ws_con_extra)),
  source_priority = rep("ws_conventional", nrow(ws_con_extra)),
  source_route = get_column_or_na(ws_con_extra, "source_route"),
  downloaded_at = rep(Sys.time(), nrow(ws_con_extra))
)

# Keep only the API columns, in the same order.
ws_tel_extra_standard <- add_missing_columns(ws_tel_extra_standard, api_columns)
ws_con_extra_standard <- add_missing_columns(ws_con_extra_standard, api_columns)

# ------------------------------------------------------------
# Consolidate final inventory
# ------------------------------------------------------------

db_inventory_all <- dplyr::bind_rows(
  db_inventory_api,
  ws_tel_extra_standard,
  ws_con_extra_standard
) |>
  dplyr::mutate(
    source_api = dplyr::coalesce(source_api, FALSE),
    source_ws_telemetric = dplyr::coalesce(source_ws_telemetric, FALSE),
    source_ws_conventional = dplyr::coalesce(source_ws_conventional, FALSE)
  ) |>
  dplyr::distinct(station_code, .keep_all = TRUE) |>
  dplyr::arrange(uf, station_code)

# Add source-presence flags based on all three inventories.
source_flags <- dplyr::full_join(
  db_inventory_api |> dplyr::transmute(station_code, in_api = TRUE),
  db_inventory_ws_tel |> dplyr::transmute(station_code, in_ws_telemetric = TRUE),
  by = "station_code"
) |>
  dplyr::full_join(
    db_inventory_ws_con |> dplyr::transmute(station_code, in_ws_conventional = TRUE),
    by = "station_code"
  ) |>
  dplyr::mutate(
    in_api = dplyr::coalesce(in_api, FALSE),
    in_ws_telemetric = dplyr::coalesce(in_ws_telemetric, FALSE),
    in_ws_conventional = dplyr::coalesce(in_ws_conventional, FALSE)
  )

# Use the conventional inventory to fill temporal availability fields
# even when the priority metadata row came from the authenticated API.
temporal_from_ws_con <- db_inventory_ws_con |>
  dplyr::select(
    station_code,
    discharge_start_date,
    discharge_end_date,
    telemetric_start_date,
    telemetric_end_date,
    stage_start_date,
    stage_end_date,
    rainfall_start_date,
    rainfall_end_date
  ) |>
  dplyr::rename(
    discharge_start_date_ws_con = discharge_start_date,
    discharge_end_date_ws_con = discharge_end_date,
    telemetric_start_date_ws_con = telemetric_start_date,
    telemetric_end_date_ws_con = telemetric_end_date,
    stage_start_date_ws_con = stage_start_date,
    stage_end_date_ws_con = stage_end_date,
    rainfall_start_date_ws_con = rainfall_start_date,
    rainfall_end_date_ws_con = rainfall_end_date
  )

db_inventory_all <- db_inventory_all |>
  dplyr::select(-source_api, -source_ws_telemetric, -source_ws_conventional) |>
  dplyr::left_join(source_flags, by = "station_code") |>
  dplyr::left_join(temporal_from_ws_con, by = "station_code") |>
  dplyr::mutate(
    source_api = in_api,
    source_ws_telemetric = in_ws_telemetric,
    source_ws_conventional = in_ws_conventional,
    discharge_start_date = dplyr::coalesce(discharge_start_date_ws_con, discharge_start_date),
    discharge_end_date = dplyr::coalesce(discharge_end_date_ws_con, discharge_end_date),
    telemetric_start_date = dplyr::coalesce(telemetric_start_date_ws_con, telemetric_start_date),
    telemetric_end_date = dplyr::coalesce(telemetric_end_date_ws_con, telemetric_end_date),
    stage_start_date = dplyr::coalesce(stage_start_date_ws_con, stage_start_date),
    stage_end_date = dplyr::coalesce(stage_end_date_ws_con, stage_end_date),
    rainfall_start_date = dplyr::coalesce(rainfall_start_date_ws_con, rainfall_start_date),
    rainfall_end_date = dplyr::coalesce(rainfall_end_date_ws_con, rainfall_end_date)
  ) |>
  dplyr::select(
    -in_api,
    -in_ws_telemetric,
    -in_ws_conventional,
    -discharge_start_date_ws_con,
    -discharge_end_date_ws_con,
    -telemetric_start_date_ws_con,
    -telemetric_end_date_ws_con,
    -stage_start_date_ws_con,
    -stage_end_date_ws_con,
    -rainfall_start_date_ws_con,
    -rainfall_end_date_ws_con
  ) |>
  dplyr::arrange(uf, station_code)

added_from_ws <- dplyr::bind_rows(
  ws_tel_extra_standard |> dplyr::mutate(added_source = "ws_telemetric"),
  ws_con_extra_standard |> dplyr::mutate(added_source = "ws_conventional")
) |>
  dplyr::arrange(added_source, station_code)

inventory_summary <- tibble::tibble(
  metric = c(
    "db_inventory_api",
    "db_inventory_ws_tel",
    "db_inventory_ws_con",
    "ws_tel_not_in_api",
    "ws_con_not_in_api_or_tel",
    "db_inventory_all"
  ),
  value = c(
    nrow(db_inventory_api),
    nrow(db_inventory_ws_tel),
    nrow(db_inventory_ws_con),
    nrow(ws_tel_extra),
    nrow(ws_con_extra),
    nrow(db_inventory_all)
  )
)

temporal_availability_summary <- tibble::tibble(
  metric = c(
    "n_stations",
    "n_discharge_start_date",
    "n_discharge_end_date",
    "n_telemetric_start_date",
    "n_telemetric_end_date",
    "n_stage_start_date",
    "n_stage_end_date",
    "n_rainfall_start_date",
    "n_rainfall_end_date"
  ),
  value = c(
    nrow(db_inventory_all),
    sum(!is.na(db_inventory_all$discharge_start_date)),
    sum(!is.na(db_inventory_all$discharge_end_date)),
    sum(!is.na(db_inventory_all$telemetric_start_date)),
    sum(!is.na(db_inventory_all$telemetric_end_date)),
    sum(!is.na(db_inventory_all$stage_start_date)),
    sum(!is.na(db_inventory_all$stage_end_date)),
    sum(!is.na(db_inventory_all$rainfall_start_date)),
    sum(!is.na(db_inventory_all$rainfall_end_date))
  )
)

# ------------------------------------------------------------
# Critical checks
# ------------------------------------------------------------

if (nrow(db_inventory_all) == 0) {
  stop("The consolidated inventory is empty.")
}

duplicate_station_codes <- db_inventory_all |>
  dplyr::count(station_code) |>
  dplyr::filter(n > 1)

if (nrow(duplicate_station_codes) > 0) {
  stop("Duplicated station_code values found in consolidated inventory.")
}

# ------------------------------------------------------------
# Save outputs
# ------------------------------------------------------------

arrow::write_parquet(db_inventory_all, db_inventory_all_path)
readr::write_csv(inventory_summary, inventory_summary_path)
readr::write_csv(added_from_ws, inventory_added_from_ws_path)
readr::write_csv(temporal_availability_summary, inventory_temporal_summary_path)

message("Inventory consolidation finished.")
message("Final inventory saved to: ", db_inventory_all_path)
message("Summary saved to: ", inventory_summary_path)
message("Stations added from webservices saved to: ", inventory_added_from_ws_path)
message("Temporal availability summary saved to: ", inventory_temporal_summary_path)

print(inventory_summary)
print(temporal_availability_summary)
