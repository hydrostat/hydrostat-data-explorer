# ============================================================
# Download discharge-related ANA HidroWebService routes
# Stage: 05_discharge_measurements
#
# Purpose:
# Download raw JSON responses from selected discharge-related
# routes for stations with has_discharge_measurements == TRUE.
#
# Current production strategy:
# - Download only selected routes using route_filter.
# - Current default: resumo_descarga.
# - curva_descarga can be run later by changing route_filter.
# - perfil_transversal will be downloaded later using dates from
#   resumo_descarga, not by blind annual windows.
#
# Important design choices:
# - Incremental and restartable by station-route.
# - Previously attempted station-route pairs are skipped.
# - Failed station-route pairs are reserved for a future retry stage.
# - Each raw JSON response is saved immediately.
# - Each request is appended immediately to discharge_request_log.csv.
# - This script does not rebuild availability/status summaries.
#   Run pipeline/R/043_update_discharge_download_summaries.R separately.
# - Data requests use curl directly instead of httr2 to avoid
#   namespace instability observed in long httr2 sessions.
# ============================================================

# Load project setup and authentication
source(file.path("pipeline", "R", "000_setup.R"), local = TRUE)
source(file.path("pipeline", "R", "010_auth.R"), local = TRUE)

# Load shared pipeline helpers
source(file.path("pipeline", "helpers", "api_download_helpers.R"), local = TRUE)

# Load packages
library(jsonlite)
library(dplyr)
library(readr)
library(DBI)
library(duckdb)
library(lubridate)
library(stringr)
library(tidyr)
library(curl)

# ============================================================
# Parameters
# ============================================================

base_url <- "https://www.ana.gov.br/hidrowebservice/EstacoesTelemetricas"

database_file <- file.path("data", "ana_hidro.duckdb")

raw_output_dir <- file.path("data", "raw", "discharge_routes")
processed_dir <- file.path("data", "processed")

candidate_stations_file <- file.path(
  processed_dir,
  "discharge_candidate_stations.csv"
)

candidate_tasks_file <- file.path(
  processed_dir,
  "discharge_candidate_station_routes.csv"
)

request_log_file <- file.path(
  processed_dir,
  "discharge_request_log.csv"
)

station_route_download_status_file <- file.path(
  processed_dir,
  "discharge_station_route_download_status.csv"
)

station_download_status_file <- file.path(
  processed_dir,
  "discharge_station_download_status.csv"
)

# Number of new stations to include in this run.
max_stations <- 3000

# Current recommended route set.
# Run curva_descarga later by changing this to c("curva_descarga").
route_filter <- c("curva_descarga")

# Optional filters for batch processing.
# Keep as NA to avoid filtering.
uf_filter <- NA_character_
basin_code_filter <- NA_character_

# A small pause reduces pressure on the API.
request_sleep_seconds <- 0.1

# Stop trying a station-route after repeated consecutive failures.
max_route_failures_per_station <- 3

# Console progress settings.
progress_every_requests <- 100
min_new_requests_for_time_estimate <- 100

# Token settings.
# ANA documents a 60-minute token validity. Refresh earlier to avoid expiry.
token_refresh_minutes <- 50
token_max_attempts <- 3
token_retry_sleep_seconds <- 30
ana_token_cache_file <- file.path("config", "ana_token_cache.rds")

# ============================================================
# Critical checks and folders
# ============================================================

if (!file.exists(database_file)) {
  stop("Missing database file: ", database_file)
}

dir.create(raw_output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)

# ============================================================
# Routes
# ============================================================

routes_all <- data.frame(
  route_name = c(
    "resumo_descarga",
    "perfil_transversal",
    "curva_descarga"
  ),
  endpoint = c(
    "/HidroSerieResumoDescarga/v1",
    "/HidroSeriePerfilTransversal/v1",
    "/HidroSerieCurvaDescarga/v1"
  ),
  use_tipo_filtro_data = c(
    TRUE,
    TRUE,
    FALSE
  ),
  stringsAsFactors = FALSE
)

routes <- routes_all %>%
  filter(route_name %in% route_filter) %>%
  mutate(
    route_name = as.character(route_name),
    endpoint = as.character(endpoint)
  )

if (nrow(routes) == 0) {
  stop("No valid routes selected in route_filter.")
}

# ============================================================
# Helper functions
# ============================================================

safe_as_date <- function(x) {
  if (inherits(x, "Date")) {
    return(x)
  }
  
  if (is.numeric(x)) {
    return(as.Date(x, origin = "1970-01-01"))
  }
  
  as.Date(x)
}

make_date_windows <- function(start_date, end_date) {
  windows <- list()
  current_start <- safe_as_date(start_date)
  final_end <- safe_as_date(end_date)
  i <- 1
  
  while (current_start <= final_end) {
    current_end <- min(current_start + 365, final_end)
    
    windows[[i]] <- data.frame(
      window_id = i,
      date_start = current_start,
      date_end = current_end,
      stringsAsFactors = FALSE
    )
    
    current_start <- current_end + 1
    i <- i + 1
  }
  
  dplyr::bind_rows(windows) %>%
    mutate(
      date_start = safe_as_date(date_start),
      date_end = safe_as_date(date_end)
    )
}

make_raw_file <- function(route_name, station_code, date_start, date_end) {
  route_dir <- file.path(
    raw_output_dir,
    paste0("route=", route_name),
    paste0("station=", station_code)
  )
  
  dir.create(route_dir, recursive = TRUE, showWarnings = FALSE)
  
  file.path(
    route_dir,
    paste0(
      "station_", station_code,
      "_", route_name,
      "_", as.character(safe_as_date(date_start)),
      "_", as.character(safe_as_date(date_end)),
      ".json"
    )
  )
}

make_log_row <- function(
    station_code,
    route_name,
    endpoint,
    date_start,
    date_end,
    http_status = NA_real_,
    api_status = NA_character_,
    api_code = NA_character_,
    api_message = NA_character_,
    n_items = NA_real_,
    success = FALSE,
    error_message = NA_character_,
    raw_file = NA_character_,
    requested_at,
    downloaded_at = Sys.time()
) {
  data.frame(
    station_code = as.character(station_code),
    route_name = as.character(route_name),
    endpoint = as.character(endpoint),
    date_start = as.character(safe_as_date(date_start)),
    date_end = as.character(safe_as_date(date_end)),
    http_status = suppressWarnings(as.numeric(http_status)),
    api_status = as.character(api_status),
    api_code = as.character(api_code),
    api_message = as.character(api_message),
    n_items = suppressWarnings(as.numeric(n_items)),
    success = as.logical(success),
    error_message = as.character(error_message),
    raw_file = as.character(raw_file),
    requested_at = as.character(requested_at),
    downloaded_at = as.character(downloaded_at),
    stringsAsFactors = FALSE
  )
}

append_request_log <- function(log_row, log_file) {
  expected_cols <- c(
    "station_code",
    "route_name",
    "endpoint",
    "date_start",
    "date_end",
    "http_status",
    "api_status",
    "api_code",
    "api_message",
    "n_items",
    "success",
    "error_message",
    "raw_file",
    "requested_at",
    "downloaded_at"
  )
  
  log_row <- as.data.frame(log_row, stringsAsFactors = FALSE)
  
  for (col in expected_cols) {
    if (!col %in% names(log_row)) {
      log_row[[col]] <- NA
    }
  }
  
  log_row <- log_row[expected_cols]
  
  if (!file.exists(log_file)) {
    utils::write.table(
      log_row,
      file = log_file,
      sep = ",",
      row.names = FALSE,
      col.names = TRUE,
      append = FALSE,
      qmethod = "double",
      fileEncoding = "UTF-8"
    )
  } else {
    utils::write.table(
      log_row,
      file = log_file,
      sep = ",",
      row.names = FALSE,
      col.names = FALSE,
      append = TRUE,
      qmethod = "double",
      fileEncoding = "UTF-8"
    )
  }
  
  invisible(TRUE)
}

read_attempted_station_route_status <- function(log_file) {
  empty_status <- data.frame(
    station_code = character(),
    route_name = character(),
    n_requests_logged = integer(),
    n_success = integer(),
    n_failed = integer(),
    n_with_items = integer(),
    total_items = numeric(),
    station_route_download_status = character(),
    stringsAsFactors = FALSE
  )
  
  if (!file.exists(log_file)) {
    return(empty_status)
  }
  
  request_log <- readr::read_csv(
    log_file,
    col_types = readr::cols(
      station_code = readr::col_character(),
      route_name = readr::col_character(),
      n_items = readr::col_double(),
      success = readr::col_logical(),
      .default = readr::col_guess()
    )
  )
  
  if (nrow(request_log) == 0) {
    return(empty_status)
  }
  
  request_log %>%
    mutate(
      station_code = as.character(station_code),
      route_name = as.character(route_name),
      success = as.logical(success),
      has_items = !is.na(n_items) & n_items > 0
    ) %>%
    group_by(station_code, route_name) %>%
    summarise(
      n_requests_logged = n(),
      n_success = sum(success, na.rm = TRUE),
      n_failed = sum(!success, na.rm = TRUE),
      n_with_items = sum(has_items, na.rm = TRUE),
      total_items = sum(n_items, na.rm = TRUE),
      station_route_download_status = dplyr::if_else(
        n_failed > 0,
        "failed",
        "completed_or_attempted"
      ),
      .groups = "drop"
    )
}

summarise_station_status <- function(station_route_status) {
  if (nrow(station_route_status) == 0) {
    return(
      data.frame(
        station_code = character(),
        n_routes_logged = integer(),
        n_requests_logged = integer(),
        n_success = integer(),
        n_failed = integer(),
        n_with_items = integer(),
        total_items = numeric(),
        station_download_status = character(),
        stringsAsFactors = FALSE
      )
    )
  }
  
  station_route_status %>%
    group_by(station_code) %>%
    summarise(
      n_routes_logged = n_distinct(route_name),
      n_requests_logged = sum(n_requests_logged, na.rm = TRUE),
      n_success = sum(n_success, na.rm = TRUE),
      n_failed = sum(n_failed, na.rm = TRUE),
      n_with_items = sum(n_with_items, na.rm = TRUE),
      total_items = sum(total_items, na.rm = TRUE),
      station_download_status = dplyr::if_else(
        n_failed > 0,
        "failed",
        "completed_or_attempted"
      ),
      .groups = "drop"
    )
}

build_query_url <- function(endpoint, station_code, use_tipo_filtro_data, date_start, date_end) {
  query <- list(
    "Código da Estação" = as.character(station_code),
    "Data Inicial (yyyy-MM-dd)" = as.character(safe_as_date(date_start)),
    "Data Final (yyyy-MM-dd)" = as.character(safe_as_date(date_end))
  )
  
  if (isTRUE(use_tipo_filtro_data)) {
    query[["Tipo Filtro Data"]] <- "DATA_LEITURA"
  }
  
  query_string <- paste(
    paste0(
      utils::URLencode(names(query), reserved = TRUE),
      "=",
      utils::URLencode(unlist(query), reserved = TRUE)
    ),
    collapse = "&"
  )
  
  paste0(base_url, endpoint, "?", query_string)
}

download_one_request <- function(station_code, route_name, endpoint,
                                 use_tipo_filtro_data, date_start, date_end,
                                 raw_file, token) {
  request_time <- Sys.time()
  
  if (is.null(token) || is.na(token) || token == "") {
    return(
      make_log_row(
        station_code = station_code,
        route_name = route_name,
        endpoint = endpoint,
        date_start = date_start,
        date_end = date_end,
        http_status = NA_real_,
        n_items = NA_real_,
        success = FALSE,
        error_message = "Missing ANA authentication token.",
        raw_file = NA_character_,
        requested_at = request_time
      )
    )
  }
  
  full_url <- build_query_url(
    endpoint = endpoint,
    station_code = station_code,
    use_tipo_filtro_data = use_tipo_filtro_data,
    date_start = date_start,
    date_end = date_end
  )
  
  response <- tryCatch(
    {
      handle <- curl::new_handle(timeout = 120)
      
      curl::handle_setheaders(
        handle,
        Authorization = paste("Bearer", token),
        accept = "*/*"
      )
      
      curl::curl_fetch_memory(
        full_url,
        handle = handle
      )
    },
    error = function(e) {
      return(e)
    }
  )
  
  if (inherits(response, "error")) {
    return(
      make_log_row(
        station_code = station_code,
        route_name = route_name,
        endpoint = endpoint,
        date_start = date_start,
        date_end = date_end,
        http_status = NA_real_,
        n_items = NA_real_,
        success = FALSE,
        error_message = paste("Request error:", conditionMessage(response)),
        raw_file = NA_character_,
        requested_at = request_time
      )
    )
  }
  
  http_status <- response$status_code
  response_text <- rawToChar(response$content)
  
  writeBin(response$content, raw_file)
  
  if (http_status < 200 || http_status >= 300) {
    return(
      make_log_row(
        station_code = station_code,
        route_name = route_name,
        endpoint = endpoint,
        date_start = date_start,
        date_end = date_end,
        http_status = http_status,
        n_items = NA_real_,
        success = FALSE,
        error_message = paste("HTTP status", http_status),
        raw_file = raw_file,
        requested_at = request_time
      )
    )
  }
  
  parsed <- tryCatch(
    {
      jsonlite::fromJSON(response_text, flatten = TRUE)
    },
    error = function(e) {
      return(e)
    }
  )
  
  if (inherits(parsed, "error")) {
    return(
      make_log_row(
        station_code = station_code,
        route_name = route_name,
        endpoint = endpoint,
        date_start = date_start,
        date_end = date_end,
        http_status = http_status,
        n_items = NA_real_,
        success = FALSE,
        error_message = paste("JSON parse error:", conditionMessage(parsed)),
        raw_file = raw_file,
        requested_at = request_time
      )
    )
  }
  
  make_log_row(
    station_code = station_code,
    route_name = route_name,
    endpoint = endpoint,
    date_start = date_start,
    date_end = date_end,
    http_status = http_status,
    api_status = get_scalar(parsed, "status"),
    api_code = get_scalar(parsed, "code"),
    api_message = get_scalar(parsed, "message"),
    n_items = safe_n_items(parsed),
    success = TRUE,
    error_message = NA_character_,
    raw_file = raw_file,
    requested_at = request_time
  )
}

# ============================================================
# Read stations
# ============================================================

con <- NULL

stations <- tryCatch(
  {
    con <- DBI::dbConnect(duckdb::duckdb(), dbdir = database_file)
    
    DBI::dbGetQuery(
      con,
      "
      select
        station_code,
        station_name,
        station_type,
        uf,
        basin_code,
        has_discharge_measurements,
        discharge_start_date,
        discharge_end_date
      from stations
      "
    )
  },
  error = function(e) {
    stop("Failed database connection/read: ", conditionMessage(e))
  },
  finally = {
    if (!is.null(con)) {
      DBI::dbDisconnect(con)
    }
  }
)

required_station_fields <- c(
  "station_code",
  "has_discharge_measurements",
  "discharge_start_date",
  "discharge_end_date"
)

missing_station_fields <- setdiff(required_station_fields, names(stations))

if (length(missing_station_fields) > 0) {
  stop(
    "Missing expected field(s) in stations table: ",
    paste(missing_station_fields, collapse = ", ")
  )
}

# ============================================================
# Build station-route candidate tasks
# ============================================================

candidate_stations_all <- stations %>%
  mutate(
    station_code = as.character(station_code),
    discharge_start_date = safe_as_date(discharge_start_date),
    discharge_end_date = safe_as_date(discharge_end_date),
    download_start_date = safe_as_date(discharge_start_date),
    download_end_date = dplyr::if_else(
      is.na(discharge_end_date),
      Sys.Date(),
      discharge_end_date
    ),
    download_end_date = safe_as_date(download_end_date),
    download_end_date = safe_as_date(
      pmin(download_end_date, Sys.Date())
    )
  ) %>%
  filter(
    has_discharge_measurements == TRUE,
    !is.na(download_start_date),
    !is.na(download_end_date),
    download_end_date >= download_start_date
  )

if (!is.na(uf_filter)) {
  candidate_stations_all <- candidate_stations_all %>%
    filter(uf == uf_filter)
}

if (!is.na(basin_code_filter)) {
  candidate_stations_all <- candidate_stations_all %>%
    filter(as.character(basin_code) == as.character(basin_code_filter))
}

candidate_stations_all <- candidate_stations_all %>%
  mutate(
    download_start_date = safe_as_date(download_start_date),
    download_end_date = safe_as_date(download_end_date),
    n_days = as.numeric(download_end_date - download_start_date) + 1,
    n_windows = ceiling(n_days / 366)
  ) %>%
  select(-n_days) %>%
  arrange(station_code)

attempted_station_route_status <- read_attempted_station_route_status(request_log_file)

readr::write_csv(
  attempted_station_route_status,
  station_route_download_status_file
)

station_status_for_display <- summarise_station_status(attempted_station_route_status)
readr::write_csv(station_status_for_display, station_download_status_file)

attempted_station_routes <- attempted_station_route_status %>%
  select(station_code, route_name) %>%
  distinct()

failed_station_routes <- attempted_station_route_status %>%
  filter(station_route_download_status == "failed") %>%
  select(station_code, route_name) %>%
  distinct()

completed_or_attempted_station_routes <- attempted_station_route_status %>%
  filter(station_route_download_status == "completed_or_attempted") %>%
  select(station_code, route_name) %>%
  distinct()

candidate_tasks_remaining <- tidyr::crossing(
  candidate_stations_all,
  routes
) %>%
  mutate(
    station_code = as.character(station_code),
    route_name = as.character(route_name),
    download_start_date = safe_as_date(download_start_date),
    download_end_date = safe_as_date(download_end_date)
  ) %>%
  anti_join(attempted_station_routes, by = c("station_code", "route_name")) %>%
  mutate(
    n_expected_requests = n_windows
  ) %>%
  arrange(station_code, match(route_name, route_filter))

remaining_station_codes <- candidate_tasks_remaining %>%
  distinct(station_code) %>%
  arrange(station_code)

n_all_candidate_stations <- nrow(candidate_stations_all)
n_already_attempted_station_routes <- nrow(attempted_station_routes)
n_failed_station_routes <- nrow(failed_station_routes)
n_completed_or_attempted_station_routes <- nrow(completed_or_attempted_station_routes)
n_remaining_candidate_stations <- nrow(remaining_station_codes)

total_all_expected_requests <- sum(candidate_tasks_remaining$n_expected_requests, na.rm = TRUE)

if (is.finite(max_stations)) {
  selected_station_codes <- remaining_station_codes %>%
    slice_head(n = max_stations) %>%
    pull(station_code)
  
  candidate_tasks <- candidate_tasks_remaining %>%
    filter(station_code %in% selected_station_codes)
} else {
  candidate_tasks <- candidate_tasks_remaining
}

if (nrow(candidate_tasks) == 0) {
  stop(
    "No new candidate station-routes available. ",
    "All selected route_filter tasks were already attempted or filtered out."
  )
}

candidate_stations <- candidate_tasks %>%
  distinct(
    station_code,
    station_name,
    uf,
    basin_code,
    download_start_date,
    download_end_date,
    n_windows
  ) %>%
  mutate(
    download_start_date = safe_as_date(download_start_date),
    download_end_date = safe_as_date(download_end_date)
  ) %>%
  arrange(station_code)

candidate_tasks <- candidate_tasks %>%
  mutate(
    download_start_date = safe_as_date(download_start_date),
    download_end_date = safe_as_date(download_end_date)
  ) %>%
  arrange(station_code, match(route_name, route_filter))

total_expected_requests <- sum(candidate_tasks$n_expected_requests, na.rm = TRUE)

readr::write_csv(candidate_stations, candidate_stations_file)
readr::write_csv(candidate_tasks, candidate_tasks_file)

cat("\n============================================================\n")
cat("Candidate station-route selection\n")
cat("============================================================\n")
cat("Route filter:                          ", paste(route_filter, collapse = ", "), "\n")
cat("All candidate stations:                ", n_all_candidate_stations, "\n")
cat("Already attempted station-routes:      ", n_already_attempted_station_routes, "\n")
cat("Completed/attempted station-routes:    ", n_completed_or_attempted_station_routes, "\n")
cat("Failed station-routes reserved later:  ", n_failed_station_routes, "\n")
cat("Remaining new stations:                ", n_remaining_candidate_stations, "\n")
cat("Stations in this run:                  ", nrow(candidate_stations), "\n")
cat("Station-route tasks in this run:       ", nrow(candidate_tasks), "\n")
cat("Expected route requests this run:      ", total_expected_requests, "\n")
cat("Expected requests remaining:           ", total_all_expected_requests, "\n")
cat("Progress update interval:              ", progress_every_requests, " requests\n")
cat("Candidate stations file:               ", candidate_stations_file, "\n")
cat("Candidate station-routes file:         ", candidate_tasks_file, "\n")
cat("Station-route status file:             ", station_route_download_status_file, "\n")

# ============================================================
# Token initialization
# ============================================================

cat("\nObtaining ANA token...\n")

current_token <- get_token_with_retries(
  max_attempts = token_max_attempts,
  sleep_seconds = token_retry_sleep_seconds,
  force_refresh = FALSE
)

token_created_at <- Sys.time()

cat("ANA token obtained. Characters: ", nchar(current_token), "\n", sep = "")
cat("Proceeding to download loop...\n")

# ============================================================
# Download loop
# ============================================================

run_start_time <- Sys.time()
stop_requested <- FALSE

cat("Download loop initialization reached.\n")
cat("\n============================================================\n")
cat("Starting download\n")
cat("============================================================\n")
cat("Started at: ", as.character(run_start_time), "\n")

n_requested <- 0L
n_skipped <- 0L
n_success <- 0L
n_failed <- 0L
n_positive <- 0L

for (station_index in seq_len(nrow(candidate_stations))) {
  if (isTRUE(stop_requested)) {
    break
  }
  
  this_station <- candidate_stations[station_index, ]
  
  station_code <- as.character(this_station$station_code)
  station_start <- safe_as_date(this_station$download_start_date)
  station_end <- safe_as_date(this_station$download_end_date)
  date_windows <- make_date_windows(station_start, station_end)
  
  station_tasks <- candidate_tasks %>%
    filter(station_code == this_station$station_code)
  
  station_start_time <- Sys.time()
  station_requested_before <- n_requested
  station_skipped_before <- n_skipped
  station_success_before <- n_success
  station_failed_before <- n_failed
  station_positive_before <- n_positive
  
  cat("\n============================================================\n")
  cat("Station ", station_index, " of ", nrow(candidate_stations), ": ", station_code, "\n", sep = "")
  cat("Name:    ", this_station$station_name, "\n", sep = "")
  cat("UF:      ", this_station$uf, "\n", sep = "")
  cat("Period:  ", as.character(station_start), " to ", as.character(station_end), "\n", sep = "")
  cat("Windows: ", nrow(date_windows), " | Routes this run: ", nrow(station_tasks),
      " | Expected requests: ", sum(station_tasks$n_expected_requests, na.rm = TRUE), "\n", sep = "")
  cat("============================================================\n")
  
  for (route_index in seq_len(nrow(station_tasks))) {
    if (isTRUE(stop_requested)) {
      break
    }
    
    this_route <- station_tasks[route_index, ]
    
    cat("  Route ", route_index, " of ", nrow(station_tasks), ": ", this_route$route_name, "\n", sep = "")
    
    route_requested_before <- n_requested
    route_skipped_before <- n_skipped
    route_success_before <- n_success
    route_failed_before <- n_failed
    route_positive_before <- n_positive
    route_consecutive_failures <- 0L
    
    for (window_index in seq_len(nrow(date_windows))) {
      if (isTRUE(stop_requested)) {
        break
      }
      
      if (route_consecutive_failures >= max_route_failures_per_station) {
        cat(
          "    Reached ",
          max_route_failures_per_station,
          " consecutive failures for this station-route. Moving on.\n",
          sep = ""
        )
        break
      }
      
      elapsed_token_minutes <- as.numeric(difftime(Sys.time(), token_created_at, units = "mins"))
      
      if (!is.na(elapsed_token_minutes) && elapsed_token_minutes >= token_refresh_minutes) {
        cat("\nRefreshing ANA token...\n")
        
        refreshed_token <- tryCatch(
          {
            get_token_with_retries(
              max_attempts = token_max_attempts,
              sleep_seconds = token_retry_sleep_seconds,
              force_refresh = TRUE
            )
          },
          error = function(e) {
            return(e)
          }
        )
        
        if (inherits(refreshed_token, "error")) {
          cat(
            "\nFailed to refresh ANA token. Stopping this run safely.\n",
            "Reason: ", conditionMessage(refreshed_token), "\n",
            sep = ""
          )
          
          stop_requested <- TRUE
          break
        }
        
        current_token <- refreshed_token
        token_created_at <- Sys.time()
        cat("ANA token refreshed. Characters: ", nchar(current_token), "\n", sep = "")
      }
      
      date_start <- safe_as_date(date_windows$date_start[window_index])
      date_end <- safe_as_date(date_windows$date_end[window_index])
      
      raw_file <- make_raw_file(
        route_name = this_route$route_name,
        station_code = station_code,
        date_start = date_start,
        date_end = date_end
      )
      
      log_row <- download_one_request(
        station_code = station_code,
        route_name = this_route$route_name,
        endpoint = this_route$endpoint,
        use_tipo_filtro_data = this_route$use_tipo_filtro_data,
        date_start = date_start,
        date_end = date_end,
        raw_file = raw_file,
        token = current_token
      )
      
      # If the token is rejected during a data request, refresh once and retry
      # before writing the final log row.
      if (!is.na(log_row$http_status[1]) && log_row$http_status[1] == 401) {
        cat("    HTTP 401 received. Refreshing token and retrying this request once.\n")
        
        refreshed_token <- tryCatch(
          {
            get_token_with_retries(
              max_attempts = token_max_attempts,
              sleep_seconds = token_retry_sleep_seconds,
              force_refresh = TRUE
            )
          },
          error = function(e) {
            return(e)
          }
        )
        
        if (!inherits(refreshed_token, "error")) {
          current_token <- refreshed_token
          token_created_at <- Sys.time()
          
          log_row <- download_one_request(
            station_code = station_code,
            route_name = this_route$route_name,
            endpoint = this_route$endpoint,
            use_tipo_filtro_data = this_route$use_tipo_filtro_data,
            date_start = date_start,
            date_end = date_end,
            raw_file = raw_file,
            token = current_token
          )
        } else {
          log_row$error_message <- paste(
            as.character(log_row$error_message[1]),
            "Token refresh after 401 failed:",
            conditionMessage(refreshed_token)
          )
        }
      }
      
      append_request_log(log_row, request_log_file)
      
      n_requested <- n_requested + 1L
      
      if (isTRUE(log_row$success[1])) {
        n_success <- n_success + 1L
        route_consecutive_failures <- 0L
      } else {
        n_failed <- n_failed + 1L
        route_consecutive_failures <- route_consecutive_failures + 1L
        
        cat(
          "    Failed request: ",
          station_code, " | ", this_route$route_name, " | ",
          as.character(date_start), " to ", as.character(date_end),
          " | ", as.character(log_row$error_message[1]), "\n",
          sep = ""
        )
      }
      
      if (!is.na(log_row$n_items[1]) && log_row$n_items[1] > 0) {
        n_positive <- n_positive + 1L
      }
      
      if (n_requested > 0 && n_requested %% progress_every_requests == 0) {
        print_progress_status(
          done = n_requested + n_skipped,
          total = total_expected_requests,
          run_start_time = run_start_time,
          label = "Overall request progress"
        )
      }
      
      Sys.sleep(request_sleep_seconds)
    }
    
    cat("    Route summary: new=", n_requested - route_requested_before,
        " | skipped=", n_skipped - route_skipped_before,
        " | success=", n_success - route_success_before,
        " | failed=", n_failed - route_failed_before,
        " | with_items=", n_positive - route_positive_before,
        "\n", sep = "")
  }
  
  station_elapsed_seconds <- as.numeric(difftime(Sys.time(), station_start_time, units = "secs"))
  
  cat("\nStation completed: ", station_code, "\n", sep = "")
  cat("  Station:    ", station_index, " of ", nrow(candidate_stations), "\n", sep = "")
  cat("  New req.:   ", n_requested - station_requested_before, "\n", sep = "")
  cat("  Skipped:    ", n_skipped - station_skipped_before, "\n", sep = "")
  cat("  Success:    ", n_success - station_success_before, "\n", sep = "")
  cat("  Failed:     ", n_failed - station_failed_before, "\n", sep = "")
  cat("  With items: ", n_positive - station_positive_before, "\n", sep = "")
  cat("  Time:       ", format_duration(station_elapsed_seconds), "\n", sep = "")
  
  print_progress_status(
    done = n_requested + n_skipped,
    total = total_expected_requests,
    run_start_time = run_start_time,
    label = "Run progress after station"
  )
}

# ============================================================
# Final message
# ============================================================

run_end_time <- Sys.time()
run_elapsed_seconds <- as.numeric(difftime(run_end_time, run_start_time, units = "secs"))
requests_completed_this_run <- n_requested + n_skipped

if (n_requested >= min_new_requests_for_time_estimate) {
  average_seconds_per_new_request <- run_elapsed_seconds / n_requested
  estimated_seconds_all_candidates <- total_all_expected_requests * average_seconds_per_new_request
} else {
  average_seconds_per_new_request <- NA_real_
  estimated_seconds_all_candidates <- NA_real_
}

average_seconds_label <- if (is.na(average_seconds_per_new_request)) {
  "Not estimated; too few new requests"
} else {
  as.character(round(average_seconds_per_new_request, 3))
}

estimated_total_time_label <- if (is.na(estimated_seconds_all_candidates)) {
  "Not estimated; too few new requests"
} else {
  format_duration(estimated_seconds_all_candidates)
}

cat("\n============================================================\n")
cat("Download run summary\n")
cat("============================================================\n")
cat("Route filter:            ", paste(route_filter, collapse = ", "), "\n")
cat("Started at:              ", as.character(run_start_time), "\n")
cat("Finished at:             ", as.character(run_end_time), "\n")
cat("Elapsed time:            ", format_duration(run_elapsed_seconds), "\n")
cat("Average sec/new request: ", average_seconds_label, "\n")
cat("Expected requests:       ", total_expected_requests, "\n")
cat("Completed this run:      ", requests_completed_this_run, "\n")
cat("New requests performed:  ", n_requested, "\n")
cat("Skipped completed:       ", n_skipped, "\n")
cat("Successful requests:     ", n_success, "\n")
cat("Failed requests:         ", n_failed, "\n")
cat("Requests with items:     ", n_positive, "\n")
cat("\nRemaining candidate estimate at current speed:\n")
cat("All candidate stations:               ", n_all_candidate_stations, "\n")
cat("Already attempted station-routes:     ", n_already_attempted_station_routes, "\n")
cat("Completed/attempted station-routes:   ", n_completed_or_attempted_station_routes, "\n")
cat("Failed station-routes reserved later: ", n_failed_station_routes, "\n")
cat("Remaining new stations:               ", n_remaining_candidate_stations, "\n")
cat("Expected requests remaining:          ", total_all_expected_requests, "\n")
cat("Estimated total time remaining:       ", estimated_total_time_label, "\n")
cat("\nFiles updated by 042:\n")
cat("Candidate stations:       ", candidate_stations_file, "\n")
cat("Candidate station-routes: ", candidate_tasks_file, "\n")
cat("Request log:              ", request_log_file, "\n")
cat("Raw JSON folder:          ", raw_output_dir, "\n")
cat("\nSummary files are not rebuilt inside 042.\n")
cat("To update availability/status summaries, run:\n")
cat("source('pipeline/R/043_update_discharge_download_summaries.R')\n")

gc()

cat("\nDone.\n")
