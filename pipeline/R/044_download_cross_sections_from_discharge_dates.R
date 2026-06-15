# ============================================================
# Download cross sections from discharge-measurement dates
# Stage: 05_discharge_measurements / cross-section download
#
# Purpose:
# Download raw JSON responses from ANA HidroWebService route
# HidroSeriePerfilTransversal using only dates where
# discharge measurements exist in the processed local database.
#
# Strategy:
# - Read station_code + measurement_datetime from discharge_measurements.
# - Convert measurement datetimes to measurement dates.
# - Build short date windows around those dates.
# - Merge nearby windows into bounded clusters.
# - Download only /HidroSeriePerfilTransversal/v1 for those clusters.
# - Save raw JSON immediately.
# - Append one row per request to cross_section_request_log.csv.
#
# Design choices:
# - This script does not use annual blind windows.
# - This script uses a separate log from the discharge route downloader.
# - The request unit is station_code + date_start + date_end.
# - Previously attempted clusters are skipped.
# - Failed clusters are reserved for a later retry script.
# - Data requests use curl directly.
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
library(curl)

# ============================================================
# Parameters
# ============================================================

base_url <- "https://www.ana.gov.br/hidrowebservice/EstacoesTelemetricas"

endpoint <- "/HidroSeriePerfilTransversal/v1"
route_name <- "perfil_transversal"
use_tipo_filtro_data <- TRUE

database_file <- file.path("data", "ana_hidro.duckdb")

raw_output_dir <- file.path("data", "raw", "discharge_cross_sections")
processed_dir <- file.path("data", "processed")

candidate_dates_file <- file.path(
  processed_dir,
  "cross_section_candidate_measurement_dates.csv"
)

candidate_clusters_file <- file.path(
  processed_dir,
  "cross_section_candidate_clusters.csv"
)

request_log_file <- file.path(
  processed_dir,
  "cross_section_request_log.csv"
)

cluster_status_file <- file.path(
  processed_dir,
  "cross_section_cluster_download_status.csv"
)

station_status_file <- file.path(
  processed_dir,
  "cross_section_station_download_status.csv"
)

# Number of new stations to include in this run.
max_stations <- 1000

# Cross-section windows are built from actual discharge-measurement dates.
cross_section_window_days <- 1
merge_gap_days <- 3
max_cluster_days <- 31

# Optional filters for batch processing.
# Keep as NA to avoid filtering.
uf_filter <- NA_character_
basin_code_filter <- NA_character_

# A small pause reduces pressure on the API.
request_sleep_seconds <- 0.2

# Stop trying a station after repeated consecutive failures.
max_consecutive_failures_per_station <- 3

# Console progress settings.
progress_every_requests <- 100
min_new_requests_for_time_estimate <- 50

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
# Helper functions
# ============================================================

safe_as_date <- function(x) {
  if (inherits(x, "Date")) {
    return(x)
  }

  if (inherits(x, c("POSIXct", "POSIXt"))) {
    return(as.Date(x))
  }

  if (is.numeric(x)) {
    return(as.Date(x, origin = "1970-01-01"))
  }

  as.Date(x)
}

build_cross_section_clusters_one_station <- function(station_dates,
                                                     window_days,
                                                     merge_gap_days,
                                                     max_cluster_days) {
  station_dates <- sort(unique(safe_as_date(station_dates)))
  station_dates <- station_dates[!is.na(station_dates)]

  if (length(station_dates) == 0) {
    return(
      data.frame(
        cluster_id = integer(),
        date_start = as.Date(character()),
        date_end = as.Date(character()),
        n_measurement_dates = integer()
      )
    )
  }

  windows <- data.frame(
    window_start = station_dates - window_days,
    window_end = station_dates + window_days,
    stringsAsFactors = FALSE
  )

  clusters <- list()
  current_start <- safe_as_date(windows$window_start[1])
  current_end <- safe_as_date(windows$window_end[1])
  cluster_id <- 1L

  if (nrow(windows) > 1) {
    for (i in 2:nrow(windows)) {
      next_start <- safe_as_date(windows$window_start[i])
      next_end <- safe_as_date(windows$window_end[i])

      merged_start <- current_start
      merged_end <- max(current_end, next_end)
      merged_days <- as.numeric(merged_end - merged_start) + 1

      should_merge <- (
        next_start <= current_end + merge_gap_days &&
          merged_days <= max_cluster_days
      )

      if (isTRUE(should_merge)) {
        current_end <- merged_end
      } else {
        n_dates <- sum(station_dates >= current_start & station_dates <= current_end)

        clusters[[length(clusters) + 1L]] <- data.frame(
          cluster_id = cluster_id,
          date_start = current_start,
          date_end = current_end,
          n_measurement_dates = n_dates,
          stringsAsFactors = FALSE
        )

        cluster_id <- cluster_id + 1L
        current_start <- next_start
        current_end <- next_end
      }
    }
  }

  n_dates <- sum(station_dates >= current_start & station_dates <= current_end)

  clusters[[length(clusters) + 1L]] <- data.frame(
    cluster_id = cluster_id,
    date_start = current_start,
    date_end = current_end,
    n_measurement_dates = n_dates,
    stringsAsFactors = FALSE
  )

  dplyr::bind_rows(clusters) %>%
    mutate(
      date_start = safe_as_date(date_start),
      date_end = safe_as_date(date_end)
    )
}

make_raw_file <- function(station_code, date_start, date_end) {
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
    cluster_id = NA_integer_,
    n_measurement_dates = NA_integer_,
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
    cluster_id = suppressWarnings(as.integer(cluster_id)),
    n_measurement_dates = suppressWarnings(as.integer(n_measurement_dates)),
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
    "cluster_id",
    "n_measurement_dates",
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

read_attempted_cluster_status <- function(log_file) {
  empty_status <- data.frame(
    station_code = character(),
    date_start = character(),
    date_end = character(),
    n_attempts = integer(),
    n_success_attempts = integer(),
    n_failed_attempts = integer(),
    ever_success = logical(),
    cluster_download_status = character(),
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
      date_start = readr::col_character(),
      date_end = readr::col_character(),
      cluster_id = readr::col_integer(),
      n_measurement_dates = readr::col_integer(),
      http_status = readr::col_double(),
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
      date_start = as.character(date_start),
      date_end = as.character(date_end),
      success = as.logical(success)
    ) %>%
    group_by(station_code, date_start, date_end) %>%
    summarise(
      n_attempts = n(),
      n_success_attempts = sum(success, na.rm = TRUE),
      n_failed_attempts = sum(!success, na.rm = TRUE),
      ever_success = any(success, na.rm = TRUE),
      cluster_download_status = dplyr::if_else(
        ever_success,
        "completed",
        "failed"
      ),
      .groups = "drop"
    )
}

summarise_station_status <- function(cluster_status) {
  if (nrow(cluster_status) == 0) {
    return(
      data.frame(
        station_code = character(),
        n_clusters_logged = integer(),
        n_completed_clusters = integer(),
        n_failed_clusters = integer(),
        station_download_status = character(),
        stringsAsFactors = FALSE
      )
    )
  }

  cluster_status %>%
    group_by(station_code) %>%
    summarise(
      n_clusters_logged = n(),
      n_completed_clusters = sum(cluster_download_status == "completed", na.rm = TRUE),
      n_failed_clusters = sum(cluster_download_status == "failed", na.rm = TRUE),
      station_download_status = dplyr::case_when(
        n_failed_clusters == 0 ~ "completed",
        n_completed_clusters > 0 ~ "partial",
        TRUE ~ "failed"
      ),
      .groups = "drop"
    )
}

build_query_url <- function(station_code, date_start, date_end) {
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

download_one_request <- function(station_code, date_start, date_end,
                                 cluster_id, n_measurement_dates,
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
        cluster_id = cluster_id,
        n_measurement_dates = n_measurement_dates,
        success = FALSE,
        error_message = "Missing ANA authentication token.",
        raw_file = NA_character_,
        requested_at = request_time
      )
    )
  }

  full_url <- build_query_url(
    station_code = station_code,
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
        cluster_id = cluster_id,
        n_measurement_dates = n_measurement_dates,
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
        cluster_id = cluster_id,
        n_measurement_dates = n_measurement_dates,
        http_status = http_status,
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
        cluster_id = cluster_id,
        n_measurement_dates = n_measurement_dates,
        http_status = http_status,
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
    cluster_id = cluster_id,
    n_measurement_dates = n_measurement_dates,
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
# Read discharge measurement dates
# ============================================================

con <- NULL

measurement_dates <- tryCatch(
  {
    con <- DBI::dbConnect(duckdb::duckdb(), dbdir = database_file)

    existing_tables <- DBI::dbGetQuery(con, "show tables")

    if (!"discharge_measurements" %in% existing_tables$name) {
      stop("DuckDB table discharge_measurements was not found.")
    }

    DBI::dbGetQuery(
      con,
      "
      select distinct
        dm.station_code,
        s.station_name,
        s.uf,
        s.basin_code,
        cast(dm.measurement_datetime as date) as measurement_date
      from discharge_measurements dm
      left join stations s
        on dm.station_code = s.station_code
      where dm.measurement_datetime is not null
      order by dm.station_code, measurement_date
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

if (nrow(measurement_dates) == 0) {
  stop("No discharge measurement dates found in discharge_measurements.")
}

measurement_dates <- measurement_dates %>%
  mutate(
    station_code = as.character(station_code),
    measurement_date = safe_as_date(measurement_date)
  ) %>%
  filter(!is.na(station_code), !is.na(measurement_date))

if (!is.na(uf_filter)) {
  measurement_dates <- measurement_dates %>%
    filter(uf == uf_filter)
}

if (!is.na(basin_code_filter)) {
  measurement_dates <- measurement_dates %>%
    filter(as.character(basin_code) == as.character(basin_code_filter))
}

if (nrow(measurement_dates) == 0) {
  stop("No measurement dates left after filters.")
}

readr::write_csv(measurement_dates, candidate_dates_file)

# ============================================================
# Build candidate clusters
# ============================================================

cat("\nBuilding cross-section download clusters...\n")

station_keys <- measurement_dates %>%
  distinct(station_code, station_name, uf, basin_code) %>%
  arrange(station_code)

cluster_list <- vector("list", nrow(station_keys))

for (i in seq_len(nrow(station_keys))) {
  station_code_i <- station_keys$station_code[i]

  dates_i <- measurement_dates %>%
    filter(station_code == station_code_i) %>%
    pull(measurement_date)

  clusters_i <- build_cross_section_clusters_one_station(
    station_dates = dates_i,
    window_days = cross_section_window_days,
    merge_gap_days = merge_gap_days,
    max_cluster_days = max_cluster_days
  )

  if (nrow(clusters_i) > 0) {
    clusters_i$station_code <- station_code_i
    clusters_i$station_name <- station_keys$station_name[i]
    clusters_i$uf <- station_keys$uf[i]
    clusters_i$basin_code <- station_keys$basin_code[i]

    clusters_i <- clusters_i %>%
      select(
        station_code,
        station_name,
        uf,
        basin_code,
        cluster_id,
        date_start,
        date_end,
        n_measurement_dates
      )
  }

  cluster_list[[i]] <- clusters_i
}

candidate_clusters_all <- dplyr::bind_rows(cluster_list) %>%
  mutate(
    station_code = as.character(station_code),
    date_start = safe_as_date(date_start),
    date_end = safe_as_date(date_end),
    n_cluster_days = as.numeric(date_end - date_start) + 1
  ) %>%
  arrange(station_code, date_start, date_end)

if (nrow(candidate_clusters_all) == 0) {
  stop("No cross-section candidate clusters were created.")
}

readr::write_csv(candidate_clusters_all, candidate_clusters_file)

# ============================================================
# Remove already attempted clusters
# ============================================================

attempted_cluster_status <- read_attempted_cluster_status(request_log_file)
readr::write_csv(attempted_cluster_status, cluster_status_file)

station_status_for_display <- summarise_station_status(attempted_cluster_status)
readr::write_csv(station_status_for_display, station_status_file)

attempted_clusters <- attempted_cluster_status %>%
  select(station_code, date_start, date_end) %>%
  distinct()

failed_clusters <- attempted_cluster_status %>%
  filter(cluster_download_status == "failed") %>%
  select(station_code, date_start, date_end) %>%
  distinct()

completed_clusters <- attempted_cluster_status %>%
  filter(cluster_download_status == "completed") %>%
  select(station_code, date_start, date_end) %>%
  distinct()

candidate_clusters_remaining <- candidate_clusters_all %>%
  mutate(
    date_start = as.character(safe_as_date(date_start)),
    date_end = as.character(safe_as_date(date_end))
  ) %>%
  anti_join(attempted_clusters, by = c("station_code", "date_start", "date_end")) %>%
  mutate(
    date_start = safe_as_date(date_start),
    date_end = safe_as_date(date_end)
  ) %>%
  arrange(station_code, date_start, date_end)

remaining_station_codes <- candidate_clusters_remaining %>%
  distinct(station_code) %>%
  arrange(station_code)

n_all_candidate_stations <- n_distinct(candidate_clusters_all$station_code)
n_all_candidate_clusters <- nrow(candidate_clusters_all)
n_already_attempted_clusters <- nrow(attempted_clusters)
n_failed_clusters <- nrow(failed_clusters)
n_completed_clusters <- nrow(completed_clusters)
n_remaining_candidate_stations <- nrow(remaining_station_codes)
n_remaining_candidate_clusters <- nrow(candidate_clusters_remaining)

if (is.finite(max_stations)) {
  selected_station_codes <- remaining_station_codes %>%
    slice_head(n = max_stations) %>%
    pull(station_code)

  candidate_clusters <- candidate_clusters_remaining %>%
    filter(station_code %in% selected_station_codes)
} else {
  candidate_clusters <- candidate_clusters_remaining
}

if (nrow(candidate_clusters) == 0) {
  stop(
    "No new cross-section candidate clusters available. ",
    "All clusters were already attempted or filtered out."
  )
}

candidate_stations <- candidate_clusters %>%
  distinct(station_code, station_name, uf, basin_code) %>%
  arrange(station_code)

total_expected_requests <- nrow(candidate_clusters)

total_all_expected_requests <- n_remaining_candidate_clusters

readr::write_csv(candidate_clusters, candidate_clusters_file)

cat("\n============================================================\n")
cat("Cross-section candidate selection\n")
cat("============================================================\n")
cat("Route:                              ", route_name, "\n")
cat("All candidate stations:             ", n_all_candidate_stations, "\n")
cat("All candidate clusters:             ", n_all_candidate_clusters, "\n")
cat("Already attempted clusters:         ", n_already_attempted_clusters, "\n")
cat("Completed clusters:                 ", n_completed_clusters, "\n")
cat("Failed clusters reserved later:     ", n_failed_clusters, "\n")
cat("Remaining new stations:             ", n_remaining_candidate_stations, "\n")
cat("Remaining new clusters:             ", n_remaining_candidate_clusters, "\n")
cat("Stations in this run:               ", nrow(candidate_stations), "\n")
cat("Clusters in this run:               ", nrow(candidate_clusters), "\n")
cat("Window days around measurements:    ", cross_section_window_days, "\n")
cat("Merge gap days:                     ", merge_gap_days, "\n")
cat("Maximum cluster days:               ", max_cluster_days, "\n")
cat("Progress update interval:           ", progress_every_requests, " requests\n")
cat("Candidate dates file:               ", candidate_dates_file, "\n")
cat("Candidate clusters file:            ", candidate_clusters_file, "\n")
cat("Cluster status file:                ", cluster_status_file, "\n")

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
cat("Proceeding to cross-section download loop...\n")

# ============================================================
# Download loop
# ============================================================

run_start_time <- Sys.time()
stop_requested <- FALSE

cat("\n============================================================\n")
cat("Starting cross-section download\n")
cat("============================================================\n")
cat("Started at: ", as.character(run_start_time), "\n")

n_requested <- 0L
n_success <- 0L
n_failed <- 0L
n_positive <- 0L

for (station_index in seq_len(nrow(candidate_stations))) {
  if (isTRUE(stop_requested)) {
    break
  }

  this_station <- candidate_stations[station_index, ]
  station_code <- as.character(this_station$station_code)

  station_clusters <- candidate_clusters %>%
    filter(station_code == this_station$station_code) %>%
    arrange(date_start, date_end)

  station_start_time <- Sys.time()
  station_requested_before <- n_requested
  station_success_before <- n_success
  station_failed_before <- n_failed
  station_positive_before <- n_positive
  station_consecutive_failures <- 0L

  cat("\n============================================================\n")
  cat("Station ", station_index, " of ", nrow(candidate_stations), ": ", station_code, "\n", sep = "")
  cat("Name:     ", this_station$station_name, "\n", sep = "")
  cat("UF:       ", this_station$uf, "\n", sep = "")
  cat("Clusters: ", nrow(station_clusters), "\n", sep = "")
  cat("============================================================\n")

  for (cluster_index in seq_len(nrow(station_clusters))) {
    if (isTRUE(stop_requested)) {
      break
    }

    if (station_consecutive_failures >= max_consecutive_failures_per_station) {
      cat(
        "    Reached ",
        max_consecutive_failures_per_station,
        " consecutive failures for this station. Moving on.\n",
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

    this_cluster <- station_clusters[cluster_index, ]

    date_start <- safe_as_date(this_cluster$date_start)
    date_end <- safe_as_date(this_cluster$date_end)
    cluster_id <- this_cluster$cluster_id
    n_measurement_dates <- this_cluster$n_measurement_dates

    raw_file <- make_raw_file(
      station_code = station_code,
      date_start = date_start,
      date_end = date_end
    )

    log_row <- download_one_request(
      station_code = station_code,
      date_start = date_start,
      date_end = date_end,
      cluster_id = cluster_id,
      n_measurement_dates = n_measurement_dates,
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
          date_start = date_start,
          date_end = date_end,
          cluster_id = cluster_id,
          n_measurement_dates = n_measurement_dates,
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
      station_consecutive_failures <- 0L
    } else {
      n_failed <- n_failed + 1L
      station_consecutive_failures <- station_consecutive_failures + 1L

      cat(
        "    Failed request: ",
        station_code, " | ", route_name, " | ",
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
        done = n_requested,
        total = total_expected_requests,
        run_start_time = run_start_time,
        label = "Overall request progress"
      )
    }

    Sys.sleep(request_sleep_seconds)
  }

  station_elapsed_seconds <- as.numeric(difftime(Sys.time(), station_start_time, units = "secs"))

  cat("\nStation completed: ", station_code, "\n", sep = "")
  cat("  Station:    ", station_index, " of ", nrow(candidate_stations), "\n", sep = "")
  cat("  New req.:   ", n_requested - station_requested_before, "\n", sep = "")
  cat("  Success:    ", n_success - station_success_before, "\n", sep = "")
  cat("  Failed:     ", n_failed - station_failed_before, "\n", sep = "")
  cat("  With items: ", n_positive - station_positive_before, "\n", sep = "")
  cat("  Time:       ", format_duration(station_elapsed_seconds), "\n", sep = "")

  print_progress_status(
    done = n_requested,
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

if (n_requested >= min_new_requests_for_time_estimate) {
  average_seconds_per_request <- run_elapsed_seconds / n_requested
  estimated_seconds_all_candidates <- total_all_expected_requests * average_seconds_per_request
} else {
  average_seconds_per_request <- NA_real_
  estimated_seconds_all_candidates <- NA_real_
}

average_seconds_label <- if (is.na(average_seconds_per_request)) {
  "Not estimated; too few requests"
} else {
  as.character(round(average_seconds_per_request, 3))
}

estimated_total_time_label <- if (is.na(estimated_seconds_all_candidates)) {
  "Not estimated; too few requests"
} else {
  format_duration(estimated_seconds_all_candidates)
}

cat("\n============================================================\n")
cat("Cross-section download run summary\n")
cat("============================================================\n")
cat("Route:                         ", route_name, "\n")
cat("Started at:                    ", as.character(run_start_time), "\n")
cat("Finished at:                   ", as.character(run_end_time), "\n")
cat("Elapsed time:                  ", format_duration(run_elapsed_seconds), "\n")
cat("Average sec/request:           ", average_seconds_label, "\n")
cat("Expected requests this run:    ", total_expected_requests, "\n")
cat("Completed this run:            ", n_requested, "\n")
cat("Successful requests:           ", n_success, "\n")
cat("Failed requests:               ", n_failed, "\n")
cat("Requests with items:           ", n_positive, "\n")
cat("\nRemaining candidate estimate at current speed:\n")
cat("All candidate stations:        ", n_all_candidate_stations, "\n")
cat("All candidate clusters:        ", n_all_candidate_clusters, "\n")
cat("Already attempted clusters:    ", n_already_attempted_clusters, "\n")
cat("Completed clusters:            ", n_completed_clusters, "\n")
cat("Failed clusters reserved later:", n_failed_clusters, "\n")
cat("Remaining new stations:        ", n_remaining_candidate_stations, "\n")
cat("Remaining new clusters:        ", n_remaining_candidate_clusters, "\n")
cat("Estimated total time remaining:", estimated_total_time_label, "\n")
cat("\nFiles updated by 044:\n")
cat("Candidate dates:               ", candidate_dates_file, "\n")
cat("Candidate clusters:            ", candidate_clusters_file, "\n")
cat("Request log:                   ", request_log_file, "\n")
cat("Raw JSON folder:               ", raw_output_dir, "\n")
cat("Cluster status:                ", cluster_status_file, "\n")
cat("Station status:                ", station_status_file, "\n")
cat("\nDone.\n")

gc()
