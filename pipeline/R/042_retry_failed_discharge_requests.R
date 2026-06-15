# ============================================================
# Retry failed discharge-related ANA HidroWebService requests
# Stage: 05_discharge_measurements
#
# Purpose:
# Retry only failed request windows already registered in
# data/processed/discharge_request_log.csv.
#
# Current default:
# - Retry failed windows for resumo_descarga.
#
# Important design choices:
# - Retry is by request window:
#   station_code + route_name + date_start + date_end.
# - A failed window is retried only if no successful request exists
#   for the same key.
# - Previous failed rows are preserved in the log.
# - New retry attempts are appended to the same request log.
# - Raw JSON files are overwritten for the same station-route-window.
# - Data requests use curl directly instead of httr2.
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
library(curl)

# ============================================================
# Parameters
# ============================================================

base_url <- "https://www.ana.gov.br/hidrowebservice/EstacoesTelemetricas"

raw_output_dir <- file.path("data", "raw", "discharge_routes")
processed_dir <- file.path("data", "processed")

request_log_file <- file.path(
  processed_dir,
  "discharge_request_log.csv"
)

retry_candidates_file <- file.path(
  processed_dir,
  "discharge_retry_failed_candidates.csv"
)

retry_summary_file <- file.path(
  processed_dir,
  "discharge_retry_failed_summary.csv"
)

# Current default route for retry.
# Change to c("curva_descarga") later if needed.
route_filter <- c("curva_descarga")

# Maximum failed windows to retry in this run.
# Use Inf to retry all remaining failures.
max_failed_requests <- 1000

# A conservative pause for retry runs.
request_sleep_seconds <- 0.2

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

if (!file.exists(request_log_file)) {
  stop("Missing request log file: ", request_log_file)
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

  invisible(TRUE)
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
# Build retry candidates
# ============================================================

cat("\nReading request log...\n")

request_log <- readr::read_csv(
  request_log_file,
  col_types = readr::cols(
    station_code = readr::col_character(),
    route_name = readr::col_character(),
    endpoint = readr::col_character(),
    date_start = readr::col_character(),
    date_end = readr::col_character(),
    http_status = readr::col_double(),
    api_status = readr::col_character(),
    api_code = readr::col_character(),
    api_message = readr::col_character(),
    n_items = readr::col_double(),
    success = readr::col_logical(),
    error_message = readr::col_character(),
    raw_file = readr::col_character(),
    requested_at = readr::col_character(),
    downloaded_at = readr::col_character()
  )
)

if (nrow(request_log) == 0) {
  stop("Request log is empty.")
}

latest_status <- request_log %>%
  mutate(
    row_id = dplyr::row_number(),
    station_code = as.character(station_code),
    route_name = as.character(route_name),
    date_start = as.character(safe_as_date(date_start)),
    date_end = as.character(safe_as_date(date_end)),
    success = as.logical(success)
  ) %>%
  group_by(station_code, route_name, date_start, date_end) %>%
  summarise(
    ever_success = any(success, na.rm = TRUE),
    last_row_id = max(row_id, na.rm = TRUE),
    last_success = success[which.max(row_id)],
    last_http_status = http_status[which.max(row_id)],
    last_error = error_message[which.max(row_id)],
    n_attempts = dplyr::n(),
    .groups = "drop"
  )

retry_candidates <- latest_status %>%
  filter(
    route_name %in% route_filter,
    !ever_success
  ) %>%
  left_join(routes, by = "route_name") %>%
  mutate(
    date_start = safe_as_date(date_start),
    date_end = safe_as_date(date_end)
  ) %>%
  arrange(station_code, route_name, date_start, date_end)

total_retry_candidates <- nrow(retry_candidates)

if (total_retry_candidates == 0) {
  cat("\nNo failed request windows to retry for route_filter: ",
      paste(route_filter, collapse = ", "), "\n", sep = "")
  quit(save = "no", status = 0)
}

if (is.finite(max_failed_requests)) {
  retry_candidates_run <- retry_candidates %>%
    slice_head(n = max_failed_requests)
} else {
  retry_candidates_run <- retry_candidates
}

readr::write_csv(retry_candidates, retry_candidates_file)

retry_error_summary <- retry_candidates %>%
  count(last_http_status, last_error, sort = TRUE)

readr::write_csv(retry_error_summary, retry_summary_file)

cat("\n============================================================\n")
cat("Failed request retry selection\n")
cat("============================================================\n")
cat("Route filter:                    ", paste(route_filter, collapse = ", "), "\n")
cat("Total failed windows to retry:    ", total_retry_candidates, "\n")
cat("Failed windows in this run:       ", nrow(retry_candidates_run), "\n")
cat("Distinct stations in this run:    ", dplyr::n_distinct(retry_candidates_run$station_code), "\n")
cat("Retry candidates file:            ", retry_candidates_file, "\n")
cat("Retry summary file:               ", retry_summary_file, "\n")
cat("Progress update interval:         ", progress_every_requests, " requests\n")
cat("Request sleep seconds:            ", request_sleep_seconds, "\n")

cat("\nFailure summary before retry:\n")
print(retry_error_summary, n = 30)

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

# ============================================================
# Retry loop
# ============================================================

run_start_time <- Sys.time()

cat("\n============================================================\n")
cat("Starting retry\n")
cat("============================================================\n")
cat("Started at: ", as.character(run_start_time), "\n")

n_requested <- 0L
n_success <- 0L
n_failed <- 0L
n_positive <- 0L

total_expected_requests <- nrow(retry_candidates_run)

for (i in seq_len(nrow(retry_candidates_run))) {
  this_request <- retry_candidates_run[i, ]

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
        "\nFailed to refresh ANA token. Stopping retry safely.\n",
        "Reason: ", conditionMessage(refreshed_token), "\n",
        sep = ""
      )
      break
    }

    current_token <- refreshed_token
    token_created_at <- Sys.time()
    cat("ANA token refreshed. Characters: ", nchar(current_token), "\n", sep = "")
  }

  station_code <- as.character(this_request$station_code)
  route_name <- as.character(this_request$route_name)
  endpoint <- as.character(this_request$endpoint)
  use_tipo_filtro_data <- as.logical(this_request$use_tipo_filtro_data)
  date_start <- safe_as_date(this_request$date_start)
  date_end <- safe_as_date(this_request$date_end)

  raw_file <- make_raw_file(
    route_name = route_name,
    station_code = station_code,
    date_start = date_start,
    date_end = date_end
  )

  log_row <- download_one_request(
    station_code = station_code,
    route_name = route_name,
    endpoint = endpoint,
    use_tipo_filtro_data = use_tipo_filtro_data,
    date_start = date_start,
    date_end = date_end,
    raw_file = raw_file,
    token = current_token
  )

  # If the token is rejected during a data request, refresh once and retry
  # before writing the final log row.
  if (!is.na(log_row$http_status[1]) && log_row$http_status[1] == 401) {
    cat("  HTTP 401 received. Refreshing token and retrying this request once.\n")

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
        route_name = route_name,
        endpoint = endpoint,
        use_tipo_filtro_data = use_tipo_filtro_data,
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
  } else {
    n_failed <- n_failed + 1L

    cat(
      "  Failed retry: ",
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
      label = "Retry progress"
    )
  }

  Sys.sleep(request_sleep_seconds)
}

# ============================================================
# Final message
# ============================================================

run_end_time <- Sys.time()
run_elapsed_seconds <- as.numeric(difftime(run_end_time, run_start_time, units = "secs"))

if (n_requested >= min_new_requests_for_time_estimate) {
  average_seconds_per_request <- run_elapsed_seconds / n_requested
  estimated_seconds_all_remaining <- total_retry_candidates * average_seconds_per_request
} else {
  average_seconds_per_request <- NA_real_
  estimated_seconds_all_remaining <- NA_real_
}

average_seconds_label <- if (is.na(average_seconds_per_request)) {
  "Not estimated; too few requests"
} else {
  as.character(round(average_seconds_per_request, 3))
}

estimated_total_time_label <- if (is.na(estimated_seconds_all_remaining)) {
  "Not estimated; too few requests"
} else {
  format_duration(estimated_seconds_all_remaining)
}

cat("\n============================================================\n")
cat("Retry run summary\n")
cat("============================================================\n")
cat("Route filter:                 ", paste(route_filter, collapse = ", "), "\n")
cat("Started at:                   ", as.character(run_start_time), "\n")
cat("Finished at:                  ", as.character(run_end_time), "\n")
cat("Elapsed time:                 ", format_duration(run_elapsed_seconds), "\n")
cat("Average sec/request:          ", average_seconds_label, "\n")
cat("Total failed windows before:  ", total_retry_candidates, "\n")
cat("Retried this run:             ", n_requested, "\n")
cat("Successful retries:           ", n_success, "\n")
cat("Failed retries:               ", n_failed, "\n")
cat("Retries with items:           ", n_positive, "\n")
cat("Estimated total retry time:   ", estimated_total_time_label, "\n")
cat("\nFiles updated by retry script:\n")
cat("Request log:                  ", request_log_file, "\n")
cat("Raw JSON folder:              ", raw_output_dir, "\n")
cat("Retry candidates file:        ", retry_candidates_file, "\n")
cat("Retry summary file:           ", retry_summary_file, "\n")
cat("\nAfter this run, recompute remaining failed windows using the diagnostic query.\n")

gc()

cat("\nDone.\n")
