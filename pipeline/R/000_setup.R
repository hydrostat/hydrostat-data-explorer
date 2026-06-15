# ============================================================
# pipeline/R/000_setup.R
# Purpose: Load packages and define shared paths/configuration
# ============================================================

# Load packages
library(httr2)
library(readr)
library(tibble)

# Define base API settings
ana_base_url <- "https://www.ana.gov.br/hidrowebservice/EstacoesTelemetricas"
ana_auth_route <- "/OAUth/v1"
ana_auth_url <- paste0(ana_base_url, ana_auth_route)

# Define local paths
config_dir <- "config"
logs_dir <- "logs"
token_cache_file <- file.path(config_dir, "ana_token_cache.rds")
auth_log_file <- file.path(logs_dir, "auth_log.csv")

# Token validity confirmed by ANA manual: 60 minutes.
# Use a safety margin and refresh after 55 minutes.
token_valid_minutes <- 55

# Ensure required local folders exist
dir.create(config_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(logs_dir, showWarnings = FALSE, recursive = TRUE)

log_auth_attempt <- function(success, http_code = NA_integer_, message = NA_character_) {
  log_row <- tibble(
    datetime = as.character(Sys.time()),
    route = ana_auth_route,
    http_code = http_code,
    success = success,
    message = message
  )

  if (file.exists(auth_log_file)) {
    write_csv(log_row, auth_log_file, append = TRUE)
  } else {
    write_csv(log_row, auth_log_file)
  }
}
