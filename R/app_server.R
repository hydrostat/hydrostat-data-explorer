# ============================================================
# app_server.R
# Purpose: Ordered server loader for HydroStat Data Explorer
# ============================================================

server <- function(input, output, session) {
  # All fragments are evaluated in this server invocation frame.
  # This preserves access to input, output, session, reactive values,
  # and objects created by previously sourced fragments.
  source(file.path("R", "app", "server_01_core_api.R"), local = TRUE)
  source(file.path("R", "app", "server_02_station_map.R"), local = TRUE)
  source(file.path("R", "app", "server_03_acquisition_series_pluviometric.R"), local = TRUE)
  source(file.path("R", "app", "server_04_flu_consistency.R"), local = TRUE)
  source(file.path("R", "app", "server_05_flu_statistics.R"), local = TRUE)
  source(file.path("R", "app", "server_06_flu_extremes.R"), local = TRUE)
  source(file.path("R", "app", "server_07_station_outputs.R"), local = TRUE)
}

