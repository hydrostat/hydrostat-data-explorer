# ============================================================
# app_data.R
# Purpose: Ordered loader for app data and acquisition helpers
# ============================================================

# Keep these sources in order because later helpers depend on
# objects and functions defined by earlier fragments.
source(file.path("R", "app", "data_01_core.R"), local = TRUE)
source(file.path("R", "app", "data_02_station.R"), local = TRUE)
source(file.path("R", "app", "data_03_tables.R"), local = TRUE)
source(file.path("R", "app", "data_04_fluviometric.R"), local = TRUE)
source(file.path("R", "app", "data_05_ana_api.R"), local = TRUE)
source(file.path("R", "app", "data_06_pluviometric.R"), local = TRUE)

