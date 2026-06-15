# ============================================================
# app_ui.R
# Purpose: Assemble the HydroStat Data Explorer user interface
# ============================================================

source(file.path("R", "app", "ui_01_shell.R"), local = TRUE)
source(file.path("R", "app", "ui_02_map_rating.R"), local = TRUE)
source(file.path("R", "app", "ui_03_fluviometric.R"), local = TRUE)
source(file.path("R", "app", "ui_04_pluviometric.R"), local = TRUE)

ui <- fluidPage(
  app_head_ui(),
  div(
    class = "app-shell",
    app_header_ui(),
    fluidRow(
      app_sidebar_ui(),
      app_main_ui()
    )
  )
)

