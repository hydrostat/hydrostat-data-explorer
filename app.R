# ============================================================
# app.R
# Purpose: Public HydroStat Data Explorer Shiny entry point
# ============================================================

source(file.path("R", "app_config.R"))

# Reuse the local diagnostic functions created during stage 09,
# but keep them in an isolated environment to avoid name conflicts
# with Shiny helper functions.
if (file.exists(app_config$diagnostic_functions_path)) {
  source(app_config$diagnostic_functions_path, local = app_diagnostic_env)
}

source(file.path("R", "app_data.R"))
source(file.path("R", "app_ui.R"))
source(file.path("R", "app_server.R"))

shinyApp(ui = ui, server = server)
