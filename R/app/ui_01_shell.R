# ============================================================
# ui_01_shell.R
# Purpose: App head, header, sidebar, and main UI shell
# ============================================================

# ============================================================
# app_head_ui
# Purpose: Create CSS, viewport, and critical inline layout styles.
# ============================================================

app_head_ui <- function() {
  tags$head(
    shiny::includeCSS(file.path("www", "styles.css")),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1")
  )
}

# ============================================================
# app_header_ui
# Purpose: Create the application title and About action.
# ============================================================

app_header_ui <- function() {
  div(
    class = "app-header",
    div(
      class = "title-block",
      h1(
        class = "app-title-logo",
        span(class = "logo-hydro", "hydro"),
        span(class = "logo-underscore", "_"),
        span(class = "logo-stat", "stat"),
        span(class = "logo-app-name", "Data Explorer")
      ),
      p(app_config$app_subtitle),
      p(class = "app-version-note", "Sistema integrado para visualização, triagem e análise de dados hidrológicos.")
    ),
    div(
      class = "header-actions",
      actionButton(
        inputId = "open_about_modal",
        label = "Sobre",
        icon = icon("circle-info"),
        class = "header-about-button"
      )
    )
  )
}

# ============================================================
# app_sidebar_ui
# Purpose: Create station search, station summary, and sidebar downloads.
# ============================================================

app_sidebar_ui <- function() {
  column(
    width = 3,
    div(
      class = "side-panel",
      
      div(
        class = "control-card",
        selectizeInput(
          inputId = "station_select",
          label = "Buscar estação",
          choices = NULL,
          selected = NULL,
          options = list(
            placeholder = "Código, nome, UF ou município",
            maxOptions = 1000
          )
        ),
        actionButton(
          inputId = "go_to_map",
          label = "Ver no mapa",
          icon = icon("map-location-dot"),
          class = "btn-default btn-block sidebar-map-button"
        )
      ),
      
      div(
        class = "station-card",
        uiOutput("station_title"),
        uiOutput("station_kpis"),
        
        h4("Produtos e disponibilidade"),
        uiOutput("station_availability_badges"),
        
        h4("Resumo cadastral"),
        uiOutput("station_metadata"),
        
        tags$details(
          class = "details-card",
          tags$summary("Ver metadados completos"),
          uiOutput("station_metadata_details")
        ),
        
        tags$details(
          class = "details-card",
          tags$summary("Ver indicadores de triagem"),
          uiOutput("station_attention")
        ),
        
        downloadButton(
          outputId = "download_station_summary",
          label = "Baixar resumo CSV",
          class = "btn-primary btn-block"
        )
      )
    )
  )
}

app_main_ui <- function() {
  column(
    width = 9,
    div(
      class = "main-panel-card",
      tabsetPanel(
        id = "main_tabs",
        type = "tabs",
        app_map_tab_ui(),
        app_rating_summary_tab_ui(),
        app_fluviometric_tab_ui(),
        app_pluviometric_tab_ui()
      )
    )
  )
}
