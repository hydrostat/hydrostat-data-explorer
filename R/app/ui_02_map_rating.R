# ============================================================
# ui_02_map_rating.R
# Purpose: Map and rating/discharge-summary user interfaces
# ============================================================

# ============================================================
# app_map_tab_ui
# Purpose: Create the shared station map tab.
# ============================================================

app_map_tab_ui <- function() {
  tabPanel(
    title = "Mapa",
    value = "Mapa",
    br(),
    div(
      class = "section-card map-card",
      div(
        class = "section-header",
        h3("Mapa de estações"),
        p("Clique em uma estação para atualizar a seleção. A estação selecionada é compartilhada por todos os ambientes de análise.")
      ),
      div(
        class = "map-layout",
        leafletOutput("station_map", height = "720px"),
        tags$details(
          class = "map-layer-panel",
          open = "open",
          tags$summary("Camadas"),
          tags$div(
            class = "map-layer-panel-body",
            
            tags$div(
              class = "map-layer-box",
              tags$h4("Tipos de posto"),
              checkboxGroupInput(
                inputId = "map_station_layers",
                label = NULL,
                choices = c(
                  "Postos fluviométricos" = "flu_stations",
                  "Postos pluviométricos" = "rainfall_stations"
                ),
                selected = c("flu_stations", "rainfall_stations")
              ),
            ),
            tags$div(
              class = "map-layer-box",
              tags$h4("Camadas espaciais"),
              uiOutput("map_spatial_layer_controls")
            )
          )
        )
      )
    )
  )
}

# ============================================================
# app_rating_summary_tab_ui
# Purpose: Create rating-curve, discharge, diagnostics, and cross-section tabs.
# ============================================================

app_rating_summary_tab_ui <- function() {
  tabPanel(
    title = "Curvas-chave e resumo de descarga",
    value = "Curvas-chave e resumo de descarga",
    br(),
    tabsetPanel(
      id = "rating_summary_tabs",
      type = "pills",
      
      tabPanel(
        title = "Visão geral",
        value = "Visão geral",
        br(),
        div(
          class = "section-card",
          div(
            class = "section-header",
            h3("Visão geral de curvas-chave e medições de descarga"),
            p("Síntese dos produtos atualmente disponíveis para a estação selecionada.")
          ),
          uiOutput("discharge_rating_overview")
        )
      ),
      
      tabPanel(
        title = "Medições de descarga",
        value = "Medições de descarga",
        br(),
        div(
          class = "section-card",
          div(
            class = "section-header",
            h3("Medições de descarga"),
            p("Visualização exploratória das medições disponíveis para a estação selecionada.")
          ),
          fluidRow(
            column(
              6,
              div(class = "plot-card measurement-plot-card", plotOutput("discharge_measurements_by_year_plot", height = "360px"))
            ),
            column(
              6,
              div(class = "plot-card measurement-plot-card", plotOutput("diagnostic_flags_stage_discharge_plot", height = "360px"))
            )
          ),
          fluidRow(
            column(
              6,
              div(class = "plot-card measurement-plot-card", plotOutput("arh23_stage_measurements_plot", height = "360px"))
            ),
            column(
              6,
              div(class = "plot-card measurement-plot-card", plotOutput("mean_velocity_stage_measurements_plot", height = "360px"))
            )
          ),
          tags$details(
            class = "details-card details-card-main",
            open = "open",
            tags$summary("Dados tabulares das medições"),
            uiOutput("measurement_table_status"),
            DTOutput("measurement_table")
          )
        )
      ),
      
      tabPanel(
        title = "Curvas-chave",
        value = "Curvas-chave",
        br(),
        div(
          class = "section-card",
          div(
            class = "section-header",
            h3("Curvas-chave")
          ),
          div(
            class = "control-card",
            uiOutput("rating_curve_selector_ui")
          ),
          div(
            class = "rating-curve-plot-grid",
            div(class = "plot-card plot-card-feature", plotOutput("rating_curves_and_measurements_plot", height = "540px")),
            div(class = "plot-card plot-card-feature", plotOutput("rating_curve_validity_timeline_plot", height = "320px"))
          ),
          tags$details(
            class = "details-card details-card-main",
            open = "open",
            tags$summary("Resumo das curvas-chave"),
            uiOutput("rating_curve_summary_table_status"),
            DTOutput("rating_curve_summary_table")
          )
        )
      ),
      
      tabPanel(
        title = "Diagnósticos",
        value = "Diagnósticos",
        br(),
        div(
          class = "section-card",
          div(
            class = "section-header section-header-with-help",
            div(
              h3("Triagem diagnóstica"),
              p("Os diagnósticos detalhados são calculados apenas para a estação selecionada.")
            ),
            actionButton(
              inputId = "open_diagnostic_help_modal",
              label = "Ajuda",
              icon = icon("circle-question"),
              class = "section-help-button"
            )
          ),
          uiOutput("diagnostic_overview_cards"),
          fluidRow(
            column(
              6,
              div(class = "plot-card measurement-plot-card", plotOutput("rating_curves_with_residual_envelopes_plot", height = "380px"))
            ),
            column(
              6,
              div(class = "plot-card measurement-plot-card", plotOutput("residual_temporal_regime_residual_discharge_plot", height = "380px"))
            )
          ),
          fluidRow(
            column(
              6,
              div(class = "plot-card measurement-plot-card", plotOutput("residual_temporal_regime_residual_time_plot", height = "380px"))
            ),
            column(
              6,
              div(class = "plot-card measurement-plot-card", plotOutput("residual_temporal_regime_stage_discharge_plot", height = "380px"))
            )
          ),
          tags$details(
            class = "details-card details-card-main",
            open = "open",
            tags$summary("Índices diagnósticos"),
            uiOutput("diagnostic_indices_table_status"),
            DTOutput("diagnostic_indices_table")
          ),
          tags$details(
            class = "details-card details-card-main",
            open = "open",
            tags$summary("Resumo técnico do diagnóstico"),
            uiOutput("diagnostic_summary_table_status"),
            DTOutput("diagnostic_summary_table")
          ),
          tags$details(
            class = "details-card details-card-main",
            open = "open",
            tags$summary("Medições sinalizadas"),
            uiOutput("diagnostic_flags_table_status"),
            DTOutput("diagnostic_flags_table")
          ),
          tags$details(
            class = "details-card details-card-main",
            open = "open",
            tags$summary("Grupos repetidos"),
            fluidRow(
              column(6, h4("Cota repetida"), uiOutput("diagnostic_repeated_stage_table_status"), DTOutput("diagnostic_repeated_stage_table")),
              column(6, h4("Vazão repetida"), uiOutput("diagnostic_repeated_discharge_table_status"), DTOutput("diagnostic_repeated_discharge_table"))
            )
          )
        )
      ),
      
      tabPanel(
        title = "Seções transversais",
        value = "Seções transversais",
        br(),
        div(
          class = "section-card",
          div(
            class = "section-header",
            h3("Seções transversais"),
            p("Visualização dos perfis transversais disponíveis para a estação selecionada.")
          ),
          
          div(
            class = "control-card",
            uiOutput("cross_section_selector_ui")
          ),
          
          fluidRow(
            column(
              6,
              div(
                class = "plot-card",
                plotOutput("cross_section_selected_profile_plot", height = "340px")
              )
            ),
            column(
              6,
              div(
                class = "plot-card",
                plotOutput("cross_section_selected_rating_curve_plot", height = "340px")
              )
            )
          ),
          
          fluidRow(
            column(
              6,
              div(
                class = "plot-card",
                plotOutput("cross_section_overlay_plot", height = "340px")
              )
            ),
            column(
              6,
              div(
                class = "plot-card",
                plotOutput("cross_section_temporal_plot", height = "340px")
              )
            )
          ),
          
          tags$details(
            class = "details-card details-card-main",
            open = "open",
            tags$summary("Dados tabulares das seções transversais"),
            uiOutput("cross_section_table_status"),
            DTOutput("cross_section_table")
          )
        )
      )
    )
  )
}

