# ============================================================
# ui_04_pluviometric.R
# Purpose: Pluviometric analysis user interface
# ============================================================

# ============================================================
# app_pluviometric_tab_ui
# Purpose: Create acquisition, series, consistency, statistics, and extreme-event tabs.
# ============================================================

app_pluviometric_tab_ui <- function() {
  tabPanel(
    title = "Análise de dados pluviométricos",
    value = "Análise de dados pluviométricos",
    br(),
    div(
      class = "section-card",
      div(
        class = "section-header section-header-with-help",
        div(
          h3("Análise de dados pluviométricos"),
          p("Ajuda geral para obtenção, visualização, falhas, estatísticas e máximos anuais de precipitação.")
        ),
        actionButton(
          inputId = "open_pluviometric_help_modal",
          label = "Ajuda",
          icon = icon("circle-question"),
          class = "section-help-button"
        )
      )
    ),
    tabsetPanel(
      id = "pluviometric_tabs",
      type = "pills",
      tabPanel(
        title = "Obtenção de dados",
        value = "Obtenção de dados",
        br(),
        div(
          class = "section-card",
          div(
            class = "section-header",
            h3("Obtenção de dados pluviométricos"),
            p("Forneça dados diários de precipitação para a estação atualmente selecionada. O código da estação no arquivo deve ser igual ao código selecionado no sistema.")
          ),
          div(
            class = "control-card",
            fluidRow(
              column(
                4,
                selectInput(
                  inputId = "pluviometric_data_source",
                  label = "Fonte dos dados",
                  choices = c(
                    "HidroWeb — arquivo ZIP completo" = "hidroweb_zip",
                    "HidroWeb — CSV de chuvas" = "hidroweb_rainfall_csv",
                    "ANA WebService — XML" = "ana_xml",
                    "ANA API — download autenticado" = "ana_api_download",
                    "ANA API — JSON enviado" = "ana_json"
                  ),
                  selected = "ana_xml"
                )
              ),
              column(
                8,
                conditionalPanel(
                  condition = "input.pluviometric_data_source == 'hidroweb_zip'",
                  fileInput(
                    inputId = "pluviometric_hidroweb_zip",
                    label = "Arquivo .zip exportado pelo HidroWeb",
                    accept = c(".zip")
                  )
                ),
                conditionalPanel(
                  condition = "input.pluviometric_data_source == 'hidroweb_rainfall_csv'",
                  fileInput(
                    inputId = "pluviometric_hidroweb_rainfall_csv",
                    label = "Arquivo CSV de chuvas do HidroWeb",
                    accept = c(".csv")
                  ),
                  helpText("Use o arquivo *_Chuvas.csv ou equivalente extraído do HidroWeb.")
                ),
                conditionalPanel(
                  condition = "input.pluviometric_data_source == 'ana_xml'",
                  radioButtons(
                    inputId = "pluviometric_xml_mode",
                    label = "Modo de obtenção",
                    choices = c(
                      "Enviar arquivo XML" = "upload",
                      "Download automático do WebService" = "download"
                    ),
                    selected = "download",
                    inline = TRUE
                  ),
                  conditionalPanel(
                    condition = "input.pluviometric_data_source == 'ana_xml' && input.pluviometric_xml_mode == 'upload'",
                    fileInput(
                      inputId = "pluviometric_xml_file",
                      label = "Arquivo XML do WebService da ANA",
                      accept = c(".xml")
                    )
                  )
                ),
                conditionalPanel(
                  condition = "input.pluviometric_data_source == 'ana_api_download'",
                  div(
                    class = "limitation-box",
                    strong("Download pela API ANA: "),
                    "o processo pode ser demorado para períodos longos. A chuva diária é baixada ano a ano. ",
                    "Se o token expirar, o download será pausado e poderá ser retomado do ponto em que parou."
                  ),
                  fluidRow(
                    column(
                      6,
                      textInput(
                        inputId = "pluviometric_api_identificador",
                        label = "Usuário / identificador ANA",
                        value = "",
                        placeholder = "CPF ou CNPJ autorizado na API"
                      )
                    ),
                    column(
                      6,
                      passwordInput(
                        inputId = "pluviometric_api_senha",
                        label = "Senha ANA",
                        value = ""
                      )
                    )
                  ),
                  div(
                    class = "extremes-download-row",
                    div(
                      class = "extremes-download-item",
                      actionButton(
                        inputId = "pluviometric_api_get_token",
                        label = "Obter token",
                        icon = icon("key"),
                        class = "btn-primary"
                      )
                    )
                  ),
                  uiOutput("pluviometric_api_token_status"),
                  radioButtons(
                    inputId = "pluviometric_api_period_mode",
                    label = "Período de download",
                    choices = c(
                      "Informar ano inicial e ano final" = "manual",
                      "Usar inventário da estação até o ano atual" = "inventory"
                    ),
                    selected = "inventory"
                  ),
                  conditionalPanel(
                    condition = "input.pluviometric_api_period_mode == 'manual'",
                    fluidRow(
                      column(
                        6,
                        numericInput(
                          inputId = "pluviometric_api_start_year",
                          label = "Ano inicial",
                          value = 1980,
                          min = 1900,
                          max = as.integer(format(Sys.Date(), "%Y")),
                          step = 1
                        )
                      ),
                      column(
                        6,
                        numericInput(
                          inputId = "pluviometric_api_end_year",
                          label = "Ano final",
                          value = as.integer(format(Sys.Date(), "%Y")),
                          min = 1900,
                          max = as.integer(format(Sys.Date(), "%Y")),
                          step = 1
                        )
                      )
                    )
                  ),
                  uiOutput("pluviometric_api_download_status"),
                  # div(
                  #   class = "download-button-row",
                  #   div(
                  #     class = "download-button-item",
                  #     downloadButton(
                  #       outputId = "pluviometric_api_download_report_download",
                  #       label = "Baixar relatório de download",
                  #       class = "btn-primary"
                  #     )
                  #   )
                  # )
                ),
                conditionalPanel(
                  condition = "input.pluviometric_data_source == 'ana_json'",
                  fileInput(
                    inputId = "pluviometric_json_file",
                    label = "Arquivo JSON da API ANA",
                    accept = c(".json")
                  )
                )
              )
            ),
            div(
              class = "extremes-download-row",
              div(
                class = "extremes-download-item",
                actionButton(
                  inputId = "pluviometric_process_data",
                  label = "Processar dados",
                  icon = icon("play"),
                  class = "btn-primary"
                )
              )
            ),
            uiOutput("pluviometric_processing_status")
          )
        ),
        uiOutput("pluviometric_acquisition_cards"),
        uiOutput("pluviometric_availability_section")
      ),
      tabPanel(
        title = "Séries de precipitação",
        value = "Séries de precipitação",
        br(),
        div(
          class = "section-card",
          div(
            class = "section-header",
            h3("Série diária de precipitação"),
            p("Hietograma diário calculado a partir dos dados carregados na sessão.")
          ),
          uiOutput("pluviometric_rainfall_series_status"),
          div(
            class = "plot-card",
            plotOutput("pluviometric_rainfall_series_plot", height = "430px")
          )
        ),
        uiOutput("pluviometric_rainfall_summary_cards")
      ),
      tabPanel(
        title = "Falhas e consistência",
        value = "Falhas e consistência",
        br(),
        div(
          class = "section-card",
          div(
            class = "section-header",
            h3("Falhas e consistência pluviométrica"),
            p("Triagem de cobertura temporal, valores ausentes, valores impossíveis, extremos suspeitos e padrões de repetição na série diária de precipitação.")
          ),
          uiOutput("pluviometric_consistency_status"),
          uiOutput("pluviometric_consistency_report_controls")
        ),
        uiOutput("pluviometric_consistency_coverage_cards"),
        uiOutput("pluviometric_consistency_value_cards"),
        tags$details(
          class = "details-card details-card-main",
          open = NA,
          tags$summary("Tabela de ocorrências de triagem"),
          div(
            class = "table-card",
            DT::DTOutput("pluviometric_consistency_issue_table")
          )
        )
      ),
      tabPanel(
        title = "Estatísticas mensais/anuais",
        value = "Estatísticas mensais/anuais",
        br(),
        div(
          class = "section-card",
          div(
            class = "section-header",
            h3("Estatísticas mensais e anuais de precipitação"),
            p("Acumulados mensais e anuais, dias chuvosos, sequências secas/úmidas e índices descritivos derivados da série diária.")
          ),
          uiOutput("pluviometric_stats_status"),
          uiOutput("pluviometric_stats_download_controls")
        ),
        uiOutput("pluviometric_stats_summary_cards"),
        tags$details(
          class = "details-card details-card-main",
          open = NA,
          tags$summary("Totais anuais e regime mensal"),
          fluidRow(
            column(
              6,
              div(
                class = "plot-card",
                plotOutput("pluviometric_stats_annual_total_plot", height = "320px")
              )
            ),
            column(
              6,
              div(
                class = "plot-card",
                plotOutput("pluviometric_stats_monthly_regime_plot", height = "320px")
              )
            )
          )
        ),
        tags$details(
          class = "details-card details-card-main",
          open = NA,
          tags$summary("Distribuição mensal e índices anuais"),
          fluidRow(
            column(
              6,
              div(
                class = "plot-card",
                plotOutput("pluviometric_stats_monthly_boxplot", height = "330px")
              )
            ),
            column(
              6,
              div(
                class = "plot-card",
                plotOutput("pluviometric_stats_annual_indices_plot", height = "330px")
              )
            )
          )
        ),
        tags$details(
          class = "details-card details-card-main",
          open = NA,
          tags$summary("Tabela mensal ano × mês"),
          div(
            class = "table-card",
            DT::DTOutput("pluviometric_stats_monthly_wide_table")
          )
        ),
        tags$details(
          class = "details-card details-card-main",
          open = NA,
          tags$summary("Tabela anual de índices"),
          div(
            class = "table-card",
            DT::DTOutput("pluviometric_stats_annual_table")
          )
        )
      ),
      tabPanel(
        title = "Eventos extremos",
        value = "Eventos extremos",
        br(),
        div(
          class = "section-card",
          div(
            class = "section-header",
            h3("Máximos anuais de precipitação"),
            p("Extração descritiva de máximos anuais de precipitação por ano hidrológico outubro/setembro. Não há ajuste de distribuição, tempo de retorno ou modelagem de frequência.")
          ),
          uiOutput("pluviometric_extremes_status"),
          div(
            class = "extremes-download-row",
            tags$div(
              class = "extremes-download-item",
              downloadButton(
                outputId = "pluviometric_extremes_annual_max_simple_download",
                label = "Máximos anuais",
                class = "btn-primary"
              )
            ),
            tags$div(
              class = "extremes-download-item",
              downloadButton(
                outputId = "pluviometric_extremes_annual_max_detailed_download",
                label = "Máximos anuais detalhados",
                class = "btn-primary"
              )
            )
          )
        ),
        uiOutput("pluviometric_extremes_summary_cards"),
        div(
          class = "section-card",
          div(
            class = "section-header",
            h3("Máximos anuais — ano hidrológico outubro/setembro"),
            p("Os pontos são coloridos pelo número de flags de triagem. Nenhum valor é excluído automaticamente.")
          ),
          div(
            class = "plot-card",
            plotOutput("pluviometric_extremes_annual_max_plot", height = "380px")
          )
        ),
        tags$details(
          class = "details-card details-card-main",
          open = NA,
          tags$summary("Tabela de máximos anuais"),
          div(
            class = "table-card",
            DT::DTOutput("pluviometric_extremes_annual_max_table")
          )
        )
      )
    )
  )
}

