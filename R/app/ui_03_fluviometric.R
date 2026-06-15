# ============================================================
# ui_03_fluviometric.R
# Purpose: Fluviometric analysis user interface
# ============================================================

# ============================================================
# app_fluviometric_tab_ui
# Purpose: Create acquisition, series, consistency, statistics, and extreme-event tabs.
# ============================================================

app_fluviometric_tab_ui <- function() {
  tabPanel(
    title = "Análise de dados fluviométricos",
    value = "Análise de dados fluviométricos",
    br(),
    div(
      class = "section-card",
      div(
        class = "section-header section-header-with-help",
        div(
          h3("Análise de dados fluviométricos"),
          p("Ajuda geral para obtenção, visualização, consistência, estatísticas e eventos extremos de séries de vazão.")
        ),
        actionButton(
          inputId = "open_fluviometric_help_modal",
          label = "Ajuda",
          icon = icon("circle-question"),
          class = "section-help-button"
        )
      )
    ),
    tabsetPanel(
      id = "fluviometric_tabs",
      type = "pills",
      tabPanel(
        title = "Obtenção de dados",
        value = "Obtenção de dados",
        br(),
        div(
          class = "section-card",
          div(
            class = "section-header",
            h3("Obtenção de dados fluviométricos"),
            p("Forneça dados diários para a estação atualmente selecionada. O código da estação no arquivo deve ser igual ao código selecionado no sistema.")
          ),
          
          div(
            class = "control-card",
            fluidRow(
              column(
                4,
                selectInput(
                  inputId = "fluviometric_data_source",
                  label = "Fonte dos dados",
                  choices = c(
                    "HidroWeb — arquivo ZIP completo" = "hidroweb_zip",
                    "HidroWeb — CSV de vazões" = "hidroweb_discharge_csv",
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
                  condition = "input.fluviometric_data_source == 'hidroweb_zip'",
                  fileInput(
                    inputId = "fluviometric_hidroweb_zip",
                    label = "Arquivo .zip exportado pelo HidroWeb",
                    accept = c(".zip")
                  ),
                  # helpText("O ZIP pode conter Vazoes.csv, Cotas.csv e arquivos de chuva. Apenas séries diárias serão utilizadas.")
                ),
                
                conditionalPanel(
                  condition = "input.fluviometric_data_source == 'hidroweb_discharge_csv'",
                  fileInput(
                    inputId = "fluviometric_hidroweb_discharge_csv",
                    label = "Arquivo CSV de vazões do HidroWeb",
                    accept = c(".csv")
                  ),
                  helpText("Use o arquivo *_Vazoes.csv extraído do HidroWeb.")
                ),
                
                conditionalPanel(
                  condition = "input.fluviometric_data_source == 'ana_xml'",
                  radioButtons(
                    inputId = "fluviometric_xml_mode",
                    label = "Modo de obtenção",
                    choices = c(
                      "Enviar arquivo XML" = "upload",
                      "Download automático do WebService" = "download"
                    ),
                    selected = "download",
                    inline = TRUE
                  ),
                  
                  conditionalPanel(
                    condition = "input.fluviometric_data_source == 'ana_xml' && input.fluviometric_xml_mode == 'upload'",
                    fileInput(
                      inputId = "fluviometric_xml_file",
                      label = "Arquivo XML do WebService da ANA",
                      accept = c(".xml")
                    )
                  ),
                  
                  conditionalPanel(
                    condition = "input.fluviometric_data_source == 'ana_xml' && input.fluviometric_xml_mode == 'download'",
                    # selectInput(
                    #   inputId = "fluviometric_xml_consistency_level",
                    #   label = "Nível de consistência solicitado",
                    #   choices = c(
                    #     "1 - Bruto" = "1",
                    #     "2 - Consistido" = "2"
                    #   ),
                    #   selected = "2"
                    # ),
                    # helpText("A requisição será montada automaticamente para a estação selecionada, com data inicial 01/01/1900, data final igual à data atual, tipoDados = 3 e o nível de consistência escolhido.")
                  ),
                  
                  # helpText("O WebService retorna dados diários de série histórica. Não haverá agregação de dados subdiários.")
                ),
                conditionalPanel(
                  condition = "input.fluviometric_data_source == 'ana_api_download'",
                  div(
                    class = "limitation-box",
                    strong("Download pela API ANA: "),
                    "o processo pode ser demorado para períodos longos. Cada rota é baixada ano a ano. ",
                    "Se o token expirar, o download será pausado e poderá ser retomado do ponto em que parou."
                  ),
                  fluidRow(
                    column(
                      6,
                      textInput(
                        inputId = "fluviometric_api_identificador",
                        label = "Usuário / identificador ANA",
                        value = "",
                        placeholder = "CPF ou CNPJ autorizado na API"
                      )
                    ),
                    column(
                      6,
                      passwordInput(
                        inputId = "fluviometric_api_senha",
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
                        inputId = "fluviometric_api_get_token",
                        label = "Obter token",
                        icon = icon("key"),
                        class = "btn-primary"
                      )
                    )
                  ),
                  uiOutput("fluviometric_api_token_status"),
                  radioButtons(
                    inputId = "fluviometric_api_period_mode",
                    label = "Período de download",
                    choices = c(
                      "Informar ano inicial e ano final" = "manual",
                      "Usar inventário da estação até o ano atual" = "inventory"
                    ),
                    selected = "inventory"
                  ),
                  conditionalPanel(
                    condition = "input.fluviometric_api_period_mode == 'manual'",
                    fluidRow(
                      column(
                        6,
                        numericInput(
                          inputId = "fluviometric_api_start_year",
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
                          inputId = "fluviometric_api_end_year",
                          label = "Ano final",
                          value = as.integer(format(Sys.Date(), "%Y")),
                          min = 1900,
                          max = as.integer(format(Sys.Date(), "%Y")),
                          step = 1
                        )
                      )
                    )
                  ),
                  uiOutput("fluviometric_api_download_status"),
                  # div(
                  #   class = "download-button-row",
                  #   div(
                  #     class = "download-button-item",
                  #     downloadButton(
                  #       outputId = "fluviometric_api_download_report_download",
                  #       label = "Baixar relatório de download",
                  #       class = "btn-primary"
                  #     )
                  #   )
                  # )
                ),
                conditionalPanel(
                  condition = "input.fluviometric_data_source == 'ana_json'",
                  fileInput(
                    inputId = "fluviometric_json_file",
                    label = "Arquivo JSON da API ANA",
                    accept = c(".json")
                  ),
                  # helpText("O app não solicita CPF/CNPJ, senha ou token. Faça o download do JSON fora do app e envie o arquivo aqui.")
                )
              )
            ),
            
            div(
              class = "extremes-download-row",
              div(
                class = "extremes-download-item",
                actionButton(
                  inputId = "fluviometric_process_data",
                  label = "Processar dados",
                  icon = icon("play"),
                  class = "btn-primary"
                )
              )
            ),
            
            uiOutput("fluviometric_processing_status")
          )
        ),
        
        uiOutput("fluviometric_acquisition_cards"),
        
        uiOutput("fluviometric_availability_section"),
      ),
      tabPanel(
        title = "Séries de vazões",
        value = "Séries de vazões",
        br(),
        
        div(
          class = "section-card",
          div(
            class = "section-header",
            h3("Hidrograma diário"),
            # p("Use zoom, pan, seleção de intervalo e duplo clique para explorar a série diária de vazões.")
          ),
          uiOutput("fluviometric_discharge_series_status"),
          div(
            class = "plot-card",
            plotly::plotlyOutput(
              outputId = "fluviometric_discharge_series_plotly",
              height = "430px"
            )
          )
        ),
        
        uiOutput("fluviometric_discharge_summary_cards")
      ),
      tabPanel(
        title = "Consistência fluviométrica",
        value = "Consistência fluviométrica",
        br(),
        
        div(
          class = "section-card",
          div(
            class = "section-header",
            h3("Consistência fluviométrica"),
            p("Avaliação integrada da série diária de vazões, série diária de cotas e curvas-chave disponíveis para a estação selecionada.")
          ),
          uiOutput("fluviometric_consistency_status"),
          uiOutput("fluviometric_consistency_report_controls")
        ),
        
        uiOutput("fluviometric_consistency_coverage_cards"),
        
        uiOutput("fluviometric_consistency_curve_cards"),
        
        uiOutput("fluviometric_consistency_hq_cards"),
        
        tags$details(
          class = "details-card details-card-main",
          open = NA,
          tags$summary("Resumo por curva-chave"),
          div(
            class = "section-header",
            # p("Resumo dos dias avaliados, geração de vazão pela curva-chave e diferenças entre vazão gerada e vazão disponibilizada.")
          ),
          div(
            class = "table-card",
            uiOutput("fluviometric_consistency_curve_summary_status"),
            DT::DTOutput("fluviometric_consistency_curve_summary_table")
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
            h3("Estatísticas mensais e anuais"),
            # p("Estatísticas, permanência e regularização calculadas a partir da série diária de vazões carregada na sessão.")
          ),
          uiOutput("fluviometric_stats_status"),
          uiOutput("fluviometric_stats_download_controls")
        ),
        
        uiOutput("fluviometric_stats_summary_cards"),
        
        tags$details(
          class = "details-card details-card-main",
          open = NA,
          tags$summary("Vazões médias mensais e anuais"),
          div(
            class = "section-header",
            # p("Resumo gráfico das vazões médias anuais e do regime mensal médio.")
          ),
          fluidRow(
            column(
              6,
              div(
                class = "plot-card",
                plotOutput("fluviometric_stats_annual_mean_plot", height = "320px")
              )
            ),
            column(
              6,
              div(
                class = "plot-card",
                plotOutput("fluviometric_stats_monthly_regime_plot", height = "320px")
              )
            )
          )
        ),
        
        tags$details(
          class = "details-card details-card-main",
          open = NA,
          tags$summary("Curvas de permanência mensal e anual"),
          div(
            class = "section-header",
            # p("Curvas de permanência obtidas pela ordenação decrescente das vazões e interpolação para probabilidades de permanência entre 1% e 99,9%.")
          ),
          radioButtons(
            inputId = "fluviometric_stats_fdc_y_scale",
            label = "Escala do eixo de vazões",
            choices = c("Linear" = "linear", "Logarítmica" = "log"),
            selected = "linear",
            inline = TRUE
          ),
          fluidRow(
            column(
              6,
              div(
                class = "plot-card",
                plotOutput("fluviometric_stats_fdc_annual_plot", height = "340px")
              )
            ),
            column(
              6,
              div(
                class = "plot-card",
                plotOutput("fluviometric_stats_fdc_monthly_plot", height = "340px")
              )
            )
          )
        ),
        
        tags$details(
          class = "details-card details-card-main",
          open = NA,
          tags$summary("Curva de regularização e diagrama de Rippl"),
          div(
            class = "section-header",
            # p("Regularização calculada a partir da série de volumes médios mensais, com demandas entre 10% e 100% da QMLT.")
          ),
          fluidRow(
            column(
              6,
              div(
                class = "plot-card",
                plotOutput("fluviometric_stats_regularization_plot", height = "340px")
              )
            ),
            column(
              6,
              div(
                class = "plot-card",
                plotOutput("fluviometric_stats_rippl_plot", height = "340px")
              )
            )
          )
        ),
        
        tags$details(
          class = "details-card details-card-main",
          open = NA,
          tags$summary("Tabela de vazões médias mensais"),
          div(
            class = "section-header",
            # p("Tabela em formato ano × mês, com média anual por ano e linha final com o regime médio mensal e a QMLT.")
          ),
          div(
            class = "table-card",
            DT::DTOutput("fluviometric_stats_monthly_wide_table")
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
            h3("Eventos extremos"),
            p("Extração e triagem de séries de extremos observados a partir da série diária de vazões carregada na sessão.")
          ),
          uiOutput("fluviometric_extremes_status")
        ),
        
        tabsetPanel(
          id = "fluviometric_extremes_subtab",
          type = "tabs",
          
          tabPanel(
            title = "Máximos anuais",
            value = "maximos_anuais",
            br(),
            
            div(
              class = "section-card",
              div(
                class = "section-header",
                h3("Máximos anuais — ano hidrológico outubro/setembro"),
                p("Máximo diário observado em cada ano hidrológico. Os pontos são coloridos pelo número de flags de triagem.")
              ),
              div(
                class = "extremes-download-row",
                tags$div(
                  class = "extremes-download-item",
                  downloadButton(
                    outputId = "fluviometric_extremes_annual_max_simple_download",
                    label = "Máximos anuais",
                    class = "btn-primary"
                  )
                ),
                tags$div(
                  class = "extremes-download-item",
                  downloadButton(
                    outputId = "fluviometric_extremes_annual_max_download",
                    label = "Máximos anuais detalhados",
                    class = "btn-primary"
                  )
                )
              )
            ),
            
            uiOutput("fluviometric_extremes_summary_cards"),
            
            div(
              class = "plot-card",
              plotOutput("fluviometric_extremes_annual_max_plot", height = "360px")
            ),
            
            tags$details(
              class = "details-card details-card-main",
              open = NA,
              tags$summary("Tabela de máximos anuais e flags"),
              div(
                class = "section-header",
                p("Nenhum valor é excluído automaticamente. Os flags indicam pontos que merecem revisão visual ou documental.")
              ),
              div(
                class = "table-card",
                DT::DTOutput("fluviometric_extremes_annual_max_table")
              )
            )
          ),
          
          tabPanel(
            title = "Mínimas anuais",
            value = "minimas_anuais",
            br(),
            
            div(
              class = "section-card",
              div(
                class = "section-header",
                h3("Mínimas anuais"),
                p("Extração de mínimas anuais por duração, calculadas como médias móveis completas dentro do ano civil.")
              ),
              
              fluidRow(
                column(
                  4,
                  div(
                    class = "control-card",
                    h4("Configuração"),
                    selectInput(
                      inputId = "fluviometric_extremes_low_flow_duration",
                      label = "Duração da mínima média móvel",
                      choices = c(
                        "1 dia" = 1,
                        "3 dias" = 3,
                        "7 dias" = 7,
                        "15 dias" = 15,
                        "30 dias" = 30
                      ),
                      selected = 7
                    )
                  )
                ),
                column(
                  4,
                  div(
                    class = "control-card",
                    h4("Exportação"),
                    div(
                      class = "extremes-download-row",
                      tags$div(
                        class = "extremes-download-item",
                        downloadButton(
                          outputId = "fluviometric_extremes_low_flow_download",
                          label = "Vazões mínimas",
                          class = "btn-primary"
                        )
                      )
                    )
                  )
                )
              )
            ),
            
            uiOutput("fluviometric_extremes_low_flow_summary_cards"),
            
            div(
              class = "plot-card",
              plotOutput("fluviometric_extremes_low_flow_plot", height = "360px")
            ),
            
            tags$details(
              class = "details-card details-card-main",
              open = NA,
              tags$summary("Tabela de mínimas anuais e flags"),
              div(
                class = "section-header",
                p("Nenhum valor é excluído automaticamente. Os flags indicam anos que merecem revisão visual ou documental.")
              ),
              div(
                class = "table-card",
                DT::DTOutput("fluviometric_extremes_low_flow_table")
              )
            )
          ),
          
          tabPanel(
            title = "POT",
            value = "pot",
            br(),
            
            div(
              class = "section-card",
              div(
                class = "section-header",
                h3("Eventos acima de limiar — POT descritivo"),
                p("Extração de picos independentes acima de um limiar. O resultado é descritivo e não estima tempo de retorno.")
              ),
              div(
                class = "extremes-download-row",
                tags$div(
                  class = "extremes-download-item",
                  downloadButton(
                    outputId = "fluviometric_extremes_pot_download",
                    label = "Série POT",
                    class = "btn-primary"
                  )
                )
              )
            ),
            
            fluidRow(
              class = "extremes-pot-control-row",
              
              column(
                4,
                div(
                  class = "control-card extremes-pot-card",
                  h4("Limiar automático"),
                  p("Busca iterativa do maior λ que mantém critérios mínimos de seleção dos eventos."),
                  actionButton(
                    inputId = "fluviometric_extremes_pot_auto",
                    label = "Calcular limiar automático",
                    icon = icon("wand-magic-sparkles"),
                    class = "btn-primary btn-block"
                  )
                )
              ),
              
              column(
                4,
                div(
                  class = "control-card extremes-pot-card",
                  h4("Limiar manual"),
                  textInput(
                    inputId = "fluviometric_extremes_pot_threshold_manual",
                    label = "Limiar de vazão (m³/s)",
                    value = "",
                    placeholder = "Ex.: 250,5"
                  ),
                  actionButton(
                    inputId = "fluviometric_extremes_pot_manual",
                    label = "Calcular POT",
                    icon = icon("calculator"),
                    class = "btn-primary btn-block"
                  )
                )
              ),
              
              column(
                4,
                uiOutput(
                  outputId = "fluviometric_extremes_pot_status",
                  class = "extremes-pot-status-output"
                )
              )
            ),
            
            div(
              class = "plot-card",
              plotOutput("fluviometric_extremes_pot_plot", height = "360px")
            )
          )
        )
      )
    )
  )
}

