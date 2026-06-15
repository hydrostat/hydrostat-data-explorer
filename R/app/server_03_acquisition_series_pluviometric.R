# ============================================================
# server_03_acquisition_series_pluviometric.R
# Purpose: Fluviometric acquisition, pluviometric module, and interactive daily discharge series.
# ============================================================
# BEGIN ORIGINAL BODY
  fluviometric_acquisition_result <- reactiveVal(NULL)
  fluviometric_processing_status <- reactiveVal(list(
    type = "empty",
    message = "Aguardando o envio ou a requisição de dados."
  ))
  
  observeEvent(selected_code(), {
    fluviometric_acquisition_result(NULL)
    fluviometric_processing_status(list(
      type = "empty",
      message = "A estação selecionada mudou. Processe novamente os dados da nova estação."
    ))
  }, ignoreInit = TRUE)
  
  observeEvent(input$fluviometric_process_data, {
    if (identical(input$fluviometric_data_source, "ana_api_download")) {
      fluviometric_processing_status(list(
        type = "empty",
        message = "Baixando dados pela API ANA..."
      ))
      
      withProgress(message = "Download pela API ANA — dados fluviométricos", value = 0, {
        tryCatch({
          api_download <- ana_run_download_loop("flu")
          
          if (identical(api_download$status, "paused_auth")) {
            fluviometric_processing_status(list(
              type = "warning",
              message = "Download pausado por problema de autorização/token. Obtenha novo token e clique em Processar dados para retomar."
            ))
            return(invisible(NULL))
          }
          
          result <- api_download$result
          
          incProgress(0.05, detail = "Validando código da estação")
          validate_fluviometric_station_code(result, selected_code())
          
          result$data$station_code <- selected_code()
          result$discharge$station_code <- selected_code()
          result$stage$station_code <- selected_code()
          result$rainfall$station_code <- selected_code()
          result$station_codes <- selected_code()
          
          fluviometric_acquisition_result(result)
          loaded_session_station_code(selected_code())
          loaded_session_data_type("flu")
          
          fluviometric_processing_status(list(
            type = "available",
            message = paste0(
              "Dados baixados pela API ANA e processados com sucesso para a estação ",
              selected_code(),
              ". Consulte o relatório de download para anos sem dados ou falhas após 3 tentativas."
            )
          ))
        }, error = function(e) {
          fluviometric_processing_status(list(
            type = "warning",
            message = clean_error_message(conditionMessage(e))
          ))
        })
      })
      
      return(invisible(NULL))
    }
    fluviometric_acquisition_result(NULL)
    fluviometric_processing_status(list(
      type = "empty",
      message = "Processando os dados informados..."
    ))
    
    withProgress(message = "Processando dados fluviométricos", value = 0.2, {
      tryCatch({
        source_type <- input$fluviometric_data_source
        result <- NULL
        
        if (identical(source_type, "hidroweb_zip")) {
          req(input$fluviometric_hidroweb_zip)
          incProgress(0.2, detail = "Lendo ZIP do HidroWeb")
          result <- read_fluviometric_from_hidroweb_zip(
            input$fluviometric_hidroweb_zip$datapath
          )
        }
        
        if (identical(source_type, "hidroweb_discharge_csv")) {
          req(input$fluviometric_hidroweb_discharge_csv)
          incProgress(0.2, detail = "Lendo CSV de vazões")
          result <- read_fluviometric_from_hidroweb_discharge_csv(
            input$fluviometric_hidroweb_discharge_csv$datapath
          )
        }
        
        if (identical(source_type, "ana_xml")) {
          xml_mode <- input$fluviometric_xml_mode
          
          if (is.null(xml_mode) || identical(xml_mode, "upload")) {
            req(input$fluviometric_xml_file)
            xml_source <- input$fluviometric_xml_file$datapath
            
            incProgress(0.2, detail = "Lendo XML enviado pelo usuário")
            result <- read_fluviometric_from_ana_xml(xml_source)
          }
          
          if (identical(xml_mode, "download")) {
            discharge_xml_source <- build_ana_historical_xml_url(
              station_code = selected_code(),
              data_type = "3",
              consistency_level = "2"
            )
            
            stage_xml_source <- build_ana_historical_xml_url(
              station_code = selected_code(),
              data_type = "1",
              consistency_level = "2"
            )
            
            incProgress(0.15, detail = "Baixando vazões do WebService ANA")
            
            discharge_result <- read_fluviometric_from_ana_xml(discharge_xml_source)
            discharge_result$source_label <- "ANA WebService XML — vazões"
            
            incProgress(0.15, detail = "Baixando cotas do WebService ANA")
            
            stage_note <- character()
            
            stage_result <- tryCatch(
              {
                tmp <- read_fluviometric_from_ana_xml(stage_xml_source)
                tmp$source_label <- "ANA WebService XML — cotas"
                tmp
              },
              error = function(e) {
                stage_note <<- paste0(
                  "Cotas não foram carregadas pelo WebService ANA: ",
                  clean_error_message(conditionMessage(e))
                )
                
                NULL
              }
            )
            
            if (!is.null(stage_result) && nrow(stage_result$stage) == 0) {
              stage_note <- c(
                stage_note,
                "O download complementar de cotas foi realizado, mas nenhuma série diária de cotas foi identificada."
              )
            }
            
            result <- combine_fluviometric_results(
              discharge_result,
              stage_result,
              source_type = "ana_xml",
              source_label = "ANA WebService XML — download automático",
              acquisition_notes = stage_note
            )
          }
        }
        
        if (identical(source_type, "ana_json")) {
          req(input$fluviometric_json_file)
          incProgress(0.2, detail = "Lendo JSON da API ANA")
          result <- read_fluviometric_from_ana_json(
            input$fluviometric_json_file$datapath
          )
        }
        
        incProgress(0.3, detail = "Validando código da estação")
        validate_fluviometric_station_code(result, selected_code())
        
        result$data$station_code <- selected_code()
        result$discharge$station_code <- selected_code()
        result$stage$station_code <- selected_code()
        result$rainfall$station_code <- selected_code()
        result$station_codes <- selected_code()
        
        fluviometric_acquisition_result(result)
        loaded_session_station_code(selected_code())
        loaded_session_data_type("flu")
        
        fluviometric_processing_status(list(
          type = "available",
          message = paste0(
            "Dados processados com sucesso para a estação ",
            selected_code(),
            "."
          )
        ))
        
        incProgress(0.3, detail = "Concluído")
      }, error = function(e) {
        fluviometric_acquisition_result(NULL)
        loaded_session_station_code(NULL)
        loaded_session_data_type(NULL)
        fluviometric_processing_status(list(
          type = "warning",
          message = paste0(
            "Não foi possível processar os dados: ",
            clean_error_message(conditionMessage(e))
          )
        ))
      })
    })
  })
  
  output$fluviometric_processing_status <- renderUI({
    status <- fluviometric_processing_status()
    
    css_class <- switch(
      status$type,
      available = "table-status available",
      warning = "table-status warning",
      empty = "table-status empty",
      "table-status empty"
    )
    
    tags$div(class = css_class, status$message)
  })
  
  output$fluviometric_acquisition_cards <- renderUI({
    result <- fluviometric_acquisition_result()
    
    if (is.null(result)) {
      return(NULL)
    }
    
    data <- result$data
    
    n_discharge <- sum(data$variable == "discharge" & !is.na(data$value), na.rm = TRUE)
    n_stage <- sum(data$variable == "stage" & !is.na(data$value), na.rm = TRUE)
    n_rainfall <- sum(data$variable == "rainfall" & !is.na(data$value), na.rm = TRUE)
    
    has_discharge <- n_discharge > 0
    has_stage <- n_stage > 0
    has_rainfall <- n_rainfall > 0
    
    rating_curves <- tryCatch(
      selected_rating_curves(),
      error = function(e) NULL
    )
    
    rating_curve_count <- if (
      is.null(rating_curves) ||
      nrow(rating_curves) == 0
    ) {
      0L
    } else if ("rating_curve_id" %in% names(rating_curves)) {
      dplyr::n_distinct(rating_curves$rating_curve_id)
    } else {
      nrow(rating_curves)
    }
    
    has_rating_curve <- rating_curve_count > 0
    
    rating_curve_text <- if (has_rating_curve) {
      paste0(
        fluviometric_format_count(rating_curve_count),
        ifelse(rating_curve_count == 1, " curva", " curvas")
      )
    } else {
      "não disponíveis"
    }
    
    data_scope <- dplyr::case_when(
      has_discharge && has_stage && has_rating_curve ~ "Vazão + cotas + curvas-chave",
      has_discharge && has_stage && !has_rating_curve ~ "Vazão + cotas",
      has_discharge && !has_stage && has_rating_curve ~ "Parcial — sem cotas",
      has_discharge && !has_stage && !has_rating_curve ~ "Parcial — sem cotas e curvas-chave",
      !has_discharge && has_stage && has_rating_curve ~ "Parcial — sem vazões",
      !has_discharge && has_stage && !has_rating_curve ~ "Parcial — sem vazões e curvas-chave",
      TRUE ~ "Parcial"
    )
    
    consistency_scope <- dplyr::case_when(
      has_discharge && has_stage && has_rating_curve ~ "Apta para análise H-Q com curvas-chave",
      has_discharge && has_stage && !has_rating_curve ~ "Apta para verificações H-Q básicas",
      has_discharge && !has_stage && has_rating_curve ~ "Parcial — sem cotas",
      has_discharge && !has_stage && !has_rating_curve ~ "Parcial — sem cotas e curvas-chave",
      !has_discharge && has_stage ~ "Parcial — sem vazões",
      TRUE ~ "Dados incompletos"
    )
    
    stage_text <- if (has_stage) {
      paste0(fluviometric_format_count(n_stage), " valores")
    } else {
      "não disponíveis"
    }
    
    rainfall_text <- if (has_rainfall) {
      paste0(fluviometric_format_count(n_rainfall), " valores")
    } else {
      "não disponíveis"
    }
    
    acquisition_notes <- character()
    
    if (!has_stage) {
      acquisition_notes <- c(
        acquisition_notes,
        "A série diária de cotas não está disponível nesta sessão. As análises de consistência fluviométrica serão parciais."
      )
    }
    
    if (!has_rating_curve) {
      acquisition_notes <- c(
        acquisition_notes,
        "Não há curvas-chave disponíveis no banco interno para a estação selecionada. As verificações com curva-chave não serão realizadas."
      )
    }
    
    if (!is.null(result$acquisition_notes)) {
      acquisition_notes <- c(acquisition_notes, as.character(result$acquisition_notes))
    }
    
    acquisition_notes <- unique(acquisition_notes)
    acquisition_notes <- acquisition_notes[!is.na(acquisition_notes) & acquisition_notes != ""]
    
    date_min <- suppressWarnings(min(data$date, na.rm = TRUE))
    date_max <- suppressWarnings(max(data$date, na.rm = TRUE))
    
    period_text <- if (is.finite(as.numeric(date_min)) && is.finite(as.numeric(date_max))) {
      paste0(format(date_min, "%d/%m/%Y"), " a ", format(date_max, "%d/%m/%Y"))
    } else {
      "não informado"
    }
    
    tags$div(
      class = "section-card",
      tags$div(
        class = "section-header",
        tags$h3("Resumo dos dados carregados"),
        tags$p("Resumo dos dados diários disponíveis na sessão atual.")
      ),
      tags$div(
        class = "overview-metric-grid fluviometric-metric-grid",
        overview_metric("Fonte", result$source_label),
        overview_metric("Estação", paste(result$station_codes, collapse = ", ")),
        overview_metric("Período", period_text),
        overview_metric("Escopo dos dados", data_scope),
        overview_metric("Vazões diárias", paste0(fluviometric_format_count(n_discharge), " valores")),
        overview_metric("Cotas diárias", stage_text),
        overview_metric("Curvas-chave", rating_curve_text),
        overview_metric("Chuvas diárias", rainfall_text),
        overview_metric("Status da consistência", consistency_scope)
      ),
      if (length(acquisition_notes) > 0) {
        tags$div(
          class = "table-status warning",
          paste(acquisition_notes, collapse = " ")
        )
      }
    )
  })
  
  output$fluviometric_availability_section <- renderUI({
    result <- fluviometric_acquisition_result()
    
    if (is.null(result)) {
      return(NULL)
    }
    
    if (nrow(result$discharge) == 0) {
      return(NULL)
    }
    
    tags$div(
      class = "section-card",
      tags$div(
        class = "section-header",
        tags$h3("Disponibilidade mensal de vazões diárias"),
        tags$p("Cada célula representa um mês. A cor indica o percentual de falhas em relação ao número esperado de dias no mês.")
      ),
      tags$div(
        class = "plot-card",
        plotOutput("fluviometric_availability_plot", height = "330px")
      )
    )
  })
  
  
  output$fluviometric_availability_plot <- renderPlot({
    result <- fluviometric_acquisition_result()
    
    validate(
      need(!is.null(result), "Nenhum dado processado nesta sessão."),
      need(nrow(result$discharge) > 0, "Não há vazões diárias disponíveis para o gráfico de disponibilidade.")
    )
    
    availability <- build_fluviometric_monthly_availability(
      data = result$data,
      variable_name = "discharge"
    )
    
    validate(
      need(nrow(availability) > 0, "Não foi possível calcular a disponibilidade mensal.")
    )
    
    failure_colors <- c(
      "100%" = "#d53e4f",
      "75–<100%" = "#fc8d59",
      "50–<75%" = "#fee08b",
      "25–<50%" = "#e6f598",
      "0–<25%" = "#99d594",
      "0%" = "#3288bd"
    )
    failure_labels <- c(
      "100%" = "100%",
      "75–<100%" = "75% a <100%",
      "50–<75%" = "50% a <75%",
      "25–<50%" = "25% a <50%",
      "0–<25%" = ">0% a <25%",
      "0%" = "0%"
    )
    
    legend_levels <- names(failure_colors)
    
    availability <- availability |>
      dplyr::mutate(
        failure_class = factor(
          as.character(failure_class),
          levels = legend_levels
        )
      )
    
    legend_dummy <- tibble::tibble(
      year = min(availability$year, na.rm = TRUE),
      month = 1,
      failure_class = factor(legend_levels, levels = legend_levels)
    )
    
    ggplot2::ggplot(
      availability,
      ggplot2::aes(
        x = year,
        y = month,
        fill = failure_class
      )
    ) +
      ggplot2::geom_tile(
        data = legend_dummy,
        ggplot2::aes(
          x = year,
          y = month,
          fill = failure_class
        ),
        alpha = 0,
        show.legend = TRUE,
        inherit.aes = FALSE
      ) +
      ggplot2::geom_tile(
        width = 1,
        height = 1,
        color = "white",
        linewidth = 0.08
      ) +
      ggplot2::scale_fill_manual(
        name = "Falhas",
        values = failure_colors,
        limits = legend_levels,
        breaks = legend_levels,
        labels = failure_labels,
        drop = FALSE,
        na.translate = FALSE,
        guide = ggplot2::guide_legend(
          override.aes = list(
            alpha = 1,
            color = NA
          )
        )
      ) +
      ggplot2::scale_x_continuous(
        name = "Ano",
        breaks = scales::pretty_breaks(n = 10),
        expand = c(0, 0)
      ) +
      ggplot2::scale_y_continuous(
        name = "Mês",
        breaks = 1:12,
        labels = 1:12,
        expand = c(0, 0)
      ) +
      ggplot2::coord_fixed(ratio = 1.2) +
      preview_plot_theme(base_size = 6) +
      ggplot2::theme(
        panel.grid = ggplot2::element_blank(),
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        panel.grid.major.x = ggplot2::element_blank(),
        panel.grid.major.y = ggplot2::element_blank(),
        panel.grid.minor.x = ggplot2::element_blank(),
        panel.grid.minor.y = ggplot2::element_blank(),
        panel.border = ggplot2::element_blank(),
        legend.position = "right",
        plot.margin = ggplot2::margin(6, 8, 6, 8)
      )
  }, res = 120)
  
  # ------------------------------------------------------------
  # Fluviometric tab: interactive daily discharge series
  # ------------------------------------------------------------
  
  # ------------------------------------------------------------
  # Session-only pluviometric data module
  # ------------------------------------------------------------
  
  pluviometric_acquisition_result <- reactiveVal(NULL)
  pluviometric_processing_status <- reactiveVal(list(
    type = "empty",
    message = "Aguardando o envio ou a requisição de dados."
  ))
  
  observeEvent(selected_code(), {
    pluviometric_acquisition_result(NULL)
    pluviometric_processing_status(list(
      type = "empty",
      message = "A estação selecionada mudou. Processe novamente os dados da nova estação."
    ))
  }, ignoreInit = TRUE)
  
  observeEvent(input$pluviometric_process_data, {
    if (identical(input$pluviometric_data_source, "ana_api_download")) {
      pluviometric_processing_status(list(
        type = "empty",
        message = "Baixando dados pela API ANA..."
      ))
      
      withProgress(message = "Download pela API ANA — dados pluviométricos", value = 0, {
        tryCatch({
          api_download <- ana_run_download_loop("plu")
          
          if (identical(api_download$status, "paused_auth")) {
            pluviometric_processing_status(list(
              type = "warning",
              message = "Download pausado por problema de autorização/token. Obtenha novo token e clique em Processar dados para retomar."
            ))
            return(invisible(NULL))
          }
          
          result <- api_download$result
          
          incProgress(0.05, detail = "Validando código da estação")
          validate_pluviometric_station_code(result, selected_code())
          
          result$data$station_code <- selected_code()
          result$rainfall$station_code <- selected_code()
          result$station_codes <- selected_code()
          
          pluviometric_acquisition_result(result)
          loaded_session_station_code(selected_code())
          loaded_session_data_type("plu")
          
          pluviometric_processing_status(list(
            type = "available",
            message = paste0(
              "Dados baixados pela API ANA e processados com sucesso para a estação ",
              selected_code(),
              ". Consulte o relatório de download para anos sem dados ou falhas após 3 tentativas."
            )
          ))
        }, error = function(e) {
          pluviometric_processing_status(list(
            type = "warning",
            message = clean_error_message(conditionMessage(e))
          ))
        })
      })
      
      return(invisible(NULL))
    }
    pluviometric_acquisition_result(NULL)
    pluviometric_processing_status(list(
      type = "empty",
      message = "Processando os dados informados..."
    ))
    
    withProgress(message = "Processando dados pluviométricos", value = 0.2, {
      tryCatch({
        source_type <- input$pluviometric_data_source
        result <- NULL
        
        if (identical(source_type, "hidroweb_zip")) {
          req(input$pluviometric_hidroweb_zip)
          incProgress(0.2, detail = "Lendo ZIP do HidroWeb")
          result <- read_pluviometric_from_hidroweb_zip(
            input$pluviometric_hidroweb_zip$datapath
          )
        }
        
        if (identical(source_type, "hidroweb_rainfall_csv")) {
          req(input$pluviometric_hidroweb_rainfall_csv)
          incProgress(0.2, detail = "Lendo CSV de chuvas")
          result <- read_pluviometric_from_hidroweb_rainfall_csv(
            input$pluviometric_hidroweb_rainfall_csv$datapath
          )
        }
        
        if (identical(source_type, "ana_xml")) {
          xml_mode <- input$pluviometric_xml_mode
          
          if (is.null(xml_mode) || identical(xml_mode, "upload")) {
            req(input$pluviometric_xml_file)
            incProgress(0.2, detail = "Lendo XML enviado pelo usuário")
            result <- read_pluviometric_from_ana_xml(
              input$pluviometric_xml_file$datapath
            )
          }
          
          if (identical(xml_mode, "download")) {
            rainfall_xml_source <- build_ana_historical_xml_url(
              station_code = selected_code(),
              data_type = "2",
              consistency_level = "2"
            )
            
            incProgress(0.2, detail = "Baixando chuvas do WebService ANA")
            result <- read_pluviometric_from_ana_xml(rainfall_xml_source)
            result$source_label <- "ANA WebService XML — download automático de chuvas"
          }
        }
        
        if (identical(source_type, "ana_json")) {
          req(input$pluviometric_json_file)
          incProgress(0.2, detail = "Lendo JSON da API ANA")
          result <- read_pluviometric_from_ana_json(
            input$pluviometric_json_file$datapath
          )
        }
        
        incProgress(0.3, detail = "Validando código da estação")
        validate_pluviometric_station_code(result, selected_code())
        
        result$data$station_code <- selected_code()
        result$rainfall$station_code <- selected_code()
        result$station_codes <- selected_code()
        
        pluviometric_acquisition_result(result)
        loaded_session_station_code(selected_code())
        loaded_session_data_type("plu")
        
        pluviometric_processing_status(list(
          type = "available",
          message = paste0(
            "Dados pluviométricos processados com sucesso para a estação ",
            selected_code(),
            "."
          )
        ))
        
        incProgress(0.3, detail = "Concluído")
      }, error = function(e) {
        pluviometric_acquisition_result(NULL)
        loaded_session_station_code(NULL)
        loaded_session_data_type(NULL)
        pluviometric_processing_status(list(
          type = "warning",
          message = paste0(
            "Não foi possível processar os dados pluviométricos: ",
            clean_error_message(conditionMessage(e))
          )
        ))
      })
    })
  })
  
  output$pluviometric_processing_status <- renderUI({
    status <- pluviometric_processing_status()
    
    css_class <- switch(
      status$type,
      available = "table-status available",
      warning = "table-status warning",
      empty = "table-status empty",
      "table-status empty"
    )
    
    tags$div(class = css_class, status$message)
  })
  
  pluviometric_status_attention <- function(x) {
    x_chr <- trimws(as.character(x))
    x_chr <- x_chr[!is.na(x_chr) & x_chr != ""]
    
    if (length(x_chr) == 0) {
      return(FALSE)
    }
    
    x_low <- tolower(x_chr)
    
    any(
      x_chr %in% c("2", "3", "4") |
        grepl("estimado|duvidoso|acumulado|suspeito|ruim", x_low)
    )
  }
  
  pluviometric_daily_series <- reactive({
    result <- pluviometric_acquisition_result()
    
    if (is.null(result) || nrow(result$rainfall) == 0) {
      return(tibble::tibble())
    }
    
    rainfall <- merge_ana_daily_variable(
      data = result$rainfall,
      variable_name = "rainfall"
    ) |>
      dplyr::rename(rainfall_mm = value) |>
      dplyr::mutate(
        source_status = as.character(source_status),
        consistency_level = as.character(consistency_level),
        daily_flag = as.character(daily_flag),
        has_source_status_attention = purrr::map_lgl(
          source_status,
          pluviometric_status_attention
        )
      )
    
    if (nrow(rainfall) == 0) {
      return(tibble::tibble())
    }
    
    first_date <- min(rainfall$date, na.rm = TRUE)
    last_date <- max(rainfall$date, na.rm = TRUE)
    all_dates <- tibble::tibble(date = seq.Date(first_date, last_date, by = "day"))
    
    daily <- rainfall |>
      dplyr::group_by(date) |>
      dplyr::summarise(
        station_code = dplyr::first(stats::na.omit(as.character(station_code))),
        rainfall_mm = if (all(is.na(rainfall_mm))) NA_real_ else dplyr::first(rainfall_mm[!is.na(rainfall_mm)]),
        n_source_records = dplyr::n(),
        n_non_missing_records = sum(!is.na(rainfall_mm)),
        source_status = paste(unique(stats::na.omit(source_status)), collapse = ", "),
        consistency_level = paste(unique(stats::na.omit(consistency_level)), collapse = ", "),
        daily_flag = paste(unique(stats::na.omit(daily_flag)), collapse = ", "),
        source = paste(unique(stats::na.omit(source)), collapse = ", "),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        source_status = dplyr::na_if(source_status, ""),
        consistency_level = dplyr::na_if(consistency_level, ""),
        daily_flag = dplyr::na_if(daily_flag, ""),
        source = dplyr::na_if(source, ""),
        has_source_status_attention = purrr::map_lgl(source_status, pluviometric_status_attention)
      )
    
    all_dates |>
      dplyr::left_join(rainfall, by = "date") |>
      dplyr::mutate(
        station_code = dplyr::coalesce(station_code, selected_code()),
        year = as.integer(format(date, "%Y")),
        month = as.integer(format(date, "%m")),
        month_label = factor(
          fluviometric_stats_month_labels[month],
          levels = fluviometric_stats_month_labels
        ),
        is_observed = !is.na(rainfall_mm),
        is_valid_nonnegative = !is.na(rainfall_mm) & rainfall_mm >= 0,
        is_wet_day = is_valid_nonnegative & rainfall_mm >= 1,
        is_dry_day = is_valid_nonnegative & rainfall_mm < 1,
        has_source_status_attention = dplyr::coalesce(has_source_status_attention, FALSE),
        n_source_records = dplyr::coalesce(n_source_records, 0L),
        max_same_consistency_records = dplyr::coalesce(max_same_consistency_records, 0L),
        has_duplicate_same_consistency = dplyr::coalesce(has_duplicate_same_consistency, FALSE)
      )
  })
  
  pluviometric_count_pct <- function(n, denominator, digits = 1) {
    if (is.na(denominator) || denominator <= 0) {
      return(paste0(fluviometric_format_count(n), " (n/a)"))
    }
    pct <- 100 * n / denominator
    paste0(
      fluviometric_format_count(n),
      " (",
      fluviometric_consistency_format_percent(pct, digits),
      "%)"
    )
  }
  
  pluviometric_longest_run <- function(flag) {
    flag <- dplyr::coalesce(as.logical(flag), FALSE)
    if (length(flag) == 0 || !any(flag)) {
      return(0L)
    }
    max(rle(flag)$lengths[rle(flag)$values], na.rm = TRUE)
  }
  
  pluviometric_rolling_sum_complete <- function(x, duration) {
    x <- as.numeric(x)
    duration <- as.integer(duration)
    out <- rep(NA_real_, length(x))
    
    if (length(x) == 0 || duration <= 0 || length(x) < duration) {
      return(out)
    }
    
    for (i in seq_len(length(x) - duration + 1L)) {
      window <- x[i:(i + duration - 1L)]
      if (!any(is.na(window))) {
        out[i + duration - 1L] <- sum(window)
      }
    }
    
    out
  }
  
  output$pluviometric_acquisition_cards <- renderUI({
    result <- pluviometric_acquisition_result()
    if (is.null(result)) {
      return(NULL)
    }
    
    daily <- pluviometric_daily_series()
    n_rainfall <- sum(daily$is_observed, na.rm = TRUE)
    n_valid <- sum(daily$is_valid_nonnegative, na.rm = TRUE)
    n_negative <- sum(!is.na(daily$rainfall_mm) & daily$rainfall_mm < 0, na.rm = TRUE)
    n_status_attention <- sum(daily$has_source_status_attention, na.rm = TRUE)
    
    date_min <- suppressWarnings(min(daily$date, na.rm = TRUE))
    date_max <- suppressWarnings(max(daily$date, na.rm = TRUE))
    period_text <- if (is.finite(as.numeric(date_min)) && is.finite(as.numeric(date_max))) {
      paste0(format(date_min, "%d/%m/%Y"), " a ", format(date_max, "%d/%m/%Y"))
    } else {
      "não informado"
    }
    
    tags$div(
      class = "section-card",
      tags$div(
        class = "section-header",
        tags$h3("Resumo dos dados pluviométricos carregados"),
        tags$p("Resumo dos dados diários disponíveis na sessão atual.")
      ),
      tags$div(
        class = "overview-metric-grid fluviometric-metric-grid",
        overview_metric("Fonte", result$source_label),
        overview_metric("Estação", paste(result$station_codes, collapse = ", ")),
        overview_metric("Período", period_text),
        overview_metric("Chuvas diárias", paste0(fluviometric_format_count(n_rainfall), " valores")),
        overview_metric("Valores válidos", paste0(fluviometric_format_count(n_valid), " valores")),
        overview_metric("Valores negativos", paste0(fluviometric_format_count(n_negative), " valores")),
        overview_metric("Status de atenção", paste0(fluviometric_format_count(n_status_attention), " dias")),
        overview_metric("Escopo", "Precipitação diária em sessão")
      ),
      tags$div(
        class = "table-status empty",
        "Os dados pluviométricos carregados ou baixados são usados apenas nesta sessão e não são gravados no banco DuckDB, em logs, cache ou dados empacotados do app."
      )
    )
  })
  
  output$pluviometric_availability_section <- renderUI({
    result <- pluviometric_acquisition_result()
    if (is.null(result) || nrow(result$rainfall) == 0) {
      return(NULL)
    }
    
    tags$div(
      class = "section-card",
      tags$div(
        class = "section-header",
        tags$h3("Disponibilidade mensal de precipitação diária"),
        tags$p("Cada célula representa um mês. A cor indica o percentual de falhas em relação ao número esperado de dias no mês.")
      ),
      tags$div(
        class = "plot-card",
        plotOutput("pluviometric_availability_plot", height = "330px")
      )
    )
  })
  
  output$pluviometric_availability_plot <- renderPlot({
    result <- pluviometric_acquisition_result()
    validate(
      need(!is.null(result), "Nenhum dado processado nesta sessão."),
      need(nrow(result$rainfall) > 0, "Não há dados diários de precipitação disponíveis.")
    )
    
    availability <- build_fluviometric_monthly_availability(
      data = result$data,
      variable_name = "rainfall"
    )
    
    validate(need(nrow(availability) > 0, "Não foi possível calcular a disponibilidade mensal."))
    
    failure_colors <- c(
      "100%" = "#d53e4f",
      "75–<100%" = "#fc8d59",
      "50–<75%" = "#fee08b",
      "25–<50%" = "#e6f598",
      "0–<25%" = "#99d594",
      "0%" = "#3288bd"
    )
    failure_labels <- c(
      "100%" = "100%",
      "75–<100%" = "75% a <100%",
      "50–<75%" = "50% a <75%",
      "25–<50%" = "25% a <50%",
      "0–<25%" = ">0% a <25%",
      "0%" = "0%"
    )
    legend_levels <- names(failure_colors)
    
    availability <- availability |>
      dplyr::mutate(failure_class = factor(as.character(failure_class), levels = legend_levels))
    
    legend_dummy <- tibble::tibble(
      year = min(availability$year, na.rm = TRUE),
      month = 1,
      failure_class = factor(legend_levels, levels = legend_levels)
    )
    
    ggplot2::ggplot(availability, ggplot2::aes(x = year, y = month, fill = failure_class)) +
      ggplot2::geom_tile(
        data = legend_dummy,
        ggplot2::aes(x = year, y = month, fill = failure_class),
        alpha = 0,
        show.legend = TRUE,
        inherit.aes = FALSE
      ) +
      ggplot2::geom_tile(width = 1, height = 1, color = "white", linewidth = 0.08) +
      ggplot2::scale_fill_manual(
        name = "Falhas",
        values = failure_colors,
        limits = legend_levels,
        breaks = legend_levels,
        labels = failure_labels,
        drop = FALSE,
        na.translate = FALSE,
        guide = ggplot2::guide_legend(override.aes = list(alpha = 1, color = NA))
      ) +
      ggplot2::scale_x_continuous(name = "Ano", breaks = scales::pretty_breaks(n = 10), expand = c(0, 0)) +
      ggplot2::scale_y_continuous(name = "Mês", breaks = 1:12, labels = 1:12, expand = c(0, 0)) +
      ggplot2::coord_fixed(ratio = 1.2) +
      preview_plot_theme(base_size = 6) +
      ggplot2::theme(
        panel.grid = ggplot2::element_blank(),
        # panel.grid = ggplot2::element_blank(),
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        panel.grid.major.x = ggplot2::element_blank(),
        panel.grid.major.y = ggplot2::element_blank(),
        panel.grid.minor.x = ggplot2::element_blank(),
        panel.grid.minor.y = ggplot2::element_blank(),
        panel.border = ggplot2::element_blank(),
        legend.position = "right",
        plot.margin = ggplot2::margin(6, 8, 6, 8)
      )
  }, res = 120)
  
  output$pluviometric_rainfall_series_status <- renderUI({
    daily <- pluviometric_daily_series()
    if (nrow(daily) == 0) {
      return(tags$div(class = "table-status empty", "Nenhum dado pluviométrico foi carregado nesta sessão."))
    }
    
    n_valid <- sum(daily$is_valid_nonnegative, na.rm = TRUE)
    n_missing <- sum(!daily$is_observed, na.rm = TRUE)
    tags$div(
      class = "table-status available",
      paste0(
        "Série com ",
        fluviometric_format_count(n_valid),
        " dias válidos e ",
        fluviometric_format_count(n_missing),
        " falhas no período."
      )
    )
  })
  
  output$pluviometric_rainfall_summary_cards <- renderUI({
    daily <- pluviometric_daily_series()
    if (nrow(daily) == 0) {
      return(NULL)
    }
    
    valid <- daily$rainfall_mm[daily$is_valid_nonnegative]
    wet_days <- sum(daily$is_wet_day, na.rm = TRUE)
    dry_days <- sum(daily$is_dry_day, na.rm = TRUE)
    total_rain <- sum(valid, na.rm = TRUE)
    n_years <- dplyr::n_distinct(daily$year)
    mean_annual <- if (n_years > 0) total_rain / n_years else NA_real_
    max_daily <- if (length(valid) > 0) max(valid, na.rm = TRUE) else NA_real_
    max_daily_date <- daily$date[which.max(dplyr::if_else(daily$is_valid_nonnegative, daily$rainfall_mm, NA_real_))]
    rx5 <- pluviometric_rolling_sum_complete(dplyr::if_else(daily$is_valid_nonnegative, daily$rainfall_mm, NA_real_), 5L)
    rx5_max <- if (any(!is.na(rx5))) max(rx5, na.rm = TRUE) else NA_real_
    
    tags$div(
      class = "section-card",
      tags$div(
        class = "section-header",
        tags$h3("Resumo da série de precipitação"),
        tags$p("Indicadores descritivos calculados a partir da série diária carregada.")
      ),
      tags$div(
        class = "overview-metric-grid fluviometric-metric-grid",
        overview_metric("Dias no período", fluviometric_format_count(nrow(daily))),
        overview_metric("Valores válidos", fluviometric_format_count(sum(daily$is_valid_nonnegative, na.rm = TRUE))),
        overview_metric("Falhas", fluviometric_format_count(sum(!daily$is_observed, na.rm = TRUE))),
        # overview_metric("Total acumulado", paste0(fluviometric_format_value(total_rain), " mm")),
        overview_metric("Média anual", paste0(fluviometric_format_value(mean_annual), " mm/ano")),
        overview_metric("Dias chuvosos", fluviometric_format_count(wet_days)),
        overview_metric("Dias secos", fluviometric_format_count(dry_days)),
        overview_metric("Máxima diária", paste0(fluviometric_format_value(max_daily), " mm")),
        overview_metric("Data da máxima", if (length(max_daily_date) == 0 || is.na(max_daily_date)) "n/a" else format(max_daily_date, "%d/%m/%Y")),
        overview_metric("Máx. 5 dias", paste0(fluviometric_format_value(rx5_max), " mm"))
      )
    )
  })
  
  output$pluviometric_rainfall_series_plot <- renderPlot({
    daily <- pluviometric_daily_series()
    validate(need(nrow(daily) > 0, "Nenhum dado pluviométrico foi carregado."))
    
    plot_data <- daily |>
      dplyr::filter(is_valid_nonnegative)
    
    validate(need(nrow(plot_data) > 0, "Não há valores válidos não negativos para o hietograma."))
    
    ggplot2::ggplot(plot_data, ggplot2::aes(x = date, y = rainfall_mm)) +
      ggplot2::geom_col(width = 1) +
      ggplot2::scale_x_date(name = "Data", date_breaks = "5 years", date_labels = "%Y") +
      ggplot2::scale_y_continuous(name = "Precipitação diária (mm)", labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
      preview_plot_theme(base_size = 7) +
      ggplot2::theme(plot.margin = ggplot2::margin(6, 8, 6, 8))
  }, res = 144)
  
  pluviometric_consistency_issue_details <- reactive({
    daily <- pluviometric_daily_series()
    if (nrow(daily) == 0) {
      return(tibble::tibble())
    }
    
    base_cols <- c("date", "year", "month", "rainfall_mm", "source_status", "consistency_level", "daily_flag")
    issue_list <- list()
    
    missing_days <- daily |>
      dplyr::filter(!is_observed) |>
      dplyr::transmute(
        dplyr::across(dplyr::all_of(base_cols)),
        tipo_ocorrencia = "Falha diária",
        grupo_ocorrencia = "Cobertura temporal",
        descricao = "Dia sem valor diário de precipitação."
      )
    issue_list <- c(issue_list, list(missing_days))
    
    negative_values <- daily |>
      dplyr::filter(!is.na(rainfall_mm), rainfall_mm < 0) |>
      dplyr::transmute(
        dplyr::across(dplyr::all_of(base_cols)),
        tipo_ocorrencia = "Precipitação negativa",
        grupo_ocorrencia = "Valor impossível",
        descricao = "Valor diário negativo de precipitação."
      )
    issue_list <- c(issue_list, list(negative_values))
    
    duplicated_days <- daily |>
      dplyr::filter(has_duplicate_same_consistency) |>
      dplyr::transmute(
        dplyr::across(dplyr::all_of(base_cols)),
        tipo_ocorrencia = "Registros duplicados no mesmo nível de consistência",
        grupo_ocorrencia = "Duplicação",
        descricao = paste0(
          "Mais de um registro para a mesma data e o mesmo nível de consistência. ",
          "Número máximo de registros no mesmo nível: ",
          max_same_consistency_records,
          "."
        )
      )
    issue_list <- c(issue_list, list(duplicated_days))
    
    status_attention <- daily |>
      dplyr::filter(has_source_status_attention) |>
      dplyr::transmute(
        dplyr::across(dplyr::all_of(base_cols)),
        tipo_ocorrencia = "Status de origem diferente de OK",
        grupo_ocorrencia = "Status da fonte",
        descricao = paste0("Status informado pela fonte: ", source_status)
      )
    issue_list <- c(issue_list, list(status_attention))
    
    fixed_high <- daily |>
      dplyr::filter(is_valid_nonnegative, rainfall_mm >= 400) |>
      dplyr::transmute(
        dplyr::across(dplyr::all_of(base_cols)),
        tipo_ocorrencia = "Chuva diária muito alta",
        grupo_ocorrencia = "Extremo para revisão",
        descricao = "Precipitação diária igual ou superior a 400 mm. Revisar o valor e o status da fonte."
      )
    issue_list <- c(issue_list, list(fixed_high))
    
    positive <- daily |>
      dplyr::filter(is_valid_nonnegative, rainfall_mm > 0)
    if (nrow(positive) >= 10) {
      log_values <- log10(positive$rainfall_mm)
      iqr_values <- stats::IQR(log_values, na.rm = TRUE)
      q3 <- stats::quantile(log_values, 0.75, na.rm = TRUE, names = FALSE)
      high_limit <- q3 + 3 * iqr_values
      robust_high <- positive |>
        dplyr::filter(log10(rainfall_mm) > high_limit) |>
        dplyr::transmute(
          dplyr::across(dplyr::all_of(base_cols)),
          tipo_ocorrencia = "Possível outlier alto",
          grupo_ocorrencia = "Outlier",
          descricao = "Valor acima do limite superior robusto calculado em log10 da chuva positiva."
        )
      issue_list <- c(issue_list, list(robust_high))
    }
    
    zero_runs <- daily |>
      dplyr::mutate(
        zero_flag = is_valid_nonnegative & rainfall_mm == 0,
        run_id = cumsum(c(TRUE, zero_flag[-1] != zero_flag[-dplyr::n()]))
      ) |>
      dplyr::group_by(run_id) |>
      dplyr::mutate(run_length = dplyr::n()) |>
      dplyr::ungroup() |>
      dplyr::filter(zero_flag, run_length >= 30) |>
      dplyr::transmute(
        dplyr::across(dplyr::all_of(base_cols)),
        tipo_ocorrencia = "Sequência longa de zeros",
        grupo_ocorrencia = "Padrão temporal",
        descricao = paste0("Dia dentro de sequência seca com ", run_length, " dias consecutivos de chuva igual a zero.")
      )
    issue_list <- c(issue_list, list(zero_runs))
    
    repeated_positive <- daily |>
      dplyr::mutate(
        value_key = dplyr::if_else(is_valid_nonnegative & rainfall_mm > 0, as.character(round(rainfall_mm, 1)), NA_character_),
        same_as_previous = value_key == dplyr::lag(value_key) & !is.na(value_key),
        run_id = cumsum(dplyr::coalesce(!same_as_previous, TRUE))
      ) |>
      dplyr::group_by(run_id) |>
      dplyr::mutate(run_length = dplyr::n()) |>
      dplyr::ungroup() |>
      dplyr::filter(!is.na(value_key), run_length >= 3) |>
      dplyr::transmute(
        dplyr::across(dplyr::all_of(base_cols)),
        tipo_ocorrencia = "Sequência de valor positivo repetido",
        grupo_ocorrencia = "Padrão temporal",
        descricao = paste0("Sequência com ", run_length, " dias consecutivos de chuva positiva repetida, arredondada a 0,1 mm.")
      )
    issue_list <- c(issue_list, list(repeated_positive))
    
    dplyr::bind_rows(issue_list) |>
      dplyr::arrange(date, grupo_ocorrencia, tipo_ocorrencia)
  })
  
  output$pluviometric_consistency_status <- renderUI({
    daily <- pluviometric_daily_series()
    if (nrow(daily) == 0) {
      return(tags$div(class = "table-status empty", "Nenhum dado pluviométrico foi carregado nesta sessão."))
    }
    issues <- pluviometric_consistency_issue_details()
    tags$div(
      class = "table-status available",
      paste0(
        "Triagem calculada para ",
        fluviometric_format_count(nrow(daily)),
        " dias. Ocorrências registradas: ",
        fluviometric_format_count(nrow(issues)),
        "."
      )
    )
  })
  
  output$pluviometric_consistency_coverage_cards <- renderUI({
    daily <- pluviometric_daily_series()
    if (nrow(daily) == 0) {
      return(NULL)
    }
    n_period <- nrow(daily)
    n_observed <- sum(daily$is_observed, na.rm = TRUE)
    n_valid <- sum(daily$is_valid_nonnegative, na.rm = TRUE)
    n_missing <- sum(!daily$is_observed, na.rm = TRUE)
    n_negative <- sum(!is.na(daily$rainfall_mm) & daily$rainfall_mm < 0, na.rm = TRUE)
    max_gap <- pluviometric_longest_run(!daily$is_observed)
    max_zero_run <- pluviometric_longest_run(daily$is_valid_nonnegative & daily$rainfall_mm == 0)
    
    fluviometric_collapsible_section(
      title = "Cobertura temporal",
      subtitle = "Resumo de disponibilidade diária e sequências de falhas/zeros.",
      open = TRUE,
      tags$div(
        class = "overview-metric-grid fluviometric-metric-grid",
        overview_metric("Dias no período", fluviometric_format_count(n_period)),
        overview_metric("Dias observados", pluviometric_count_pct(n_observed, n_period)),
        overview_metric("Dias válidos", pluviometric_count_pct(n_valid, n_period)),
        overview_metric("Falhas", pluviometric_count_pct(n_missing, n_period)),
        overview_metric("Valores negativos", pluviometric_count_pct(n_negative, n_period)),
        overview_metric("Maior sequência de falhas", paste0(fluviometric_format_count(max_gap), " dias")),
        overview_metric("Maior sequência de zeros", paste0(fluviometric_format_count(max_zero_run), " dias")),
        overview_metric("Anos com dados", fluviometric_format_count(dplyr::n_distinct(daily$year[daily$is_observed])))
      )
    )
  })
  
  output$pluviometric_consistency_value_cards <- renderUI({
    daily <- pluviometric_daily_series()
    if (nrow(daily) == 0) {
      return(NULL)
    }
    issues <- pluviometric_consistency_issue_details()
    n_status <- sum(daily$has_source_status_attention, na.rm = TRUE)
    n_duplicates <- sum(daily$has_duplicate_same_consistency, na.rm = TRUE)
    n_high_fixed <- sum(daily$is_valid_nonnegative & daily$rainfall_mm >= 400, na.rm = TRUE)
    n_repeated <- sum(issues$tipo_ocorrencia == "Sequência de valor positivo repetido", na.rm = TRUE)
    n_zero_runs <- sum(issues$tipo_ocorrencia == "Sequência longa de zeros", na.rm = TRUE)
    n_outlier <- sum(issues$tipo_ocorrencia == "Possível outlier alto", na.rm = TRUE)
    
    fluviometric_collapsible_section(
      title = "Valores e padrões temporais",
      subtitle = "Flags de triagem não-exclusivos. Nenhum valor é excluído automaticamente.",
      open = TRUE,
      tags$div(
        class = "overview-metric-grid fluviometric-metric-grid",
        overview_metric("Status de atenção", fluviometric_format_count(n_status)),
        overview_metric("Dias duplicados", fluviometric_format_count(n_duplicates)),
        overview_metric("Chuva ≥ 400 mm", fluviometric_format_count(n_high_fixed)),
        overview_metric("Possível Outlier alto", fluviometric_format_count(n_outlier)),
        overview_metric("Valor positivo repetido", fluviometric_format_count(n_repeated)),
        overview_metric("Zeros em sequência longa", fluviometric_format_count(n_zero_runs))
      )
    )
  })
  
  output$pluviometric_consistency_report_controls <- renderUI({
    daily <- pluviometric_daily_series()
    if (nrow(daily) == 0) {
      return(NULL)
    }
    issues <- pluviometric_consistency_issue_details()
    tags$div(
      class = "extremes-download-row",
      tags$div(
        class = "extremes-download-item",
        downloadButton(
          outputId = "pluviometric_consistency_issue_report_download",
          label = paste0("Baixar relatório de ocorrências", if (nrow(issues) > 0) paste0(" (", fluviometric_format_count(nrow(issues)), ")") else ""),
          class = "btn-primary"
        )
      )
    )
  })
  
  output$pluviometric_consistency_issue_report_download <- downloadHandler(
    filename = function() {
      paste0("consistencia_pluviometrica_", selected_code(), "_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      issue_table <- pluviometric_consistency_issue_details()
      fluviometric_stats_write_csv_bom(issue_table, file, digits = 3)
    }
  )
  
  output$pluviometric_consistency_issue_table <- DT::renderDT({
    issues <- pluviometric_consistency_issue_details()
    
    display <- issues |>
      dplyr::transmute(
        Data = format(date, "%d/%m/%Y"),
        Ano = as.integer(year),
        Mês = as.integer(month),
        `Precipitação (mm)` = rainfall_mm,
        `Status ANA` = as.character(source_status),
        `Nível de consistência` = as.character(consistency_level),
        `Tipo de ocorrência` = tipo_ocorrencia,
        Grupo = grupo_ocorrencia,
        Descrição = descricao
      ) |>
      dplyr::mutate(
        dplyr::across(
          dplyr::any_of(c(
            "Ano",
            "Mês"
          )),
          ~ dplyr::if_else(
            is.na(.x),
            NA_character_,
            formatC(.x, format = "d")
          )
        ),
        dplyr::across(
          dplyr::any_of(c(
            "Precipitação (mm)"
          )),
          ~ dplyr::if_else(
            is.na(.x),
            NA_character_,
            formatC(.x, format = "f", digits = 1, big.mark = ".", decimal.mark = ",")
          )
        )
      )
    
    DT::datatable(
      sanitize_table_for_dt(display),
      rownames = FALSE,
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })
  
  pluviometric_stats_daily <- reactive({
    pluviometric_daily_series() |>
      dplyr::mutate(
        rainfall_valid_mm = dplyr::if_else(is_valid_nonnegative, rainfall_mm, NA_real_),
        rainfall_wet_mm = dplyr::if_else(is_wet_day, rainfall_mm, NA_real_)
      )
  })
  
  pluviometric_stats_annual <- reactive({
    daily <- pluviometric_stats_daily()
    if (nrow(daily) == 0) {
      return(tibble::tibble())
    }
    
    daily |>
      dplyr::group_by(year) |>
      dplyr::arrange(date, .by_group = TRUE) |>
      dplyr::summarise(
        days_expected = dplyr::n(),
        valid_days = sum(is_valid_nonnegative, na.rm = TRUE),
        missing_days = sum(!is_observed, na.rm = TRUE),
        negative_days = sum(!is.na(rainfall_mm) & rainfall_mm < 0, na.rm = TRUE),
        total_mm = sum(rainfall_valid_mm, na.rm = TRUE),
        wet_days = sum(is_wet_day, na.rm = TRUE),
        dry_days = sum(is_dry_day, na.rm = TRUE),
        rx1day_mm = if (any(is_valid_nonnegative)) max(rainfall_valid_mm, na.rm = TRUE) else NA_real_,
        rx5day_mm = {
          rx5 <- pluviometric_rolling_sum_complete(rainfall_valid_mm, 5L)
          if (any(!is.na(rx5))) max(rx5, na.rm = TRUE) else NA_real_
        },
        r10mm_days = sum(is_valid_nonnegative & rainfall_mm >= 10, na.rm = TRUE),
        r20mm_days = sum(is_valid_nonnegative & rainfall_mm >= 20, na.rm = TRUE),
        r50mm_days = sum(is_valid_nonnegative & rainfall_mm >= 50, na.rm = TRUE),
        sdii_mm_per_wet_day = ifelse(wet_days > 0, total_mm / wet_days, NA_real_),
        cdd_days = pluviometric_longest_run(is_dry_day),
        cwd_days = pluviometric_longest_run(is_wet_day),
        failure_pct = 100 * missing_days / days_expected,
        .groups = "drop"
      )
  })
  
  pluviometric_stats_monthly <- reactive({
    daily <- pluviometric_stats_daily()
    if (nrow(daily) == 0) {
      return(tibble::tibble())
    }
    
    daily |>
      dplyr::group_by(year, month, month_label) |>
      dplyr::summarise(
        days_expected = dplyr::n(),
        valid_days = sum(is_valid_nonnegative, na.rm = TRUE),
        missing_days = sum(!is_observed, na.rm = TRUE),
        total_mm = sum(rainfall_valid_mm, na.rm = TRUE),
        wet_days = sum(is_wet_day, na.rm = TRUE),
        rx1day_mm = if (any(is_valid_nonnegative)) max(rainfall_valid_mm, na.rm = TRUE) else NA_real_,
        failure_pct = 100 * missing_days / days_expected,
        .groups = "drop"
      )
  })
  
  output$pluviometric_stats_status <- renderUI({
    daily <- pluviometric_stats_daily()
    if (nrow(daily) == 0) {
      return(tags$div(class = "table-status empty", "Nenhum dado pluviométrico foi carregado nesta sessão."))
    }
    annual <- pluviometric_stats_annual()
    tags$div(
      class = "table-status available",
      paste0(
        "Estatísticas calculadas para ",
        fluviometric_format_count(nrow(annual)),
        " anos civis. Os máximos anuais da aba de eventos extremos usam ano hidrológico outubro/setembro."
      )
    )
  })
  
  output$pluviometric_stats_summary_cards <- renderUI({
    annual <- pluviometric_stats_annual()
    monthly <- pluviometric_stats_monthly()
    if (nrow(annual) == 0) {
      return(NULL)
    }
    
    wettest_year <- annual$year[which.max(annual$total_mm)]
    driest_year <- annual$year[which.min(annual$total_mm)]
    mean_annual <- mean(annual$total_mm, na.rm = TRUE)
    median_annual <- stats::median(annual$total_mm, na.rm = TRUE)
    max_daily <- max(annual$rx1day_mm, na.rm = TRUE)
    max_5day <- max(annual$rx5day_mm, na.rm = TRUE)
    mean_wet_days <- mean(annual$wet_days, na.rm = TRUE)
    mean_sdii <- mean(annual$sdii_mm_per_wet_day, na.rm = TRUE)
    mean_month <- monthly |>
      dplyr::group_by(month_label) |>
      dplyr::summarise(mean_total_mm = mean(total_mm, na.rm = TRUE), .groups = "drop")
    wettest_month <- as.character(mean_month$month_label[which.max(mean_month$mean_total_mm)])
    driest_month <- as.character(mean_month$month_label[which.min(mean_month$mean_total_mm)])
    
    tags$div(
      class = "section-card",
      tags$div(
        class = "section-header",
        tags$h3("Resumo das estatísticas de precipitação"),
        tags$p("Indicadores anuais e mensais descritivos. Não há preenchimento de falhas ou modelagem.")
      ),
      tags$div(
        class = "overview-metric-grid fluviometric-metric-grid",
        overview_metric("Precipitação média anual", paste0(fluviometric_format_value(mean_annual), " mm")),
        overview_metric("Precipitação mediana anual", paste0(fluviometric_format_value(median_annual), " mm")),
        overview_metric("Ano mais chuvoso", paste0(wettest_year, " — ", fluviometric_format_value(max(annual$total_mm, na.rm = TRUE)), " mm")),
        overview_metric("Ano mais seco", paste0(driest_year, " — ", fluviometric_format_value(min(annual$total_mm, na.rm = TRUE)), " mm")),
        overview_metric("Mês médio mais chuvoso", wettest_month),
        overview_metric("Mês médio mais seco", driest_month),
        overview_metric("Dias chuvosos/ano", fluviometric_format_value(mean_wet_days, digits = 1)),
        overview_metric("SDII médio", paste0(fluviometric_format_value(mean_sdii), " mm/dia chuvoso")),
        overview_metric("Maior chuva diária", paste0(fluviometric_format_value(max_daily), " mm")),
        overview_metric("Maior acumulado 5 dias", paste0(fluviometric_format_value(max_5day), " mm"))
      )
    )
  })
  
  output$pluviometric_stats_download_controls <- renderUI({
    annual <- pluviometric_stats_annual()
    if (nrow(annual) == 0) {
      return(NULL)
    }
    tags$div(
      class = "extremes-download-row",
      tags$div(
        class = "extremes-download-item",
        downloadButton(
          outputId = "pluviometric_stats_annual_download",
          label = "Índices anuais",
          class = "btn-primary"
        )
      ),
      tags$div(
        class = "extremes-download-item",
        downloadButton(
          outputId = "pluviometric_stats_monthly_download",
          label = "Totais mensais",
          class = "btn-primary"
        )
      )
    )
  })
  
  output$pluviometric_stats_annual_download <- downloadHandler(
    filename = function() paste0("estatisticas_anuais_pluviometricas_", selected_code(), "_", format(Sys.Date(), "%Y%m%d"), ".csv"),
    content = function(file) fluviometric_stats_write_csv_bom(pluviometric_stats_annual(), file, digits = 3)
  )
  
  output$pluviometric_stats_monthly_download <- downloadHandler(
    filename = function() paste0("estatisticas_mensais_pluviometricas_", selected_code(), "_", format(Sys.Date(), "%Y%m%d"), ".csv"),
    content = function(file) fluviometric_stats_write_csv_bom(pluviometric_stats_monthly(), file, digits = 3)
  )
  
  output$pluviometric_stats_annual_total_plot <- renderPlot({
    annual <- pluviometric_stats_annual()
    validate(need(nrow(annual) > 0, "Não há estatísticas anuais disponíveis."))
    ggplot2::ggplot(annual, ggplot2::aes(x = year, y = total_mm)) +
      ggplot2::geom_line() +
      ggplot2::scale_x_continuous(name = "Ano civil", breaks = scales::pretty_breaks(n = 7)) +
      ggplot2::scale_y_continuous(name = "Precipitação anual (mm)", labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
      preview_plot_theme(base_size = 5) +
      ggplot2::theme(plot.margin = ggplot2::margin(6, 8, 6, 8))
  }, res = 144)
  
  output$pluviometric_stats_monthly_regime_plot <- renderPlot({
    monthly <- pluviometric_stats_monthly()
    validate(need(nrow(monthly) > 0, "Não há estatísticas mensais disponíveis."))
    regime <- monthly |>
      dplyr::group_by(month, month_label) |>
      dplyr::summarise(mean_total_mm = mean(total_mm, na.rm = TRUE), .groups = "drop")
    ggplot2::ggplot(regime, ggplot2::aes(x = month_label, y = mean_total_mm)) +
      ggplot2::geom_col() +
      ggplot2::scale_x_discrete(name = "Mês") +
      ggplot2::scale_y_continuous(name = "Precipitação média mensal (mm)", labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
      preview_plot_theme(base_size = 5) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1), plot.margin = ggplot2::margin(6, 8, 6, 8))
  }, res = 144)
  
  output$pluviometric_stats_monthly_boxplot <- renderPlot({
    monthly <- pluviometric_stats_monthly()
    validate(need(nrow(monthly) > 0, "Não há estatísticas mensais disponíveis."))
    ggplot2::ggplot(monthly, ggplot2::aes(x = month_label, y = total_mm)) +
      ggplot2::geom_boxplot(outlier.size = 0.8) +
      ggplot2::scale_x_discrete(name = "Mês") +
      ggplot2::scale_y_continuous(name = "Total mensal (mm)", labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
      preview_plot_theme(base_size = 5) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1), plot.margin = ggplot2::margin(6, 8, 6, 8))
  }, res = 144)
  
  output$pluviometric_stats_annual_indices_plot <- renderPlot({
    annual <- pluviometric_stats_annual()
    validate(need(nrow(annual) > 0, "Não há estatísticas anuais disponíveis."))
    plot_data <- annual |>
      dplyr::select(year, R10 = r10mm_days, R20 = r20mm_days, R50 = r50mm_days) |>
      tidyr::pivot_longer(cols = c(R10, R20, R50), names_to = "indice", values_to = "dias")
    ggplot2::ggplot(plot_data, ggplot2::aes(x = year, y = dias, color = indice)) +
      ggplot2::geom_line(linewidth = 0.45) +
      ggplot2::geom_point(size = 0.8) +
      ggplot2::scale_x_continuous(name = "Ano civil", breaks = scales::pretty_breaks(n = 7)) +
      ggplot2::scale_y_continuous(name = "Número de dias") +
      preview_plot_theme(base_size = 5) +
      ggplot2::theme(legend.position = "bottom", plot.margin = ggplot2::margin(6, 8, 6, 8))
  }, res = 144)
  
  output$pluviometric_stats_monthly_wide_table <- DT::renderDT({
    monthly <- pluviometric_stats_monthly()
    display <- monthly |>
      dplyr::select(year, month_label, total_mm) |>
      tidyr::pivot_wider(names_from = month_label, values_from = total_mm) |>
      dplyr::arrange(year) |>
      dplyr::rename(Ano = year)
    formatted_display <- format_display_table_values(display)
    
    if ("Ano" %in% names(formatted_display)) {
      formatted_display$Ano <- ifelse(
        is.na(display$Ano),
        NA_character_,
        as.character(as.integer(display$Ano))
      )
    }
    DT::datatable(
      sanitize_table_for_dt(format_display_table_values(display)),
      rownames = FALSE,
      options = list(pageLength = 12, scrollX = TRUE)
    )
  })
  
  output$pluviometric_stats_annual_table <- DT::renderDT({
    annual <- pluviometric_stats_annual()
    
    display <- annual |>
      dplyr::transmute(
        `Ano civil` = as.integer(year),
        `Falhas (%)` = failure_pct,
        `Total anual (mm)` = total_mm,
        `Dias chuvosos` = as.integer(wet_days),
        `Dias secos` = as.integer(dry_days),
        `Rx1day (mm)` = rx1day_mm,
        `Rx5day (mm)` = rx5day_mm,
        `R10mm` = as.integer(r10mm_days),
        `R20mm` = as.integer(r20mm_days),
        `R50mm` = as.integer(r50mm_days),
        `SDII (mm/dia chuvoso)` = sdii_mm_per_wet_day,
        `CDD (dias)` = as.integer(cdd_days),
        `CWD (dias)` = as.integer(cwd_days)
      ) |>
      dplyr::mutate(
        dplyr::across(
          dplyr::any_of(c(
            "Ano civil",
            "Dias chuvosos",
            "Dias secos",
            "R10mm",
            "R20mm",
            "R50mm",
            "CDD (dias)",
            "CWD (dias)"
          )),
          ~ dplyr::if_else(
            is.na(.x),
            NA_character_,
            formatC(.x, format = "d")
          )
        ),
        dplyr::across(
          dplyr::any_of(c(
            "Falhas (%)",
            "Total anual (mm)",
            "Rx1day (mm)",
            "Rx5day (mm)",
            "SDII (mm/dia chuvoso)"
          )),
          ~ dplyr::if_else(
            is.na(.x),
            NA_character_,
            formatC(.x, format = "f", digits = 1, big.mark = ".", decimal.mark = ",")
          )
        )
      )
    
    DT::datatable(
      sanitize_table_for_dt(display),
      rownames = FALSE,
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })
  
  pluviometric_hydrological_year <- function(date) {
    year <- as.integer(format(date, "%Y"))
    month <- as.integer(format(date, "%m"))
    dplyr::if_else(month >= 10L, year + 1L, year)
  }
  
  
  pluviometric_duration_windows <- function(daily, duration_days = 1L) {
    duration_days <- as.integer(duration_days)
    daily <- daily |>
      dplyr::arrange(date) |>
      dplyr::mutate(
        hydrological_year_day = pluviometric_hydrological_year(date),
        rainfall_for_window = dplyr::if_else(is_valid_nonnegative, rainfall_mm, NA_real_)
      )
    
    if (nrow(daily) == 0) {
      return(tibble::tibble())
    }
    
    if (duration_days <= 1L) {
      return(
        daily |>
          dplyr::transmute(
            date_start = date,
            date_end = date,
            date_center = date,
            hydrological_year = hydrological_year_day,
            p_max_mm = rainfall_for_window,
            is_complete = !is.na(rainfall_for_window),
            has_status_attention_window = has_source_status_attention
          )
      )
    }
    
    out <- vector("list", max(nrow(daily) - duration_days + 1L, 0L))
    if (length(out) == 0) {
      return(tibble::tibble())
    }
    
    for (i in seq_along(out)) {
      j <- i + duration_days - 1L
      window <- daily[i:j, , drop = FALSE]
      same_hyd_year <- dplyr::n_distinct(window$hydrological_year_day) == 1L
      complete <- same_hyd_year && !any(is.na(window$rainfall_for_window))
      out[[i]] <- tibble::tibble(
        date_start = window$date[[1]],
        date_end = window$date[[nrow(window)]],
        date_center = window$date[[ceiling(nrow(window) / 2)]],
        hydrological_year = window$hydrological_year_day[[nrow(window)]],
        p_max_mm = if (complete) sum(window$rainfall_for_window) else NA_real_,
        is_complete = complete,
        has_status_attention_window = any(window$has_source_status_attention, na.rm = TRUE)
      )
    }
    
    dplyr::bind_rows(out)
  }
  
  pluviometric_extremes_annual_max <- reactive({
    daily <- pluviometric_daily_series()
    if (nrow(daily) == 0) {
      return(tibble::tibble())
    }
    
    duration <- 1L
    
    daily_hy <- daily |>
      dplyr::mutate(hydrological_year = pluviometric_hydrological_year(date))
    
    year_summary <- daily_hy |>
      dplyr::group_by(hydrological_year) |>
      dplyr::summarise(
        hydrological_year_start = as.Date(paste0(dplyr::first(hydrological_year) - 1L, "-10-01")),
        hydrological_year_end = as.Date(paste0(dplyr::first(hydrological_year), "-09-30")),
        first_date_in_series = min(date, na.rm = TRUE),
        last_date_in_series = max(date, na.rm = TRUE),
        days_expected_in_session = dplyr::n(),
        valid_days = sum(is_valid_nonnegative, na.rm = TRUE),
        missing_days = sum(!is_observed, na.rm = TRUE),
        negative_days = sum(!is.na(rainfall_mm) & rainfall_mm < 0, na.rm = TRUE),
        status_attention_days = sum(has_source_status_attention, na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        is_partial_hydrological_year = first_date_in_series > hydrological_year_start | last_date_in_series < hydrological_year_end,
        missing_fraction = dplyr::if_else(days_expected_in_session > 0, missing_days / days_expected_in_session, NA_real_),
        valid_fraction = dplyr::if_else(days_expected_in_session > 0, valid_days / days_expected_in_session, NA_real_)
      )
    
    windows <- pluviometric_duration_windows(daily, duration_days = duration)
    valid_windows <- windows |>
      dplyr::filter(is_complete, !is.na(p_max_mm))
    
    max_windows <- valid_windows |>
      dplyr::group_by(hydrological_year) |>
      dplyr::filter(p_max_mm == max(p_max_mm, na.rm = TRUE)) |>
      dplyr::mutate(n_tied_max_windows = dplyr::n()) |>
      dplyr::slice(1) |>
      dplyr::ungroup()
    
    n_windows <- windows |>
      dplyr::group_by(hydrological_year) |>
      dplyr::summarise(
        n_windows_expected = dplyr::n(),
        n_valid_windows = sum(is_complete & !is.na(p_max_mm), na.rm = TRUE),
        .groups = "drop"
      )
    
    annual <- year_summary |>
      dplyr::left_join(n_windows, by = "hydrological_year") |>
      dplyr::left_join(
        max_windows |>
          dplyr::select(
            hydrological_year,
            date_start,
            date_end,
            date_center,
            p_max_mm,
            n_tied_max_windows,
            has_status_attention_window
          ),
        by = "hydrological_year"
      ) |>
      dplyr::mutate(
        duration_days = duration,
        n_windows_expected = dplyr::coalesce(n_windows_expected, 0L),
        n_valid_windows = dplyr::coalesce(n_valid_windows, 0L),
        valid_window_fraction = dplyr::if_else(n_windows_expected > 0, n_valid_windows / n_windows_expected, NA_real_),
        flag_few_valid_days = valid_fraction < 0.80,
        flag_partial_hydrological_year = is_partial_hydrological_year,
        flag_few_valid_windows = valid_window_fraction < 0.80,
        flag_zero_maximum = !is.na(p_max_mm) & p_max_mm <= 0,
        flag_negative_values_year = negative_days > 0,
        flag_status_on_max_window = dplyr::coalesce(has_status_attention_window, FALSE),
        flag_tied_maximum_within_year = dplyr::coalesce(n_tied_max_windows > 1, FALSE)
      )
    
    if (sum(!is.na(annual$p_max_mm) & annual$p_max_mm > 0) >= 4) {
      log_values <- log10(annual$p_max_mm[!is.na(annual$p_max_mm) & annual$p_max_mm > 0])
      iqr_values <- stats::IQR(log_values, na.rm = TRUE)
      q1 <- stats::quantile(log_values, 0.25, na.rm = TRUE, names = FALSE)
      q3 <- stats::quantile(log_values, 0.75, na.rm = TRUE, names = FALSE)
      high_limit <- q3 + 3 * iqr_values
      low_limit <- q1 - 3 * iqr_values
      annual <- annual |>
        dplyr::mutate(
          flag_high_outlier = !is.na(p_max_mm) & p_max_mm > 0 & log10(p_max_mm) > high_limit,
          flag_low_outlier = !is.na(p_max_mm) & p_max_mm > 0 & log10(p_max_mm) < low_limit
        )
    } else {
      annual <- annual |>
        dplyr::mutate(
          flag_high_outlier = FALSE,
          flag_low_outlier = FALSE
        )
    }
    
    annual <- annual |>
      dplyr::mutate(
        p_max_round = round(p_max_mm, 1),
        flag_repeated_annual_maximum = !is.na(p_max_round) &
          (duplicated(p_max_round) | duplicated(p_max_round, fromLast = TRUE)),
        flag_possible_underestimated_maximum = flag_low_outlier &
          (flag_few_valid_days | flag_few_valid_windows | flag_partial_hydrological_year)
      )
    
    flag_cols <- grep("^flag_", names(annual), value = TRUE)
    annual$n_flags <- rowSums(annual[, flag_cols, drop = FALSE], na.rm = TRUE)
    
    flag_labels <- c(
      flag_few_valid_days = "Poucos dias válidos",
      flag_partial_hydrological_year = "Ano hidrológico parcial",
      flag_few_valid_windows = "Poucas janelas válidas",
      flag_zero_maximum = "Máximo igual a zero",
      flag_negative_values_year = "Valor negativo no ano",
      flag_status_on_max_window = "Status de atenção na janela",
      flag_tied_maximum_within_year = "Empate no ano",
      flag_high_outlier = "Possível outlier alto",
      flag_low_outlier = "Possível outlier baixo",
      flag_repeated_annual_maximum = "Máximo repetido entre anos",
      flag_possible_underestimated_maximum = "Possível máximo subestimado"
    )
    
    annual$flags_resumo <- apply(
      annual[, flag_cols, drop = FALSE],
      1,
      function(values) {
        values <- dplyr::coalesce(as.logical(values), FALSE)
        active <- flag_labels[names(values)[values]]
        if (length(active) == 0) "Sem flags" else paste(active, collapse = "; ")
      }
    )
    
    annual <- annual |>
      dplyr::mutate(
        nivel_atencao = dplyr::case_when(
          n_flags == 0 ~ "Sem flags",
          n_flags <= 2 ~ "Atenção baixa",
          n_flags <= 4 ~ "Atenção moderada",
          TRUE ~ "Atenção alta"
        )
      ) |>
      dplyr::arrange(hydrological_year)
    
    annual
  })
  
  output$pluviometric_extremes_status <- renderUI({
    daily <- pluviometric_daily_series()
    if (nrow(daily) == 0) {
      return(tags$div(class = "table-status empty", "Nenhum dado pluviométrico foi carregado nesta sessão."))
    }
    annual <- pluviometric_extremes_annual_max()
    tags$div(
      class = "table-status available",
      paste0(
        "Máximos anuais diários calculados por ano hidrológico outubro/setembro. ",
        "Anos avaliados: ",
        fluviometric_format_count(nrow(annual)),
        "."
      )
    )
  })
  
  output$pluviometric_extremes_summary_cards <- renderUI({
    annual <- pluviometric_extremes_annual_max()
    if (nrow(annual) == 0) {
      return(NULL)
    }
    valid <- annual |>
      dplyr::filter(!is.na(p_max_mm))
    if (nrow(valid) == 0) {
      return(NULL)
    }
    max_row <- valid[which.max(valid$p_max_mm), ]
    median_max <- stats::median(valid$p_max_mm, na.rm = TRUE)
    n_flagged <- sum(valid$n_flags > 0, na.rm = TRUE)
    
    tags$div(
      class = "section-card",
      tags$div(
        class = "section-header",
        tags$h3("Resumo dos máximos anuais de precipitação"),
        tags$p("Série extraída de forma descritiva. Flags são sinais de triagem, não regras automáticas de exclusão.")
      ),
      tags$div(
        class = "overview-metric-grid fluviometric-metric-grid",
        # overview_metric("Duração", paste0(unique(valid$duration_days), " dia(s)")),
        overview_metric("Anos avaliados", fluviometric_format_count(nrow(valid))),
        overview_metric("Máximo observado", paste0(fluviometric_format_value(max_row$p_max_mm), " mm")),
        overview_metric("Ano do máximo", as.character(max_row$hydrological_year)),
        overview_metric("Mediana dos máximos", paste0(fluviometric_format_value(median_max), " mm")),
        overview_metric("Anos com flags", pluviometric_count_pct(n_flagged, nrow(valid))),
        overview_metric("Maior nº de flags", fluviometric_format_count(max(valid$n_flags, na.rm = TRUE)))
      )
    )
  })
  
  output$pluviometric_extremes_annual_max_plot <- renderPlot({
    annual <- pluviometric_extremes_annual_max()
    
    plot_data <- annual |>
      dplyr::filter(!is.na(p_max_mm)) |>
      dplyr::mutate(
        flag_class = dplyr::case_when(
          is.na(n_flags) | n_flags <= 0 ~ "0",
          n_flags == 1 ~ "1",
          n_flags == 2 ~ "2",
          n_flags >= 3 ~ "3+",
          TRUE ~ "0"
        ),
        flag_class = factor(flag_class, levels = c("0", "1", "2", "3+"))
      )
    
    validate(need(nrow(plot_data) > 0, "Não há máximos anuais válidos para plotar."))

    ggplot2::ggplot(
      plot_data,
      ggplot2::aes(x = hydrological_year, y = p_max_mm)
    ) +
      ggplot2::geom_segment(
        ggplot2::aes(
          xend = hydrological_year,
          y = 0,
          yend = p_max_mm,
          color = "#94a3b8"
        ),
        linewidth = 0.35
      ) +
      ggplot2::geom_point(
        ggplot2::aes(color = flag_class),
        size = 1.8
      ) +
      ggplot2::scale_color_manual(
        values = c(
          "0" = "#3288bd",
          "1" = "#fee08b",
          "2" = "#fc8d59",
          "3+" = "#d53e4f"
        ),
        drop = FALSE,
        name = "Nº de flags"
      ) +
      ggplot2::scale_x_continuous(
        name = "Ano hidrológico",
        breaks = scales::pretty_breaks(n = 10)
      ) +
      ggplot2::scale_y_continuous(
        name = "Precipitação máxima diária (mm)",
        labels = scales::label_number(big.mark = ".", decimal.mark = ",")
      ) +
      preview_plot_theme(base_size = 5) +
      ggplot2::theme(
        legend.position = "bottom",
        plot.margin = ggplot2::margin(6, 8, 6, 8)
      )
  }, res = 144)
  
  output$pluviometric_extremes_annual_max_table <- DT::renderDT({
    annual <- pluviometric_extremes_annual_max()
    
    display <- annual |>
      dplyr::transmute(
        `Ano hidrológico` = as.integer(hydrological_year),
        `Data do máximo` = format(date_center, "%d/%m/%Y"),
        `Máximo diário (mm)` = p_max_mm,
        `Falhas no ano` = as.integer(missing_days),
        `Nº de flags` = as.integer(n_flags),
        `Nível de atenção` = nivel_atencao,
        Flags = flags_resumo
      ) |>
      dplyr::mutate(
        dplyr::across(
          dplyr::any_of(c(
            "Ano hidrológico",
            "Falhas no ano",
            "Nº de flags"
          )),
          ~ dplyr::if_else(
            is.na(.x),
            NA_character_,
            formatC(.x, format = "d")
          )
        ),
        dplyr::across(
          dplyr::any_of(c(
            "Máximo diário (mm)"
          )),
          ~ dplyr::if_else(
            is.na(.x),
            NA_character_,
            formatC(.x, format = "f", digits = 1, big.mark = ".", decimal.mark = ",")
          )
        )
      )
    
    DT::datatable(
      sanitize_table_for_dt(display),
      rownames = FALSE,
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })
  
  output$pluviometric_extremes_annual_max_simple_download <- downloadHandler(
    filename = function() paste0("maximos_anuais_pluviometricos_", selected_code(), "_", format(Sys.Date(), "%Y%m%d"), ".csv"),
    content = function(file) {
      simple <- pluviometric_extremes_annual_max() |>
        dplyr::transmute(
          hydrological_year = hydrological_year,
          date_max = date_center,
          p_max_mm = p_max_mm
        )
      fluviometric_stats_write_csv_bom(simple, file, digits = 3)
    }
  )
  
  output$pluviometric_extremes_annual_max_detailed_download <- downloadHandler(
    filename = function() paste0("maximos_anuais_pluviometricos_detalhado_", selected_code(), "_", format(Sys.Date(), "%Y%m%d"), ".csv"),
    content = function(file) {
      fluviometric_stats_write_csv_bom(pluviometric_extremes_annual_max(), file, digits = 3)
    }
  )

  fluviometric_format_count <- function(x) {
    if (length(x) == 0 || is.na(x)) {
      return("—")
    }
    
    formatC(
      as.numeric(x),
      format = "f",
      digits = 0,
      big.mark = ".",
      decimal.mark = ","
    )
  }
  
  fluviometric_format_value <- function(x, digits = 2) {
    if (length(x) == 0 || is.na(x) || !is.finite(x)) {
      return("—")
    }
    
    formatC(
      as.numeric(x),
      format = "f",
      digits = digits,
      big.mark = ".",
      decimal.mark = ","
    )
  }
  
  fluviometric_format_value_vector <- function(x, digits = 2) {
    out <- rep("—", length(x))
    ok <- !is.na(x) & is.finite(x)
    
    out[ok] <- formatC(
      as.numeric(x[ok]),
      format = "f",
      digits = digits,
      big.mark = ".",
      decimal.mark = ","
    )
    
    out
  }
  
  fluviometric_empty_plotly <- function(message) {
    plotly::plot_ly() |>
      plotly::layout(
        xaxis = list(visible = FALSE),
        yaxis = list(visible = FALSE),
        annotations = list(
          list(
            text = message,
            x = 0.5,
            y = 0.5,
            xref = "paper",
            yref = "paper",
            showarrow = FALSE,
            font = list(size = 13, color = "#64748b")
          )
        ),
        margin = list(l = 20, r = 20, t = 20, b = 20)
      ) |>
      plotly::config(displayModeBar = FALSE)
  }
  
  fluviometric_discharge_series <- reactive({
    result <- fluviometric_acquisition_result()
    
    if (is.null(result) || is.null(result$discharge) || nrow(result$discharge) == 0) {
      return(tibble::tibble())
    }
    
    discharge <- result$discharge |>
      dplyr::mutate(
        date = as.Date(date),
        discharge_m3s = as.numeric(value),
        consistency_priority = dplyr::case_when(
          as.character(consistency_level) == "2" ~ 1L,
          as.character(consistency_level) == "1" ~ 2L,
          TRUE ~ 3L
        ),
        value_priority = dplyr::if_else(is.na(discharge_m3s), 2L, 1L)
      ) |>
      dplyr::filter(!is.na(date)) |>
      dplyr::arrange(date, consistency_priority, value_priority) |>
      dplyr::group_by(date) |>
      dplyr::slice(1) |>
      dplyr::ungroup() |>
      dplyr::select(
        date,
        discharge_m3s,
        source_status,
        consistency_level,
        source
      )
    
    if (nrow(discharge) == 0) {
      return(tibble::tibble())
    }
    
    daily_grid <- tibble::tibble(
      date = seq.Date(
        min(discharge$date, na.rm = TRUE),
        max(discharge$date, na.rm = TRUE),
        by = "day"
      )
    )
    
    daily_grid |>
      dplyr::left_join(discharge, by = "date") |>
      dplyr::arrange(date)
  })
  
  output$fluviometric_discharge_series_status <- renderUI({
    discharge <- fluviometric_discharge_series()
    
    if (nrow(discharge) == 0) {
      return(
        tags$div(
          class = "table-status empty",
          "Nenhuma série diária de vazões foi carregada para a estação selecionada."
        )
      )
    }
    
    n_valid <- sum(!is.na(discharge$discharge_m3s))
    n_missing <- sum(is.na(discharge$discharge_m3s))
    
    tags$div(
      class = "table-status available",
      paste0(
        "Série de vazões disponível: ",
        fluviometric_format_count(n_valid),
        " valores válidos"
      ),
      if (n_missing > 0) {
        paste0(" e ", fluviometric_format_count(n_missing), " falhas no período.")
      } else {
        "."
      }
    )
  })
  
  output$fluviometric_discharge_summary_cards <- renderUI({
    discharge <- fluviometric_discharge_series()
    
    if (nrow(discharge) == 0) {
      return(NULL)
    }
    
    values <- discharge$discharge_m3s
    valid_values <- values[!is.na(values)]
    
    date_min <- suppressWarnings(min(discharge$date, na.rm = TRUE))
    date_max <- suppressWarnings(max(discharge$date, na.rm = TRUE))
    
    period_text <- if (is.finite(as.numeric(date_min)) && is.finite(as.numeric(date_max))) {
      paste0(format(date_min, "%d/%m/%Y"), " a ", format(date_max, "%d/%m/%Y"))
    } else {
      "não informado"
    }
    
    if (length(valid_values) == 0) {
      q_min <- q_mean <- q_median <- q_max <- NA_real_
    } else {
      q_min <- min(valid_values, na.rm = TRUE)
      q_mean <- mean(valid_values, na.rm = TRUE)
      q_median <- stats::median(valid_values, na.rm = TRUE)
      q_max <- max(valid_values, na.rm = TRUE)
    }
    
    tags$div(
      class = "section-card",
      tags$div(
        class = "section-header",
        tags$h3("Resumo da série de vazões"),
        # tags$p("Resumo calculado diretamente a partir dos dados diários carregados na sessão.")
      ),
      tags$div(
        class = "overview-metric-grid fluviometric-metric-grid",
        overview_metric("Período", period_text),
        overview_metric("Dias no período", fluviometric_format_count(nrow(discharge))),
        overview_metric("Valores válidos", fluviometric_format_count(sum(!is.na(values)))),
        overview_metric("Falhas", fluviometric_format_count(sum(is.na(values)))),
        overview_metric("Vazão mínima", paste0(fluviometric_format_value(q_min), " m³/s")),
        overview_metric("Vazão média", paste0(fluviometric_format_value(q_mean), " m³/s")),
        overview_metric("Vazão mediana", paste0(fluviometric_format_value(q_median), " m³/s")),
        overview_metric("Vazão máxima", paste0(fluviometric_format_value(q_max), " m³/s"))
      )
    )
  })
  
  output$fluviometric_discharge_series_plotly <- plotly::renderPlotly({
    discharge <- fluviometric_discharge_series()
    
    if (nrow(discharge) == 0) {
      return(
        fluviometric_empty_plotly(
          "Carregue uma série diária de vazões na aba Obtenção de dados."
        )
      )
    }
    
    plot_data <- discharge |>
      dplyr::mutate(
        discharge_text = dplyr::if_else(
          is.na(discharge_m3s),
          "ausente",
          paste0(fluviometric_format_value_vector(discharge_m3s), " m³/s")
        ),
        consistency_text = dplyr::coalesce(as.character(consistency_level), "—"),
        status_text = dplyr::coalesce(as.character(source_status), "—"),
        hover_text = paste0(
          "Data: ", format(date, "%d/%m/%Y"),
          "<br>Vazão: ", discharge_text,
          "<br>Nível de consistência: ", consistency_text,
          "<br>Status: ", status_text
        )
      )
    
    plotly::plot_ly(
      data = plot_data,
      x = ~date,
      y = ~discharge_m3s,
      type = "scatter",
      mode = "lines",
      name = "Vazão diária",
      text = ~hover_text,
      hoverinfo = "text",
      line = list(
        width = 1.1,
        color = "#2b8cbe"
      ),
      connectgaps = FALSE
    ) |>
      plotly::layout(
        xaxis = list(
          title = list(
            text = "Data",
            font = list(size = 15, family = "Segoe UI, Arial, sans-serif", color = "#000000")
          ),
          rangeslider = list(
            visible = TRUE,
            bgcolor = "#f8fafc",
            bordercolor = "#dbe4ee",
            borderwidth = 1
          ),
          rangeselector = list(
            buttons = list(
              list(count = 1, label = "1 ano", step = "year", stepmode = "backward"),
              list(count = 5, label = "5 anos", step = "year", stepmode = "backward"),
              list(count = 10, label = "10 anos", step = "year", stepmode = "backward"),
              list(step = "all", label = "Tudo")
            ),
            bgcolor = "#f8fafc",
            bordercolor = "#dbe4ee",
            borderwidth = 1
          ),
          fixedrange = FALSE,
          showline = TRUE,
          linecolor = "#000000",
          linewidth = 1,
          ticks = "outside",
          tickcolor = "#000000",
          tickwidth = 1,
          tickfont = list(size = 13, family = "Segoe UI, Arial, sans-serif", color = "#334155"),
          gridcolor = "#e5e7eb",
          gridwidth = 1,
          zeroline = FALSE
        ),
        yaxis = list(
          title = list(
            text = "Vazão (m³/s)",
            font = list(size = 15, family = "Segoe UI, Arial, sans-serif", color = "#000000")
          ),
          fixedrange = TRUE,
          showline = TRUE,
          linecolor = "#000000",
          linewidth = 1,
          ticks = "outside",
          tickcolor = "#000000",
          tickwidth = 1,
          tickfont = list(size = 13, family = "Segoe UI, Arial, sans-serif", color = "#334155"),
          gridcolor = "#d1d5db",
          gridwidth = 1,
          zeroline = FALSE
        ),
        # legend = list(
        #   orientation = "h",
        #   x = 0,
        #   y = -0.35,
        #   font = list(size = 13, family = "Segoe UI, Arial, sans-serif", color = "#334155")
        # ),
        plot_bgcolor = "#ffffff",
        paper_bgcolor = "#ffffff",
        margin = list(l = 62, r = 16, t = 18, b = 55),
        hovermode = "x unified",
        font = list(
          family = "Segoe UI, Arial, sans-serif",
          size = 13,
          color = "#334155"
        )
      ) |>
      plotly::config(
        scrollZoom = TRUE,
        displaylogo = FALSE
      )
  })
  

