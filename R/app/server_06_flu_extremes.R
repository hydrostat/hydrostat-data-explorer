# ============================================================
# server_06_flu_extremes.R
# Purpose: Annual maxima, POT, and annual low-flow analyses.
# ============================================================
# BEGIN ORIGINAL BODY
  # ------------------------------------------------------------
  # Fluviometric tab: extreme events - annual maxima
  # ------------------------------------------------------------
  
  fluviometric_extremes_hydrological_year <- function(date) {
    date <- as.Date(date)
    year <- as.integer(format(date, "%Y"))
    month <- as.integer(format(date, "%m"))
    dplyr::if_else(month >= 10L, year + 1L, year)
  }
  
  fluviometric_extremes_yes_blank <- function(x) {
    x <- as.logical(x)
    x[is.na(x)] <- FALSE
    ifelse(x, "Sim", "")
  }
  
  fluviometric_extremes_flag_palette <- function() {
    c(
      "0" = "#3288bd",
      "1" = "#fee08b",
      "2" = "#fc8d59",
      "3+" = "#d53e4f"
    )
  }
  
  fluviometric_extremes_annual_maxima <- reactive({
    daily <- fluviometric_stats_daily()
    
    if (nrow(daily) == 0 || !any(daily$has_discharge, na.rm = TRUE)) {
      return(tibble::tibble())
    }
    
    daily_hydro <- daily |>
      dplyr::mutate(
        date = as.Date(date),
        discharge_m3s = as.numeric(discharge_m3s),
        has_discharge = is.finite(discharge_m3s),
        hydrological_year = fluviometric_extremes_hydrological_year(date),
        hydrological_year_label = paste0(hydrological_year - 1L, "/", hydrological_year),
        period_start = as.Date(sprintf("%04d-10-01", hydrological_year - 1L)),
        period_end = as.Date(sprintf("%04d-09-30", hydrological_year))
      ) |>
      dplyr::filter(!is.na(date), !is.na(hydrological_year))
    
    if (nrow(daily_hydro) == 0) {
      return(tibble::tibble())
    }
    
    annual_base <- daily_hydro |>
      dplyr::group_by(hydrological_year, hydrological_year_label, period_start, period_end) |>
      dplyr::summarise(
        n_days_expected = as.integer(dplyr::first(period_end) - dplyr::first(period_start) + 1L),
        n_days_in_series = dplyr::n(),
        n_days_valid = sum(has_discharge, na.rm = TRUE),
        n_days_missing = n_days_expected - n_days_valid,
        completeness_pct = 100 * n_days_valid / n_days_expected,
        missing_fraction = n_days_missing / n_days_expected,
        q_max_m3s = if (any(has_discharge, na.rm = TRUE)) {
          max(discharge_m3s[has_discharge], na.rm = TRUE)
        } else {
          NA_real_
        },
        .groups = "drop"
      ) |>
      dplyr::arrange(hydrological_year)
    
    max_ties <- daily_hydro |>
      dplyr::inner_join(
        annual_base |>
          dplyr::select(hydrological_year, q_max_m3s),
        by = "hydrological_year"
      ) |>
      dplyr::filter(
        has_discharge,
        is.finite(discharge_m3s),
        is.finite(q_max_m3s),
        dplyr::near(discharge_m3s, q_max_m3s)
      ) |>
      dplyr::group_by(hydrological_year) |>
      dplyr::summarise(
        date_max = min(date, na.rm = TRUE),
        n_days_equal_to_max = dplyr::n(),
        dates_equal_to_max = paste(format(sort(date), "%Y-%m-%d"), collapse = "; "),
        flag_tied_max_within_year = n_days_equal_to_max > 1L,
        flag_flat_peak = any(diff(as.integer(sort(date))) == 1L),
        .groups = "drop"
      )
    
    annual_max <- annual_base |>
      dplyr::left_join(max_ties, by = "hydrological_year")
    
    # Gaps around the selected maximum: two days before and two days after.
    date_lookup <- daily_hydro |>
      dplyr::select(date, has_discharge)
    
    series_min_date <- min(daily_hydro$date, na.rm = TRUE)
    series_max_date <- max(daily_hydro$date, na.rm = TRUE)
    
    gap_window <- lapply(seq_len(nrow(annual_max)), function(i) {
      date_max <- annual_max$date_max[i]
      
      if (is.na(date_max)) {
        return(tibble::tibble(
          hydrological_year = annual_max$hydrological_year[i],
          n_missing_near_max = NA_integer_,
          flag_gap_near_max = FALSE,
          flag_strong_gap_near_max = FALSE,
          flag_adjacent_gap_near_max = FALSE,
          flag_window_truncated = FALSE
        ))
      }
      
      window_dates <- seq.Date(date_max - 2L, date_max + 2L, by = "day")
      match_index <- match(window_dates, date_lookup$date)
      has_window <- date_lookup$has_discharge[match_index]
      has_window[is.na(has_window)] <- FALSE
      
      adjacent_dates <- c(date_max - 1L, date_max + 1L)
      adjacent_missing <- !has_window[window_dates %in% adjacent_dates]
      
      n_missing <- sum(!has_window)
      
      tibble::tibble(
        hydrological_year = annual_max$hydrological_year[i],
        n_missing_near_max = n_missing,
        flag_gap_near_max = n_missing > 0L,
        flag_strong_gap_near_max = n_missing >= 2L,
        flag_adjacent_gap_near_max = any(adjacent_missing),
        flag_window_truncated = any(window_dates < series_min_date | window_dates > series_max_date)
      )
    }) |>
      dplyr::bind_rows()
    
    annual_max <- annual_max |>
      dplyr::left_join(gap_window, by = "hydrological_year")
    
    # Join with fluviometric consistency occurrences on the date of the annual maximum.
    consistency_issues <- fluviometric_consistency_issue_details()
    
    if (!is.null(consistency_issues) && nrow(consistency_issues) > 0) {
      issues_by_date <- consistency_issues |>
        dplyr::mutate(data = as.Date(data)) |>
        dplyr::filter(!is.na(data)) |>
        dplyr::group_by(data) |>
        dplyr::summarise(
          n_consistency_issues_on_max = dplyr::n(),
          consistency_issue_types_on_max = paste(sort(unique(ocorrencia)), collapse = "; "),
          .groups = "drop"
        ) |>
        dplyr::rename(date_max = data)
    } else {
      issues_by_date <- tibble::tibble(
        date_max = as.Date(character()),
        n_consistency_issues_on_max = integer(),
        consistency_issue_types_on_max = character()
      )
    }
    
    annual_max <- annual_max |>
      dplyr::left_join(issues_by_date, by = "date_max") |>
      dplyr::mutate(
        n_consistency_issues_on_max = dplyr::coalesce(n_consistency_issues_on_max, 0L),
        consistency_issue_types_on_max = dplyr::coalesce(consistency_issue_types_on_max, ""),
        flag_consistency_issue_on_max = n_consistency_issues_on_max > 0L
      )
    
    valid_max <- annual_max |>
      dplyr::filter(is.finite(q_max_m3s))
    
    n_valid_max <- nrow(valid_max)
    
    if (n_valid_max > 0) {
      annual_max <- annual_max |>
        dplyr::mutate(
          rank_ascending = dplyr::if_else(
            is.finite(q_max_m3s),
            as.integer(rank(q_max_m3s, ties.method = "min", na.last = "keep")),
            NA_integer_
          ),
          rank_descending = dplyr::if_else(
            is.finite(q_max_m3s),
            as.integer(rank(-q_max_m3s, ties.method = "min", na.last = "keep")),
            NA_integer_
          ),
          flag_papalexiou_candidate = is.finite(q_max_m3s) &
            !is.na(rank_ascending) &
            rank_ascending <= 0.40 * n_valid_max &
            missing_fraction >= 1 / 3
        )
    } else {
      annual_max <- annual_max |>
        dplyr::mutate(
          rank_ascending = NA_integer_,
          rank_descending = NA_integer_,
          flag_papalexiou_candidate = FALSE
        )
    }
    
    # IQR outlier screening on log10 annual maxima.
    positive_max <- annual_max$q_max_m3s[is.finite(annual_max$q_max_m3s) & annual_max$q_max_m3s > 0]
    
    if (length(positive_max) >= 4) {
      log_max <- log10(positive_max)
      q1 <- as.numeric(stats::quantile(log_max, 0.25, na.rm = TRUE, names = FALSE))
      q3 <- as.numeric(stats::quantile(log_max, 0.75, na.rm = TRUE, names = FALSE))
      iqr <- q3 - q1
      
      if (is.finite(iqr) && iqr > 0) {
        lower_attention <- q1 - 1.5 * iqr
        lower_strong <- q1 - 3.0 * iqr
        upper_attention <- q3 + 1.5 * iqr
        upper_strong <- q3 + 3.0 * iqr
        
        annual_max <- annual_max |>
          dplyr::mutate(
            log_q_max = dplyr::if_else(q_max_m3s > 0, log10(q_max_m3s), NA_real_),
            high_outlier_iqr_class = dplyr::case_when(
              is.finite(log_q_max) & log_q_max > upper_strong ~ "forte",
              is.finite(log_q_max) & log_q_max > upper_attention ~ "atenção",
              TRUE ~ "sem sinal"
            ),
            low_outlier_iqr_class = dplyr::case_when(
              is.finite(log_q_max) & log_q_max < lower_strong ~ "forte",
              is.finite(log_q_max) & log_q_max < lower_attention ~ "atenção",
              TRUE ~ "sem sinal"
            ),
            flag_high_outlier_iqr = high_outlier_iqr_class != "sem sinal",
            flag_low_outlier_iqr = low_outlier_iqr_class != "sem sinal"
          )
      } else {
        annual_max <- annual_max |>
          dplyr::mutate(
            log_q_max = dplyr::if_else(q_max_m3s > 0, log10(q_max_m3s), NA_real_),
            high_outlier_iqr_class = "sem sinal",
            low_outlier_iqr_class = "sem sinal",
            flag_high_outlier_iqr = FALSE,
            flag_low_outlier_iqr = FALSE
          )
      }
    } else {
      annual_max <- annual_max |>
        dplyr::mutate(
          log_q_max = dplyr::if_else(q_max_m3s > 0, log10(q_max_m3s), NA_real_),
          high_outlier_iqr_class = "sem sinal",
          low_outlier_iqr_class = "sem sinal",
          flag_high_outlier_iqr = FALSE,
          flag_low_outlier_iqr = FALSE
        )
    }
    
    # Repeated annual maxima across hydrological years.
    repeated_max <- annual_max |>
      dplyr::filter(is.finite(q_max_m3s)) |>
      dplyr::mutate(q_max_key = sprintf("%.3f", q_max_m3s)) |>
      dplyr::add_count(q_max_key, name = "n_years_same_q_max") |>
      dplyr::transmute(
        hydrological_year,
        n_years_same_q_max,
        flag_repeated_annual_max_across_years = n_years_same_q_max > 1L
      )
    
    annual_max <- annual_max |>
      dplyr::left_join(repeated_max, by = "hydrological_year") |>
      dplyr::mutate(
        n_years_same_q_max = dplyr::coalesce(n_years_same_q_max, 0L),
        flag_repeated_annual_max_across_years = dplyr::coalesce(flag_repeated_annual_max_across_years, FALSE)
      )
    
    primary_flag_labels <- c(
      flag_gap_near_max = "falhas no entorno do máximo",
      flag_high_outlier_iqr = "outlier alto",
      flag_low_outlier_iqr = "outlier baixo",
      flag_papalexiou_candidate = "candidato à exclusão",
      flag_consistency_issue_on_max = "ocorrência na consistência",
      flag_tied_max_within_year = "empate no máximo anual",
      flag_repeated_annual_max_across_years = "máximo repetido entre anos"
    )
    
    primary_flag_columns <- names(primary_flag_labels)
    
    for (column in primary_flag_columns) {
      if (!column %in% names(annual_max)) {
        annual_max[[column]] <- FALSE
      }
      annual_max[[column]] <- as.logical(annual_max[[column]])
      annual_max[[column]][is.na(annual_max[[column]])] <- FALSE
    }
    
    flag_matrix <- annual_max[, primary_flag_columns, drop = FALSE]
    annual_max$n_flags <- rowSums(as.data.frame(flag_matrix))
    
    annual_max$flags_resumo <- apply(flag_matrix, 1, function(row_flags) {
      active <- primary_flag_labels[as.logical(row_flags)]
      if (length(active) == 0) {
        return("sem flag")
      }
      paste(active, collapse = "; ")
    })
    
    annual_max |>
      dplyr::mutate(
        attention_score = dplyr::case_when(
          n_flags == 0L ~ 0L,
          flag_papalexiou_candidate | flag_consistency_issue_on_max ~ pmax(n_flags, 2L),
          TRUE ~ n_flags
        ),
        nivel_atencao = dplyr::case_when(
          attention_score == 0L ~ "sem flag",
          attention_score == 1L ~ "revisar",
          attention_score == 2L ~ "atenção moderada",
          TRUE ~ "atenção alta"
        ),
        n_flags_class = dplyr::case_when(
          n_flags == 0L ~ "0",
          n_flags == 1L ~ "1",
          n_flags == 2L ~ "2",
          TRUE ~ "3+"
        ),
        n_flags_class = factor(n_flags_class, levels = c("0", "1", "2", "3+")),
        period_label = paste0(format(period_start, "%Y-%m-%d"), " a ", format(period_end, "%Y-%m-%d"))
      ) |>
      dplyr::arrange(hydrological_year)
  })
  
  output$fluviometric_extremes_status <- renderUI({
    result <- fluviometric_acquisition_result()
    
    if (is.null(result)) {
      return(
        tags$div(
          class = "table-status empty",
          "Nenhum dado fluviométrico foi carregado. Use primeiro a aba Obtenção de dados."
        )
      )
    }
    
    maxima <- fluviometric_extremes_annual_maxima()
    
    if (nrow(maxima) == 0 || !any(is.finite(maxima$q_max_m3s))) {
      return(
        tags$div(
          class = "table-status warning",
          "A sessão atual não possui vazões diárias válidas para extrair máximos anuais."
        )
      )
    }
    
    tags$div(
      class = "table-status available",
      "Máximos anuais extraídos por ano hidrológico outubro/setembro. Os flags são sinais de triagem e não removem automaticamente nenhum valor."
    )
  })
  
  output$fluviometric_extremes_summary_cards <- renderUI({
    maxima <- fluviometric_extremes_annual_maxima()
    
    if (nrow(maxima) == 0 || !any(is.finite(maxima$q_max_m3s))) {
      return(NULL)
    }
    
    valid_maxima <- maxima |>
      dplyr::filter(is.finite(q_max_m3s))
    
    record_max <- valid_maxima |>
      dplyr::slice_max(q_max_m3s, n = 1, with_ties = FALSE)
    
    tags$div(
      class = "section-card",
      tags$div(
        class = "section-header",
        tags$h3("Resumo dos máximos anuais"),
        tags$p("Síntese da extração por ano hidrológico e dos flags de triagem.")
      ),
      tags$div(
        class = "overview-metric-grid fluviometric-metric-grid",
        overview_metric("Anos hidrológicos", fluviometric_format_count(nrow(valid_maxima))),
        overview_metric("Anos com flags", fluviometric_format_count(sum(valid_maxima$n_flags > 0L, na.rm = TRUE))),
        overview_metric("Candidatos à exclusão", fluviometric_format_count(sum(valid_maxima$flag_papalexiou_candidate, na.rm = TRUE))),
        overview_metric("Ocorrências na consistência", fluviometric_format_count(sum(valid_maxima$flag_consistency_issue_on_max, na.rm = TRUE))),
        overview_metric(
          "Maior máximo observado",
          paste0(
            record_max$hydrological_year_label[1],
            " — ",
            fluviometric_format_value(record_max$q_max_m3s[1]),
            " m³/s"
          )
        ),
        overview_metric("Máximos empatados no ano", fluviometric_format_count(sum(valid_maxima$flag_tied_max_within_year, na.rm = TRUE)))
      )
    )
  })
  
  output$fluviometric_extremes_annual_max_plot <- renderPlot({
    plot_data <- fluviometric_extremes_annual_maxima() |>
      dplyr::filter(is.finite(q_max_m3s))
    
    if (nrow(plot_data) == 0) {
      draw_empty_plot("Sem dados suficientes para extrair máximos anuais.")
      return(invisible(NULL))
    }
    
    flag_palette <- fluviometric_extremes_flag_palette()
    
    ggplot2::ggplot(
      plot_data,
      ggplot2::aes(x = hydrological_year, y = q_max_m3s)
    ) +
      ggplot2::geom_segment(
        ggplot2::aes(xend = hydrological_year, y = 0, yend = q_max_m3s),
        color = "#94a3b8",
        linewidth = 0.35,
        lineend = "round"
      ) +
      ggplot2::geom_point(
        ggplot2::aes(fill = n_flags_class),
        shape = 21,
        color = "grey20",
        stroke = 0.25,
        size = 1.8,
        show.legend = TRUE
      ) +
      ggplot2::scale_fill_manual(
        name = "Nº de flags",
        values = flag_palette,
        limits = c("0", "1", "2", "3+"),
        breaks = c("0", "1", "2", "3+"),
        labels = c("0", "1", "2", "3+"),
        drop = FALSE,
        na.translate = FALSE
      ) +
      ggplot2::guides(
        fill = ggplot2::guide_legend(
          nrow = 1,
          override.aes = list(
            shape = 21,
            fill = unname(flag_palette[c("0", "1", "2", "3+")]),
            color = "grey20",
            size = 2.6,
            stroke = 0.25
          )
        )
      ) +
      ggplot2::scale_x_continuous(
        name = "Ano hidrológico",
        breaks = scales::pretty_breaks(n = 8)
      ) +
      ggplot2::scale_y_continuous(
        name = "Máximo anual (m³/s)",
        labels = scales::label_number(decimal.mark = ",", big.mark = "."),
        expand = ggplot2::expansion(mult = c(0.02, 0.08))
      ) +
      preview_plot_theme(base_size = 5.5) +
      ggplot2::theme(
        legend.position = "bottom",
        plot.margin = ggplot2::margin(6, 8, 6, 8)
      )
  }, res = 144)
  
  output$fluviometric_extremes_annual_max_table <- DT::renderDT({
    maxima <- fluviometric_extremes_annual_maxima()
    
    if (nrow(maxima) == 0) {
      return(DT::datatable(
        tibble::tibble(mensagem = "Nenhum máximo anual disponível."),
        rownames = FALSE,
        options = list(dom = "t")
      ))
    }
    
    table_data <- maxima |>
      dplyr::mutate(
        outlier_label = dplyr::case_when(
          flag_high_outlier_iqr ~ "alto",
          flag_low_outlier_iqr ~ "baixo",
          TRUE ~ ""
        )
      ) |>
      dplyr::transmute(
        `Ano hidrológico` = hydrological_year_label,
        `Data do máximo` = format(date_max, "%Y-%m-%d"),
        `Q máxima (m³/s)` = round(q_max_m3s, 3),
        `Falhas (%)` = round(100 * missing_fraction, 1),
        `Rank` = rank_descending,
        `Dias empatados no máximo` = n_days_equal_to_max,
        `Datas empatadas` = dates_equal_to_max,
        `Falhas no entorno` = fluviometric_extremes_yes_blank(flag_gap_near_max),
        `Outlier` = outlier_label,
        `Candidato à exclusão` = fluviometric_extremes_yes_blank(flag_papalexiou_candidate),
        `Ocorrência na consistência` = fluviometric_extremes_yes_blank(flag_consistency_issue_on_max),
        `Máximo repetido entre anos` = fluviometric_extremes_yes_blank(flag_repeated_annual_max_across_years),
        `Nº flags` = n_flags,
        `Nível de atenção` = nivel_atencao,
        `Flags` = flags_resumo
      )
    
    DT::datatable(
      table_data,
      rownames = FALSE,
      filter = "top",
      options = list(
        pageLength = 12,
        scrollX = TRUE,
        order = list(list(0, "asc")),
        language = list(url = "//cdn.datatables.net/plug-ins/1.13.6/i18n/pt-BR.json")
      )
    )
  })
  
  output$fluviometric_extremes_annual_max_simple_download <- downloadHandler(
    filename = function() {
      paste0(
        "maximos_anuais_vazao_simples_",
        as.character(selected_code()),
        "_",
        format(Sys.Date(), "%Y%m%d"),
        ".csv"
      )
    },
    content = function(file) {
      table_data <- fluviometric_extremes_annual_maxima() |>
        dplyr::filter(is.finite(q_max_m3s)) |>
        dplyr::transmute(
          ano_hidrologico = hydrological_year_label,
          data_maximo = date_max,
          q_max_m3s = q_max_m3s
        )
      
      if (nrow(table_data) == 0) {
        table_data <- tibble::tibble(
          mensagem = "Nenhum máximo anual disponível."
        )
      }
      
      fluviometric_stats_write_csv_bom(table_data, file, digits = 3)
    }
  )
  
  output$fluviometric_extremes_annual_max_download <- downloadHandler(
    filename = function() {
      paste0(
        "maximos_anuais_vazao_detalhado_",
        as.character(selected_code()),
        "_",
        format(Sys.Date(), "%Y%m%d"),
        ".csv"
      )
    },
    content = function(file) {
      table_data <- fluviometric_extremes_annual_maxima() |>
        dplyr::mutate(
          outlier_label = dplyr::case_when(
            flag_high_outlier_iqr ~ "alto",
            flag_low_outlier_iqr ~ "baixo",
            TRUE ~ ""
          )
        ) |>
        dplyr::transmute(
          ano_hidrologico = hydrological_year_label,
          data_maximo = date_max,
          q_max_m3s = q_max_m3s,
          falhas_pct = 100 * missing_fraction,
          rank = rank_descending,
          n_dias_iguais_ao_maximo = n_days_equal_to_max,
          datas_iguais_ao_maximo = dates_equal_to_max,
          falhas_no_entorno = flag_gap_near_max,
          outlier = outlier_label,
          candidato_a_exclusao = flag_papalexiou_candidate,
          ocorrencia_na_consistencia = flag_consistency_issue_on_max,
          maximo_repetido_entre_anos = flag_repeated_annual_max_across_years,
          n_flags = n_flags,
          nivel_atencao = nivel_atencao,
          flags_resumo = flags_resumo
        )
      
      if (nrow(table_data) == 0) {
        table_data <- tibble::tibble(
          mensagem = "Nenhum máximo anual disponível."
        )
      }
      
      fluviometric_stats_write_csv_bom(table_data, file, digits = 3)
    }
  )
  
  # ------------------------------------------------------------
  # Fluviometric tab: POT descriptive series
  # ------------------------------------------------------------
  
  fluviometric_extremes_pot_threshold_value <- reactiveVal(NA_real_)
  fluviometric_extremes_pot_threshold_method <- reactiveVal("")
  fluviometric_extremes_pot_auto_table <- reactiveVal(tibble::tibble())
  
  fluviometric_extremes_parse_number <- function(x) {
    x <- trimws(as.character(x))
    x <- gsub("\\.", "", x)
    x <- gsub(",", ".", x)
    value <- suppressWarnings(as.numeric(x))
    if (!is.finite(value)) {
      return(NA_real_)
    }
    value
  }
  
  fluviometric_extremes_pot_daily <- reactive({
    daily <- fluviometric_stats_daily()
    
    if (nrow(daily) == 0) {
      return(tibble::tibble())
    }
    
    daily |>
      dplyr::mutate(
        date = as.Date(date),
        discharge_m3s = as.numeric(discharge_m3s),
        has_discharge = is.finite(discharge_m3s) & discharge_m3s > 0
      ) |>
      dplyr::filter(!is.na(date)) |>
      dplyr::arrange(date)
  })
  
  fluviometric_extremes_pot_test <- function(events, daily, min_separation_days = 7L, alpha = 0.05) {
    if (nrow(events) == 0 || nrow(daily) == 0) {
      return(tibble::tibble(
        n_events = 0L,
        lambda_observed = NA_real_,
        min_separation_ok = FALSE,
        kendall_p_value = NA_real_,
        kendall_ok = FALSE,
        dispersion_index = NA_real_,
        poisson_lower = NA_real_,
        poisson_upper = NA_real_,
        poisson_ok = FALSE,
        hypotheses_ok = FALSE,
        message = "Nenhum evento POT foi extraído."
      ))
    }
    
    first_year <- min(as.integer(format(daily$date, "%Y")), na.rm = TRUE)
    last_year <- max(as.integer(format(daily$date, "%Y")), na.rm = TRUE)
    all_years <- seq(first_year, last_year)
    n_years <- length(all_years)
    
    counts <- tibble::tibble(year = all_years) |>
      dplyr::left_join(
        events |>
          dplyr::count(year, name = "n_events_year"),
        by = "year"
      ) |>
      dplyr::mutate(n_events_year = dplyr::coalesce(n_events_year, 0L))
    
    n_events <- nrow(events)
    lambda_observed <- n_events / n_years
    
    date_diffs <- diff(as.integer(events$date_peak))
    min_separation_ok <- length(date_diffs) == 0 || all(date_diffs >= min_separation_days)
    
    if (n_events >= 6 && stats::sd(events$q_peak_m3s, na.rm = TRUE) > 0) {
      kendall_test <- suppressWarnings(stats::cor.test(
        events$q_peak_m3s[-n_events],
        events$q_peak_m3s[-1],
        method = "kendall",
        exact = FALSE
      ))
      kendall_p_value <- as.numeric(kendall_test$p.value)
      kendall_ok <- is.finite(kendall_p_value) && kendall_p_value >= alpha
    } else {
      kendall_p_value <- NA_real_
      kendall_ok <- TRUE
    }
    
    if (n_years >= 5 && mean(counts$n_events_year, na.rm = TRUE) > 0) {
      dispersion_index <- stats::var(counts$n_events_year, na.rm = TRUE) /
        mean(counts$n_events_year, na.rm = TRUE)
      poisson_lower <- stats::qchisq(alpha / 2, df = n_years - 1L) / (n_years - 1L)
      poisson_upper <- stats::qchisq(1 - alpha / 2, df = n_years - 1L) / (n_years - 1L)
      poisson_ok <- is.finite(dispersion_index) &&
        dispersion_index >= poisson_lower &&
        dispersion_index <= poisson_upper
    } else {
      dispersion_index <- NA_real_
      poisson_lower <- NA_real_
      poisson_upper <- NA_real_
      poisson_ok <- FALSE
    }
    
    enough_events <- n_events >= 10L
    
    selection_ok <- enough_events && min_separation_ok
    
    hypotheses_ok <- selection_ok &&
      kendall_ok &&
      poisson_ok
    
    message <- dplyr::case_when(
      hypotheses_ok ~ "A série POT atende aos critérios diagnósticos adotados.",
      !enough_events ~ "A série POT tem poucos eventos para avaliação.",
      !min_separation_ok ~ "A série POT não atende ao critério mínimo de separação entre eventos.",
      selection_ok && !kendall_ok ~ "Limiar selecionado, mas há sinal de dependência serial entre picos sucessivos.",
      selection_ok && !poisson_ok ~ "Limiar selecionado, mas a contagem anual de eventos não é compatível com a hipótese de ocorrência aproximadamente Poisson.",
      selection_ok ~ "Limiar selecionado, mas a série POT requer atenção.",
      TRUE ~ "A série POT requer revisão."
    )
    
    tibble::tibble(
      n_events = n_events,
      lambda_observed = lambda_observed,
      min_separation_ok = min_separation_ok,
      kendall_p_value = kendall_p_value,
      kendall_ok = kendall_ok,
      dispersion_index = dispersion_index,
      poisson_lower = poisson_lower,
      poisson_upper = poisson_upper,
      poisson_ok = poisson_ok,
      selection_ok = selection_ok,
      hypotheses_ok = hypotheses_ok,
      message = message
    )
  }
  
  fluviometric_extremes_pot_complete_daily <- function(daily) {
    if (nrow(daily) == 0) {
      return(tibble::tibble())
    }
    
    date_seq <- seq.Date(
      min(daily$date, na.rm = TRUE),
      max(daily$date, na.rm = TRUE),
      by = "day"
    )
    
    tibble::tibble(date = date_seq) |>
      dplyr::left_join(
        daily |>
          dplyr::select(date, discharge_m3s, has_discharge),
        by = "date"
      ) |>
      dplyr::mutate(
        discharge_m3s = as.numeric(discharge_m3s),
        has_discharge = is.finite(discharge_m3s) & discharge_m3s > 0
      )
  }
  
  fluviometric_extremes_pot_extract <- function(daily, threshold, run_length_days = 7L) {
    if (!is.finite(threshold) || threshold <= 0 || nrow(daily) == 0) {
      return(tibble::tibble())
    }
    
    daily_complete <- fluviometric_extremes_pot_complete_daily(daily)
    
    if (nrow(daily_complete) == 0) {
      return(tibble::tibble())
    }
    
    q <- daily_complete$discharge_m3s
    q[!is.finite(q)] <- NA_real_
    names(q) <- format(daily_complete$date, "%Y-%m-%d")
    
    clusters_list <- evd::clusters(
      data = q,
      u = threshold,
      r = as.integer(run_length_days),
      cmax = FALSE,
      keep.names = TRUE,
      plot = FALSE
    )
    
    if (length(clusters_list) == 0) {
      return(tibble::tibble())
    }
    
    events <- lapply(seq_along(clusters_list), function(i) {
      cluster_values <- clusters_list[[i]]
      
      if (length(cluster_values) == 0 || all(!is.finite(cluster_values))) {
        return(NULL)
      }
      
      cluster_dates <- as.Date(names(cluster_values))
      q_values <- as.numeric(cluster_values)
      i_max <- which.max(q_values)
      
      tibble::tibble(
        event_id = i,
        date_start = min(cluster_dates, na.rm = TRUE),
        date_end = max(cluster_dates, na.rm = TRUE),
        date_peak = cluster_dates[i_max],
        year = as.integer(format(cluster_dates[i_max], "%Y")),
        q_peak_m3s = q_values[i_max],
        threshold_m3s = threshold,
        excess_m3s = q_values[i_max] - threshold,
        n_exceedance_days = length(q_values)
      )
    }) |>
      dplyr::bind_rows()
    
    if (nrow(events) == 0) {
      return(tibble::tibble())
    }
    
    events |>
      dplyr::arrange(date_peak) |>
      dplyr::mutate(event_id = dplyr::row_number()) |>
      dplyr::select(
        event_id,
        date_start,
        date_end,
        date_peak,
        year,
        q_peak_m3s,
        threshold_m3s,
        excess_m3s,
        n_exceedance_days
      )
  }

  fluviometric_extremes_pot_auto_threshold <- function(daily, progress_fun = NULL) {
    daily_complete <- fluviometric_extremes_pot_complete_daily(daily)
    
    positive_q <- daily_complete$discharge_m3s[
      is.finite(daily_complete$discharge_m3s) &
        daily_complete$discharge_m3s > 0
    ]
    
    if (length(positive_q) < 30) {
      return(tibble::tibble())
    }
    
    n_years <- as.numeric(
      max(daily_complete$date, na.rm = TRUE) -
        min(daily_complete$date, na.rm = TRUE) + 1L
    ) / 365.25
    
    lambda_grid <- seq(1, 5, by = 0.5)
    
    threshold_grid <- stats::quantile(
      positive_q,
      probs = seq(0.50, 0.995, length.out = 70),
      na.rm = TRUE,
      names = FALSE,
      type = 8
    ) |>
      unique() |>
      sort(decreasing = TRUE)
    
    if (!is.null(progress_fun)) {
      progress_fun(0.20, "Testando limiares candidatos")
    }
    
    threshold_evaluation <- lapply(seq_along(threshold_grid), function(i) {
      threshold <- threshold_grid[i]
      
      if (!is.null(progress_fun) && i %% 5L == 0L) {
        progress_fun(
          0.20 + 0.45 * i / length(threshold_grid),
          "Avaliando número de eventos por limiar"
        )
      }
      
      events <- fluviometric_extremes_pot_extract(
        daily = daily_complete,
        threshold = threshold,
        run_length_days = 7L
      )
      
      tibble::tibble(
        threshold_m3s = threshold,
        n_events = nrow(events),
        lambda_observed = nrow(events) / n_years
      )
    }) |>
      dplyr::bind_rows() |>
      dplyr::filter(n_events > 0L) |>
      dplyr::arrange(dplyr::desc(threshold_m3s))
    
    if (nrow(threshold_evaluation) == 0) {
      return(tibble::tibble())
    }
    
    if (!is.null(progress_fun)) {
      progress_fun(0.70, "Testando hipóteses POT")
    }
    
    diagnostics <- lapply(lambda_grid, function(lambda_target) {
      n_target <- ceiling(lambda_target * n_years)
      
      possible <- threshold_evaluation |>
        dplyr::filter(n_events >= n_target)
      
      if (nrow(possible) > 0) {
        selected_candidate <- possible |>
          dplyr::slice_max(threshold_m3s, n = 1, with_ties = FALSE)
      } else {
        selected_candidate <- threshold_evaluation |>
          dplyr::slice_max(n_events, n = 1, with_ties = FALSE)
      }
      
      threshold <- selected_candidate$threshold_m3s[1]
      
      events <- fluviometric_extremes_pot_extract(
        daily = daily_complete,
        threshold = threshold,
        run_length_days = 7L
      )
      
      test <- fluviometric_extremes_pot_test(
        events = events,
        daily = daily_complete,
        min_separation_days = 7L,
        alpha = 0.05
      )
      
      tibble::tibble(
        lambda_target = lambda_target,
        threshold_m3s = threshold
      ) |>
        dplyr::bind_cols(test)
    }) |>
      dplyr::bind_rows() |>
      dplyr::arrange(lambda_target) |>
      dplyr::mutate(
        reached_target_lambda = lambda_observed >= lambda_target * 0.85,
        selection_ok = selection_ok & reached_target_lambda,
        hypotheses_ok = hypotheses_ok & reached_target_lambda,
        accepted_initial_block = cumall(selection_ok)
      )
    
    diagnostics
  }
  
  fluviometric_extremes_pot_series <- reactive({
    daily <- fluviometric_extremes_pot_daily()
    threshold <- fluviometric_extremes_pot_threshold_value()
    
    if (nrow(daily) == 0 || !is.finite(threshold)) {
      return(tibble::tibble())
    }
    
    fluviometric_extremes_pot_extract(
      daily = daily,
      threshold = threshold,
      run_length_days = 7L
    )
  })
  
  fluviometric_extremes_pot_diagnostics <- reactive({
    daily <- fluviometric_extremes_pot_daily()
    events <- fluviometric_extremes_pot_series()
    
    fluviometric_extremes_pot_test(
      events = events,
      daily = daily,
      min_separation_days = 7L,
      alpha = 0.05
    )
  })
  
  observeEvent(input$fluviometric_extremes_pot_auto, {
    daily <- fluviometric_extremes_pot_daily()
    
    if (nrow(daily) == 0 || !any(daily$has_discharge, na.rm = TRUE)) {
      showNotification("Não há série diária de vazões válida para calcular o POT.", type = "warning")
      return(invisible(NULL))
    }
    
    diagnostics <- shiny::withProgress(
      message = "Calculando limiar POT automático",
      value = 0,
      {
        progress_fun <- function(value, detail) {
          shiny::setProgress(value = value, detail = detail)
        }
        
        fluviometric_extremes_pot_auto_threshold(
          daily = daily,
          progress_fun = progress_fun
        )
      }
    )
    
    fluviometric_extremes_pot_auto_table(diagnostics)
    
    if (nrow(diagnostics) == 0) {
      fluviometric_extremes_pot_threshold_value(NA_real_)
      fluviometric_extremes_pot_threshold_method("")
      showNotification("Não foi possível estimar o limiar automático.", type = "warning")
      return(invisible(NULL))
    }
    
    accepted <- diagnostics |>
      dplyr::filter(accepted_initial_block, selection_ok)
    
    if (nrow(accepted) == 0) {
      fluviometric_extremes_pot_threshold_value(NA_real_)
      fluviometric_extremes_pot_threshold_method("automático")
      
      showNotification(
        "Nenhum limiar automático da grade testada atingiu os critérios mínimos de seleção. Informe um limiar manual ou revise os critérios.",
        type = "warning",
        duration = 10
      )
      
      return(invisible(NULL))
    }
    
    selected <- accepted |>
      dplyr::slice_max(lambda_target, n = 1, with_ties = FALSE)
    
    threshold <- selected$threshold_m3s[1]
    
    fluviometric_extremes_pot_threshold_value(threshold)
    fluviometric_extremes_pot_threshold_method("automático")
    
    updateTextInput(
      session = session,
      inputId = "fluviometric_extremes_pot_threshold_manual",
      value = format(round(threshold, 3), decimal.mark = ",", big.mark = ".")
    )
    
    notification_type <- if (isTRUE(selected$hypotheses_ok[1])) {
      "message"
    } else {
      "warning"
    }
    
    showNotification(
      paste0(
        "Limiar POT automático: ",
        fluviometric_format_value(threshold),
        " m³/s; λ observado: ",
        fluviometric_format_value(selected$lambda_observed[1]),
        " eventos/ano."
      ),
      type = notification_type
    )
  })
  
  observeEvent(input$fluviometric_extremes_pot_manual, {
    threshold <- fluviometric_extremes_parse_number(input$fluviometric_extremes_pot_threshold_manual)
    
    if (!is.finite(threshold) || threshold <= 0) {
      showNotification("Informe um limiar de vazão válido.", type = "warning")
      return(invisible(NULL))
    }
    
    fluviometric_extremes_pot_threshold_value(threshold)
    fluviometric_extremes_pot_threshold_method("manual")
    fluviometric_extremes_pot_auto_table(tibble::tibble())
  })
  
  output$fluviometric_extremes_pot_status <- renderUI({
    threshold <- fluviometric_extremes_pot_threshold_value()
    method <- fluviometric_extremes_pot_threshold_method()
    
    auto_diagnostics <- fluviometric_extremes_pot_auto_table()
    
    if (!is.finite(threshold) && identical(method, "automático") && nrow(auto_diagnostics) > 0) {
      return(
        div(
          class = "table-status warning extremes-pot-status-card",
          tags$strong("Série POT"),
          tags$br(),
          "Nenhum limiar automático da grade testada atingiu os critérios mínimos de seleção.",
          tags$br(),
          "Use um limiar manual ou revise os critérios de extração."
        )
      )
    }
    
    if (!is.finite(threshold)) {
      return(
        div(
          class = "table-status empty extremes-pot-status-card",
          tags$strong("Série POT"),
          tags$br(),
          "Defina um limiar manual ou calcule o limiar automático para extrair a série POT."
        )
      )
    }
    
    diagnostics <- fluviometric_extremes_pot_diagnostics()
    
    if (nrow(diagnostics) == 0) {
      return(
        div(
          class = "table-status warning extremes-pot-status-card",
          tags$strong("Série POT"),
          tags$br(),
          "Não foi possível avaliar a série POT."
        )
      )
    }
    
    status_class <- if (isTRUE(diagnostics$hypotheses_ok[1])) {
      "table-status available"
    } else {
      "table-status warning"
    }
    
    div(
      class = paste(status_class, "extremes-pot-status-card"),
      tags$strong("Série POT"),
      tags$br(),
      paste0("Método do limiar: ", method),
      tags$br(),
      paste0("Limiar: ", fluviometric_format_value(threshold), " m³/s"),
      tags$br(),
      paste0("Eventos: ", diagnostics$n_events[1]),
      tags$br(),
      paste0("λ observado: ", fluviometric_format_value(diagnostics$lambda_observed[1]), " eventos/ano"),
      tags$br(),
      diagnostics$message[1]
    )
  })
  
  output$fluviometric_extremes_pot_plot <- renderPlot({
    pot_data <- fluviometric_extremes_pot_series()
    daily <- fluviometric_extremes_pot_daily()
    threshold <- fluviometric_extremes_pot_threshold_value()
    
    if (!is.finite(threshold)) {
      draw_empty_plot("Defina um limiar para extrair a série POT.")
      return(invisible(NULL))
    }
    
    if (nrow(pot_data) == 0) {
      draw_empty_plot("Nenhum pico independente foi encontrado acima do limiar definido.")
      return(invisible(NULL))
    }
    
    year_bands <- tibble::tibble(
      year = seq(
        min(as.integer(format(daily$date, "%Y")), na.rm = TRUE),
        max(as.integer(format(daily$date, "%Y")), na.rm = TRUE)
      )
    ) |>
      dplyr::mutate(
        xmin = as.Date(sprintf("%04d-01-01", year)),
        xmax = as.Date(sprintf("%04d-12-31", year)),
        draw_band = dplyr::row_number() %% 2L == 1L
      ) |>
      dplyr::filter(draw_band)
    
    ggplot2::ggplot(pot_data, ggplot2::aes(x = date_peak, y = q_peak_m3s)) +
      ggplot2::geom_rect(
        data = year_bands,
        ggplot2::aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
        inherit.aes = FALSE,
        fill = '#fc8d59', # "#dbeafe",
        alpha = 0.05
      ) +
      ggplot2::geom_hline(
        yintercept = threshold,
        linewidth = 0.45,
        linetype = "dashed",
        color = "grey35"
      ) +
      ggplot2::geom_segment(
        ggplot2::aes(xend = date_peak, y = threshold_m3s, yend = q_peak_m3s),
        color = "#1f77b4",
        linewidth = 0.35,
        lineend = "round"
      ) +
      ggplot2::geom_segment(
        ggplot2::aes(xend = date_peak, y = 0, yend = threshold_m3s),
        color = "#94a3b8",
        linewidth = 0.35,
        lineend = "round"
      ) +
      ggplot2::geom_point(
        shape = 21,
        fill = "#1f77b4",
        color = "grey20",
        stroke = 0.25,
        size = 2.0
      ) +
      ggplot2::scale_x_date(
        name = "Data",
        date_breaks = "5 years",
        date_labels = "%Y",
        expand = ggplot2::expansion(mult = c(0.01, 0.02))
      ) +
      ggplot2::scale_y_continuous(
        name = "POT (m³/s)",
        labels = scales::label_number(decimal.mark = ",", big.mark = "."),
        limits = c(0,NA)
        # expand = ggplot2::expansion(mult = c(0.04, 0.08))
      ) +
      preview_plot_theme(base_size = 5.5) +
      ggplot2::theme(
        legend.position = "none",
        plot.margin = ggplot2::margin(6, 8, 6, 8)
      )
  }, res = 144)
  
  output$fluviometric_extremes_pot_download <- downloadHandler(
    filename = function() {
      paste0(
        "serie_pot_vazao_",
        as.character(selected_code()),
        "_",
        format(Sys.Date(), "%Y%m%d"),
        ".csv"
      )
    },
    content = function(file) {
      pot_data <- fluviometric_extremes_pot_series()
      diagnostics <- fluviometric_extremes_pot_diagnostics()
      method <- fluviometric_extremes_pot_threshold_method()
      
      if (nrow(pot_data) == 0) {
        table_data <- tibble::tibble(
          mensagem = "Nenhum evento POT disponível."
        )
      } else {
        table_data <- pot_data |>
          dplyr::mutate(
            metodo_limiar = method,
            lambda_observado = diagnostics$lambda_observed[1],
            hipoteses_pot_atendidas = diagnostics$hypotheses_ok[1],
            mensagem_diagnostico = diagnostics$message[1]
          ) |>
          dplyr::transmute(
            evento = event_id,
            data_pico = date_peak,
            ano = year,
            q_pico_m3s = q_peak_m3s,
            limiar_m3s = threshold_m3s,
            excesso_m3s = excess_m3s,
            metodo_limiar = metodo_limiar,
            lambda_observado = lambda_observado,
            hipoteses_pot_atendidas = hipoteses_pot_atendidas,
            mensagem_diagnostico = mensagem_diagnostico
          )
      }
      
      fluviometric_stats_write_csv_bom(table_data, file, digits = 3)
    }
  )
  
  # ------------------------------------------------------------
  # Fluviometric tab: annual low flows
  # ------------------------------------------------------------
  
  fluviometric_extremes_low_flow_duration <- reactive({
    duration <- suppressWarnings(as.integer(input$fluviometric_extremes_low_flow_duration))
    
    if (!duration %in% c(1L, 3L, 7L, 15L, 30L)) {
      return(7L)
    }
    
    duration
  })
  
  fluviometric_extremes_rolling_complete_mean <- function(x, duration) {
    x <- as.numeric(x)
    valid <- is.finite(x)
    
    if (length(x) < duration) {
      return(rep(NA_real_, length(x)))
    }
    
    x0 <- ifelse(valid, x, 0)
    
    rolling_sum <- as.numeric(stats::filter(
      x0,
      rep(1, duration),
      sides = 1
    ))
    
    rolling_valid <- as.numeric(stats::filter(
      as.integer(valid),
      rep(1, duration),
      sides = 1
    ))
    
    out <- rep(NA_real_, length(x))
    complete <- is.finite(rolling_valid) & rolling_valid == duration
    
    out[complete] <- rolling_sum[complete] / duration
    
    out
  }
  
  fluviometric_extremes_low_flow_daily <- reactive({
    daily <- fluviometric_stats_daily()
    
    if (nrow(daily) == 0) {
      return(tibble::tibble())
    }
    
    daily_unique <- daily |>
      dplyr::mutate(
        date = as.Date(date),
        discharge_m3s = as.numeric(discharge_m3s)
      ) |>
      dplyr::filter(!is.na(date)) |>
      dplyr::arrange(date) |>
      dplyr::group_by(date) |>
      dplyr::summarise(
        discharge_m3s = dplyr::first(discharge_m3s[is.finite(discharge_m3s)]),
        .groups = "drop"
      )
    
    if (nrow(daily_unique) == 0) {
      return(tibble::tibble())
    }
    
    date_seq <- seq.Date(
      min(daily_unique$date, na.rm = TRUE),
      max(daily_unique$date, na.rm = TRUE),
      by = "day"
    )
    
    tibble::tibble(date = date_seq) |>
      dplyr::left_join(daily_unique, by = "date") |>
      dplyr::mutate(
        discharge_m3s = as.numeric(discharge_m3s),
        has_discharge = is.finite(discharge_m3s),
        year = as.integer(format(date, "%Y"))
      )
  })
  
  fluviometric_extremes_low_flow_annual <- reactive({
    daily <- fluviometric_extremes_low_flow_daily()
    duration <- fluviometric_extremes_low_flow_duration()
    
    if (nrow(daily) == 0 || !any(daily$has_discharge, na.rm = TRUE)) {
      return(tibble::tibble())
    }
    
    annual_base <- daily |>
      dplyr::group_by(year) |>
      dplyr::summarise(
        period_start = as.Date(sprintf("%04d-01-01", dplyr::first(year))),
        period_end = as.Date(sprintf("%04d-12-31", dplyr::first(year))),
        n_days_expected = as.integer(dplyr::first(period_end) - dplyr::first(period_start) + 1L),
        n_days_valid = sum(has_discharge, na.rm = TRUE),
        n_days_missing = n_days_expected - n_days_valid,
        missing_fraction = n_days_missing / n_days_expected,
        .groups = "drop"
      ) |>
      dplyr::arrange(year)
    
    annual_minima <- lapply(split(daily, daily$year), function(df) {
      df <- df |>
        dplyr::arrange(date)
      
      year_value <- df$year[1]
      n_windows_expected <- max(nrow(df) - duration + 1L, 0L)
      
      rolling_q <- fluviometric_extremes_rolling_complete_mean(
        df$discharge_m3s,
        duration
      )
      
      valid_windows <- is.finite(rolling_q)
      n_windows_valid <- sum(valid_windows, na.rm = TRUE)
      
      if (n_windows_valid == 0) {
        return(tibble::tibble(
          year = year_value,
          duration_days = duration,
          n_windows_expected = n_windows_expected,
          n_windows_valid = 0L,
          windows_valid_pct = 0,
          q_min_nday_m3s = NA_real_,
          date_start = as.Date(NA),
          date_end = as.Date(NA),
          date_center = as.Date(NA),
          n_windows_equal_to_min = NA_integer_,
          dates_equal_to_min = ""
        ))
      }
      
      q_min <- min(rolling_q, na.rm = TRUE)
      min_indexes <- which(is.finite(rolling_q) & dplyr::near(rolling_q, q_min))
      first_min_index <- min_indexes[1]
      
      date_end <- df$date[first_min_index]
      date_start <- date_end - duration + 1L
      date_center <- date_start + floor((duration - 1L) / 2L)
      
      tibble::tibble(
        year = year_value,
        duration_days = duration,
        n_windows_expected = n_windows_expected,
        n_windows_valid = n_windows_valid,
        windows_valid_pct = 100 * n_windows_valid / n_windows_expected,
        q_min_nday_m3s = q_min,
        date_start = date_start,
        date_end = date_end,
        date_center = date_center,
        n_windows_equal_to_min = length(min_indexes),
        dates_equal_to_min = paste(format(df$date[min_indexes], "%Y-%m-%d"), collapse = "; ")
      )
    }) |>
      dplyr::bind_rows()
    
    low_flow <- annual_base |>
      dplyr::left_join(annual_minima, by = "year")
    
    date_lookup <- daily |>
      dplyr::select(date, has_discharge)
    
    series_min_date <- min(daily$date, na.rm = TRUE)
    series_max_date <- max(daily$date, na.rm = TRUE)
    
    month_names_pt <- c(
      "janeiro", "fevereiro", "março", "abril", "maio", "junho",
      "julho", "agosto", "setembro", "outubro", "novembro", "dezembro"
    )
    
    month_failures <- lapply(seq_len(nrow(low_flow)), function(i) {
      date_center <- low_flow$date_center[i]
      
      if (is.na(date_center)) {
        return(tibble::tibble(
          year = low_flow$year[i],
          month_minimum = NA_integer_,
          month_minimum_label = "",
          failures_pct_month_minimum = NA_real_
        ))
      }
      
      month_minimum <- as.integer(format(date_center, "%m"))
      month_start <- as.Date(sprintf(
        "%04d-%02d-01",
        as.integer(format(date_center, "%Y")),
        month_minimum
      ))
      month_end <- seq.Date(month_start, by = "month", length.out = 2L)[2] - 1L
      month_dates <- seq.Date(month_start, month_end, by = "day")
      
      match_index <- match(month_dates, date_lookup$date)
      has_month <- date_lookup$has_discharge[match_index]
      has_month[is.na(has_month)] <- FALSE
      
      tibble::tibble(
        year = low_flow$year[i],
        month_minimum = month_minimum,
        month_minimum_label = month_names_pt[month_minimum],
        failures_pct_month_minimum = 100 * sum(!has_month) / length(month_dates)
      )
    }) |>
      dplyr::bind_rows()
    
    gap_window <- lapply(seq_len(nrow(low_flow)), function(i) {
      date_start <- low_flow$date_start[i]
      date_end <- low_flow$date_end[i]
      
      if (is.na(date_start) || is.na(date_end)) {
        return(tibble::tibble(
          year = low_flow$year[i],
          n_missing_near_minimum = NA_integer_,
          flag_gap_near_minimum = FALSE,
          flag_window_truncated_minimum = FALSE
        ))
      }
      
      window_dates <- seq.Date(date_start - 2L, date_end + 2L, by = "day")
      match_index <- match(window_dates, date_lookup$date)
      has_window <- date_lookup$has_discharge[match_index]
      has_window[is.na(has_window)] <- FALSE
      
      tibble::tibble(
        year = low_flow$year[i],
        n_missing_near_minimum = sum(!has_window),
        flag_gap_near_minimum = any(!has_window),
        flag_window_truncated_minimum = any(window_dates < series_min_date | window_dates > series_max_date)
      )
    }) |>
      dplyr::bind_rows()
    
    low_flow <- low_flow |>
      dplyr::left_join(month_failures, by = "year") |>
      dplyr::left_join(gap_window, by = "year")
    
    consistency_issues <- fluviometric_consistency_issue_details()
    
    if (!is.null(consistency_issues) && nrow(consistency_issues) > 0) {
      issue_data <- consistency_issues |>
        dplyr::mutate(data = as.Date(data)) |>
        dplyr::filter(!is.na(data))
      
      consistency_window <- lapply(seq_len(nrow(low_flow)), function(i) {
        date_start <- low_flow$date_start[i]
        date_end <- low_flow$date_end[i]
        
        if (is.na(date_start) || is.na(date_end) || nrow(issue_data) == 0) {
          return(tibble::tibble(
            year = low_flow$year[i],
            n_consistency_issues_on_min_window = 0L,
            consistency_issue_types_on_min_window = ""
          ))
        }
        
        issues <- issue_data |>
          dplyr::filter(data >= date_start, data <= date_end)
        
        tibble::tibble(
          year = low_flow$year[i],
          n_consistency_issues_on_min_window = nrow(issues),
          consistency_issue_types_on_min_window = if (nrow(issues) > 0) {
            paste(sort(unique(issues$ocorrencia)), collapse = "; ")
          } else {
            ""
          }
        )
      }) |>
        dplyr::bind_rows()
    } else {
      consistency_window <- tibble::tibble(
        year = low_flow$year,
        n_consistency_issues_on_min_window = 0L,
        consistency_issue_types_on_min_window = ""
      )
    }
    
    low_flow <- low_flow |>
      dplyr::left_join(consistency_window, by = "year") |>
      dplyr::mutate(
        flag_consistency_issue_on_min_window = n_consistency_issues_on_min_window > 0L
      )
    
    valid_minima <- low_flow |>
      dplyr::filter(is.finite(q_min_nday_m3s))
    
    n_valid_minima <- nrow(valid_minima)
    
    if (n_valid_minima > 0) {
      low_flow <- low_flow |>
        dplyr::mutate(
          rank_ascending = dplyr::if_else(
            is.finite(q_min_nday_m3s),
            as.integer(rank(q_min_nday_m3s, ties.method = "min", na.last = "keep")),
            NA_integer_
          ),
          rank_descending = dplyr::if_else(
            is.finite(q_min_nday_m3s),
            as.integer(rank(-q_min_nday_m3s, ties.method = "min", na.last = "keep")),
            NA_integer_
          ),
          flag_candidate_exclusion = is.finite(q_min_nday_m3s) &
            !is.na(rank_descending) &
            rank_descending <= 0.40 * n_valid_minima &
            (
              missing_fraction >= 1 / 3 |
                windows_valid_pct < 66.7
            )
        )
    } else {
      low_flow <- low_flow |>
        dplyr::mutate(
          rank_ascending = NA_integer_,
          rank_descending = NA_integer_,
          flag_candidate_exclusion = FALSE
        )
    }
    
    positive_minima <- low_flow$q_min_nday_m3s[
      is.finite(low_flow$q_min_nday_m3s) &
        low_flow$q_min_nday_m3s > 0
    ]
    
    if (length(positive_minima) >= 4) {
      log_minima <- log10(positive_minima)
      q1 <- as.numeric(stats::quantile(log_minima, 0.25, na.rm = TRUE, names = FALSE))
      q3 <- as.numeric(stats::quantile(log_minima, 0.75, na.rm = TRUE, names = FALSE))
      iqr <- q3 - q1
      
      if (is.finite(iqr) && iqr > 0) {
        lower_attention <- q1 - 1.5 * iqr
        lower_strong <- q1 - 3.0 * iqr
        upper_attention <- q3 + 1.5 * iqr
        upper_strong <- q3 + 3.0 * iqr
        
        low_flow <- low_flow |>
          dplyr::mutate(
            log_q_min = dplyr::if_else(q_min_nday_m3s > 0, log10(q_min_nday_m3s), NA_real_),
            high_outlier_iqr_class = dplyr::case_when(
              is.finite(log_q_min) & log_q_min > upper_strong ~ "forte",
              is.finite(log_q_min) & log_q_min > upper_attention ~ "atenção",
              TRUE ~ "sem sinal"
            ),
            low_outlier_iqr_class = dplyr::case_when(
              is.finite(log_q_min) & log_q_min < lower_strong ~ "forte",
              is.finite(log_q_min) & log_q_min < lower_attention ~ "atenção",
              TRUE ~ "sem sinal"
            ),
            flag_high_outlier_iqr = high_outlier_iqr_class != "sem sinal",
            flag_low_outlier_iqr = low_outlier_iqr_class != "sem sinal"
          )
      } else {
        low_flow <- low_flow |>
          dplyr::mutate(
            log_q_min = dplyr::if_else(q_min_nday_m3s > 0, log10(q_min_nday_m3s), NA_real_),
            high_outlier_iqr_class = "sem sinal",
            low_outlier_iqr_class = "sem sinal",
            flag_high_outlier_iqr = FALSE,
            flag_low_outlier_iqr = FALSE
          )
      }
    } else {
      low_flow <- low_flow |>
        dplyr::mutate(
          log_q_min = dplyr::if_else(q_min_nday_m3s > 0, log10(q_min_nday_m3s), NA_real_),
          high_outlier_iqr_class = "sem sinal",
          low_outlier_iqr_class = "sem sinal",
          flag_high_outlier_iqr = FALSE,
          flag_low_outlier_iqr = FALSE
        )
    }
    
    low_flow <- low_flow |>
      dplyr::mutate(
        flag_few_valid_windows = is.finite(windows_valid_pct) & windows_valid_pct < 80,
        flag_failures_month_minimum = is.finite(failures_pct_month_minimum) &
          failures_pct_month_minimum > 0,
        flag_zero_or_negative_minimum = is.finite(q_min_nday_m3s) &
          q_min_nday_m3s <= 0
      )
    
    flag_labels <- c(
      flag_few_valid_windows = "poucas janelas válidas",
      flag_failures_month_minimum = "falhas no mês da mínima",
      flag_gap_near_minimum = "falhas no entorno da mínima",
      flag_high_outlier_iqr = "outlier alto",
      flag_low_outlier_iqr = "outlier baixo",
      flag_candidate_exclusion = "candidato à exclusão",
      flag_consistency_issue_on_min_window = "ocorrência na consistência",
      flag_zero_or_negative_minimum = "vazão nula/negativa"
    )
    
    flag_columns <- names(flag_labels)
    
    for (column in flag_columns) {
      if (!column %in% names(low_flow)) {
        low_flow[[column]] <- FALSE
      }
      low_flow[[column]] <- as.logical(low_flow[[column]])
      low_flow[[column]][is.na(low_flow[[column]])] <- FALSE
    }
    
    flag_matrix <- low_flow[, flag_columns, drop = FALSE]
    low_flow$n_flags <- rowSums(as.data.frame(flag_matrix))
    
    low_flow$flags_resumo <- apply(flag_matrix, 1, function(row_flags) {
      active <- flag_labels[as.logical(row_flags)]
      if (length(active) == 0) {
        return("sem flag")
      }
      paste(active, collapse = "; ")
    })
    
    low_flow |>
      dplyr::mutate(
        attention_score = dplyr::case_when(
          n_flags == 0L ~ 0L,
          flag_candidate_exclusion | flag_consistency_issue_on_min_window ~ pmax(n_flags, 2L),
          TRUE ~ n_flags
        ),
        nivel_atencao = dplyr::case_when(
          attention_score == 0L ~ "sem flag",
          attention_score == 1L ~ "revisar",
          attention_score == 2L ~ "atenção moderada",
          TRUE ~ "atenção alta"
        ),
        n_flags_class = dplyr::case_when(
          n_flags == 0L ~ "0",
          n_flags == 1L ~ "1",
          n_flags == 2L ~ "2",
          TRUE ~ "3+"
        ),
        n_flags_class = factor(n_flags_class, levels = c("0", "1", "2", "3+"))
      ) |>
      dplyr::arrange(year)
  })
  
  output$fluviometric_extremes_low_flow_summary_cards <- renderUI({
    low_flow <- fluviometric_extremes_low_flow_annual()
    duration <- fluviometric_extremes_low_flow_duration()
    
    if (nrow(low_flow) == 0 || !any(is.finite(low_flow$q_min_nday_m3s))) {
      return(NULL)
    }
    
    valid_low_flow <- low_flow |>
      dplyr::filter(is.finite(q_min_nday_m3s))
    
    record_min <- valid_low_flow |>
      dplyr::slice_min(q_min_nday_m3s, n = 1, with_ties = FALSE)
    
    tags$div(
      class = "section-card",
      tags$div(
        class = "section-header",
        tags$h3("Resumo das mínimas anuais"),
        tags$p("Síntese da extração por ano civil e dos flags de triagem.")
      ),
      tags$div(
        class = "overview-metric-grid fluviometric-metric-grid",
        overview_metric("Duração", paste0(duration, " dias")),
        overview_metric("Anos válidos", fluviometric_format_count(nrow(valid_low_flow))),
        overview_metric("Anos com flags", fluviometric_format_count(sum(valid_low_flow$n_flags > 0L, na.rm = TRUE))),
        overview_metric("Candidatos à exclusão", fluviometric_format_count(sum(valid_low_flow$flag_candidate_exclusion, na.rm = TRUE))),
        overview_metric("Falhas no mês da mínima", fluviometric_format_count(sum(valid_low_flow$flag_failures_month_minimum, na.rm = TRUE))),
        overview_metric(
          "Menor mínima observada",
          paste0(
            record_min$year[1],
            " — ",
            fluviometric_format_value(record_min$q_min_nday_m3s[1]),
            " m³/s"
          )
        )
      )
    )
  })
  
  output$fluviometric_extremes_low_flow_plot <- renderPlot({
    plot_data <- fluviometric_extremes_low_flow_annual() |>
      dplyr::filter(is.finite(q_min_nday_m3s))
    
    duration <- fluviometric_extremes_low_flow_duration()
    
    if (nrow(plot_data) == 0) {
      draw_empty_plot("Sem dados suficientes para extrair mínimas anuais.")
      return(invisible(NULL))
    }
    
    flag_palette <- fluviometric_extremes_flag_palette()
    
    ggplot2::ggplot(
      plot_data,
      ggplot2::aes(x = year, y = q_min_nday_m3s)
    ) +
      ggplot2::geom_segment(
        ggplot2::aes(xend = year, y = 0, yend = q_min_nday_m3s),
        color = "#94a3b8",
        linewidth = 0.35,
        lineend = "round"
      ) +
      ggplot2::geom_point(
        ggplot2::aes(fill = n_flags_class),
        shape = 21,
        color = "grey20",
        stroke = 0.25,
        size = 2.4,
        show.legend = TRUE
      ) +
      ggplot2::scale_fill_manual(
        name = "Nº de flags",
        values = flag_palette,
        limits = c("0", "1", "2", "3+"),
        breaks = c("0", "1", "2", "3+"),
        labels = c("0", "1", "2", "3+"),
        drop = FALSE,
        na.translate = FALSE
      ) +
      ggplot2::guides(
        fill = ggplot2::guide_legend(
          nrow = 1,
          override.aes = list(
            shape = 21,
            fill = unname(flag_palette[c("0", "1", "2", "3+")]),
            color = "grey20",
            size = 2.6,
            stroke = 0.25
          )
        )
      ) +
      ggplot2::scale_x_continuous(
        name = "Ano civil",
        breaks = scales::pretty_breaks(n = 8)
      ) +
      ggplot2::scale_y_continuous(
        name = paste0("Mínima anual de ", duration, " dias (m³/s)"),
        labels = scales::label_number(decimal.mark = ",", big.mark = "."),
        expand = ggplot2::expansion(mult = c(0.02, 0.08))
      ) +
      preview_plot_theme(base_size = 5.5) +
      ggplot2::theme(
        legend.position = "bottom",
        plot.margin = ggplot2::margin(6, 8, 6, 8)
      )
  }, res = 144)
  
  output$fluviometric_extremes_low_flow_table <- DT::renderDT({
    low_flow <- fluviometric_extremes_low_flow_annual()
    
    if (nrow(low_flow) == 0) {
      return(DT::datatable(
        tibble::tibble(mensagem = "Nenhuma mínima anual disponível."),
        rownames = FALSE,
        options = list(dom = "t")
      ))
    }
    
    table_data <- low_flow |>
      dplyr::mutate(
        outlier_label = dplyr::case_when(
          flag_high_outlier_iqr ~ "alto",
          flag_low_outlier_iqr ~ "baixo",
          TRUE ~ ""
        )
      ) |>
      dplyr::transmute(
        `Ano` = year,
        `Data inicial` = format(date_start, "%Y-%m-%d"),
        `Data final` = format(date_end, "%Y-%m-%d"),
        `Q mínima (m³/s)` = round(q_min_nday_m3s, 3),
        `Mês da mínima` = month_minimum_label,
        `Falhas no ano (%)` = round(100 * missing_fraction, 1),
        `Falhas no mês (%)` = round(failures_pct_month_minimum, 1),
        `Janelas válidas (%)` = round(windows_valid_pct, 1),
        `Rank` = rank_ascending,
        `Falhas no entorno` = fluviometric_extremes_yes_blank(flag_gap_near_minimum),
        `Outlier` = outlier_label,
        `Candidato à exclusão` = fluviometric_extremes_yes_blank(flag_candidate_exclusion),
        `Ocorrência na consistência` = fluviometric_extremes_yes_blank(flag_consistency_issue_on_min_window),
        `Vazão nula/negativa` = fluviometric_extremes_yes_blank(flag_zero_or_negative_minimum),
        `Nº flags` = n_flags,
        `Nível de atenção` = nivel_atencao,
        `Flags` = flags_resumo
      )
    
    DT::datatable(
      table_data,
      rownames = FALSE,
      filter = "top",
      options = list(
        pageLength = 12,
        scrollX = TRUE,
        order = list(list(0, "asc")),
        language = list(url = "//cdn.datatables.net/plug-ins/1.13.6/i18n/pt-BR.json")
      )
    )
  })
  
  output$fluviometric_extremes_low_flow_download <- downloadHandler(
    filename = function() {
      paste0(
        "vazoes_minimas_",
        fluviometric_extremes_low_flow_duration(),
        "d_",
        as.character(selected_code()),
        "_",
        format(Sys.Date(), "%Y%m%d"),
        ".csv"
      )
    },
    content = function(file) {
      low_flow <- fluviometric_extremes_low_flow_annual()
      
      if (nrow(low_flow) == 0) {
        table_data <- tibble::tibble(
          mensagem = "Nenhuma mínima anual disponível."
        )
      } else {
        table_data <- low_flow |>
          dplyr::mutate(
            outlier_label = dplyr::case_when(
              flag_high_outlier_iqr ~ "alto",
              flag_low_outlier_iqr ~ "baixo",
              TRUE ~ ""
            )
          ) |>
          dplyr::transmute(
            ano = year,
            duracao_dias = duration_days,
            data_inicial = date_start,
            data_final = date_end,
            data_central = date_center,
            q_minima_m3s = q_min_nday_m3s,
            mes_minima = month_minimum_label,
            falhas_ano_pct = 100 * missing_fraction,
            falhas_mes_minima_pct = failures_pct_month_minimum,
            janelas_possiveis = n_windows_expected,
            janelas_validas = n_windows_valid,
            janelas_validas_pct = windows_valid_pct,
            rank = rank_ascending,
            n_falhas_entorno_minima = n_missing_near_minimum,
            falhas_no_entorno = flag_gap_near_minimum,
            outlier = outlier_label,
            candidato_a_exclusao = flag_candidate_exclusion,
            ocorrencia_na_consistencia = flag_consistency_issue_on_min_window,
            tipos_ocorrencia_consistencia = consistency_issue_types_on_min_window,
            vazao_nula_ou_negativa = flag_zero_or_negative_minimum,
            n_flags = n_flags,
            nivel_atencao = nivel_atencao,
            flags_resumo = flags_resumo
          )
      }
      
      fluviometric_stats_write_csv_bom(table_data, file, digits = 3)
    }
  )
  

