# ============================================================
# server_07_station_outputs.R
# Purpose: Diagnostic/rating plots, tables, cross-section outputs, metadata, and final downloads.
# ============================================================
# BEGIN ORIGINAL BODY
  output$diagnostic_overview_cards <- renderUI({
    diagnostics <- selected_diagnostics()
    summary <- extract_diagnostic_table(diagnostics, c("summary", "diagnostic_summary", "station_summary"))
    
    translate_diagnostic_card_value <- function(x) {
      if (length(x) == 0 || is.null(x) || is.na(x) || identical(as.character(x), "")) {
        return("não disponível")
      }
      
      x_chr <- as.character(x[[1]])
      
      value_map <- c(
        "not_available" = "não disponível",
        "none" = "nenhum",
        "very_low" = "muito baixo",
        "low" = "baixo",
        "moderate" = "moderado",
        "high" = "alto",
        "very_high" = "muito alto",
        "low_coverage" = "baixa cobertura",
        "moderate_coverage" = "cobertura moderada",
        "high_coverage" = "alta cobertura",
        "no_evidence" = "sem evidência",
        "weak_evidence" = "evidência fraca",
        "moderate_evidence" = "evidência moderada",
        "strong_evidence" = "evidência forte",
        "low_attention" = "atenção baixa",
        "moderate_attention" = "atenção moderada",
        "high_attention" = "atenção alta"
      )
      
      mapped <- unname(value_map[x_chr])
      
      if (length(mapped) == 1 && !is.na(mapped) && mapped != "") {
        return(mapped)
      }
      
      x_chr
    }
    
    first_translated_value <- function(table, candidates) {
      value <- first_metric_value(table, candidates)
      translate_diagnostic_card_value(value)
    }
    
    triage_class <- first_translated_value(
      summary,
      c(
        "diagnostic_attention_class_label_pt",
        "diagnostic_attention_class",
        "attention_class"
      )
    )
    
    triage_score <- value_as_text(
      first_metric_value(summary, c("diagnostic_attention_score", "attention_score", "quality_index")),
      "diagnostic_attention_score"
    )
    
    rating_match <- value_as_text(
      first_metric_value(summary, c("rating_match_fraction")),
      "frac_rating_match"
    )
    
    residual_median <- value_as_text(
      first_metric_value(summary, c("median_abs_rating_log_residual")),
      "median_abs_rating_log_residual"
    )
    
    temporal_regimes <- value_as_text(
      first_metric_value(summary, c("n_temporal_regimes")),
      "n_temporal_regimes"
    )
    
    temporal_evidence <- first_translated_value(
      summary,
      c(
        "temporal_regime_evidence_class_label_pt",
        "temporal_regime_evidence_class"
      )
    )
    
    tags$div(
      class = "diagnostic-card-grid",
      metric_card("Triagem", triage_class, paste("Escore:", triage_score), "primary"),
      metric_card("Pareamento curva-chave", rating_match, "Fração de medições válidas pareadas"),
      metric_card("Resíduo típico", residual_median, "Mediana do resíduo log absoluto"),
      metric_card("Regimes temporais", temporal_regimes, paste("Evidência:", temporal_evidence))
    )
  })
  
  output$discharge_measurements_by_year_plot <- renderPlot({
    tryCatch({
      m <- measurement_plot_data()
      
      if (nrow(m) == 0 || !any(!is.na(m$measurement_year))) {
        draw_empty_plot("Não há medições datadas para este gráfico.")
        return(invisible(NULL))
      }
      
      by_year <- m %>%
        dplyr::filter(!is.na(measurement_year)) %>%
        dplyr::count(measurement_year, name = "n_measurements")
      
      print(
        ggplot(by_year, aes(x = measurement_year, y = n_measurements)) +
          geom_col(width = 0.85, fill="gray98", color = "black", linewidth = 0.2) +
          labs(
            # title = "Medições de descarga por ano",
            x = "Ano",
            y = "Número de medições"
          ) +
          preview_plot_theme(base_size = 6)
      )
    }, error = function(e) {
      draw_empty_plot(paste("Gráfico indisponível:", clean_error_message(conditionMessage(e))))
    })
  }, res = 120)
  
  output$diagnostic_flags_stage_discharge_plot <- renderPlot({
    tryCatch({
      m <- measurement_plot_data()
      
      if (nrow(m) == 0 || !any(!is.na(m$stage_cm)) || !any(!is.na(m$discharge_m3s))) {
        draw_empty_plot("Não há medições de cota e vazão para este gráfico.")
        return(invisible(NULL))
      }
      
      flag_data <- m %>%
        dplyr::filter(!is.na(stage_cm), !is.na(discharge_m3s)) %>%
        dplyr::mutate(
          zero_or_negative_flag = dplyr::coalesce(stage_zero_or_negative_flag, FALSE) |
            dplyr::coalesce(discharge_zero_or_negative_flag, FALSE),
          repeated_value_flag = dplyr::coalesce(repeated_stage_variable_discharge_flag, FALSE) |
            dplyr::coalesce(repeated_discharge_variable_stage_flag, FALSE),
          diagnostic_flag = dplyr::case_when(
            zero_or_negative_flag ~ "Cota ou vazão \u2264 0",
            repeated_value_flag ~ "Valor repetido em atenção",
            TRUE ~ "Sem sinal destacado"
          ),
          diagnostic_flag = factor(
            diagnostic_flag,
            levels = c("Sem sinal destacado", "Valor repetido em atenção", "Cota ou vazão \u2264 0")
          )
        )
      
      if (nrow(flag_data) == 0) {
        draw_empty_plot("Não há medições de cota e vazão para este gráfico.")
        return(invisible(NULL))
      }
      
      flag_palette <- c(
        "Valor repetido em atenção" = "#fc8d62",
        "Cota ou vazão ≤ 0" = "#e41a1c"
      )
      
      legend_key_data <- tibble::tibble(
        discharge_m3s = min(flag_data$discharge_m3s, na.rm = TRUE),
        stage_cm = min(flag_data$stage_cm, na.rm = TRUE),
        diagnostic_flag = factor(names(flag_palette), levels = levels(flag_data$diagnostic_flag))
      )
      
      print(
        ggplot(flag_data, aes(x = discharge_m3s, y = stage_cm)) +
          geom_point(
            data = flag_data %>% dplyr::filter(diagnostic_flag == "Sem sinal destacado"),
            color = "grey15",
            alpha = 0.6,
            size = 1.6
          ) +
          geom_point(
            data = legend_key_data,
            aes(x = discharge_m3s, y = stage_cm, color = diagnostic_flag),
            alpha = 0,
            size = 1.8,
            show.legend = TRUE
          ) +
          geom_point(
            data = flag_data %>% dplyr::filter(diagnostic_flag != "Sem sinal destacado"),
            aes(color = diagnostic_flag),
            alpha = 0.68,
            size = 1.8
          ) +
          scale_color_manual(
            name = NULL,
            values = flag_palette,
            limits = names(flag_palette),
            drop = FALSE
          ) +
          guides(color = guide_legend(override.aes = list(alpha = 0.68, size = 2))) +
          labs(
            x = expression(bold("Vazão (" * m^3 * "/s)")),
            y = "Cota (cm)",
            # color = "Sinal"
          ) +
          preview_plot_theme()
      )
    }, error = function(e) {
      draw_empty_plot(paste("Gráfico indisponível:", clean_error_message(conditionMessage(e))))
    })
  })
  
  output$arh23_stage_measurements_plot <- renderPlot({
    tryCatch({
      m <- measurement_plot_data()
      
      if (nrow(m) == 0 || !any(!is.na(m$area_rh_two_thirds)) || !any(!is.na(m$stage_cm))) {
        draw_empty_plot("Não há campos hidráulicos suficientes para este gráfico.")
        return(invisible(NULL))
      }
      
      plot_data <- m %>%
        dplyr::filter(
          !is.na(area_rh_two_thirds),
          !is.na(stage_cm),
          is.finite(area_rh_two_thirds),
          area_rh_two_thirds > 0
        )
      
      if (nrow(plot_data) == 0) {
        draw_empty_plot("Não há campos hidráulicos suficientes para este gráfico.")
        return(invisible(NULL))
      }
      
      print(
        ggplot(plot_data, aes(x = area_rh_two_thirds, y = stage_cm)) +
          geom_point(alpha = 0.6, size = 1.6,  fill="gray55", color = "gray15") +
          labs(
            # title = expression("Medições de descarga: " * A %.% R[h]^{2/3} * " × cota"),
            x = expression(bold(A %.% R[h]^{2/3})),
            y = "Cota (cm)",
            # caption = "Rh é aproximado a partir de largura, profundidade média e área molhada,\n o perímetro molhado não está disponível na exportação atual."
          ) +
          preview_plot_theme(base_size = 6)
      )
    }, error = function(e) {
      draw_empty_plot(paste("Gráfico indisponível:", clean_error_message(conditionMessage(e))))
    })
  }, res = 120)
  
  output$mean_velocity_stage_measurements_plot <- renderPlot({
    tryCatch({
      m <- measurement_plot_data()
      
      if (nrow(m) == 0 || !any(!is.na(m$mean_velocity_ms)) || !any(!is.na(m$stage_cm))) {
        draw_empty_plot("Não há campos de velocidade média e cota para este gráfico.")
        return(invisible(NULL))
      }
      
      plot_data <- m %>%
        dplyr::filter(!is.na(mean_velocity_ms), !is.na(stage_cm))
      
      if (nrow(plot_data) == 0) {
        draw_empty_plot("Não há campos de velocidade média e cota para este gráfico.")
        return(invisible(NULL))
      }
      
      print(
        ggplot(plot_data, aes(x = mean_velocity_ms, y = stage_cm)) +
          geom_point(alpha = 0.6, size = 1.6,  fill="gray55", colour = "gray15") +
          labs(
            # title = "Medições de descarga: velocidade média × cota",
            x = "Velocidade média (m/s)",
            y = "Cota (cm)"
          ) +
          preview_plot_theme(base_size = 6)
      )
    }, error = function(e) {
      draw_empty_plot(paste("Gráfico indisponível:", clean_error_message(conditionMessage(e))))
    })
  }, res = 120)
  
  make_app_plot_palette <- function(labels, palette = "Dark 3") {
    labels <- as.character(labels)
    labels <- unique(labels[!is.na(labels) & nzchar(labels)])
    if (length(labels) == 0) {
      return(character(0))
    }
    colors <- grDevices::hcl.colors(length(labels), palette = palette)
    names(colors) <- labels
    colors
  }
  
  residual_axis_limits <- function(x, probs = c(0.01, 0.99), min_half_range = 0.20) {
    x <- x[!is.na(x) & is.finite(x)]
    
    if (length(x) < 5) {
      return(NULL)
    }
    
    lim <- as.numeric(stats::quantile(x, probs = probs, na.rm = TRUE, names = FALSE))
    
    if (any(!is.finite(lim)) || lim[1] == lim[2]) {
      return(NULL)
    }
    
    lim[1] <- min(lim[1], 0)
    lim[2] <- max(lim[2], 0)
    
    center <- mean(lim)
    half_range <- max((lim[2] - lim[1]) / 2, min_half_range)
    
    c(center - half_range, center + half_range)
  }
  
  extract_break_dates <- function(temporal_regime) {
    if (is.null(temporal_regime) ||
        is.null(temporal_regime$model_scores) ||
        nrow(temporal_regime$model_scores) == 0 ||
        !"break_dates" %in% names(temporal_regime$model_scores) ||
        is.na(temporal_regime$model_scores$break_dates[1])) {
      return(as.Date(character(0)))
    }
    
    as.Date(strsplit(temporal_regime$model_scores$break_dates[1], ";", fixed = TRUE)[[1]])
  }
  
  prepare_curve_palette <- function(diagnostics, curve_points = NULL) {
    curve_metadata <- extract_diagnostic_table(diagnostics, c("curve_metadata")) %>%
      filter_rating_curve_table_for_display()
    
    labels <- character(0)
    
    if (!is.null(curve_points) && nrow(curve_points) > 0 && "curve_label" %in% names(curve_points)) {
      labels <- curve_points$curve_label
    } else if (nrow(curve_metadata) > 0 && "curve_label" %in% names(curve_metadata)) {
      labels <- curve_metadata$curve_label
    }
    
    make_app_plot_palette(labels)
  }
  
  prepare_rating_curve_points_for_plot <- function(diagnostics) {
    curve_points <- extract_diagnostic_table(diagnostics, c("rating_curve_points")) 
    
    if (nrow(curve_points) == 0 ||
        !all(c("stage_cm", "discharge_m3s") %in% names(curve_points))) {
      return(tibble::tibble())
    }
    
    if (!"curve_label" %in% names(curve_points)) {
      curve_points$curve_label <- "Curva-chave"
    }
    if (!"curve_segment_label" %in% names(curve_points)) {
      curve_points$curve_segment_label <- as.character(curve_points$curve_label)
    }
    
    curve_points %>%
      dplyr::mutate(
        stage_cm = as_numeric_app(stage_cm),
        discharge_m3s = as_numeric_app(discharge_m3s),
        curve_label = as.character(curve_label),
        curve_segment_label = as.character(curve_segment_label)
      ) %>%
      dplyr::filter(is.finite(stage_cm), is.finite(discharge_m3s))
  }
  
  prepare_measurements_for_rating_plot <- function() {
    selected_measurements() %>%
      dplyr::mutate(
        stage_cm_plot = as_numeric_app(stage_cm_app),
        discharge_m3s_plot = as_numeric_app(discharge_m3s_app)
      ) %>%
      dplyr::filter(is.finite(stage_cm_plot), is.finite(discharge_m3s_plot))
  }
  
  prepare_measurements_for_selected_rating_curve_plot <- function() {
    selected_measurements_for_rating_curve_plot() %>%
      dplyr::mutate(
        stage_cm_plot = as_numeric_app(stage_cm_app),
        discharge_m3s_plot = as_numeric_app(discharge_m3s_app)
      ) %>%
      dplyr::filter(is.finite(stage_cm_plot), is.finite(discharge_m3s_plot))
  }
  
  output$rating_curves_and_measurements_plot <- renderPlot({
    tryCatch({
      diagnostics <- selected_diagnostics()
      
      all_curve_points <- prepare_rating_curve_points_for_plot(diagnostics)
      all_best_match <- extract_diagnostic_table(diagnostics, c("best_rating_match"))
      
      curve_points <- all_curve_points %>%
        filter_rating_curve_table_for_display()
      
      measurements <- prepare_measurements_for_selected_rating_curve_plot()
      
      best_match <- all_best_match %>%
        filter_rating_curve_table_for_display()
      
      if (nrow(curve_points) == 0 && nrow(measurements) == 0) {
        draw_empty_plot("Não há pontos suficientes para desenhar curvas-chave ou medições.")
        return(invisible(NULL))
      }
      
      curve_palette_labels <- character(0)
      
      if (nrow(all_curve_points) > 0 && "curve_label" %in% names(all_curve_points)) {
        curve_palette_labels <- c(curve_palette_labels, all_curve_points$curve_label)
      }
      
      if (nrow(all_best_match) > 0 && "curve_label" %in% names(all_best_match)) {
        curve_palette_labels <- c(curve_palette_labels, all_best_match$curve_label)
      }
      
      curve_palette <- make_app_plot_palette(curve_palette_labels)
      
      p <- ggplot()
      
      if (nrow(measurements) > 0) {
        p <- p +
          geom_point(
            data = measurements,
            aes(x = discharge_m3s_plot, y = stage_cm_plot),
            # aes(x = stage_cm_plot, y = discharge_m3s_plot),
            
            color = "grey45",
            alpha = 0.6,
            size = 1.6
          )
      }
      
      if (nrow(best_match) > 0 && all(c("stage_cm", "discharge_m3s", "curve_label") %in% names(best_match))) {
        best_match <- best_match %>%
          dplyr::mutate(
            stage_cm = as_numeric_app(stage_cm),
            discharge_m3s = as_numeric_app(discharge_m3s),
            curve_label = as.character(curve_label)
          ) %>%
          dplyr::filter(is.finite(stage_cm), is.finite(discharge_m3s))
        
        if (nrow(best_match) > 0) {
          p <- p +
            geom_point(
              data = best_match,
              aes(y = stage_cm, x = discharge_m3s, color = curve_label),
              alpha = 0.6,
              size = 1.6
            )
        }
      }
      
      if (nrow(curve_points) > 0) {
        p <- p +
          geom_line(
            data = curve_points,
            aes(y = stage_cm, x = discharge_m3s, group = curve_segment_label, color = curve_label),
            alpha = 0.85,
            linewidth = 0.6
          )
      }
      
      if (length(curve_palette) > 0) {
        p <- p + scale_color_manual(values = curve_palette, limits = names(curve_palette), drop = FALSE)
      }
      
      print(
        p +
          labs(
            # title = "Curvas-chave e medições de descarga",
            y = "Cota (cm)",
            x = expression(bold("Vazão (" * m^3 * "/s)")),
            color = NULL, #"Curva-chave",
            # caption = "Pontos cinza mostram todas as medições. Pontos coloridos correspondem à janela válida de cada curva em data e cota."
          ) +
          preview_plot_theme(base_size = 6)
      )
    }, error = function(e) {
      draw_empty_plot(paste("Gráfico indisponível:", clean_error_message(conditionMessage(e))))
    })
  }, res=120)
  
  output$rating_curve_validity_timeline_plot <- renderPlot({
    tryCatch({
      diagnostics <- selected_diagnostics()
      curve_metadata <- extract_diagnostic_table(diagnostics, c("curve_metadata"))
      rc_summary <- selected_rating_curve_summary()
      
      if (nrow(rc_summary) == 0 || nrow(curve_metadata) == 0) {
        draw_empty_plot("Não há resumo de validade de curva-chave para este gráfico.")
        return(invisible(NULL))
      }
      
      timeline <- rc_summary %>%
        dplyr::mutate(
          rating_curve_id = as.character(rating_curve_id),
          valid_from = as.Date(parse_app_datetime(valid_from)),
          valid_to = as.Date(parse_app_datetime(valid_to)),
          stage_min_cm = as_numeric_app(stage_min_cm),
          stage_max_cm = as_numeric_app(stage_max_cm)
        ) %>%
        dplyr::left_join(
          curve_metadata %>% dplyr::select(rating_curve_id, curve_label),
          by = "rating_curve_id"
        ) %>%
        dplyr::mutate(valid_to_plot = dplyr::coalesce(valid_to, Sys.Date())) %>%
        dplyr::filter(
          !is.na(valid_from),
          !is.na(valid_to_plot),
          !is.na(stage_min_cm),
          !is.na(stage_max_cm),
          stage_max_cm > stage_min_cm
        )
      
      if (nrow(timeline) == 0) {
        draw_empty_plot("Não há janelas válidas de curva-chave para este gráfico.")
        return(invisible(NULL))
      }
      
      curve_palette <- make_app_plot_palette(timeline$curve_label)
      
      print(
        ggplot(timeline) +
          geom_rect(
            aes(
              xmin = valid_from,
              xmax = valid_to_plot,
              ymin = stage_min_cm,
              ymax = stage_max_cm,
              fill = curve_label,
              color = curve_label
            ),
            alpha = 0.20,
            linewidth = 0.5
          ) +
          # scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
          scale_x_date(date_labels = "%Y") +
          scale_color_manual(values = curve_palette, limits = names(curve_palette), drop = FALSE) +
          scale_fill_manual(values = curve_palette, limits = names(curve_palette), drop = FALSE) +
          guides(color = "none") +
          labs(
            # title = "Janelas de validade das curvas-chave",
            x = "Data",
            y = "Faixa válida de cota (cm)",
            fill = NULL, #"Curva-chave",
            # caption = "A largura indica validade temporal; a altura indica a faixa válida de cotas."
          ) +
          preview_plot_theme(base_size = 6)
      )
    }, error = function(e) {
      draw_empty_plot(paste("Gráfico indisponível:", clean_error_message(conditionMessage(e))))
    })
  }, res = 120)
  
  output$rating_curves_with_residual_envelopes_plot <- renderPlot({
    tryCatch({
      diagnostics <- selected_diagnostics()
      curve_points <- prepare_rating_curve_points_for_plot(diagnostics)
      residual_envelopes <- extract_diagnostic_table(diagnostics, c("residual_envelopes"))
      residual_points <- extract_diagnostic_table(diagnostics, c("residual_points", "best_rating_match"))
      measurements <- prepare_measurements_for_rating_plot()
      
      if (nrow(curve_points) == 0 || nrow(residual_envelopes) == 0) {
        draw_empty_plot("Não há resíduos suficientes para calcular envelopes empíricos.")
        return(invisible(NULL))
      }
      
      envelope_points <- curve_points %>%
        dplyr::left_join(
          residual_envelopes %>%
            dplyr::select(
              rating_curve_segment_id,
              envelope_lower_log_residual,
              envelope_upper_log_residual,
              has_residual_envelope
            ),
          by = "rating_curve_segment_id"
        ) %>%
        dplyr::filter(dplyr::coalesce(has_residual_envelope, FALSE))
      
      curve_palette <- prepare_curve_palette(diagnostics, curve_points)
      p <- ggplot()
      
      if (nrow(envelope_points) > 0) {
        p <- p +
          geom_ribbon(
            data = envelope_points,
            aes(
              x = stage_cm,
              ymin = discharge_m3s * exp(envelope_lower_log_residual),
              ymax = discharge_m3s * exp(envelope_upper_log_residual),
              group = curve_segment_label,
              fill = curve_label
            ),
            alpha = 0.15
          )
      }
      
      if (nrow(measurements) > 0) {
        p <- p +
          geom_point(
            data = measurements,
            aes(x = stage_cm_plot, y = discharge_m3s_plot),
            color = "grey55",
            alpha = 0.20,
            size = 1.2
          )
      }
      
      if (nrow(residual_points) > 0 && "outside_residual_envelope" %in% names(residual_points)) {
        outside_points <- residual_points %>%
          dplyr::filter(dplyr::coalesce(as.logical(outside_residual_envelope), FALSE)) %>%
          dplyr::mutate(
            stage_cm = as_numeric_app(stage_cm),
            discharge_m3s = as_numeric_app(discharge_m3s)
          ) %>%
          dplyr::filter(is.finite(stage_cm), is.finite(discharge_m3s))
        
        if (nrow(outside_points) > 0) {
          p <- p +
            geom_point(
              data = outside_points,
              aes(x = stage_cm, y = discharge_m3s),
              color = "black",
              alpha = 0.85,
              size = 1.6
            )
        }
      }
      
      p <- p +
        geom_line(
          data = curve_points,
          aes(x = stage_cm, y = discharge_m3s, group = curve_segment_label, color = curve_label),
          alpha = 0.85,
          linewidth = 0.6
        )
      
      if (length(curve_palette) > 0) {
        p <- p +
          scale_color_manual(values = curve_palette, limits = names(curve_palette), drop = FALSE) +
          scale_fill_manual(values = curve_palette, limits = names(curve_palette), drop = FALSE)
      }
      
      print(
        p +
          labs(
            # title = "Curvas-chave com envelopes empíricos dos resíduos",
            x = "Cota (cm)",
            y = expression(bold("Vazão (" * m^3 * "/s)")),
            color = NULL, #"Curva-chave",
            fill = NULL, #"Curva-chave",
            caption = "Pontos pretos estão fora dos envelopes empíricos"
          ) +
          preview_plot_theme(base_size = 6)
      )
    }, error = function(e) {
      draw_empty_plot(paste("Gráfico indisponível:", clean_error_message(conditionMessage(e))))
    })
  }, res = 120)
  
  output$residual_temporal_regime_residual_discharge_plot <- renderPlot({
    tryCatch({
      diagnostics <- selected_diagnostics()
      temporal_regime <- diagnostics$temporal_regime
      regime_points <- extract_temporal_regime_table(diagnostics, "points")
      
      if (nrow(regime_points) == 0 ||
          !all(c("discharge_m3s", "power_log_residual") %in% names(regime_points))) {
        draw_empty_plot("Não há dados suficientes para o rastreamento de regimes temporais.")
        return(invisible(NULL))
      }
      
      if (!"regime_label" %in% names(regime_points)) {
        regime_points$regime_label <- "Regime único"
      }
      
      regime_points <- regime_points %>%
        dplyr::mutate(
          discharge_m3s = as_numeric_app(discharge_m3s),
          power_log_residual = as_numeric_app(power_log_residual),
          regime_label = as.character(regime_label)
        ) %>%
        dplyr::filter(is.finite(discharge_m3s), discharge_m3s > 0, is.finite(power_log_residual))
      
      if (nrow(regime_points) == 0) {
        draw_empty_plot("Não há dados suficientes para o gráfico de resíduo por vazão.")
        return(invisible(NULL))
      }
      
      regime_palette <- make_app_plot_palette(regime_points$regime_label)
      residual_limits <- residual_axis_limits(regime_points$power_log_residual)
      
      p <- ggplot(regime_points, aes(x = discharge_m3s, y = power_log_residual, color = regime_label)) +
        geom_hline(yintercept = 0, linewidth = 0.3, alpha = 0.6) +
        geom_point(alpha = 0.6, size = 1.6) +
        scale_x_log10() +
        scale_color_manual(values = regime_palette, limits = names(regime_palette), drop = FALSE) +
        labs(
          # title = "Regimes resíduo-temporais: resíduo × vazão",
          x = expression(bold("Vazão (" * m^3 * "/s, escala log)")),
          y = "log(Qobs) - log(Qfit)",
          color = NULL, #"Regime",
          # caption = "Este gráfico verifica se os regimes de resíduos são dominados pela magnitude da vazão."
        ) +
        preview_plot_theme(base_size = 6)
      
      if (!is.null(residual_limits)) {
        p <- p + coord_cartesian(ylim = residual_limits)
      }
      
      print(p)
    }, error = function(e) {
      draw_empty_plot(paste("Gráfico indisponível:", clean_error_message(conditionMessage(e))))
    })
  },res = 120)
  
  output$residual_temporal_regime_residual_time_plot <- renderPlot({
    tryCatch({
      diagnostics <- selected_diagnostics()
      temporal_regime <- diagnostics$temporal_regime
      regime_points <- extract_temporal_regime_table(diagnostics, "points")
      
      if (nrow(regime_points) == 0 ||
          !all(c("measurement_date", "power_log_residual") %in% names(regime_points))) {
        draw_empty_plot("Não há dados suficientes para o rastreamento de regimes temporais.")
        return(invisible(NULL))
      }
      
      if (!"regime_label" %in% names(regime_points)) {
        regime_points$regime_label <- "Regime único"
      }
      
      regime_points <- regime_points %>%
        dplyr::mutate(
          measurement_date = as.Date(measurement_date),
          power_log_residual = as_numeric_app(power_log_residual),
          regime_label = as.character(regime_label)
        ) %>%
        dplyr::filter(!is.na(measurement_date), is.finite(power_log_residual))
      
      if (nrow(regime_points) == 0) {
        draw_empty_plot("Não há dados suficientes para o gráfico temporal de resíduos.")
        return(invisible(NULL))
      }
      
      regime_palette <- make_app_plot_palette(regime_points$regime_label)
      break_dates <- extract_break_dates(temporal_regime)
      residual_limits <- residual_axis_limits(regime_points$power_log_residual)
      
      p <- ggplot(regime_points, aes(x = measurement_date, y = power_log_residual, color = regime_label)) +
        geom_hline(yintercept = 0, linewidth = 0.3, alpha = 0.6) +
        geom_point(alpha = 0.6, size = 1.6) +
        scale_color_manual(values = regime_palette, limits = names(regime_palette), drop = FALSE) +
        labs(
          # title = "Regimes resíduo-temporais: resíduo no tempo",
          x = "Data da medição",
          y = "log(Qobs) - log(Qfit)",
          color = NULL, #"Regime",
          # caption = "O eixo y usa limites robustos para legibilidade; resíduos extremos permanecem nas tabelas."
        ) +
        preview_plot_theme(base_size = 6)
      
      if (length(break_dates) > 0) {
        p <- p + geom_vline(xintercept = break_dates, linetype = "dashed", linewidth = 0.5, alpha = 0.7)
      }
      
      if (!is.null(residual_limits)) {
        p <- p + coord_cartesian(ylim = residual_limits)
      }
      
      print(p)
    }, error = function(e) {
      draw_empty_plot(paste("Gráfico indisponível:", clean_error_message(conditionMessage(e))))
    })
  },res = 120)
  
  output$residual_temporal_regime_stage_discharge_plot <- renderPlot({
    tryCatch({
      diagnostics <- selected_diagnostics()
      regime_points <- extract_temporal_regime_table(diagnostics, "points")
      power_curve_points <- extract_diagnostic_table(diagnostics, c("power_curve_points"))
      
      if (nrow(regime_points) == 0 ||
          !all(c("discharge_m3s", "stage_cm") %in% names(regime_points))) {
        draw_empty_plot("Não há dados suficientes para o gráfico cota-vazão por regime.")
        return(invisible(NULL))
      }
      
      if (!"regime_label" %in% names(regime_points)) {
        regime_points$regime_label <- "Regime único"
      }
      
      regime_points <- regime_points %>%
        dplyr::mutate(
          discharge_m3s = as_numeric_app(discharge_m3s),
          stage_cm = as_numeric_app(stage_cm),
          regime_label = as.character(regime_label)
        ) %>%
        dplyr::filter(is.finite(discharge_m3s), is.finite(stage_cm))
      
      if (nrow(regime_points) == 0) {
        draw_empty_plot("Não há dados suficientes para o gráfico cota-vazão por regime.")
        return(invisible(NULL))
      }
      
      regime_palette <- make_app_plot_palette(regime_points$regime_label)
      
      p <- ggplot(regime_points, aes(x = discharge_m3s, y = stage_cm, color = regime_label)) +
        geom_point(alpha = 0.62, size = 1.45) +
        scale_color_manual(values = regime_palette, limits = names(regime_palette), drop = FALSE) +
        labs(
          # title = "Regimes resíduo-temporais: vazão × cota",
          x = expression(bold("Vazão (" * m^3 * "/s)")),
          y = "Cota (cm)",
          color = NULL, #"Regime",
          # caption = "Regimes são períodos contíguos de triagem, não uma classificação final."
        ) +
        preview_plot_theme(base_size = 6)
      
      if (nrow(power_curve_points) > 0 && all(c("discharge_m3s", "stage_cm") %in% names(power_curve_points))) {
        power_curve_points <- power_curve_points %>%
          dplyr::mutate(
            discharge_m3s = as_numeric_app(discharge_m3s),
            stage_cm = as_numeric_app(stage_cm)
          ) %>%
          dplyr::filter(is.finite(discharge_m3s), is.finite(stage_cm))
        
        if (nrow(power_curve_points) > 0) {
          p <- p +
            geom_line(
              data = power_curve_points,
              aes(x = discharge_m3s, y = stage_cm),
              inherit.aes = FALSE,
              color = "black",
              linewidth = 0.8,
              alpha = 0.75
            )
        }
      }
      
      print(p)
    }, error = function(e) {
      draw_empty_plot(paste("Gráfico indisponível:", clean_error_message(conditionMessage(e))))
    })
  }, res = 120)
  
  
  table_status_ui <- function(data, singular, plural = NULL) {
    if (is.null(plural)) {
      plural <- paste0(singular, "s")
    }
    
    n <- tryCatch(nrow(as_display_table(data)), error = function(e) NA_integer_)
    
    if (is.na(n)) {
      return(tags$div(class = "table-status warning", "Não foi possível verificar a disponibilidade da tabela."))
    }
    
    if (n == 0) {
      return(tags$div(class = "table-status empty", "Sem registros disponíveis para a estação selecionada."))
    }
    
    label <- if (n == 1) singular else plural
    tags$div(class = "table-status available", paste0(n, " ", label, " disponíveis para a estação selecionada."))
  }
  
  output$measurement_table_status <- renderUI({
    table_status_ui(selected_measurements(), "medição", "medições")
  })
  
  output$rating_curve_summary_table_status <- renderUI({
    table_status_ui(rating_curve_summary_with_equations(), "registro de resumo", "registros de resumo")
  })
  
  output$cross_section_table_status <- renderUI({
    table_status_ui(selected_cross_section_data(), "seção transversal", "seções transversais")
  })
  
  output$diagnostic_summary_table_status <- renderUI({
    diagnostics <- selected_diagnostics()
    table_status_ui(extract_diagnostic_table(diagnostics, c("summary", "diagnostic_summary", "station_summary")), "registro", "registros")
  })
  
  output$diagnostic_indices_table_status <- renderUI({
    diagnostics <- selected_diagnostics()
    table_status_ui(extract_diagnostic_table(diagnostics, c("indices", "diagnostic_indices")), "índice", "índices")
  })
  
  output$diagnostic_flags_table_status <- renderUI({
    diagnostics <- selected_diagnostics()
    table <- extract_diagnostic_table(diagnostics, c("measurement_flags", "flags", "flagged_measurements")) %>%
      filter_flagged_measurements_for_display()
    table_status_ui(table, "medição sinalizada", "medições sinalizadas")
  })
  
  output$diagnostic_repeated_stage_table_status <- renderUI({
    diagnostics <- selected_diagnostics()
    table_status_ui(extract_repeated_group_details(diagnostics, "same_stage_variable_discharge"), "grupo", "grupos")
  })
  
  output$diagnostic_repeated_discharge_table_status <- renderUI({
    diagnostics <- selected_diagnostics()
    table_status_ui(extract_repeated_group_details(diagnostics, "same_discharge_variable_stage"), "grupo", "grupos")
  })
  
  render_table <- function(data, preferred_columns = NULL, hidden_columns = NULL, empty_message = "Nenhum registro disponível.", filter = "top", page_length = 10, keep_only_preferred = FALSE, escape = TRUE) {
    tryCatch({
      if (!is.null(hidden_columns) && nrow(as_display_table(data)) > 0) {
        data <- as_display_table(data) %>%
          dplyr::select(-dplyr::any_of(hidden_columns))
      }
      
      table <- prepare_display_table(data, preferred_columns, keep_only_preferred = keep_only_preferred)
      
      if (nrow(table) == 0) {
        table <- tibble::tibble(Mensagem = empty_message)
        filter <- "none"
      }
      
      table <- sanitize_table_for_dt(table)
      
      DT::datatable(
        table,
        rownames = FALSE,
        filter = filter,
        escape = escape,
        options = list(
          pageLength = page_length,
          scrollX = TRUE,
          autoWidth = TRUE,
          deferRender = TRUE,
          language = list(
            search = "Buscar:",
            lengthMenu = "Mostrar _MENU_ registros",
            info = "Mostrando _START_ a _END_ de _TOTAL_ registros",
            paginate = list(previous = "Anterior", `next` = "Próxima")
          )
        )
      )
    }, error = function(e) {
      message <- paste("Tabela indisponível:", clean_error_message(conditionMessage(e)))
      DT::datatable(
        tibble::tibble(Mensagem = message),
        rownames = FALSE,
        filter = "none",
        escape = escape,
        options = list(
          pageLength = 1,
          dom = "t",
          language = list(
            emptyTable = "Tabela indisponível."
          )
        )
      )
    })
  }
  
  output$measurement_table <- DT::renderDT({
    render_table(
      selected_measurements(),
      preferred_columns = c(
        "station_code", "measurement_datetime", "measurement_date", "consistency_level",
        "stage_cm", "discharge_m3s", "wetted_area_m2", "width_m",
        "mean_depth_m", "mean_velocity_ms", "last_update"
      ),
      empty_message = "Nenhuma medição de descarga disponível para esta estação.",
      keep_only_preferred = TRUE
    )
  })
  
  output$rating_curve_summary_table <- DT::renderDT({
    render_table(
      rating_curve_summary_with_equations(),
      preferred_columns = c(
        "station_code", "valid_from", "valid_to", "stage_min_cm", "stage_max_cm",
        "discharge_min_m3s", "discharge_max_m3s", "equation_display"
      ),
      hidden_columns = c("curve_id", "rating_curve_id", "segment_id", "rating_curve_segment_id", "segment_number", "coefficient_a", "coefficient_h0", "coefficient_n", "coefficient_b", "a", "h0", "b", "n"),
      empty_message = "Nenhum resumo de curva-chave disponível para esta estação.",
      keep_only_preferred = TRUE,
      escape = FALSE
    )
  })
  
  output$diagnostic_summary_table <- DT::renderDT({
    diagnostics <- selected_diagnostics()
    table <- extract_diagnostic_table(diagnostics, c("summary", "diagnostic_summary", "station_summary"))
    render_table(
      table,
      preferred_columns = c(
        "station_code", "n_valid_measurements", "rating_match_fraction",
        "median_abs_rating_log_residual", "n_temporal_regimes",
        "temporal_regime_evidence_class"
      ),
      empty_message = "Nenhum resumo diagnóstico disponível.",
      filter = "none",
      keep_only_preferred = TRUE
    )
  })
  
  output$diagnostic_indices_table <- DT::renderDT({
    diagnostics <- selected_diagnostics()
    table <- extract_diagnostic_table(diagnostics, c("indices", "diagnostic_indices"))
    render_table(
      table,
      preferred_columns = c(
        "index_group", "index_name", "index_value", "index_unit",
        "index_class", "index_description"
      ),
      hidden_columns = c("station_code", "display_order"),
      empty_message = "Nenhum índice diagnóstico disponível.",
      filter = "top",
      page_length = 8,
      keep_only_preferred = TRUE
    )
  })
  
  output$diagnostic_flags_table <- DT::renderDT({
    diagnostics <- selected_diagnostics()
    table <- extract_diagnostic_table(diagnostics, c("measurement_flags", "flags", "flagged_measurements")) %>%
      filter_flagged_measurements_for_display()
    render_table(
      table,
      preferred_columns = c(
        "measurement_date", "stage_cm", "discharge_m3s", "rating_relative_residual_pct",
        "outside_residual_envelope", "stage_zero_or_negative_flag",
        "discharge_zero_or_negative_flag", "repeated_stage_variable_discharge_flag",
        "repeated_discharge_variable_stage_flag"
      ),
      empty_message = "Nenhuma medição sinalizada disponível.",
      keep_only_preferred = TRUE
    )
  })
  
  output$diagnostic_repeated_stage_table <- DT::renderDT({
    diagnostics <- selected_diagnostics()
    table <- extract_repeated_group_details(diagnostics, "same_stage_variable_discharge")
    render_table(
      table,
      preferred_columns = c("group_type", "group_value", "n_group", "spread_value", "relative_spread"),
      empty_message = "Nenhum grupo de cota repetida em atenção.",
      keep_only_preferred = TRUE
    )
  })
  
  output$diagnostic_repeated_discharge_table <- DT::renderDT({
    diagnostics <- selected_diagnostics()
    table <- extract_repeated_group_details(diagnostics, "same_discharge_variable_stage")
    render_table(
      table,
      preferred_columns = c("group_type", "group_value", "n_group", "spread_value", "relative_spread"),
      empty_message = "Nenhum grupo de vazão repetida em atenção.",
      keep_only_preferred = TRUE
    )
  })
  
  output$cross_section_selected_profile_plot <- renderPlot({
    tryCatch({
      vertices <- selected_cross_section_vertices()
      y_limits <- cross_section_top_stage_limits()
      
      if (nrow(vertices) == 0) {
        draw_empty_plot("Nenhum vértice disponível para a seção transversal selecionada.")
        return(invisible(NULL))
      }
      
      plot_obj <- ggplot2::ggplot(
        vertices,
        ggplot2::aes(x = vertex_distance_m, y = vertex_stage_cm)
      ) +
        ggplot2::geom_line(linewidth = 0.8, color = "#7f0000") +
        ggplot2::geom_point(size = 1.8, color = "#7f0000") +
        ggplot2::labs(
          x = "Distância horizontal (m)",
          y = "Cota (cm)",
          title = "Seção transversal selecionada"
        ) +
        preview_plot_theme(base_size = 6)
      
      if (!is.null(y_limits)) {
        plot_obj <- plot_obj + ggplot2::coord_cartesian(ylim = y_limits)
      }
      
      print(plot_obj)
    }, error = function(e) {
      draw_empty_plot(paste("Gráfico indisponível:", clean_error_message(conditionMessage(e))))
    })
  }, res = 120)
  
  output$cross_section_selected_rating_curve_plot <- renderPlot({
    tryCatch({
      curve_points <- selected_valid_rating_curve_points_for_cross_section()
      section_date <- selected_cross_section_date()
      y_limits <- cross_section_top_stage_limits()
      
      if (nrow(curve_points) == 0) {
        draw_empty_plot("Nenhuma curva-chave válida encontrada para a data da seção selecionada.")
        return(invisible(NULL))
      }
      
      plot_obj <- ggplot2::ggplot(
        curve_points,
        ggplot2::aes(
          x = discharge_m3s,
          y = stage_cm,
          group = curve_segment_label
        )
      ) +
        ggplot2::geom_line(linewidth = 0.75, color = station_map_colors$rating_curves) +
        ggplot2::labs(
          x = "Vazão (m³/s)",
          y = "Cota (cm)",
          title = "Curva-chave válida"
          # subtitle = ifelse(
          #   is.na(section_date),
          #   "Data da seção não disponível",
          #   paste0("Data da seção: ", format(section_date, "%Y-%m-%d"))
          # )
        ) +
        preview_plot_theme(base_size = 6)
      
      if (!is.null(y_limits)) {
        plot_obj <- plot_obj + ggplot2::coord_cartesian(ylim = y_limits)
      }
      
      print(plot_obj)
    }, error = function(e) {
      draw_empty_plot(paste("Gráfico indisponível:", clean_error_message(conditionMessage(e))))
    })
  },res = 120)
  
  output$cross_section_overlay_plot <- renderPlot({
    tryCatch({
      vertices <- selected_cross_section_vertices_all()
      section_id <- selected_cross_section_id()
      
      if (nrow(vertices) == 0) {
        draw_empty_plot("Nenhum vértice de seção transversal disponível para esta estação.")
        return(invisible(NULL))
      }
      
      vertices <- vertices %>%
        dplyr::filter(
          is.finite(vertex_distance_m),
          is.finite(vertex_stage_cm)
        ) %>%
        dplyr::arrange(cross_section_id, vertex_order) %>%
        dplyr::mutate(
          selected_section = as.character(cross_section_id) == as.character(section_id)
        )
      
      if (nrow(vertices) == 0) {
        draw_empty_plot("Não há pares válidos de distância e cota para plotar as seções transversais.")
        return(invisible(NULL))
      }
      
      print(
        ggplot2::ggplot() +
          ggplot2::geom_path(
            data = vertices %>% dplyr::filter(!selected_section),
            ggplot2::aes(
              x = vertex_distance_m,
              y = vertex_stage_cm,
              group = cross_section_id
            ),
            linewidth = 0.45,
            alpha = 0.65,
            color = "#fdbb84"
          ) +
          ggplot2::geom_path(
            data = vertices %>% dplyr::filter(selected_section),
            ggplot2::aes(
              x = vertex_distance_m,
              y = vertex_stage_cm,
              group = cross_section_id
            ),
            linewidth = 1.1,
            alpha = 0.85,
            color = "#7f0000"
          ) +
          ggplot2::labs(
            x = "Distância horizontal (m)",
            y = "Cota (cm)",
            title = "Comparação entre seções transversais",
            subtitle = "Linhas claras mostram todas as seções; linha escura mostra a seção selecionada"
          ) +
          ggplot2::theme(legend.position = "none") +
          preview_plot_theme(base_size = 6) 
          
      )
    }, error = function(e) {
      draw_empty_plot(paste("Gráfico indisponível:", clean_error_message(conditionMessage(e))))
    })
  },res = 120)
  
  output$cross_section_temporal_plot <- renderPlot({
    tryCatch({
      sections <- selected_cross_sections()
      section_id <- selected_cross_section_id()
      
      if (nrow(sections) == 0) {
        draw_empty_plot("Nenhuma seção transversal disponível para esta estação.")
        return(invisible(NULL))
      }
      
      plot_data <- sections %>%
        dplyr::mutate(
          measurement_date = as.Date(measurement_datetime),
          vertex_stage_range_cm = as_numeric_app(vertex_stage_max_cm) - as_numeric_app(vertex_stage_min_cm),
          vertex_distance_span_m = as_numeric_app(vertex_distance_max_m) - as_numeric_app(vertex_distance_min_m),
          n_vertices_app = as_numeric_app(n_vertices),
          selected_section = as.character(cross_section_id) == as.character(section_id)
        ) %>%
        dplyr::filter(!is.na(measurement_date)) %>%
        dplyr::mutate(
          metric_value = dplyr::case_when(
            is.finite(vertex_stage_range_cm) ~ vertex_stage_range_cm,
            is.finite(vertex_distance_span_m) ~ vertex_distance_span_m,
            TRUE ~ n_vertices_app
          ),
          metric_label = dplyr::case_when(
            is.finite(vertex_stage_range_cm) ~ "Amplitude vertical (cm)",
            is.finite(vertex_distance_span_m) ~ "Amplitude horizontal (m)",
            TRUE ~ "Número de vértices"
          )
        ) %>%
        dplyr::filter(is.finite(metric_value))
      
      if (nrow(plot_data) == 0) {
        draw_empty_plot("Não há métricas geométricas válidas para resumir as seções transversais.")
        return(invisible(NULL))
      }
      
      y_label <- plot_data$metric_label[which(!is.na(plot_data$metric_label))[1]]
      if (is.na(y_label)) {
        y_label <- "Métrica da seção"
      }
      
      print(
        ggplot2::ggplot(
          plot_data,
          ggplot2::aes(x = measurement_date, y = metric_value)
        ) +
          ggplot2::geom_line(alpha = 0.55, color = "gray50") +
          ggplot2::geom_point(
            data = plot_data %>% dplyr::filter(!selected_section),
            ggplot2::aes(size = n_vertices_app),
            alpha = 0.55,
            color = "gray55"
          ) +
          ggplot2::geom_point(
            data = plot_data %>% dplyr::filter(selected_section),
            ggplot2::aes(size = n_vertices_app),
            alpha = 0.95,
            color = "gray15"
          ) +
          ggplot2::scale_size_continuous(name = "Vértices", range = c(2, 6)) +
          ggplot2::labs(
            x = "Data do levantamento",
            y = y_label,
            title = "Resumo temporal das seções transversais",
            subtitle = "O ponto escuro indica a seção selecionada"
          ) +
          preview_plot_theme(base_size = 6)
      )
    }, error = function(e) {
      draw_empty_plot(paste("Gráfico indisponível:", clean_error_message(conditionMessage(e))))
    })
  },res = 120)
  
  output$cross_section_table <- DT::renderDT({
    render_table(
      selected_cross_sections(),
      preferred_columns = c(
        "station_code",
        "measurement_datetime",
        "consistency_level",
        "survey_number",
        "section_type",
        "n_vertices",
        "n_vertices_reported",
        "vertex_distance_min_m",
        "vertex_distance_max_m",
        "vertex_stage_min_cm",
        "vertex_stage_max_cm",
        "distance_pipf_m",
        "last_update"
      ),
      empty_message = "Nenhuma seção transversal disponível para esta estação.",
      keep_only_preferred = TRUE
    )
  })
  
  output$source_limitations <- renderUI({
    tagList(
      h3(app_config$app_name),
      p(app_config$app_subtitle),
      p(
        "O HydroStat Data Explorer é um sistema integrado para visualização, triagem e análise de dados hidrológicos ",
        "associados a estações da ANA. O aplicativo combina produtos derivados locais, dados fornecidos pelo usuário ",
        "e downloads públicos em sessão quando disponíveis."
      ),
      div(
        class = "institutional-grid",
        div(
          class = "institutional-card",
          strong("Produtos locais"),
          p("Mapa, cadastro de estações, curvas-chave, medições de descarga, seções transversais e resumos derivados são lidos a partir da base compacta do aplicativo.")
        ),
        div(
          class = "institutional-card",
          strong("Dados em sessão"),
          p("Séries de vazão, cota e precipitação enviadas ou baixadas durante o uso ficam apenas na sessão ativa do usuário.")
        ),
        div(
          class = "institutional-card",
          strong("Sem credenciais do autor"),
          p("O app não armazena CPF/CNPJ, senha ou token do autor do projeto.")
        ),
        div(
          class = "institutional-card",
          strong("Triagem hidrológica"),
          p("Os indicadores e alertas são ferramentas de apoio à revisão visual. Eles não representam classificação oficial de qualidade da ANA.")
        )
      ),
      div(
        class = "limitation-box",
        strong("Escopo atual: "),
        "mapa de estações, curvas-chave e resumo de descarga, análise de séries fluviométricas, análise de séries pluviométricas, falhas, consistência, estatísticas e eventos extremos descritivos."
      ),
      h4("Uso recomendado"),
      tags$ul(
        tags$li("Confira se a estação selecionada corresponde ao arquivo ou dado baixado antes de interpretar os resultados."),
        tags$li("Use os indicadores como sinais de triagem e não como regras automáticas de exclusão."),
        tags$li("Séries carregadas na sessão não são gravadas no banco local, em logs persistentes ou em arquivos internos do aplicativo."),
        tags$li(
          "Código e documentação: ",
          tags$a(
            app_config$app_repository,
            href = app_config$app_repository_url,
            target = "_blank",
            rel = "noopener noreferrer"
          )
        )
      )
    )
  })
  
  output$download_station_summary <- downloadHandler(
    filename = function() {
      paste0("station_", selected_code(), "_summary.csv")
    },
    content = function(file) {
      station <- selected_station()
      metadata <- station_metadata_fields(station) %>%
        dplyr::select(label, value) %>%
        tidyr::pivot_wider(names_from = label, values_from = value)
      attention <- station_attention_fields(station) %>%
        dplyr::select(label, value) %>%
        tidyr::pivot_wider(names_from = label, values_from = value)
      
      export <- dplyr::bind_cols(metadata, attention)
      readr::write_csv(export, file)
    }
  )
  
  output$fluviometric_api_download_report_download <- downloadHandler(
    filename = function() {
      paste0("relatorio_download_api_ana_flu_", selected_code(), ".csv")
    },
    content = function(file) {
      report <- ana_download_report_for_module("flu")
      readr::write_excel_csv(report, file)
    }
  )
  
  output$pluviometric_api_download_report_download <- downloadHandler(
    filename = function() {
      paste0("relatorio_download_api_ana_plu_", selected_code(), ".csv")
    },
    content = function(file) {
      report <- ana_download_report_for_module("plu")
      readr::write_excel_csv(report, file)
    }
  )
  
  
