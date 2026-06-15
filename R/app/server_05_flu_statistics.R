# ============================================================
# server_05_flu_statistics.R
# Purpose: Monthly/annual flow statistics, duration, regularization, and Rippl outputs.
# ============================================================
# BEGIN ORIGINAL BODY
  # ------------------------------------------------------------
  # Fluviometric tab: monthly and annual statistics
  # ------------------------------------------------------------
  
  fluviometric_stats_month_labels <- c(
    "Jan", "Fev", "Mar", "Abr", "Mai", "Jun",
    "Jul", "Ago", "Set", "Out", "Nov", "Dez"
  )
  
  fluviometric_stats_safe_mean <- function(x) {
    x <- x[is.finite(x)]
    if (length(x) == 0) return(NA_real_)
    mean(x, na.rm = TRUE)
  }
  
  fluviometric_stats_safe_min <- function(x) {
    x <- x[is.finite(x)]
    if (length(x) == 0) return(NA_real_)
    min(x, na.rm = TRUE)
  }
  
  fluviometric_stats_safe_max <- function(x) {
    x <- x[is.finite(x)]
    if (length(x) == 0) return(NA_real_)
    max(x, na.rm = TRUE)
  }
  
  fluviometric_stats_qmlt <- function(daily) {
    fluviometric_stats_safe_mean(daily$discharge_m3s)
  }
  
  fluviometric_stats_write_csv_bom <- function(data, file, digits = 3) {
    data <- dplyr::as_tibble(data)
    
    data <- data |>
      dplyr::mutate(
        dplyr::across(
          where(is.numeric),
          ~ round(.x, digits)
        )
      )
    
    csv_decimal <- getOption("OutDec", ".")
    
    if (!csv_decimal %in% c(".", ",")) {
      csv_decimal <- "."
    }
    
    csv_separator <- if (identical(csv_decimal, ",")) {
      ";"
    } else {
      ","
    }
    
    temp_file <- tempfile(fileext = ".csv")
    
    utils::write.table(
      data,
      file = temp_file,
      sep = csv_separator,
      dec = csv_decimal,
      row.names = FALSE,
      col.names = TRUE,
      quote = TRUE,
      na = "",
      qmethod = "double",
      fileEncoding = "UTF-8"
    )
    
    input_raw <- readBin(
      temp_file,
      what = "raw",
      n = file.info(temp_file)$size
    )
    
    output_con <- file(file, open = "wb")
    on.exit(close(output_con), add = TRUE)
    
    # UTF-8 BOM for better compatibility with Excel on Windows.
    writeBin(as.raw(c(0xEF, 0xBB, 0xBF)), output_con)
    writeBin(input_raw, output_con)
    
    unlink(temp_file)
  }
  
  fluviometric_stats_fdc_probabilities <- function() {
    unique(round(c(
      seq(0.1, 1, by = 0.1),
      seq(1.25, 5, by = 0.25),
      seq(6, 94, by = 1),
      seq(95, 99, by = 0.25),
      seq(99.1, 99.9, by = 0.1)
    ), 2))
  }
  
  fluviometric_stats_normal_probability_trans <- function() {
    scales::trans_new(
      name = "normal_probability_percent",
      transform = function(x) {
        stats::qnorm(pmin(pmax(x, 0.1), 99.9) / 100)
      },
      inverse = function(x) {
        100 * stats::pnorm(x)
      },
      domain = c(0.1, 99.9)
    )
  }
  
  fluviometric_stats_flow_duration <- function(values, target_probabilities = NULL) {
    values <- values[is.finite(values)]
    
    if (length(values) == 0) {
      return(tibble::tibble())
    }
    
    if (is.null(target_probabilities)) {
      target_probabilities <- fluviometric_stats_fdc_probabilities()
    }
    
    sorted_values <- sort(values, decreasing = TRUE)
    n_values <- length(sorted_values)
    permanence <- seq_len(n_values) / n_values * 100
    
    if (n_values == 1) {
      return(
        tibble::tibble(
          permanence_pct = target_probabilities,
          discharge_m3s = sorted_values[1]
        )
      )
    }
    
    tibble::tibble(
      permanence_pct = target_probabilities,
      discharge_m3s = as.numeric(stats::approx(
        x = permanence,
        y = sorted_values,
        xout = target_probabilities,
        rule = 2
      )$y)
    )
  }
  
  fluviometric_stats_flow_value <- function(values, permanence_pct) {
    duration <- fluviometric_stats_flow_duration(
      values = values,
      target_probabilities = permanence_pct
    )
    
    if (nrow(duration) == 0) {
      return(NA_real_)
    }
    
    duration$discharge_m3s[1]
  }
  
  fluviometric_stats_min_years_required <- 5L
  
  fluviometric_stats_has_required_years <- function(annual_data) {
    if (is.null(annual_data) || nrow(annual_data) == 0) {
      return(FALSE)
    }
    
    n_years <- annual_data |>
      dplyr::filter(is.finite(q_mean_m3s)) |>
      dplyr::summarise(n = dplyr::n_distinct(year)) |>
      dplyr::pull(n)
    
    isTRUE(n_years >= fluviometric_stats_min_years_required)
  }
  
  fluviometric_stats_rolling_5yr <- reactive({
    annual <- fluviometric_stats_annual() |>
      dplyr::filter(is.finite(q_mean_m3s)) |>
      dplyr::arrange(year)
    
    if (nrow(annual) < 5) {
      return(tibble::tibble())
    }
    
    years <- annual$year
    
    windows <- lapply(seq(min(years), max(years) - 4), function(start_year) {
      end_year <- start_year + 4
      
      window_data <- annual |>
        dplyr::filter(year >= start_year, year <= end_year)
      
      if (nrow(window_data) != 5) {
        return(NULL)
      }
      
      tibble::tibble(
        start_year = start_year,
        end_year = end_year,
        period_label = paste0(start_year, "–", end_year),
        q_mean_5yr_m3s = mean(window_data$q_mean_m3s, na.rm = TRUE),
        q_min_annual_m3s = min(window_data$q_mean_m3s, na.rm = TRUE),
        q_max_annual_m3s = max(window_data$q_mean_m3s, na.rm = TRUE)
      )
    })
    
    dplyr::bind_rows(windows)
  })
  
  fluviometric_stats_dry_5yr_window <- reactive({
    rolling <- fluviometric_stats_rolling_5yr()
    
    if (nrow(rolling) == 0) {
      return(tibble::tibble())
    }
    
    rolling |>
      dplyr::slice_min(q_mean_5yr_m3s, n = 1, with_ties = FALSE)
  })
  
  fluviometric_stats_wet_5yr_window <- reactive({
    rolling <- fluviometric_stats_rolling_5yr()
    
    if (nrow(rolling) == 0) {
      return(tibble::tibble())
    }
    
    rolling |>
      dplyr::slice_max(q_mean_5yr_m3s, n = 1, with_ties = FALSE)
  })
  
  fluviometric_stats_daily <- reactive({
    series <- fluviometric_discharge_series()
    
    if (is.null(series) || nrow(series) == 0) {
      return(tibble::tibble())
    }
    
    series |>
      dplyr::mutate(
        date = as.Date(date),
        discharge_m3s = as.numeric(discharge_m3s),
        has_discharge = is.finite(discharge_m3s),
        year = as.integer(format(date, "%Y")),
        month = as.integer(format(date, "%m")),
        month_label = factor(
          fluviometric_stats_month_labels[month],
          levels = fluviometric_stats_month_labels
        )
      ) |>
      dplyr::filter(!is.na(date))
  })
  
  fluviometric_stats_monthly <- reactive({
    daily <- fluviometric_stats_daily()
    
    if (nrow(daily) == 0) {
      return(tibble::tibble())
    }
    
    daily |>
      dplyr::group_by(year, month, month_label) |>
      dplyr::summarise(
        days_in_period = dplyr::n(),
        days_with_discharge = sum(has_discharge, na.rm = TRUE),
        days_missing = days_in_period - days_with_discharge,
        failure_pct = 100 * days_missing / days_in_period,
        q_mean_m3s = fluviometric_stats_safe_mean(discharge_m3s),
        q_min_m3s = fluviometric_stats_safe_min(discharge_m3s),
        q_max_m3s = fluviometric_stats_safe_max(discharge_m3s),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        period = sprintf("%04d-%02d", year, month)
      ) |>
      dplyr::arrange(year, month)
  })
  
  fluviometric_stats_annual <- reactive({
    daily <- fluviometric_stats_daily()
    
    if (nrow(daily) == 0) {
      return(tibble::tibble())
    }
    
    daily |>
      dplyr::group_by(year) |>
      dplyr::summarise(
        days_in_period = dplyr::n(),
        days_with_discharge = sum(has_discharge, na.rm = TRUE),
        days_missing = days_in_period - days_with_discharge,
        failure_pct = 100 * days_missing / days_in_period,
        q_mean_m3s = fluviometric_stats_safe_mean(discharge_m3s),
        q_min_m3s = fluviometric_stats_safe_min(discharge_m3s),
        q_max_m3s = fluviometric_stats_safe_max(discharge_m3s),
        .groups = "drop"
      ) |>
      dplyr::arrange(year)
  })
  
  fluviometric_stats_monthly_regime <- reactive({
    monthly <- fluviometric_stats_monthly()
    
    if (nrow(monthly) == 0) {
      return(tibble::tibble())
    }
    
    monthly |>
      dplyr::filter(is.finite(q_mean_m3s)) |>
      dplyr::group_by(month, month_label) |>
      dplyr::summarise(
        years_with_data = dplyr::n_distinct(year),
        q_mean_m3s = fluviometric_stats_safe_mean(q_mean_m3s),
        q_min_m3s = fluviometric_stats_safe_min(q_mean_m3s),
        q_max_m3s = fluviometric_stats_safe_max(q_mean_m3s),
        .groups = "drop"
      ) |>
      dplyr::arrange(month)
  })
  
  fluviometric_stats_monthly_wide <- reactive({
    monthly <- fluviometric_stats_monthly()
    annual <- fluviometric_stats_annual()
    regime <- fluviometric_stats_monthly_regime()
    daily <- fluviometric_stats_daily()
    
    if (nrow(monthly) == 0) {
      return(tibble::tibble())
    }
    
    wide <- monthly |>
      dplyr::transmute(
        Ano = as.character(year),
        month_label = as.character(month_label),
        q_mean_m3s = q_mean_m3s
      ) |>
      tidyr::pivot_wider(
        names_from = month_label,
        values_from = q_mean_m3s
      )
    
    for (month_name in fluviometric_stats_month_labels) {
      if (!month_name %in% names(wide)) {
        wide[[month_name]] <- NA_real_
      }
    }
    
    annual_lookup <- annual |>
      dplyr::transmute(
        Ano = as.character(year),
        `Média` = q_mean_m3s
      )
    
    wide <- wide |>
      dplyr::left_join(annual_lookup, by = "Ano") |>
      dplyr::select(
        Ano,
        dplyr::all_of(fluviometric_stats_month_labels),
        `Média`
      ) |>
      dplyr::arrange(suppressWarnings(as.integer(Ano)))
    
    month_mean_values <- stats::setNames(
      as.list(rep(NA_real_, length(fluviometric_stats_month_labels))),
      fluviometric_stats_month_labels
    )
    
    if (nrow(regime) > 0) {
      for (i in seq_len(nrow(regime))) {
        month_mean_values[[as.character(regime$month_label[i])]] <- regime$q_mean_m3s[i]
      }
    }
    
    media_row <- tibble::as_tibble_row(
      c(
        list(Ano = "Média"),
        month_mean_values,
        list(`Média` = fluviometric_stats_qmlt(daily))
      )
    )
    
    dplyr::bind_rows(wide, media_row)
  })
  
  fluviometric_stats_monthly_volume <- reactive({
    monthly <- fluviometric_stats_monthly()
    
    if (nrow(monthly) == 0) {
      return(tibble::tibble())
    }
    
    monthly |>
      dplyr::filter(is.finite(q_mean_m3s)) |>
      dplyr::mutate(
        date = as.Date(paste0(period, "-01")),
        month_index = dplyr::row_number(),
        volume_hm3 = q_mean_m3s * days_in_period * 86400 / 1e6
      ) |>
      dplyr::arrange(date)
  })
  
  fluviometric_stats_regularization_from_monthly_volume <- function(
    monthly_volume,
    qmlt,
    scenario_label,
    demand_pct = seq(10, 100, by = 1)
  ) {
    if (
      is.null(monthly_volume) ||
      nrow(monthly_volume) == 0 ||
      !is.finite(qmlt) ||
      qmlt <= 0
    ) {
      return(tibble::tibble())
    }
    
    result <- lapply(demand_pct, function(pct) {
      demand_q_m3s <- qmlt * pct / 100
      
      demand_volume_hm3 <- demand_q_m3s *
        monthly_volume$days_in_period *
        86400 / 1e6
      
      deficit <- numeric(nrow(monthly_volume))
      previous_deficit <- 0
      
      for (i in seq_len(nrow(monthly_volume))) {
        previous_deficit <- max(
          0,
          previous_deficit + demand_volume_hm3[i] - monthly_volume$volume_hm3[i]
        )
        
        deficit[i] <- previous_deficit
      }
      
      tibble::tibble(
        cenario = scenario_label,
        demand_pct_qmlt = pct,
        demand_m3s = demand_q_m3s,
        regularization_volume_hm3 = max(deficit, na.rm = TRUE)
      )
    })
    
    dplyr::bind_rows(result)
  }
  
  fluviometric_stats_critical_5yr_monthly_volume <- reactive({
    monthly_volume <- fluviometric_stats_monthly_volume()
    dry_window <- fluviometric_stats_dry_5yr_window()
    
    if (nrow(monthly_volume) == 0 || nrow(dry_window) == 0) {
      return(tibble::tibble())
    }
    
    monthly_volume |>
      dplyr::filter(
        year >= dry_window$start_year[1],
        year <= dry_window$end_year[1]
      ) |>
      dplyr::mutate(
        month_index = dplyr::row_number()
      )
  })
  
  fluviometric_stats_regularization <- reactive({
    annual <- fluviometric_stats_annual()
    
    if (!fluviometric_stats_has_required_years(annual)) {
      return(tibble::tibble())
    }
    
    monthly_volume <- fluviometric_stats_monthly_volume()
    critical_volume <- fluviometric_stats_critical_5yr_monthly_volume()
    daily <- fluviometric_stats_daily()
    
    qmlt <- fluviometric_stats_qmlt(daily)
    
    global_curve <- fluviometric_stats_regularization_from_monthly_volume(
      monthly_volume = monthly_volume,
      qmlt = qmlt,
      scenario_label = "Série completa"
    )
    
    critical_curve <- fluviometric_stats_regularization_from_monthly_volume(
      monthly_volume = critical_volume,
      qmlt = qmlt,
      scenario_label = "5 anos mais secos"
    )
    
    dplyr::bind_rows(global_curve, critical_curve)
  })
  
  fluviometric_stats_rippl <- reactive({
    annual <- fluviometric_stats_annual()
    
    if (!fluviometric_stats_has_required_years(annual)) {
      return(tibble::tibble())
    }
    
    critical_volume <- fluviometric_stats_critical_5yr_monthly_volume()
    dry_window <- fluviometric_stats_dry_5yr_window()
    
    if (nrow(critical_volume) == 0 || nrow(dry_window) == 0) {
      return(tibble::tibble())
    }
    
    critical_volume |>
      dplyr::mutate(
        critical_period = dry_window$period_label[1],
        cumulative_volume_hm3 = cumsum(volume_hm3)
      ) |>
      dplyr::select(
        critical_period,
        month_index,
        date,
        year,
        month,
        period,
        q_mean_m3s,
        volume_hm3,
        cumulative_volume_hm3
      )
  })
  
  fluviometric_stats_draw_fdc_plot <- function(duration_data, title, subtitle, y_scale = "linear") {
    if (is.null(y_scale)) {
      y_scale <- "linear"
    }
    
    plot_data <- duration_data |>
      dplyr::filter(
        is.finite(permanence_pct),
        is.finite(discharge_m3s)
      )
    
    if (identical(y_scale, "log")) {
      plot_data <- plot_data |>
        dplyr::filter(discharge_m3s > 0)
    }
    
    if (nrow(plot_data) == 0) {
      draw_empty_plot("Sem dados suficientes para a curva de permanência.")
      return(invisible(NULL))
    }
    
    p <- ggplot2::ggplot(
      plot_data,
      ggplot2::aes(x = permanence_pct, y = discharge_m3s)
    ) +
      ggplot2::geom_line(linewidth = 0.45,
                         lineend = "round",
                         linejoin = "round") +
      ggplot2::scale_x_continuous(
        name = "Permanência (%)",
        trans = fluviometric_stats_normal_probability_trans(),
        breaks = c(1, 2, 5, 10, 20, 50, 80, 90, 95, 99),
        labels = c("1", "2", "5", "10", "20", "50", "80", "90", "95", "99"),
        limits = c(0.5, 99)
      ) +
      # ggplot2::labs(
      #   title = title,
      #   subtitle = subtitle
      # ) +
      preview_plot_theme(base_size = 4.5) +
      ggplot2::theme(
        legend.position = "none",
        plot.margin = ggplot2::margin(6, 8, 6, 8)
      )
    
    if (identical(y_scale, "log")) {
      p +
        ggplot2::scale_y_log10(
          name = "Vazão (m³/s)",
          labels = scales::label_number(decimal.mark = ",", big.mark = ".")
        )
    } else {
      p +
        ggplot2::scale_y_continuous(
          name = "Vazão (m³/s)",
          labels = scales::label_number(decimal.mark = ",", big.mark = ".")
        )
    }
  }
  
  output$fluviometric_stats_status <- renderUI({
    result <- fluviometric_acquisition_result()
    
    if (is.null(result)) {
      return(
        tags$div(
          class = "table-status empty",
          "Nenhum dado fluviométrico foi carregado. Use primeiro a aba Obtenção de dados."
        )
      )
    }
    
    daily <- fluviometric_stats_daily()
    
    if (nrow(daily) == 0 || !any(daily$has_discharge, na.rm = TRUE)) {
      return(
        tags$div(
          class = "table-status warning",
          "A sessão atual não possui vazões diárias válidas para calcular estatísticas mensais e anuais."
        )
      )
    }
    
    tags$div(
      class = "table-status available",
      "Estatísticas calculadas a partir da série diária de vazões carregada na sessão. As falhas não são preenchidas nem interpoladas."
    )
  })
  
  output$fluviometric_stats_download_controls <- renderUI({
    daily <- fluviometric_stats_daily()
    
    if (nrow(daily) == 0 || !any(daily$has_discharge, na.rm = TRUE)) {
      return(NULL)
    }
    
    tags$div(
      class = "control-card",
      tags$div(
        class = "download-button-row",
        tags$div(
          class = "download-button-item",
          downloadButton(
            outputId = "fluviometric_stats_monthly_wide_download",
            label = "Tabela mensal",
            class = "btn-primary"
          )
        ),
        tags$div(
          class = "download-button-item",
          downloadButton(
            outputId = "fluviometric_stats_annual_download",
            label = "Anuais",
            class = "btn-primary"
          )
        ),
        tags$div(
          class = "download-button-item",
          downloadButton(
            outputId = "fluviometric_stats_fdc_download",
            label = "Permanência",
            class = "btn-primary"
          )
        ),
        tags$div(
          class = "download-button-item",
          downloadButton(
            outputId = "fluviometric_stats_regularization_download",
            label = "Regularização",
            class = "btn-primary"
          )
        ),
        tags$div(
          class = "download-button-item",
          downloadButton(
            outputId = "fluviometric_stats_rippl_download",
            label = "Rippl",
            class = "btn-primary"
          )
        )
      )
    )
  })
  
  output$fluviometric_stats_summary_cards <- renderUI({
    daily <- fluviometric_stats_daily()
    monthly <- fluviometric_stats_monthly()
    annual <- fluviometric_stats_annual()
    
    if (nrow(daily) == 0 || !any(daily$has_discharge, na.rm = TRUE)) {
      return(NULL)
    }
    
    valid_daily <- daily$discharge_m3s[daily$has_discharge]
    valid_monthly <- monthly$q_mean_m3s[is.finite(monthly$q_mean_m3s)]
    valid_annual <- annual |> dplyr::filter(is.finite(q_mean_m3s))
    
    qmlt <- fluviometric_stats_qmlt(daily)
    q90_monthly <- fluviometric_stats_flow_value(valid_monthly, 90)
    q95_monthly <- fluviometric_stats_flow_value(valid_monthly, 95)
    
    dry_year_text <- "—"
    wet_year_text <- "—"
    
    if (nrow(valid_annual) > 0) {
      dry_year <- valid_annual |>
        dplyr::slice_min(q_mean_m3s, n = 1, with_ties = FALSE)
      
      wet_year <- valid_annual |>
        dplyr::slice_max(q_mean_m3s, n = 1, with_ties = FALSE)
      
      dry_year_text <- paste0(
        dry_year$year,
        " (",
        fluviometric_format_value(dry_year$q_mean_m3s),
        " m³/s)"
      )
      
      wet_year_text <- paste0(
        wet_year$year,
        " (",
        fluviometric_format_value(wet_year$q_mean_m3s),
        " m³/s)"
      )
    }
    
    dry_5yr <- fluviometric_stats_dry_5yr_window()
    wet_5yr <- fluviometric_stats_wet_5yr_window()
    
    dry_5yr_text <- "—"
    wet_5yr_text <- "—"
    
    if (nrow(dry_5yr) > 0) {
      dry_5yr_text <- paste0(
        dry_5yr$period_label[1],
        " (",
        fluviometric_format_value(dry_5yr$q_mean_5yr_m3s[1]),
        " m³/s)"
      )
    }
    
    if (nrow(wet_5yr) > 0) {
      wet_5yr_text <- paste0(
        wet_5yr$period_label[1],
        " (",
        fluviometric_format_value(wet_5yr$q_mean_5yr_m3s[1]),
        " m³/s)"
      )
    }
    
    n_period <- nrow(daily)
    n_valid <- sum(daily$has_discharge, na.rm = TRUE)
    n_months <- sum(monthly$days_with_discharge > 0, na.rm = TRUE)
    n_years <- nrow(valid_annual)
    
    tags$div(
      class = "section-card",
      tags$div(
        class = "section-header",
        tags$h3("Indicadores centrais"),
        tags$p("Síntese da série diária e das agregações mensal e anual.")
      ),
      tags$div(
        class = "overview-metric-grid fluviometric-metric-grid",
        overview_metric("QMLT", paste0(fluviometric_format_value(qmlt), " m³/s")),
        overview_metric("Q90 mensal", paste0(fluviometric_format_value(q90_monthly), " m³/s")),
        overview_metric("Q95 mensal", paste0(fluviometric_format_value(q95_monthly), " m³/s")),
        overview_metric("Dias com vazão", fluviometric_consistency_count_pct(n_valid, n_period)),
        overview_metric("Meses com dados", fluviometric_format_count(n_months)),
        overview_metric("Anos com dados", fluviometric_format_count(n_years)),
        overview_metric("Ano mais seco", dry_year_text),
        overview_metric("Ano mais úmido", wet_year_text),
        overview_metric("5 anos mais secos", dry_5yr_text),
        overview_metric("5 anos mais úmidos", wet_5yr_text),
        overview_metric("Vazão mínima diária", paste0(fluviometric_format_value(fluviometric_stats_safe_min(valid_daily)), " m³/s")),
        overview_metric("Vazão máxima diária", paste0(fluviometric_format_value(fluviometric_stats_safe_max(valid_daily)), " m³/s"))
      )
    )
  })
  
  output$fluviometric_stats_annual_mean_plot <- renderPlot({
    plot_data <- fluviometric_stats_annual() |>
      dplyr::filter(is.finite(q_mean_m3s))
    
    if (nrow(plot_data) == 0) {
      draw_empty_plot("Sem dados suficientes para calcular vazões médias anuais.")
      return(invisible(NULL))
    }
    
    ggplot2::ggplot(
      plot_data,
      ggplot2::aes(x = year, y = q_mean_m3s)
    ) +
      ggplot2::geom_line(
        ggplot2::aes(group = 1),
        linewidth = 0.45,
        lineend = "round",
        linejoin = "round"
      ) +
      ggplot2::geom_point(size = 1.5) +
      ggplot2::scale_x_continuous(
        name = "Ano civil",
        breaks = scales::pretty_breaks(n = 7)
      ) +
      ggplot2::scale_y_continuous(
        name = "Q média anual (m³/s)",
        labels = scales::label_number(decimal.mark = ",", big.mark = ".")
      ) +
      # ggplot2::labs(
      #   title = "Q média anual × ano",
      #   subtitle = "Média anual calculada a partir dos dias com vazão válida."
      # ) +
      preview_plot_theme(base_size = 4.5) +
      ggplot2::theme(
        legend.position = "none",
        plot.margin = ggplot2::margin(6, 8, 6, 8)
      )
  }, res = 144)
  
  output$fluviometric_stats_monthly_regime_plot <- renderPlot({
    plot_data <- fluviometric_stats_monthly_regime() |>
      dplyr::filter(is.finite(q_mean_m3s))
    
    if (nrow(plot_data) == 0) {
      draw_empty_plot("Sem dados suficientes para calcular o regime mensal.")
      return(invisible(NULL))
    }
    
    ggplot2::ggplot(
      plot_data,
      ggplot2::aes(x = month_label, y = q_mean_m3s)
    ) +
      # ggplot2::geom_col(width = 0.75) +
      ggplot2::geom_line(
        ggplot2::aes(group = 1),
        linewidth = 0.45,
        lineend = "round",
        linejoin = "round"
      ) +
      ggplot2::geom_point(size = 1.5) +
      ggplot2::scale_x_discrete(name = "Mês") +
      ggplot2::scale_y_continuous(
        name = "Q média mensal (m³/s)",
        labels = scales::label_number(decimal.mark = ",", big.mark = ".")
      ) +
      # ggplot2::labs(
      #   title = "Q média mensal × mês",
      #   subtitle = "Média de todos os janeiros, fevereiros, ..., dezembros."
      # ) +
      preview_plot_theme(base_size = 4.5) +
      ggplot2::theme(
        legend.position = "none",
        plot.margin = ggplot2::margin(6, 8, 6, 8)
      )
  }, res = 144)
  
  output$fluviometric_stats_fdc_annual_plot <- renderPlot({
    annual <- fluviometric_stats_annual()
    
    if (!fluviometric_stats_has_required_years(annual)) {
      draw_empty_plot("Curva de permanência disponível apenas para postos com pelo menos 5 anos de dados.")
      return(invisible(NULL))
    }
    
    duration_data <- fluviometric_stats_flow_duration(annual$q_mean_m3s)
    
    fluviometric_stats_draw_fdc_plot(
      duration_data = duration_data,
      # title = "Curva de permanência anual",
      # subtitle = "Baseada nas vazões médias anuais.",
      y_scale = input$fluviometric_stats_fdc_y_scale
    )
  }, res = 144)
  
  output$fluviometric_stats_fdc_monthly_plot <- renderPlot({
    annual <- fluviometric_stats_annual()
    
    if (!fluviometric_stats_has_required_years(annual)) {
      draw_empty_plot("Curva de permanência disponível apenas para postos com pelo menos 5 anos de dados.")
      return(invisible(NULL))
    }
    
    monthly_values <- fluviometric_stats_monthly()$q_mean_m3s
    duration_data <- fluviometric_stats_flow_duration(monthly_values)
    
    fluviometric_stats_draw_fdc_plot(
      duration_data = duration_data,
      # title = "Curva de permanência mensal",
      # subtitle = "Baseada nas vazões médias mensais.",
      y_scale = input$fluviometric_stats_fdc_y_scale
    )
  }, res = 144)
  
  output$fluviometric_stats_regularization_plot <- renderPlot({
    plot_data <- fluviometric_stats_regularization()
    
    if (nrow(plot_data) == 0) {
      draw_empty_plot("Curva de regularização disponível apenas para postos com pelo menos 5 anos de dados.")
      return(invisible(NULL))
    }
    
    ggplot2::ggplot(
      plot_data,
      ggplot2::aes(
        x = demand_pct_qmlt,
        y = regularization_volume_hm3,
        color = cenario,
        group = cenario
      )
    ) +
      ggplot2::geom_line(linewidth = 0.45,
                         lineend = "round",
                         linejoin = "round") +
      ggplot2::scale_x_continuous(
        name = "Demanda (% da QMLT)",
        breaks = seq(10, 100, by = 10)
      ) +
      ggplot2::scale_y_continuous(
        name = "Volume de regularização (Hm³)",
        labels = scales::label_number(decimal.mark = ",", big.mark = ".")
      ) +
      ggplot2::scale_color_manual(values = c('black', '#2b8cbe')) +
      ggplot2::labs(
        # title = "Curva de regularização",
        # subtitle = "Série completa e janela crítica de 5 anos secos.",
        color = NULL
      ) +
      preview_plot_theme(base_size = 4.5) +
      ggplot2::theme(
        legend.position = "bottom",
        plot.margin = ggplot2::margin(6, 8, 6, 8)
      )
  }, res = 144)
  
  output$fluviometric_stats_rippl_plot <- renderPlot({
    plot_data <- fluviometric_stats_rippl()
    
    if (nrow(plot_data) == 0) {
      draw_empty_plot("Diagrama de Rippl disponível apenas para postos com pelo menos 5 anos de dados.")
      return(invisible(NULL))
    }
    
    critical_period <- unique(plot_data$critical_period)
    critical_period <- critical_period[!is.na(critical_period)][1]
    
    ggplot2::ggplot(
      plot_data,
      ggplot2::aes(x = month_index, y = cumulative_volume_hm3)
    ) +
      ggplot2::geom_line(linewidth = 0.45,
                         lineend = "round",
                         linejoin = "round") +
      ggplot2::scale_x_continuous(
        name = "Mês desde o início da janela crítica",
        breaks = scales::pretty_breaks(n = 8)
      ) +
      ggplot2::scale_y_continuous(
        name = "Volume acumulado (Hm³)",
        labels = scales::label_number(decimal.mark = ",", big.mark = ".")
      ) +
      ggplot2::labs(
        # title = "Diagrama de Rippl",
        caption = paste0("Volume mensal acumulado nos 5 anos mais secos: ", critical_period, ".")
      ) +
      preview_plot_theme(base_size = 4.5) +
      ggplot2::theme(
        legend.position = "none",
        plot.margin = ggplot2::margin(6, 8, 6, 8),
        plot.caption = element_text(size = 5.5)
      )
  }, res = 144)
  
  output$fluviometric_stats_monthly_wide_table <- DT::renderDT({
    table_data <- fluviometric_stats_monthly_wide()
    
    if (nrow(table_data) == 0) {
      return(
        DT::datatable(
          tibble::tibble(Mensagem = "Nenhuma tabela mensal disponível."),
          rownames = FALSE,
          options = list(dom = "t")
        )
      )
    }
    
    numeric_columns <- setdiff(names(table_data), "Ano")
    
    table_data[numeric_columns] <- lapply(
      table_data[numeric_columns],
      function(x) round(as.numeric(x), 2)
    )
    
    DT::datatable(
      table_data,
      rownames = FALSE,
      class = "compact stripe hover",
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        order = list(list(0, "asc"))
      )
    )
  })
  
  output$fluviometric_stats_monthly_wide_download <- downloadHandler(
    filename = function() {
      paste0(
        "vazoes_medias_mensais_",
        as.character(selected_code()),
        "_",
        format(Sys.Date(), "%Y%m%d"),
        ".csv"
      )
    },
    content = function(file) {
      table_data <- fluviometric_stats_monthly_wide()
      
      if (nrow(table_data) == 0) {
        table_data <- tibble::tibble(
          mensagem = "Nenhuma tabela mensal disponível."
        )
      }
      
      fluviometric_stats_write_csv_bom(table_data, file)
    }
  )
  
  output$fluviometric_stats_annual_download <- downloadHandler(
    filename = function() {
      paste0(
        "vazoes_medias_anuais_",
        as.character(selected_code()),
        "_",
        format(Sys.Date(), "%Y%m%d"),
        ".csv"
      )
    },
    content = function(file) {
      table_data <- fluviometric_stats_annual() |>
        dplyr::transmute(
          ano = year,
          q_media_m3s = q_mean_m3s,
          dias_com_vazao = days_with_discharge,
          falhas_pct = failure_pct
        )
      
      if (nrow(table_data) == 0) {
        table_data <- tibble::tibble(
          mensagem = "Nenhuma estatística anual disponível."
        )
      }
      
      fluviometric_stats_write_csv_bom(table_data, file, digits = 3)
    }
  )
  
  output$fluviometric_stats_fdc_download <- downloadHandler(
    filename = function() {
      paste0(
        "curvas_permanencia_",
        as.character(selected_code()),
        "_",
        format(Sys.Date(), "%Y%m%d"),
        ".csv"
      )
    },
    content = function(file) {
      annual <- fluviometric_stats_annual()
      
      if (!fluviometric_stats_has_required_years(annual)) {
        table_data <- tibble::tibble(
          mensagem = "Curvas de permanência disponíveis apenas para postos com pelo menos 5 anos de dados."
        )
        
        fluviometric_stats_write_csv_bom(table_data, file)
        return(invisible(NULL))
      }
      
      export_probabilities <- 1:100
      
      annual_fdc <- fluviometric_stats_flow_duration(
        annual$q_mean_m3s,
        target_probabilities = export_probabilities
      ) |>
        dplyr::mutate(escala = "Anual")
      
      monthly_fdc <- fluviometric_stats_flow_duration(
        fluviometric_stats_monthly()$q_mean_m3s,
        target_probabilities = export_probabilities
      ) |>
        dplyr::mutate(escala = "Mensal")
      
      table_data <- dplyr::bind_rows(annual_fdc, monthly_fdc) |>
        dplyr::transmute(
          escala,
          permanencia_pct = permanence_pct,
          vazao_m3s = discharge_m3s
        )
      
      fluviometric_stats_write_csv_bom(table_data, file, digits = 3)
    }
  )
  
  output$fluviometric_stats_regularization_download <- downloadHandler(
    filename = function() {
      paste0(
        "curva_regularizacao_",
        as.character(selected_code()),
        "_",
        format(Sys.Date(), "%Y%m%d"),
        ".csv"
      )
    },
    content = function(file) {
      table_data <- fluviometric_stats_regularization() |>
        dplyr::transmute(
          cenario,
          demanda_pct_qmlt = demand_pct_qmlt,
          demanda_m3s = demand_m3s,
          volume_regularizacao_hm3 = regularization_volume_hm3
        )
      
      if (nrow(table_data) == 0) {
        table_data <- tibble::tibble(
          mensagem = "Curva de regularização disponível apenas para postos com pelo menos 5 anos de dados."
        )
      }
      
      fluviometric_stats_write_csv_bom(table_data, file, digits = 3)
    }
  )
  
  output$fluviometric_stats_rippl_download <- downloadHandler(
    filename = function() {
      paste0(
        "diagrama_rippl_",
        as.character(selected_code()),
        "_",
        format(Sys.Date(), "%Y%m%d"),
        ".csv"
      )
    },
    content = function(file) {
      table_data <- fluviometric_stats_rippl() |>
        dplyr::transmute(
          periodo_critico = critical_period,
          mes_indice = month_index,
          periodo = period,
          ano = year,
          mes = month,
          q_media_mensal_m3s = q_mean_m3s,
          volume_mensal_hm3 = volume_hm3,
          volume_acumulado_hm3 = cumulative_volume_hm3
        )
      
      if (nrow(table_data) == 0) {
        table_data <- tibble::tibble(
          mensagem = "Diagrama de Rippl disponível apenas para postos com pelo menos 5 anos de dados."
        )
      }
      
      fluviometric_stats_write_csv_bom(table_data, file, digits = 3)
    }
  )
  

