# ============================================================
# server_04_flu_consistency.R
# Purpose: Fluviometric coverage and stage-discharge consistency analysis.
# ============================================================
# BEGIN ORIGINAL BODY
  # ------------------------------------------------------------
  # Fluviometric tab: consistency analysis
  # ------------------------------------------------------------
  
  fluviometric_consistency_format_percent <- function(x, digits = 1) {
    if (length(x) == 0 || is.na(x) || !is.finite(x)) {
      return("—")
    }
    
    formatC(
      as.numeric(x),
      format = "f",
      digits = digits,
      decimal.mark = ",",
      big.mark = "."
    )
  }
  
  fluviometric_consistency_count_pct <- function(n, denominator, digits = 1) {
    n <- suppressWarnings(as.numeric(n))
    denominator <- suppressWarnings(as.numeric(denominator))
    
    if (length(n) == 0 || is.na(n)) {
      n <- 0
    }
    
    count_text <- fluviometric_format_count(n)
    
    if (length(denominator) == 0 || is.na(denominator) || denominator <= 0) {
      return(paste0(count_text, " (—)"))
    }
    
    pct <- 100 * n / denominator
    
    paste0(
      count_text,
      " (",
      fluviometric_consistency_format_percent(pct, digits),
      "%)"
    )
  }
  
  fluviometric_consistency_longest_run <- function(flag) {
    flag <- as.logical(flag)
    flag[is.na(flag)] <- FALSE
    
    if (!any(flag)) {
      return(0L)
    }
    
    runs <- rle(flag)
    max(runs$lengths[runs$values])
  }
  
  fluviometric_consistency_safe_median <- function(x) {
    x <- x[is.finite(x)]
    
    if (length(x) == 0) {
      return(NA_real_)
    }
    
    stats::median(x, na.rm = TRUE)
  }
  
  fluviometric_consistency_safe_quantile <- function(x, probability = 0.95) {
    x <- x[is.finite(x)]
    
    if (length(x) == 0) {
      return(NA_real_)
    }
    
    as.numeric(stats::quantile(x, probs = probability, na.rm = TRUE, names = FALSE))
  }
  
  fluviometric_prepare_discharge_daily <- function(data) {
    if (is.null(data) || nrow(data) == 0) {
      return(tibble::tibble())
    }
    
    data |>
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
      dplyr::transmute(
        date,
        discharge_m3s,
        discharge_status = as.character(source_status),
        discharge_consistency_level = as.character(consistency_level)
      )
  }
  
  fluviometric_prepare_stage_daily <- function(data) {
    if (is.null(data) || nrow(data) == 0) {
      return(tibble::tibble())
    }
    
    data |>
      dplyr::mutate(
        date = as.Date(date),
        stage_cm = as.numeric(value),
        consistency_priority = dplyr::case_when(
          as.character(consistency_level) == "2" ~ 1L,
          as.character(consistency_level) == "1" ~ 2L,
          TRUE ~ 3L
        ),
        value_priority = dplyr::if_else(is.na(stage_cm), 2L, 1L)
      ) |>
      dplyr::filter(!is.na(date)) |>
      dplyr::arrange(date, consistency_priority, value_priority) |>
      dplyr::group_by(date) |>
      dplyr::slice(1) |>
      dplyr::ungroup() |>
      dplyr::transmute(
        date,
        stage_cm,
        stage_status = as.character(source_status),
        stage_consistency_level = as.character(consistency_level)
      )
  }
  
  fluviometric_prepare_rating_curve_segments <- function(curves) {
    if (is.null(curves) || nrow(curves) == 0) {
      return(tibble::tibble())
    }
    
    curves <- dplyr::as_tibble(curves)
    
    if (!"rating_curve_id" %in% names(curves)) {
      curves$rating_curve_id <- as.character(seq_len(nrow(curves)))
    }
    
    if (!"rating_curve_segment_id" %in% names(curves)) {
      curves$rating_curve_segment_id <- paste0(curves$rating_curve_id, "_", seq_len(nrow(curves)))
    }
    
    if (!"segment_number" %in% names(curves)) {
      curves$segment_number <- seq_len(nrow(curves))
    }
    
    if (!"valid_from" %in% names(curves)) {
      curves$valid_from <- NA
    }
    
    if (!"valid_to" %in% names(curves)) {
      curves$valid_to <- NA
    }
    
    for (field in c("stage_min_cm", "stage_max_cm", "coefficient_a", "coefficient_h0", "coefficient_n")) {
      if (!field %in% names(curves)) {
        curves[[field]] <- NA_real_
      }
    }
    
    curves |>
      dplyr::mutate(
        rating_curve_id = as.character(rating_curve_id),
        rating_curve_segment_id = as.character(rating_curve_segment_id),
        segment_number = suppressWarnings(as.integer(segment_number)),
        valid_from = as.Date(parse_app_datetime(valid_from)),
        valid_to = as.Date(parse_app_datetime(valid_to)),
        stage_min_cm = as_numeric_app(stage_min_cm),
        stage_max_cm = as_numeric_app(stage_max_cm),
        coefficient_a = as_numeric_app(coefficient_a),
        coefficient_h0 = as_numeric_app(coefficient_h0),
        coefficient_n = as_numeric_app(coefficient_n)
      ) |>
      dplyr::filter(!is.na(rating_curve_id))
  }
  
  fluviometric_match_daily_rating_curves <- function(daily, curves) {
    if (is.null(daily) || nrow(daily) == 0) {
      return(tibble::tibble())
    }
    
    daily <- daily |>
      dplyr::mutate(
        obs_id = dplyr::row_number(),
        has_discharge = !is.na(discharge_m3s),
        has_stage = !is.na(stage_cm)
      )
    
    if (is.null(curves) || nrow(curves) == 0) {
      return(
        daily |>
          dplyr::mutate(
            n_valid_date_curve_segments = 0L,
            n_valid_date_rating_curves = 0L,
            n_applicable_curve_segments = 0L,
            n_applicable_rating_curves = 0L,
            min_valid_stage_cm = NA_real_,
            max_valid_stage_cm = NA_real_,
            rating_curve_id = NA_character_,
            rating_curve_segment_id = NA_character_,
            selected_curve_valid_from = as.Date(NA),
            selected_curve_valid_to = as.Date(NA),
            selected_stage_min_cm = NA_real_,
            selected_stage_max_cm = NA_real_,
            q_generated_m3s = NA_real_,
            q_generation_error = FALSE,
            has_valid_date_curve = FALSE,
            stage_below_curve_range = FALSE,
            stage_above_curve_range = FALSE,
            stage_outside_curve_range = FALSE,
            discharge_without_stage = has_discharge & !has_stage,
            discharge_without_curve = has_discharge,
            discharge_before_first_curve = FALSE,
            discharge_after_last_curve = FALSE,
            complete_hq_curve_day = FALSE,
            ambiguous_curve_day = FALSE,
            q_difference_evaluable = FALSE,
            relative_error = NA_real_,
            abs_relative_error = NA_real_,
            q_diff_gt_5 = FALSE,
            q_diff_gt_10 = FALSE,
            q_diff_gt_25 = FALSE,
            discharge_nonpositive = has_discharge & discharge_m3s <= 0,
            stage_nonpositive = has_stage & stage_cm <= 0
          )
      )
    }
    
    curve_starts <- curves$valid_from[!is.na(curves$valid_from)]
    curve_ends <- curves$valid_to[!is.na(curves$valid_to)]
    has_open_ended_curve <- any(is.na(curves$valid_to))
    
    first_curve_start <- if (length(curve_starts) > 0) min(curve_starts) else as.Date(NA)
    last_curve_end <- if (!has_open_ended_curve && length(curve_ends) > 0) max(curve_ends) else as.Date(NA)
    
    obs <- daily |>
      dplyr::select(
        obs_id,
        date,
        discharge_m3s,
        stage_cm,
        has_discharge,
        has_stage
      )
    
    curve_cols <- curves |>
      dplyr::select(
        rating_curve_id,
        rating_curve_segment_id,
        segment_number,
        valid_from,
        valid_to,
        stage_min_cm,
        stage_max_cm,
        coefficient_a,
        coefficient_h0,
        coefficient_n
      )
    
    candidates <- merge(obs, curve_cols, by = NULL)
    
    candidates <- candidates |>
      dplyr::mutate(
        date_in_curve_period =
          (is.na(valid_from) | date >= valid_from) &
          (is.na(valid_to) | date <= valid_to),
        stage_in_curve_range =
          has_stage &
          is.finite(stage_cm) &
          is.finite(stage_min_cm) &
          is.finite(stage_max_cm) &
          stage_cm >= stage_min_cm &
          stage_cm <= stage_max_cm
      )
    
    valid_date_candidates <- candidates |>
      dplyr::filter(date_in_curve_period)
    
    if (nrow(valid_date_candidates) > 0) {
      date_summary <- valid_date_candidates |>
        dplyr::group_by(obs_id) |>
        dplyr::summarise(
          n_valid_date_curve_segments = dplyr::n_distinct(rating_curve_segment_id),
          n_valid_date_rating_curves = dplyr::n_distinct(rating_curve_id),
          min_valid_stage_cm = ifelse(
            any(is.finite(stage_min_cm)),
            min(stage_min_cm[is.finite(stage_min_cm)], na.rm = TRUE),
            NA_real_
          ),
          max_valid_stage_cm = ifelse(
            any(is.finite(stage_max_cm)),
            max(stage_max_cm[is.finite(stage_max_cm)], na.rm = TRUE),
            NA_real_
          ),
          .groups = "drop"
        )
    } else {
      date_summary <- tibble::tibble(
        obs_id = integer(),
        n_valid_date_curve_segments = integer(),
        n_valid_date_rating_curves = integer(),
        min_valid_stage_cm = numeric(),
        max_valid_stage_cm = numeric()
      )
    }
    
    applicable_candidates <- valid_date_candidates |>
      dplyr::filter(stage_in_curve_range) |>
      dplyr::mutate(
        effective_stage_m = stage_cm / 100 - coefficient_h0,
        q_generated_m3s = dplyr::if_else(
          is.finite(effective_stage_m) &
            effective_stage_m > 0 &
            is.finite(coefficient_a) &
            is.finite(coefficient_n),
          coefficient_a * (effective_stage_m ^ coefficient_n),
          NA_real_
        ),
        q_generation_error =
          is.na(q_generated_m3s) |
          !is.finite(q_generated_m3s) |
          q_generated_m3s <= 0
      )
    
    if (nrow(applicable_candidates) > 0) {
      applicable_summary <- applicable_candidates |>
        dplyr::group_by(obs_id) |>
        dplyr::summarise(
          n_applicable_curve_segments = dplyr::n_distinct(rating_curve_segment_id),
          n_applicable_rating_curves = dplyr::n_distinct(rating_curve_id),
          .groups = "drop"
        )
      
      best_match <- applicable_candidates |>
        dplyr::arrange(
          obs_id,
          dplyr::desc(valid_from),
          rating_curve_id,
          segment_number
        ) |>
        dplyr::group_by(obs_id) |>
        dplyr::slice(1) |>
        dplyr::ungroup() |>
        dplyr::transmute(
          obs_id,
          rating_curve_id,
          rating_curve_segment_id,
          selected_curve_valid_from = valid_from,
          selected_curve_valid_to = valid_to,
          selected_stage_min_cm = stage_min_cm,
          selected_stage_max_cm = stage_max_cm,
          q_generated_m3s,
          q_generation_error
        )
    } else {
      applicable_summary <- tibble::tibble(
        obs_id = integer(),
        n_applicable_curve_segments = integer(),
        n_applicable_rating_curves = integer()
      )
      
      best_match <- tibble::tibble(
        obs_id = integer(),
        rating_curve_id = character(),
        rating_curve_segment_id = character(),
        selected_curve_valid_from = as.Date(character()),
        selected_curve_valid_to = as.Date(character()),
        selected_stage_min_cm = numeric(),
        selected_stage_max_cm = numeric(),
        q_generated_m3s = numeric(),
        q_generation_error = logical()
      )
    }
    
    out <- daily |>
      dplyr::left_join(date_summary, by = "obs_id") |>
      dplyr::left_join(applicable_summary, by = "obs_id") |>
      dplyr::left_join(best_match, by = "obs_id")
    
    for (field in c(
      "n_valid_date_curve_segments",
      "n_valid_date_rating_curves",
      "n_applicable_curve_segments",
      "n_applicable_rating_curves"
    )) {
      out[[field]][is.na(out[[field]])] <- 0L
    }
    
    out |>
      dplyr::mutate(
        q_generation_error = dplyr::coalesce(q_generation_error, FALSE),
        has_valid_date_curve = n_valid_date_curve_segments > 0,
        stage_below_curve_range =
          has_stage &
          has_valid_date_curve &
          is.finite(min_valid_stage_cm) &
          stage_cm < min_valid_stage_cm,
        stage_above_curve_range =
          has_stage &
          has_valid_date_curve &
          is.finite(max_valid_stage_cm) &
          stage_cm > max_valid_stage_cm,
        stage_outside_curve_range =
          stage_below_curve_range |
          stage_above_curve_range |
          (has_stage & has_valid_date_curve & n_applicable_curve_segments == 0),
        discharge_without_stage = has_discharge & !has_stage,
        discharge_without_curve = has_discharge & !has_valid_date_curve,
        discharge_before_first_curve =
          has_discharge &
          !is.na(first_curve_start) &
          date < first_curve_start,
        discharge_after_last_curve =
          has_discharge &
          !is.na(last_curve_end) &
          date > last_curve_end,
        complete_hq_curve_day =
          has_discharge &
          has_stage &
          n_applicable_curve_segments > 0,
        ambiguous_curve_day =
          has_stage &
          n_applicable_curve_segments > 1,
        q_difference_evaluable =
          has_discharge &
          is.finite(discharge_m3s) &
          discharge_m3s > 0 &
          is.finite(q_generated_m3s) &
          q_generated_m3s > 0,
        relative_error = dplyr::if_else(
          q_difference_evaluable,
          (q_generated_m3s - discharge_m3s) / discharge_m3s,
          NA_real_
        ),
        abs_relative_error = abs(relative_error),
        q_diff_gt_5 = q_difference_evaluable & abs_relative_error > 0.05,
        q_diff_gt_10 = q_difference_evaluable & abs_relative_error > 0.10,
        q_diff_gt_25 = q_difference_evaluable & abs_relative_error > 0.25,
        discharge_nonpositive = has_discharge & discharge_m3s <= 0,
        stage_nonpositive = has_stage & stage_cm <= 0
      )
  }
  
  fluviometric_consistency_daily <- reactive({
    result <- fluviometric_acquisition_result()
    
    if (is.null(result)) {
      return(tibble::tibble())
    }
    
    discharge <- fluviometric_prepare_discharge_daily(result$discharge)
    stage <- fluviometric_prepare_stage_daily(result$stage)
    
    all_dates <- c(discharge$date, stage$date)
    all_dates <- all_dates[!is.na(all_dates)]
    
    if (length(all_dates) == 0) {
      return(tibble::tibble())
    }
    
    daily <- tibble::tibble(
      date = seq.Date(
        min(all_dates, na.rm = TRUE),
        max(all_dates, na.rm = TRUE),
        by = "day"
      )
    ) |>
      dplyr::left_join(discharge, by = "date") |>
      dplyr::left_join(stage, by = "date") |>
      dplyr::arrange(date)
    
    curves <- tryCatch(
      selected_rating_curves(),
      error = function(e) NULL
    )
    
    curves <- fluviometric_prepare_rating_curve_segments(curves)
    
    fluviometric_match_daily_rating_curves(daily, curves)
  })
  
  fluviometric_consistency_issue_details <- reactive({
    daily <- fluviometric_consistency_daily()
    result <- fluviometric_acquisition_result()
    
    if (is.null(result) || nrow(daily) == 0) {
      return(tibble::tibble())
    }
    
    station_code_text <- if (!is.null(result$station_codes)) {
      paste(result$station_codes, collapse = ", ")
    } else {
      as.character(selected_code())
    }
    
    base <- daily |>
      dplyr::mutate(
        station_code = station_code_text,
        relative_error_pct = 100 * relative_error,
        abs_relative_error_pct = 100 * abs_relative_error
      )
    
    make_issue <- function(flag, issue_group, issue_code, issue_label, issue_description) {
      flag <- as.logical(flag)
      flag[is.na(flag)] <- FALSE
      
      if (!any(flag)) {
        return(tibble::tibble())
      }
      
      base[flag, , drop = FALSE] |>
        dplyr::transmute(
          data = date,
          codigo_estacao = station_code,
          grupo = issue_group,
          codigo_ocorrencia = issue_code,
          ocorrencia = issue_label,
          descricao = issue_description,
          vazao_m3s = discharge_m3s,
          cota_cm = stage_cm,
          curva_chave = rating_curve_id,
          ramo_curva = rating_curve_segment_id,
          cota_min_curva_cm = selected_stage_min_cm,
          cota_max_curva_cm = selected_stage_max_cm,
          q_gerada_m3s = q_generated_m3s,
          erro_relativo_pct = relative_error_pct,
          erro_absoluto_relativo_pct = abs_relative_error_pct,
          n_curvas_validas_na_data = n_valid_date_curve_segments,
          n_curvas_aplicaveis = n_applicable_curve_segments,
          status_vazao = discharge_status,
          status_cota = stage_status,
          nivel_consistencia_vazao = discharge_consistency_level,
          nivel_consistencia_cota = stage_consistency_level
        )
    }
    
    issue_table <- dplyr::bind_rows(
      make_issue(
        daily$has_discharge & !daily$has_stage,
        "Cobertura dos dados diários",
        "vazao_sem_cota",
        "Vazão sem cota",
        "Há vazão média diária, mas não há cota média diária correspondente na mesma data."
      ),
      make_issue(
        daily$has_stage & !daily$has_discharge,
        "Cobertura dos dados diários",
        "cota_sem_vazao",
        "Cota sem vazão",
        "Há cota média diária, mas não há vazão média diária correspondente na mesma data."
      ),
      make_issue(
        !daily$has_discharge,
        "Cobertura dos dados diários",
        "falha_vazao",
        "Falha em vazão",
        "Não há vazão média diária na data avaliada."
      ),
      make_issue(
        !daily$has_stage,
        "Cobertura dos dados diários",
        "falha_cota",
        "Falha em cota",
        "Não há cota média diária na data avaliada."
      ),
      make_issue(
        !daily$has_discharge & !daily$has_stage,
        "Cobertura dos dados diários",
        "falha_simultanea_cota_vazao",
        "Falha simultânea de cota e vazão",
        "Não há vazão nem cota média diária na data avaliada."
      ),
      make_issue(
        daily$discharge_without_curve,
        "Cobertura por curvas-chave",
        "vazao_sem_curva_valida",
        "Vazão sem curva-chave válida",
        "Há vazão diária, mas não há curva-chave válida para a data da observação."
      ),
      make_issue(
        daily$discharge_before_first_curve,
        "Cobertura por curvas-chave",
        "vazao_antes_primeira_curva",
        "Vazão antes da primeira curva-chave",
        "A vazão ocorre antes da data inicial da primeira curva-chave disponível."
      ),
      make_issue(
        daily$discharge_after_last_curve,
        "Cobertura por curvas-chave",
        "vazao_apos_ultima_curva",
        "Vazão após a última curva-chave",
        "A vazão ocorre após a data final da última curva-chave disponível."
      ),
      make_issue(
        daily$stage_below_curve_range,
        "Consistência cota–vazão–curva-chave",
        "cota_abaixo_faixa_curva",
        "Cota abaixo da faixa da curva",
        "A cota diária está abaixo da cota mínima da curva-chave válida na data."
      ),
      make_issue(
        daily$stage_above_curve_range,
        "Consistência cota–vazão–curva-chave",
        "cota_acima_faixa_curva",
        "Cota acima da faixa da curva",
        "A cota diária está acima da cota máxima da curva-chave válida na data."
      ),
      make_issue(
        daily$ambiguous_curve_day,
        "Cobertura por curvas-chave",
        "curvas_ambiguas",
        "Mais de uma curva aplicável",
        "Mais de um ramo ou curva-chave é aplicável à mesma data e cota."
      ),
      make_issue(
        daily$q_generation_error,
        "Consistência cota–vazão–curva-chave",
        "erro_geracao_q",
        "Erro na geração da vazão",
        "Há cota e curva aplicável, mas não foi possível gerar uma vazão válida pela equação da curva-chave."
      ),
      make_issue(
        daily$q_diff_gt_5,
        "Consistência cota–vazão–curva-chave",
        "diferenca_q_gt_5",
        "Diferença entre Q gerada e Q disponibilizada maior que 5%",
        "A vazão gerada pela curva-chave difere da vazão disponibilizada em mais de 5%."
      ),
      make_issue(
        daily$q_diff_gt_10,
        "Consistência cota–vazão–curva-chave",
        "diferenca_q_gt_10",
        "Diferença entre Q gerada e Q disponibilizada maior que 10%",
        "A vazão gerada pela curva-chave difere da vazão disponibilizada em mais de 10%."
      ),
      make_issue(
        daily$q_diff_gt_25,
        "Consistência cota–vazão–curva-chave",
        "diferenca_q_gt_25",
        "Diferença entre Q gerada e Q disponibilizada maior que 25%",
        "A vazão gerada pela curva-chave difere da vazão disponibilizada em mais de 25%."
      ),
      make_issue(
        daily$discharge_nonpositive,
        "Consistência dos valores diários",
        "vazao_menor_igual_zero",
        "Vazão menor ou igual a zero",
        "A vazão diária é menor ou igual a zero."
      ),
      make_issue(
        daily$stage_nonpositive,
        "Consistência dos valores diários",
        "cota_menor_igual_zero",
        "Cota menor ou igual a zero",
        "A cota diária é menor ou igual a zero."
      )
    )
    
    if (nrow(issue_table) == 0) {
      return(tibble::tibble())
    }
    
    issue_table |>
      dplyr::arrange(data, grupo, codigo_ocorrencia)
  })
  
  fluviometric_collapsible_section <- function(title, subtitle = NULL, ..., open = TRUE) {
    tags$details(
      class = "details-card details-card-main",
      open = if (isTRUE(open)) NA else NULL,
      tags$summary(title),
      tags$div(
        class = "section-header",
        if (!is.null(subtitle)) {
          tags$p(subtitle)
        }
      ),
      ...
    )
  }
  
  fluviometric_consistency_monthly_gaps <- function(daily, variable = "discharge") {
    if (is.null(daily) || nrow(daily) == 0) {
      return(tibble::tibble())
    }
    
    has_variable <- if (identical(variable, "stage")) {
      daily$has_stage
    } else {
      daily$has_discharge
    }
    
    daily |>
      dplyr::mutate(
        year = lubridate::year(date),
        month = lubridate::month(date),
        has_value = has_variable
      ) |>
      dplyr::group_by(year, month) |>
      dplyr::summarise(
        days_expected = dplyr::n(),
        days_observed = sum(has_value, na.rm = TRUE),
        days_missing = days_expected - days_observed,
        failure_pct = 100 * days_missing / days_expected,
        .groups = "drop"
      ) |>
      dplyr::mutate(
        failure_class = dplyr::case_when(
          failure_pct == 100 ~ "100%",
          failure_pct >= 75 ~ "75–<100%",
          failure_pct >= 50 ~ "50–<75%",
          failure_pct >= 25 ~ "25–<50%",
          failure_pct > 0 ~ "0–<25%",
          TRUE ~ "0%"
        ),
        failure_class = factor(
          failure_class,
          levels = c("100%", "75–<100%", "50–<75%", "25–<50%", "0–<25%", "0%")
        )
      )
  }
  
  fluviometric_consistency_gap_plot <- function(monthly_data, title, subtitle = NULL) {
    if (is.null(monthly_data) || nrow(monthly_data) == 0) {
      draw_empty_plot("Sem dados suficientes para calcular falhas mensais.")
      return(invisible(NULL))
    }
    
    failure_colors <- c(
      "100%" = "#d53e4f",
      "75–<100%" = "#fc8d59",
      "50–<75%" = "#fee08b",
      "25–<50%" = "#e6f598",
      "0–<25%" = "#99d594",
      "0%" = "#3288bd"
    )
    
    failure_labels <- c(
      "100%" = "100",
      "75–<100%" = "75–99",
      "50–<75%" = "50–74",
      "25–<50%" = "25–49",
      "0–<25%" = "1–24",
      "0%" = "0"
    )
    
    legend_levels <- names(failure_colors)
    
    monthly_data <- monthly_data |>
      dplyr::mutate(
        failure_class = factor(
          as.character(failure_class),
          levels = legend_levels
        )
      )
    
    legend_dummy <- tibble::tibble(
      year = min(monthly_data$year, na.rm = TRUE),
      month = 1,
      failure_class = factor(legend_levels, levels = legend_levels)
    )
    
    ggplot2::ggplot(
      monthly_data,
      ggplot2::aes(x = year, y = month, fill = failure_class)
    ) +
      ggplot2::geom_tile(
        data = legend_dummy,
        ggplot2::aes(x = year, y = month, fill = failure_class),
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
        name = "Falhas (%)",
        values = failure_colors,
        limits = legend_levels,
        breaks = legend_levels,
        labels = failure_labels,
        drop = FALSE,
        na.translate = FALSE,
        guide = ggplot2::guide_legend(
          nrow = 1,
          byrow = TRUE,
          override.aes = list(
            alpha = 1,
            fill = unname(failure_colors),
            color = NA
          )
        )
      ) +
      ggplot2::scale_x_continuous(
        name = "Ano",
        breaks = scales::pretty_breaks(n = 8),
        expand = c(0, 0)
      ) +
      ggplot2::scale_y_continuous(
        name = "Mês",
        breaks = 1:12,
        labels = 1:12,
        expand = c(0, 0)
      ) +
      ggplot2::coord_fixed(ratio = 1.15) +
      ggplot2::labs(
        title = title,
        # subtitle = subtitle
      ) +
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
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title = ggplot2::element_text(size = 7, face = "bold"),
        legend.text = ggplot2::element_text(size = 7),
        legend.key.size = grid::unit(0.28, "cm"),
        legend.spacing.x = grid::unit(0.08, "cm"),
        legend.margin = ggplot2::margin(0, 0, 0, 0),
        plot.margin = ggplot2::margin(6, 8, 2, 8)
      )
  }

  output$fluviometric_consistency_status <- renderUI({
    result <- fluviometric_acquisition_result()
    
    if (is.null(result)) {
      return(
        tags$div(
          class = "table-status empty",
          "Nenhum dado fluviométrico foi carregado. Use primeiro a aba Obtenção de dados."
        )
      )
    }
    
    daily <- fluviometric_consistency_daily()
    
    if (nrow(daily) == 0) {
      return(
        tags$div(
          class = "table-status warning",
          "Os dados carregados não possuem datas válidas para a análise de consistência."
        )
      )
    }
    
    has_discharge <- any(daily$has_discharge, na.rm = TRUE)
    has_stage <- any(daily$has_stage, na.rm = TRUE)
    has_curve <- any(daily$has_valid_date_curve, na.rm = TRUE)
    
    if (has_discharge && has_stage && has_curve) {
      tags$div(
        class = "table-status available",
        "Análise completa disponível: a sessão possui vazões diárias, cotas diárias e curvas-chave válidas para pelo menos parte do período."
      )
    } else if (has_discharge && has_stage && !has_curve) {
      tags$div(
        class = "table-status warning",
        "Análise parcial: a sessão possui vazões e cotas, mas não há curva-chave válida no período avaliado."
      )
    } else if (has_discharge && !has_stage) {
      tags$div(
        class = "table-status warning",
        "Análise parcial: a sessão possui vazões, mas não possui cotas diárias correspondentes."
      )
    } else {
      tags$div(
        class = "table-status warning",
        "Análise parcial: os dados carregados não possuem vazão diária suficiente para a análise fluviométrica."
      )
    }
  })
  
  output$fluviometric_consistency_coverage_cards <- renderUI({
    daily <- fluviometric_consistency_daily()
    
    if (nrow(daily) == 0) {
      return(NULL)
    }
    
    n_period <- nrow(daily)
    n_discharge <- sum(daily$has_discharge, na.rm = TRUE)
    n_stage <- sum(daily$has_stage, na.rm = TRUE)
    n_both <- sum(daily$has_discharge & daily$has_stage, na.rm = TRUE)
    
    n_missing_discharge <- sum(!daily$has_discharge, na.rm = TRUE)
    n_missing_stage <- sum(!daily$has_stage, na.rm = TRUE)
    n_discharge_without_stage <- sum(daily$discharge_without_stage, na.rm = TRUE)
    n_stage_without_discharge <- sum(daily$has_stage & !daily$has_discharge, na.rm = TRUE)
    n_both_missing <- sum(!daily$has_discharge & !daily$has_stage, na.rm = TRUE)
    
    max_discharge_gap <- fluviometric_consistency_longest_run(!daily$has_discharge)
    max_stage_gap <- fluviometric_consistency_longest_run(!daily$has_stage)
    
    period_text <- paste0(
      format(min(daily$date, na.rm = TRUE), "%d/%m/%Y"),
      " a ",
      format(max(daily$date, na.rm = TRUE), "%d/%m/%Y")
    )
    
    fluviometric_collapsible_section(
      title = "Cobertura dos dados diários",
      # subtitle = "Cobertura conjunta das séries diárias de vazão e cota no período carregado.",
      open = TRUE,
      
      tags$div(
        class = "overview-metric-grid fluviometric-metric-grid",
        overview_metric("Período avaliado", period_text),
        overview_metric("Dias no período", fluviometric_format_count(n_period)),
        overview_metric("Dias com vazão", fluviometric_consistency_count_pct(n_discharge, n_period)),
        overview_metric("Dias com cota", fluviometric_consistency_count_pct(n_stage, n_period)),
        overview_metric("Dias com vazão + cota", fluviometric_consistency_count_pct(n_both, n_period)),
        overview_metric("Falhas em vazão", fluviometric_consistency_count_pct(n_missing_discharge, n_period)),
        overview_metric("Falhas em cota", fluviometric_consistency_count_pct(n_missing_stage, n_period)),
        overview_metric("Vazões sem cota", fluviometric_consistency_count_pct(n_discharge_without_stage, n_discharge)),
        overview_metric("Cotas sem vazão", fluviometric_consistency_count_pct(n_stage_without_discharge, n_stage)),
        overview_metric("Falhas simultâneas", fluviometric_consistency_count_pct(n_both_missing, n_period)),
        overview_metric("Maior falha em vazão", paste0(fluviometric_format_count(max_discharge_gap), " dias")),
        overview_metric("Maior falha em cota", paste0(fluviometric_format_count(max_stage_gap), " dias"))
      ),
      
      tags$br(),
      
      fluidRow(
        column(
          6,
          div(
            class = "plot-card",
            plotOutput(
              "fluviometric_consistency_discharge_gap_plot",
              height = "300px"
            )
          )
        ),
        column(
          6,
          div(
            class = "plot-card",
            plotOutput(
              "fluviometric_consistency_stage_gap_plot",
              height = "300px"
            )
          )
        )
      )
    )
  })
  
  output$fluviometric_consistency_curve_cards <- renderUI({
    daily <- fluviometric_consistency_daily()
    
    if (nrow(daily) == 0) {
      return(NULL)
    }
    
    curves <- tryCatch(
      selected_rating_curves(),
      error = function(e) NULL
    )
    
    n_curves <- if (is.null(curves) || nrow(curves) == 0) {
      0L
    } else if ("rating_curve_id" %in% names(curves)) {
      dplyr::n_distinct(curves$rating_curve_id)
    } else {
      nrow(curves)
    }
    
    n_discharge <- sum(daily$has_discharge, na.rm = TRUE)
    n_stage <- sum(daily$has_stage, na.rm = TRUE)
    n_both <- sum(daily$has_discharge & daily$has_stage, na.rm = TRUE)
    
    n_complete <- sum(daily$complete_hq_curve_day, na.rm = TRUE)
    n_without_curve <- sum(daily$discharge_without_curve, na.rm = TRUE)
    n_before_curve <- sum(daily$discharge_before_first_curve, na.rm = TRUE)
    n_after_curve <- sum(daily$discharge_after_last_curve, na.rm = TRUE)
    n_stage_outside <- sum(daily$stage_outside_curve_range, na.rm = TRUE)
    n_ambiguous <- sum(daily$ambiguous_curve_day, na.rm = TRUE)
    
    n_stage_with_valid_date_curve <- sum(
      daily$has_stage & daily$has_valid_date_curve,
      na.rm = TRUE
    )
    
    max_applicable_curves <- suppressWarnings(
      max(daily$n_applicable_curve_segments, na.rm = TRUE)
    )
    
    if (!is.finite(max_applicable_curves)) {
      max_applicable_curves <- 0L
    }
    
    fluviometric_collapsible_section(
      title = "Cobertura por curvas-chave",
      # subtitle = "Verificação da disponibilidade temporal e da faixa de validade das curvas-chave para as observações diárias.",
      open = TRUE,
      tags$div(
        class = "overview-metric-grid fluviometric-metric-grid",
        overview_metric("Curvas-chave", fluviometric_format_count(n_curves)),
        overview_metric("Dias com vazão + cota + curva", fluviometric_consistency_count_pct(n_complete, n_both)),
        overview_metric("Vazões sem curva válida", fluviometric_consistency_count_pct(n_without_curve, n_discharge)),
        overview_metric("Vazões antes da primeira curva", fluviometric_consistency_count_pct(n_before_curve, n_discharge)),
        overview_metric("Vazões após a última curva", fluviometric_consistency_count_pct(n_after_curve, n_discharge)),
        overview_metric("Cotas fora da faixa da curva", fluviometric_consistency_count_pct(n_stage_outside, n_stage_with_valid_date_curve)),
        overview_metric("Dias ambíguos", fluviometric_consistency_count_pct(n_ambiguous, n_stage)),
        overview_metric("Máx. curvas aplicáveis/dia", fluviometric_format_count(max_applicable_curves))
      )
    )
  })
  
  output$fluviometric_consistency_hq_cards <- renderUI({
    daily <- fluviometric_consistency_daily()
    
    if (nrow(daily) == 0) {
      return(NULL)
    }
    
    n_complete <- sum(daily$complete_hq_curve_day, na.rm = TRUE)
    n_generation_error <- sum(daily$q_generation_error, na.rm = TRUE)
    n_evaluable <- sum(daily$q_difference_evaluable, na.rm = TRUE)
    
    n_diff_5 <- sum(daily$q_diff_gt_5, na.rm = TRUE)
    n_diff_10 <- sum(daily$q_diff_gt_10, na.rm = TRUE)
    n_diff_25 <- sum(daily$q_diff_gt_25, na.rm = TRUE)
    
    median_abs_error <- 100 * fluviometric_consistency_safe_median(daily$abs_relative_error)
    p95_abs_error <- 100 * fluviometric_consistency_safe_quantile(daily$abs_relative_error, 0.95)
    
    n_discharge <- sum(daily$has_discharge, na.rm = TRUE)
    n_stage <- sum(daily$has_stage, na.rm = TRUE)
    
    n_discharge_nonpositive <- sum(daily$discharge_nonpositive, na.rm = TRUE)
    n_stage_nonpositive <- sum(daily$stage_nonpositive, na.rm = TRUE)
    
    fluviometric_collapsible_section(
      title = "Consistência cota–vazão–curva-chave",
      # subtitle = "Comparação entre a vazão disponibilizada e a vazão gerada a partir da cota diária e da curva-chave aplicável.",
      open = TRUE,
      
      tags$div(
        class = "overview-metric-grid fluviometric-metric-grid",
        overview_metric("Dias avaliáveis Q(H)", fluviometric_format_count(n_complete)),
        overview_metric("Erro na geração da vazão", fluviometric_consistency_count_pct(n_generation_error, n_complete)),
        overview_metric("Diferença Q > 5%", fluviometric_consistency_count_pct(n_diff_5, n_evaluable)),
        overview_metric("Diferença Q > 10%", fluviometric_consistency_count_pct(n_diff_10, n_evaluable)),
        overview_metric("Diferença Q > 25%", fluviometric_consistency_count_pct(n_diff_25, n_evaluable)),
        overview_metric("Erro mediano absoluto", paste0(fluviometric_consistency_format_percent(median_abs_error), "%")),
        overview_metric("Erro P95 absoluto", paste0(fluviometric_consistency_format_percent(p95_abs_error), "%")),
        overview_metric("Vazão ≤ 0", fluviometric_consistency_count_pct(n_discharge_nonpositive, n_discharge)),
        overview_metric("Cota ≤ 0", fluviometric_consistency_count_pct(n_stage_nonpositive, n_stage))
      ),
      
      tags$br(),
      
      div(
        class = "plot-card",
        plotOutput(
          "fluviometric_consistency_relative_error_plot",
          height = "310px"
        )
      )
    )
  })
  
  output$fluviometric_consistency_report_controls <- renderUI({
    result <- fluviometric_acquisition_result()
    
    if (is.null(result)) {
      return(NULL)
    }
    
    issues <- fluviometric_consistency_issue_details()
    
    tags$div(
      class = "control-card",
      downloadButton(
        outputId = "fluviometric_consistency_issue_report_download",
        label = paste0(
          "Baixar relatório detalhado de ocorrências",
          if (nrow(issues) > 0) {
            paste0(" (", fluviometric_format_count(nrow(issues)), ")")
          } else {
            ""
          }
        ),
        class = "btn-primary"
      ),
      tags$p(
        class = "help-block",
        "O arquivo CSV lista as datas associadas às falhas e inconsistências detectadas na análise."
      )
    )
  })
  
  output$fluviometric_consistency_issue_report_download <- downloadHandler(
    filename = function() {
      station_code <- as.character(selected_code())
      
      paste0(
        "consistencia_fluviometrica_",
        station_code,
        "_",
        format(Sys.Date(), "%Y%m%d"),
        ".csv"
      )
    },
    content = function(file) {
      issue_table <- fluviometric_consistency_issue_details()
      
      if (nrow(issue_table) == 0) {
        issue_table <- tibble::tibble(
          mensagem = "Nenhuma ocorrência foi identificada para os critérios avaliados."
        )
      }
      
      temp_file <- tempfile(fileext = ".csv")
      
      utils::write.csv2(
        issue_table,
        file = temp_file,
        row.names = FALSE,
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
  )
  
  output$fluviometric_consistency_discharge_gap_plot <- renderPlot({
    daily <- fluviometric_consistency_daily()
    
    if (nrow(daily) == 0) {
      draw_empty_plot("Sem dados de vazão para calcular falhas mensais.")
      return(invisible(NULL))
    }
    
    monthly_data <- fluviometric_consistency_monthly_gaps(
      daily = daily,
      variable = "discharge"
    )
    
    fluviometric_consistency_gap_plot(
      monthly_data = monthly_data,
      title = "Falhas mensais de vazão",
      subtitle = "Percentual mensal de dias sem vazão média diária."
    )
  }, res = 120)
  
  output$fluviometric_consistency_stage_gap_plot <- renderPlot({
    daily <- fluviometric_consistency_daily()
    
    if (nrow(daily) == 0 || !any(daily$has_stage, na.rm = TRUE)) {
      draw_empty_plot("Sem dados de cota para calcular falhas mensais.")
      return(invisible(NULL))
    }
    
    monthly_data <- fluviometric_consistency_monthly_gaps(
      daily = daily,
      variable = "stage"
    )
    
    fluviometric_consistency_gap_plot(
      monthly_data = monthly_data,
      title = "Falhas mensais de cota",
      subtitle = "Percentual mensal de dias sem cota média diária."
    )
  }, res = 120)
  
  output$fluviometric_consistency_relative_error_plot <- renderPlot({
    daily <- fluviometric_consistency_daily()
    
    plot_data <- daily |>
      dplyr::filter(
        q_difference_evaluable,
        is.finite(relative_error)
      ) |>
      dplyr::mutate(
        relative_error_pct = 100 * relative_error
      )
    
    if (nrow(plot_data) == 0) {
      draw_empty_plot("Sem dados suficientes para comparar Q gerada e Q disponibilizada.")
      return(invisible(NULL))
    }
    
    ggplot2::ggplot(
      plot_data,
      ggplot2::aes(x = date, y = relative_error_pct)
    ) +
      ggplot2::geom_hline(
        yintercept = 0,
        linewidth = 0.35
      ) +
      ggplot2::geom_hline(
        yintercept = c(-10, 10),
        linewidth = 0.25,
        linetype = "dashed"
      ) +
      ggplot2::geom_line(
        linewidth = 0.25,
        alpha = 0.75
      ) +
      ggplot2::scale_x_date(
        name = "Data",
        date_breaks = "5 years",
        date_labels = "%Y",
        expand = ggplot2::expansion(mult = c(0.005, 0.005))
      ) +
      ggplot2::scale_y_continuous(
        name = "Erro relativo (%)",
        labels = scales::label_number(decimal.mark = ",", big.mark = ".")
      ) +
      ggplot2::labs(
        title = "Erro relativo entre Q gerada e Q disponibilizada",
        subtitle = "Erro = 100 × (Q gerada pela curva-chave − Q disponibilizada) / Q disponibilizada."
      ) +
      preview_plot_theme(base_size = 6) +
      ggplot2::theme(
        legend.position = "none",
        plot.margin = ggplot2::margin(6, 8, 6, 8)
      )
  }, res = 120)

  output$fluviometric_consistency_curve_summary_status <- renderUI({
    daily <- fluviometric_consistency_daily()
    
    if (nrow(daily) == 0) {
      return(
        tags$div(
          class = "table-status empty",
          "Nenhum dado disponível para o resumo por curva-chave."
        )
      )
    }
    
    n_evaluable <- sum(daily$q_difference_evaluable, na.rm = TRUE)
    
    if (n_evaluable == 0) {
      return(
        tags$div(
          class = "table-status warning",
          "Não há dias suficientes com vazão, cota e curva-chave aplicável para comparar vazão gerada e vazão disponibilizada."
        )
      )
    }
    
    tags$div(
      class = "table-status available",
      paste0(
        "Resumo calculado com ",
        fluviometric_format_count(n_evaluable),
        " dias em que foi possível comparar Q gerada e Q disponibilizada."
      )
    )
  })
  
  output$fluviometric_consistency_curve_summary_table <- DT::renderDT({
    daily <- fluviometric_consistency_daily()
    
    if (
      nrow(daily) == 0 ||
      !"rating_curve_segment_id" %in% names(daily) ||
      all(is.na(daily$rating_curve_segment_id))
    ) {
      return(
        DT::datatable(
          tibble::tibble(Mensagem = "Nenhum resumo por curva-chave disponível."),
          rownames = FALSE,
          options = list(dom = "t")
        )
      )
    }
    
    format_curve_date_range <- function(date_start, date_end) {
      start_text <- ifelse(
        is.na(date_start),
        "sem início",
        format(date_start, "%d/%m/%Y")
      )
      
      end_text <- ifelse(
        is.na(date_end),
        "sem fim",
        format(date_end, "%d/%m/%Y")
      )
      
      paste0(start_text, " a ", end_text)
    }
    
    format_stage_range <- function(stage_min, stage_max) {
      if (is.na(stage_min) || is.na(stage_max)) {
        return("faixa de cota não informada")
      }
      
      paste0(
        formatC(stage_min, format = "f", digits = 0, big.mark = ".", decimal.mark = ","),
        "–",
        formatC(stage_max, format = "f", digits = 0, big.mark = ".", decimal.mark = ","),
        " cm"
      )
    }
    
    table_data <- daily |>
      dplyr::filter(!is.na(rating_curve_segment_id)) |>
      dplyr::group_by(rating_curve_id, rating_curve_segment_id) |>
      dplyr::summarise(
        inicio_validade = if (all(is.na(selected_curve_valid_from))) {
          as.Date(NA)
        } else {
          min(selected_curve_valid_from, na.rm = TRUE)
        },
        fim_validade = if (all(is.na(selected_curve_valid_to))) {
          as.Date(NA)
        } else {
          max(selected_curve_valid_to, na.rm = TRUE)
        },
        cota_min_cm = if (all(is.na(selected_stage_min_cm))) {
          NA_real_
        } else {
          min(selected_stage_min_cm, na.rm = TRUE)
        },
        cota_max_cm = if (all(is.na(selected_stage_max_cm))) {
          NA_real_
        } else {
          max(selected_stage_max_cm, na.rm = TRUE)
        },
        dias_com_match = dplyr::n(),
        dias_com_q_gerada = sum(is.finite(q_generated_m3s), na.rm = TRUE),
        erros_geracao = sum(q_generation_error, na.rm = TRUE),
        dias_com_comparacao = sum(q_difference_evaluable, na.rm = TRUE),
        diferenca_gt_5 = sum(q_diff_gt_5, na.rm = TRUE),
        diferenca_gt_10 = sum(q_diff_gt_10, na.rm = TRUE),
        diferenca_gt_25 = sum(q_diff_gt_25, na.rm = TRUE),
        erro_mediano_pct = 100 * fluviometric_consistency_safe_median(abs_relative_error),
        erro_p95_pct = 100 * fluviometric_consistency_safe_quantile(abs_relative_error, 0.95),
        .groups = "drop"
      ) |>
      dplyr::arrange(inicio_validade, fim_validade, cota_min_cm, cota_max_cm, rating_curve_id, rating_curve_segment_id)
    
    curve_labels <- table_data |>
      dplyr::distinct(rating_curve_id, inicio_validade, fim_validade) |>
      dplyr::arrange(inicio_validade, fim_validade, rating_curve_id) |>
      dplyr::mutate(
        curva_numero = dplyr::row_number(),
        periodo_validade = mapply(
          format_curve_date_range,
          inicio_validade,
          fim_validade,
          USE.NAMES = FALSE
        ),
        curva_label = paste0("Curva ", curva_numero, " — ", periodo_validade)
      )
    
    table_data <- table_data |>
      dplyr::left_join(
        curve_labels |> dplyr::select(rating_curve_id, curva_numero, curva_label),
        by = "rating_curve_id"
      ) |>
      dplyr::group_by(curva_numero) |>
      dplyr::arrange(cota_min_cm, cota_max_cm, rating_curve_segment_id, .by_group = TRUE) |>
      dplyr::mutate(
        ramo_numero = dplyr::row_number(),
        faixa_cota = mapply(
          format_stage_range,
          cota_min_cm,
          cota_max_cm,
          USE.NAMES = FALSE
        ),
        ramo_label = paste0("Ramo ", ramo_numero, " — ", faixa_cota)
      ) |>
      dplyr::ungroup() |>
      dplyr::mutate(
        erro_mediano_pct = round(erro_mediano_pct, 2),
        erro_p95_pct = round(erro_p95_pct, 2)
      ) |>
      dplyr::transmute(
        `Curva-chave` = curva_label,
        `Ramo` = ramo_label,
        `Dias com curva aplicável` = dias_com_match,
        `Dias com Q gerada` = dias_com_q_gerada,
        `Erros de geração` = erros_geracao,
        `Dias comparados` = dias_com_comparacao,
        `Dif. > 5%` = diferenca_gt_5,
        `Dif. > 10%` = diferenca_gt_10,
        `Dif. > 25%` = diferenca_gt_25,
        `Erro mediano absoluto (%)` = erro_mediano_pct,
        `Erro P95 absoluto (%)` = erro_p95_pct
      )
    
    DT::datatable(
      table_data,
      rownames = FALSE,
      class = "compact stripe hover",
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        order = list(list(0, "asc"), list(1, "asc"))
      )
    )
  })
  

