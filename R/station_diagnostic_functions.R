# ============================================================
# station_diagnostic_functions.R
# Purpose: Reusable station-level diagnostic functions for Shiny and previews.
# ============================================================

# This file does not call the ANA API.
# It operates on station-level data already loaded from the local Shiny export.

# Load packages
library(dplyr)

# ------------------------------------------------------------
# Default parameters
# ------------------------------------------------------------

station_diagnostic_default_params <- function() {
  list(
    # Zero-value rules
    stage_zero_tolerance_cm = 0,
    discharge_zero_tolerance_m3s = 0,
    
    # Repeated-value attention rules
    min_repeated_group_size = 5,
    stage_group_round_digits = 2,
    discharge_group_round_digits = 3,
    min_stage_spread_cm_for_repeated_discharge = 5,
    min_abs_discharge_spread_m3s_for_repeated_stage = 0.1,
    min_rel_discharge_spread_for_repeated_stage = 0.10,
    
    # Rating-curve residual rules
    min_residual_points_per_segment = 5,
    residual_envelope_sd_multiplier = 1.96,
    
    # Power-law baseline model for temporal-regime screening
    # Q = a * (H - h0)^b, where Q is discharge_m3s and H is stage in meters.
    min_power_model_points = 30,
    n_h0_grid = 40,
    h0_min_offset_m = 0.005,
    h0_grid_span_multiplier = 2.0,
    h0_grid_min_span_m = 1.0,
    h0_grid_max_span_m = 20.0,
    min_power_exponent = 0.05,
    max_power_exponent = 10,
    
    # Temporal-regime screening rules
    min_regime_measurements = 30,
    max_temporal_regimes = 3,
    max_break_candidates = 25,
    min_regime_fraction = 0.15,
    min_regime_span_years = 4,
    min_log_residual_shift = log(1.25),
    residual_shift_mad_fraction = 0.75,
    min_break_gain = 0.12,
    min_incremental_gain_for_second_break = 0.06,
    
    # Fitted-curve preview points
    n_stage_points_per_segment = 80,
    n_stage_points_power_curve = 120
  )
}

# ------------------------------------------------------------
# Small utilities
# ------------------------------------------------------------

safe_divide <- function(num, den) {
  ifelse(is.na(den) | den == 0, NA_real_, num / den)
}

safe_min <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)
  min(x)
}

safe_max <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)
  max(x)
}

safe_mad <- function(x) {
  x <- x[!is.na(x) & is.finite(x)]
  if (length(x) < 2) return(NA_real_)
  stats::mad(x, constant = 1, na.rm = TRUE)
}

safe_first <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA)
  x[1]
}

format_coef <- function(x, digits = 3) {
  out <- formatC(x, format = "f", digits = digits)
  out <- sub("\\.?0+$", "", out)
  out[is.na(x)] <- "NA"
  out
}

make_equation_text <- function(a, h0, n) {
  sign_text <- ifelse(is.na(h0), "-", ifelse(h0 < 0, "+", "-"))
  h0_abs <- abs(h0)
  
  paste0(
    "Q=", format_coef(a),
    "(H", sign_text, format_coef(h0_abs), ")^",
    format_coef(n)
  )
}

class_percent <- function(x) {
  dplyr::case_when(
    is.na(x) ~ "not_available",
    x == 0 ~ "none",
    x < 0.01 ~ "very_low",
    x < 0.05 ~ "low",
    x < 0.15 ~ "moderate",
    TRUE ~ "high"
  )
}

class_residual <- function(x) {
  dplyr::case_when(
    is.na(x) ~ "not_available",
    x < 0.10 ~ "low",
    x < 0.25 ~ "moderate",
    x < 0.50 ~ "high",
    TRUE ~ "very_high"
  )
}

standardize_measurements <- function(measurements) {
  if (is.null(measurements) || nrow(measurements) == 0) {
    return(data.frame())
  }
  
  measurements <- measurements %>%
    mutate(
      station_code = as.character(station_code)
    )
  
  if ("measurement_date" %in% names(measurements)) {
    measurements$measurement_date <- as.Date(measurements$measurement_date)
  } else if ("measurement_datetime" %in% names(measurements)) {
    measurements$measurement_date <- as.Date(measurements$measurement_datetime)
  } else {
    measurements$measurement_date <- as.Date(NA)
  }
  
  measurements$measurement_year <- as.integer(format(measurements$measurement_date, "%Y"))
  
  measurements
}

standardize_rating_curves <- function(rating_curves) {
  if (is.null(rating_curves) || nrow(rating_curves) == 0) {
    return(data.frame())
  }
  
  rating_curves %>%
    mutate(
      station_code = as.character(station_code),
      valid_from = as.Date(valid_from),
      valid_to = as.Date(valid_to)
    )
}

# ------------------------------------------------------------
# Rating-curve utilities
# ------------------------------------------------------------

predict_rating_discharge <- function(stage_cm, coefficient_a, coefficient_h0, coefficient_n) {
  stage_m <- stage_cm / 100
  base_m <- stage_m - coefficient_h0
  
  ifelse(
    !is.na(base_m) & base_m > 0 &
      !is.na(coefficient_a) & !is.na(coefficient_n),
    coefficient_a * (base_m ^ coefficient_n),
    NA_real_
  )
}

make_curve_metadata <- function(curves) {
  curves <- standardize_rating_curves(curves)
  if (nrow(curves) == 0) return(data.frame())
  
  curves %>%
    arrange(valid_from, valid_to, rating_curve_id, segment_number) %>%
    group_by(rating_curve_id) %>%
    summarise(
      valid_from_first = safe_first(valid_from),
      valid_to_first = safe_first(valid_to),
      n_curve_segments = n_distinct(rating_curve_segment_id),
      stage_min_curve_cm = safe_min(stage_min_cm),
      stage_max_curve_cm = safe_max(stage_max_cm),
      .groups = "drop"
    ) %>%
    arrange(valid_from_first, valid_to_first, rating_curve_id) %>%
    mutate(
      curve_short_label = paste0("CC ", row_number()),
      curve_label = paste0(
        curve_short_label,
        " | ",
        n_curve_segments,
        ifelse(n_curve_segments == 1, " segmento", " segmentos")
      )
    )
}

make_curve_segment_metadata <- function(curves, curve_metadata = NULL) {
  curves <- standardize_rating_curves(curves)
  if (nrow(curves) == 0) return(data.frame())
  
  if (is.null(curve_metadata)) {
    curve_metadata <- make_curve_metadata(curves)
  }
  
  curves %>%
    filter(
      !is.na(stage_min_cm),
      !is.na(stage_max_cm),
      !is.na(coefficient_a),
      !is.na(coefficient_h0),
      !is.na(coefficient_n),
      stage_max_cm > stage_min_cm
    ) %>%
    left_join(
      curve_metadata %>% select(rating_curve_id, curve_short_label, curve_label),
      by = "rating_curve_id"
    ) %>%
    mutate(
      segment_equation = make_equation_text(coefficient_a, coefficient_h0, coefficient_n),
      curve_segment_label = paste0(
        curve_short_label,
        " | seg ",
        segment_number,
        " | H=",
        round(stage_min_cm, 0),
        "–",
        round(stage_max_cm, 0),
        " cm"
      )
    ) %>%
    arrange(valid_from, valid_to, rating_curve_id, segment_number)
}

make_rating_curve_points <- function(curve_segments, n_points = 80) {
  if (is.null(curve_segments) || nrow(curve_segments) == 0) return(data.frame())
  
  out <- lapply(seq_len(nrow(curve_segments)), function(i) {
    row <- curve_segments[i, ]
    stages_cm <- seq(row$stage_min_cm, row$stage_max_cm, length.out = n_points)
    discharge <- predict_rating_discharge(
      stage_cm = stages_cm,
      coefficient_a = row$coefficient_a,
      coefficient_h0 = row$coefficient_h0,
      coefficient_n = row$coefficient_n
    )
    
    data.frame(
      station_code = row$station_code,
      rating_curve_id = row$rating_curve_id,
      rating_curve_segment_id = row$rating_curve_segment_id,
      segment_number = row$segment_number,
      curve_label = row$curve_label,
      curve_segment_label = row$curve_segment_label,
      valid_from = row$valid_from,
      valid_to = row$valid_to,
      stage_min_cm = row$stage_min_cm,
      stage_max_cm = row$stage_max_cm,
      stage_cm = stages_cm,
      discharge_m3s = discharge,
      stringsAsFactors = FALSE
    )
  })
  
  bind_rows(out) %>%
    filter(!is.na(discharge_m3s), is.finite(discharge_m3s), discharge_m3s >= 0)
}

match_measurements_to_rating_curves <- function(measurements, curve_segments) {
  measurements <- standardize_measurements(measurements)
  if (nrow(measurements) == 0 || is.null(curve_segments) || nrow(curve_segments) == 0) {
    return(data.frame())
  }
  
  m <- measurements %>%
    mutate(.measurement_id = row_number()) %>%
    filter(
      !is.na(measurement_date),
      !is.na(stage_cm),
      !is.na(discharge_m3s),
      stage_cm > 0,
      discharge_m3s > 0
    )
  
  if (nrow(m) == 0) return(data.frame())
  
  out <- lapply(seq_len(nrow(curve_segments)), function(i) {
    row <- curve_segments[i, ]
    valid_from_date <- as.Date(row$valid_from)
    valid_to_date <- as.Date(row$valid_to)
    
    if (is.na(valid_from_date)) return(data.frame())
    if (is.na(valid_to_date)) valid_to_date <- Sys.Date()
    
    matched <- m %>%
      filter(
        measurement_date >= valid_from_date,
        measurement_date <= valid_to_date,
        stage_cm >= row$stage_min_cm,
        stage_cm <= row$stage_max_cm
      )
    
    if (nrow(matched) == 0) return(data.frame())
    
    q_hat <- predict_rating_discharge(
      stage_cm = matched$stage_cm,
      coefficient_a = row$coefficient_a,
      coefficient_h0 = row$coefficient_h0,
      coefficient_n = row$coefficient_n
    )
    
    matched %>%
      mutate(
        rating_curve_id = row$rating_curve_id,
        rating_curve_segment_id = row$rating_curve_segment_id,
        segment_number = row$segment_number,
        curve_label = row$curve_label,
        curve_segment_label = row$curve_segment_label,
        rating_predicted_discharge_m3s = q_hat,
        rating_log_residual = log(discharge_m3s) - log(rating_predicted_discharge_m3s),
        rating_relative_residual_pct = 100 * (exp(rating_log_residual) - 1)
      ) %>%
      filter(
        !is.na(rating_predicted_discharge_m3s),
        is.finite(rating_predicted_discharge_m3s),
        rating_predicted_discharge_m3s > 0,
        !is.na(rating_log_residual),
        is.finite(rating_log_residual)
      )
  })
  
  bind_rows(out)
}

make_best_rating_match <- function(rating_matches) {
  if (is.null(rating_matches) || nrow(rating_matches) == 0) return(data.frame())
  
  rating_matches %>%
    group_by(.measurement_id) %>%
    arrange(abs(rating_log_residual), rating_curve_segment_id, .by_group = TRUE) %>%
    slice(1) %>%
    ungroup()
}

make_residual_envelopes <- function(best_matches, params = station_diagnostic_default_params()) {
  if (is.null(best_matches) || nrow(best_matches) == 0) return(data.frame())
  
  best_matches %>%
    filter(!is.na(rating_log_residual), is.finite(rating_log_residual)) %>%
    group_by(station_code, rating_curve_id, rating_curve_segment_id, curve_label, curve_segment_label) %>%
    summarise(
      n_residual_points = n(),
      mean_log_residual = mean(rating_log_residual, na.rm = TRUE),
      median_log_residual = median(rating_log_residual, na.rm = TRUE),
      sd_log_residual = ifelse(n() >= 2, sd(rating_log_residual, na.rm = TRUE), NA_real_),
      median_abs_log_residual = median(abs(rating_log_residual), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      envelope_lower_log_residual = mean_log_residual - params$residual_envelope_sd_multiplier * sd_log_residual,
      envelope_upper_log_residual = mean_log_residual + params$residual_envelope_sd_multiplier * sd_log_residual,
      has_residual_envelope = n_residual_points >= params$min_residual_points_per_segment & !is.na(sd_log_residual)
    )
}

add_envelope_flags <- function(best_matches, envelopes) {
  if (is.null(best_matches) || nrow(best_matches) == 0) return(data.frame())
  if (is.null(envelopes) || nrow(envelopes) == 0) {
    return(best_matches %>% mutate(outside_residual_envelope = NA))
  }
  
  best_matches %>%
    left_join(
      envelopes %>%
        select(
          rating_curve_segment_id,
          envelope_lower_log_residual,
          envelope_upper_log_residual,
          has_residual_envelope
        ),
      by = "rating_curve_segment_id"
    ) %>%
    mutate(
      outside_residual_envelope = ifelse(
        has_residual_envelope &
          !is.na(rating_log_residual) &
          (rating_log_residual < envelope_lower_log_residual |
             rating_log_residual > envelope_upper_log_residual),
        TRUE,
        FALSE
      )
    )
}

# ------------------------------------------------------------
# Measurement flags
# ------------------------------------------------------------

# Return NA for an all-missing group without emitting min()/max() warnings.
safe_min_or_na <- function(x) {
  values <- x[!is.na(x)]
  if (length(values) == 0L) return(NA_real_)
  min(values)
}

safe_max_or_na <- function(x) {
  values <- x[!is.na(x)]
  if (length(values) == 0L) return(NA_real_)
  max(values)
}

make_measurement_flags <- function(measurements, params = station_diagnostic_default_params()) {
  m <- standardize_measurements(measurements)
  if (nrow(m) == 0) return(data.frame())
  
  m <- m %>%
    mutate(
      .measurement_id = row_number(),
      stage_zero_or_negative_flag = !is.na(stage_cm) & stage_cm <= params$stage_zero_tolerance_cm,
      discharge_zero_or_negative_flag = !is.na(discharge_m3s) & discharge_m3s <= params$discharge_zero_tolerance_m3s,
      stage_group = ifelse(!is.na(stage_cm), round(stage_cm, params$stage_group_round_digits), NA_real_),
      discharge_group = ifelse(!is.na(discharge_m3s), round(discharge_m3s, params$discharge_group_round_digits), NA_real_)
    )
  
  repeated_stage_groups <- m %>%
    filter(!is.na(stage_group), !is.na(discharge_m3s)) %>%
    group_by(stage_group) %>%
    summarise(
      n_repeated_stage_group = n(),
      discharge_min_m3s_in_stage_group = safe_min_or_na(discharge_m3s),
      discharge_max_m3s_in_stage_group = safe_max_or_na(discharge_m3s),
      discharge_median_m3s_in_stage_group = median(discharge_m3s, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      discharge_spread_m3s_in_stage_group = discharge_max_m3s_in_stage_group - discharge_min_m3s_in_stage_group,
      discharge_rel_spread_in_stage_group = safe_divide(discharge_spread_m3s_in_stage_group, abs(discharge_median_m3s_in_stage_group)),
      repeated_stage_variable_discharge_group_flag =
        n_repeated_stage_group >= params$min_repeated_group_size &
        discharge_spread_m3s_in_stage_group >= params$min_abs_discharge_spread_m3s_for_repeated_stage &
        discharge_rel_spread_in_stage_group >= params$min_rel_discharge_spread_for_repeated_stage
    )
  
  repeated_discharge_groups <- m %>%
    filter(!is.na(discharge_group), !is.na(stage_cm)) %>%
    group_by(discharge_group) %>%
    summarise(
      n_repeated_discharge_group = n(),
      stage_min_cm_in_discharge_group = safe_min_or_na(stage_cm),
      stage_max_cm_in_discharge_group = safe_max_or_na(stage_cm),
      .groups = "drop"
    ) %>%
    mutate(
      stage_spread_cm_in_discharge_group = stage_max_cm_in_discharge_group - stage_min_cm_in_discharge_group,
      repeated_discharge_variable_stage_group_flag =
        n_repeated_discharge_group >= params$min_repeated_group_size &
        stage_spread_cm_in_discharge_group >= params$min_stage_spread_cm_for_repeated_discharge
    )
  
  m %>%
    left_join(repeated_stage_groups, by = "stage_group") %>%
    left_join(repeated_discharge_groups, by = "discharge_group") %>%
    mutate(
      repeated_stage_variable_discharge_flag = ifelse(is.na(repeated_stage_variable_discharge_group_flag), FALSE, repeated_stage_variable_discharge_group_flag),
      repeated_discharge_variable_stage_flag = ifelse(is.na(repeated_discharge_variable_stage_group_flag), FALSE, repeated_discharge_variable_stage_group_flag),
      any_obvious_measurement_attention_flag = stage_zero_or_negative_flag |
        discharge_zero_or_negative_flag |
        repeated_stage_variable_discharge_flag |
        repeated_discharge_variable_stage_flag
    )
}

make_repeated_value_group_details <- function(measurement_flags) {
  if (is.null(measurement_flags) || nrow(measurement_flags) == 0) return(data.frame())
  
  repeated_stage <- measurement_flags %>%
    filter(repeated_stage_variable_discharge_flag) %>%
    distinct(
      station_code,
      group_type = "same_stage_variable_discharge",
      group_value = stage_group,
      n_group = n_repeated_stage_group,
      spread_value = discharge_spread_m3s_in_stage_group,
      relative_spread = discharge_rel_spread_in_stage_group
    )
  
  repeated_discharge <- measurement_flags %>%
    filter(repeated_discharge_variable_stage_flag) %>%
    distinct(
      station_code,
      group_type = "same_discharge_variable_stage",
      group_value = discharge_group,
      n_group = n_repeated_discharge_group,
      spread_value = stage_spread_cm_in_discharge_group,
      relative_spread = NA_real_
    )
  
  bind_rows(repeated_stage, repeated_discharge)
}

# ------------------------------------------------------------
# Power-law baseline model and temporal regimes
# ------------------------------------------------------------

robust_centered_sse <- function(x) {
  x <- x[!is.na(x) & is.finite(x)]
  if (length(x) == 0) return(NA_real_)
  med <- median(x)
  abs_dev <- abs(x - med)
  cap <- stats::quantile(abs_dev, probs = 0.90, na.rm = TRUE, names = FALSE)
  if (is.na(cap) || cap <= 0) cap <- max(abs_dev, na.rm = TRUE)
  if (is.na(cap) || cap <= 0) return(0)
  sum(pmin(abs_dev, cap) ^ 2)
}

make_h0_candidates <- function(stage_m, params = station_diagnostic_default_params()) {
  stage_m <- stage_m[!is.na(stage_m) & is.finite(stage_m) & stage_m > 0]
  if (length(stage_m) == 0) return(numeric(0))
  
  h_min <- min(stage_m)
  h_max <- max(stage_m)
  h_span <- h_max - h_min
  grid_span <- max(params$h0_grid_min_span_m, params$h0_grid_span_multiplier * h_span)
  grid_span <- min(grid_span, params$h0_grid_max_span_m)
  
  lower <- h_min - grid_span
  upper <- h_min - params$h0_min_offset_m
  
  if (!is.finite(lower) || !is.finite(upper) || lower >= upper) {
    return(numeric(0))
  }
  
  unique(seq(lower, upper, length.out = params$n_h0_grid))
}

fit_power_rating_baseline <- function(measurements, params = station_diagnostic_default_params()) {
  df <- standardize_measurements(measurements) %>%
    filter(
      !is.na(stage_cm),
      !is.na(discharge_m3s),
      !is.na(measurement_date),
      stage_cm > 0,
      discharge_m3s > 0
    ) %>%
    mutate(
      stage_m = stage_cm / 100,
      log_observed_discharge = log(discharge_m3s)
    )
  
  if (nrow(df) < params$min_power_model_points || length(unique(df$stage_m)) < 3) {
    return(list(points = data.frame(), model = data.frame()))
  }
  
  h0_candidates <- make_h0_candidates(df$stage_m, params = params)
  if (length(h0_candidates) == 0) {
    return(list(points = data.frame(), model = data.frame()))
  }
  
  fits <- lapply(h0_candidates, function(h0) {
    tmp <- df %>%
      mutate(
        effective_stage_m = stage_m - h0,
        log_effective_stage = log(effective_stage_m)
      ) %>%
      filter(
        !is.na(log_effective_stage),
        is.finite(log_effective_stage),
        effective_stage_m > 0
      )
    
    if (nrow(tmp) < params$min_power_model_points || length(unique(tmp$log_effective_stage)) < 2) {
      return(NULL)
    }
    
    fit <- tryCatch(
      stats::lm(log_observed_discharge ~ log_effective_stage, data = tmp),
      error = function(e) NULL
    )
    
    if (is.null(fit)) return(NULL)
    
    coefs <- stats::coef(fit)
    intercept <- unname(coefs[1])
    exponent_b <- unname(coefs[2])
    
    if (
      is.na(intercept) || is.na(exponent_b) ||
      exponent_b <= params$min_power_exponent ||
      exponent_b > params$max_power_exponent
    ) {
      return(NULL)
    }
    
    predicted_log_discharge <- as.numeric(stats::predict(fit, newdata = tmp))
    log_residual <- tmp$log_observed_discharge - predicted_log_discharge
    
    data.frame(
      h0_m = h0,
      coefficient_a = exp(intercept),
      coefficient_b = exponent_b,
      robust_sse = robust_centered_sse(log_residual),
      median_abs_log_residual = median(abs(log_residual), na.rm = TRUE),
      n_model_points = nrow(tmp),
      stringsAsFactors = FALSE
    )
  })
  
  fit_table <- bind_rows(fits)
  if (nrow(fit_table) == 0) {
    return(list(points = data.frame(), model = data.frame()))
  }
  
  best_model <- fit_table %>%
    arrange(robust_sse, median_abs_log_residual, abs(h0_m)) %>%
    slice(1) %>%
    mutate(
      model_type = "power_rating_baseline",
      equation = make_equation_text(coefficient_a, h0_m, coefficient_b)
    )
  
  points <- df %>%
    mutate(
      effective_stage_m = stage_m - best_model$h0_m,
      power_predicted_discharge_m3s = ifelse(
        effective_stage_m > 0,
        best_model$coefficient_a * (effective_stage_m ^ best_model$coefficient_b),
        NA_real_
      ),
      power_log_residual = log(discharge_m3s) - log(power_predicted_discharge_m3s),
      power_relative_residual_pct = 100 * (exp(power_log_residual) - 1)
    ) %>%
    filter(
      !is.na(power_predicted_discharge_m3s),
      is.finite(power_predicted_discharge_m3s),
      power_predicted_discharge_m3s > 0,
      !is.na(power_log_residual),
      is.finite(power_log_residual)
    )
  
  list(points = points, model = best_model)
}

make_power_curve_points <- function(power_model, stage_min_cm, stage_max_cm, params = station_diagnostic_default_params()) {
  if (is.null(power_model) || nrow(power_model) == 0 || is.na(stage_min_cm) || is.na(stage_max_cm)) {
    return(data.frame())
  }
  
  stages_cm <- seq(stage_min_cm, stage_max_cm, length.out = params$n_stage_points_power_curve)
  stage_m <- stages_cm / 100
  effective_stage_m <- stage_m - power_model$h0_m[1]
  discharge <- ifelse(
    effective_stage_m > 0,
    power_model$coefficient_a[1] * (effective_stage_m ^ power_model$coefficient_b[1]),
    NA_real_
  )
  
  data.frame(
    stage_cm = stages_cm,
    discharge_m3s = discharge,
    stringsAsFactors = FALSE
  ) %>%
    filter(!is.na(discharge_m3s), is.finite(discharge_m3s), discharge_m3s > 0)
}

make_break_candidates <- function(points, params = station_diagnostic_default_params()) {
  dates <- sort(unique(as.Date(points$measurement_date)))
  if (length(dates) < 3) return(as.Date(character(0)))
  
  candidate_dates <- dates[-length(dates)]
  if (length(candidate_dates) > params$max_break_candidates) {
    idx <- unique(round(seq(1, length(candidate_dates), length.out = params$max_break_candidates)))
    candidate_dates <- candidate_dates[idx]
  }
  
  as.Date(candidate_dates)
}

assign_temporal_regimes <- function(points, breaks) {
  points <- points %>% arrange(measurement_date, measurement_datetime)
  breaks <- sort(as.Date(breaks))
  
  if (length(breaks) == 0) {
    return(points %>% mutate(regime_number = 1L))
  }
  
  regime_number <- rep(1L, nrow(points))
  for (i in seq_along(breaks)) {
    regime_number[as.Date(points$measurement_date) > breaks[i]] <- i + 1L
  }
  
  points %>% mutate(regime_number = regime_number)
}

evaluate_temporal_partition <- function(points, breaks, params = station_diagnostic_default_params()) {
  points <- points %>%
    filter(!is.na(power_log_residual), is.finite(power_log_residual)) %>%
    arrange(measurement_date, measurement_datetime)
  
  if (nrow(points) == 0) return(data.frame())
  
  global_dispersion <- robust_centered_sse(points$power_log_residual)
  if (is.na(global_dispersion) || global_dispersion <= 0) global_dispersion <- 0
  
  assigned <- assign_temporal_regimes(points, breaks)
  
  regime_summary <- assigned %>%
    group_by(regime_number) %>%
    summarise(
      n_points = n(),
      date_start = min(measurement_date, na.rm = TRUE),
      date_end = max(measurement_date, na.rm = TRUE),
      date_span_years = as.numeric(date_end - date_start) / 365.25,
      median_log_residual = median(power_log_residual, na.rm = TRUE),
      robust_sse = robust_centered_sse(power_log_residual),
      .groups = "drop"
    ) %>%
    mutate(point_fraction = n_points / nrow(assigned))
  
  segmented_dispersion <- sum(regime_summary$robust_sse, na.rm = TRUE)
  dispersion_gain <- ifelse(global_dispersion > 0, 1 - segmented_dispersion / global_dispersion, 0)
  
  residual_shift_log <- if (nrow(regime_summary) >= 2) {
    max(regime_summary$median_log_residual, na.rm = TRUE) - min(regime_summary$median_log_residual, na.rm = TRUE)
  } else {
    0
  }
  
  global_mad <- safe_mad(points$power_log_residual)
  required_shift <- max(params$min_log_residual_shift, params$residual_shift_mad_fraction * global_mad, na.rm = TRUE)
  if (!is.finite(required_shift)) required_shift <- params$min_log_residual_shift
  
  accepted <- all(regime_summary$n_points >= params$min_regime_measurements) &&
    all(regime_summary$point_fraction >= params$min_regime_fraction) &&
    all(regime_summary$date_span_years >= params$min_regime_span_years) &&
    residual_shift_log >= required_shift &&
    dispersion_gain >= params$min_break_gain
  
  data.frame(
    n_regimes = nrow(regime_summary),
    break_dates = ifelse(length(breaks) == 0, NA_character_, paste(as.character(breaks), collapse = ";")),
    n_points = nrow(assigned),
    min_regime_points = min(regime_summary$n_points, na.rm = TRUE),
    min_regime_fraction = min(regime_summary$point_fraction, na.rm = TRUE),
    min_regime_span_years = min(regime_summary$date_span_years, na.rm = TRUE),
    residual_shift_log = residual_shift_log,
    residual_shift_pct = 100 * (exp(residual_shift_log) - 1),
    required_residual_shift_log = required_shift,
    dispersion_gain = dispersion_gain,
    accepted = accepted,
    stringsAsFactors = FALSE
  )
}

find_best_temporal_regime_model <- function(points, params = station_diagnostic_default_params()) {
  points <- points %>% arrange(measurement_date, measurement_datetime)
  base_model <- evaluate_temporal_partition(points, breaks = as.Date(character(0)), params = params)
  candidates <- make_break_candidates(points, params = params)
  
  one_break_scores <- data.frame()
  two_break_scores <- data.frame()
  best_one <- data.frame()
  best_two <- data.frame()
  
  if (length(candidates) > 0 && params$max_temporal_regimes >= 2) {
    one_break_scores <- bind_rows(lapply(candidates, function(b1) {
      evaluate_temporal_partition(points, breaks = as.Date(b1), params = params)
    }))
    
    if (nrow(one_break_scores) > 0 && "accepted" %in% names(one_break_scores)) {
      best_one <- one_break_scores %>%
        filter(accepted) %>%
        arrange(desc(dispersion_gain), desc(residual_shift_log), break_dates) %>%
        slice(1)
    }
  }
  
  # Greedy two-break search: use the best accepted one-break model as anchor.
  if (
    length(candidates) > 1 &&
    params$max_temporal_regimes >= 3 &&
    nrow(best_one) > 0 &&
    !is.na(best_one$break_dates)
  ) {
    first_break <- as.Date(strsplit(best_one$break_dates, ";", fixed = TRUE)[[1]][1])
    second_candidates <- candidates[candidates != first_break]
    
    if (length(second_candidates) > 0) {
      two_break_scores <- bind_rows(lapply(second_candidates, function(b2) {
        evaluate_temporal_partition(points, breaks = sort(as.Date(c(first_break, b2))), params = params)
      }))
      
      if (nrow(two_break_scores) > 0 && "accepted" %in% names(two_break_scores)) {
        best_two <- two_break_scores %>%
          filter(accepted) %>%
          arrange(desc(dispersion_gain), desc(residual_shift_log), break_dates) %>%
          slice(1)
      }
    }
  }
  
  selected <- base_model
  evidence_class <- "no_evidence"
  
  if (nrow(best_one) > 0) {
    selected <- best_one
    evidence_class <- ifelse(
      best_one$dispersion_gain >= 0.25 && best_one$residual_shift_log >= log(1.50),
      "strong_evidence",
      "moderate_evidence"
    )
  } else if (nrow(one_break_scores) > 0 && "dispersion_gain" %in% names(one_break_scores)) {
    near_one <- one_break_scores %>%
      arrange(desc(dispersion_gain), desc(residual_shift_log)) %>%
      slice(1)
    
    if (nrow(near_one) > 0 && near_one$dispersion_gain >= params$min_break_gain * 0.60) {
      evidence_class <- "weak_evidence"
    }
  }
  
  if (nrow(best_two) > 0) {
    one_gain <- ifelse(nrow(best_one) > 0, best_one$dispersion_gain, 0)
    
    if ((best_two$dispersion_gain - one_gain) >= params$min_incremental_gain_for_second_break) {
      selected <- best_two
      evidence_class <- "strong_evidence"
    }
  }
  
  all_scores <- bind_rows(
    base_model %>% mutate(candidate_model = "one_regime"),
    if (nrow(one_break_scores) > 0) one_break_scores %>% mutate(candidate_model = "two_regimes") else data.frame(),
    if (nrow(two_break_scores) > 0) two_break_scores %>% mutate(candidate_model = "three_regimes_greedy") else data.frame()
  )
  
  list(
    selected = selected %>% mutate(regime_evidence_class = evidence_class),
    all_scores = all_scores
  )
}

fit_residual_temporal_regimes <- function(measurements, station_code_value = NA_character_, params = station_diagnostic_default_params()) {
  baseline <- fit_power_rating_baseline(measurements, params = params)
  
  if (nrow(baseline$points) == 0) {
    return(list(
      points = data.frame(),
      summary = data.frame(),
      model_scores = data.frame(),
      candidate_scores = data.frame(),
      power_model = baseline$model
    ))
  }
  
  model <- find_best_temporal_regime_model(baseline$points, params = params)
  selected <- model$selected
  breaks <- as.Date(character(0))
  
  if (nrow(selected) > 0 && !is.na(selected$break_dates[1])) {
    breaks <- as.Date(strsplit(selected$break_dates[1], ";", fixed = TRUE)[[1]])
  }
  
  regime_points <- assign_temporal_regimes(baseline$points, breaks = breaks) %>%
    mutate(
      station_code = as.character(station_code_value),
      regime_id = paste0("R", regime_number)
    )
  
  regime_summary <- regime_points %>%
    group_by(station_code, regime_number, regime_id) %>%
    summarise(
      n_points = n(),
      date_start = min(measurement_date, na.rm = TRUE),
      date_end = max(measurement_date, na.rm = TRUE),
      date_span_years = as.numeric(date_end - date_start) / 365.25,
      stage_min_cm = min(stage_cm, na.rm = TRUE),
      stage_max_cm = max(stage_cm, na.rm = TRUE),
      discharge_min_m3s = min(discharge_m3s, na.rm = TRUE),
      discharge_max_m3s = max(discharge_m3s, na.rm = TRUE),
      median_log_residual = median(power_log_residual, na.rm = TRUE),
      median_relative_residual_pct = median(power_relative_residual_pct, na.rm = TRUE),
      median_abs_log_residual = median(abs(power_log_residual), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      regime_label = paste0(
        "Regime ", regime_number,
        ": ", format(date_start, "%Y"), "–", format(date_end, "%Y")
      )
    )
  
  regime_points <- regime_points %>%
    left_join(
      regime_summary %>% select(station_code, regime_id, regime_label),
      by = c("station_code", "regime_id")
    )
  
  model_scores <- selected %>%
    mutate(
      station_code = as.character(station_code_value),
      baseline_model_type = ifelse(nrow(baseline$model) > 0, baseline$model$model_type[1], NA_character_),
      baseline_equation = ifelse(nrow(baseline$model) > 0, baseline$model$equation[1], NA_character_),
      baseline_h0_m = ifelse(nrow(baseline$model) > 0, baseline$model$h0_m[1], NA_real_),
      baseline_coefficient_a = ifelse(nrow(baseline$model) > 0, baseline$model$coefficient_a[1], NA_real_),
      baseline_coefficient_b = ifelse(nrow(baseline$model) > 0, baseline$model$coefficient_b[1], NA_real_)
    )
  
  candidate_scores <- model$all_scores %>%
    mutate(station_code = as.character(station_code_value))
  
  list(
    points = regime_points,
    summary = regime_summary,
    model_scores = model_scores,
    candidate_scores = candidate_scores,
    power_model = baseline$model
  )
}

# ------------------------------------------------------------
# Summary and index builders
# ------------------------------------------------------------

make_diagnostic_summary <- function(measurement_flags, rating_curves, best_matches = data.frame(), residual_points = data.frame(), temporal_regime = NULL, detailed = TRUE) {
  station_code_value <- NA_character_
  if (!is.null(measurement_flags) && nrow(measurement_flags) > 0 && "station_code" %in% names(measurement_flags)) {
    station_code_value <- as.character(measurement_flags$station_code[1])
  } else if (!is.null(rating_curves) && nrow(rating_curves) > 0 && "station_code" %in% names(rating_curves)) {
    station_code_value <- as.character(rating_curves$station_code[1])
  }
  
  n_measurements <- ifelse(is.null(measurement_flags), 0, nrow(measurement_flags))
  n_valid_measurements <- ifelse(
    n_measurements == 0,
    0,
    sum(!is.na(measurement_flags$stage_cm) & !is.na(measurement_flags$discharge_m3s) & measurement_flags$stage_cm > 0 & measurement_flags$discharge_m3s > 0)
  )
  
  n_stage_zero <- ifelse(n_measurements == 0, 0, sum(measurement_flags$stage_zero_or_negative_flag, na.rm = TRUE))
  n_discharge_zero <- ifelse(n_measurements == 0, 0, sum(measurement_flags$discharge_zero_or_negative_flag, na.rm = TRUE))
  n_rep_stage <- ifelse(n_measurements == 0, 0, sum(measurement_flags$repeated_stage_variable_discharge_flag, na.rm = TRUE))
  n_rep_discharge <- ifelse(n_measurements == 0, 0, sum(measurement_flags$repeated_discharge_variable_stage_flag, na.rm = TRUE))
  
  curves <- standardize_rating_curves(rating_curves)
  n_rating_curves <- ifelse(nrow(curves) == 0, 0, dplyr::n_distinct(curves$rating_curve_id))
  n_rating_curve_segments <- ifelse(nrow(curves) == 0, 0, dplyr::n_distinct(curves$rating_curve_segment_id))
  
  n_matched <- ifelse(is.null(best_matches) || nrow(best_matches) == 0, 0, nrow(best_matches))
  rating_match_fraction <- safe_divide(n_matched, n_valid_measurements)
  median_abs_rating_log_residual <- ifelse(n_matched > 0, median(abs(best_matches$rating_log_residual), na.rm = TRUE), NA_real_)
  outside_envelope_fraction <- ifelse(
    !is.null(residual_points) && nrow(residual_points) > 0 && "outside_residual_envelope" %in% names(residual_points),
    safe_divide(sum(residual_points$outside_residual_envelope, na.rm = TRUE), sum(!is.na(residual_points$outside_residual_envelope))),
    NA_real_
  )
  
  regime_evidence_class <- NA_character_
  n_temporal_regimes <- NA_integer_
  baseline_power_equation <- NA_character_
  baseline_power_h0_m <- NA_real_
  baseline_power_a <- NA_real_
  baseline_power_b <- NA_real_
  
  if (!is.null(temporal_regime) && !is.null(temporal_regime$model_scores) && nrow(temporal_regime$model_scores) > 0) {
    regime_evidence_class <- temporal_regime$model_scores$regime_evidence_class[1]
    n_temporal_regimes <- temporal_regime$model_scores$n_regimes[1]
    baseline_power_equation <- temporal_regime$model_scores$baseline_equation[1]
    baseline_power_h0_m <- temporal_regime$model_scores$baseline_h0_m[1]
    baseline_power_a <- temporal_regime$model_scores$baseline_coefficient_a[1]
    baseline_power_b <- temporal_regime$model_scores$baseline_coefficient_b[1]
  }
  
  score <- 0
  score <- score + ifelse(safe_divide(n_stage_zero, n_measurements) >= 0.05, 1, 0)
  score <- score + ifelse(safe_divide(n_discharge_zero, n_measurements) >= 0.05, 1, 0)
  score <- score + ifelse(safe_divide(n_rep_stage, n_measurements) >= 0.15, 1, 0)
  score <- score + ifelse(safe_divide(n_rep_discharge, n_measurements) >= 0.15, 1, 0)
  score <- score + ifelse(!is.na(rating_match_fraction) & rating_match_fraction < 0.60, 1, 0)
  score <- score + ifelse(!is.na(median_abs_rating_log_residual) & median_abs_rating_log_residual >= 0.25, 1, 0)
  score <- score + ifelse(!is.na(outside_envelope_fraction) & outside_envelope_fraction >= 0.15, 1, 0)
  
  diagnostic_attention_class <- dplyr::case_when(
    score <= 1 ~ "low_attention",
    score <= 3 ~ "moderate_attention",
    TRUE ~ "high_attention"
  )
  
  data.frame(
    station_code = station_code_value,
    n_measurements = n_measurements,
    n_valid_measurements = n_valid_measurements,
    n_stage_zero_or_negative = n_stage_zero,
    pct_stage_zero_or_negative = safe_divide(n_stage_zero, n_measurements),
    n_discharge_zero_or_negative = n_discharge_zero,
    pct_discharge_zero_or_negative = safe_divide(n_discharge_zero, n_measurements),
    n_repeated_stage_variable_discharge_points = n_rep_stage,
    pct_repeated_stage_variable_discharge_points = safe_divide(n_rep_stage, n_measurements),
    n_repeated_discharge_variable_stage_points = n_rep_discharge,
    pct_repeated_discharge_variable_stage_points = safe_divide(n_rep_discharge, n_measurements),
    n_rating_curves = n_rating_curves,
    n_rating_curve_segments = n_rating_curve_segments,
    rating_match_fraction = rating_match_fraction,
    median_abs_rating_log_residual = median_abs_rating_log_residual,
    outside_residual_envelope_fraction = outside_envelope_fraction,
    n_temporal_regimes = n_temporal_regimes,
    temporal_regime_evidence_class = regime_evidence_class,
    baseline_power_equation = baseline_power_equation,
    baseline_power_h0_m = baseline_power_h0_m,
    baseline_power_a = baseline_power_a,
    baseline_power_b = baseline_power_b,
    diagnostic_attention_score = score,
    diagnostic_attention_class = diagnostic_attention_class,
    diagnostic_detail_level = ifelse(detailed, "detailed_station_level", "light_station_summary"),
    stringsAsFactors = FALSE
  )
}

make_diagnostic_indices <- function(summary) {
  if (is.null(summary) || nrow(summary) == 0) return(data.frame())
  
  bind_rows(
    data.frame(
      station_code = summary$station_code,
      index_group = "Sinais nas medições",
      index_name = "Fração de cotas ≤ 0",
      index_value = summary$pct_stage_zero_or_negative,
      index_unit = "fração",
      index_class = class_percent(summary$pct_stage_zero_or_negative),
      index_description = "Fração de medições de descarga com cota ≤ 0.",
      display_order = 10,
      stringsAsFactors = FALSE
    ),
    data.frame(
      station_code = summary$station_code,
      index_group = "Sinais nas medições",
      index_name = "Fração de vazões ≤ 0",
      index_value = summary$pct_discharge_zero_or_negative,
      index_unit = "fração",
      index_class = class_percent(summary$pct_discharge_zero_or_negative),
      index_description = "Fração de medições de descarga com vazão ≤ 0.",
      display_order = 20,
      stringsAsFactors = FALSE
    ),
    data.frame(
      station_code = summary$station_code,
      index_group = "Valores repetidos",
      index_name = "Fração de cotas repetidas com vazão variável",
      index_value = summary$pct_repeated_stage_variable_discharge_points,
      index_unit = "fração",
      index_class = class_percent(summary$pct_repeated_stage_variable_discharge_points),
      index_description = "Fração de medições em grupos de cota repetida com vazão variável.",
      display_order = 30,
      stringsAsFactors = FALSE
    ),
    data.frame(
      station_code = summary$station_code,
      index_group = "Valores repetidos",
      index_name = "Fração de vazões repetidas com cota variável",
      index_value = summary$pct_repeated_discharge_variable_stage_points,
      index_unit = "fração",
      index_class = class_percent(summary$pct_repeated_discharge_variable_stage_points),
      index_description = "Fração de medições em grupos de vazão repetida com cota variável.",
      display_order = 40,
      stringsAsFactors = FALSE
    ),
    data.frame(
      station_code = summary$station_code,
      index_group = "Resíduos da curva-chave",
      index_name = "Fração pareada com curva-chave",
      index_value = summary$rating_match_fraction,
      index_unit = "fração",
      index_class = dplyr::case_when(
        is.na(summary$rating_match_fraction) ~ "not_available",
        summary$rating_match_fraction >= 0.90 ~ "high_coverage",
        summary$rating_match_fraction >= 0.60 ~ "moderate_coverage",
        TRUE ~ "low_coverage"
      ),
      index_description = "Fração de medições válidas pareadas a um segmento de curva-chave por data e cota.",
      display_order = 50,
      stringsAsFactors = FALSE
    ),
    data.frame(
      station_code = summary$station_code,
      index_group = "Resíduos da curva-chave",
      index_name = "Mediana do resíduo log absoluto",
      index_value = summary$median_abs_rating_log_residual,
      index_unit = "razão logarítmica",
      index_class = class_residual(summary$median_abs_rating_log_residual),
      index_description = "Mediana do resíduo logarítmico absoluto entre a vazão medida e a vazão estimada pela curva-chave.",
      display_order = 60,
      stringsAsFactors = FALSE
    ),
    data.frame(
      station_code = summary$station_code,
      index_group = "Regimes temporais",
      index_name = "Evidência de regimes temporais nos resíduos",
      index_value = summary$n_temporal_regimes,
      index_unit = "nº de regimes",
      index_class = ifelse(is.na(summary$temporal_regime_evidence_class), "not_available", summary$temporal_regime_evidence_class),
      index_description = "Classe de evidência da triagem de regimes temporais nos resíduos, baseada em Q = a(H - h0)^b.",
      display_order = 70,
      stringsAsFactors = FALSE
    ),
    data.frame(
      station_code = summary$station_code,
      index_group = "Resumo diagnóstico",
      index_name = "Escore de atenção diagnóstica",
      index_value = summary$diagnostic_attention_score,
      index_unit = "escore",
      index_class = summary$diagnostic_attention_class,
      index_description = "Escore preliminar de atenção para revisão visual. Não é uma nota oficial de qualidade hidrológica.",
      display_order = 80,
      stringsAsFactors = FALSE
    )
  ) %>%
    arrange(station_code, display_order)
}

# ------------------------------------------------------------
# Public station-level diagnostic function
# ------------------------------------------------------------

calculate_station_diagnostics <- function(
    measurements,
    rating_curves,
    rating_curve_summary = data.frame(),
    params = station_diagnostic_default_params(),
    detailed = TRUE) {
  
  m <- standardize_measurements(measurements)
  rc <- standardize_rating_curves(rating_curves)
  
  measurement_flags <- make_measurement_flags(m, params = params)
  repeated_group_details <- make_repeated_value_group_details(measurement_flags)
  
  curve_metadata <- make_curve_metadata(rc)
  curve_segments <- make_curve_segment_metadata(rc, curve_metadata = curve_metadata)
  rating_curve_points <- make_rating_curve_points(curve_segments, n_points = params$n_stage_points_per_segment)
  
  rating_matches <- data.frame()
  best_rating_match <- data.frame()
  residual_envelopes <- data.frame()
  residual_points <- data.frame()
  temporal_regime <- NULL
  power_curve_points <- data.frame()
  
  if (isTRUE(detailed)) {
    rating_matches <- match_measurements_to_rating_curves(measurement_flags, curve_segments)
    best_rating_match <- make_best_rating_match(rating_matches)
    residual_envelopes <- make_residual_envelopes(best_rating_match, params = params)
    residual_points <- add_envelope_flags(best_rating_match, residual_envelopes)
    
    temporal_regime <- fit_residual_temporal_regimes(measurement_flags, station_code_value = safe_first(measurement_flags$station_code), params = params)
    
    if (!is.null(temporal_regime$power_model) && nrow(temporal_regime$power_model) > 0 && nrow(measurement_flags) > 0) {
      valid_stage <- measurement_flags$stage_cm[!is.na(measurement_flags$stage_cm) & measurement_flags$stage_cm > 0]
      if (length(valid_stage) > 1) {
        power_curve_points <- make_power_curve_points(
          temporal_regime$power_model,
          stage_min_cm = min(valid_stage),
          stage_max_cm = max(valid_stage),
          params = params
        )
      }
    }
  }
  
  summary <- make_diagnostic_summary(
    measurement_flags = measurement_flags,
    rating_curves = rc,
    best_matches = best_rating_match,
    residual_points = residual_points,
    temporal_regime = temporal_regime,
    detailed = detailed
  )
  
  indices <- make_diagnostic_indices(summary)
  
  list(
    summary = summary,
    indices = indices,
    measurement_flags = measurement_flags,
    repeated_group_details = repeated_group_details,
    curve_metadata = curve_metadata,
    curve_segments = curve_segments,
    rating_curve_points = rating_curve_points,
    rating_matches = rating_matches,
    best_rating_match = best_rating_match,
    residual_envelopes = residual_envelopes,
    residual_points = residual_points,
    temporal_regime = temporal_regime,
    power_curve_points = power_curve_points
  )
}
