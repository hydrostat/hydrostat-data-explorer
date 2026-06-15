# ============================================================
# data_03_tables.R
# Purpose: Data-table preparation and Portuguese display helpers.
# ============================================================
# BEGIN ORIGINAL BODY
# ------------------------------------------------------------
# Data-table display helpers
# ------------------------------------------------------------

as_display_table <- function(x) {
  if (is.null(x)) {
    return(tibble::tibble())
  }
  
  if (is.data.frame(x)) {
    return(dplyr::as_tibble(x))
  }
  
  if (is.atomic(x)) {
    return(tibble::tibble(value = as.character(x)))
  }
  
  tibble::tibble(value = paste(utils::capture.output(str(x, max.level = 1)), collapse = " "))
}

format_numeric_table_value <- function(x, field) {
  if (length(x) == 0 || is.null(x)) {
    return(character())
  }
  
  field <- as.character(field)[[1]]
  
  if (field %in% c("station_code", "basin_code", "curve_id", "rating_curve_id", "segment_id", "rating_curve_segment_id", "cross_section_id", "cross_section_vertex_id")) {
    return(as.character(x))
  }
  
  x_num <- suppressWarnings(as.numeric(x))
  out <- rep("", length(x_num))
  ok <- !is.na(x_num) & is.finite(x_num)
  
  integer_like_field <- field %in% c(
    "consistency_level", "display_order", "n_group", "n_segments",
    "n_rating_curves", "n_rating_curve_segments", "n_measurements",
    "n_discharge_measurements", "n_valid_measurements", "n_temporal_regimes",
    "n_cross_sections", "n_cross_section_profiles", "n_cross_section_vertices"
  ) || stringr::str_detect(field, "^n_|_count$|number$")
  
  if (isTRUE(integer_like_field) && all(abs(x_num[ok] - round(x_num[ok])) < 1e-9)) {
    out[ok] <- format(round(x_num[ok]), big.mark = ",", scientific = FALSE, trim = TRUE)
  } else {
    out[ok] <- format(round(x_num[ok], 2), nsmall = 2, big.mark = ",", scientific = FALSE, trim = TRUE)
  }
  
  out
}

translate_display_values <- function(data) {
  if (nrow(data) == 0) {
    return(data)
  }
  
  replace_values <- function(x, map) {
    x_chr <- as.character(x)
    mapped <- unname(map[x_chr])
    x_chr[!is.na(mapped)] <- mapped[!is.na(mapped)]
    x_chr
  }
  
  class_map <- c(
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
  
  group_type_map <- c(
    "same_stage_variable_discharge" = "mesma cota com vazão variável",
    "same_discharge_variable_stage" = "mesma vazão com cota variável"
  )
  
  if ("index_class" %in% names(data)) {
    data$index_class <- replace_values(data$index_class, class_map)
  }
  
  if ("diagnostic_attention_class" %in% names(data)) {
    data$diagnostic_attention_class <- replace_values(data$diagnostic_attention_class, class_map)
  }
  
  if ("temporal_regime_evidence_class" %in% names(data)) {
    data$temporal_regime_evidence_class <- replace_values(data$temporal_regime_evidence_class, class_map)
  }
  
  if ("group_type" %in% names(data)) {
    data$group_type <- replace_values(data$group_type, group_type_map)
  }
  
  data
}

format_display_table_values <- function(data) {
  if (nrow(data) == 0) {
    return(data)
  }
  
  data <- translate_display_values(data)
  
  date_fields <- c(
    "measurement_datetime", "measurement_date", "valid_from", "valid_to",
    "last_update", "first_last_update", "last_last_update", "first_downloaded_at",
    "downloaded_at", "discharge_start_date", "discharge_end_date",
    "telemetric_start_date", "telemetric_end_date", "stage_start_date", "stage_end_date",
    "rainfall_start_date", "rainfall_end_date"
  )
  
  for (field in intersect(names(data), date_fields)) {
    parsed <- parse_app_datetime(data[[field]])
    if (length(parsed) == nrow(data) && any(!is.na(parsed))) {
      data[[field]] <- format(as.Date(parsed), "%Y-%m-%d")
    }
  }
  
  if ("latitude" %in% names(data)) {
    data$latitude <- purrr::map_chr(data$latitude, ~ format_coordinate_dms(.x, "latitude"))
  }
  if ("longitude" %in% names(data)) {
    data$longitude <- purrr::map_chr(data$longitude, ~ format_coordinate_dms(.x, "longitude"))
  }
  
  numeric_display_fields <- c(
    "index_value", "rating_match_fraction", "median_abs_rating_log_residual",
    "outside_residual_envelope_fraction", "rating_relative_residual_pct",
    "relative_spread", "spread_value", "stage_cm", "discharge_m3s",
    "stage_min_cm", "stage_max_cm", "discharge_min_m3s", "discharge_max_m3s",
    "wetted_area_m2", "width_m", "mean_depth_m", "mean_velocity_ms",
    "frac_stage_le_zero", "frac_discharge_le_zero",
    "frac_repeated_stage_attention", "frac_repeated_discharge_attention",
    "quality_index", "diagnostic_attention_score", "attention_score",
    "n_cross_sections", "n_cross_section_profiles", "n_cross_section_vertices"
  )
  
  for (field in names(data)) {
    if (is.numeric(data[[field]])) {
      data[[field]] <- format_numeric_table_value(data[[field]], field)
    } else if (field %in% numeric_display_fields) {
      numeric_candidate <- suppressWarnings(as.numeric(data[[field]]))
      if (any(!is.na(numeric_candidate) & is.finite(numeric_candidate))) {
        data[[field]] <- format_numeric_table_value(numeric_candidate, field)
      }
    }
  }
  
  data
}

format_nested_value_for_dt <- function(value) {
  if (is.null(value) || length(value) == 0) {
    return("")
  }
  
  if (inherits(value, "POSIXt")) {
    return(format(value[[1]], "%Y-%m-%d %H:%M:%S", tz = "UTC"))
  }
  
  if (inherits(value, "Date")) {
    return(format(value[[1]], "%Y-%m-%d"))
  }
  
  if (is.atomic(value) && length(value) == 1) {
    if (is.na(value)) {
      return("")
    }
    return(as.character(value))
  }
  
  paste(utils::capture.output(str(value, max.level = 1)), collapse = " ")
}

sanitize_vector_for_dt <- function(x) {
  if (inherits(x, "POSIXt")) {
    return(format(x, "%Y-%m-%d %H:%M:%S", tz = "UTC"))
  }
  
  if (inherits(x, "Date")) {
    return(format(x, "%Y-%m-%d"))
  }
  
  if (inherits(x, "difftime")) {
    return(as.character(x))
  }
  
  if (is.factor(x)) {
    return(as.character(x))
  }
  
  if (is.logical(x)) {
    return(dplyr::case_when(
      is.na(x) ~ "",
      x ~ "Sim",
      TRUE ~ "Não"
    ))
  }
  
  if (is.list(x) && !is.data.frame(x)) {
    return(purrr::map_chr(x, format_nested_value_for_dt))
  }
  
  if (is.matrix(x) || is.array(x)) {
    return(apply(x, 1, function(row) paste(row, collapse = "; ")))
  }
  
  if (is.numeric(x)) {
    x[!is.finite(x)] <- NA_real_
    return(x)
  }
  
  x_chr <- as.character(x)
  x_chr[is.na(x_chr)] <- ""
  x_chr
}

sanitize_table_for_dt <- function(data) {
  data <- as_display_table(data)
  
  if (nrow(data) == 0) {
    return(data)
  }
  
  data <- data %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), sanitize_vector_for_dt))
  
  names(data) <- make.unique(ifelse(names(data) == "", "Campo", names(data)), sep = " ")
  data
}

extract_diagnostic_table <- function(diagnostics, names_to_try) {
  if (!is.list(diagnostics)) {
    return(tibble::tibble())
  }
  
  for (name in names_to_try) {
    if (name %in% names(diagnostics)) {
      table <- as_display_table(diagnostics[[name]])
      if (name %in% c("indices", "diagnostic_indices")) {
        table <- enrich_index_table_with_display_labels(table)
      }
      if (name %in% c("summary", "diagnostic_summary", "station_summary")) {
        table <- enrich_summary_with_display_labels(table)
      }
      return(table)
    }
  }
  
  tibble::tibble()
}

drop_app_helper_columns <- function(data) {
  if (nrow(data) == 0) {
    return(data)
  }
  
  data %>%
    dplyr::select(-dplyr::any_of(c("measurement_datetime_app", "stage_cm_app", "discharge_m3s_app", "curve_id_app")))
}

preferred_display_column <- function(column, data) {
  display_candidates <- list(
    index_group = c("index_group_label_pt", "index_group"),
    index_name = c("index_name_label_pt", "index_name"),
    index_unit = c("index_unit_label_pt", "index_unit"),
    index_class = c("index_class_label_pt", "index_class"),
    index_description = c("index_description_pt", "index_description"),
    diagnostic_attention_class = c("diagnostic_attention_class_label_pt", "diagnostic_attention_class"),
    temporal_regime_evidence_class = c("temporal_regime_evidence_class_label_pt", "temporal_regime_evidence_class"),
    diagnostic_detail_level = c("diagnostic_detail_level_label_pt", "diagnostic_detail_level"),
    group_type = c("group_type_label_pt", "group_type"),
    map_status_code = c("map_status_label_pt", "map_status_code"),
    cross_section_record_class = c("cross_section_record_class_label_pt", "cross_section_record_class"),
    cross_section_vertex_class = c("cross_section_vertex_class_label_pt", "cross_section_vertex_class"),
    cross_section_geometry_class = c("cross_section_geometry_class_label_pt", "cross_section_geometry_class")
  )
  
  if (column %in% names(display_candidates)) {
    candidate <- first_existing_name(data, display_candidates[[column]])
    if (!is.na(candidate)) {
      return(candidate)
    }
  }
  
  column
}

expand_preferred_columns_for_display <- function(data, preferred_columns) {
  if (is.null(preferred_columns)) {
    return(NULL)
  }
  
  preferred <- purrr::map_chr(preferred_columns, preferred_display_column, data = data)
  
  if (any(preferred_columns %in% c("index_name", "index_value", "index_class")) && "index_symbol" %in% names(data)) {
    insert_after <- match(preferred_display_column("index_name", data), preferred)
    if (!is.na(insert_after) && !"index_symbol" %in% preferred) {
      preferred <- append(preferred, "index_symbol", after = insert_after)
    }
  }
  
  unique(preferred)
}

prepare_display_table <- function(data, preferred_columns = NULL, keep_only_preferred = FALSE) {
  data <- as_display_table(data)
  
  if (nrow(data) == 0) {
    return(data)
  }
  
  data <- drop_app_helper_columns(data)
  
  preferred_columns_display <- expand_preferred_columns_for_display(data, preferred_columns)
  
  if (!is.null(preferred_columns_display)) {
    preferred <- preferred_columns_display[preferred_columns_display %in% names(data)]
    if (length(preferred) > 0) {
      if (isTRUE(keep_only_preferred)) {
        data <- data %>% dplyr::select(dplyr::all_of(preferred))
      } else {
        remaining <- setdiff(names(data), preferred)
        data <- data %>% dplyr::select(dplyr::all_of(c(preferred, remaining)))
      }
    }
  }
  
  data <- format_display_table_values(data)
  names(data) <- make.unique(purrr::map_chr(names(data), field_label), sep = " ")
  sanitize_table_for_dt(data)
}


