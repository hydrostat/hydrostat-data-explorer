# ============================================================
# data_02_station.R
# Purpose: Station index, station readers, rating-curve display, sidebar, and diagnostic-label helpers.
# ============================================================
# BEGIN ORIGINAL BODY
# ------------------------------------------------------------
# Station index and product availability
# ------------------------------------------------------------

normalize_map_status <- function(x) {
  x_lower <- stringr::str_to_lower(as.character(x))
  
  dplyr::case_when(
    stringr::str_detect(x_lower, "rating|curve|curva") &
      stringr::str_detect(x_lower, "measurement|medicao|medi[cç][aã]o|descarga|discharge") ~
      "with_measurements_and_rating_curves",
    stringr::str_detect(x_lower, "measurement|medicao|medi[cç][aã]o|descarga|discharge") ~
      "with_measurements_only",
    stringr::str_detect(x_lower, "registration|cadastro|inventory|inventario|invent[aá]rio|only") ~
      "registration_only",
    TRUE ~ NA_character_
  )
}

derive_station_map_group <- function(stations) {
  if ("has_product_rating_curves" %in% names(stations) || "has_product_discharge_summary" %in% names(stations)) {
    has_rating_curves <- if ("has_product_rating_curves" %in% names(stations)) coerce_logical_indicator(stations$has_product_rating_curves) else rep(FALSE, nrow(stations))
    has_discharge <- if ("has_product_discharge_summary" %in% names(stations)) coerce_logical_indicator(stations$has_product_discharge_summary) else rep(FALSE, nrow(stations))
    
    return(dplyr::case_when(
      has_rating_curves ~ "with_measurements_and_rating_curves",
      has_discharge ~ "with_measurements_only",
      TRUE ~ "registration_only"
    ))
  }
  
  status_column <- first_existing_name(
    stations,
    c("map_group", "map_status_code", "map_status", "station_map_status", "station_status", "data_status")
  )
  
  if (!is.na(status_column)) {
    normalized <- normalize_map_status(stations[[status_column]])
    if (any(!is.na(normalized))) {
      return(dplyr::coalesce(normalized, "registration_only"))
    }
  }
  
  has_measurements <- infer_any_indicator(
    stations,
    c(
      "has_discharge_measurements", "has_measurements", "has_discharge_data",
      "n_measurements", "n_discharge_measurements", "n_measurement_records",
      "measurement_count", "discharge_measurement_count"
    )
  )
  
  has_rating_curves <- infer_any_indicator(
    stations,
    c(
      "has_rating_curves", "has_rating_curve", "n_rating_curves",
      "n_rating_curve_segments", "n_curve_segments", "rating_curve_count"
    )
  )
  
  dplyr::case_when(
    has_measurements & has_rating_curves ~ "with_measurements_and_rating_curves",
    has_measurements ~ "with_measurements_only",
    TRUE ~ "registration_only"
  )
}

add_station_search_label <- function(stations) {
  station_name_col <- first_existing_name(stations, c("station_name", "name", "station"))
  uf_col <- first_existing_name(stations, c("uf", "state", "state_abbrev"))
  municipality_col <- first_existing_name(stations, c("municipality", "municipio", "city"))
  
  station_name <- if (!is.na(station_name_col)) stations[[station_name_col]] else rep("Estação", nrow(stations))
  uf <- if (!is.na(uf_col)) stations[[uf_col]] else rep(NA_character_, nrow(stations))
  municipality <- if (!is.na(municipality_col)) stations[[municipality_col]] else rep(NA_character_, nrow(stations))
  
  stations %>%
    dplyr::mutate(
      station_search_label = paste0(
        station_code,
        " — ",
        dplyr::coalesce(as.character(station_name), "Estação sem nome"),
        ifelse(!is.na(uf), paste0(" / ", uf), ""),
        ifelse(!is.na(municipality), paste0(" — ", municipality), "")
      )
    )
}

join_station_summary <- function(stations, summary_table) {
  if (nrow(summary_table) == 0 || !("station_code" %in% names(summary_table))) {
    return(stations)
  }
  
  summary_table <- summary_table %>%
    dplyr::mutate(station_code = as.character(station_code)) %>%
    dplyr::distinct(station_code, .keep_all = TRUE)
  
  new_columns <- setdiff(names(summary_table), names(stations))
  if (length(new_columns) == 0) {
    return(stations)
  }
  
  dplyr::left_join(
    stations,
    summary_table %>% dplyr::select(dplyr::all_of(c("station_code", new_columns))),
    by = "station_code"
  )
}

map_group_priority_value <- function(map_group) {
  dplyr::case_when(
    map_group == "with_measurements_and_rating_curves" ~ 1L,
    map_group == "with_measurements_only" ~ 2L,
    map_group == "registration_only" ~ 3L,
    TRUE ~ 9L
  )
}

map_product_layer_priority_value <- function(map_layer) {
  dplyr::case_when(
    map_layer == "flu_registration" ~ 1L,
    map_layer == "rainfall_registration" ~ 2L,
    map_layer == "flu_rainfall_registration" ~ 3L,
    map_layer == "flu_with_data" ~ 4L,
    map_layer == "rainfall_with_data" ~ 5L,
    map_layer == "flu_rainfall_with_data" ~ 6L,
    TRUE ~ 9L
  )
}

ensure_station_product_indicators <- function(stations) {
  if (nrow(stations) == 0) {
    return(stations)
  }
  
  ensure_logical <- function(data, field, fallback) {
    if (field %in% names(data)) {
      coerce_logical_indicator(data[[field]])
    } else {
      fallback
    }
  }
  
  fallback_discharge <- infer_positive_count_indicator(
    stations,
    c(
      "n_measurements", "n_discharge_measurements",
      "n_measurement_records", "n_discharge_measurement_records",
      "measurement_count", "discharge_measurement_count"
    )
  )
  
  fallback_rating <- infer_positive_count_indicator(
    stations,
    c(
      "n_rating_curves", "n_rating_curve_segments",
      "n_curve_segments", "rating_curve_count",
      "rating_curve_segment_count"
    )
  )
  
  fallback_cross_sections <- infer_positive_count_indicator(
    stations,
    c(
      "n_cross_sections", "n_cross_section_profiles", "n_cross_section_vertices",
      "cross_section_count", "cross_section_profile_count", "cross_section_vertex_count"
    )
  )
  station_type_text <- if ("station_type" %in% names(stations)) {
    as.character(stations$station_type)
  } else {
    rep("", nrow(stations))
  }
  
  is_fluviometric_station <- stringr::str_detect(
    stringr::str_to_lower(station_type_text),
    "fluvi"
  )
  
  is_pluviometric_station <- stringr::str_detect(
    stringr::str_to_lower(station_type_text),
    "pluvi"
  )
  fallback_flu <- infer_inventory_availability_indicator(
    stations,
    flag_candidates = c("has_discharge_data", "has_discharge_measurements", "has_inventory_flu_data"),
    date_candidates = c("discharge_start_date")
  ) | is_fluviometric_station
  
  fallback_rainfall <- infer_inventory_availability_indicator(
    stations,
    flag_candidates = c("has_rainfall_data", "has_inventory_rainfall_data"),
    date_candidates = c("rainfall_start_date")
  ) | is_pluviometric_station
  
  fallback_stage <- infer_inventory_availability_indicator(
    stations,
    flag_candidates = c("has_stage_data", "has_inventory_stage_data"),
    date_candidates = c("stage_start_date")
  )
  
  fallback_telemetry <- infer_inventory_availability_indicator(
    stations,
    flag_candidates = c("has_telemetry", "has_inventory_telemetry"),
    date_candidates = c("telemetric_start_date")
  )
  
  stations %>%
    dplyr::mutate(
      has_station_registration = ensure_logical(., "has_station_registration", rep(TRUE, nrow(.))),
      has_product_discharge_summary = ensure_logical(., "has_product_discharge_summary", fallback_discharge),
      has_product_rating_curves = ensure_logical(., "has_product_rating_curves", fallback_rating),
      has_product_cross_sections = ensure_logical(., "has_product_cross_sections", fallback_cross_sections),
      has_inventory_flu_data = ensure_logical(., "has_inventory_flu_data", fallback_flu),
      has_inventory_rainfall_data = ensure_logical(., "has_inventory_rainfall_data", fallback_rainfall),
      has_inventory_stage_data = ensure_logical(., "has_inventory_stage_data", fallback_stage),
      has_inventory_telemetry = ensure_logical(., "has_inventory_telemetry", fallback_telemetry),
      has_product_flu_data = ensure_logical(., "has_product_flu_data", rep(FALSE, nrow(.))),
      has_product_rainfall_data = ensure_logical(., "has_product_rainfall_data", rep(FALSE, nrow(.))),
      has_product_stage_data = ensure_logical(., "has_product_stage_data", rep(FALSE, nrow(.)))
    )
}

add_station_product_indicators <- ensure_station_product_indicators

enforce_unique_station_index <- function(stations) {
  if (nrow(stations) == 0 || !("station_code" %in% names(stations))) {
    return(stations)
  }
  
  stations %>%
    dplyr::mutate(
      station_code = stringr::str_trim(as.character(station_code)),
      map_group_priority = map_group_priority_value(map_group)
    ) %>%
    dplyr::arrange(station_code, map_group_priority) %>%
    dplyr::distinct(station_code, .keep_all = TRUE) %>%
    dplyr::select(-map_group_priority)
}

load_station_index <- function(con) {
  set_app_display_dictionary(con)
  
  stations <- read_app_table(con, "stations_minimal")
  
  if (nrow(stations) == 0) {
    stop("Table stations_minimal was not found or is empty in exports/shiny_minimal.duckdb.", call. = FALSE)
  }
  
  stations <- stations %>%
    dplyr::mutate(station_code = stringr::str_trim(as.character(station_code))) %>%
    dplyr::filter(!is.na(station_code), station_code != "") %>%
    dplyr::distinct(station_code, .keep_all = TRUE)
  
  priority_summary_tables <- c(
    "station_product_availability",
    "station_assessment_summary",
    "station_data_availability",
    "station_diagnostic_summary",
    "station_measurement_indices",
    "station_rating_curve_indices",
    "station_cross_section_indices",
    "station_discharge_products_summary",
    "station_map_status"
  )
  
  for (table_name in priority_summary_tables) {
    stations <- join_station_summary(stations, read_app_table(con, table_name))
  }
  
  stations <- stations %>%
    ensure_station_product_indicators()
  
  if (!"map_status_label_pt" %in% names(stations)) {
    stations$map_status_label_pt <- NA_character_
  }
  
  stations <- stations %>%
    dplyr::mutate(
      map_group = derive_station_map_group(.),
      map_group_label = dplyr::case_when(
        !is.na(map_status_label_pt) & map_status_label_pt != "" ~ as.character(map_status_label_pt),
        map_group == "with_measurements_and_rating_curves" ~ "Medições + curvas-chave",
        map_group == "with_measurements_only" ~ "Somente medições",
        TRUE ~ "Somente cadastro"
      )
    ) %>%
    enforce_unique_station_index() %>%
    add_station_search_label()
  
  stations
}

load_source_metadata <- function(con) {
  metadata <- read_app_table(con, "metadata")
  
  if (nrow(metadata) == 0) {
    return(tibble::tibble(key = "metadata", value = "Nenhuma tabela de metadados foi encontrada no banco do Shiny."))
  }
  
  if (all(c("key", "value") %in% names(metadata))) {
    return(metadata %>% dplyr::select(key, value))
  }
  
  metadata
}

# ------------------------------------------------------------
# Station-level data readers
# ------------------------------------------------------------

standardize_measurements_for_app <- function(measurements) {
  if (nrow(measurements) == 0) {
    return(measurements)
  }
  
  date_col <- first_existing_name(
    measurements,
    c("measurement_datetime", "measurement_date", "date", "datetime", "data_medicao", "Data_Medicao")
  )
  stage_col <- first_existing_name(
    measurements,
    c("stage_cm", "stage", "cota_cm", "cota", "water_level_cm", "h_cm")
  )
  discharge_col <- first_existing_name(
    measurements,
    c("discharge_m3s", "discharge", "vazao", "flow_m3s", "q_m3s")
  )
  
  measurements %>%
    dplyr::mutate(
      measurement_datetime_app = if (!is.na(date_col)) parse_app_datetime(.data[[date_col]]) else as.POSIXct(NA, tz = "UTC"),
      stage_cm_app = if (!is.na(stage_col)) as_numeric_app(.data[[stage_col]]) else NA_real_,
      discharge_m3s_app = if (!is.na(discharge_col)) as_numeric_app(.data[[discharge_col]]) else NA_real_
    )
}

standardize_rating_curves_for_app <- function(rating_curves) {
  if (nrow(rating_curves) == 0) {
    return(rating_curves)
  }
  
  stage_col <- first_existing_name(
    rating_curves,
    c("stage_cm", "stage", "cota_cm", "cota", "water_level_cm", "h_cm")
  )
  discharge_col <- first_existing_name(
    rating_curves,
    c("discharge_m3s", "discharge", "vazao", "flow_m3s", "q_m3s")
  )
  curve_col <- first_existing_name(
    rating_curves,
    c("curve_id", "rating_curve_id", "rating_id", "curve_code", "rating_curve_code", "segment_id")
  )
  
  rating_curves %>%
    dplyr::mutate(
      stage_cm_app = if (!is.na(stage_col)) as_numeric_app(.data[[stage_col]]) else NA_real_,
      discharge_m3s_app = if (!is.na(discharge_col)) as_numeric_app(.data[[discharge_col]]) else NA_real_,
      curve_id_app = if (!is.na(curve_col)) as.character(.data[[curve_col]]) else "rating_curve"
    )
}

get_station_measurements <- function(con, station_code) {
  read_station_table(con, "discharge_measurements", station_code) %>%
    standardize_measurements_for_app()
}

get_station_rating_curves <- function(con, station_code) {
  read_station_table(con, "rating_curves", station_code) %>%
    standardize_rating_curves_for_app()
}

get_station_rating_curve_summary <- function(con, station_code) {
  read_station_table(con, "rating_curve_summary", station_code)
}

get_station_cross_sections <- function(con, station_code) {
  read_station_table(con, "cross_sections", station_code)
}

get_station_cross_section_vertices <- function(con, station_code) {
  read_station_table(con, "cross_section_vertices", station_code)
}

get_station_cross_section_summary <- function(con, station_code) {
  read_station_table(con, "cross_section_summary", station_code)
}

# ------------------------------------------------------------
# Rating-curve display helpers
# ------------------------------------------------------------

format_rating_coefficient <- function(x, digits = 3) {
  x <- suppressWarnings(as.numeric(x))
  out <- rep(NA_character_, length(x))
  ok <- !is.na(x) & is.finite(x)
  out[ok] <- format(round(x[ok], digits), nsmall = digits, scientific = FALSE, trim = TRUE)
  out
}

first_numeric_column <- function(data, candidates) {
  column <- first_existing_name(data, candidates)
  if (is.na(column)) {
    return(rep(NA_real_, nrow(data)))
  }
  as_numeric_app(data[[column]])
}

add_rating_curve_equation_display <- function(data) {
  data <- as_display_table(data)
  if (nrow(data) == 0) {
    return(data)
  }
  
  if ("equation_display" %in% names(data)) {
    return(data)
  }
  
  a <- first_numeric_column(
    data,
    c("coefficient_a", "coef_a", "a", "param_a", "rating_a", "rating_curve_a", "CoeficienteA", "Coeficiente_A")
  )
  h0 <- first_numeric_column(
    data,
    c("coefficient_h0", "coef_h0", "h0", "param_h0", "rating_h0", "rating_curve_h0", "h0_m", "h0_cm", "CoeficienteH0", "Coeficiente_H0")
  )
  b <- first_numeric_column(
    data,
    c("coefficient_b", "coefficient_n", "coef_b", "coef_n", "b", "n", "param_b", "param_n", "exponent", "exponent_b", "rating_b", "rating_curve_b", "CoeficienteB", "CoeficienteN", "Coeficiente_B", "Coeficiente_N")
  )
  
  valid <- !is.na(a) & is.finite(a) & !is.na(h0) & is.finite(h0) & !is.na(b) & is.finite(b)
  
  equation <- rep("—", nrow(data))
  if (any(valid)) {
    sign_text <- ifelse(h0[valid] < 0, " + ", " − ")
    equation[valid] <- paste0(
      "<span class='rating-equation'>Q = ",
      format_rating_coefficient(a[valid], 3),
      "(H", sign_text, format_rating_coefficient(abs(h0[valid]), 3), ")<sup>",
      format_rating_coefficient(b[valid], 3),
      "</sup></span>"
    )
  }
  
  data$equation_display <- equation
  data
}

# ------------------------------------------------------------
# Sidebar and selected-station display helpers
# ------------------------------------------------------------

station_display_title <- function(station_row) {
  station_name <- value_as_text(station_row$station_name, "station_name")
  station_code <- value_as_text(station_row$station_code, "station_code")
  uf <- if ("uf" %in% names(station_row)) value_as_text(station_row$uf, "uf") else "—"
  municipality <- if ("municipality" %in% names(station_row)) value_as_text(station_row$municipality, "municipality") else "—"
  
  list(
    title = station_name,
    subtitle = paste0(station_code, " • ", municipality, " / ", uf),
    code = station_code
  )
}

station_metadata_fields <- function(station_row) {
  candidates <- c(
    "station_code", "station_name", "station_type", "uf", "municipality",
    "basin_code", "basin_name", "river_name", "operator", "responsible_agency",
    "is_operating", "latitude", "longitude", "drainage_area",
    "discharge_start_date", "discharge_end_date", "telemetric_start_date", "telemetric_end_date"
  )
  
  columns <- select_existing(station_row, candidates)
  
  tibble::tibble(
    field = columns,
    label = purrr::map_chr(columns, field_label),
    value = purrr::map_chr(columns, ~ value_as_text(station_row[[.x]], .x))
  )
}

station_metadata_detail_fields <- function(station_row) {
  candidates <- c(
    "station_code", "station_name", "station_type", "uf", "municipality",
    "basin_code", "basin_name", "river_name", "operator", "responsible_agency",
    "is_operating", "latitude", "longitude", "drainage_area",
    "discharge_start_date", "discharge_end_date", "telemetric_start_date", "telemetric_end_date",
    "stage_start_date", "stage_end_date", "rainfall_start_date", "rainfall_end_date",
    "last_update"
  )
  
  columns <- select_existing(station_row, candidates)
  
  tibble::tibble(
    field = columns,
    label = purrr::map_chr(columns, field_label),
    value = purrr::map_chr(columns, ~ value_as_text(station_row[[.x]], .x))
  )
}

station_indicator_value <- function(station_row, flag_candidates = character(), count_candidates = character(), date_candidates = character()) {
  flag_column <- first_existing_name(station_row, flag_candidates)
  if (!is.na(flag_column)) {
    flag_value <- station_row[[flag_column]][[1]]
    if (!is.na(flag_value)) {
      return(isTRUE(coerce_logical_indicator(flag_value)))
    }
  }
  
  count_column <- first_existing_name(station_row, count_candidates)
  if (!is.na(count_column)) {
    count_value <- suppressWarnings(as.numeric(station_row[[count_column]][[1]]))
    if (!is.na(count_value)) {
      return(count_value > 0)
    }
  }
  
  date_column <- first_existing_name(station_row, date_candidates)
  if (!is.na(date_column)) {
    date_value <- station_row[[date_column]][[1]]
    return(!is_missing_value(date_value))
  }
  
  FALSE
}

availability_status_symbol <- function(available) {
  ifelse(isTRUE(available), "\u2713", "\u2715")
}

station_availability_badge_fields <- function(station_row) {
  labels <- c(
    "Resumo de descarga",
    "Curvas-chave",
    "Seções transversais",
    "Dados flu (cadastro ANA)",
    "Dados plu (cadastro ANA)",
    "Cotagrama (cadastro ANA)"
  )
  
  available <- c(
    station_indicator_value(
      station_row,
      flag_candidates = c("has_product_discharge_summary"),
      count_candidates = c("n_discharge_measurements", "n_measurements", "measurement_count"),
      date_candidates = character()
    ),
    station_indicator_value(
      station_row,
      flag_candidates = c("has_product_rating_curves"),
      count_candidates = c("n_rating_curves", "n_rating_curve_segments", "rating_curve_count"),
      date_candidates = character()
    ),
    station_indicator_value(
      station_row,
      flag_candidates = c("has_product_cross_sections", "has_cross_sections_processed"),
      count_candidates = c("n_cross_sections", "n_cross_section_profiles", "n_cross_section_vertices"),
      date_candidates = character()
    ),
    station_indicator_value(
      station_row,
      flag_candidates = c("has_inventory_flu_data", "has_discharge_data", "has_discharge_measurements"),
      count_candidates = character(),
      date_candidates = c("discharge_start_date")
    ),
    station_indicator_value(
      station_row,
      flag_candidates = c("has_inventory_rainfall_data", "has_rainfall_data"),
      count_candidates = character(),
      date_candidates = c("rainfall_start_date")
    ),
    station_indicator_value(
      station_row,
      flag_candidates = c("has_inventory_stage_data", "has_stage_data"),
      count_candidates = character(),
      date_candidates = c("stage_start_date")
    )
  )
  
  tibble::tibble(
    label = labels,
    available = available,
    status = purrr::map_chr(available, availability_status_symbol)
  )
}

station_attention_fields <- function(station_row) {
  candidates <- c(
    "diagnostic_attention_class_label_pt",
    "diagnostic_attention_score",
    "temporal_regime_evidence_class_label_pt",
    "rating_match_fraction",
    "median_abs_rating_log_residual",
    "cross_section_record_class_label_pt",
    "cross_section_geometry_class_label_pt"
  )
  
  columns <- select_existing(station_row, candidates)
  
  if (length(columns) == 0) {
    return(tibble::tibble(
      field = "diagnostic_summary",
      label = "Resumo diagnóstico",
      value = "Nenhuma tabela de resumo diagnóstico foi encontrada."
    ))
  }
  
  tibble::tibble(
    field = columns,
    label = purrr::map_chr(columns, field_label),
    value = purrr::map_chr(columns, ~ value_as_text(station_row[[.x]], .x))
  )
}

first_station_value <- function(station_row, candidates) {
  column <- first_existing_name(station_row, candidates)
  if (is.na(column)) {
    return(NA)
  }
  station_row[[column]][[1]]
}

inventory_availability_text <- function(station_row, label_candidates, flag_candidates = character(), date_candidates = character()) {
  label_column <- first_existing_name(station_row, label_candidates)
  if (!is.na(label_column)) {
    value <- station_row[[label_column]][[1]]
    if (!is_missing_value(value)) {
      return(as.character(value))
    }
  }
  
  available <- station_indicator_value(
    station_row,
    flag_candidates = flag_candidates,
    count_candidates = character(),
    date_candidates = date_candidates
  )
  
  ifelse(isTRUE(available), "Sim", "Não")
}

station_kpi_fields <- function(station_row) {
  tibble::tibble(
    label = c("Medições descarga", "Curvas-chave", "Seções", "Dados flu", "Dados plu", "Cotas"),
    value = c(
      value_as_text(
        first_station_value(
          station_row,
          c("n_discharge_measurements", "n_measurements", "n_measurement_records", "measurement_count")
        ),
        "n_discharge_measurements"
      ),
      value_as_text(
        first_station_value(
          station_row,
          c("n_rating_curves", "n_rating_curve_segments", "rating_curve_count")
        ),
        "n_rating_curves"
      ),
      value_as_text(
        first_station_value(
          station_row,
          c("n_cross_sections", "n_cross_section_profiles", "n_cross_section_vertices")
        ),
        "n_cross_sections"
      ),
      inventory_availability_text(
        station_row,
        label_candidates = c("has_inventory_flu_data_label_pt"),
        flag_candidates = c("has_inventory_flu_data", "has_discharge_data", "has_discharge_measurements"),
        date_candidates = c("discharge_start_date")
      ),
      inventory_availability_text(
        station_row,
        label_candidates = c("has_inventory_rainfall_data_label_pt"),
        flag_candidates = c("has_inventory_rainfall_data", "has_rainfall_data"),
        date_candidates = c("rainfall_start_date")
      ),
      inventory_availability_text(
        station_row,
        label_candidates = c("has_inventory_stage_data_label_pt"),
        flag_candidates = c("has_inventory_stage_data", "has_stage_data"),
        date_candidates = c("stage_start_date")
      )
    )
  )
}

# ------------------------------------------------------------
# Diagnostic fallback and canonicalization
# ------------------------------------------------------------

fallback_station_diagnostics <- function(measurements, rating_curves) {
  if (nrow(measurements) == 0) {
    return(list(
      summary = tibble::tibble(message = "Nenhuma medição de descarga disponível para esta estação."),
      measurement_flags = tibble::tibble(),
      repeated_stage_groups = tibble::tibble(),
      repeated_discharge_groups = tibble::tibble(),
      note = "Nenhum resultado de diagnóstico sob demanda foi retornado."
    ))
  }
  
  measurements_flagged <- measurements %>%
    dplyr::mutate(
      flag_stage_le_zero = !is.na(stage_cm_app) & stage_cm_app <= 0,
      flag_discharge_le_zero = !is.na(discharge_m3s_app) & discharge_m3s_app <= 0
    )
  
  repeated_stage_groups <- measurements_flagged %>%
    dplyr::filter(!is.na(stage_cm_app), !is.na(discharge_m3s_app)) %>%
    dplyr::mutate(stage_cm_rounded = round(stage_cm_app, 0)) %>%
    dplyr::group_by(stage_cm_rounded) %>%
    dplyr::summarise(
      n_measurements = dplyr::n(),
      discharge_min_m3s = min(discharge_m3s_app, na.rm = TRUE),
      discharge_max_m3s = max(discharge_m3s_app, na.rm = TRUE),
      discharge_range_m3s = discharge_max_m3s - discharge_min_m3s,
      .groups = "drop"
    ) %>%
    dplyr::filter(n_measurements >= 5, discharge_range_m3s > 0) %>%
    dplyr::arrange(dplyr::desc(n_measurements))
  
  repeated_discharge_groups <- measurements_flagged %>%
    dplyr::filter(!is.na(stage_cm_app), !is.na(discharge_m3s_app)) %>%
    dplyr::mutate(discharge_m3s_rounded = round(discharge_m3s_app, 2)) %>%
    dplyr::group_by(discharge_m3s_rounded) %>%
    dplyr::summarise(
      n_measurements = dplyr::n(),
      stage_min_cm = min(stage_cm_app, na.rm = TRUE),
      stage_max_cm = max(stage_cm_app, na.rm = TRUE),
      stage_range_cm = stage_max_cm - stage_min_cm,
      .groups = "drop"
    ) %>%
    dplyr::filter(n_measurements >= 5, stage_range_cm > 0) %>%
    dplyr::arrange(dplyr::desc(n_measurements))
  
  valid_dates <- measurements_flagged$measurement_datetime_app[!is.na(measurements_flagged$measurement_datetime_app)]
  
  diagnostic_summary <- tibble::tibble(
    n_measurements = nrow(measurements_flagged),
    date_start = if (length(valid_dates) > 0) min(valid_dates) else as.POSIXct(NA, tz = "UTC"),
    date_end = if (length(valid_dates) > 0) max(valid_dates) else as.POSIXct(NA, tz = "UTC"),
    n_stage_le_zero = sum(measurements_flagged$flag_stage_le_zero, na.rm = TRUE),
    n_discharge_le_zero = sum(measurements_flagged$flag_discharge_le_zero, na.rm = TRUE),
    n_repeated_stage_groups = nrow(repeated_stage_groups),
    n_repeated_discharge_groups = nrow(repeated_discharge_groups),
    n_rating_curve_rows = nrow(rating_curves),
    diagnostic_source = "fallback_app_diagnostics"
  )
  
  list(
    summary = diagnostic_summary,
    measurement_flags = measurements_flagged,
    repeated_stage_groups = repeated_stage_groups,
    repeated_discharge_groups = repeated_discharge_groups,
    note = "Diagnóstico simplificado. Diagnósticos completos de resíduos e regimes temporais exigem R/station_diagnostic_functions.R."
  )
}

canonicalize_measurements_for_diagnostics <- function(measurements) {
  if (is.null(measurements) || nrow(measurements) == 0) {
    return(tibble::tibble())
  }
  
  measurements <- dplyr::as_tibble(measurements)
  
  if (!"station_code" %in% names(measurements)) {
    measurements$station_code <- NA_character_
  }
  
  if ("measurement_datetime_app" %in% names(measurements)) {
    measurements$measurement_datetime <- measurements$measurement_datetime_app
    measurements$measurement_date <- as.Date(measurements$measurement_datetime_app)
  } else if ("measurement_datetime" %in% names(measurements)) {
    measurements$measurement_datetime <- parse_app_datetime(measurements$measurement_datetime)
    measurements$measurement_date <- as.Date(measurements$measurement_datetime)
  } else if ("measurement_date" %in% names(measurements)) {
    measurements$measurement_date <- as.Date(measurements$measurement_date)
    measurements$measurement_datetime <- as.POSIXct(measurements$measurement_date, tz = "UTC")
  } else {
    measurements$measurement_date <- as.Date(NA)
    measurements$measurement_datetime <- as.POSIXct(NA, tz = "UTC")
  }
  
  if ("stage_cm_app" %in% names(measurements)) {
    measurements$stage_cm <- as_numeric_app(measurements$stage_cm_app)
  } else if ("stage_cm" %in% names(measurements)) {
    measurements$stage_cm <- as_numeric_app(measurements$stage_cm)
  }
  
  if ("discharge_m3s_app" %in% names(measurements)) {
    measurements$discharge_m3s <- as_numeric_app(measurements$discharge_m3s_app)
  } else if ("discharge_m3s" %in% names(measurements)) {
    measurements$discharge_m3s <- as_numeric_app(measurements$discharge_m3s)
  }
  
  measurements %>%
    dplyr::mutate(station_code = as.character(station_code))
}

canonicalize_rating_curves_for_diagnostics <- function(rating_curves) {
  if (is.null(rating_curves) || nrow(rating_curves) == 0) {
    return(tibble::tibble())
  }
  
  curves <- dplyr::as_tibble(rating_curves)
  
  if (!"station_code" %in% names(curves)) {
    curves$station_code <- NA_character_
  }
  
  if (!"rating_curve_id" %in% names(curves)) {
    curve_col <- first_existing_name(curves, c("curve_id", "rating_curve_code", "curve_code", "rating_id"))
    curves$rating_curve_id <- if (!is.na(curve_col)) as.character(curves[[curve_col]]) else as.character(seq_len(nrow(curves)))
  }
  
  if (!"rating_curve_segment_id" %in% names(curves)) {
    segment_col <- first_existing_name(curves, c("segment_id", "rating_segment_id", "curve_segment_id"))
    curves$rating_curve_segment_id <- if (!is.na(segment_col)) as.character(curves[[segment_col]]) else paste0(curves$rating_curve_id, "_", seq_len(nrow(curves)))
  }
  
  if (!"segment_number" %in% names(curves)) {
    segment_col <- first_existing_name(curves, c("segment_id", "segment", "segmento"))
    if (!is.na(segment_col)) {
      curves$segment_number <- suppressWarnings(as.integer(curves[[segment_col]]))
    } else {
      curves$segment_number <- ave(seq_len(nrow(curves)), curves$rating_curve_id, FUN = seq_along)
    }
  }
  
  for (field in c("stage_min_cm", "stage_max_cm", "coefficient_a", "coefficient_h0", "coefficient_n")) {
    if (field %in% names(curves)) {
      curves[[field]] <- as_numeric_app(curves[[field]])
    }
  }
  
  if ("valid_from" %in% names(curves)) {
    curves$valid_from <- as.Date(parse_app_datetime(curves$valid_from))
  }
  if ("valid_to" %in% names(curves)) {
    curves$valid_to <- as.Date(parse_app_datetime(curves$valid_to))
  }
  
  curves %>%
    dplyr::mutate(
      station_code = as.character(station_code),
      rating_curve_id = as.character(rating_curve_id),
      rating_curve_segment_id = as.character(rating_curve_segment_id)
    )
}

# ------------------------------------------------------------
# Enrichment of on-demand diagnostics with DB display labels
# ------------------------------------------------------------

coalesce_text_column <- function(data, target, source) {
  if (!source %in% names(data)) {
    return(data)
  }
  
  if (!target %in% names(data)) {
    data[[target]] <- data[[source]]
    return(data)
  }
  
  missing_target <- is.na(data[[target]]) | trimws(as.character(data[[target]])) == ""
  data[[target]][missing_target] <- data[[source]][missing_target]
  data
}

enrich_index_table_with_display_labels <- function(indices) {
  indices <- as_display_table(indices)
  if (nrow(indices) == 0) {
    return(indices)
  }
  
  reference <- .app_display_cache$diagnostic_index_labels
  if (!is.data.frame(reference) || nrow(reference) == 0 || !"index_name" %in% names(reference) || !"index_name" %in% names(indices)) {
    return(indices)
  }
  
  label_columns <- c(
    "index_group_label_pt", "index_name_label_pt", "index_unit_label_pt",
    "index_class_label_pt", "index_description_pt",
    "index_symbol", "index_formula_pt", "index_interpretation_pt"
  )
  
  reference <- reference %>%
    dplyr::select(dplyr::any_of(c("index_name", label_columns))) %>%
    dplyr::filter(!is.na(index_name), index_name != "") %>%
    dplyr::distinct(index_name, .keep_all = TRUE)
  
  if (nrow(reference) == 0) {
    return(indices)
  }
  
  suffix <- "__db_label"
  joined <- dplyr::left_join(indices, reference, by = "index_name", suffix = c("", suffix))
  
  for (column in label_columns) {
    source_column <- paste0(column, suffix)
    if (source_column %in% names(joined)) {
      joined <- coalesce_text_column(joined, column, source_column)
    }
  }
  
  joined %>% dplyr::select(-dplyr::ends_with(suffix))
}

enrich_group_details_with_display_labels <- function(table) {
  table <- as_display_table(table)
  if (nrow(table) == 0 || !"group_type" %in% names(table)) {
    return(table)
  }
  
  if (!"group_type_label_pt" %in% names(table)) {
    table$group_type_label_pt <- purrr::map_chr(table$group_type, function(x) {
      label <- cached_value_label("group_type", x)
      ifelse(is.na(label), as.character(x), label)
    })
  }
  
  table
}

enrich_summary_with_display_labels <- function(summary) {
  summary <- as_display_table(summary)
  if (nrow(summary) == 0) {
    return(summary)
  }
  
  class_pairs <- list(
    diagnostic_attention_class = "diagnostic_attention_class_label_pt",
    temporal_regime_evidence_class = "temporal_regime_evidence_class_label_pt",
    diagnostic_detail_level = "diagnostic_detail_level_label_pt",
    cross_section_record_class = "cross_section_record_class_label_pt",
    cross_section_vertex_class = "cross_section_vertex_class_label_pt",
    cross_section_geometry_class = "cross_section_geometry_class_label_pt"
  )
  
  for (code_column in names(class_pairs)) {
    label_column <- class_pairs[[code_column]]
    if (code_column %in% names(summary) && !label_column %in% names(summary)) {
      summary[[label_column]] <- purrr::map_chr(summary[[code_column]], function(x) {
        label <- cached_value_label(code_column, x)
        ifelse(is.na(label), as.character(x), label)
      })
    }
  }
  
  summary
}

enrich_diagnostics_with_display_labels <- function(diagnostics) {
  if (!is.list(diagnostics)) {
    return(diagnostics)
  }
  
  if ("indices" %in% names(diagnostics)) {
    diagnostics$indices <- enrich_index_table_with_display_labels(diagnostics$indices)
  }
  if ("diagnostic_indices" %in% names(diagnostics)) {
    diagnostics$diagnostic_indices <- enrich_index_table_with_display_labels(diagnostics$diagnostic_indices)
  }
  if ("summary" %in% names(diagnostics)) {
    diagnostics$summary <- enrich_summary_with_display_labels(diagnostics$summary)
  }
  if ("diagnostic_summary" %in% names(diagnostics)) {
    diagnostics$diagnostic_summary <- enrich_summary_with_display_labels(diagnostics$diagnostic_summary)
  }
  if ("repeated_group_details" %in% names(diagnostics)) {
    diagnostics$repeated_group_details <- enrich_group_details_with_display_labels(diagnostics$repeated_group_details)
  }
  
  diagnostics
}

run_on_demand_station_diagnostics <- function(
    station_code,
    station_row,
    measurements,
    rating_curves,
    rating_curve_summary = tibble::tibble()) {
  
  measurements_for_diagnostics <- canonicalize_measurements_for_diagnostics(measurements)
  rating_curves_for_diagnostics <- canonicalize_rating_curves_for_diagnostics(rating_curves)
  
  diagnostic_environment <- if (exists("app_diagnostic_env", inherits = TRUE)) app_diagnostic_env else globalenv()
  
  if (exists("calculate_station_diagnostics", mode = "function", envir = diagnostic_environment, inherits = TRUE)) {
    diagnostic_function <- get("calculate_station_diagnostics", mode = "function", envir = diagnostic_environment, inherits = TRUE)
    
    result <- tryCatch(
      diagnostic_function(
        measurements = measurements_for_diagnostics,
        rating_curves = rating_curves_for_diagnostics,
        rating_curve_summary = rating_curve_summary,
        detailed = TRUE
      ),
      error = function(e) e
    )
    
    if (!inherits(result, "error")) {
      result$diagnostic_function_used <- "calculate_station_diagnostics"
      return(enrich_diagnostics_with_display_labels(result))
    }
    
    fallback <- fallback_station_diagnostics(measurements, rating_curves)
    fallback$diagnostic_error <- conditionMessage(result)
    return(enrich_diagnostics_with_display_labels(fallback))
  }
  
  enrich_diagnostics_with_display_labels(fallback_station_diagnostics(measurements, rating_curves))
}

filter_flagged_measurements_for_display <- function(measurement_flags) {
  table <- as_display_table(measurement_flags)
  if (nrow(table) == 0) {
    return(table)
  }
  
  flag_columns <- c(
    "stage_zero_or_negative_flag",
    "discharge_zero_or_negative_flag",
    "repeated_stage_variable_discharge_flag",
    "repeated_discharge_variable_stage_flag",
    "any_obvious_measurement_attention_flag",
    "flag_stage_le_zero",
    "flag_discharge_le_zero"
  )
  flag_columns <- flag_columns[flag_columns %in% names(table)]
  
  if (length(flag_columns) == 0) {
    return(table)
  }
  
  keep <- Reduce(`|`, lapply(flag_columns, function(column) {
    value <- table[[column]]
    if (is.logical(value)) {
      return(dplyr::coalesce(value, FALSE))
    }
    as.character(value) %in% c("TRUE", "True", "true", "1", "Sim", "sim")
  }))
  
  table[keep, , drop = FALSE]
}

extract_repeated_group_details <- function(diagnostics, group_type_value) {
  table <- extract_diagnostic_table(diagnostics, c("repeated_group_details"))
  
  if (nrow(table) == 0) {
    if (identical(group_type_value, "same_stage_variable_discharge")) {
      return(extract_diagnostic_table(diagnostics, c("repeated_stage_groups", "same_stage_groups")))
    }
    return(extract_diagnostic_table(diagnostics, c("repeated_discharge_groups", "same_discharge_groups")))
  }
  
  if (!"group_type" %in% names(table)) {
    return(table)
  }
  
  table %>%
    enrich_group_details_with_display_labels() %>%
    dplyr::filter(group_type == group_type_value)
}

extract_temporal_regime_table <- function(diagnostics, table_name) {
  if (!is.list(diagnostics) || !"temporal_regime" %in% names(diagnostics)) {
    return(tibble::tibble())
  }
  
  temporal_regime <- diagnostics$temporal_regime
  if (!is.list(temporal_regime) || !table_name %in% names(temporal_regime)) {
    return(tibble::tibble())
  }
  
  as_display_table(temporal_regime[[table_name]])
}


