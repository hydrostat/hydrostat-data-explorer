# ============================================================
# data_01_core.R
# Purpose: Database access, display dictionaries, spatial loading, and generic app helpers.
# ============================================================
# BEGIN ORIGINAL BODY
# ------------------------------------------------------------
# Database helpers
# ------------------------------------------------------------

check_shiny_database <- function(db_path) {
  if (!file.exists(db_path)) {
    stop("Missing Shiny database: ", db_path, call. = FALSE)
  }
}

connect_shiny_database <- function() {
  check_shiny_database(app_config$db_path)
  DBI::dbConnect(duckdb::duckdb(), dbdir = app_config$db_path, read_only = TRUE)
}

disconnect_shiny_database <- function(con) {
  try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE)
}

app_db_tables <- function(con) {
  DBI::dbListTables(con)
}

app_table_exists <- function(con, table_name) {
  table_name %in% app_db_tables(con)
}

app_table_fields <- function(con, table_name) {
  if (!app_table_exists(con, table_name)) {
    return(character())
  }
  DBI::dbListFields(con, table_name)
}

read_app_table <- function(con, table_name) {
  if (!app_table_exists(con, table_name)) {
    return(tibble::tibble())
  }
  dplyr::as_tibble(DBI::dbReadTable(con, table_name))
}

read_app_table_columns <- function(con, table_name, columns) {
  if (!app_table_exists(con, table_name)) {
    return(tibble::tibble())
  }
  
  available <- intersect(columns, app_table_fields(con, table_name))
  if (length(available) == 0) {
    return(tibble::tibble())
  }
  
  table_sql <- as.character(DBI::dbQuoteIdentifier(con, table_name))
  column_sql <- paste(as.character(DBI::dbQuoteIdentifier(con, available)), collapse = ", ")
  query <- paste0("select distinct ", column_sql, " from ", table_sql)
  
  dplyr::as_tibble(DBI::dbGetQuery(con, query))
}

read_station_table <- function(con, table_name, station_code) {
  if (!app_table_exists(con, table_name)) {
    return(tibble::tibble())
  }
  
  station_code_sql <- as.character(DBI::dbQuoteString(con, as.character(station_code)))
  table_sql <- as.character(DBI::dbQuoteIdentifier(con, table_name))
  
  query <- paste0(
    "select * from ", table_sql,
    " where cast(station_code as varchar) = ", station_code_sql
  )
  
  dplyr::as_tibble(DBI::dbGetQuery(con, query))
}

# ------------------------------------------------------------
# Cached display dictionaries from the Shiny database
# ------------------------------------------------------------

.app_display_cache <- new.env(parent = emptyenv())
.app_display_cache$field_labels <- tibble::tibble()
.app_display_cache$value_labels <- tibble::tibble()
.app_display_cache$diagnostic_index_labels <- tibble::tibble()
.app_display_cache$quality_index_labels <- tibble::tibble()

set_app_display_dictionary <- function(con) {
  data_dictionary <- read_app_table(con, "data_dictionary")
  data_dictionary_values <- read_app_table(con, "data_dictionary_values")
  
  if (nrow(data_dictionary) > 0) {
    required <- c("column_name", "label_pt")
    if (all(required %in% names(data_dictionary))) {
      .app_display_cache$field_labels <- data_dictionary %>%
        dplyr::filter(!is.na(column_name), column_name != "") %>%
        dplyr::mutate(
          column_name = as.character(column_name),
          label_pt = as.character(label_pt)
        ) %>%
        dplyr::distinct(column_name, .keep_all = TRUE)
    }
  }
  
  if (nrow(data_dictionary_values) > 0) {
    required <- c("column_name", "value_code", "value_label_pt")
    if (all(required %in% names(data_dictionary_values))) {
      .app_display_cache$value_labels <- data_dictionary_values %>%
        dplyr::filter(!is.na(column_name), !is.na(value_code)) %>%
        dplyr::mutate(
          column_name = as.character(column_name),
          value_code = as.character(value_code),
          value_label_pt = as.character(value_label_pt)
        ) %>%
        dplyr::distinct(column_name, value_code, .keep_all = TRUE)
    }
  }
  
  .app_display_cache$diagnostic_index_labels <- read_app_table_columns(
    con,
    "station_diagnostic_indices",
    c(
      "index_group", "index_group_label_pt",
      "index_name", "index_name_label_pt",
      "index_unit", "index_unit_label_pt",
      "index_class", "index_class_label_pt",
      "index_description", "index_description_pt",
      "index_symbol", "index_formula_pt", "index_interpretation_pt",
      "display_order"
    )
  )
  
  .app_display_cache$quality_index_labels <- read_app_table_columns(
    con,
    "station_quality_indices",
    c(
      "index_group", "index_group_label_pt",
      "index_name", "index_name_label_pt",
      "index_unit", "index_unit_label_pt",
      "index_class", "index_class_label_pt",
      "index_description", "index_description_pt",
      "index_symbol", "index_formula_pt", "index_interpretation_pt",
      "display_order"
    )
  )
  
  invisible(TRUE)
}

cached_field_label <- function(field) {
  field <- as.character(field)[[1]]
  
  labels <- .app_display_cache$field_labels
  if (is.data.frame(labels) && nrow(labels) > 0 && all(c("column_name", "label_pt") %in% names(labels))) {
    label <- labels %>%
      dplyr::filter(column_name == field) %>%
      dplyr::pull(label_pt) %>%
      head(1)
    
    if (length(label) == 1 && !is.na(label) && label != "") {
      return(label)
    }
  }
  
  NA_character_
}

cached_value_label <- function(field_name, field_value) {
  if (
    is.null(field_name) || is.na(field_name) ||
    is.null(field_value) || length(field_value) == 0 ||
    is.na(field_value)
  ) {
    return(NA_character_)
  }
  
  field_name_chr <- as.character(field_name)[[1]]
  field_value_chr <- as.character(field_value)[[1]]
  
  labels <- .app_display_cache$value_labels
  
  if (
    is.data.frame(labels) &&
    nrow(labels) > 0 &&
    all(c("column_name", "value_code", "value_label_pt") %in% names(labels))
  ) {
    label <- labels %>%
      dplyr::filter(
        .data$column_name == .env$field_name_chr,
        .data$value_code == .env$field_value_chr
      ) %>%
      dplyr::pull(.data$value_label_pt) %>%
      head(1)
    
    if (length(label) == 1 && !is.na(label) && label != "") {
      return(label)
    }
  }
  
  NA_character_
}
# ------------------------------------------------------------
# Generic helpers
# ------------------------------------------------------------

load_spatial_layers <- function(spatial_layers_path) {
  if (is.null(spatial_layers_path) || !file.exists(spatial_layers_path)) {
    return(list())
  }
  
  layers <- tryCatch(
    readRDS(spatial_layers_path),
    error = function(e) list()
  )
  
  if (!is.list(layers)) {
    return(list())
  }
  
  layers[
    vapply(
      layers,
      function(x) !is.null(x) && is.data.frame(x) && nrow(x) > 0,
      logical(1)
    )
  ]
}

first_existing_name <- function(data, candidates) {
  candidates <- candidates[candidates %in% names(data)]
  if (length(candidates) == 0) {
    return(NA_character_)
  }
  candidates[[1]]
}

select_existing <- function(data, candidates) {
  candidates[candidates %in% names(data)]
}

as_numeric_app <- function(x) {
  if (is.numeric(x)) {
    return(as.numeric(x))
  }
  x_chr <- stringr::str_replace_all(as.character(x), ",", ".")
  suppressWarnings(as.numeric(x_chr))
}

parse_app_datetime <- function(x) {
  if (inherits(x, "POSIXt")) {
    return(x)
  }
  if (inherits(x, "Date")) {
    return(as.POSIXct(x, tz = "UTC"))
  }
  
  x_chr <- as.character(x)
  x_chr <- stringr::str_replace(x_chr, "Z$", "")
  x_chr <- stringr::str_replace_all(x_chr, "T", " ")
  
  suppressWarnings(as.POSIXct(
    x_chr,
    tz = "UTC",
    tryFormats = c(
      "%Y-%m-%d %H:%M:%OS",
      "%Y-%m-%d %H:%M:%S",
      "%Y-%m-%d",
      "%d/%m/%Y %H:%M:%S",
      "%d/%m/%Y"
    )
  ))
}

is_missing_value <- function(x) {
  length(x) == 0 || is.null(x) || is.na(x) || identical(as.character(x), "")
}

format_coordinate_dms <- function(value, field = NULL) {
  value <- suppressWarnings(as.numeric(value[[1]]))
  if (is.na(value)) {
    return("—")
  }
  
  direction <- ""
  if (!is.null(field) && identical(field, "latitude")) {
    direction <- ifelse(value < 0, "S", "N")
  }
  if (!is.null(field) && identical(field, "longitude")) {
    direction <- ifelse(value < 0, "W", "E")
  }
  
  value_abs <- abs(value)
  degrees <- floor(value_abs)
  minutes_float <- (value_abs - degrees) * 60
  minutes <- floor(minutes_float)
  seconds <- round((minutes_float - minutes) * 60)
  
  if (seconds >= 60) {
    seconds <- 0
    minutes <- minutes + 1
  }
  if (minutes >= 60) {
    minutes <- 0
    degrees <- degrees + 1
  }
  
  paste0(
    sprintf("%02d", degrees), "° ",
    sprintf("%02d", minutes), "' ",
    sprintf("%02d", seconds), "\"",
    ifelse(direction == "", "", paste0(" ", direction))
  )
}

format_app_date_value <- function(x) {
  if (inherits(x, "Date")) {
    return(format(x[[1]], "%Y-%m-%d"))
  }
  if (inherits(x, "POSIXt")) {
    return(format(as.Date(x[[1]]), "%Y-%m-%d"))
  }
  
  parsed <- parse_app_datetime(x)
  if (length(parsed) > 0 && !is.na(parsed[[1]])) {
    return(format(as.Date(parsed[[1]]), "%Y-%m-%d"))
  }
  
  as.character(x[[1]])
}

is_boolean_display_field <- function(field) {
  if (is.null(field) || is.na(field)) {
    return(FALSE)
  }
  
  field <- as.character(field)
  field %in% c(
    "is_operating",
    "has_discharge_measurements",
    "has_telemetry",
    "has_stage_data",
    "has_rainfall_data",
    "has_measurements",
    "has_rating_curves",
    "has_rating_curve"
  ) ||
    stringr::str_starts(field, "is_") ||
    stringr::str_starts(field, "has_") ||
    stringr::str_starts(field, "flag_") ||
    stringr::str_ends(field, "_flag")
}

field_label <- function(field) {
  if (is.null(field) || length(field) == 0 || is.na(field) || identical(as.character(field), "")) {
    return("Campo")
  }
  
  field <- as.character(field)[[1]]
  
  fallback_field_labels <- c(
    "station_code" = "Código da estação",
    "station_name" = "Nome da estação",
    "station_type" = "Tipo de estação",
    "uf" = "UF",
    "uf_name" = "Nome da UF",
    "municipality" = "Município",
    "basin_code" = "Código da bacia",
    "basin_name" = "Bacia hidrográfica",
    "sub_basin_code" = "Código da sub-bacia",
    "river_name" = "Rio",
    "operator" = "Operadora",
    "responsible_agency" = "Responsável",
    "is_operating" = "Em operação",
    "latitude" = "Latitude",
    "longitude" = "Longitude",
    "altitude" = "Altitude",
    "drainage_area" = "Área de drenagem",
    "last_update" = "Última atualização",
    
    "discharge_start_date" = "Início das medições de vazão",
    "discharge_end_date" = "Fim das medições de vazão",
    "telemetric_start_date" = "Início da telemetria",
    "telemetric_end_date" = "Fim da telemetria",
    "stage_start_date" = "Início da série de cotas",
    "stage_end_date" = "Fim da série de cotas",
    "rainfall_start_date" = "Início da série de chuva",
    "rainfall_end_date" = "Fim da série de chuva",
    
    "key" = "Chave",
    "value" = "Valor",
    "table_or_view" = "Tabela ou view",
    
    "n_discharge_measurements" = "Medições de descarga",
    "n_measurements" = "Número de medições",
    "n_valid_measurements" = "Medições válidas",
    "n_rating_curves" = "Curvas-chave",
    "n_rating_curve_segments" = "Segmentos de curva-chave",
    "n_cross_sections" = "Seções transversais",
    "n_cross_section_profiles" = "Perfis de seção transversal",
    "n_cross_section_vertices" = "Vértices de seção transversal",
    
    "measurement_datetime" = "Data/hora da medição",
    "measurement_date" = "Data da medição",
    "measurement_year" = "Ano da medição",
    "consistency_level" = "Nível de consistência",
    "stage_cm" = "Cota",
    "discharge_m3s" = "Vazão",
    "wetted_area_m2" = "Área molhada",
    "width_m" = "Largura",
    "mean_depth_m" = "Profundidade média",
    "mean_velocity_ms" = "Velocidade média",
    
    "valid_from" = "Início da vigência",
    "valid_to" = "Fim da vigência",
    "stage_min_cm" = "Cota mínima",
    "stage_max_cm" = "Cota máxima",
    "discharge_min_m3s" = "Vazão mínima",
    "discharge_max_m3s" = "Vazão máxima",
    "equation_display" = "Equação",
    
    "curve_id" = "ID da curva",
    "rating_curve_id" = "ID da curva-chave",
    "segment_id" = "ID do segmento",
    "rating_curve_segment_id" = "ID do segmento da curva-chave",
    "segment_number" = "Segmento",
    "curve_label" = "Curva-chave",
    "curve_segment_label" = "Segmento da curva-chave",
    
    "index_group" = "Grupo do índice",
    "index_group_label_pt" = "Grupo do índice",
    "index_name" = "Nome do índice",
    "index_name_label_pt" = "Nome do índice",
    "index_symbol" = "Sigla",
    "index_value" = "Valor do índice",
    "index_unit" = "Unidade do índice",
    "index_unit_label_pt" = "Unidade do índice",
    "index_class" = "Classe do índice",
    "index_class_label_pt" = "Classe do índice",
    "index_description" = "Descrição do índice",
    "index_description_pt" = "Descrição do índice",
    "index_formula_pt" = "Fórmula",
    "index_interpretation_pt" = "Interpretação",
    "display_order" = "Ordem",
    
    "diagnostic_attention_score" = "Escore de atenção diagnóstica",
    "diagnostic_attention_class" = "Classe de atenção diagnóstica",
    "diagnostic_attention_class_label_pt" = "Classe de atenção diagnóstica",
    "rating_match_fraction" = "Fração pareada com curva-chave",
    "median_abs_rating_log_residual" = "Mediana do resíduo log absoluto",
    "outside_residual_envelope_fraction" = "Fração fora do envelope empírico",
    "n_temporal_regimes" = "Número de regimes temporais",
    "temporal_regime_evidence_class" = "Evidência de regime temporal",
    "temporal_regime_evidence_class_label_pt" = "Evidência de regime temporal",
    "diagnostic_detail_level" = "Nível do diagnóstico",
    "diagnostic_detail_level_label_pt" = "Nível do diagnóstico",
    
    "rating_predicted_discharge_m3s" = "Vazão estimada pela curva-chave",
    "rating_log_residual" = "Resíduo logarítmico da curva-chave",
    "rating_relative_residual_pct" = "Resíduo relativo da curva-chave (%)",
    "outside_residual_envelope" = "Fora do envelope empírico",
    "envelope_lower_log_residual" = "Limite inferior do envelope",
    "envelope_upper_log_residual" = "Limite superior do envelope",
    "has_residual_envelope" = "Possui envelope empírico",
    
    "stage_zero_or_negative_flag" = "Cota ≤ 0",
    "discharge_zero_or_negative_flag" = "Vazão ≤ 0",
    "repeated_stage_variable_discharge_flag" = "Cota repetida com vazão variável",
    "repeated_discharge_variable_stage_flag" = "Vazão repetida com cota variável",
    "any_obvious_measurement_attention_flag" = "Algum sinal de atenção",
    "stage_group" = "Grupo de cota",
    "discharge_group" = "Grupo de vazão",
    
    "group_type" = "Tipo de grupo",
    "group_type_label_pt" = "Tipo de grupo",
    "group_value" = "Valor do grupo",
    "n_group" = "Número de medições no grupo",
    "spread_value" = "Amplitude no grupo",
    "relative_spread" = "Amplitude relativa",
    
    "regime_number" = "Número do regime",
    "regime_id" = "ID do regime",
    "regime_label" = "Regime",
    "power_predicted_discharge_m3s" = "Vazão estimada pela curva de referência",
    "power_log_residual" = "Resíduo logarítmico da curva de referência",
    "power_relative_residual_pct" = "Resíduo relativo da curva de referência (%)",
    
    "cross_section_id" = "ID da seção transversal",
    "cross_section_vertex_id" = "ID do vértice da seção transversal",
    "cross_section_record_class" = "Classe dos registros de seção transversal",
    "cross_section_record_class_label_pt" = "Classe dos registros de seção transversal",
    "cross_section_vertex_class" = "Classe dos vértices de seção transversal",
    "cross_section_vertex_class_label_pt" = "Classe dos vértices de seção transversal",
    "cross_section_geometry_class" = "Classe geométrica da seção transversal",
    "cross_section_geometry_class_label_pt" = "Classe geométrica da seção transversal",
    "first_cross_section_datetime" = "Primeira seção transversal",
    "last_cross_section_datetime" = "Última seção transversal",
    "cross_section_period_years" = "Período com seções transversais",
    "cross_section_distance_span_m" = "Amplitude horizontal da seção",
    "cross_section_stage_range_cm" = "Amplitude de cotas da seção",
    
    "survey_number" = "Número do levantamento",
    "section_type" = "Tipo de seção",
    "source_record_id" = "ID do registro de origem",
    "n_vertices" = "Número de vértices",
    "n_vertices_reported" = "Número de vértices informado",
    "distance_pipf_m" = "Distância PIPF (m)",
    "x_distance_min_m" = "Distância mínima informada (m)",
    "x_distance_max_m" = "Distância máxima informada (m)",
    "y_stage_min_cm" = "Cota mínima informada (cm)",
    "y_stage_max_cm" = "Cota máxima informada (cm)",
    "geometry_stage_step_cm" = "Intervalo vertical da geometria (cm)",
    "vertex_distance_min_m" = "Distância mínima dos vértices (m)",
    "vertex_distance_max_m" = "Distância máxima dos vértices (m)",
    "vertex_stage_min_cm" = "Cota mínima dos vértices (cm)",
    "vertex_stage_max_cm" = "Cota máxima dos vértices (cm)",
    "n_missing_vertex_distance" = "Vértices sem distância",
    "n_missing_vertex_stage" = "Vértices sem cota",
    "observation" = "Observação",
    "source_route" = "Rota de origem",
    "first_downloaded_at" = "Primeiro download",
    "last_downloaded_at" = "Último download",
    "downloaded_at" = "Download",
    "processed_at" = "Processado em",
    "vertex_order" = "Ordem do vértice",
    "vertex_distance_m" = "Distância do vértice (m)",
    "vertex_stage_cm" = "Cota do vértice (cm)",
    "n_consistency_level_1" = "Seções com consistência 1",
    "n_consistency_level_2" = "Seções com consistência 2",
    "n_section_type_1" = "Seções tipo 1",
    "n_section_type_2" = "Seções tipo 2",
    "min_vertex_distance_m" = "Menor distância dos vértices (m)",
    "max_vertex_distance_m" = "Maior distância dos vértices (m)",
    "min_vertex_stage_cm" = "Menor cota dos vértices (cm)",
    "max_vertex_stage_cm" = "Maior cota dos vértices (cm)"
    
    
  )
  
  fallback_label <- unname(fallback_field_labels[field])
  if (length(fallback_label) == 1 && !is.na(fallback_label) && fallback_label != "") {
    return(fallback_label)
  }
  
  dictionary_label <- cached_field_label(field)
  if (!is.na(dictionary_label) && dictionary_label != "" && dictionary_label != field) {
    return(dictionary_label)
  }
  
  if (exists("app_field_labels", inherits = TRUE)) {
    label <- unname(app_field_labels[field])
    if (length(label) == 1 && !is.na(label) && !identical(label, "")) {
      return(label)
    }
  }
  
  field %>%
    stringr::str_replace_all("_", " ") %>%
    stringr::str_to_sentence()
}

value_as_text <- function(x, field = NULL) {
  if (length(x) == 0 || is.null(x)) {
    return("—")
  }
  
  x <- x[[1]]
  
  if (is.na(x) || identical(as.character(x), "")) {
    return("—")
  }
  
  if (!is.null(field)) {
    dictionary_label <- cached_value_label(field, x)
    if (!is.na(dictionary_label) && dictionary_label != "") {
      return(dictionary_label)
    }
  }
  
  if (is.logical(x)) {
    return(ifelse(isTRUE(x), "Sim", "Não"))
  }
  
  if (inherits(x, "POSIXt")) {
    return(format(x, "%Y-%m-%d %H:%M"))
  }
  
  if (inherits(x, "Date")) {
    return(format(x, "%Y-%m-%d"))
  }
  
  date_fields <- c(
    "discharge_start_date", "discharge_end_date", "stage_start_date", "stage_end_date",
    "rainfall_start_date", "rainfall_end_date", "telemetric_start_date", "telemetric_end_date",
    "valid_from", "valid_to", "last_update", "first_last_update", "last_last_update", "first_downloaded_at",
    "downloaded_at", "measurement_date", "measurement_datetime"
  )
  if (!is.null(field) && field %in% date_fields) {
    return(format_app_date_value(x))
  }
  
  if (is.numeric(x)) {
    if (!is.null(field) && field %in% c("latitude", "longitude")) {
      return(format_coordinate_dms(x, field))
    }
    
    if (!is.null(field) && stringr::str_starts(field, "frac_")) {
      return(scales::percent(x, accuracy = 0.1))
    }
    
    if (abs(x) >= 1000) {
      return(format(round(x, 2), big.mark = ",", scientific = FALSE, trim = TRUE))
    }
    
    return(format(round(x, 3), scientific = FALSE, trim = TRUE))
  }
  
  x_chr <- as.character(x)
  x_lower <- stringr::str_to_lower(x_chr)
  
  if (is_boolean_display_field(field)) {
    if (x_lower %in% c("true", "t", "yes", "y", "sim", "s", "1")) {
      return("Sim")
    }
    if (x_lower %in% c("false", "f", "no", "n", "nao", "não", "0")) {
      return("Não")
    }
  }
  
  if (!is.null(field) && field %in% c("station_type")) {
    x_chr <- stringr::str_replace_all(x_chr, "Fluviometrica", "Fluviométrica")
    x_chr <- stringr::str_replace_all(x_chr, "Pluviometrica", "Pluviométrica")
  }
  
  x_chr
}

coerce_logical_indicator <- function(x) {
  if (is.logical(x)) {
    return(dplyr::coalesce(x, FALSE))
  }
  
  if (is.numeric(x)) {
    return(!is.na(x) & x > 0)
  }
  
  x_chr <- tolower(trimws(as.character(x)))
  x_chr %in% c("true", "t", "yes", "y", "sim", "s", "1")
}

infer_any_indicator <- function(data, candidates) {
  columns <- candidates[candidates %in% names(data)]
  
  if (length(columns) == 0) {
    return(rep(FALSE, nrow(data)))
  }
  
  Reduce(
    `|`,
    lapply(columns, function(column) coerce_logical_indicator(data[[column]])),
    init = rep(FALSE, nrow(data))
  )
}

infer_positive_count_indicator <- function(data, candidates) {
  columns <- candidates[candidates %in% names(data)]
  
  if (length(columns) == 0) {
    return(rep(FALSE, nrow(data)))
  }
  
  Reduce(
    `|`,
    lapply(columns, function(column) {
      value <- suppressWarnings(as.numeric(data[[column]]))
      !is.na(value) & value > 0
    }),
    init = rep(FALSE, nrow(data))
  )
}

infer_nonmissing_date_indicator <- function(data, candidates) {
  columns <- candidates[candidates %in% names(data)]
  
  if (length(columns) == 0) {
    return(rep(FALSE, nrow(data)))
  }
  
  Reduce(
    `|`,
    lapply(columns, function(column) {
      value <- data[[column]]
      !is.na(value) & trimws(as.character(value)) != ""
    }),
    init = rep(FALSE, nrow(data))
  )
}

infer_inventory_availability_indicator <- function(data, flag_candidates = character(), date_candidates = character()) {
  infer_any_indicator(data, flag_candidates) |
    infer_nonmissing_date_indicator(data, date_candidates)
}


