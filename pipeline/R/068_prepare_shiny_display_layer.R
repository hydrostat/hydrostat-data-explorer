# ============================================================
# pipeline/R/068_prepare_shiny_display_layer.R
#
# Purpose:
# Add Shiny-ready Portuguese display fields, labels, value
# dictionaries, and product-availability summaries to the local
# Shiny DuckDB export.
#
# Input:
#   exports/shiny_minimal.duckdb
#
# Required previous steps:
#   pipeline/R/060_export_shiny_minimal.R
#   pipeline/R/061_check_shiny_export_local.R
#   pipeline/R/062_calculate_station_quality_indices.R
#   pipeline/R/063_calculate_station_diagnostic_summaries.R
#   pipeline/R/067_prepare_shiny_spatial_layers.R, if spatial layers are used
#
# Main outputs written to the same DuckDB database:
#   station_product_availability
#   data_dictionary
#   data_dictionary_values
#
# Also updates, when available:
#   station_quality_indices
#   station_diagnostic_indices
#   station_diagnostic_summary
#   station_data_availability
#   station_assessment_summary
#   station_map_status
#
# This script does not call ANA APIs, does not use credentials,
# and does not alter hydrological diagnostic logic.
# ============================================================

# Load packages
library(DBI)
library(duckdb)
library(dplyr)
library(tidyr)
library(stringr)

# Load shared pipeline helpers
source(file.path("pipeline", "helpers", "duckdb_helpers.R"), local = TRUE)

# ------------------------------------------------------------
# Paths
# ------------------------------------------------------------

shiny_db <- file.path("exports", "shiny_minimal.duckdb")
output_dir <- file.path("outputs", "station_assessment")

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(shiny_db)) {
  stop("Missing Shiny export database: ", shiny_db)
}

message("============================================================")
message("068_prepare_shiny_display_layer")
message("============================================================")
message("Input database: ", shiny_db)
message("This script adds display fields and dictionaries only.")

# ------------------------------------------------------------
# Helper functions
# ------------------------------------------------------------

sql_string <- function(con, x) {
  as.character(DBI::dbQuoteString(con, x))
}

recode_with_fallback <- function(
    x,
    map,
    code_col = "code",
    label_col = "label_pt",
    missing_label = "não classificado"
) {
  x_chr <- as.character(x)
  
  out <- map[[label_col]][match(x_chr, as.character(map[[code_col]]))]
  
  out <- ifelse(
    is.na(x_chr) | x_chr == "",
    missing_label,
    out
  )
  
  out <- ifelse(
    is.na(out) | out == "",
    x_chr,
    out
  )
  
  out
}

get_col <- function(df, col, default = NA) {
  if (col %in% names(df)) {
    df[[col]]
  } else {
    rep(default, nrow(df))
  }
}

safe_count <- function(con, table_name) {
  DBI::dbGetQuery(
    con,
    paste0("SELECT COUNT(*) AS n FROM ", quote_ident(table_name))
  )$n[1]
}

has_table <- function(con, table_name) {
  table_name %in% DBI::dbListTables(con)
}

yes_no_label <- function(x) {
  case_when(
    is.na(x) ~ "não informado",
    x ~ "sim",
    TRUE ~ "não"
  )
}

# ------------------------------------------------------------
# Connect
# ------------------------------------------------------------

con <- DBI::dbConnect(duckdb::duckdb(), shiny_db, read_only = FALSE)
on.exit(DBI::dbDisconnect(con), add = TRUE)

required_tables <- c(
  "stations_minimal",
  "station_data_availability",
  "station_assessment_summary",
  "station_map_status",
  "station_quality_indices"
)

missing_tables <- setdiff(required_tables, DBI::dbListTables(con))

if (length(missing_tables) > 0) {
  stop(
    "Missing required table(s): ",
    paste(missing_tables, collapse = ", "),
    ". Run scripts 060, 062 and 063 before this script."
  )
}

# ------------------------------------------------------------
# Display dictionaries
# ------------------------------------------------------------

index_group_map <- data.frame(
  code = c(
    "Station registration",
    "Discharge measurements",
    "Rating curves",
    "Cross sections",
    "Station assessment",
    "Future indices",
    "Diagnostic calculation",
    "Measurement flags",
    "Repeated values",
    "Rating-curve matching",
    "Rating-curve residuals",
    "Temporal regimes",
    "Diagnostic attention"
  ),
  label_pt = c(
    "Cadastro da estação",
    "Medições de descarga",
    "Curvas-chave",
    "Seções transversais",
    "Resumo da estação",
    "Indicadores futuros",
    "Cálculo diagnóstico",
    "Sinais nas medições",
    "Valores repetidos",
    "Pareamento com curva-chave",
    "Resíduos da curva-chave",
    "Regimes temporais",
    "Atenção diagnóstica"
  ),
  stringsAsFactors = FALSE
)

index_name_map <- data.frame(
  code = c(
    "Coordinates available",
    "Drainage area available",
    "Registered discharge period available",
    "Processed discharge measurements available",
    "Number of discharge measurements",
    "Years with discharge measurements",
    "Stage availability in discharge measurements",
    "Discharge availability in discharge measurements",
    "Processed rating curves available",
    "Number of rating curves",
    "Number of rating-curve segments",
    "Overlapping rating-curve validity pairs",
    "Processed cross-section profiles available",
    "Number of cross-section profiles",
    "Number of cross-section vertices",
    "Cross-section distance span",
    "Preliminary information score",
    "Daily stage/discharge gap indicators",
    "Discharge without valid rating curve",
    
    "Fraction of stage <= 0",
    "Fraction of stages <= 0",
    "Fraction of zero or negative stage values",
    "Fraction of discharge <= 0",
    "Fraction of discharges <= 0",
    "Fraction of zero or negative discharge values",
    "Fraction of repeated stage with variable discharge",
    "Fraction of repeated discharge with variable stage",
    "Fraction paired with rating curve",
    "Rating-curve match fraction",
    "Median absolute log residual",
    "Median absolute rating log residual",
    "Fraction outside empirical envelope",
    "Temporal-regime evidence",
    "Number of temporal regimes",
    "Diagnostic attention score"
  ),
  label_pt = c(
    "Coordenadas disponíveis",
    "Área de drenagem disponível",
    "Período cadastral de descarga líquida disponível",
    "Medições de descarga processadas disponíveis",
    "Número de medições de descarga",
    "Anos com medições de descarga",
    "Disponibilidade de cota nas medições de descarga",
    "Disponibilidade de vazão nas medições de descarga",
    "Curvas-chave processadas disponíveis",
    "Número de curvas-chave",
    "Número de segmentos de curva-chave",
    "Pares de curvas-chave com vigência sobreposta",
    "Seções transversais processadas disponíveis",
    "Número de perfis de seção transversal",
    "Número de vértices de seções transversais",
    "Extensão horizontal da seção transversal",
    "Escore preliminar de informação",
    "Indicadores de falhas em séries diárias de cota/vazão",
    "Vazão sem curva-chave válida",
    
    "Fração de cotas ≤ 0",
    "Fração de cotas ≤ 0",
    "Fração de cotas nulas ou negativas",
    "Fração de vazões ≤ 0",
    "Fração de vazões ≤ 0",
    "Fração de vazões nulas ou negativas",
    "Fração de cotas repetidas com vazão variável",
    "Fração de vazões repetidas com cota variável",
    "Fração pareada com curva-chave",
    "Fração pareada com curva-chave",
    "Mediana do resíduo logarítmico absoluto",
    "Mediana do resíduo logarítmico absoluto",
    "Fração fora do envelope empírico",
    "Evidência de regimes temporais nos resíduos",
    "Número de regimes temporais",
    "Escore de atenção diagnóstica"
  ),
  stringsAsFactors = FALSE
)

index_symbol_map <- data.frame(
  code = c(
    "Fraction of stage <= 0",
    "Fraction of stages <= 0",
    "Fraction of zero or negative stage values",
    "Fraction of discharge <= 0",
    "Fraction of discharges <= 0",
    "Fraction of zero or negative discharge values",
    "Fraction of repeated stage with variable discharge",
    "Fraction of repeated discharge with variable stage",
    "Fraction paired with rating curve",
    "Rating-curve match fraction",
    "Median absolute log residual",
    "Median absolute rating log residual",
    "Fraction outside empirical envelope",
    "Temporal-regime evidence",
    "Number of temporal regimes",
    "Diagnostic attention score",
    "Preliminary information score"
  ),
  symbol = c(
    "FH0",
    "FH0",
    "FH0",
    "FQ0",
    "FQ0",
    "FQ0",
    "FHRQ",
    "FQRH",
    "FPC",
    "FPC",
    "RLA",
    "RLA",
    "FEE",
    "ERT",
    "NRT",
    "EAD",
    "EPI"
  ),
  formula_pt = c(
    "FH0 = n(H ≤ 0) / N",
    "FH0 = n(H ≤ 0) / N",
    "FH0 = n(H ≤ 0) / N",
    "FQ0 = n(Qobs ≤ 0) / N",
    "FQ0 = n(Qobs ≤ 0) / N",
    "FQ0 = n(Qobs ≤ 0) / N",
    "FHRQ = n(medições em grupos de cota repetida com vazão variável) / N",
    "FQRH = n(medições em grupos de vazão repetida com cota variável) / N",
    "FPC = Np / Nv",
    "FPC = Np / Nv",
    "RLA = mediana(|log(Qobs) − log(Qcc)|)",
    "RLA = mediana(|log(Qobs) − log(Qcc)|)",
    "FEE = n(medições fora do envelope) / n(medições com envelope calculado)",
    "ERT = triagem dos resíduos da curva de referência Q = a(H − h0)^b",
    "NRT = número de regimes temporais indicados pela triagem",
    "EAD = soma de sinais diagnósticos de atenção",
    "EPI = escore de completude dos produtos locais disponíveis"
  ),
  interpretation_pt = c(
    "Valores maiores indicam maior frequência de cotas nulas ou negativas nas medições.",
    "Valores maiores indicam maior frequência de cotas nulas ou negativas nas medições.",
    "Valores maiores indicam maior frequência de cotas nulas ou negativas nas medições.",
    "Valores maiores indicam maior frequência de vazões nulas ou negativas nas medições.",
    "Valores maiores indicam maior frequência de vazões nulas ou negativas nas medições.",
    "Valores maiores indicam maior frequência de vazões nulas ou negativas nas medições.",
    "Valores maiores indicam mais pontos em grupos de mesma cota com variação de vazão.",
    "Valores maiores indicam mais pontos em grupos de mesma vazão com variação de cota.",
    "Valores maiores indicam maior fração de medições pareadas com curva-chave válida.",
    "Valores maiores indicam maior fração de medições pareadas com curva-chave válida.",
    "Valores maiores indicam maior diferença multiplicativa típica entre vazão observada e vazão estimada pela curva-chave.",
    "Valores maiores indicam maior diferença multiplicativa típica entre vazão observada e vazão estimada pela curva-chave.",
    "Valores maiores indicam maior fração de pontos fora do envelope empírico de resíduos.",
    "Indica se a triagem encontrou sinais de mudança temporal nos resíduos da relação cota-vazão.",
    "Indica a quantidade de regimes temporais sugeridos pela triagem.",
    "Valores maiores indicam maior atenção diagnóstica para revisão visual.",
    "Valores maiores indicam maior disponibilidade de informações no banco local do aplicativo."
  ),
  stringsAsFactors = FALSE
)

class_map <- data.frame(
  code = c(
    "not_available",
    "none",
    "none_detected",
    "available",
    "missing",
    "not_classified",
    "very_low",
    "low",
    "moderate",
    "high",
    "very_high",
    "low_coverage",
    "moderate_coverage",
    "high_coverage",
    "no_evidence",
    "weak_evidence",
    "moderate_evidence",
    "strong_evidence",
    "low_attention",
    "moderate_attention",
    "high_attention",
    "not_calculable_current_export",
    "calculation_failed",
    "very_limited",
    "limited",
    "substantial",
    "very_short",
    "short",
    "long",
    "single_curve",
    "few_curves",
    "multiple_curves",
    "many_curves",
    "single_segment",
    "few_segments",
    "multiple_segments",
    "many_segments",
    "single_profile",
    "few_profiles",
    "multiple_profiles",
    "many_profiles",
    "single_date_or_unknown_span",
    "incomplete_geometry",
    "invalid_or_flat_geometry",
    "geometry_available",
    "high_information",
    "moderate_information",
    "limited_information",
    "basic_registration_or_sparse_products",
    "registration_only_or_incomplete",
    "measurements_and_rating_curves",
    "measurements_only",
    "rating_curves_only",
    "registration_only",
    "missing_coordinates",
    "light_station_summary",
    "detailed_station_level",
    "narrow",
    "wide"
  ),
  label_pt = c(
    "não disponível",
    "nenhum",
    "nenhum detectado",
    "disponível",
    "ausente",
    "não classificado",
    "muito baixo",
    "baixo",
    "moderado",
    "alto",
    "muito alto",
    "baixa cobertura",
    "cobertura moderada",
    "alta cobertura",
    "sem evidência",
    "evidência fraca",
    "evidência moderada",
    "evidência forte",
    "atenção baixa",
    "atenção moderada",
    "atenção alta",
    "não calculável no export atual",
    "falha no cálculo",
    "muito limitado",
    "limitado",
    "substancial",
    "muito curto",
    "curto",
    "longo",
    "curva única",
    "poucas curvas",
    "múltiplas curvas",
    "muitas curvas",
    "segmento único",
    "poucos segmentos",
    "múltiplos segmentos",
    "muitos segmentos",
    "perfil único",
    "poucos perfis",
    "múltiplos perfis",
    "muitos perfis",
    "data única ou intervalo desconhecido",
    "geometria incompleta",
    "geometria inválida ou plana",
    "geometria disponível",
    "alta disponibilidade de informação",
    "disponibilidade moderada de informação",
    "disponibilidade limitada de informação",
    "cadastro básico ou produtos esparsos",
    "somente cadastro ou informação incompleta",
    "medições de descarga e curvas-chave",
    "somente medições de descarga",
    "somente curvas-chave",
    "somente cadastro da estação",
    "coordenadas ausentes",
    "resumo leve da estação",
    "diagnóstico detalhado da estação",
    "estreita",
    "ampla"
  ),
  description_pt = c(
    "Informação não disponível para este item.",
    "Nenhum registro ou sinal foi encontrado.",
    "Nenhum sinal foi detectado no critério avaliado.",
    "Informação disponível no banco local.",
    "Informação ausente no banco local.",
    "Indicador sem classe categórica específica.",
    "Classe muito baixa.",
    "Classe baixa.",
    "Classe moderada.",
    "Classe alta.",
    "Classe muito alta.",
    "Baixa cobertura dos dados necessários.",
    "Cobertura moderada dos dados necessários.",
    "Alta cobertura dos dados necessários.",
    "Sem evidência diagnóstica detectada.",
    "Evidência diagnóstica fraca.",
    "Evidência diagnóstica moderada.",
    "Evidência diagnóstica forte.",
    "Baixa necessidade de atenção diagnóstica.",
    "Necessidade moderada de atenção diagnóstica.",
    "Alta necessidade de atenção diagnóstica.",
    "Indicador planejado, mas não calculável com o export atual.",
    "O cálculo do indicador falhou.",
    "Poucos registros disponíveis.",
    "Número limitado de registros disponíveis.",
    "Quantidade substancial de registros disponíveis.",
    "Período temporal muito curto.",
    "Período temporal curto.",
    "Período temporal longo.",
    "Existe uma única curva-chave.",
    "Existem poucas curvas-chave.",
    "Existem múltiplas curvas-chave.",
    "Existem muitas curvas-chave.",
    "Existe um único segmento de curva-chave.",
    "Existem poucos segmentos de curva-chave.",
    "Existem múltiplos segmentos de curva-chave.",
    "Existem muitos segmentos de curva-chave.",
    "Existe um único perfil de seção transversal.",
    "Existem poucos perfis de seção transversal.",
    "Existem múltiplos perfis de seção transversal.",
    "Existem muitos perfis de seção transversal.",
    "Há uma única data ou não há intervalo temporal definido.",
    "A geometria da seção transversal está incompleta.",
    "A geometria disponível parece inválida ou sem variação.",
    "A geometria básica da seção transversal está disponível.",
    "A estação possui alta disponibilidade de informações locais.",
    "A estação possui disponibilidade moderada de informações locais.",
    "A estação possui disponibilidade limitada de informações locais.",
    "A estação possui cadastro básico ou produtos locais esparsos.",
    "A estação possui somente cadastro ou informação incompleta.",
    "A estação possui medições de descarga e curvas-chave no banco local.",
    "A estação possui medições de descarga, mas não possui curvas-chave no banco local.",
    "A estação possui curvas-chave, mas não possui medições de descarga no banco local.",
    "A estação possui somente cadastro no banco local do aplicativo.",
    "A estação não possui coordenadas válidas para mapeamento.",
    "Resumo leve usado para filtros, mapas e visão geral.",
    "Diagnóstico detalhado calculado para a estação selecionada.",
    "Amplitude pequena para o critério avaliado.",
    "Amplitude grande para o critério avaliado."
  ),
  stringsAsFactors = FALSE
)

unit_map <- data.frame(
  code = c(
    "records",
    "years",
    "%",
    "curves",
    "segments",
    "profiles",
    "vertices",
    "pairs",
    "m",
    "cm",
    "m3/s",
    "m³/s",
    "0-100",
    NA_character_
  ),
  label_pt = c(
    "registros",
    "anos",
    "%",
    "curvas",
    "segmentos",
    "perfis",
    "vértices",
    "pares",
    "m",
    "cm",
    "m³/s",
    "m³/s",
    "0–100",
    "sem unidade"
  ),
  stringsAsFactors = FALSE
)

group_type_map <- data.frame(
  code = c(
    "same_stage_variable_discharge",
    "same_discharge_variable_stage"
  ),
  label_pt = c(
    "mesma cota com vazão variável",
    "mesma vazão com cota variável"
  ),
  stringsAsFactors = FALSE
)

map_status_map <- data.frame(
  code = c(
    "measurements_and_rating_curves",
    "measurements_only",
    "rating_curves_only",
    "registration_only",
    "missing_coordinates"
  ),
  label_pt = c(
    "Medições de descarga e curvas-chave",
    "Somente medições de descarga",
    "Somente curvas-chave",
    "Somente cadastro da estação",
    "Coordenadas ausentes"
  ),
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------
# Function to enrich index tables
# ------------------------------------------------------------

enrich_index_table <- function(df) {
  if (!"index_group" %in% names(df)) df$index_group <- NA_character_
  if (!"index_name" %in% names(df)) df$index_name <- NA_character_
  if (!"index_unit" %in% names(df)) df$index_unit <- NA_character_
  if (!"index_class" %in% names(df)) df$index_class <- NA_character_
  if (!"index_description" %in% names(df)) df$index_description <- NA_character_
  
  df <- df %>%
    mutate(
      index_group_label_pt = recode_with_fallback(
        index_group,
        index_group_map,
        missing_label = "grupo não classificado"
      ),
      index_name_label_pt = recode_with_fallback(
        index_name,
        index_name_map,
        missing_label = "índice não classificado"
      ),
      index_unit_label_pt = recode_with_fallback(
        index_unit,
        unit_map,
        missing_label = "sem unidade"
      ),
      index_class_label_pt = recode_with_fallback(
        index_class,
        class_map,
        missing_label = "não classificado"
      ),
      index_class_description_pt = recode_with_fallback(
        index_class,
        class_map,
        label_col = "description_pt",
        missing_label = "Indicador sem classe categórica específica."
      ),
      index_description_pt = case_when(
        index_name %in% index_name_map$code ~ paste0(
          index_name_label_pt,
          ". Indicador preparado para exibição no aplicativo."
        ),
        !is.na(index_description) & index_description != "" ~ index_description,
        TRUE ~ "Indicador calculado a partir dos produtos locais disponíveis no banco do aplicativo."
      ),
      index_symbol = index_symbol_map$symbol[match(index_name, index_symbol_map$code)],
      index_formula_pt = index_symbol_map$formula_pt[match(index_name, index_symbol_map$code)],
      index_interpretation_pt = index_symbol_map$interpretation_pt[match(index_name, index_symbol_map$code)]
    ) %>%
    mutate(
      index_symbol = ifelse(is.na(index_symbol), NA_character_, index_symbol),
      index_formula_pt = ifelse(
        is.na(index_formula_pt),
        "Fórmula não especificada para este indicador.",
        index_formula_pt
      ),
      index_interpretation_pt = ifelse(
        is.na(index_interpretation_pt),
        "Indicador usado como apoio à triagem e à interpretação visual.",
        index_interpretation_pt
      )
    )
  
  df
}

# ------------------------------------------------------------
# Enrich station_quality_indices
# ------------------------------------------------------------

station_quality_indices <- DBI::dbReadTable(con, "station_quality_indices")
station_quality_indices <- enrich_index_table(station_quality_indices)

DBI::dbWriteTable(
  con,
  "station_quality_indices",
  station_quality_indices,
  overwrite = TRUE
)

# ------------------------------------------------------------
# Enrich station_diagnostic_indices, if available
# ------------------------------------------------------------

if (has_table(con, "station_diagnostic_indices")) {
  station_diagnostic_indices <- DBI::dbReadTable(con, "station_diagnostic_indices")
  station_diagnostic_indices <- enrich_index_table(station_diagnostic_indices)
  
  DBI::dbWriteTable(
    con,
    "station_diagnostic_indices",
    station_diagnostic_indices,
    overwrite = TRUE
  )
}

# ------------------------------------------------------------
# Enrich station_diagnostic_summary, if available
# ------------------------------------------------------------

if (has_table(con, "station_diagnostic_summary")) {
  station_diagnostic_summary <- DBI::dbReadTable(con, "station_diagnostic_summary")
  
  if ("diagnostic_attention_class" %in% names(station_diagnostic_summary)) {
    station_diagnostic_summary$diagnostic_attention_class_label_pt <-
      recode_with_fallback(
        station_diagnostic_summary$diagnostic_attention_class,
        class_map,
        missing_label = "não disponível"
      )
  }
  
  if ("temporal_regime_evidence_class" %in% names(station_diagnostic_summary)) {
    station_diagnostic_summary$temporal_regime_evidence_class_label_pt <-
      recode_with_fallback(
        station_diagnostic_summary$temporal_regime_evidence_class,
        class_map,
        missing_label = "não disponível"
      )
  }
  
  if ("diagnostic_detail_level" %in% names(station_diagnostic_summary)) {
    station_diagnostic_summary$diagnostic_detail_level_label_pt <-
      recode_with_fallback(
        station_diagnostic_summary$diagnostic_detail_level,
        class_map,
        missing_label = "não disponível"
      )
  }
  
  if ("cross_section_record_class" %in% names(station_diagnostic_summary)) {
    station_diagnostic_summary$cross_section_record_class_label_pt <-
      recode_with_fallback(
        station_diagnostic_summary$cross_section_record_class,
        class_map,
        missing_label = "não disponível"
      )
  }
  
  if ("cross_section_vertex_class" %in% names(station_diagnostic_summary)) {
    station_diagnostic_summary$cross_section_vertex_class_label_pt <-
      recode_with_fallback(
        station_diagnostic_summary$cross_section_vertex_class,
        class_map,
        missing_label = "não disponível"
      )
  }
  
  if ("cross_section_geometry_class" %in% names(station_diagnostic_summary)) {
    station_diagnostic_summary$cross_section_geometry_class_label_pt <-
      recode_with_fallback(
        station_diagnostic_summary$cross_section_geometry_class,
        class_map,
        missing_label = "não disponível"
      )
  }
  
  DBI::dbWriteTable(
    con,
    "station_diagnostic_summary",
    station_diagnostic_summary,
    overwrite = TRUE
  )
}

# ------------------------------------------------------------
# Enrich station_assessment_summary
# ------------------------------------------------------------

station_assessment_summary <- DBI::dbReadTable(con, "station_assessment_summary")

if ("preliminary_information_class" %in% names(station_assessment_summary)) {
  station_assessment_summary$preliminary_information_class_label_pt <-
    recode_with_fallback(
      station_assessment_summary$preliminary_information_class,
      class_map,
      missing_label = "não disponível"
    )
}

if ("station_assessment_status" %in% names(station_assessment_summary)) {
  station_assessment_summary$station_assessment_status_label_pt <-
    recode_with_fallback(
      station_assessment_summary$station_assessment_status,
      class_map,
      missing_label = "não disponível"
    )
}

if ("cross_section_record_class" %in% names(station_assessment_summary)) {
  station_assessment_summary$cross_section_record_class_label_pt <-
    recode_with_fallback(
      station_assessment_summary$cross_section_record_class,
      class_map,
      missing_label = "não disponível"
    )
}

if ("cross_section_vertex_class" %in% names(station_assessment_summary)) {
  station_assessment_summary$cross_section_vertex_class_label_pt <-
    recode_with_fallback(
      station_assessment_summary$cross_section_vertex_class,
      class_map,
      missing_label = "não disponível"
    )
}

if ("cross_section_temporal_class" %in% names(station_assessment_summary)) {
  station_assessment_summary$cross_section_temporal_class_label_pt <-
    recode_with_fallback(
      station_assessment_summary$cross_section_temporal_class,
      class_map,
      missing_label = "não disponível"
    )
}

if ("cross_section_geometry_class" %in% names(station_assessment_summary)) {
  station_assessment_summary$cross_section_geometry_class_label_pt <-
    recode_with_fallback(
      station_assessment_summary$cross_section_geometry_class,
      class_map,
      missing_label = "não disponível"
    )
}

DBI::dbWriteTable(
  con,
  "station_assessment_summary",
  station_assessment_summary,
  overwrite = TRUE
)

# ------------------------------------------------------------
# Enrich station_data_availability
# ------------------------------------------------------------

station_data_availability <- DBI::dbReadTable(con, "station_data_availability")

boolean_label_columns <- names(station_data_availability)[
  grepl("^has_", names(station_data_availability))
]

for (col in boolean_label_columns) {
  label_col <- paste0(col, "_label_pt")
  station_data_availability[[label_col]] <- yes_no_label(
    as.logical(station_data_availability[[col]])
  )
}

class_columns <- names(station_data_availability)[
  grepl("_class$", names(station_data_availability))
]

for (col in class_columns) {
  label_col <- paste0(col, "_label_pt")
  station_data_availability[[label_col]] <- recode_with_fallback(
    station_data_availability[[col]],
    class_map,
    missing_label = "não disponível"
  )
}

DBI::dbWriteTable(
  con,
  "station_data_availability",
  station_data_availability,
  overwrite = TRUE
)

# ------------------------------------------------------------
# Enrich station_map_status
# ------------------------------------------------------------

station_map_status <- DBI::dbReadTable(con, "station_map_status")

if ("map_status" %in% names(station_map_status)) {
  station_map_status$map_status_label_pt <- recode_with_fallback(
    station_map_status$map_status,
    map_status_map,
    missing_label = "não disponível"
  )
}

station_map_status$product_summary_label_pt <- case_when(
  get_col(station_map_status, "has_discharge_measurements_processed", FALSE) &
    get_col(station_map_status, "has_rating_curves_processed", FALSE) &
    get_col(station_map_status, "has_cross_sections_processed", FALSE) ~
    "Medições de descarga, curvas-chave e seções transversais disponíveis",
  
  get_col(station_map_status, "has_discharge_measurements_processed", FALSE) &
    get_col(station_map_status, "has_rating_curves_processed", FALSE) ~
    "Medições de descarga e curvas-chave disponíveis",
  
  get_col(station_map_status, "has_discharge_measurements_processed", FALSE) &
    get_col(station_map_status, "has_cross_sections_processed", FALSE) ~
    "Medições de descarga e seções transversais disponíveis",
  
  get_col(station_map_status, "has_rating_curves_processed", FALSE) &
    get_col(station_map_status, "has_cross_sections_processed", FALSE) ~
    "Curvas-chave e seções transversais disponíveis",
  
  get_col(station_map_status, "has_discharge_measurements_processed", FALSE) ~
    "Medições de descarga disponíveis",
  
  get_col(station_map_status, "has_rating_curves_processed", FALSE) ~
    "Curvas-chave disponíveis",
  
  get_col(station_map_status, "has_cross_sections_processed", FALSE) ~
    "Seções transversais disponíveis",
  
  TRUE ~ "Sem produtos hidrológicos processados no banco local"
)

DBI::dbWriteTable(
  con,
  "station_map_status",
  station_map_status,
  overwrite = TRUE
)

# ------------------------------------------------------------
# Create station_product_availability
# ------------------------------------------------------------

stations_minimal <- DBI::dbReadTable(con, "stations_minimal") %>%
  mutate(station_code = as.character(station_code))

station_product_availability <- station_data_availability %>%
  mutate(station_code = as.character(station_code)) %>%
  select(
    station_code,
    any_of(c(
      "has_discharge_measurements_processed",
      "has_rating_curves_processed",
      "has_cross_sections_processed",
      "has_cross_section_vertices_processed",
      "n_measurements",
      "n_rating_curves",
      "n_rating_curve_segments",
      "n_cross_sections",
      "n_cross_section_vertices"
    ))
  ) %>%
  right_join(
    stations_minimal %>%
      transmute(
        station_code = as.character(station_code),
        has_station_registration = TRUE,
        has_inventory_flu_data = as.logical(get_col(stations_minimal, "has_discharge_measurements", FALSE)),
        has_inventory_rainfall_data = as.logical(get_col(stations_minimal, "has_rainfall_data", FALSE)),
        has_inventory_stage_data = as.logical(get_col(stations_minimal, "has_stage_data", FALSE)),
        has_inventory_telemetry = as.logical(get_col(stations_minimal, "has_telemetry", FALSE))
      ),
    by = "station_code"
  ) %>%
  mutate(
    n_discharge_measurements = coalesce(as.numeric(get_col(., "n_measurements", 0)), 0),
    n_rating_curves = coalesce(as.numeric(get_col(., "n_rating_curves", 0)), 0),
    n_rating_curve_segments = coalesce(as.numeric(get_col(., "n_rating_curve_segments", 0)), 0),
    n_cross_sections = coalesce(as.numeric(get_col(., "n_cross_sections", 0)), 0),
    n_cross_section_profiles = n_cross_sections,
    n_cross_section_vertices = coalesce(as.numeric(get_col(., "n_cross_section_vertices", 0)), 0),
    
    has_product_discharge_summary = n_discharge_measurements > 0,
    has_product_rating_curves = n_rating_curves > 0 | n_rating_curve_segments > 0,
    has_product_cross_sections = n_cross_sections > 0 | n_cross_section_vertices > 0,
    
    has_product_flu_data = FALSE,
    has_product_rainfall_data = FALSE,
    has_product_stage_data = FALSE,
    
    has_station_registration_label_pt = yes_no_label(has_station_registration),
    has_product_discharge_summary_label_pt = yes_no_label(has_product_discharge_summary),
    has_product_rating_curves_label_pt = yes_no_label(has_product_rating_curves),
    has_product_cross_sections_label_pt = yes_no_label(has_product_cross_sections),
    has_inventory_flu_data_label_pt = yes_no_label(has_inventory_flu_data),
    has_inventory_rainfall_data_label_pt = yes_no_label(has_inventory_rainfall_data),
    has_inventory_stage_data_label_pt = yes_no_label(has_inventory_stage_data),
    has_inventory_telemetry_label_pt = yes_no_label(has_inventory_telemetry),
    has_product_flu_data_label_pt = yes_no_label(has_product_flu_data),
    has_product_rainfall_data_label_pt = yes_no_label(has_product_rainfall_data),
    has_product_stage_data_label_pt = yes_no_label(has_product_stage_data),
    
    map_status_code = case_when(
      has_product_discharge_summary & has_product_rating_curves ~ "measurements_and_rating_curves",
      has_product_discharge_summary & !has_product_rating_curves ~ "measurements_only",
      !has_product_discharge_summary & has_product_rating_curves ~ "rating_curves_only",
      TRUE ~ "registration_only"
    ),
    map_status_label_pt = recode_with_fallback(
      map_status_code,
      map_status_map,
      missing_label = "não disponível"
    ),
    
    product_summary_label_pt = case_when(
      has_product_discharge_summary & has_product_rating_curves & has_product_cross_sections ~
        "Medições de descarga, curvas-chave e seções transversais disponíveis",
      has_product_discharge_summary & has_product_rating_curves ~
        "Medições de descarga e curvas-chave disponíveis",
      has_product_discharge_summary & has_product_cross_sections ~
        "Medições de descarga e seções transversais disponíveis",
      has_product_rating_curves & has_product_cross_sections ~
        "Curvas-chave e seções transversais disponíveis",
      has_product_discharge_summary ~
        "Medições de descarga disponíveis",
      has_product_rating_curves ~
        "Curvas-chave disponíveis",
      has_product_cross_sections ~
        "Seções transversais disponíveis",
      TRUE ~
        "Sem produtos hidrológicos processados no banco local"
    )
  ) %>%
  select(
    station_code,
    has_station_registration,
    has_station_registration_label_pt,
    n_discharge_measurements,
    has_product_discharge_summary,
    has_product_discharge_summary_label_pt,
    n_rating_curves,
    n_rating_curve_segments,
    has_product_rating_curves,
    has_product_rating_curves_label_pt,
    n_cross_sections,
    n_cross_section_profiles,
    n_cross_section_vertices,
    has_product_cross_sections,
    has_product_cross_sections_label_pt,
    has_inventory_flu_data,
    has_inventory_flu_data_label_pt,
    has_inventory_rainfall_data,
    has_inventory_rainfall_data_label_pt,
    has_inventory_stage_data,
    has_inventory_stage_data_label_pt,
    has_inventory_telemetry,
    has_inventory_telemetry_label_pt,
    has_product_flu_data,
    has_product_flu_data_label_pt,
    has_product_rainfall_data,
    has_product_rainfall_data_label_pt,
    has_product_stage_data,
    has_product_stage_data_label_pt,
    map_status_code,
    map_status_label_pt,
    product_summary_label_pt
  )

DBI::dbWriteTable(
  con,
  "station_product_availability",
  station_product_availability,
  overwrite = TRUE
)

# ------------------------------------------------------------
# Create data_dictionary
# ------------------------------------------------------------

table_labels <- data.frame(
  table_name = c(
    "stations_minimal",
    "station_product_availability",
    "station_data_availability",
    "station_assessment_summary",
    "station_map_status",
    "station_quality_indices",
    "station_diagnostic_summary",
    "station_diagnostic_indices",
    "station_cross_section_indices",
    "discharge_measurements",
    "rating_curve_summary",
    "rating_curves",
    "cross_sections",
    "cross_section_vertices",
    "cross_section_summary",
    "metadata",
    "data_dictionary",
    "data_dictionary_values",
    "export_row_counts"
  ),
  table_label_pt = c(
    "Estações",
    "Disponibilidade de produtos por estação",
    "Disponibilidade de dados por estação",
    "Resumo de avaliação da estação",
    "Estado da estação no mapa",
    "Indicadores de qualidade e disponibilidade",
    "Resumo diagnóstico da estação",
    "Indicadores diagnósticos",
    "Indicadores de seções transversais",
    "Medições de descarga",
    "Resumo de curvas-chave",
    "Segmentos de curvas-chave",
    "Seções transversais",
    "Vértices das seções transversais",
    "Resumo de seções transversais",
    "Metadados",
    "Dicionário de dados",
    "Dicionário de valores categóricos",
    "Contagem de linhas do export"
  ),
  stringsAsFactors = FALSE
)

column_label_map <- data.frame(
  column_name = c(
    "station_code",
    "station_name",
    "station_type",
    "uf",
    "municipality",
    "basin_code",
    "basin_name",
    "river_name",
    "latitude",
    "longitude",
    "drainage_area",
    "measurement_datetime",
    "valid_from",
    "valid_to",
    "stage_cm",
    "discharge_m3s",
    "rating_curve_id",
    "rating_curve_segment_id",
    "cross_section_id",
    "cross_section_vertex_id",
    "vertex_order",
    "vertex_distance_m",
    "vertex_stage_cm",
    "n_measurements",
    "n_rating_curves",
    "n_rating_curve_segments",
    "n_cross_sections",
    "n_cross_section_profiles",
    "n_cross_section_vertices",
    "has_product_discharge_summary",
    "has_product_rating_curves",
    "has_product_cross_sections",
    "index_group",
    "index_group_label_pt",
    "index_name",
    "index_name_label_pt",
    "index_unit",
    "index_unit_label_pt",
    "index_class",
    "index_class_label_pt",
    "index_description_pt",
    "index_symbol",
    "index_formula_pt",
    "index_interpretation_pt",
    "diagnostic_attention_class",
    "diagnostic_attention_class_label_pt",
    "temporal_regime_evidence_class",
    "temporal_regime_evidence_class_label_pt",
    "map_status_code",
    "map_status_label_pt",
    "product_summary_label_pt"
  ),
  label_pt = c(
    "Código da estação",
    "Nome da estação",
    "Tipo da estação",
    "UF",
    "Município",
    "Código da bacia",
    "Nome da bacia",
    "Nome do rio",
    "Latitude",
    "Longitude",
    "Área de drenagem",
    "Data/hora da medição",
    "Início da vigência",
    "Fim da vigência",
    "Cota",
    "Vazão",
    "Identificador da curva-chave",
    "Identificador do segmento da curva-chave",
    "Identificador da seção transversal",
    "Identificador do vértice da seção transversal",
    "Ordem do vértice",
    "Distância horizontal do vértice",
    "Cota do vértice",
    "Número de medições",
    "Número de curvas-chave",
    "Número de segmentos de curva-chave",
    "Número de seções transversais",
    "Número de perfis de seção transversal",
    "Número de vértices de seções transversais",
    "Resumo de descarga disponível",
    "Curvas-chave disponíveis",
    "Seções transversais disponíveis",
    "Grupo do índice",
    "Grupo do índice",
    "Nome do índice",
    "Nome do índice",
    "Unidade do índice",
    "Unidade do índice",
    "Classe do índice",
    "Classe do índice",
    "Descrição do índice",
    "Símbolo do índice",
    "Fórmula do índice",
    "Interpretação do índice",
    "Classe de atenção diagnóstica",
    "Classe de atenção diagnóstica",
    "Evidência de regimes temporais",
    "Evidência de regimes temporais",
    "Código do estado no mapa",
    "Estado no mapa",
    "Resumo dos produtos disponíveis"
  ),
  stringsAsFactors = FALSE
)

table_columns <- DBI::dbGetQuery(
  con,
  "SELECT table_name, column_name, data_type, ordinal_position
   FROM information_schema.columns
   WHERE table_schema = 'main'
   ORDER BY table_name, ordinal_position"
)

data_dictionary <- table_columns %>%
  left_join(table_labels, by = "table_name") %>%
  left_join(column_label_map, by = "column_name") %>%
  mutate(
    label_pt = ifelse(is.na(label_pt) | label_pt == "", column_name, label_pt),
    description_pt = paste0(
      "Campo `", column_name, "` da tabela `", table_name,
      "`, preparado para uso local no aplicativo Shiny."
    ),
    unit = case_when(
      str_detect(column_name, "_cm$|stage_cm|cota") ~ "cm",
      str_detect(column_name, "_m$|distance|distancia") ~ "m",
      str_detect(column_name, "_m3s$|discharge") ~ "m³/s",
      str_detect(column_name, "latitude|longitude") ~ "graus decimais",
      str_detect(column_name, "pct_|fraction") ~ "%",
      TRUE ~ NA_character_
    ),
    value_type = data_type,
    display_group = ifelse(
      is.na(table_label_pt),
      table_name,
      table_label_pt
    ),
    display_order = ordinal_position,
    show_in_shiny = !str_detect(
      tolower(column_name),
      "raw|token|senha|password|cpf|cnpj|credential|secret"
    )
  ) %>%
  select(
    table_name,
    column_name,
    label_pt,
    description_pt,
    unit,
    value_type,
    display_group,
    display_order,
    show_in_shiny
  )

DBI::dbWriteTable(
  con,
  "data_dictionary",
  data_dictionary,
  overwrite = TRUE
)

# ------------------------------------------------------------
# Create data_dictionary_values
# ------------------------------------------------------------

data_dictionary_values <- bind_rows(
  class_map %>%
    transmute(
      table_name = "(multiple)",
      column_name = "index_class / diagnostic_attention_class / status_class",
      value_code = code,
      value_label_pt = label_pt,
      value_description_pt = description_pt,
      display_order = row_number()
    ),
  
  group_type_map %>%
    transmute(
      table_name = "(diagnostic detail tables)",
      column_name = "group_type",
      value_code = code,
      value_label_pt = label_pt,
      value_description_pt = paste0("Tipo de grupo repetido: ", label_pt, "."),
      display_order = row_number()
    ),
  
  index_group_map %>%
    transmute(
      table_name = "station_quality_indices / station_diagnostic_indices",
      column_name = "index_group",
      value_code = code,
      value_label_pt = label_pt,
      value_description_pt = paste0("Grupo de indicadores: ", label_pt, "."),
      display_order = row_number()
    ),
  
  map_status_map %>%
    transmute(
      table_name = "station_map_status / station_product_availability",
      column_name = "map_status_code",
      value_code = code,
      value_label_pt = label_pt,
      value_description_pt = paste0("Categoria usada no mapa: ", label_pt, "."),
      display_order = row_number()
    )
)

DBI::dbWriteTable(
  con,
  "data_dictionary_values",
  data_dictionary_values,
  overwrite = TRUE
)

# ------------------------------------------------------------
# Update metadata
# ------------------------------------------------------------

metadata_update <- data.frame(
  key = c(
    "stage_09e_shiny_display_layer_processed_at",
    "stage_09e_shiny_display_layer_script",
    "stage_09e_shiny_display_layer_note",
    "stage_09e_shiny_display_layer_language"
  ),
  value = c(
    as.character(Sys.time()),
    "pipeline/R/068_prepare_shiny_display_layer.R",
    "Portuguese display labels, descriptions, value dictionaries, and product availability fields were added to the Shiny export database.",
    "Technical codes were preserved; Portuguese display fields were added in parallel."
  ),
  stringsAsFactors = FALSE
)

if (has_table(con, "metadata")) {
  metadata_existing <- DBI::dbReadTable(con, "metadata") %>%
    filter(!key %in% metadata_update$key)
  
  DBI::dbWriteTable(
    con,
    "metadata",
    bind_rows(metadata_existing, metadata_update),
    overwrite = TRUE
  )
}

# ------------------------------------------------------------
# Critical checks
# ------------------------------------------------------------

label_check_tables <- c(
  "station_quality_indices",
  "station_diagnostic_indices"
)

label_checks <- data.frame(
  table_name = character(),
  field_name = character(),
  n_missing = integer(),
  stringsAsFactors = FALSE
)

for (tbl in label_check_tables) {
  if (has_table(con, tbl)) {
    df <- DBI::dbReadTable(con, tbl)
    
    fields <- intersect(
      c(
        "index_group_label_pt",
        "index_name_label_pt",
        "index_class_label_pt",
        "index_description_pt"
      ),
      names(df)
    )
    
    for (field in fields) {
      label_checks <- rbind(
        label_checks,
        data.frame(
          table_name = tbl,
          field_name = field,
          n_missing = sum(is.na(df[[field]]) | df[[field]] == ""),
          stringsAsFactors = FALSE
        )
      )
    }
  }
}

failed_label_checks <- label_checks %>%
  filter(n_missing > 0)

if (nrow(failed_label_checks) > 0) {
  print(failed_label_checks)
  stop("Some required display-label fields have missing values.")
}

row_counts <- data.frame(
  table_name = c(
    "station_product_availability",
    "data_dictionary",
    "data_dictionary_values",
    "station_quality_indices",
    if (has_table(con, "station_diagnostic_indices")) "station_diagnostic_indices" else NA_character_
  ),
  n_rows = NA_real_,
  stringsAsFactors = FALSE
) %>%
  filter(!is.na(table_name))

row_counts$n_rows <- as.numeric(vapply(row_counts$table_name, safe_count, numeric(1), con = con))

write.csv(
  row_counts,
  file.path(output_dir, "068_shiny_display_layer_row_counts.csv"),
  row.names = FALSE
)

write.csv(
  label_checks,
  file.path(output_dir, "068_shiny_display_label_checks.csv"),
  row.names = FALSE
)

DBI::dbExecute(con, "CHECKPOINT")

# ------------------------------------------------------------
# Console checks requested for this stage
# ------------------------------------------------------------

message("Finished preparing Shiny display layer.")
message("Output database: ", shiny_db)

message("Row counts:")
print(row_counts)

message("Label checks:")
print(label_checks)

message("station_product_availability count:")
print(DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM station_product_availability"))

message("Diagnostic index names:")
if (has_table(con, "station_diagnostic_indices")) {
  print(DBI::dbGetQuery(
    con,
    "SELECT DISTINCT index_name, index_name_label_pt
     FROM station_diagnostic_indices
     ORDER BY index_name"
  ))
}

message("Diagnostic index classes:")
if (has_table(con, "station_diagnostic_indices")) {
  print(DBI::dbGetQuery(
    con,
    "SELECT DISTINCT index_class, index_class_label_pt
     FROM station_diagnostic_indices
     ORDER BY index_class"
  ))
}

message("Data dictionary count:")
print(DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM data_dictionary"))

message("Data dictionary values count:")
print(DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM data_dictionary_values"))

message("Important: R/app_data.R can now read *_label_pt, *_description_pt, index_symbol, index_formula_pt, and station_product_availability directly from the database.")
