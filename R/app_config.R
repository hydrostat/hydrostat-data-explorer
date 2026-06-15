# ============================================================
# app_config.R
# Purpose: Load packages and define app-wide settings
# ============================================================

library(shiny)
library(DBI)
library(duckdb)
library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(stringr)
library(ggplot2)
library(leaflet)
library(sf)
library(DT)
library(htmltools)
library(scales)
library(ragg)

options(shiny.useragg = TRUE)
app_diagnostic_env <- new.env(parent = globalenv())

app_config <- list(
  app_name = "HydroStat Data Explorer",
  app_subtitle = "Sistema de análise de dados hidrológicos",
  app_repository = "hydrostat-data-explorer",
  app_repository_url = "https://github.com/hydrostat/hydrostat-data-explorer",
  db_path = file.path("exports", "shiny_minimal.duckdb"),
  diagnostic_functions_path = file.path("R", "station_diagnostic_functions.R"),
  map_default_lng = -53.0,
  map_default_lat = -14.2,
  map_default_zoom = 4,
  selected_station_zoom = 7,
  spatial_layers_path = file.path("exports", "spatial_layers", "shiny_spatial_layers.rds"),
  ana_api_base_url = "https://www.ana.gov.br/hidrowebservice/EstacoesTelemetricas",
  ana_api_auth_path = "/OAUth/v1",
  ana_api_daily_discharge_path = "/HidroSerieVazao/v1",
  ana_api_daily_stage_path = "/HidroSerieCotas/v1",
  ana_api_daily_rainfall_path = "/HidroSerieChuva/v1",
  ana_api_max_attempts_per_task = 3,
  ana_api_request_timeout_seconds = 120
)

station_map_groups <- list(
  flu_with_data = "Postos fluviométricos com dados",
  flu_registration = "Postos fluviométricos — cadastro",
  rainfall_with_data = "Postos pluviométricos com dados",
  rainfall_registration = "Postos pluviométricos — cadastro",
  flu_rainfall_with_data = "Postos fluviométricos/pluviométricos com dados",
  flu_rainfall_registration = "Postos fluviométricos/pluviométricos — cadastro"
)

station_map_colors <- list(
  flu_with_data = "#1F77B4",
  flu_registration = "#9ECAE1",
  rainfall_with_data = "#8FBF88",
  rainfall_registration = "#E5F3DF",
  flu_rainfall_with_data = "#756BB1",
  flu_rainfall_registration = "#CBC9E2",
  selected = "#E85D04"
)

spatial_map_groups <- list(
  brazil_boundary = "Contorno do Brasil",
  states = "Estados",
  basins = "Macrobacias",
  rivers_small = "Rios menores",
  rivers_medium = "Rios médios",
  rivers_large = "Rios principais"
)

spatial_map_layer_order <- c(
  "brazil_boundary",
  "states",
  "basins",
  "rivers_small",
  "rivers_medium",
  "rivers_large"
)

spatial_map_default_layers <- c(
  "brazil_boundary",
  "states"
)

spatial_map_colors <- list(
  brazil_boundary = "#000000",
  states = "#5a5a5a",
  basins = "#737373",
  rivers_large = "#08519c",
  rivers_medium = "#3182bd",
  rivers_small = "#9ecae1"
)

spatial_map_weights <- list(
  brazil_boundary = 1.6,
  states = 0.5,
  basins = 0.4,
  rivers_large = 1.5,
  rivers_medium = 1.0,
  rivers_small = 0.8
)

spatial_map_opacity <- list(
  brazil_boundary = 0.95,
  states = 0.65,
  basins = 0.65,
  rivers_large = 0.85,
  rivers_medium = 0.65,
  rivers_small = 0.45
)

spatial_map_dash_array <- list(
  brazil_boundary = NULL,
  states = NULL,
  basins = NULL,
  rivers_large = NULL,
  rivers_medium = NULL,
  rivers_small = NULL
)

app_field_labels <- c(
  station_code = "Código ANA",
  station_name = "Nome da estação",
  station_type = "Tipo de estação",
  uf = "UF",
  municipality = "Município",
  basin_code = "Código da bacia",
  basin_name = "Bacia hidrográfica",
  river_name = "Rio",
  operator = "Operadora",
  responsible_agency = "Responsável",
  is_operating = "Em operação",
  latitude = "Latitude",
  longitude = "Longitude",
  altitude = "Altitude",
  drainage_area = "Área de drenagem",
  discharge_start_date = "Início das medições",
  discharge_end_date = "Fim das medições",
  stage_start_date = "Início da série de cota",
  stage_end_date = "Fim da série de cota",
  rainfall_start_date = "Início da série de chuva",
  rainfall_end_date = "Fim da série de chuva",
  telemetric_start_date = "Início da telemetria",
  telemetric_end_date = "Fim da telemetria",
  map_group_label = "Grupo no mapa",
  diagnostic_attention_class = "Classe de triagem diagnóstica",
  attention_class = "Classe de triagem diagnóstica",
  diagnostic_attention_score = "Índice de triagem diagnóstica",
  attention_score = "Índice de triagem diagnóstica",
  quality_index = "Índice de triagem",
  n_measurements = "Número de medições hidrométricas",
  n_discharge_measurements = "Número de medições hidrométricas de descarga",
  n_rating_curves = "Número de curvas-chave",
  n_rating_curve_segments = "Número de segmentos de curva-chave",
  frac_stage_le_zero = "Fração com cota ≤ 0",
  frac_discharge_le_zero = "Fração com vazão ≤ 0",
  frac_repeated_stage_attention = "Fração com cota repetida em atenção",
  frac_repeated_discharge_attention = "Fração com vazão repetida em atenção",
  measurement_datetime = "Data da medição",
  measurement_date = "Data da medição",
  consistency_level = "Nível de consistência",
  last_update = "Última atualização",
  stage_cm = "Cota (cm)",
  discharge_m3s = "Vazão (m³/s)",
  wetted_area_m2 = "Área molhada (m²)",
  width_m = "Largura (m)",
  mean_depth_m = "Profundidade média (m)",
  mean_velocity_ms = "Velocidade média (m/s)",
  curve_id = "Curva-chave",
  rating_curve_id = "Curva-chave",
  segment_id = "Segmento",
  valid_from = "Válida desde",
  valid_to = "Válida até",
  stage_min_cm = "Cota mínima (cm)",
  stage_max_cm = "Cota máxima (cm)",
  discharge_min_m3s = "Vazão mínima (m³/s)",
  discharge_max_m3s = "Vazão máxima (m³/s)",
  coefficient_a = "Coeficiente a",
  coefficient_h0 = "Coeficiente h0",
  coefficient_n = "Coeficiente b",
  coefficient_b = "Coeficiente b",
  equation_display = "Equação",
  segment_equation = "Equação",
  key = "Campo",
  value = "Valor",
  table_or_view = "Tabela ou visão",
  n_valid_measurements = "Medições válidas",
  n_stage_zero_or_negative = "Cotas ≤ 0",
  pct_stage_zero_or_negative = "Fração de cotas ≤ 0",
  n_discharge_zero_or_negative = "Vazões ≤ 0",
  pct_discharge_zero_or_negative = "Fração de vazões ≤ 0",
  n_repeated_stage_variable_discharge_points = "Pontos com cota repetida e vazão variável",
  pct_repeated_stage_variable_discharge_points = "Fração com cota repetida e vazão variável",
  n_repeated_discharge_variable_stage_points = "Pontos com vazão repetida e cota variável",
  pct_repeated_discharge_variable_stage_points = "Fração com vazão repetida e cota variável",
  rating_match_fraction = "Fração pareada com curva-chave",
  median_abs_rating_log_residual = "Mediana do resíduo log absoluto",
  outside_residual_envelope_fraction = "Fração fora do envelope residual",
  n_temporal_regimes = "Regimes temporais",
  temporal_regime_evidence_class = "Evidência de regime temporal",
  baseline_power_equation = "Equação de base",
  baseline_power_h0_m = "h0 de base (m)",
  baseline_power_a = "Coeficiente a de base",
  baseline_power_b = "Expoente b de base",
  diagnostic_detail_level = "Nível de detalhe diagnóstico",
  index_group = "Grupo do índice",
  index_name = "Nome do índice",
  index_value = "Valor do índice",
  index_unit = "Unidade",
  index_class = "Classe",
  index_description = "Descrição",
  display_order = "Ordem",
  group_type = "Tipo de grupo",
  group_value = "Valor do grupo",
  n_group = "Número de pontos no grupo",
  spread_value = "Amplitude",
  relative_spread = "Amplitude relativa",
  rating_predicted_discharge_m3s = "Vazão pela curva-chave (m³/s)",
  rating_log_residual = "Resíduo log da curva-chave",
  rating_relative_residual_pct = "Resíduo relativo da curva-chave (%)",
  outside_residual_envelope = "Fora do envelope residual",
  power_log_residual = "Resíduo log do modelo de base",
  power_relative_residual_pct = "Resíduo relativo do modelo de base (%)",
  regime_number = "Regime temporal",
  regime_label = "Rótulo do regime"
)
