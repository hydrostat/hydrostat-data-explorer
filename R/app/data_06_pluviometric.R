# ============================================================
# data_06_pluviometric.R
# Purpose: User-provided daily pluviometric data parsing and validation.
# ============================================================
# BEGIN ORIGINAL BODY
# ------------------------------------------------------------
# User-provided daily pluviometric data helpers
# ------------------------------------------------------------

build_pluviometric_result <- function(data, source_type, source_label) {
  required_cols <- c("station_code", "date", "variable", "value")
  missing_cols <- setdiff(required_cols, names(data))
  
  if (length(missing_cols) > 0 || nrow(data) == 0) {
    stop(
      "O arquivo foi lido, mas não contém uma série diária reconhecida. ",
      "Verifique se o arquivo corresponde ao tipo selecionado: ZIP do HidroWeb, *_Chuvas.csv, XML da operação HidroSerieHistorica ou JSON da API com campos diários.",
      call. = FALSE
    )
  }
  
  rainfall <- data |>
    dplyr::filter(variable == "rainfall") |>
    dplyr::arrange(date)
  
  if (nrow(rainfall) == 0) {
    stop(
      "O arquivo foi lido, mas não contém colunas diárias de precipitação. ",
      "Para este módulo, use dados de chuva diária, como colunas Chuva01, Chuva_01 ou equivalentes.",
      call. = FALSE
    )
  }
  
  station_codes <- sort(unique(stats::na.omit(as.character(rainfall$station_code))))
  
  if (length(station_codes) == 0) {
    stop(
      "Os dados foram lidos, mas não foi possível identificar o código da estação.",
      call. = FALSE
    )
  }
  
  list(
    source_type = source_type,
    source_label = source_label,
    station_codes = station_codes,
    data = rainfall,
    rainfall = rainfall
  )
}

read_pluviometric_from_hidroweb_zip <- function(path) {
  unzip_dir <- tempfile("hidroweb_plu_zip_")
  dir.create(unzip_dir, recursive = TRUE, showWarnings = FALSE)
  
  extracted_files <- utils::unzip(path, exdir = unzip_dir)
  csv_files <- extracted_files[grepl("\\.csv$", extracted_files, ignore.case = TRUE)]
  
  if (length(csv_files) == 0) {
    stop("O ZIP não contém arquivos CSV.", call. = FALSE)
  }
  
  rainfall_files <- csv_files[grepl("Chuva|Chuvas|Pluv", basename(csv_files), ignore.case = TRUE)]
  
  if (length(rainfall_files) == 0) {
    stop("O ZIP não contém arquivo diário de chuva reconhecido.", call. = FALSE)
  }
  
  daily_data <- purrr::map_dfr(rainfall_files, function(file) {
    table <- read_hidroweb_csv_table(file)
    standardize_ana_daily_table(
      data = table,
      source_label = paste0("HidroWeb ZIP: ", basename(file))
    ) |>
      dplyr::filter(variable == "rainfall")
  })
  
  build_pluviometric_result(
    data = daily_data,
    source_type = "hidroweb_zip",
    source_label = "HidroWeb ZIP completo"
  )
}

read_pluviometric_from_hidroweb_rainfall_csv <- function(path) {
  table <- read_hidroweb_csv_table(path)
  
  daily_data <- standardize_ana_daily_table(
    data = table,
    source_label = paste0("HidroWeb CSV: ", basename(path))
  ) |>
    dplyr::filter(variable == "rainfall")
  
  build_pluviometric_result(
    data = daily_data,
    source_type = "hidroweb_rainfall_csv",
    source_label = "HidroWeb CSV de chuvas"
  )
}

read_pluviometric_from_ana_xml <- function(path_or_url) {
  table <- read_ana_xml_series_table(path_or_url)
  
  daily_data <- standardize_ana_daily_table(
    data = table,
    source_label = "ANA WebService XML"
  ) |>
    dplyr::filter(variable == "rainfall")
  
  build_pluviometric_result(
    data = daily_data,
    source_type = "ana_xml",
    source_label = "ANA WebService XML"
  )
}

read_pluviometric_from_ana_json <- function(path) {
  table <- read_ana_json_series_table(path)
  
  daily_data <- standardize_ana_daily_table(
    data = table,
    source_label = "ANA API JSON"
  ) |>
    dplyr::filter(variable == "rainfall")
  
  build_pluviometric_result(
    data = daily_data,
    source_type = "ana_json",
    source_label = "ANA API JSON"
  )
}

validate_pluviometric_station_code <- function(result, selected_station_code) {
  validate_fluviometric_station_code(result, selected_station_code)
}

