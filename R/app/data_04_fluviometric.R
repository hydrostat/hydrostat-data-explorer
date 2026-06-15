# ============================================================
# data_04_fluviometric.R
# Purpose: User-provided daily fluviometric data parsing and validation.
# ============================================================
# BEGIN ORIGINAL BODY
# ------------------------------------------------------------
# User-provided daily fluviometric data helpers
# ------------------------------------------------------------

parse_ana_numeric <- function(x) {
  if (length(x) == 0 || is.null(x)) {
    return(numeric())
  }
  
  if (is.numeric(x)) {
    return(as.numeric(x))
  }
  
  x_chr <- trimws(as.character(x))
  x_chr[x_chr %in% c("", "NA", "NaN", "NULL", "null")] <- NA_character_
  x_chr <- gsub(",", ".", x_chr, fixed = TRUE)
  
  suppressWarnings(as.numeric(x_chr))
}

parse_ana_date <- function(x) {
  if (length(x) == 0 || is.null(x)) {
    return(as.Date(character()))
  }
  
  x_chr <- trimws(as.character(x))
  x_chr[x_chr %in% c("", "NA", "NULL", "null")] <- NA_character_
  
  suppressWarnings(
    as.Date(
      x_chr,
      tryFormats = c(
        "%d/%m/%Y",
        "%Y-%m-%d",
        "%Y-%m-%d %H:%M:%S",
        "%Y-%m-%d %H:%M:%OS"
      )
    )
  )
}

make_ana_month_day_date <- function(month_date, day_value) {
  year_value <- as.integer(format(month_date, "%Y"))
  month_value <- as.integer(format(month_date, "%m"))
  
  date_value <- suppressWarnings(
    as.Date(
      ISOdate(
        year = year_value,
        month = month_value,
        day = day_value,
        tz = "UTC"
      )
    )
  )
  
  valid_date <- !is.na(month_date) &
    !is.na(date_value) &
    format(date_value, "%Y-%m") == format(month_date, "%Y-%m")
  
  date_value[!valid_date] <- as.Date(NA)
  date_value
}

find_first_column <- function(data, candidates) {
  found <- intersect(candidates, names(data))
  if (length(found) == 0) {
    return(NA_character_)
  }
  found[[1]]
}

empty_standardized_daily_series <- function() {
  tibble::tibble(
    station_code = character(),
    date = as.Date(character()),
    variable = character(),
    value = numeric(),
    unit = character(),
    source_status = character(),
    consistency_level = character(),
    daily_flag = character(),
    source = character(),
    source_column = character()
  )
}

build_ana_historical_xml_url <- function(
    station_code,
    data_type = "3",
    consistency_level = "2"
) {
  station_code <- as.character(station_code)[1]
  data_type <- as.character(data_type)[1]
  consistency_level <- as.character(consistency_level)[1]
  
  if (is.na(station_code) || station_code == "") {
    stop("Código da estação não informado para montar a URL do WebService.", call. = FALSE)
  }
  
  if (!data_type %in% c("1", "2", "3")) {
    stop(
      "Tipo de dado inválido para o WebService ANA. Use 1 para cotas, 2 para chuvas ou 3 para vazões.",
      call. = FALSE
    )
  }
  
  data_end <- format(Sys.Date(), "%d/%m/%Y")
  
  paste0(
    "http://telemetriaws1.ana.gov.br/ServiceANA.asmx/HidroSerieHistorica",
    "?codEstacao=", station_code,
    "&dataInicio=01/01/1900",
    "&dataFim=", data_end,
    "&tipoDados=", data_type,
    "&nivelConsistencia=", consistency_level
  )
}

read_hidroweb_csv_table <- function(path) {
  lines <- readLines(path, warn = FALSE, encoding = "Latin1")
  header_candidates <- which(grepl("^\\s*EstacaoCodigo\\s*;", lines))
  
  if (length(header_candidates) == 0) {
    stop("Não foi possível localizar o cabeçalho EstacaoCodigo no arquivo CSV.", call. = FALSE)
  }
  header_line <- header_candidates[[1]]
  utils::read.csv2(
    file = path,
    skip = header_line - 1,
    stringsAsFactors = FALSE,
    na.strings = c("", "NA", "NULL", "null"),
    fileEncoding = "Latin1",
    check.names = FALSE
  ) |>
    tibble::as_tibble()
}

read_ana_xml_series_table <- function(path_or_url) {
  xml_path <- path_or_url
  
  if (grepl("^https?://", path_or_url, ignore.case = TRUE)) {
    xml_path <- tempfile(fileext = ".xml")
    
    download_result <- tryCatch(
      {
        utils::download.file(path_or_url, xml_path, mode = "wb", quiet = TRUE)
        TRUE
      },
      error = function(e) {
        e
      }
    )
    
    if (inherits(download_result, "error")) {
      stop(
        "Não foi possível fazer o download automático pelo WebService da ANA. ",
        "Verifique a conexão com a internet e tente novamente. ",
        "Se o problema persistir, baixe o XML manualmente e envie o arquivo.",
        call. = FALSE
      )
    }
  }
  
  doc <- xml2::read_xml(xml_path)
  nodes <- xml2::xml_find_all(doc, ".//*[local-name()='SerieHistorica']")
  
  if (length(nodes) == 0) {
    stop(
      "O XML foi lido, mas não contém registros SerieHistorica. ",
      "Verifique se o arquivo corresponde à operação HidroSerieHistorica do WebService da ANA.",
      call. = FALSE
    )
  }
  
  records <- lapply(nodes, function(node) {
    children <- xml2::xml_children(node)
    values <- xml2::xml_text(children)
    names(values) <- xml2::xml_name(children)
    as.list(values)
  })
  
  dplyr::bind_rows(records)
}

read_ana_json_series_table <- function(path) {
  content <- jsonlite::fromJSON(path, flatten = TRUE)
  
  if (is.null(content$items) || length(content$items) == 0) {
    stop("O JSON não contém o campo items com registros de série histórica.", call. = FALSE)
  }
  
  tibble::as_tibble(content$items)
}


