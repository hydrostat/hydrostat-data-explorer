# ============================================================
# data_05_ana_api.R
# Purpose: Session-only ANA authentication and annual API download helpers.
# ============================================================
# BEGIN ORIGINAL BODY
# ------------------------------------------------------------
# ANA authenticated API helpers for session-only Shiny downloads
# ------------------------------------------------------------

ana_api_items_to_tibble <- function(items) {
  if (is.null(items) || length(items) == 0) {
    return(tibble::tibble())
  }
  
  if (is.data.frame(items)) {
    return(tibble::as_tibble(items))
  }
  
  if (is.list(items) && length(items) > 0 && is.list(items[[1]])) {
    return(dplyr::bind_rows(items))
  }
  
  tibble::as_tibble(items)
}

ana_api_safe_n_items <- function(parsed) {
  if (is.null(parsed$items)) {
    return(0L)
  }
  
  if (is.data.frame(parsed$items)) {
    return(nrow(parsed$items))
  }
  
  if (is.list(parsed$items)) {
    return(length(parsed$items))
  }
  
  1L
}

ana_api_get_scalar <- function(x, field) {
  if (!is.null(x[[field]]) && length(x[[field]]) > 0) {
    return(as.character(x[[field]][1]))
  }
  
  NA_character_
}

ana_api_authenticate_session <- function(identificador, senha) {
  identificador <- trimws(as.character(identificador)[1])
  senha <- as.character(senha)[1]
  
  if (is.na(identificador) || identificador == "" || is.na(senha) || senha == "") {
    stop("Informe o identificador e a senha da API ANA.", call. = FALSE)
  }
  
  auth_url <- paste0(app_config$ana_api_base_url, app_config$ana_api_auth_path)
  
  response <- httr2::request(auth_url) |>
    httr2::req_method("GET") |>
    httr2::req_headers(
      Identificador = identificador,
      Senha = senha,
      accept = "*/*"
    ) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_timeout(app_config$ana_api_request_timeout_seconds) |>
    httr2::req_perform()
  
  http_status <- httr2::resp_status(response)
  response_text <- httr2::resp_body_string(response)
  
  if (http_status < 200 || http_status >= 300) {
    stop(
      "Falha na autenticação da API ANA. Código HTTP: ",
      http_status,
      ".",
      call. = FALSE
    )
  }
  
  parsed <- tryCatch(
    jsonlite::fromJSON(response_text, simplifyVector = FALSE),
    error = function(e) e
  )
  
  if (inherits(parsed, "error")) {
    stop("A resposta de autenticação da API ANA não pôde ser lida como JSON.", call. = FALSE)
  }
  
  token <- NULL
  
  if (!is.null(parsed$items$tokenautenticacao)) {
    token <- parsed$items$tokenautenticacao
  }
  
  if (is.null(token) && is.list(parsed$items) && length(parsed$items) > 0) {
    if (!is.null(parsed$items[[1]]$tokenautenticacao)) {
      token <- parsed$items[[1]]$tokenautenticacao
    }
  }
  
  if (is.null(token) || length(token) == 0 || is.na(token) || token == "") {
    stop("A resposta de autenticação não contém tokenautenticacao válido.", call. = FALSE)
  }
  
  list(
    token = as.character(token),
    created_at = Sys.time(),
    expires_at = Sys.time() + 60 * 60
  )
}

ana_api_is_authorization_problem <- function(http_status, response_text = "", parsed = NULL) {
  if (!is.na(http_status) && http_status %in% c(401, 403)) {
    return(TRUE)
  }
  
  text <- stringr::str_to_lower(paste(response_text, collapse = " "))
  
  if (stringr::str_detect(text, "token|autoriz|auth|credencial|expir")) {
    return(TRUE)
  }
  
  if (!is.null(parsed)) {
    api_message <- stringr::str_to_lower(
      paste(
        ana_api_get_scalar(parsed, "status"),
        ana_api_get_scalar(parsed, "message"),
        ana_api_get_scalar(parsed, "code"),
        collapse = " "
      )
    )
    
    if (stringr::str_detect(api_message, "token|autoriz|auth|credencial|expir")) {
      return(TRUE)
    }
  }
  
  FALSE
}

ana_api_build_query_url <- function(endpoint, station_code, date_start, date_end) {
  query <- list(
    "Código da Estação" = as.character(station_code),
    "Data Inicial (yyyy-MM-dd)" = as.character(as.Date(date_start)),
    "Data Final (yyyy-MM-dd)" = as.character(as.Date(date_end)),
    "Tipo Filtro Data" = "DATA_LEITURA"
  )
  
  query_string <- paste(
    paste0(
      utils::URLencode(names(query), reserved = TRUE),
      "=",
      utils::URLencode(unlist(query), reserved = TRUE)
    ),
    collapse = "&"
  )
  
  paste0(app_config$ana_api_base_url, endpoint, "?", query_string)
}

ana_api_make_report_row <- function(task, status, n_attempts, n_records, message) {
  tibble::tibble(
    station_code = as.character(task$station_code),
    module = as.character(task$module),
    route_name = as.character(task$route_name),
    variable = as.character(task$variable),
    year = as.integer(task$year),
    date_start = as.character(as.Date(task$date_start)),
    date_end = as.character(as.Date(task$date_end)),
    status = as.character(status),
    n_attempts = as.integer(n_attempts),
    n_records = as.integer(n_records),
    message = as.character(message)
  )
}

ana_api_download_task_once <- function(task, token) {
  if (is.null(token) || is.na(token) || token == "") {
    return(list(
      status = "paused_auth",
      data = tibble::tibble(),
      report = ana_api_make_report_row(
        task, "paused_auth", 0L, NA_integer_,
        "Token ausente. Obtenha novo token para continuar."
      )
    ))
  }
  
  request_url <- ana_api_build_query_url(
    endpoint = task$endpoint,
    station_code = task$station_code,
    date_start = task$date_start,
    date_end = task$date_end
  )
  
  response <- tryCatch(
    {
      httr2::request(request_url) |>
        httr2::req_headers(
          Authorization = paste("Bearer", token),
          accept = "*/*"
        ) |>
        httr2::req_error(is_error = function(resp) FALSE) |>
        httr2::req_timeout(app_config$ana_api_request_timeout_seconds) |>
        httr2::req_perform()
    },
    error = function(e) e
  )
  
  if (inherits(response, "error")) {
    return(list(
      status = "request_failed",
      data = tibble::tibble(),
      message = paste("Erro de requisição:", conditionMessage(response))
    ))
  }
  
  http_status <- httr2::resp_status(response)
  response_text <- httr2::resp_body_string(response)
  
  if (ana_api_is_authorization_problem(http_status, response_text)) {
    return(list(
      status = "paused_auth",
      data = tibble::tibble(),
      report = ana_api_make_report_row(
        task, "paused_auth", 1L, NA_integer_,
        "Token expirado, inválido ou não autorizado. Obtenha novo token para retomar."
      )
    ))
  }
  
  if (http_status < 200 || http_status >= 300) {
    return(list(
      status = "request_failed",
      data = tibble::tibble(),
      message = paste("Código HTTP", http_status)
    ))
  }
  
  parsed <- tryCatch(
    jsonlite::fromJSON(response_text, flatten = TRUE),
    error = function(e) e
  )
  
  if (inherits(parsed, "error")) {
    return(list(
      status = "request_failed",
      data = tibble::tibble(),
      message = paste("Erro ao ler JSON:", conditionMessage(parsed))
    ))
  }
  
  if (ana_api_is_authorization_problem(http_status, response_text, parsed)) {
    return(list(
      status = "paused_auth",
      data = tibble::tibble(),
      report = ana_api_make_report_row(
        task, "paused_auth", 1L, NA_integer_,
        "Resposta da API indica problema de autorização. Obtenha novo token para retomar."
      )
    ))
  }
  
  n_items <- ana_api_safe_n_items(parsed)
  
  if (n_items == 0) {
    return(list(
      status = "empty",
      data = tibble::tibble(),
      report = ana_api_make_report_row(
        task, "empty", 1L, 0L,
        "Resposta válida, mas sem registros para o período."
      )
    ))
  }
  
  items <- ana_api_items_to_tibble(parsed$items)
  
  list(
    status = "success",
    data = items,
    report = ana_api_make_report_row(
      task, "success", 1L, nrow(items),
      "Download concluído."
    )
  )
}

ana_api_download_task_with_retries <- function(task, token, max_attempts = 3) {
  last_message <- NA_character_
  
  for (attempt in seq_len(max_attempts)) {
    result <- ana_api_download_task_once(task, token)
    
    if (identical(result$status, "paused_auth")) {
      result$report$n_attempts <- attempt
      return(result)
    }
    
    if (identical(result$status, "success")) {
      result$report$n_attempts <- attempt
      return(result)
    }
    
    if (identical(result$status, "empty")) {
      result$report$n_attempts <- attempt
      return(result)
    }
    
    last_message <- result$message
  }
  
  list(
    status = "failed_after_3_attempts",
    data = tibble::tibble(),
    report = ana_api_make_report_row(
      task,
      "failed_after_3_attempts",
      max_attempts,
      0L,
      paste("Falha após", max_attempts, "tentativas.", last_message)
    )
  )
}

ana_api_make_year_tasks <- function(module, station_code, start_year, end_year) {
  years <- seq.int(as.integer(start_year), as.integer(end_year))
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  
  routes <- if (identical(module, "flu")) {
    tibble::tibble(
      route_name = c("vazao_diaria", "cotas_diarias"),
      endpoint = c(app_config$ana_api_daily_discharge_path, app_config$ana_api_daily_stage_path),
      variable = c("discharge", "stage")
    )
  } else {
    tibble::tibble(
      route_name = "chuva_diaria",
      endpoint = app_config$ana_api_daily_rainfall_path,
      variable = "rainfall"
    )
  }
  
  tidyr::crossing(
    tibble::tibble(year = years),
    routes
  ) |>
    dplyr::mutate(
      module = module,
      station_code = as.character(station_code),
      date_start = as.Date(paste0(year, "-01-01")),
      date_end = dplyr::if_else(
        year == current_year,
        Sys.Date(),
        as.Date(paste0(year, "-12-31"))
      ),
      task_id = dplyr::row_number()
    ) |>
    dplyr::select(
      task_id,
      module,
      station_code,
      route_name,
      endpoint,
      variable,
      year,
      date_start,
      date_end
    )
}

build_fluviometric_result_from_ana_api <- function(raw_data, download_report) {
  daily_data <- standardize_ana_daily_table(
    data = raw_data,
    source_label = "ANA API — download autenticado"
  )
  
  result <- build_fluviometric_result(
    data = daily_data,
    source_type = "ana_api_download",
    source_label = "ANA API — download autenticado"
  )
  
  result$download_report <- download_report
  result
}

build_pluviometric_result_from_ana_api <- function(raw_data, download_report) {
  daily_data <- standardize_ana_daily_table(
    data = raw_data,
    source_label = "ANA API — download autenticado"
  ) |>
    dplyr::filter(variable == "rainfall")
  
  result <- build_pluviometric_result(
    data = daily_data,
    source_type = "ana_api_download",
    source_label = "ANA API — download autenticado"
  )
  
  result$download_report <- download_report
  result
}

normalize_ana_daily_series <- function(data, variable_prefix, variable_name, unit, source_label) {
  if (nrow(data) == 0) {
    return(empty_standardized_daily_series())
  }
  
  station_col <- find_first_column(
    data,
    c("EstacaoCodigo", "codigoestacao", "CodigoEstacao", "CodigoDaEstacao")
  )
  
  date_col <- find_first_column(
    data,
    c("Data", "DataHora", "Data_Hora_Dado")
  )
  
  consistency_col <- find_first_column(
    data,
    c("NivelConsistencia", "Nivel_Consistencia")
  )
  
  daily_col <- find_first_column(
    data,
    c("MediaDiaria", "Mediadiaria")
  )
  
  if (is.na(station_col)) {
    stop("Não foi possível identificar o código da estação nos dados.", call. = FALSE)
  }
  
  if (is.na(date_col)) {
    stop("Não foi possível identificar a coluna de data nos dados.", call. = FALSE)
  }
  
  value_match <- stringr::str_match(
    names(data),
    paste0("^", variable_prefix, "_?(\\d{2})$")
  )
  
  status_match <- stringr::str_match(
    names(data),
    paste0("^", variable_prefix, "_?(\\d{2})_?Status$")
  )
  
  value_cols <- names(data)[!is.na(value_match[, 2])]
  value_days <- value_match[!is.na(value_match[, 2]), 2]
  
  status_cols <- names(data)[!is.na(status_match[, 2])]
  status_days <- status_match[!is.na(status_match[, 2]), 2]
  
  if (length(value_cols) == 0) {
    return(empty_standardized_daily_series())
  }
  
  month_date <- parse_ana_date(data[[date_col]])
  station_code <- as.character(data[[station_col]])
  
  consistency_level <- if (!is.na(consistency_col)) {
    as.character(data[[consistency_col]])
  } else {
    NA_character_
  }
  
  daily_flag <- if (!is.na(daily_col)) {
    as.character(data[[daily_col]])
  } else {
    NA_character_
  }
  
  out <- purrr::map_dfr(seq_along(value_cols), function(i) {
    day_value <- as.integer(value_days[[i]])
    value_col <- value_cols[[i]]
    status_col <- status_cols[match(value_days[[i]], status_days)]
    
    status_value <- if (!is.na(status_col)) {
      as.character(data[[status_col]])
    } else {
      rep(NA_character_, nrow(data))
    }
    
    date_value <- make_ana_month_day_date(
      month_date = month_date,
      day_value = day_value
    )
    
    valid_date <- !is.na(date_value)
    
    tibble::tibble(
      station_code = station_code,
      date = date_value,
      variable = variable_name,
      value = parse_ana_numeric(data[[value_col]]),
      unit = unit,
      source_status = status_value,
      consistency_level = consistency_level,
      daily_flag = daily_flag,
      source = source_label,
      source_column = value_col
    ) |>
      dplyr::filter(valid_date)
  })
  
  out
}

merge_ana_daily_variable <- function(data, variable_name) {
  if (is.null(data) || nrow(data) == 0) {
    return(tibble::tibble())
  }
  
  variable_data <- data |>
    dplyr::filter(variable == variable_name, !is.na(date)) |>
    dplyr::mutate(
      station_code = trimws(as.character(station_code)),
      date = as.Date(date),
      value = as.numeric(value),
      consistency_level = as.character(consistency_level),
      daily_flag = as.character(daily_flag),
      consistency_key = dplyr::if_else(
        is.na(consistency_level) | consistency_level == "",
        "sem_nivel",
        consistency_level
      )
    )
  
  if (nrow(variable_data) == 0) {
    return(tibble::tibble())
  }
  
  source_counts <- variable_data |>
    dplyr::count(station_code, variable, date, name = "n_source_records")
  
  same_consistency_counts <- variable_data |>
    dplyr::count(
      station_code,
      variable,
      date,
      consistency_key,
      name = "n_same_consistency_records"
    ) |>
    dplyr::group_by(station_code, variable, date) |>
    dplyr::summarise(
      max_same_consistency_records = max(n_same_consistency_records, na.rm = TRUE),
      has_duplicate_same_consistency = any(n_same_consistency_records > 1L),
      .groups = "drop"
    )
  
  variable_data |>
    dplyr::left_join(source_counts, by = c("station_code", "variable", "date")) |>
    dplyr::left_join(same_consistency_counts, by = c("station_code", "variable", "date")) |>
    dplyr::mutate(
      consistency_priority = dplyr::case_when(
        consistency_level == "2" ~ 1L,
        consistency_level == "1" ~ 2L,
        TRUE ~ 3L
      ),
      value_priority = dplyr::if_else(is.na(value), 2L, 1L),
      daily_priority = dplyr::case_when(
        daily_flag == "1" ~ 1L,
        daily_flag == "0" ~ 2L,
        TRUE ~ 3L
      )
    ) |>
    dplyr::arrange(
      station_code,
      variable,
      date,
      consistency_priority,
      value_priority,
      daily_priority
    ) |>
    dplyr::group_by(station_code, variable, date) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::select(
      station_code,
      date,
      variable,
      value,
      unit,
      source_status,
      consistency_level,
      daily_flag,
      source,
      source_column,
      n_source_records,
      max_same_consistency_records,
      has_duplicate_same_consistency
    )
}

standardize_ana_daily_table <- function(data, source_label) {
  discharge <- normalize_ana_daily_series(
    data = data,
    variable_prefix = "Vazao",
    variable_name = "discharge",
    unit = "m3/s",
    source_label = source_label
  )
  
  stage <- normalize_ana_daily_series(
    data = data,
    variable_prefix = "Cota",
    variable_name = "stage",
    unit = "cm",
    source_label = source_label
  )
  
  rainfall <- normalize_ana_daily_series(
    data = data,
    variable_prefix = "Chuva",
    variable_name = "rainfall",
    unit = "mm",
    source_label = source_label
  )
  
  dplyr::bind_rows(discharge, stage, rainfall)
}

build_fluviometric_result <- function(data, source_type, source_label) {
  required_cols <- c("station_code", "date", "variable", "value")
  missing_cols <- setdiff(required_cols, names(data))
  
  if (length(missing_cols) > 0 || nrow(data) == 0) {
    stop(
      "O arquivo foi lido, mas não contém uma série diária reconhecida. ",
      "Verifique se o arquivo corresponde ao tipo selecionado: ZIP do HidroWeb, *_Vazoes.csv, XML da operação HidroSerieHistorica ou JSON da API com campos diários.",
      call. = FALSE
    )
  }
  
  data <- data |>
    dplyr::arrange(variable, date)
  
  station_codes <- sort(unique(stats::na.omit(as.character(data$station_code))))
  
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
    data = data,
    discharge = dplyr::filter(data, variable == "discharge"),
    stage = dplyr::filter(data, variable == "stage"),
    rainfall = dplyr::filter(data, variable == "rainfall")
  )
}

combine_fluviometric_results <- function(
    ...,
    source_type = NULL,
    source_label = NULL,
    acquisition_notes = character()
) {
  results <- list(...)
  results <- results[!vapply(results, is.null, logical(1))]
  
  if (length(results) == 0) {
    stop("Nenhum resultado fluviométrico foi informado para combinação.", call. = FALSE)
  }
  
  data_list <- lapply(
    results,
    function(x) {
      if (!is.null(x$data) && is.data.frame(x$data)) {
        return(x$data)
      }
      
      tibble::tibble()
    }
  )
  
  combined_data <- dplyr::bind_rows(data_list)
  
  if (nrow(combined_data) == 0) {
    stop("Nenhuma série diária foi encontrada nos resultados combinados.", call. = FALSE)
  }
  
  if (is.null(source_type) || length(source_type) == 0 || is.na(source_type) || source_type == "") {
    source_type <- results[[1]]$source_type
  }
  
  if (is.null(source_label) || length(source_label) == 0 || is.na(source_label) || source_label == "") {
    source_label <- results[[1]]$source_label
  }
  
  combined <- build_fluviometric_result(
    data = combined_data,
    source_type = source_type,
    source_label = source_label
  )
  
  result_notes <- unlist(
    lapply(
      results,
      function(x) {
        if (!is.null(x$acquisition_notes)) {
          return(as.character(x$acquisition_notes))
        }
        
        character()
      }
    ),
    use.names = FALSE
  )
  
  combined$acquisition_notes <- unique(c(result_notes, acquisition_notes))
  combined$acquisition_notes <- combined$acquisition_notes[
    !is.na(combined$acquisition_notes) & combined$acquisition_notes != ""
  ]
  
  combined
}
read_fluviometric_from_hidroweb_zip <- function(path) {
  unzip_dir <- tempfile("hidroweb_zip_")
  dir.create(unzip_dir, recursive = TRUE, showWarnings = FALSE)
  
  extracted_files <- utils::unzip(path, exdir = unzip_dir)
  csv_files <- extracted_files[grepl("\\.csv$", extracted_files, ignore.case = TRUE)]
  
  if (length(csv_files) == 0) {
    stop("O ZIP não contém arquivos CSV.", call. = FALSE)
  }
  
  discharge_files <- csv_files[grepl("Vazoes", basename(csv_files), ignore.case = TRUE)]
  stage_files <- csv_files[grepl("Cotas", basename(csv_files), ignore.case = TRUE)]
  rainfall_files <- csv_files[grepl("Chuva|Chuvas|Pluv", basename(csv_files), ignore.case = TRUE)]
  
  selected_files <- c(discharge_files, stage_files, rainfall_files)
  
  if (length(selected_files) == 0) {
    stop("O ZIP não contém arquivos diários de vazões, cotas ou chuva reconhecidos.", call. = FALSE)
  }
  
  daily_data <- purrr::map_dfr(selected_files, function(file) {
    table <- read_hidroweb_csv_table(file)
    standardize_ana_daily_table(
      data = table,
      source_label = paste0("HidroWeb ZIP: ", basename(file))
    )
  })
  
  build_fluviometric_result(
    data = daily_data,
    source_type = "hidroweb_zip",
    source_label = "HidroWeb ZIP completo"
  )
}

read_fluviometric_from_hidroweb_discharge_csv <- function(path) {
  table <- read_hidroweb_csv_table(path)
  
  daily_data <- standardize_ana_daily_table(
    data = table,
    source_label = paste0("HidroWeb CSV: ", basename(path))
  )
  
  if (nrow(dplyr::filter(daily_data, variable == "discharge")) == 0) {
    stop(
      "O arquivo enviado não contém colunas diárias de vazão. ",
      "Para esta opção, envie o arquivo *_Vazoes.csv do HidroWeb, com colunas como Vazao01, Vazao_01 ou equivalentes. ",
      "Arquivos de perfil transversal, cotas, chuva, inventário ou metadados não são aceitos nesta opção.",
      call. = FALSE
    )
  }
  
  build_fluviometric_result(
    data = daily_data,
    source_type = "hidroweb_discharge_csv",
    source_label = "HidroWeb CSV de vazões"
  )
}

read_fluviometric_from_ana_xml <- function(path_or_url) {
  table <- read_ana_xml_series_table(path_or_url)
  
  daily_data <- standardize_ana_daily_table(
    data = table,
    source_label = "ANA WebService XML"
  )
  
  build_fluviometric_result(
    data = daily_data,
    source_type = "ana_xml",
    source_label = "ANA WebService XML"
  )
}

read_fluviometric_from_ana_json <- function(path) {
  table <- read_ana_json_series_table(path)
  
  daily_data <- standardize_ana_daily_table(
    data = table,
    source_label = "ANA API JSON"
  )
  
  build_fluviometric_result(
    data = daily_data,
    source_type = "ana_json",
    source_label = "ANA API JSON"
  )
}

normalize_station_code_for_comparison <- function(x) {
  x <- trimws(as.character(x))
  x <- gsub("^0+", "", x)
  x[x == ""] <- "0"
  x
}

validate_fluviometric_station_code <- function(result, selected_station_code) {
  selected_station_code <- trimws(as.character(selected_station_code))
  source_codes <- trimws(as.character(result$station_codes))
  source_codes <- source_codes[source_codes != ""]
  
  if (length(source_codes) == 0) {
    stop("Não foi possível validar o código da estação nos dados fornecidos.", call. = FALSE)
  }
  
  source_codes_compare <- normalize_station_code_for_comparison(source_codes)
  selected_code_compare <- normalize_station_code_for_comparison(selected_station_code)
  
  if (length(unique(source_codes_compare)) > 1) {
    stop(
      "Os dados fornecidos contêm mais de uma estação: ",
      paste(source_codes, collapse = ", "),
      ". Forneça dados somente da estação selecionada.",
      call. = FALSE
    )
  }
  
  if (!identical(source_codes_compare[[1]], selected_code_compare[[1]])) {
    stop(
      "O código da estação nos dados fornecidos é ",
      source_codes[[1]],
      ", mas a estação selecionada no sistema é ",
      selected_station_code,
      ".",
      call. = FALSE
    )
  }
  
  invisible(TRUE)
}
build_fluviometric_monthly_availability <- function(data, variable_name = "discharge") {
  variable_data <- data |>
    dplyr::filter(variable == variable_name)
  
  if (nrow(variable_data) == 0) {
    return(tibble::tibble())
  }
  
  valid_dates <- variable_data |>
    dplyr::filter(!is.na(date)) |>
    dplyr::pull(date)
  
  if (length(valid_dates) == 0) {
    return(tibble::tibble())
  }
  
  first_month <- as.Date(format(min(valid_dates, na.rm = TRUE), "%Y-%m-01"))
  last_month <- as.Date(format(max(valid_dates, na.rm = TRUE), "%Y-%m-01"))
  
  month_sequence <- seq.Date(first_month, last_month, by = "month")
  
  month_grid <- tibble::tibble(
    month_start = month_sequence,
    year = as.integer(format(month_start, "%Y")),
    month = as.integer(format(month_start, "%m")),
    next_month = seq.Date(first_month, last_month + 40, by = "month")[-1]
  ) |>
    dplyr::mutate(
      month_end = next_month - 1,
      days_expected = as.integer(month_end - month_start + 1)
    ) |>
    dplyr::select(-next_month)
  
  observed_days <- variable_data |>
    dplyr::filter(!is.na(date), !is.na(value)) |>
    dplyr::mutate(
      month_start = as.Date(format(date, "%Y-%m-01"))
    ) |>
    dplyr::distinct(month_start, date) |>
    dplyr::count(month_start, name = "days_observed")
  
  month_grid |>
    dplyr::left_join(observed_days, by = "month_start") |>
    dplyr::mutate(
      days_observed = dplyr::coalesce(days_observed, 0L),
      days_observed = pmin(days_observed, days_expected),
      days_missing = pmax(days_expected - days_observed, 0L),
      failure_pct = 100 * days_missing / days_expected,
      failure_class = dplyr::case_when(
        failure_pct == 100 ~ "100%",
        failure_pct >= 75 & failure_pct < 100 ~ "75–<100%",
        failure_pct >= 50 & failure_pct < 75 ~ "50–<75%",
        failure_pct >= 25 & failure_pct < 50 ~ "25–<50%",
        failure_pct > 0 & failure_pct < 25 ~ "0–<25%",
        failure_pct == 0 ~ "0%",
        TRUE ~ NA_character_
      ),
      failure_class = factor(
        failure_class,
        levels = c("100%", "75–<100%", "50–<75%", "25–<50%", "0–<25%", "0%")
      )
    )
}


