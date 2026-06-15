# ============================================================
# pipeline/R/010_auth.R
# Purpose: Authenticate with ANA HidroWebService and manage token cache
# ============================================================

source(file.path("pipeline", "R", "000_setup.R"), local = TRUE)

ana_get_token <- function(force_refresh = FALSE) {
  # Reuse the token to avoid unnecessary authentication requests.
  if (!force_refresh && file.exists(token_cache_file)) {
    token_cache <- readRDS(token_cache_file)

    if (!is.null(token_cache$token) &&
        !is.null(token_cache$expires_at) &&
        Sys.time() < token_cache$expires_at) {
      return(token_cache$token)
    }
  }

  identificador <- Sys.getenv("ANA_HIDRO_IDENTIFICADOR")
  senha <- Sys.getenv("ANA_HIDRO_SENHA")

  if (identificador == "" || senha == "") {
    log_auth_attempt(FALSE, NA_integer_, "Missing ANA credentials in environment variables")
    stop("Missing ANA credentials. Check ANA_HIDRO_IDENTIFICADOR and ANA_HIDRO_SENHA in your local .Renviron.")
  }

  response <- request(ana_auth_url) |>
    req_method("GET") |>
    req_headers(
      Identificador = identificador,
      Senha = senha
    ) |>
    req_error(is_error = function(resp) FALSE) |>
    req_perform()

  http_code <- resp_status(response)
  
  content_type <- httr2::resp_content_type(response)
  
  if (!grepl("json", content_type, ignore.case = TRUE)) {
    response_text <- httr2::resp_body_string(response)
    
    stop(
      "Authentication response was not JSON. Content-Type: ",
      content_type,
      ". First characters: ",
      substr(response_text, 1, 300)
    )
  }
  
  response_body <- resp_body_json(response, simplifyVector = FALSE)

  api_message <- response_body$message
  if (is.null(api_message)) {
    api_message <- NA_character_
  }

  if (http_code < 200 || http_code >= 300) {
    log_auth_attempt(FALSE, http_code, api_message)
    stop(paste0("ANA authentication failed. HTTP status: ", http_code))
  }

  token <- NULL

  if (!is.null(response_body$items$tokenautenticacao)) {
    token <- response_body$items$tokenautenticacao
  }

  if (is.null(token) && is.list(response_body$items) && length(response_body$items) > 0) {
    if (!is.null(response_body$items[[1]]$tokenautenticacao)) {
      token <- response_body$items[[1]]$tokenautenticacao
    }
  }

  if (is.null(token) || token == "") {
    log_auth_attempt(FALSE, http_code, "Missing tokenautenticacao in response")
    stop("ANA authentication response did not include tokenautenticacao.")
  }

  token_cache <- list(
    token = token,
    created_at = Sys.time(),
    expires_at = Sys.time() + token_valid_minutes * 60
  )

  saveRDS(token_cache, token_cache_file)
  log_auth_attempt(TRUE, http_code, api_message)

  token
}

# Manual test:
# token <- ana_get_token()
# nchar(token)
