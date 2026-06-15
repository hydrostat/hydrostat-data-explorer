# ============================================================
# 03_validate_release.R
# Purpose: Validate the prepared public repository without
#          modifying the runtime database or source files.
# Run from the hydrostat-data-explorer repository root.
# ============================================================

repository_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

if (!file.exists(file.path(repository_dir, "app.R"))) {
  stop("Run this script from the hydrostat-data-explorer repository root.", call. = FALSE)
}

required_files <- c(
  "app.R",
  "R/app_config.R",
  "R/app_data.R",
  "R/app_ui.R",
  "R/app_server.R",
  "R/station_diagnostic_functions.R",
  "www/styles.css",
  "exports/shiny_minimal.duckdb",
  "exports/spatial_layers/shiny_spatial_layers.rds",
  "README.md",
  "LICENSE",
  "CITATION.cff",
  "PRIVACY.md",
  "DATA_NOTICE.md",
  ".gitignore",
  ".gitattributes"
)

missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0) {
  stop(
    paste("Missing required release files:", paste(missing_files, collapse = "\n")),
    call. = FALSE
  )
}

r_files <- list.files(
  repository_dir,
  pattern = "\\.R$",
  recursive = TRUE,
  full.names = TRUE
)

parse_results <- lapply(r_files, function(path) {
  error <- tryCatch(
    {
      parse(file = path, encoding = "UTF-8")
      NA_character_
    },
    error = function(e) conditionMessage(e)
  )

  data.frame(
    file = substring(gsub("\\\\", "/", path), nchar(repository_dir) + 2),
    parsed = is.na(error),
    error = ifelse(is.na(error), "", error),
    stringsAsFactors = FALSE
  )
})

parse_results <- do.call(rbind, parse_results)
if (!all(parse_results$parsed)) {
  print(parse_results[!parse_results$parsed, , drop = FALSE])
  stop("One or more R files failed to parse.", call. = FALSE)
}

runtime_packages <- c(
  "shiny", "DBI", "duckdb", "dplyr", "tidyr", "purrr", "readr",
  "stringr", "ggplot2", "leaflet", "sf", "DT", "htmltools", "scales",
  "plotly", "httr2", "jsonlite", "lubridate", "evd", "xml2", "ragg"
)

missing_packages <- runtime_packages[
  !vapply(runtime_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Missing runtime packages: ",
    paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}

forbidden_paths <- c(
  paste0("C:", "/", "Users", "/"),
  paste0("C:", "\\", "Users", "\\"),
  "ana_api_get_clean"
)

source_files <- c(
  list.files("R", recursive = TRUE, full.names = TRUE),
  "app.R"
)

source_text <- unlist(lapply(source_files, readLines, warn = FALSE, encoding = "UTF-8"))
forbidden_matches <- unique(unlist(lapply(
  forbidden_paths,
  function(pattern) grep(pattern, source_text, value = TRUE, fixed = TRUE)
)))

if (length(forbidden_matches) > 0) {
  stop("A private/local path was found in runtime source files.", call. = FALSE)
}

sensitive_patterns <- c(
  "ANA_HIDRO_IDENTIFICADOR=",
  "ANA_HIDRO_SENHA=",
  "Authorization: Bearer "
)

sensitive_matches <- unique(unlist(lapply(
  sensitive_patterns,
  function(pattern) grep(pattern, source_text, value = TRUE, fixed = TRUE)
)))

if (length(sensitive_matches) > 0) {
  stop("Potential embedded credential or token content was found.", call. = FALSE)
}

con <- NULL
metadata <- NULL
station_check <- NULL

tryCatch(
  {
    con <- DBI::dbConnect(
      duckdb::duckdb(),
      dbdir = file.path("exports", "shiny_minimal.duckdb"),
      read_only = TRUE
    )

    required_database_objects <- c(
      "metadata",
      "stations_minimal",
      "discharge_measurements",
      "rating_curves",
      "cross_sections",
      "cross_section_vertices"
    )

    available_objects <- DBI::dbListTables(con)
    missing_objects <- setdiff(required_database_objects, available_objects)
    if (length(missing_objects) > 0) {
      stop(
        "Missing required database objects: ",
        paste(missing_objects, collapse = ", "),
        call. = FALSE
      )
    }

    metadata <- DBI::dbGetQuery(
      con,
      "SELECT CAST(key AS VARCHAR) AS key, CAST(value AS VARCHAR) AS value FROM metadata"
    )

    required_metadata_keys <- c(
      "source_metadata.shiny_authenticated_download_model",
      "api_statement",
      "privacy_statement"
    )

    missing_metadata_keys <- setdiff(required_metadata_keys, metadata$key)
    if (length(missing_metadata_keys) > 0) {
      stop(
        "Publication metadata was not patched. Missing keys: ",
        paste(missing_metadata_keys, collapse = ", "),
        call. = FALSE
      )
    }

    station_check <- DBI::dbGetQuery(
      con,
      paste(
        "SELECT COUNT(*) AS n_rows,",
        "COUNT(DISTINCT CAST(station_code AS VARCHAR)) AS n_unique",
        "FROM stations_minimal"
      )
    )

    if (station_check$n_rows[[1]] != station_check$n_unique[[1]]) {
      stop("Duplicate station codes were found in stations_minimal.", call. = FALSE)
    }
  },
  finally = {
    if (!is.null(con)) {
      DBI::dbDisconnect(con, shutdown = TRUE)
      con <- NULL
    }
  }
)

spatial_layers <- readRDS(
  file.path("exports", "spatial_layers", "shiny_spatial_layers.rds")
)

if (is.null(spatial_layers) || length(spatial_layers) == 0) {
  stop("The spatial layer RDS is empty.", call. = FALSE)
}

validation_dir <- file.path(
  dirname(repository_dir),
  "hydrostat-data-explorer_validation"
)
dir.create(validation_dir, recursive = TRUE, showWarnings = FALSE)

validation_report <- c(
  paste("Validation date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
  paste("Repository:", repository_dir),
  paste("R:", R.version.string),
  paste("Platform:", R.version$platform),
  paste("Parsed R files:", nrow(parse_results)),
  paste("Stations:", station_check$n_rows[[1]]),
  paste("Unique station codes:", station_check$n_unique[[1]]),
  paste("Spatial objects:", length(spatial_layers)),
  paste(
    "DuckDB MiB:",
    round(file.info(file.path("exports", "shiny_minimal.duckdb"))$size / 1024^2, 2)
  ),
  paste(
    "Spatial RDS MiB:",
    round(file.info(file.path("exports", "spatial_layers", "shiny_spatial_layers.rds"))$size / 1024^2, 2)
  ),
  paste("manifest.json present:", file.exists("manifest.json")),
  "",
  "Runtime package versions:",
  paste(
    runtime_packages,
    vapply(
      runtime_packages,
      function(package) as.character(utils::packageVersion(package)),
      character(1)
    ),
    sep = " = "
  ),
  "",
  "sessionInfo():",
  capture.output(sessionInfo())
)

validation_report_path <- file.path(
  validation_dir,
  paste0("release_validation_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt")
)
writeLines(validation_report, validation_report_path, useBytes = TRUE)

message("Release validation passed.")
message("Parsed R files: ", nrow(parse_results))
message("Stations: ", station_check$n_rows[[1]])
message("Spatial objects: ", length(spatial_layers))
message("Validation report: ", normalizePath(validation_report_path, winslash = "/"))
