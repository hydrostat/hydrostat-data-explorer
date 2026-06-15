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

parts_dir <- file.path("exports", "database_parts")
parts_manifest_path <- file.path(parts_dir, "database_parts_manifest.csv")

required_files <- c(
  "app.R",
  "R/app_config.R",
  "R/app_data.R",
  "R/app_ui.R",
  "R/app_server.R",
  "R/station_diagnostic_functions.R",
  "www/styles.css",
  parts_manifest_path,
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

parts_manifest <- utils::read.csv(
  parts_manifest_path,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

required_parts_columns <- c(
  "part_order",
  "part_file",
  "part_size_bytes",
  "part_sha256",
  "database_size_bytes",
  "database_sha256"
)

missing_parts_columns <- setdiff(required_parts_columns, names(parts_manifest))
if (length(missing_parts_columns) > 0 || nrow(parts_manifest) == 0) {
  stop("The database-parts manifest is invalid.", call. = FALSE)
}

parts_manifest <- parts_manifest[order(parts_manifest$part_order), , drop = FALSE]
database_part_files <- file.path(parts_dir, parts_manifest$part_file)
missing_database_parts <- database_part_files[!file.exists(database_part_files)]

if (length(missing_database_parts) > 0) {
  stop(
    "Missing publication database parts: ",
    paste(missing_database_parts, collapse = ", "),
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
  "shiny", "DBI", "duckdb", "digest", "dplyr", "tidyr", "purrr", "readr",
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

source(file.path("R", "app_config.R"), local = globalenv())
source(file.path("R", "app_data.R"), local = globalenv())

required_runtime_helpers <- c(
  "connect_shiny_database",
  "disconnect_shiny_database",
  "app_db_tables",
  "app_table_exists",
  "app_table_fields",
  "read_app_table",
  "read_app_table_columns",
  "read_station_table",
  "load_station_index"
)

missing_runtime_helpers <- required_runtime_helpers[
  !vapply(
    required_runtime_helpers,
    exists,
    logical(1),
    mode = "function",
    inherits = TRUE
  )
]

if (length(missing_runtime_helpers) > 0) {
  stop(
    "Missing runtime helper functions: ",
    paste(missing_runtime_helpers, collapse = ", "),
    call. = FALSE
  )
}

validate_shiny_database_parts(
  parts_dir = app_config$db_parts_dir,
  manifest = parts_manifest,
  check_hashes = TRUE
)

app_diagnostic_env$resolved_db_path <- NULL
app_diagnostic_env$resolved_db_source <- NULL

reconstructed_db <- resolve_shiny_database_path(
  prefer_complete = FALSE,
  force_rebuild = TRUE
)

con <- NULL
metadata <- NULL
station_check <- NULL

tryCatch(
  {
    con <- DBI::dbConnect(
      duckdb::duckdb(),
      dbdir = reconstructed_db,
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

    runtime_station_index <- load_station_index(con)
    if (
      nrow(runtime_station_index) != station_check$n_rows[[1]] ||
        dplyr::n_distinct(runtime_station_index$station_code) != station_check$n_unique[[1]]
    ) {
      stop("The runtime station-index load did not match the database counts.", call. = FALSE)
    }
  },
  finally = {
    if (!is.null(con)) {
      disconnect_shiny_database(con)
      con <- NULL
    }
  }
)

complete_db <- file.path("exports", "shiny_minimal.duckdb")
complete_db_available <- file.exists(complete_db) &&
  !is_git_lfs_pointer_file(complete_db) &&
  isTRUE(file.info(complete_db)$size > 1024^2)

if (complete_db_available) {
  expected_hash <- unique(tolower(as.character(parts_manifest$database_sha256)))
  complete_hash <- tolower(shiny_database_sha256(complete_db))

  if (length(expected_hash) != 1 || !identical(complete_hash, expected_hash)) {
    stop("The local complete DuckDB differs from the database-parts manifest.", call. = FALSE)
  }
}

spatial_layers <- readRDS(
  file.path("exports", "spatial_layers", "shiny_spatial_layers.rds")
)

if (is.null(spatial_layers) || length(spatial_layers) == 0) {
  stop("The spatial layer RDS is empty.", call. = FALSE)
}

manifest_present <- file.exists("manifest.json")
if (manifest_present) {
  deployment_manifest <- jsonlite::fromJSON("manifest.json", simplifyVector = FALSE)
  deployment_files <- names(deployment_manifest$files)

  if ("exports/shiny_minimal.duckdb" %in% deployment_files) {
    stop("manifest.json still includes the complete DuckDB.", call. = FALSE)
  }

  missing_manifest_parts <- setdiff(
    gsub("\\\\", "/", database_part_files),
    deployment_files
  )

  if (length(missing_manifest_parts) > 0) {
    stop(
      "manifest.json is missing database parts: ",
      paste(missing_manifest_parts, collapse = ", "),
      call. = FALSE
    )
  }
}

message("Release validation passed.")
message("Parsed R files: ", nrow(parse_results))
message("Database parts: ", nrow(parts_manifest))
message("Reconstructed database bytes: ", file.info(reconstructed_db)$size)
message("Reconstructed database SHA-256: ", shiny_database_sha256(reconstructed_db))
message("Stations: ", station_check$n_rows[[1]])
message("Spatial objects: ", length(spatial_layers))
message("Local complete DuckDB available: ", complete_db_available)
message("manifest.json present: ", manifest_present)
