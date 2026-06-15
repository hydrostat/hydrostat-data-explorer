# ============================================================
# pipeline/R/061_check_shiny_export_local.R
# Inspect the local Shiny DuckDB export before running the app
# ============================================================

# Load packages
library(DBI)
library(duckdb)

# Load shared pipeline helpers
source(file.path("pipeline", "helpers", "duckdb_helpers.R"), local = TRUE)

# Define paths
export_db <- file.path("exports", "shiny_minimal.duckdb")

app_files <- c(
  "app.R",
  file.path("R", "app_config.R"),
  file.path("R", "app_data.R"),
  file.path("R", "app_ui.R"),
  file.path("R", "app_server.R"),
  file.path("R", "station_diagnostic_functions.R")
)

# Critical check: local export must exist.
if (!file.exists(export_db)) {
  stop("Local Shiny export database not found: ", export_db)
}

export_db_size_mb <- round(file.info(export_db)$size / 1024^2, 2)

# Connect to the local Shiny export.
con <- DBI::dbConnect(duckdb::duckdb(), dbdir = export_db, read_only = TRUE)
on.exit(DBI::dbDisconnect(con), add = TRUE)

# ------------------------------------------------------------
# Helper functions
# ------------------------------------------------------------

count_rows <- function(object_name) {
  DBI::dbGetQuery(
    con,
    paste0("SELECT COUNT(*) AS n_rows FROM ", quote_ident(object_name))
  )$n_rows[1]
}

get_export_objects <- function() {
  tables <- DBI::dbGetQuery(
    con,
    "SELECT table_name, table_type
     FROM information_schema.tables
     WHERE table_schema = 'main'
     ORDER BY table_type, table_name"
  )
  
  views <- tryCatch(
    {
      DBI::dbGetQuery(
        con,
        "SELECT table_name, 'VIEW' AS table_type
         FROM information_schema.views
         WHERE table_schema = 'main'
         ORDER BY table_name"
      )
    },
    error = function(e) {
      data.frame(
        table_name = character(),
        table_type = character(),
        stringsAsFactors = FALSE
      )
    }
  )
  
  objects <- rbind(tables, views)
  objects <- unique(objects)
  objects[order(objects$table_type, objects$table_name), ]
}

get_columns <- function(object_name) {
  DBI::dbGetQuery(
    con,
    paste0(
      "SELECT column_name, data_type ",
      "FROM information_schema.columns ",
      "WHERE table_schema = 'main' ",
      "AND table_name = ", DBI::dbQuoteString(con, object_name), " ",
      "ORDER BY ordinal_position"
    )
  )
}

# ------------------------------------------------------------
# Expected Shiny export objects
# ------------------------------------------------------------

expected_tables <- c(
  "stations_minimal",
  "station_discharge_products_summary",
  "discharge_measurements",
  "discharge_measurements_summary_by_station",
  "discharge_measurements_summary_by_year",
  "rating_curve_summary",
  "rating_curves",
  "cross_sections",
  "cross_section_vertices",
  "cross_section_summary",
  "metadata",
  "data_dictionary",
  "export_row_counts"
)

expected_views <- c(
  "v_station_discharge_products_summary",
  "v_discharge_measurements_with_station",
  "v_rating_curves_with_station",
  "v_rating_curve_summary_with_station",
  "v_cross_sections_with_station",
  "v_cross_section_vertices_with_station",
  "v_cross_section_summary_with_station"
)

expected_objects <- c(expected_tables, expected_views)

export_objects <- get_export_objects()

message("============================================================")
message("Local Shiny export inspection")
message("============================================================")
message("Database: ", export_db)
message("Database size: ", export_db_size_mb, " MB")
message("Objects found:")
print(export_objects)

missing_objects <- setdiff(expected_objects, export_objects$table_name)

if (length(missing_objects) > 0) {
  stop(
    "The following expected Shiny export objects are missing: ",
    paste(missing_objects, collapse = ", ")
  )
}

message("OK: all expected Shiny export objects were found.")

# ------------------------------------------------------------
# Row counts
# ------------------------------------------------------------

export_table_counts <- data.frame(
  table_name = expected_tables,
  n_rows = as.numeric(vapply(expected_tables, count_rows, numeric(1))),
  stringsAsFactors = FALSE
)

message("============================================================")
message("Export table counts")
message("============================================================")
print(export_table_counts)

empty_tables <- export_table_counts$table_name[export_table_counts$n_rows == 0]

if (length(empty_tables) > 0) {
  stop("The following exported tables are empty: ", paste(empty_tables, collapse = ", "))
}

message("OK: all expected exported tables are non-empty.")

# ------------------------------------------------------------
# Column inspection without loading full tables
# ------------------------------------------------------------

message("============================================================")
message("Column names by expected table")
message("============================================================")

export_columns <- data.frame(
  table_name = character(),
  column_name = character(),
  data_type = character(),
  stringsAsFactors = FALSE
)

for (table_name in expected_tables) {
  table_columns <- get_columns(table_name)
  table_columns$table_name <- table_name
  table_columns <- table_columns[, c("table_name", "column_name", "data_type")]
  
  export_columns <- rbind(export_columns, table_columns)
  
  message("\n", table_name)
  print(table_columns[, c("column_name", "data_type")])
}

# Check that known heavy raw fields are not present in the vertex export.
vertex_columns <- export_columns$column_name[export_columns$table_name == "cross_section_vertices"]

forbidden_vertex_columns <- c(
  "raw_verticais",
  "observation",
  "raw_file",
  "raw_file_path",
  "first_raw_file",
  "source_file",
  "source_path",
  "local_file",
  "response_body",
  "raw_json",
  "json_response"
)

unexpected_vertex_columns <- intersect(forbidden_vertex_columns, vertex_columns)

if (length(unexpected_vertex_columns) > 0) {
  stop(
    "cross_section_vertices contains heavy/raw columns that should not be in the Shiny export: ",
    paste(unexpected_vertex_columns, collapse = ", ")
  )
}

message("OK: cross_section_vertices does not include heavy raw vertex fields.")

# Check sensitive column names across all exported tables.
sensitive_column_pattern <- "token|senha|password|cpf|cnpj|identificador|credential|secret"

sensitive_columns <- export_columns[
  grepl(sensitive_column_pattern, tolower(export_columns$column_name)),
  ,
  drop = FALSE
]

if (nrow(sensitive_columns) > 0) {
  stop(
    "Sensitive-looking column names were found in the Shiny export: ",
    paste(
      paste0(sensitive_columns$table_name, ".", sensitive_columns$column_name),
      collapse = ", "
    )
  )
}

message("OK: no sensitive-looking column names were found in exported tables.")

# ------------------------------------------------------------
# Metadata checks
# ------------------------------------------------------------

metadata_rows <- DBI::dbGetQuery(
  con,
  "SELECT key, value
   FROM metadata"
)

metadata_required_keys <- c(
  "export_name",
  "export_database_path",
  "source_database_path",
  "export_datetime",
  "security_statement",
  "api_statement",
  "raw_data_statement",
  "limitations"
)

missing_metadata_keys <- setdiff(metadata_required_keys, metadata_rows$key)

if (length(missing_metadata_keys) > 0) {
  stop(
    "The metadata table is missing required keys: ",
    paste(missing_metadata_keys, collapse = ", ")
  )
}

message("OK: metadata contains the required export/security/API statements.")

# ------------------------------------------------------------
# Session-only acquisition security check
# ------------------------------------------------------------

# The current public app intentionally supports user-initiated ANA
# API and legacy WebService downloads. Network/acquisition code is
# therefore expected. The validation below reports those patterns
# for transparency and fails only on forbidden credential storage,
# project-author credential variables, token caches, or suspicious
# persistence of sensitive authentication values.

network_patterns <- data.frame(
  pattern_name = c(
    "httr package",
    "httr2 package",
    "curl package",
    "httr GET",
    "httr POST",
    "httr2 request",
    "httr2 req_perform",
    "curl handle",
    "download.file",
    "ANA OAuth route",
    "Authorization header",
    "Bearer token",
    "ANA HidroSerie route",
    "ANA HidroInventario route",
    "ANA HidroWebService URL",
    "ANA legacy telemetria URL"
  ),
  regex = c(
    "library\\s*\\(\\s*httr\\s*\\)|require\\s*\\(\\s*httr\\s*\\)|httr::",
    "library\\s*\\(\\s*httr2\\s*\\)|require\\s*\\(\\s*httr2\\s*\\)|httr2::",
    "library\\s*\\(\\s*curl\\s*\\)|require\\s*\\(\\s*curl\\s*\\)|curl::",
    "\\bGET\\s*\\(",
    "\\bPOST\\s*\\(",
    "request\\s*\\(",
    "req_perform\\s*\\(",
    "new_handle\\s*\\(|handle_setheaders\\s*\\(",
    "download\\.file\\s*\\(",
    "OAUth",
    "Authorization",
    "Bearer",
    "HidroSerie",
    "HidroInventario",
    "hidrowebservice/EstacoesTelemetricas",
    "telemetriaws1\\.ana\\.gov\\.br"
  ),
  ignore_case = c(
    TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE,
    TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE
  ),
  stringsAsFactors = FALSE
)

forbidden_patterns <- data.frame(
  pattern_name = c(
    "project-author ANA identifier variable",
    "project-author ANA password variable",
    "local .Renviron access",
    "local ANA token cache",
    "environment write with sensitive ANA variable",
    "persistent write near authentication secrets"
  ),
  regex = c(
    "ANA_HIDRO_IDENTIFICADOR",
    "ANA_HIDRO_SENHA",
    "\\.Renviron",
    "ana_token_cache|token_cache\\.rds",
    "Sys\\.setenv\\s*\\([^\\n]*(ANA_HIDRO|SENHA|PASSWORD|TOKEN|CPF|CNPJ)",
    "(saveRDS|save\\s*\\(|writeLines|write\\.|write_csv|write_delim|dbWriteTable)[^\\n]*(senha|password|token|identificador|cpf|cnpj)|(senha|password|token|identificador|cpf|cnpj)[^\\n]*(saveRDS|save\\s*\\(|writeLines|write\\.|write_csv|write_delim|dbWriteTable)"
  ),
  ignore_case = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
  stringsAsFactors = FALSE
)

existing_app_files <- app_files[file.exists(app_files)]

if (length(existing_app_files) == 0) {
  warning("No Shiny app files were found for acquisition/security checking.")
} else {
  acquisition_rows <- data.frame(
    file = character(),
    line = integer(),
    pattern_name = character(),
    text = character(),
    stringsAsFactors = FALSE
  )

  forbidden_rows <- acquisition_rows

  for (app_file in existing_app_files) {
    file_text <- readLines(app_file, warn = FALSE)

    for (i in seq_len(nrow(network_patterns))) {
      matched_line <- grepl(
        network_patterns$regex[i],
        file_text,
        ignore.case = network_patterns$ignore_case[i],
        perl = TRUE
      )

      if (any(matched_line)) {
        acquisition_rows <- rbind(
          acquisition_rows,
          data.frame(
            file = app_file,
            line = which(matched_line),
            pattern_name = network_patterns$pattern_name[i],
            text = trimws(file_text[matched_line]),
            stringsAsFactors = FALSE
          )
        )
      }
    }

    for (i in seq_len(nrow(forbidden_patterns))) {
      matched_line <- grepl(
        forbidden_patterns$regex[i],
        file_text,
        ignore.case = forbidden_patterns$ignore_case[i],
        perl = TRUE
      )

      if (any(matched_line)) {
        forbidden_rows <- rbind(
          forbidden_rows,
          data.frame(
            file = app_file,
            line = which(matched_line),
            pattern_name = forbidden_patterns$pattern_name[i],
            text = trimws(file_text[matched_line]),
            stringsAsFactors = FALSE
          )
        )
      }
    }
  }

  message("============================================================")
  message("Session-only ANA acquisition/security check")
  message("============================================================")

  if (nrow(acquisition_rows) > 0) {
    message("INFO: expected network/acquisition patterns were found.")
    message("These are allowed because downloads are user-initiated and session-only.")
    print(acquisition_rows)
  } else {
    message("INFO: no network/acquisition patterns were found.")
  }

  if (nrow(forbidden_rows) > 0) {
    print(forbidden_rows)

    stop(
      "Forbidden credential/cache/persistence patterns were found in Shiny files. ",
      "Review the printed file/line matches above."
    )
  }

  message("OK: no forbidden credential, token-cache, or sensitive persistence patterns were found.")
}

# ------------------------------------------------------------
# Console summary
# ------------------------------------------------------------

message("============================================================")
message("Shiny export local check completed successfully.")
message("============================================================")
message("Database: ", export_db)
message("Database size: ", export_db_size_mb, " MB")
message("Run the app from the project root with: shiny::runApp()")

print(export_table_counts)
