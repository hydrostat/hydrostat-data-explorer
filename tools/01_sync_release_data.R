# ============================================================
# 01_sync_release_data.R
# Purpose: Copy approved runtime data from the stable baseline
#          and update publication metadata in the copied database.
# Run from the hydrostat-data-explorer repository root.
# ============================================================

repository_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

default_baseline_dir <- file.path(dirname(repository_dir), "ana_api_get_clean")
baseline_dir <- Sys.getenv(
  "HYDROSTAT_BASELINE_DIR",
  unset = default_baseline_dir
)

baseline_dir <- normalizePath(baseline_dir, winslash = "/", mustWork = TRUE)

if (!file.exists(file.path(repository_dir, "app.R"))) {
  stop("Run this script from the hydrostat-data-explorer repository root.", call. = FALSE)
}

source_db <- file.path(baseline_dir, "exports", "shiny_minimal.duckdb")
source_spatial <- file.path(
  baseline_dir,
  "exports",
  "spatial_layers",
  "shiny_spatial_layers.rds"
)

target_db <- file.path(repository_dir, "exports", "shiny_minimal.duckdb")
target_spatial <- file.path(
  repository_dir,
  "exports",
  "spatial_layers",
  "shiny_spatial_layers.rds"
)

required_source_files <- c(source_db, source_spatial)
missing_source_files <- required_source_files[!file.exists(required_source_files)]

if (length(missing_source_files) > 0) {
  stop(
    paste(
      "Missing required baseline files:",
      paste(missing_source_files, collapse = "\n")
    ),
    call. = FALSE
  )
}

dir.create(dirname(target_db), recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(target_spatial), recursive = TRUE, showWarnings = FALSE)

# Preserve any previous public-copy data outside the repository.
existing_targets <- c(target_db, target_spatial)
existing_targets <- existing_targets[file.exists(existing_targets)]

if (length(existing_targets) > 0) {
  backup_root <- file.path(
    dirname(repository_dir),
    "hydrostat-data-explorer_backups",
    format(Sys.time(), "%Y%m%d_%H%M%S")
  )
  dir.create(backup_root, recursive = TRUE, showWarnings = FALSE)

  for (path in existing_targets) {
    relative_path <- substring(
      normalizePath(path, winslash = "/", mustWork = TRUE),
      nchar(repository_dir) + 2
    )
    backup_path <- file.path(backup_root, relative_path)
    dir.create(dirname(backup_path), recursive = TRUE, showWarnings = FALSE)

    copied <- file.copy(path, backup_path, overwrite = FALSE, copy.date = TRUE)
    if (!copied) {
      stop("Could not create backup for: ", path, call. = FALSE)
    }
  }

  message("Previous public-copy data backed up at: ", backup_root)
}

copy_checked <- function(from, to) {
  copied <- file.copy(from, to, overwrite = TRUE, copy.date = TRUE)
  if (!copied || !file.exists(to)) {
    stop("Failed to copy: ", from, call. = FALSE)
  }

  source_size <- file.info(from)$size
  target_size <- file.info(to)$size

  if (!identical(as.numeric(source_size), as.numeric(target_size))) {
    stop("Copied file size differs from source: ", to, call. = FALSE)
  }
}

copy_checked(source_db, target_db)
copy_checked(source_spatial, target_spatial)

if (!requireNamespace("DBI", quietly = TRUE) ||
    !requireNamespace("duckdb", quietly = TRUE)) {
  stop("Packages DBI and duckdb are required to patch publication metadata.", call. = FALSE)
}

con <- NULL
tryCatch(
  {
    con <- DBI::dbConnect(duckdb::duckdb(), dbdir = target_db, read_only = FALSE)

    if (!DBI::dbExistsTable(con, "metadata")) {
      stop("The copied publication database has no metadata table.", call. = FALSE)
    }

    metadata_columns <- DBI::dbListFields(con, "metadata")
    if (!all(c("key", "value") %in% metadata_columns)) {
      stop("The metadata table does not contain key/value columns.", call. = FALSE)
    }

    current_metadata <- DBI::dbGetQuery(
      con,
      "SELECT CAST(key AS VARCHAR) AS key, CAST(value AS VARCHAR) AS value FROM metadata"
    )

    remove_keys <- c(
      "source_metadata.open_decision",
      "source_metadata.shiny_authenticated_download_model",
      "api_statement",
      "privacy_statement"
    )

    current_metadata <- current_metadata[!current_metadata$key %in% remove_keys, , drop = FALSE]

    publication_metadata <- data.frame(
      key = c(
        "source_metadata.shiny_authenticated_download_model",
        "api_statement",
        "privacy_statement"
      ),
      value = c(
        paste(
          "Authenticated ANA API downloads are supported using credentials supplied by the user only for the active Shiny session.",
          "Credentials are cleared after authentication; tokens, downloaded series, partial data, and reports are not persisted by the application."
        ),
        paste(
          "The bundled export contains no ANA credentials, CPF/CNPJ, passwords, or authentication tokens.",
          "The Shiny application may perform user-initiated authenticated ANA API requests using session-only credentials and token state."
        ),
        paste(
          "Uploaded and downloaded daily series, partial download state, authentication tokens, and derived session analyses remain in memory for the active session",
          "and are not written by the application to DuckDB, project files, or persistent caches."
        )
      ),
      stringsAsFactors = FALSE
    )

    updated_metadata <- rbind(current_metadata, publication_metadata)
    DBI::dbWriteTable(con, "metadata", updated_metadata, overwrite = TRUE)
  },
  finally = {
    if (!is.null(con)) {
      DBI::dbDisconnect(con, shutdown = TRUE)
      con <- NULL
    }
  }
)

# Confirm that the copied database can be reopened read-only.
check_con <- NULL
metadata_check <- NULL
tryCatch(
  {
    check_con <- DBI::dbConnect(duckdb::duckdb(), dbdir = target_db, read_only = TRUE)
    metadata_check <- DBI::dbGetQuery(
      check_con,
      paste0(
        "SELECT key, value FROM metadata WHERE key IN (",
        paste(sprintf("'%s'", publication_metadata$key), collapse = ", "),
        ") ORDER BY key"
      )
    )
  },
  finally = {
    if (!is.null(check_con)) {
      DBI::dbDisconnect(check_con, shutdown = TRUE)
      check_con <- NULL
    }
  }
)

if (nrow(metadata_check) != nrow(publication_metadata)) {
  stop("Publication metadata patch could not be verified.", call. = FALSE)
}

message("Runtime data copied and publication metadata updated successfully.")
message("Database: ", normalizePath(target_db, winslash = "/", mustWork = TRUE))
message("Spatial layer: ", normalizePath(target_spatial, winslash = "/", mustWork = TRUE))
