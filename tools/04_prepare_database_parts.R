# ============================================================
# 04_prepare_database_parts.R
# Purpose: Split the publication DuckDB into Git-compatible
#          binary parts and validate exact reconstruction.
# Run from the hydrostat-data-explorer repository root.
# ============================================================

repository_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

if (!file.exists(file.path(repository_dir, "app.R"))) {
  stop("Run this script from the hydrostat-data-explorer repository root.", call. = FALSE)
}

required_packages <- c("DBI", "duckdb", "digest")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Install the following packages interactively before preparing database parts: ",
    paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}

source_db <- file.path(repository_dir, "exports", "shiny_minimal.duckdb")
parts_dir <- file.path(repository_dir, "exports", "database_parts")
part_size_bytes <- 40 * 1024^2

is_lfs_pointer <- function(path) {
  if (!file.exists(path) || is.na(file.info(path)$size) || file.info(path)$size > 4096) {
    return(FALSE)
  }

  first_line <- tryCatch(
    readLines(path, n = 1, warn = FALSE, encoding = "UTF-8"),
    error = function(e) character()
  )

  length(first_line) == 1 &&
    identical(first_line, "version https://git-lfs.github.com/spec/v1")
}

sha256_file <- function(path) {
  digest::digest(
    object = path,
    algo = "sha256",
    file = TRUE,
    serialize = FALSE
  )
}

assemble_parts <- function(part_paths, output_path) {
  output_connection <- file(output_path, open = "wb")
  on.exit(try(close(output_connection), silent = TRUE), add = TRUE)
  buffer_size <- 8 * 1024^2

  for (part_path in part_paths) {
    input_connection <- file(part_path, open = "rb")
    on.exit(try(close(input_connection), silent = TRUE), add = TRUE)

    repeat {
      buffer <- readBin(input_connection, what = "raw", n = buffer_size)
      if (length(buffer) == 0) {
        break
      }
      writeBin(buffer, output_connection)
    }

    close(input_connection)
  }

  close(output_connection)
  invisible(output_path)
}

if (!file.exists(source_db)) {
  stop("Missing local publication database: ", source_db, call. = FALSE)
}

if (is_lfs_pointer(source_db)) {
  stop(
    "The local publication database is a Git LFS pointer. Restore the full DuckDB before splitting it.",
    call. = FALSE
  )
}

source_size <- as.numeric(file.info(source_db)$size)
if (is.na(source_size) || source_size <= 1024^2) {
  stop("The local publication database has an unexpected size.", call. = FALSE)
}

source_sha256 <- sha256_file(source_db)
staging_dir <- tempfile("hydrostat_database_parts_")
dir.create(staging_dir, recursive = TRUE, showWarnings = FALSE)
on.exit(unlink(staging_dir, recursive = TRUE, force = TRUE), add = TRUE)

input_connection <- file(source_db, open = "rb")
on.exit(try(close(input_connection), silent = TRUE), add = TRUE)

part_records <- list()
part_order <- 0L

repeat {
  buffer <- readBin(input_connection, what = "raw", n = part_size_bytes)
  if (length(buffer) == 0) {
    break
  }

  part_order <- part_order + 1L
  part_file <- sprintf("shiny_minimal.duckdb.part%03d", part_order)
  part_path <- file.path(staging_dir, part_file)
  writeBin(buffer, part_path)

  part_records[[part_order]] <- data.frame(
    part_order = part_order,
    part_file = part_file,
    part_size_bytes = as.numeric(file.info(part_path)$size),
    part_sha256 = sha256_file(part_path),
    database_size_bytes = source_size,
    database_sha256 = source_sha256,
    stringsAsFactors = FALSE
  )
}

close(input_connection)

if (length(part_records) < 2) {
  stop("The publication database was not divided into multiple parts.", call. = FALSE)
}

parts_manifest <- do.call(rbind, part_records)
part_paths <- file.path(staging_dir, parts_manifest$part_file)

if (any(parts_manifest$part_size_bytes > 50 * 1024^2)) {
  stop("At least one database part exceeds the 50 MiB publication target.", call. = FALSE)
}

reconstructed_db <- file.path(staging_dir, "shiny_minimal_reconstructed.duckdb")
assemble_parts(part_paths, reconstructed_db)

reconstructed_size <- as.numeric(file.info(reconstructed_db)$size)
reconstructed_sha256 <- sha256_file(reconstructed_db)

if (!identical(reconstructed_size, source_size)) {
  stop("Reconstructed database size differs from the source database.", call. = FALSE)
}

if (!identical(tolower(reconstructed_sha256), tolower(source_sha256))) {
  stop("Reconstructed database SHA-256 differs from the source database.", call. = FALSE)
}

con <- NULL
station_check <- NULL
tryCatch(
  {
    con <- DBI::dbConnect(
      duckdb::duckdb(),
      dbdir = reconstructed_db,
      read_only = TRUE
    )

    station_check <- DBI::dbGetQuery(
      con,
      paste(
        "SELECT COUNT(*) AS n_rows,",
        "COUNT(DISTINCT CAST(station_code AS VARCHAR)) AS n_unique",
        "FROM stations_minimal"
      )
    )

    if (station_check$n_rows[[1]] != station_check$n_unique[[1]]) {
      stop("Duplicate station codes were found after database reconstruction.", call. = FALSE)
    }
  },
  finally = {
    if (!is.null(con)) {
      DBI::dbDisconnect(con, shutdown = TRUE)
      con <- NULL
    }
  }
)

dir.create(parts_dir, recursive = TRUE, showWarnings = FALSE)
old_parts <- list.files(
  parts_dir,
  pattern = "^shiny_minimal\\.duckdb\\.part[0-9]{3}$",
  full.names = TRUE
)
unlink(old_parts, force = TRUE)
unlink(file.path(parts_dir, "database_parts_manifest.csv"), force = TRUE)

copy_ok <- file.copy(part_paths, parts_dir, overwrite = TRUE)
if (!all(copy_ok)) {
  stop("Could not copy all validated database parts into the repository.", call. = FALSE)
}

utils::write.csv(
  parts_manifest,
  file.path(parts_dir, "database_parts_manifest.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

message("Database parts created and validated.")
message("Source bytes: ", format(source_size, scientific = FALSE))
message("Source SHA-256: ", source_sha256)
message("Parts: ", nrow(parts_manifest))
message("Largest part MiB: ", round(max(parts_manifest$part_size_bytes) / 1024^2, 2))
message("Stations: ", station_check$n_rows[[1]])
message("Output: ", normalizePath(parts_dir, winslash = "/", mustWork = TRUE))
