# ============================================================
# 02_generate_manifest.R
# Purpose: Generate manifest.json for the Shiny runtime only.
# Run from the hydrostat-data-explorer repository root.
# ============================================================

repository_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

if (!file.exists(file.path(repository_dir, "app.R"))) {
  stop("Run this script from the hydrostat-data-explorer repository root.", call. = FALSE)
}

required_packages <- c("digest", "ragg", "rsconnect")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    paste0(
      "Install the following packages interactively before generating the manifest: ",
      paste(missing_packages, collapse = ", ")
    ),
    call. = FALSE
  )
}

parts_dir <- file.path("exports", "database_parts")
parts_manifest_path <- file.path(parts_dir, "database_parts_manifest.csv")

if (!file.exists(parts_manifest_path)) {
  stop(
    "Missing database-parts manifest. Run tools/04_prepare_database_parts.R first.",
    call. = FALSE
  )
}

parts_manifest <- utils::read.csv(
  parts_manifest_path,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

required_manifest_columns <- c("part_order", "part_file", "part_size_bytes")
missing_manifest_columns <- setdiff(required_manifest_columns, names(parts_manifest))
if (length(missing_manifest_columns) > 0 || nrow(parts_manifest) == 0) {
  stop("The database-parts manifest is invalid.", call. = FALSE)
}

parts_manifest <- parts_manifest[order(parts_manifest$part_order), , drop = FALSE]
database_part_files <- file.path(parts_dir, parts_manifest$part_file)
missing_database_parts <- database_part_files[!file.exists(database_part_files)]

if (length(missing_database_parts) > 0) {
  stop(
    paste(
      "Missing database parts:",
      paste(missing_database_parts, collapse = "\n")
    ),
    call. = FALSE
  )
}

observed_part_sizes <- as.numeric(file.info(database_part_files)$size)
expected_part_sizes <- as.numeric(parts_manifest$part_size_bytes)
if (any(is.na(observed_part_sizes)) || any(observed_part_sizes != expected_part_sizes)) {
  stop("One or more database parts have an unexpected size.", call. = FALSE)
}

runtime_files <- c(
  "app.R",
  list.files("R", recursive = TRUE, full.names = TRUE, all.files = FALSE),
  list.files("www", recursive = TRUE, full.names = TRUE, all.files = FALSE),
  parts_manifest_path,
  database_part_files,
  file.path("exports", "spatial_layers", "shiny_spatial_layers.rds")
)

runtime_files <- unique(gsub("\\\\", "/", runtime_files))
runtime_files <- runtime_files[file.exists(runtime_files)]

required_runtime_files <- c(
  "app.R",
  parts_manifest_path,
  database_part_files,
  file.path("exports", "spatial_layers", "shiny_spatial_layers.rds")
)

missing_runtime_files <- required_runtime_files[!file.exists(required_runtime_files)]
if (length(missing_runtime_files) > 0) {
  stop(
    paste(
      "Missing required runtime files:",
      paste(missing_runtime_files, collapse = "\n")
    ),
    call. = FALSE
  )
}

rsconnect::writeManifest(
  appDir = repository_dir,
  appFiles = runtime_files,
  appPrimaryDoc = "app.R",
  appMode = "shiny",
  dependencyResolution = "library",
  verbose = TRUE,
  quiet = FALSE
)

manifest_path <- file.path(repository_dir, "manifest.json")
if (!file.exists(manifest_path)) {
  stop("manifest.json was not created.", call. = FALSE)
}

manifest <- jsonlite::fromJSON(manifest_path, simplifyVector = FALSE)
manifest_files <- names(manifest$files)

if ("exports/shiny_minimal.duckdb" %in% manifest_files) {
  stop("The complete DuckDB must not be included in manifest.json.", call. = FALSE)
}

missing_manifest_parts <- setdiff(gsub("\\\\", "/", database_part_files), manifest_files)
if (length(missing_manifest_parts) > 0) {
  stop(
    "manifest.json is missing database parts: ",
    paste(missing_manifest_parts, collapse = ", "),
    call. = FALSE
  )
}

message("manifest.json created at: ", normalizePath(manifest_path, winslash = "/"))
message("Runtime files included: ", length(runtime_files))
message("Database parts included: ", length(database_part_files))
