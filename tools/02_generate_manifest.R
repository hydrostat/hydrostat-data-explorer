# ============================================================
# 02_generate_manifest.R
# Purpose: Generate manifest.json for the Shiny runtime only.
# Run from the hydrostat-data-explorer repository root.
# ============================================================

repository_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

if (!file.exists(file.path(repository_dir, "app.R"))) {
  stop("Run this script from the hydrostat-data-explorer repository root.", call. = FALSE)
}

required_packages <- c("ragg", "rsconnect")
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

runtime_files <- c(
  "app.R",
  list.files("R", recursive = TRUE, full.names = TRUE, all.files = FALSE),
  list.files("www", recursive = TRUE, full.names = TRUE, all.files = FALSE),
  file.path("exports", "shiny_minimal.duckdb"),
  file.path("exports", "spatial_layers", "shiny_spatial_layers.rds")
)

runtime_files <- unique(gsub("\\\\", "/", runtime_files))
runtime_files <- runtime_files[file.exists(runtime_files)]

required_runtime_files <- c(
  "app.R",
  file.path("exports", "shiny_minimal.duckdb"),
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

message("manifest.json created at: ", normalizePath(manifest_path, winslash = "/"))
message("Runtime files included: ", length(runtime_files))
