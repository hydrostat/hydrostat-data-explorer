# Pipeline helpers

This folder contains final shared helpers used only by the numbered
data-production pipeline. The Shiny runtime does not source these files.

## Files

- `ana_parse_helpers.R`: parsing, station-code, datetime, path, and ID helpers shared by scripts 050-052.
- `api_download_helpers.R`: simple progress, response, and token-retry utilities shared by scripts 042/042-retry/044.
- `duckdb_helpers.R`: SQL identifier quoting shared by scripts 060/061/063/068.

Route-specific request construction, raw-file naming, request-log schemas,
retry orchestration, and product-specific file resolution remain in their
original scripts because their behavior is not identical.

Run numbered scripts from the project root.
