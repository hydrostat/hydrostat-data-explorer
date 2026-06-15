# Publication tools

These scripts support preparation of the public repository. They are not sourced by the Shiny runtime and are excluded from the deployment manifest.

Run them from the repository root in this order:

1. `01_sync_release_data.R` — copy approved runtime data from the stable baseline and patch metadata in the copied DuckDB;
2. `03_validate_release.R` — validate the public copy;
3. `02_generate_manifest.R` — generate `manifest.json` after all runtime packages are installed.

The stable baseline is not modified by these scripts.
