# Publication tools

These scripts support preparation of the public repository. They are not sourced by the Shiny runtime and are excluded from the deployment manifest.

Run them from the repository root in this order:

1. `01_sync_release_data.R` — copy approved runtime data from the stable baseline and patch metadata in the copied DuckDB;
2. `04_prepare_database_parts.R` — split the complete DuckDB into validated Git-compatible binary parts and test exact reconstruction;
3. `02_generate_manifest.R` — generate `manifest.json` with the database parts and runtime files;
4. `03_validate_release.R` — validate source files, parts, forced reconstruction, DuckDB contents and deployment manifest.

The stable baseline is not modified by these scripts. The complete DuckDB in the public working copy remains local and ignored by Git after the fragmentation fallback is adopted.
