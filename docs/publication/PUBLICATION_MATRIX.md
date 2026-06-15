# Publication inclusion and exclusion matrix

| Item | Public repository | Deployment bundle | Notes |
|---|---:|---:|---|
| `app.R` | Include | Include | Shiny entry point |
| `R/` | Include | Include | Runtime source blocks and diagnostic helpers |
| `www/` | Include | Include | CSS and static resources |
| `exports/shiny_minimal.duckdb` | Include with Git LFS | Include | Read-only publication database |
| `exports/spatial_layers/shiny_spatial_layers.rds` | Include | Include | Simplified runtime spatial object |
| `manifest.json` | Include after generation | Required | Generated from validated R environment |
| `pipeline/` | Include | Exclude | Public rebuild pipeline; requires local inputs |
| `docs/` | Include | Exclude | Sanitized public documentation only |
| `tools/` | Include | Exclude | Publication preparation and validation |
| `README.md`, `LICENSE`, `CITATION.cff` | Include | Exclude | Repository documentation and metadata |
| `PRIVACY.md`, `SECURITY.md`, `DATA_NOTICE.md` | Include | Exclude | Public governance and attribution |
| `.github/` | Include | Exclude | Repository metadata and issue template |
| `data/` | Exclude | Exclude | Raw/processed/private local products |
| local analytical DuckDB | Exclude | Exclude | Rebuild database, not the publication database |
| `logs/`, `outputs/`, token caches | Exclude | Exclude | Local/private/transient |
| `.Renviron`, `.Rhistory`, `.RData` | Exclude | Exclude | Credentials and local R state |
| archives, ZIPs, backups, audits | Exclude | Exclude | Development and rollback artifacts |
| internal chat/context documentation | Exclude | Exclude | Retained only in the private baseline |
