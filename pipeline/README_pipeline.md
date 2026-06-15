# Data-production pipeline

The numbered scripts in `pipeline/R/` rebuild the local ANA data products and the compact Shiny publication export.

The pipeline is included for transparency and reproducibility, but it is not part of the deployed Shiny runtime.

## Important boundaries

- Run scripts from the repository root.
- Review paths and parameters before execution.
- Local credentials must remain in `.Renviron` and must never be committed.
- Raw downloads, logs, local databases and processed products are excluded from the public repository.
- Acquisition scripts may require an ANA account and are subject to ANA access rules.
- Do not run the complete pipeline against the publication DuckDB in place.

## Main sequence

```text
000 setup
010 authentication
020–023 station inventories
030 station database
042–044 discharge/rating-curve/cross-section acquisition
050–052 processing
053–054 local DuckDB products
060–063 Shiny export and station diagnostics
067 spatial publication layer
068 Portuguese display/enrichment layer
```

Not every script should be rerun for a routine application deployment. Rebuild operations must use the private baseline inputs and must be validated before replacing public products.

## Shared helpers

Shared functions that were confirmed to be equivalent are stored in `pipeline/helpers/`. Product-specific and route-specific logic remains inside the numbered scripts.

## Runtime separation

The Shiny runtime is located in:

- `app.R`;
- `R/app_config.R`;
- `R/app_data.R`;
- `R/app_ui.R`;
- `R/app_server.R`;
- `R/station_diagnostic_functions.R`;
- `R/app/`;
- `www/`.

The runtime does not source files from `pipeline/`.

## Reproducibility limits

The repository does not include the complete raw and processed input archive. Reproducing the final database requires the appropriate source data, ANA access where applicable, local storage, and the pipeline package/system dependencies documented in `docs/DEPENDENCIES.md`.
