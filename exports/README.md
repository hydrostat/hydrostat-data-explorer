# Runtime exports

This directory contains the publication data products required by the Shiny application.

Repository/deployment products:

```text
database_parts/database_parts_manifest.csv
database_parts/shiny_minimal.duckdb.part001 ...
spatial_layers/shiny_spatial_layers.rds
```

The complete local file `shiny_minimal.duckdb` is used for development and for generating the validated parts, but it is ignored by Git. During cloud startup, the application verifies the parts, reconstructs the exact database in temporary storage and opens it read-only.

Do not place raw downloads, complete daily time series, private logs, credentials, token caches or local analytical databases in this directory.
