# Publication database parts

The deployed DuckDB is stored in this directory as validated binary parts smaller than 50 MiB.

Generate or refresh the parts from the local complete database with:

```r
source(file.path("tools", "04_prepare_database_parts.R"))
```

The runtime verifies each part, reconstructs the exact DuckDB in `tempdir()`, validates its size and SHA-256 hash, and opens the reconstructed file in read-only mode. The complete `exports/shiny_minimal.duckdb` remains a local development product and is not tracked in the current Git tree.

Do not edit, recompress, rename or reorder the part files manually.
