# Batch 2D report — DuckDB fragmentation fallback

## Trigger

The first Posit Connect Cloud deployment received the Git LFS pointer for `exports/shiny_minimal.duckdb` instead of the 149,172,224-byte database. DuckDB therefore reported that the deployed file existed but was not a valid database.

Enabling GitHub's “Include Git LFS objects in archives” setting did not change the Connect Cloud result.

## Decision

The current Git tree stores the publication database as ordinary binary parts smaller than 50 MiB. The complete database remains unchanged locally and in the stable private baseline.

## Runtime behavior

- Local development continues to use `exports/shiny_minimal.duckdb` when the complete file is available.
- Cloud deployment verifies all parts by size and SHA-256.
- The exact database is reconstructed once under `tempdir()` for each R process.
- The reconstructed database is validated by complete size and SHA-256.
- DuckDB opens the reconstructed file read-only.
- Later sessions in the same process reuse the validated temporary file.

## Files changed

```text
.gitattributes
.gitignore
.rscignore
R/app_config.R
R/app/data_01_core.R
tools/02_generate_manifest.R
tools/03_validate_release.R
tools/04_prepare_database_parts.R
README.md
START_HERE.md
exports/README.md
exports/database_parts/README.md
tools/README.md
docs/DEPENDENCIES.md
docs/DEPLOYMENT.md
docs/RELEASE_CHECKLIST.md
docs/publication/PUBLICATION_MATRIX.md
docs/publication/RUNTIME_FILE_LIST.txt
```

## Files generated locally after applying the batch

```text
exports/database_parts/database_parts_manifest.csv
exports/database_parts/shiny_minimal.duckdb.part001 ... partNNN
manifest.json
```

## Preservation

No schema, table, view, reactive identifier, analytical method or session-data rule was changed. The private baseline `ana_api_get_clean` remains untouched.
