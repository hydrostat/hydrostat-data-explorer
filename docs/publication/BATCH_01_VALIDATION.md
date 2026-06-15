# Batch 1 static validation

The assembled publication copy passed the following checks in the artifact environment:

- required repository files present;
- citation and issue-template YAML parsed successfully;
- local Markdown links resolved;
- no user-specific local path or username found;
- no embedded credential or token value detected by the publication scan;
- all 51 R files passed a balanced delimiter/string scan;
- original runtime files had already passed the R preflight parser before this batch;
- Git ignore rules exclude local data, logs, configuration, archives, diagnostics and outputs;
- the publication DuckDB path is not ignored and has the Git LFS filter attribute;
- the spatial runtime RDS path is not ignored;
- the current archive intentionally contains neither the DuckDB, spatial RDS nor `manifest.json`;
- `ragg` and `sf` are explicit runtime dependencies;
- future pipeline metadata describes the implemented session-only API model.

Full R parsing and application execution must be repeated on the user's R 4.6.0 environment after runtime data are synchronized and `ragg` is installed.
