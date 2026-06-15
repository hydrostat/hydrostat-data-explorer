# Deployment preparation

## Target structure

The public repository is intended to live at:

```text
<projects-directory>/hydrostat-data-explorer
```

The complete private/rebuild baseline remains separate:

```text
<projects-directory>/ana_api_get_clean
```

Do not nest the public repository inside the baseline.

## 1. Synchronize approved runtime data

From the public repository root, run:

```r
source(file.path("tools", "01_sync_release_data.R"))
```

The script copies only:

- `exports/shiny_minimal.duckdb`;
- `exports/spatial_layers/shiny_spatial_layers.rds`.

It then updates publication metadata in the copied DuckDB. The baseline database is opened read-only and is not modified.

## 2. Prepare Git-compatible database parts

Posit Connect Cloud did not materialize the Git LFS object during GitHub publication. The deployed database is therefore represented by ordinary binary Git files smaller than 50 MiB.

Run:

```r
source(file.path("tools", "04_prepare_database_parts.R"))
```

The script:

- reads the complete local publication DuckDB;
- creates 40 MiB binary parts under `exports/database_parts/`;
- records size and SHA-256 for every part and for the complete database;
- reconstructs the database in an external temporary workspace;
- confirms byte size and SHA-256 identity;
- opens the reconstructed file with DuckDB in read-only mode;
- confirms station-code uniqueness.

Do not manually edit, rename, reorder or recompress the parts.

The complete `exports/shiny_minimal.duckdb` remains in the local working copy for development, but it is ignored by Git after the fallback is adopted.

## 3. Install publication-only requirements

Install packages interactively, not inside runtime scripts:

```r
install.packages(c("digest", "ragg", "rsconnect"))
```

All other runtime packages must also be available at the versions intended for publication.

## 4. Generate `manifest.json`

Run from the repository root:

```r
source(file.path("tools", "02_generate_manifest.R"))
```

The generated manifest includes:

- `app.R`;
- runtime source under `R/`;
- static resources under `www/`;
- the database-parts manifest and binary parts;
- `exports/spatial_layers/shiny_spatial_layers.rds`.

It excludes:

- the complete local DuckDB;
- `pipeline/`;
- `docs/`;
- `tools/`;
- repository-only documents.

Regenerate the manifest whenever runtime files, database parts or package versions change.

## 5. Validate the local public copy

Run:

```r
source(file.path("tools", "03_validate_release.R"))
```

The validator forces reconstruction from parts even when the complete local database is present. Then start the app in a fresh R session:

```r
shiny::runApp()
```

Complete the critical regression checklist in `RELEASE_CHECKLIST.md`.

## 6. Replace the Git LFS version in the current branch

The initial release commit stored the DuckDB through Git LFS. Keep that historical commit unchanged, but remove the complete database from the current Git tree:

```bash
git rm --cached exports/shiny_minimal.duckdb
git add .gitattributes .gitignore .rscignore
git add R/app_config.R R/app/data_01_core.R
git add tools/02_generate_manifest.R tools/03_validate_release.R tools/04_prepare_database_parts.R
git add exports/database_parts manifest.json README.md START_HERE.md docs exports/README.md tools/README.md
git status --short
```

The complete local database should remain on disk but should no longer appear as tracked or untracked because `.gitignore` excludes it.

Confirm that no current file uses LFS:

```bash
git lfs ls-files
git check-attr filter -- exports/database_parts/*.part
```

`git lfs ls-files` may still display historical information depending on Git LFS version, but the staged database parts must show `filter: unspecified` and must not be LFS pointers.

## 7. Commit and push

After reviewing the staged changes:

```bash
git commit -m "Use validated database parts for cloud deployment"
git push origin main
```

## 8. Republish on Posit Connect Cloud

Republish the existing content from:

```text
Repository: hydrostat/hydrostat-data-explorer
Branch: main
Primary file: app.R
```

At process startup, the app:

1. uses the complete DuckDB directly when it is available locally;
2. otherwise verifies the repository parts;
3. reconstructs the exact DuckDB under `tempdir()`;
4. validates size and SHA-256;
5. opens the reconstructed file read-only;
6. reuses that file for later sessions in the same R process.

## 9. Staged deployment checks

Confirm that:

- the application starts without a DuckDB format error;
- startup time remains acceptable after reconstruction;
- DuckDB opens read-only;
- the serialized spatial layer loads;
- external map tiles load;
- ANA endpoints are reachable;
- memory remains acceptable with multiple sessions;
- no session token or downloaded series persists after session termination.

Do not announce the public release until the full checklist passes.

## Rollback

The private baseline remains unchanged. To roll back the public copy:

1. stop the staged deployment;
2. reset the Git branch to commit `ca47d74` or another validated commit;
3. retain or restore the complete local DuckDB from the baseline;
4. regenerate the appropriate `manifest.json` for the selected commit;
5. redeploy and repeat regression tests.
