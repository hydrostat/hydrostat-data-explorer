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

## 1. Copy the prepared repository

Extract the Batch 1 archive into the `Projects` directory so that `app.R` is located at the public repository root.

## 2. Copy and patch runtime data

From the public repository root, run:

```r
source(file.path("tools", "01_sync_release_data.R"))
```

The script copies only:

- `exports/shiny_minimal.duckdb`;
- `exports/spatial_layers/shiny_spatial_layers.rds`.

It then updates publication metadata in the copied DuckDB. The baseline database is opened read-only and is not modified.

## 3. Install publication-only requirements

Install packages interactively, not inside runtime scripts:

```r
install.packages(c("ragg", "rsconnect"))
```

All other runtime packages must also be available at the versions intended for publication.

## 4. Validate the local public copy

Run:

```r
source(file.path("tools", "03_validate_release.R"))
```

Then start the app in a fresh R session:

```r
shiny::runApp()
```

Complete the critical regression checklist in `RELEASE_CHECKLIST.md`.

## 5. Generate `manifest.json`

Run from the repository root:

```r
source(file.path("tools", "02_generate_manifest.R"))
```

The script defines the runtime file list explicitly. `pipeline/`, `docs/`, and `tools/` remain in the GitHub repository but are excluded from the application bundle.

Regenerate the manifest whenever runtime package versions or runtime source files change materially.

## 6. Initialize Git and Git LFS

Git LFS must be installed before adding the DuckDB.

```bash
git init
git lfs install
git lfs track "exports/shiny_minimal.duckdb"
git add .gitattributes
git add .
git commit -m "Prepare HydroStat Data Explorer public release"
git branch -M main
git remote add origin https://github.com/hydrostat/hydrostat-data-explorer.git
git push -u origin main
```

Verify before pushing:

```bash
git lfs ls-files
git status
```

The DuckDB must appear in `git lfs ls-files`. If it does not, do not push.

## 7. Staged Posit Connect Cloud deployment

Publish a test deployment first. Confirm that:

- the Git LFS object is materialized rather than left as a pointer file;
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
2. reset the Git branch to the last validated commit;
3. recopy the runtime database and spatial RDS from the baseline when required;
4. regenerate `manifest.json`;
5. redeploy and repeat regression tests.
