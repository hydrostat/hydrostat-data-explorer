# Start here — Publication Batch 1

This archive is the prepared source for the permanent public repository.

## 1. Extract

Extract the folder as a sibling of the stable baseline:

```text
<projects-directory>/
├── ana_api_get_clean/
└── hydrostat-data-explorer/
```

Rename the extracted folder to `hydrostat-data-explorer` when necessary.

## 2. Open the public project

Open:

```text
hydrostat-data-explorer/hydrostat-data-explorer.Rproj
```

Set the working directory to the repository root.

## 3. Install the two missing publication packages

Run interactively:

```r
install.packages(c("ragg", "rsconnect"))
```

Do not place installation commands inside application scripts.

## 4. Copy approved runtime data and validate

```r
source(file.path("tools", "01_sync_release_data.R"))
source(file.path("tools", "03_validate_release.R"))
shiny::runApp()
```

The synchronization script reads from the sibling `ana_api_get_clean` folder by default. Set `HYDROSTAT_BASELINE_DIR` only when the baseline is elsewhere.

## 5. Generate the deployment manifest

After the app passes local regression:

```r
source(file.path("tools", "02_generate_manifest.R"))
```

Then follow `docs/DEPLOYMENT.md` for Git LFS, GitHub and staged Posit Connect Cloud deployment.

Do not push to GitHub before confirming that `exports/shiny_minimal.duckdb` appears in `git lfs ls-files`.
