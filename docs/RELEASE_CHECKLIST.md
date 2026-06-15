# Public release checklist

## Repository and security

- [ ] Public repository path is separate from `ana_api_get_clean`.
- [ ] `.Renviron`, credentials, tokens and token caches are absent.
- [ ] `data/`, logs, raw downloads, backups and audit workspaces are absent.
- [ ] `exports/shiny_minimal.duckdb` is tracked through Git LFS.
- [ ] `manifest.json` is in the same directory as `app.R`.
- [ ] Repository URL and author metadata are correct.
- [ ] README, license, citation, privacy, security and data notice are present.

## Local startup

- [ ] All R files parse.
- [ ] All runtime packages are installed.
- [ ] App starts in a fresh R session.
- [ ] DuckDB opens read-only.
- [ ] Station index loads without duplicate station codes.
- [ ] Spatial RDS loads.
- [ ] No project-relative path resolves outside the repository.

## Core application

- [ ] Main map renders.
- [ ] Station search and selection work.
- [ ] Map click updates the selected station.
- [ ] Station-type and spatial-layer controls work independently.
- [ ] Selected-station sidebar and CSV download work.
- [ ] Discharge measurements load and export correctly.
- [ ] Rating curves, tables and diagnostics render.
- [ ] Cross-section selector, plots, tables and downloads work.
- [ ] Empty/no-data stations show controlled messages.

## Fluviometric workflows

- [ ] Upload workflows work.
- [ ] Legacy public download works where available.
- [ ] Authenticated ANA download works over HTTPS.
- [ ] Discharge and paired stage data remain session-only.
- [ ] Consistency, statistics and extreme-event tabs work.
- [ ] Token expiration/resume behavior works where practical.
- [ ] Changing station after loading data requires confirmation and clears data only after approval.

## Pluviometric workflows

- [ ] Upload workflows work.
- [ ] Legacy public download works where available.
- [ ] Authenticated ANA rainfall download works.
- [ ] Rainfall data and derived outputs remain session-only.
- [ ] Series, consistency, statistics and annual maxima work.

## Deployment behavior

- [ ] HTTPS is active.
- [ ] Git LFS database is fully available in the deployed filesystem.
- [ ] Cartographic tiles and ANA endpoints are reachable.
- [ ] Startup and selected-station query times remain acceptable.
- [ ] Two or more independent sessions do not share session data or token state.
- [ ] Memory use is acceptable for the hosting tier.
- [ ] CSV files open with expected UTF-8/Excel behavior.
- [ ] Error and timeout messages do not expose secrets.
- [ ] Session end removes in-memory token and user data.

## Release

- [ ] Staged deployment passed all critical tests.
- [ ] Public URL was added to GitHub repository metadata.
- [ ] Release tag and release notes were created.
- [ ] Stable baseline and rollback instructions were retained.
