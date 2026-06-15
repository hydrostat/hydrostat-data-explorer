# Rollback instructions

## Principle

`ana_api_get_clean` remains the stable source and is never edited by publication preparation scripts.

## Before GitHub initialization

Delete the incomplete public folder and extract the latest validated publication archive again.

## After Git initialization

Create a commit after each validated publication batch. To roll back:

```bash
git status
git log --oneline
git reset --hard <validated-commit>
```

Do not use `reset --hard` when uncommitted work must be preserved. Create a branch or copy first.

The initial commit `ca47d74` used Git LFS for the complete DuckDB. Later commits use ordinary binary parts. Resetting to the initial commit may therefore require `git lfs pull`, while resetting to the fragmentation fallback does not.

## Runtime data restoration

Rerun:

```r
source(file.path("tools", "01_sync_release_data.R"))
source(file.path("tools", "04_prepare_database_parts.R"))
source(file.path("tools", "02_generate_manifest.R"))
source(file.path("tools", "03_validate_release.R"))
```

The synchronization script copies fresh runtime products from the stable baseline and backs up existing public-copy data outside the repository before overwriting. The fragmentation script validates exact reconstruction before replacing the repository parts.
