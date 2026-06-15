# Rollback instructions

## Principle

`ana_api_get_clean` remains the stable source and is never edited by publication preparation scripts.

## Before GitHub initialization

Delete the incomplete public folder and extract the latest validated Batch 1 archive again.

## After Git initialization

Create a commit after each validated publication batch. To roll back:

```bash
git status
git log --oneline
git reset --hard <validated-commit>
git lfs pull
```

Do not use `reset --hard` when uncommitted work must be preserved. Create a branch or copy first.

## Runtime data restoration

Rerun:

```r
source(file.path("tools", "01_sync_release_data.R"))
```

The script copies fresh runtime products from the stable baseline and backs up existing public-copy data outside the repository before overwriting.

After restoring data, regenerate `manifest.json` and repeat validation.
