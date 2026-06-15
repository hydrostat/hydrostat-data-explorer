# Publication Batch 1 report

**Date:** 2026-06-15  
**Repository:** `hydrostat/hydrostat-data-explorer`  
**Author:** Wilson Fernandes  
**ORCID:** 0000-0002-9731-2320  
**Code license:** MIT

## Goal

Prepare a permanent public-repository copy while preserving the stable private baseline:

```text
<projects-directory>/ana_api_get_clean
```

The intended public working copy is:

```text
<projects-directory>/hydrostat-data-explorer
```

## Completed in this batch

- reorganized the supplied runtime and pipeline files into a publication-ready repository copy;
- replaced the obsolete project README with public application documentation;
- added MIT license and citation metadata;
- added data, privacy, security and contribution notices;
- added public deployment, dependency and release-checklist documentation;
- added Git LFS configuration for the publication DuckDB;
- added a restrictive `.gitignore` for private/local products;
- added `.rscignore` and an explicit manifest-generation script so the public pipeline and documentation are excluded from the runtime bundle;
- added scripts to copy approved runtime data from the baseline, patch metadata, generate `manifest.json`, and validate the release copy;
- declared `ragg` explicitly as a runtime dependency;
- confirmed the repository URL in runtime configuration;
- changed the About content from a planned repository reference to a public repository link;
- updated future pipeline metadata so the authenticated session-only ANA workflow is no longer described as undecided;
- kept the database schema and runtime data contracts unchanged.

## Runtime data not embedded in this archive

The following files must be copied from the stable baseline with `tools/01_sync_release_data.R`:

```text
exports/shiny_minimal.duckdb
exports/spatial_layers/shiny_spatial_layers.rds
```

They were not available inside the source-code ZIP used to assemble this batch.

## Manifest status

`manifest.json` is intentionally not fabricated. It must be generated on the user's R 4.6.0 publication environment after `ragg` and `rsconnect` are installed and after the runtime data files are present.

Use:

```r
source(file.path("tools", "02_generate_manifest.R"))
```

## Validation status

The preflight preceding this batch confirmed:

```text
all R files parsed                 yes
missing critical runtime packages none, except ragg not yet installed
DuckDB open time                   0.18 s
station-index load time            2.83 s
station-index size                 76.88 MiB
spatial-layer load time            0.02 s
spatial-layer size                 1.78 MiB
```

The assembled Batch 1 repository has been checked statically for required files, YAML validity, local-path leakage in runtime code, and publication structure. Full R parsing and app execution must be repeated after extraction because this assembly environment does not contain R.

## Pending before GitHub push

1. extract the repository into the permanent sibling folder;
2. install `ragg` and `rsconnect` interactively;
3. run `01_sync_release_data.R`;
4. run `03_validate_release.R`;
5. start and regress the app locally;
6. run `02_generate_manifest.R`;
7. initialize Git LFS and confirm that the DuckDB is tracked;
8. create a first commit and push to a test/public GitHub repository;
9. confirm that the hosting platform materializes the Git LFS object;
10. perform staged deployment regression tests.

## Rollback

The stable baseline remains unchanged. The public copy can be deleted and recreated from the Batch 1 archive plus the approved runtime data at any time.
