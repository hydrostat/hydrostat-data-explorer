# Publication database schema

## Database

The public application reads:

```text
exports/shiny_minimal.duckdb
```

The file is opened read-only by the Shiny runtime.

## Integration key

`station_code` is the public integration key across station-level tables and views.

## Main publication products

The database contains 24 physical tables and 7 views in the current release. The main product groups are:

### Station catalog and availability

- `stations_minimal`;
- `station_discharge_products_summary`;
- `station_product_availability`;
- `station_assessment_summary`;
- `station_data_availability`;
- `station_map_status`.

### Discharge measurements

- `discharge_measurements`;
- `discharge_measurements_summary_by_station`;
- `discharge_measurements_summary_by_year`.

These are point measurements, not a complete continuous daily discharge series.

### Rating curves

- `rating_curve_summary`;
- `rating_curves`.

### Cross sections

- `cross_sections`;
- `cross_section_vertices`;
- `cross_section_summary`.

`cross_section_id` links section records to their ordered vertices.

### Diagnostics and display support

- station-level measurement, rating-curve, cross-section, quality and diagnostic indices;
- metadata;
- data dictionaries and Portuguese labels;
- export row counts.

## Views

Views provide station-enriched access to the main product tables. Runtime queries filter at the database level by `station_code` or product identifiers.

## Session-only data

The publication database does not store complete daily discharge, stage or rainfall series loaded by users. It also does not store:

- ANA credentials or tokens;
- partial API downloads;
- session reports;
- session analytical products;
- raw API responses.

## Metadata

The release metadata includes security, API and privacy statements. The implemented authenticated download model is recorded under:

```text
source_metadata.shiny_authenticated_download_model
api_statement
privacy_statement
```

## Current-release stability rule

The current flat contracts, including `cross_section_vertices`, are preserved for the first public release. Schema normalization and alternate backends are deferred to a future major version and require complete regression testing.
