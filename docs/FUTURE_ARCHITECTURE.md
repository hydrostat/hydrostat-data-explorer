# Deferred future backend architecture

## Current decision

The first public release preserves the current DuckDB schema and runtime data-access contracts.

Tests of compact rebuilds, removal of unused objects, normalized cross-section vertices, normalized diagnostic dictionaries and nested list structures did not provide enough combined benefit in package size, query speed, simplicity and maintenance to justify migration before publication.

## Candidate major-version direction

A future major version may evaluate a publication-specific data mart with:

- one compact `station_catalog` loaded at startup;
- explicit station-filtered query helpers;
- narrower public fact tables;
- one canonical display dictionary;
- optional Parquet fact datasets accessed through DuckDB.

## Expected benefits

The strongest possible gains are expected in:

- startup time;
- per-process and per-session memory;
- selective station queries;
- clearer data contracts;
- simpler reactive invalidation.

Large reductions in compressed file size are not guaranteed because DuckDB already compresses repeated metadata efficiently.

## Required migration process

Any redesign must be treated as a separate major version with:

1. instrumentation of the current release;
2. a formal public data contract;
3. an external prototype database;
4. dual-run value comparison;
5. complete functional regression;
6. performance and memory benchmarks;
7. documented rollback.

The current release remains the regression baseline throughout that work.
