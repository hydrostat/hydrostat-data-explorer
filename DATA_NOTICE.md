# Data notice

## Scope of the MIT license

The MIT license in this repository applies to the original source code and project documentation created for HydroStat Data Explorer, except where another attribution or license is explicitly indicated.

It does not automatically apply to:

- source data supplied by the Agência Nacional de Águas e Saneamento Básico (ANA);
- products derived from ANA data;
- third-party map tiles, spatial data, software, trademarks or documentation;
- files uploaded by users;
- data downloaded by users during an application session.

## Data source

The application uses hydrological information originating mainly from ANA HidroWebService and related ANA services.

Users should consult the original source, metadata, terms and quality information before reusing or interpreting any data product.

## Bundled publication database

`exports/shiny_minimal.duckdb` contains selected station metadata and derived products required by the public application. It is not a complete archive of ANA data and does not contain complete daily discharge, stage or rainfall time series.

The database may include:

- station metadata and availability summaries;
- discharge measurements;
- rating curves;
- cross sections and vertices;
- derived station-level indicators;
- Portuguese display dictionaries and metadata.

## No official endorsement

HydroStat Data Explorer is an independent project. It is not an official ANA application and does not imply ANA endorsement, certification or validation.
