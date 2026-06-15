# Data sources, scope and limitations

## Primary source

HydroStat Data Explorer uses hydrological information originating mainly from the Agência Nacional de Águas e Saneamento Básico (ANA), including HidroWebService and related ANA webservices.

The application is independent and is not an official ANA product.

## Bundled products

The publication database supports:

- station search and mapping;
- station metadata and data availability;
- discharge-measurement products;
- rating curves;
- cross sections;
- station-level diagnostic and display summaries.

The database is a selected publication product, not a complete replica of ANA systems.

## Session-only series

Complete daily discharge, stage and rainfall series are not bundled in the publication database. They may be supplied by the user or obtained during the active session through supported workflows.

Session series and derived results are discarded when the session ends unless the user explicitly downloads an output.

## Diagnostic interpretation

Indicators, flags and attention classes are screening tools designed to support visual inspection. They are not:

- official ANA quality grades;
- automatic data-rejection rules;
- substitutes for source documentation;
- substitutes for professional hydrological assessment.

## Extreme events

Current extreme-event outputs are descriptive. The application does not provide a complete frequency analysis, official return periods or official return levels.

## Availability and network dependence

Map tiles and user-initiated ANA downloads depend on external network services. Availability, response format, rate limits and endpoint behavior may change outside this project's control.

## Temporal and locale behavior

Dates are handled primarily as hydrological daily dates. Deployment tests must verify timezone, locale, encoding and decimal-format behavior on the Linux host.
