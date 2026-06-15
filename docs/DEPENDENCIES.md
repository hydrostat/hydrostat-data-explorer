# Dependencies

## Reference environment

The publication preflight was performed with:

```text
R 4.6.0
Windows 11 x64
UTF-8 locale
DuckDB R package 1.5.2
```

The target host uses Linux, so a staged deployment must verify binary compatibility, locale, timezone and system-library behavior.

## Direct runtime packages

```text
shiny
DBI
duckdb
dplyr
tidyr
purrr
readr
stringr
ggplot2
leaflet
DT
htmltools
scales
plotly
httr2
jsonlite
lubridate
evd
xml2
ragg
```

`manifest.json` is the authoritative deployment dependency record and must be regenerated from the validated publication environment.

## Publication tooling

```text
rsconnect
Git
Git LFS
```

These tools are not loaded by the running application.

## Pipeline-only packages

The reconstruction pipeline additionally uses packages such as:

```text
arrow
curl
XML
htmlwidgets
rmapshaper
sf
```

Pipeline dependencies are excluded from the Shiny deployment manifest unless they are also required transitively by the runtime.

## Likely Linux system requirements

Depending on the package binaries available to the hosting platform, installation may require support for:

- libcurl and TLS certificates;
- libxml2;
- GDAL, GEOS and PROJ for spatial packages;
- font and graphics libraries used by `ragg`;
- a DuckDB build compatible with the publication database file.

These requirements must be confirmed from deployment logs rather than installed by application scripts.
