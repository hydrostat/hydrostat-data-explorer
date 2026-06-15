# ============================================================
# duckdb_helpers.R
# Purpose: Shared SQL identifier quoting helper.
# Used by: pipeline/R/060, 061, 063, and 068 scripts.
#
# These functions were extracted only after confirming that
# their definitions were equivalent in all source scripts.
# ============================================================

quote_ident <- function(x) {
  paste0('"', gsub('"', '""', x), '"')
}

