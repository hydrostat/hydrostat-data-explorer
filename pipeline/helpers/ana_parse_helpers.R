# ============================================================
# ana_parse_helpers.R
# Purpose: Shared parsing, station-code, datetime, path, and ID helpers.
# Used by: pipeline/R/050, 051, and 052 processing scripts.
#
# These functions were extracted only after confirming that
# their definitions were equivalent in all source scripts.
# ============================================================

find_column <- function(data, candidates) {
  idx <- match(tolower(candidates), tolower(names(data)))
  idx <- idx[!is.na(idx)]

  if (length(idx) == 0) {
    return(NA_character_)
  }

  names(data)[idx[1]]
}

pick_column <- function(data, candidates) {
  column_name <- find_column(data, candidates)

  if (is.na(column_name)) {
    return(rep(NA_character_, nrow(data)))
  }

  data[[column_name]]
}

parse_decimal <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x[x %in% c("", "NA", "NaN", "null", "NULL")] <- NA_character_

  has_comma <- grepl(",", x)
  has_dot <- grepl("\\.", x)

  both_marks <- has_comma & has_dot
  x[both_marks] <- gsub("\\.", "", x[both_marks])
  x[both_marks] <- gsub(",", ".", x[both_marks])

  comma_decimal <- has_comma & !has_dot
  x[comma_decimal] <- gsub(",", ".", x[comma_decimal])

  suppressWarnings(as.numeric(x))
}

parse_integer_simple <- function(x) {
  suppressWarnings(as.integer(parse_decimal(x)))
}

parse_datetime_api <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x[x %in% c("", "NA", "NaN", "null", "NULL")] <- NA_character_
  x <- gsub("T", " ", x)

  out <- suppressWarnings(
    lubridate::ymd_hms(x, tz = "UTC", quiet = TRUE, truncated = 3)
  )

  missing_ymd <- is.na(out) & !is.na(x)
  if (any(missing_ymd)) {
    out[missing_ymd] <- suppressWarnings(
      lubridate::ymd(x[missing_ymd], tz = "UTC", quiet = TRUE)
    )
  }

  missing_dmy_hms <- is.na(out) & !is.na(x)
  if (any(missing_dmy_hms)) {
    out[missing_dmy_hms] <- suppressWarnings(
      lubridate::dmy_hms(x[missing_dmy_hms], tz = "UTC", quiet = TRUE, truncated = 3)
    )
  }

  missing_dmy <- is.na(out) & !is.na(x)
  if (any(missing_dmy)) {
    out[missing_dmy] <- suppressWarnings(
      lubridate::dmy(x[missing_dmy], tz = "UTC", quiet = TRUE)
    )
  }

  out
}

standardize_station_code <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x <- sub("\\.0$", "", x)
  x[x %in% c("", "NA", "NaN", "null", "NULL")] <- NA_character_

  numeric_code <- !is.na(x) & grepl("^[0-9]+$", x) & nchar(x) <= 8
  x[numeric_code] <- stringr::str_pad(x[numeric_code], width = 8, side = "left", pad = "0")

  x
}

to_project_path <- function(path) {
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  prefix <- paste0(project_root, "/")

  ifelse(startsWith(path, prefix), substring(path, nchar(prefix) + 1), path)
}

safe_min_datetime <- function(x) {
  if (all(is.na(x))) {
    return(as.POSIXct(NA))
  }

  min(x, na.rm = TRUE)
}

safe_max_datetime <- function(x) {
  if (all(is.na(x))) {
    return(as.POSIXct(NA))
  }

  max(x, na.rm = TRUE)
}

id_component <- function(x) {
  if (inherits(x, "POSIXct") || inherits(x, "POSIXt")) {
    x <- ifelse(is.na(x), NA_character_, format(x, "%Y%m%d%H%M%S", tz = "UTC"))
  } else if (inherits(x, "Date")) {
    x <- ifelse(is.na(x), NA_character_, format(x, "%Y%m%d"))
  } else {
    x <- as.character(x)
  }

  x[is.na(x) | x == ""] <- "NA"
  x <- trimws(x)
  x <- gsub("[^A-Za-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)

  x
}

make_id <- function(...) {
  parts <- list(...)
  parts <- lapply(parts, id_component)

  out <- parts[[1]]
  if (length(parts) > 1) {
    for (i in 2:length(parts)) {
      out <- paste(out, parts[[i]], sep = "__")
    }
  }

  out
}

