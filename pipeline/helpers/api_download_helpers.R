# ============================================================
# api_download_helpers.R
# Purpose: Shared progress, response, and token-retry utilities.
# Used by: pipeline/R/042 download, 042 retry, and 044 cross-section scripts.
#
# These functions were extracted only after confirming that
# their definitions were equivalent in all source scripts.
# ============================================================

format_duration <- function(seconds) {
  seconds <- as.numeric(seconds)

  if (is.na(seconds) || !is.finite(seconds)) {
    return(NA_character_)
  }

  seconds <- round(seconds)
  hours <- seconds %/% 3600
  minutes <- (seconds %% 3600) %/% 60
  secs <- seconds %% 60

  sprintf("%02d:%02d:%02d", hours, minutes, secs)
}

estimate_remaining_time <- function(done, total, elapsed_seconds) {
  if (done <= 0 || total <= 0 || elapsed_seconds <= 0) {
    return(NA_real_)
  }

  rate <- done / elapsed_seconds
  remaining <- total - done

  if (remaining <= 0) {
    return(0)
  }

  remaining / rate
}

print_progress_status <- function(done, total, run_start_time, label = "Progress") {
  elapsed_seconds <- as.numeric(difftime(Sys.time(), run_start_time, units = "secs"))
  remaining_seconds <- estimate_remaining_time(done, total, elapsed_seconds)

  eta <- if (is.na(remaining_seconds)) {
    NA_character_
  } else {
    as.character(Sys.time() + remaining_seconds)
  }

  cat("\n", label, "\n", sep = "")
  cat("  Done:       ", done, " of ", total, "\n", sep = "")
  cat("  Elapsed:    ", format_duration(elapsed_seconds), "\n", sep = "")
  cat("  Remaining:  ", format_duration(remaining_seconds), "\n", sep = "")
  cat("  ETA:        ", eta, "\n", sep = "")
}

safe_n_items <- function(parsed) {
  if (is.null(parsed$items)) {
    return(0L)
  }

  if (is.data.frame(parsed$items)) {
    return(nrow(parsed$items))
  }

  if (is.list(parsed$items)) {
    return(length(parsed$items))
  }

  1L
}

get_scalar <- function(x, field) {
  if (!is.null(x[[field]]) && length(x[[field]]) > 0) {
    return(as.character(x[[field]][1]))
  }

  NA_character_
}

get_token_with_retries <- function(max_attempts, sleep_seconds, force_refresh = FALSE) {
  last_error <- NULL

  for (attempt in seq_len(max_attempts)) {
    if (isTRUE(force_refresh) && file.exists(ana_token_cache_file)) {
      file.remove(ana_token_cache_file)
    }

    token <- tryCatch(
      {
        ana_get_token()
      },
      error = function(e) {
        return(e)
      }
    )

    if (!inherits(token, "error") && !is.null(token) && !is.na(token) && nzchar(token)) {
      return(token)
    }

    last_error <- token
    cat("Token attempt ", attempt, " failed. Waiting ", sleep_seconds, " seconds...\n", sep = "")
    Sys.sleep(sleep_seconds)
  }

  if (inherits(last_error, "error")) {
    stop("Failed to obtain ANA token: ", conditionMessage(last_error))
  }

  stop("Failed to obtain ANA token.")
}

