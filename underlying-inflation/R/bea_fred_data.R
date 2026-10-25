# Standalone BEA and FRED ingestion for the underlying-inflation subproject.
#
# Public entry points:
#   fetch_bea_inputs(start_year = 1987L, refresh = FALSE)
#   fetch_dallas_data(refresh = FALSE)
#
# fetch_bea_inputs returns list(price, nominal). Each is a monthly long table
# with columns table, line_number, series_code, description, date, value.
# U20404 is the price index and U20405 is nominal PCE, both from the BEA
# NIUnderlyingDetail dataset, in their published units.
#
# fetch_dallas_data returns a data.table with columns date,
# dallas_monthly_annualized, dallas_yoy. The first two columns come from FRED
# PCETRIM1M158SFRBDAL and PCETRIM12M159SFRBDAL. The 1-month series starts in
# 1977-02, the 12-month series in 1978-01, so the returned table starts at the
# first month covered by both (1978-01).
#
# Project root and cache:
#   The project root is the underlying-inflation directory. It is taken from
#   options("underlying.project_root") (or, if that is unset, environment
#   variable UNDERLYING_PROJECT_ROOT). Both may be relative to getwd. When
#   neither is set, the root is found by walking up from getwd looking for
#   underlying-inflation/R/bea_fred_data.R, and then, when available, from this
#   file's path. Cache files live under <root>/data/raw.
#
#   BEA downloads are cached in chunks of three to five calendar years as
#   bea_<table>_<first>_<last>.csv, with a .meta sidecar. Cached chunks are
#   checked on every call: the chunk containing the current year must reach the
#   most recent plausibly published month, while older chunks must reach
#   December of their last year.
#   Stale, missing, or invalid chunks are refetched even when refresh = FALSE.
#   A failed refresh is an error; an older cache is never silently substituted.
#   refresh = TRUE refetches every chunk. New cache files are staged first and
#   committed only after both BEA tables have been fetched and validated.
#
#   Each FRED series is cached as fred_<series>.csv plus a .meta sidecar and is
#   refreshed when it is missing, invalid, or behind the latest plausible PCE
#   month. fetch_dallas_data joins the two series only over their common,
#   complete monthly coverage.
#
# Authentication:
#   BEA_API_KEY is used first. Otherwise <root>/.bea_api_key is read, then
#   <parent>/.bea_api_key for convenience when the subproject lives inside the
#   main trimmed-mean repository. Keys are never printed, logged, or written to
#   cache metadata. A key is only required when a BEA download is actually
#   needed.

if (!requireNamespace("data.table", quietly = TRUE)) {
  stop("Package 'data.table' is required.", call. = FALSE)
}

.ui_chunk_width <- 5L
.ui_bea_tables <- c(price = "U20404", nominal = "U20405")
.ui_fred_series <- c(
  dallas_monthly_annualized = "PCETRIM1M158SFRBDAL",
  dallas_yoy = "PCETRIM12M159SFRBDAL"
)
.ui_fred_first_period <- c(
  PCETRIM1M158SFRBDAL = "1977-02-01",
  PCETRIM12M159SFRBDAL = "1978-01-01"
)
.ui_meta_format <- "underlying-inflation-cache-v1"
.ui_missing_tokens <- c(
  "", ".", "..", "...", ".....", "NA", "N/A", "N.A.", "(D)", "(NA)", "--",
  "\u2014"
)
.ui_module_file <- local({
  candidates <- character()
  args <- commandArgs(trailingOnly = FALSE)
  file_arguments <- grep("^--file=", args, value = TRUE)
  if (length(file_arguments)) {
    candidates <- c(candidates, sub("^--file=", "", file_arguments[[1L]]))
  }
  frames <- sys.frames()
  for (i in seq_along(frames)) {
    ofile <- frames[[i]]$ofile
    if (!is.null(ofile) && length(ofile) == 1L && nzchar(ofile)) {
      candidates <- c(candidates, ofile)
    }
  }
  candidates <- unique(candidates[nzchar(candidates)])
  hit <- candidates[
    basename(candidates) == "bea_fred_data.R" &
      basename(dirname(candidates)) == "R" &
      basename(dirname(dirname(candidates))) == "underlying-inflation"
  ]
  if (!length(hit)) return(NA_character_)
  tryCatch(
    normalizePath(hit[[1L]], winslash = "/", mustWork = FALSE),
    error = function(e) NA_character_
  )
})

.ui_stop <- function(...) {
  stop(..., call. = FALSE)
}

.ui_short <- function(text, max_chars = 400L) {
  text <- paste(as.character(text), collapse = " ")
  if (nchar(text) > max_chars) {
    paste0(substr(text, 1L, max_chars), "...")
  } else {
    text
  }
}

.ui_redact <- function(text, key = NULL) {
  text <- paste(as.character(text), collapse = " ")
  if (!is.null(key) && length(key) == 1L && nzchar(key)) {
    text <- gsub(key, "[REDACTED]", text, fixed = TRUE)
  }
  text
}

.ui_clean_text <- function(text) {
  text <- trimws(as.character(text))
  text <- sub("^\ufeff", "", text)
  text
}

.ui_is_absolute <- function(path) {
  grepl("^([A-Za-z]:[/\\\\]|[/\\\\])", path)
}

.ui_explicit_root <- function() {
  root <- getOption("underlying.project_root", NULL)
  if (!is.null(root)) {
    if (!is.character(root) || length(root) != 1L || is.na(root)) {
      .ui_stop(
        "options(\"underlying.project_root\") must be a single directory path."
      )
    }
    root <- trimws(root)
    if (nzchar(root)) return(list(path = root, source = "option"))
  }
  root <- Sys.getenv("UNDERLYING_PROJECT_ROOT", unset = "")
  if (nzchar(trimws(root))) {
    return(list(path = trimws(root), source = "UNDERLYING_PROJECT_ROOT"))
  }
  NULL
}

.ui_normalize_root <- function(path, source) {
  path <- path.expand(path)
  if (!.ui_is_absolute(path)) path <- file.path(getwd(), path)
  if (!dir.exists(path)) {
    .ui_stop(source, " points to a directory that does not exist: ", path)
  }
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)

  # Accept the repository root as well as the subproject root when the caller
  # points at the parent repository.
  child <- file.path(path, "underlying-inflation")
  if (dir.exists(child) &&
      file.exists(file.path(child, "R", "bea_fred_data.R")) &&
      !file.exists(file.path(path, "R", "bea_fred_data.R"))) {
    path <- normalizePath(child, winslash = "/", mustWork = TRUE)
  }
  path
}

.ui_root_from_walk <- function() {
  current <- tryCatch(
    normalizePath(getwd(), winslash = "/", mustWork = TRUE),
    error = function(e) NULL
  )
  if (is.null(current)) return(NULL)
  repeat {
    child_marker <- file.path(
      current, "underlying-inflation", "R", "bea_fred_data.R"
    )
    if (file.exists(child_marker)) {
      return(normalizePath(
        file.path(current, "underlying-inflation"),
        winslash = "/", mustWork = TRUE
      ))
    }
    own_marker <- file.path(current, "R", "bea_fred_data.R")
    if (basename(current) == "underlying-inflation" &&
        file.exists(own_marker)) {
      return(current)
    }
    parent <- dirname(current)
    if (identical(parent, current)) break
    current <- parent
  }
  NULL
}

.ui_project_root <- function() {
  explicit <- .ui_explicit_root()
  if (!is.null(explicit)) {
    return(.ui_normalize_root(explicit$path, explicit$source))
  }
  root <- .ui_root_from_walk()
  if (!is.null(root)) return(root)
  if (!is.na(.ui_module_file)) {
    candidate <- dirname(dirname(.ui_module_file))
    if (basename(candidate) == "underlying-inflation" && dir.exists(candidate)) {
      return(normalizePath(candidate, winslash = "/", mustWork = TRUE))
    }
  }
  .ui_stop(
    "Could not locate the underlying-inflation project root. Set ",
    "options(underlying.project_root = \"/path/to/underlying-inflation\") or ",
    "UNDERLYING_PROJECT_ROOT, or run from the subproject or repository root."
  )
}

.ui_raw_dir <- function(root = .ui_project_root()) {
  raw_dir <- file.path(root, "data", "raw")
  if (!dir.exists(raw_dir)) {
    dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
    if (!dir.exists(raw_dir)) {
      .ui_stop("Could not create cache directory: ", raw_dir)
    }
  }
  normalizePath(raw_dir, winslash = "/", mustWork = TRUE)
}

.ui_validate_year <- function(x, name) {
  if (length(x) != 1L || is.na(x)) {
    .ui_stop("`", name, "` must be a single year.")
  }
  value <- suppressWarnings(as.numeric(x))
  if (is.na(value) || value != floor(value) || value < 1900 || value > 2200) {
    .ui_stop("`", name, "` must be an integer year between 1900 and 2200.")
  }
  as.integer(value)
}

.ui_validate_refresh <- function(refresh) {
  if (!is.logical(refresh) || length(refresh) != 1L || is.na(refresh)) {
    .ui_stop("`refresh` must be TRUE or FALSE.")
  }
  refresh
}

.ui_year_chunks <- function(start_year, end_year) {
  span <- end_year - start_year + 1L
  if (span < 3L) {
    # Keep every API request at least three years wide. The extra leading years
    # are cached but removed by the final start_year filter.
    start_year <- start_year - (3L - span)
  }
  starts <- seq.int(start_year, end_year, by = .ui_chunk_width)
  chunks <- lapply(starts, function(start) {
    seq.int(start, min(start + .ui_chunk_width - 1L, end_year))
  })
  n_chunks <- length(chunks)
  if (n_chunks >= 2L && length(chunks[[n_chunks]]) < 3L) {
    combined <- c(chunks[[n_chunks - 1L]], chunks[[n_chunks]])
    midpoint <- floor(length(combined) / 2L)
    chunks[[n_chunks - 1L]] <- combined[seq_len(midpoint)]
    chunks[[n_chunks]] <- combined[(midpoint + 1L):length(combined)]
  }
  chunks
}

.ui_previous_month <- function(now = Sys.Date()) {
  month_start <- as.Date(format(now, "%Y-%m-01"))
  seq(month_start, by = "-1 month", length.out = 2L)[[2L]]
}

.ui_latest_floor <- function(now = Sys.Date()) {
  month_start <- as.Date(format(now, "%Y-%m-01"))
  seq(month_start, by = "-1 month", length.out = 3L)[[3L]]
}

# BEA and FRED publish a reference month near the end of the following month.
# Before the 26th, the prior month's release is not normally available yet, so
# the most recent plausibly published month is two months back.
.ui_expected_published_month <- function(now = Sys.Date()) {
  if (as.integer(format(now, "%d")) >= 26L) {
    .ui_previous_month(now)
  } else {
    .ui_latest_floor(now)
  }
}

.ui_expected_latest <- function(years, now = Sys.Date()) {
  max_year <- max(years)
  current_year <- as.integer(format(now, "%Y"))
  if (max_year < current_year) {
    as.Date(sprintf("%d-12-01", max_year))
  } else {
    .ui_expected_published_month(now)
  }
}

.ui_now_text <- function(time = Sys.time()) {
  format(time, "%Y-%m-%dT%H:%M:%S%z")
}

.ui_parse_line_number <- function(x, context) {
  if (is.numeric(x)) {
    value <- as.numeric(x)
    bad <- is.na(value) | !is.finite(value) | value != floor(value) | value < 1
  } else {
    text <- .ui_clean_text(x)
    ok <- grepl("^[0-9]+$", text)
    value <- suppressWarnings(as.numeric(text))
    bad <- !ok | is.na(value) | value < 1
  }
  if (any(bad)) {
    examples <- unique(as.character(x)[bad])
    examples <- examples[seq_len(min(5L, length(examples)))]
    .ui_stop(
      context, " contains missing or non-integer LineNumber values; examples: ",
      paste(examples, collapse = ", ")
    )
  }
  as.integer(value)
}

.ui_parse_bea_period <- function(x, context) {
  if (inherits(x, "Date") || inherits(x, "IDate")) {
    dates <- as.Date(x)
    bad <- is.na(dates) | format(dates, "%d") != "01"
    if (any(bad)) {
      .ui_stop(context, " contains nonmonthly dates (expected the first of a month).")
    }
    return(unname(as.Date(format(dates, "%Y-%m-%d"))))
  }
  text <- toupper(.ui_clean_text(x))
  monthly <- !is.na(text) & grepl("^[0-9]{4}M[0-9]{1,2}$", text)
  dates <- rep(as.Date(NA_character_), length(text))
  if (any(monthly)) {
    year <- substr(text[monthly], 1L, 4L)
    month <- suppressWarnings(
      as.integer(sub("^[0-9]{4}M", "", text[monthly]))
    )
    ok <- !is.na(month) & month >= 1L & month <= 12L
    index <- which(monthly)[ok]
    dates[index] <- as.Date(
      sprintf("%s-%02d-01", year[ok], month[ok]),
      format = "%Y-%m-%d"
    )
  }
  bad <- is.na(dates)
  if (any(bad)) {
    examples <- unique(text[bad])
    examples <- examples[seq_len(min(5L, length(examples)))]
    .ui_stop(
      context, " must contain strict monthly YYYYMmm periods; examples: ",
      paste(examples, collapse = ", ")
    )
  }
  unname(as.Date(format(dates, "%Y-%m-%d")))
}

.ui_parse_iso_month <- function(x, context) {
  if (inherits(x, "Date") || inherits(x, "IDate")) {
    dates <- as.Date(x)
    bad <- is.na(dates) | format(dates, "%d") != "01"
    if (any(bad)) {
      .ui_stop(context, " contains nonmonthly dates (expected the first of a month).")
    }
    return(unname(as.Date(format(dates, "%Y-%m-%d"))))
  }
  text <- .ui_clean_text(x)
  matches <- !is.na(text) & grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", text)
  dates <- rep(as.Date(NA_character_), length(text))
  if (any(matches)) {
    parsed <- suppressWarnings(as.Date(text[matches], format = "%Y-%m-%d"))
    ok <- !is.na(parsed) &
      format(parsed, "%Y-%m-%d") == text[matches] &
      format(parsed, "%d") == "01"
    index <- which(matches)[ok]
    dates[index] <- parsed[ok]
  }
  bad <- is.na(dates)
  if (any(bad)) {
    examples <- unique(text[bad])
    examples <- examples[seq_len(min(5L, length(examples)))]
    .ui_stop(
      context, " must contain strict YYYY-MM-DD monthly dates; examples: ",
      paste(examples, collapse = ", ")
    )
  }
  dates
}

.ui_parse_numeric <- function(x, context, require_complete = FALSE) {
  if (is.numeric(x)) {
    value <- as.numeric(x)
    value[is.nan(value)] <- NA_real_
  } else {
    text <- .ui_clean_text(x)
    text <- gsub(",", "", text, fixed = TRUE)
    missing <- toupper(text) %in% .ui_missing_tokens
    text[missing] <- NA_character_
    value <- suppressWarnings(as.numeric(text))
    bad <- is.na(value) & !is.na(text)
    if (any(bad)) {
      examples <- unique(text[bad])
      examples <- examples[seq_len(min(5L, length(examples)))]
      .ui_stop(
        context, " contains values that are not numeric; examples: ",
        paste(examples, collapse = ", ")
      )
    }
  }
  if (any(is.infinite(value))) {
    .ui_stop(context, " contains infinite numeric values.")
  }
  if (require_complete && anyNA(value)) {
    first <- which(is.na(value))[[1L]]
    .ui_stop(
      context, " contains ", sum(is.na(value)),
      " missing numeric value(s); first at row ", first, "."
    )
  }
  value
}

.ui_assert_monthly_sequence <- function(dates, context) {
  dates <- as.Date(dates)
  if (!length(dates)) {
    .ui_stop(context, " contains no monthly dates.")
  }
  if (anyNA(dates)) {
    .ui_stop(context, " contains missing dates.")
  }
  if (any(format(dates, "%d") != "01")) {
    .ui_stop(context, " contains dates that are not the first of a month.")
  }
  if (anyDuplicated(dates)) {
    duplicate <- dates[duplicated(dates)][[1L]]
    .ui_stop(context, " contains duplicate month ", format(duplicate), ".")
  }
  dates <- sort(dates)
  if (length(dates) > 1L) {
    month_id <- as.integer(format(dates, "%Y")) * 12L +
      as.integer(format(dates, "%m"))
    if (any(diff(month_id) != 1L)) {
      gap <- which(diff(month_id) != 1L)[[1L]]
      .ui_stop(
        context, " is not monthly-contiguous; a month is missing before ",
        format(dates[gap + 1L]), "."
      )
    }
  }
  invisible(dates)
}

.ui_find_column <- function(columns, candidates) {
  hit <- match(tolower(candidates), tolower(columns))
  hit <- hit[!is.na(hit)]
  if (!length(hit)) return(NA_character_)
  columns[[hit[[1L]]]]
}

.ui_text_column <- function(x, context, name, required = TRUE) {
  text <- .ui_clean_text(x)
  text[is.na(text) | !nzchar(text)] <- NA_character_
  if (required && anyNA(text)) {
    .ui_stop(context, " has missing or empty ", name, " values.")
  }
  text
}

.ui_dedupe_bea <- function(dt, context) {
  unique_dt <- unique(dt)
  keys <- c("table", "line_number", "date")
  duplicate <- duplicated(unique_dt, by = keys) |
    duplicated(unique_dt, by = keys, fromLast = TRUE)
  if (any(duplicate)) {
    first <- unique_dt[duplicate][[1L]]
    .ui_stop(
      context, " contains conflicting duplicate line-date rows (line ",
      first$line_number, " on ", format(first$date), ")."
    )
  }
  unique_dt[]
}

.ui_normalize_bea <- function(raw, table, years,
                               context = paste0("BEA response for table ", table)) {
  if (is.list(raw) && !is.data.frame(raw)) {
    if (is.data.frame(raw$Data)) {
      raw <- raw$Data
    } else {
      .ui_stop(context, " is not tabular (class: ", paste(class(raw), collapse = "/"), ").")
    }
  }
  if (!is.data.frame(raw)) {
    .ui_stop(context, " is not tabular (class: ", paste(class(raw), collapse = "/"), ").")
  }
  dt <- data.table::as.data.table(raw)
  columns <- names(dt)

  line_col <- .ui_find_column(columns, "LineNumber")
  time_col <- .ui_find_column(columns, c("TimePeriod", "Year"))
  value_col <- .ui_find_column(columns, "DataValue")
  series_col <- .ui_find_column(columns, c("SeriesCode", "SeriesID"))
  description_col <- .ui_find_column(columns, c("LineDescription", "Description"))

  missing <- c(
    if (is.na(line_col)) "LineNumber",
    if (is.na(time_col)) "TimePeriod",
    if (is.na(value_col)) "DataValue",
    if (is.na(series_col)) "SeriesCode",
    if (is.na(description_col)) "LineDescription"
  )
  if (length(missing)) {
    .ui_stop(
      context, " is missing required column(s): ", paste(missing, collapse = ", "),
      ". Columns returned: ", paste(columns, collapse = ", "), "."
    )
  }

  line_number <- .ui_parse_line_number(dt[[line_col]], context)
  dates <- .ui_parse_bea_period(dt[[time_col]], context)
  values <- .ui_parse_numeric(dt[[value_col]], context)
  series_code <- .ui_text_column(dt[[series_col]], context, "SeriesCode")
  description <- .ui_text_column(dt[[description_col]], context, "LineDescription")

  out <- data.table::data.table(
    table = rep(table, nrow(dt)),
    line_number = line_number,
    series_code = series_code,
    description = description,
    date = dates,
    value = values
  )
  years_seen <- as.integer(format(out$date, "%Y"))
  out <- out[years_seen >= min(years) & years_seen <= max(years)]
  if (nrow(out) == 0L) {
    .ui_stop(
      context, " has no observations in years ", min(years), "-", max(years), "."
    )
  }
  if (anyNA(out$value)) {
    .ui_stop(context, " contains missing numeric values in the requested years.")
  }

  out <- .ui_dedupe_bea(out, context)
  .ui_assert_monthly_sequence(unique(out$date), context)
  data.table::setorder(out, line_number, date)
  out[]
}

.ui_read_bea_cache <- function(path, table, years) {
  context <- paste0("Cached BEA chunk ", path)
  dt <- tryCatch(
    data.table::fread(path, data.table = TRUE, na.strings = c("NA", "")),
    error = function(e) {
      .ui_stop(context, " could not be read: ", conditionMessage(e))
    }
  )
  required <- c(
    "table", "line_number", "series_code", "description", "date", "value"
  )
  missing <- setdiff(required, names(dt))
  if (length(missing)) {
    .ui_stop(context, " is missing column(s): ", paste(missing, collapse = ", "))
  }
  if (nrow(dt) == 0L) .ui_stop(context, " is empty.")

  table_text <- .ui_clean_text(dt$table)
  if (anyNA(table_text) || any(table_text != table)) {
    .ui_stop(context, " contains a table other than ", table, ".")
  }
  line_number <- .ui_parse_line_number(dt$line_number, context)
  dates <- .ui_parse_iso_month(dt$date, context)
  values <- .ui_parse_numeric(dt$value, context, require_complete = TRUE)
  series_code <- .ui_text_column(dt$series_code, context, "series_code")
  description <- .ui_text_column(dt$description, context, "description")

  out <- data.table::data.table(
    table = rep(table, nrow(dt)),
    line_number = line_number,
    series_code = series_code,
    description = description,
    date = dates,
    value = values
  )
  years_seen <- as.integer(format(out$date, "%Y"))
  if (any(years_seen < min(years) | years_seen > max(years))) {
    .ui_stop(context, " contains observations outside the chunk years.")
  }
  out <- .ui_dedupe_bea(out, context)
  .ui_assert_monthly_sequence(unique(out$date), context)
  data.table::setorder(out, line_number, date)
  out[]
}

.ui_meta_value <- function(x) {
  if (length(x) == 0L) return("")
  x <- x[!is.na(x)]
  if (!length(x)) return("")
  paste(as.character(x), collapse = ",")
}

.ui_write_meta <- function(meta, path) {
  if (!is.list(meta) || is.null(names(meta)) || any(!nzchar(names(meta)))) {
    .ui_stop("Cache metadata must be a named list.")
  }
  values <- vapply(meta, .ui_meta_value, character(1L))
  if (any(grepl("[\r\n]", values))) {
    .ui_stop("Cache metadata contains a line break.")
  }
  lines <- paste0(names(meta), ": ", values)
  directory <- dirname(path)
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(paste0(basename(path), "."), tmpdir = directory, fileext = ".tmp")
  on.exit(unlink(tmp), add = TRUE)
  tryCatch(
    writeLines(lines, tmp, useBytes = TRUE),
    error = function(e) {
      .ui_stop("Could not write cache metadata ", path, ": ", conditionMessage(e))
    }
  )
  if (!file.copy(tmp, path, overwrite = TRUE)) {
    .ui_stop("Could not replace cache metadata ", path, ".")
  }
  invisible(path)
}

.ui_make_meta <- function(data, dataset, table, series, years, request) {
  dates <- as.Date(data$date)
  if (!nrow(data) || anyNA(dates)) {
    .ui_stop("Cannot build cache metadata for empty or invalid data.")
  }
  list(
    format = .ui_meta_format,
    dataset = dataset,
    table = if (is.null(table) || !nzchar(table)) "none" else table,
    series = if (is.null(series) || !nzchar(series)) "none" else series,
    frequency = "M",
    years = paste(sort(unique(as.integer(years))), collapse = ","),
    request = request,
    fetched_at = .ui_now_text(),
    first_period = format(min(dates), "%Y-%m-%d"),
    latest_period = format(max(dates), "%Y-%m-%d"),
    rows = as.character(nrow(data))
  )
}

.ui_read_meta <- function(path) {
  if (!file.exists(path)) return(NULL)
  raw <- tryCatch(read.dcf(path, all = TRUE), error = function(e) NULL)
  if (is.null(raw) || nrow(raw) != 1L) return(NULL)
  values <- as.list(raw[1L, ])
  names(values) <- colnames(raw)
  values
}

.ui_meta_issues <- function(meta, dataset, table, series, years, data) {
  issues <- character()
  add <- function(text) issues <<- c(issues, text)

  required <- c(
    "format", "dataset", "table", "series", "frequency", "years",
    "request", "fetched_at", "first_period", "latest_period", "rows"
  )
  if (is.null(meta) || !is.list(meta)) return("metadata is missing or malformed")
  missing <- setdiff(required, names(meta))
  if (length(missing)) {
    return(paste0("metadata is missing field(s): ", paste(missing, collapse = ", ")))
  }
  if (any(vapply(meta[required], function(x) {
    length(x) != 1L || is.na(x) || !nzchar(trimws(as.character(x)))
  }, logical(1L)))) {
    add("metadata has empty required field(s)")
  }

  if (!identical(as.character(meta$format), .ui_meta_format)) {
    add("unsupported metadata format")
  }
  if (!identical(as.character(meta$dataset), dataset)) {
    add("dataset does not match the cache file")
  }
  expected_table <- if (is.null(table) || !nzchar(table)) "none" else table
  if (!identical(as.character(meta$table), expected_table)) {
    add("table does not match the cache file")
  }
  expected_series <- if (is.null(series) || !nzchar(series)) "none" else series
  if (!identical(as.character(meta$series), expected_series)) {
    add("series does not match the cache file")
  }
  if (!identical(as.character(meta$frequency), "M")) {
    add("frequency is not monthly")
  }

  meta_years <- suppressWarnings(
    as.integer(strsplit(as.character(meta$years), ",", fixed = TRUE)[[1L]])
  )
  expected_years <- sort(unique(as.integer(years)))
  if (anyNA(meta_years) || !identical(meta_years, expected_years)) {
    add("years do not match the cache file")
  }

  rows <- suppressWarnings(as.integer(as.character(meta$rows)))
  if (is.na(rows) || rows != nrow(data)) {
    add("row count does not match the cache file")
  }

  first_period <- tryCatch(
    .ui_parse_iso_month(meta$first_period, "Cache metadata first_period"),
    error = function(e) as.Date(NA_character_)
  )
  latest_period <- tryCatch(
    .ui_parse_iso_month(meta$latest_period, "Cache metadata latest_period"),
    error = function(e) as.Date(NA_character_)
  )
  if (is.na(first_period) || first_period != min(as.Date(data$date))) {
    add("first_period does not match the cache file")
  }
  if (is.na(latest_period) || latest_period != max(as.Date(data$date))) {
    add("latest_period does not match the cache file")
  }
  if (!grepl(
    "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}([+-][0-9]{4}|Z)$",
    as.character(meta$fetched_at)
  )) {
    add("fetched_at is not an ISO timestamp")
  }
  unique(issues)
}

.ui_bea_chunk_path <- function(raw_dir, table, years) {
  file.path(
    raw_dir,
    sprintf("bea_%s_%d_%d.csv", table, min(years), max(years))
  )
}

.ui_fred_cache_path <- function(raw_dir, series) {
  file.path(raw_dir, paste0("fred_", series, ".csv"))
}

.ui_fred_url <- function(series) {
  paste0("https://fred.stlouisfed.org/graph/fredgraph.csv?id=", series)
}

.ui_bea_request_text <- function(table, years) {
  paste0(
    "Method=GetData; datasetname=NIUnderlyingDetail; TableName=", table,
    "; Frequency=M; Year=", paste(years, collapse = ","),
    "; ResultFormat=JSON"
  )
}

.ui_bea_cache_fresh <- function(meta, years, now = Sys.Date()) {
  expected_first <- as.Date(sprintf("%d-01-01", min(years)))
  expected_latest <- .ui_expected_latest(years, now = now)
  first_period <- suppressWarnings(as.Date(as.character(meta$first_period)))
  latest_period <- suppressWarnings(as.Date(as.character(meta$latest_period)))
  !is.na(first_period) && first_period <= expected_first &&
    !is.na(latest_period) && latest_period >= expected_latest
}

.ui_fred_cache_fresh <- function(meta, series, now = Sys.Date()) {
  expected_first <- as.Date(.ui_fred_first_period[[series]])
  expected_latest <- .ui_expected_published_month(now)
  first_period <- suppressWarnings(as.Date(as.character(meta$first_period)))
  latest_period <- suppressWarnings(as.Date(as.character(meta$latest_period)))
  !is.na(first_period) && first_period <= expected_first &&
    !is.na(latest_period) && latest_period >= expected_latest
}

.ui_inspect_bea_cache <- function(path, table, years, now = Sys.Date()) {
  if (!file.exists(path)) {
    return(list(status = "missing", reason = "cache missing"))
  }
  data <- NULL
  read_error <- NULL
  data <- tryCatch(
    .ui_read_bea_cache(path, table, years),
    error = function(e) {
      read_error <<- conditionMessage(e)
      NULL
    }
  )
  if (is.null(data)) {
    return(list(
      status = "invalid", reason = paste0("invalid cache: ", read_error)
    ))
  }

  meta <- .ui_read_meta(paste0(path, ".meta"))
  issues <- .ui_meta_issues(
    meta,
    dataset = "BEA NIUnderlyingDetail",
    table = table,
    series = NULL,
    years = years,
    data = data
  )
  if (length(issues)) {
    return(list(
      status = "invalid",
      reason = paste0("invalid cache metadata: ", paste(issues, collapse = "; ")),
      data = data
    ))
  }
  if (!.ui_bea_cache_fresh(meta, years, now = now)) {
    return(list(
      status = "stale",
      reason = paste0(
        "stale cache ending ", meta$latest_period, "; expected ",
        format(.ui_expected_latest(years, now), "%Y-%m-%d")
      ),
      data = data,
      meta = meta
    ))
  }
  list(status = "ok", reason = NULL, data = data, meta = meta)
}

.ui_parse_fred_file <- function(path, series, context, normalized = FALSE) {
  raw <- tryCatch(
    data.table::fread(path, data.table = TRUE),
    error = function(e) {
      .ui_stop(context, " could not be parsed: ", conditionMessage(e))
    }
  )
  if (ncol(raw) < 2L) {
    .ui_stop(context, " has fewer than two columns.")
  }
  first_header <- tolower(.ui_clean_text(names(raw)[[1L]]))
  if (!first_header %in% c("observation_date", "date")) {
    .ui_stop(context, " has an unexpected first column: ", names(raw)[[1L]])
  }
  second_header <- toupper(.ui_clean_text(names(raw)[[2L]]))
  expected_header <- if (normalized) "VALUE" else toupper(series)
  if (!identical(second_header, expected_header)) {
    .ui_stop(
      context, " has an unexpected second column: ", names(raw)[[2L]],
      if (normalized) " (expected value)." else paste0(" (expected ", series, ").")
    )
  }

  dates <- .ui_parse_iso_month(raw[[1L]], context)
  values <- .ui_parse_numeric(raw[[2L]], context)
  if (anyNA(values)) {
    first <- which(is.na(values))[[1L]]
    .ui_stop(
      context, " has a missing value at ", format(dates[[first]]), "."
    )
  }
  if (anyDuplicated(dates)) {
    .ui_stop(
      context, " contains duplicate dates (first: ",
      format(dates[duplicated(dates)][[1L]]), ")."
    )
  }
  .ui_assert_monthly_sequence(dates, context)
  expected_first <- as.Date(.ui_fred_first_period[[series]])
  if (min(dates) > expected_first) {
    .ui_stop(
      context, " starts on ", format(min(dates)),
      ", later than the published series start ", format(expected_first), "."
    )
  }
  out <- data.table::data.table(date = dates, value = values)
  data.table::setorder(out, date)
  out[]
}

.ui_read_fred_cache <- function(path, series) {
  .ui_parse_fred_file(
    path, series,
    paste0("Cached FRED series ", series, " (", path, ")"),
    normalized = TRUE
  )
}

.ui_inspect_fred_cache <- function(path, series, now = Sys.Date()) {
  if (!file.exists(path)) {
    return(list(status = "missing", reason = "cache missing"))
  }
  data <- NULL
  read_error <- NULL
  data <- tryCatch(
    .ui_read_fred_cache(path, series),
    error = function(e) {
      read_error <<- conditionMessage(e)
      NULL
    }
  )
  if (is.null(data)) {
    return(list(
      status = "invalid", reason = paste0("invalid cache: ", read_error)
    ))
  }

  meta <- .ui_read_meta(paste0(path, ".meta"))
  years <- sort(unique(as.integer(format(data$date, "%Y"))))
  issues <- .ui_meta_issues(
    meta,
    dataset = "FRED",
    table = NULL,
    series = series,
    years = years,
    data = data
  )
  if (length(issues)) {
    return(list(
      status = "invalid",
      reason = paste0("invalid cache metadata: ", paste(issues, collapse = "; ")),
      data = data
    ))
  }
  if (!.ui_fred_cache_fresh(meta, series, now = now)) {
    return(list(
      status = "stale",
      reason = paste0(
        "stale cache ending ", meta$latest_period, "; expected ",
        format(.ui_expected_published_month(now), "%Y-%m-%d")
      ),
      data = data,
      meta = meta
    ))
  }
  list(status = "ok", reason = NULL, data = data, meta = meta)
}

.ui_parse_key <- function(key, source) {
  key <- sub("^\ufeff", "", trimws(as.character(key)))
  if (nchar(key) >= 2L) {
    first <- substr(key, 1L, 1L)
    last <- substr(key, nchar(key), nchar(key))
    if (first == last && first %in% c("\"", "'")) {
      key <- substr(key, 2L, nchar(key) - 1L)
    }
  }
  key <- gsub("[[:space:]]", "", key)
  if (!nzchar(key)) {
    .ui_stop("BEA API key from ", source, " is empty.")
  }
  if (nchar(key) != 36L) {
    .ui_stop(
      "BEA API key from ", source,
      " is invalid: expected 36 characters (key not shown)."
    )
  }
  key
}

.ui_read_key <- function(root = .ui_project_root()) {
  env_key <- Sys.getenv("BEA_API_KEY", unset = "")
  if (nzchar(trimws(env_key))) {
    return(.ui_parse_key(env_key, "BEA_API_KEY"))
  }

  candidates <- unique(c(
    file.path(root, ".bea_api_key"),
    file.path(dirname(root), ".bea_api_key")
  ))
  existing <- candidates[file.exists(candidates)]
  if (!length(existing)) {
    .ui_stop(
      "BEA API key not found. Set BEA_API_KEY or create ", candidates[[1L]],
      " containing exactly one key (a parent key file is also accepted)."
    )
  }
  key_file <- existing[[1L]]

  lines <- tryCatch(
    readLines(key_file, warn = FALSE, encoding = "UTF-8"),
    error = function(e) {
      .ui_stop("Could not read BEA key file ", key_file, ": ", conditionMessage(e))
    }
  )
  lines <- .ui_clean_text(lines)
  lines <- lines[nzchar(lines) & !startsWith(lines, "#")]
  if (length(lines) == 0L) {
    .ui_stop("BEA key file ", key_file, " is empty.")
  }
  if (length(lines) > 1L) {
    .ui_stop(
      "BEA key file ", key_file, " must contain exactly one key; found ",
      length(lines), " non-empty lines."
    )
  }
  .ui_parse_key(lines[[1L]], key_file)
}

.ui_make_stage <- function(raw_dir) {
  stage_dir <- tempfile(".cache-stage-", tmpdir = raw_dir)
  if (!dir.create(stage_dir, recursive = TRUE, showWarnings = FALSE)) {
    .ui_stop("Could not create cache staging directory in ", raw_dir, ".")
  }
  normalizePath(stage_dir, winslash = "/", mustWork = TRUE)
}

.ui_write_csv <- function(data, path, context) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tryCatch(
    data.table::fwrite(data, path, na = "NA"),
    error = function(e) {
      .ui_stop("Could not write ", context, " to ", path, ": ", conditionMessage(e))
    }
  )
  info <- file.info(path)
  if (!file.exists(path) || is.na(info$size) || info$size <= 0) {
    .ui_stop("Wrote an empty ", context, " to ", path, ".")
  }
  invisible(path)
}

.ui_stage_cache <- function(data, meta, stage_dir, target) {
  staged_csv <- file.path(stage_dir, basename(target))
  staged_meta <- paste0(staged_csv, ".meta")
  .ui_write_csv(data, staged_csv, "cache file")
  .ui_write_meta(meta, staged_meta)
  list(
    target = target,
    target_meta = paste0(target, ".meta"),
    staged_csv = staged_csv,
    staged_meta = staged_meta
  )
}

.ui_commit_cache <- function(entries, raw_dir) {
  entries <- Filter(function(x) !is.null(x), entries)
  if (!length(entries)) return(invisible(NULL))
  targets <- vapply(entries, function(entry) entry$target, character(1L))
  if (anyDuplicated(targets)) {
    .ui_stop("Cache commit contains duplicate target files.")
  }

  backup_dir <- tempfile(".cache-backup-", tmpdir = raw_dir)
  if (!dir.create(backup_dir, recursive = TRUE, showWarnings = FALSE)) {
    .ui_stop("Could not create cache backup directory in ", raw_dir, ".")
  }
  on.exit(unlink(backup_dir, recursive = TRUE, force = TRUE), add = TRUE)

  files <- list()
  for (i in seq_along(entries)) {
    entry <- entries[[i]]
    for (kind in c("csv", "meta")) {
      target <- if (identical(kind, "csv")) entry$target else entry$target_meta
      staged <- if (identical(kind, "csv")) entry$staged_csv else entry$staged_meta
      exists <- file.exists(target)
      backup <- file.path(backup_dir, sprintf("%03d_%s", i, kind))
      if (exists && !file.copy(target, backup, overwrite = TRUE)) {
        .ui_stop("Could not back up cache file ", target, ".")
      }
      files[[length(files) + 1L]] <- list(
        target = target,
        staged = staged,
        existed = exists,
        backup = backup
      )
    }
  }

  restore <- function() {
    for (item in rev(files)) {
      if (isTRUE(item$existed)) {
        try(file.copy(item$backup, item$target, overwrite = TRUE), silent = TRUE)
      } else {
        try(unlink(item$target, force = TRUE), silent = TRUE)
      }
    }
    invisible(NULL)
  }

  tryCatch({
    for (item in files) {
      if (!file.copy(item$staged, item$target, overwrite = TRUE)) {
        .ui_stop("Could not replace cache file ", item$target, ".")
      }
      info <- file.info(item$target)
      if (is.na(info$size) || info$size <= 0) {
        .ui_stop("Cache file ", item$target, " is empty after commit.")
      }
    }
  }, error = function(e) {
    restore()
    .ui_stop("Could not commit cache update: ", conditionMessage(e))
  })
  invisible(NULL)
}

.ui_require_bea <- function() {
  if (!requireNamespace("bea.R", quietly = TRUE)) {
    .ui_stop(
      "Package 'bea.R' is required for live BEA downloads; install it with ",
      "install.packages(\"bea.R\")."
    )
  }
}

.ui_fetch_bea_chunk <- function(table, years, key) {
  .ui_require_bea()
  spec <- list(
    UserID = key,
    Method = "GetData",
    datasetname = "NIUnderlyingDetail",
    TableName = table,
    Frequency = "M",
    Year = paste(years, collapse = ","),
    ResultFormat = "JSON"
  )

  captured <- character()
  raw <- tryCatch(
    withCallingHandlers(
      bea.R::beaGet(spec, asTable = TRUE, asWide = FALSE, iTableStyle = FALSE),
      warning = function(w) {
        captured <<- c(captured, conditionMessage(w))
        invokeRestart("muffleWarning")
      },
      message = function(m) {
        captured <<- c(captured, conditionMessage(m))
        invokeRestart("muffleMessage")
      }
    ),
    error = function(e) {
      .ui_stop(
        "BEA API request failed: ",
        .ui_short(.ui_redact(conditionMessage(e), key))
      )
    }
  )

  if (is.list(raw) && !is.data.frame(raw) && is.data.frame(raw$Data)) {
    raw <- raw$Data
  }
  if (!is.data.frame(raw)) {
    detail <- c(
      if (is.character(raw)) raw else character(),
      captured
    )
    detail <- detail[nzchar(detail)]
    if (!length(detail)) {
      detail <- paste0(
        "unexpected result class: ", paste(class(raw), collapse = "/")
      )
    }
    .ui_stop(
      "BEA API returned an error: ",
      .ui_short(.ui_redact(paste(unique(detail), collapse = "; "), key))
    )
  }
  if (nrow(raw) == 0L) {
    detail <- if (length(captured)) paste(unique(captured), collapse = "; ") else "no data rows"
    .ui_stop(
      "BEA API returned no data: ",
      .ui_short(.ui_redact(detail, key))
    )
  }
  tryCatch(
    .ui_normalize_bea(raw, table, years),
    error = function(e) {
      .ui_stop(.ui_redact(conditionMessage(e), key))
    }
  )
}

.ui_validate_bea_chunk_span <- function(data, years, table) {
  first_expected <- as.Date(sprintf("%d-01-01", min(years)))
  if (min(data$date) > first_expected) {
    .ui_stop(
      "BEA response for table ", table, " starts on ", format(min(data$date)),
      ", later than the requested chunk start ", format(first_expected), "."
    )
  }
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  if (max(years) < current_year) {
    last_expected <- as.Date(sprintf("%d-12-01", max(years)))
    if (max(data$date) < last_expected) {
      .ui_stop(
        "BEA response for table ", table, " ends on ", format(max(data$date)),
        ", earlier than the complete chunk end ", format(last_expected), "."
      )
    }
  } else {
    floor <- .ui_latest_floor()
    if (max(data$date) < floor) {
      .ui_stop(
        "BEA response for table ", table, " ends on ", format(max(data$date)),
        ", earlier than the two-month freshness floor ", format(floor), "."
      )
    }
  }
  invisible(TRUE)
}

.ui_finalize_bea <- function(data, table, start_year, end_year, context) {
  required <- c(
    "table", "line_number", "series_code", "description", "date", "value"
  )
  missing <- setdiff(required, names(data))
  if (length(missing)) {
    .ui_stop(context, " is missing column(s): ", paste(missing, collapse = ", "))
  }
  years_seen <- as.integer(format(as.Date(data$date), "%Y"))
  data <- data[
    years_seen >= start_year & years_seen <= end_year
  ]
  if (nrow(data) == 0L) {
    .ui_stop(context, " has no observations in years ", start_year, "-", end_year, ".")
  }
  data <- .ui_dedupe_bea(data, context)
  .ui_assert_monthly_sequence(unique(data$date), context)
  if (anyNA(data$value)) {
    .ui_stop(context, " contains missing numeric values.")
  }

  expected_first <- as.Date(sprintf("%d-01-01", start_year))
  if (min(data$date) != expected_first) {
    .ui_stop(
      context, " starts on ", format(min(data$date)),
      "; expected the requested first month ", format(expected_first), "."
    )
  }
  data.table::setcolorder(data, required)
  data.table::setorder(data, line_number, date)
  data[]
}

.ui_validate_bea_pair <- function(price, nominal, start_year) {
  price_months <- sort(unique(as.Date(price$date)))
  nominal_months <- sort(unique(as.Date(nominal$date)))
  if (length(price_months) != length(nominal_months) ||
      any(price_months != nominal_months)) {
    difference <- setdiff(price_months, nominal_months)
    if (!length(difference)) difference <- setdiff(nominal_months, price_months)
    .ui_stop(
      "BEA price and nominal tables do not cover the same months; ",
      "first difference: ", format(difference[[1L]])
    )
  }
  if (min(price$date) != as.Date(sprintf("%d-01-01", start_year))) {
    .ui_stop("BEA inputs do not start in the requested start year.")
  }
  invisible(TRUE)
}

.ui_download_fred <- function(url, dest, series) {
  status <- tryCatch(
    suppressWarnings(
      utils::download.file(url, dest, mode = "wb", quiet = TRUE)
    ),
    error = function(e) {
      .ui_stop(
        "Failed to download FRED series ", series, ": ", conditionMessage(e)
      )
    }
  )
  if (!is.null(status) && !identical(as.integer(status), 0L)) {
    .ui_stop(
      "Failed to download FRED series ", series, " (status ", status, ")."
    )
  }
  info <- file.info(dest)
  if (!file.exists(dest) || is.na(info$size) || info$size <= 0) {
    .ui_stop("FRED series ", series, " downloaded as an empty file.")
  }
  invisible(dest)
}

.ui_fetch_fred_series <- function(series, dest, cache_reason = NULL) {
  url <- .ui_fred_url(series)
  tryCatch(
    .ui_download_fred(url, dest, series),
    error = function(e) {
      detail <- conditionMessage(e)
      if (!is.null(cache_reason) && nzchar(cache_reason)) {
        detail <- paste0(detail, " (cached copy was unusable: ", cache_reason, ")")
      }
      .ui_stop(detail)
    }
  )
  data <- .ui_parse_fred_file(
    dest, series,
    paste0("FRED series ", series)
  )
  floor <- .ui_latest_floor()
  if (max(data$date) < floor) {
    .ui_stop(
      "FRED series ", series, " ends on ", format(max(data$date)),
      ", earlier than the two-month freshness floor ", format(floor), "."
    )
  }
  data
}

.ui_join_dallas <- function(monthly, yoy) {
  monthly <- data.table::copy(monthly)
  yoy <- data.table::copy(yoy)
  data.table::setnames(monthly, "value", "dallas_monthly_annualized")
  data.table::setnames(yoy, "value", "dallas_yoy")

  if (any(!is.finite(monthly$dallas_monthly_annualized)) ||
      any(!is.finite(yoy$dallas_yoy))) {
    .ui_stop("Dallas FRED series contain non-finite values.")
  }
  if (anyDuplicated(monthly$date) || anyDuplicated(yoy$date)) {
    .ui_stop("Dallas FRED series contain duplicate dates.")
  }
  last <- min(max(monthly$date), max(yoy$date))
  if (max(monthly$date) != max(yoy$date)) {
    .ui_stop(
      "Dallas FRED series end on different months (",
      format(max(monthly$date)), " and ", format(max(yoy$date)),
      "); refusing to return a truncated join."
    )
  }
  first <- max(min(monthly$date), min(yoy$date))
  if (first > last) {
    .ui_stop("Dallas FRED series have no common monthly coverage.")
  }

  monthly <- monthly[date >= first & date <= last]
  yoy <- yoy[date >= first & date <= last]
  merged <- merge(monthly, yoy, by = "date", all = FALSE, sort = TRUE)
  expected <- seq(first, last, by = "month")
  same_months <- nrow(merged) == length(expected) &&
    !anyNA(merged$date) &&
    all(format(merged$date, "%Y-%m") == format(expected, "%Y-%m"))
  if (!same_months) {
    .ui_stop(
      "Dallas FRED series do not have complete common monthly coverage from ",
      format(first), " to ", format(last), "."
    )
  }
  if (anyNA(merged$dallas_monthly_annualized) || anyNA(merged$dallas_yoy)) {
    .ui_stop("Dallas join contains missing values.")
  }
  data.table::setcolorder(
    merged, c("date", "dallas_monthly_annualized", "dallas_yoy")
  )
  merged[]
}

#' Fetch and normalize the BEA price and nominal inputs.
#'
#' U20404 is returned as `price` and U20405 as `nominal`. Both are monthly long
#' tables with columns table, line_number, series_code, description, date, and
#' value. Cache chunks are reused when they are complete; current-year and
#' incomplete chunks are refetched automatically. A live BEA download needs
#' BEA_API_KEY or .bea_api_key.
#'
#' @param start_year First calendar year to return.
#' @param refresh If TRUE, refetch every cache chunk.
#' @return list(price, nominal), each a data.table.
fetch_bea_inputs <- function(start_year = 1987L, refresh = FALSE) {
  start_year <- .ui_validate_year(start_year, "start_year")
  refresh <- .ui_validate_refresh(refresh)
  end_year <- as.integer(format(Sys.Date(), "%Y"))
  if (start_year > end_year) {
    .ui_stop("`start_year` cannot be later than the current year.")
  }

  root <- .ui_project_root()
  raw_dir <- .ui_raw_dir(root)
  chunks <- .ui_year_chunks(start_year, end_year)
  stage_dir <- NULL
  stage_entries <- list()
  key <- NULL
  collected <- list()

  on.exit({
    if (!is.null(stage_dir)) {
      unlink(stage_dir, recursive = TRUE, force = TRUE)
    }
  }, add = TRUE)

  for (role in names(.ui_bea_tables)) {
    table <- .ui_bea_tables[[role]]
    parts <- vector("list", length(chunks))
    for (i in seq_along(chunks)) {
      years <- chunks[[i]]
      target <- .ui_bea_chunk_path(raw_dir, table, years)
      inspected <- NULL
      reason <- if (refresh) "refresh requested" else "cache missing"

      if (file.exists(target)) {
        inspected <- .ui_inspect_bea_cache(target, table, years)
        if (!refresh && identical(inspected$status, "ok")) {
          parts[[i]] <- inspected$data
          next
        }
        if (!refresh) reason <- inspected$reason
      }
      if (is.null(reason) || !nzchar(reason)) reason <- "cache unusable"

      if (is.null(key)) key <- .ui_read_key(root)
      fetched <- tryCatch(
        .ui_fetch_bea_chunk(table, years, key),
        error = function(e) {
          .ui_stop(
            "BEA fetch failed for table ", table, ", years ",
            min(years), "-", max(years), " (", reason, "): ",
            conditionMessage(e)
          )
        }
      )
      .ui_validate_bea_chunk_span(fetched, years, table)

      old_latest <- NA
      if (!is.null(inspected)) {
        if (!is.null(inspected$data) && nrow(inspected$data)) {
          old_latest <- max(inspected$data$date)
        } else if (!is.null(inspected$meta)) {
          old_latest <- suppressWarnings(
            as.Date(as.character(inspected$meta$latest_period))
          )
        }
      }
      new_latest <- max(fetched$date)
      if (!is.na(old_latest) && new_latest < old_latest) {
        .ui_stop(
          "BEA response for table ", table, ", years ", min(years), "-",
          max(years), " ends at ", format(new_latest),
          ", earlier than the cached ", format(old_latest),
          "; refusing to replace newer cache data."
        )
      }

      if (is.null(stage_dir)) stage_dir <- .ui_make_stage(raw_dir)
      meta <- .ui_make_meta(
        fetched,
        dataset = "BEA NIUnderlyingDetail",
        table = table,
        series = NULL,
        years = years,
        request = .ui_bea_request_text(table, years)
      )
      stage_entries[[length(stage_entries) + 1L]] <- .ui_stage_cache(
        fetched, meta, stage_dir, target
      )
      parts[[i]] <- fetched
    }

    combined <- data.table::rbindlist(parts, use.names = TRUE)
    collected[[role]] <- .ui_finalize_bea(
      combined, table, start_year, end_year,
      paste0("BEA table ", table)
    )
  }

  .ui_validate_bea_pair(collected$price, collected$nominal, start_year)
  .ui_commit_cache(stage_entries, raw_dir)
  list(price = collected$price, nominal = collected$nominal)
}

#' Fetch and join the Dallas Fed trimmed-mean PCE series from FRED.
#'
#' Returns monthly date, dallas_monthly_annualized (PCETRIM1M158SFRBDAL), and
#' dallas_yoy (PCETRIM12M159SFRBDAL). Values are percentage points as published.
#' Because the 1-month series starts before the 12-month series, the result
#' begins at their first common month. Both series must have the same latest
#' month and complete common monthly coverage.
#'
#' @param refresh If TRUE, redownload both FRED series.
#' @return A data.table with columns date, dallas_monthly_annualized,
#'   dallas_yoy.
fetch_dallas_data <- function(refresh = FALSE) {
  refresh <- .ui_validate_refresh(refresh)
  root <- .ui_project_root()
  raw_dir <- .ui_raw_dir(root)
  stage_dir <- NULL
  stage_entries <- list()
  parts <- list()

  on.exit({
    if (!is.null(stage_dir)) {
      unlink(stage_dir, recursive = TRUE, force = TRUE)
    }
  }, add = TRUE)

  for (name in names(.ui_fred_series)) {
    series <- .ui_fred_series[[name]]
    target <- .ui_fred_cache_path(raw_dir, series)
    inspected <- NULL
    reason <- if (refresh) "refresh requested" else "cache missing"

    if (file.exists(target)) {
      inspected <- .ui_inspect_fred_cache(target, series)
      if (!refresh && identical(inspected$status, "ok")) {
        parts[[name]] <- inspected$data
        next
      }
      if (!refresh) reason <- inspected$reason
    }
    if (is.null(reason) || !nzchar(reason)) reason <- "cache unusable"

    if (is.null(stage_dir)) stage_dir <- .ui_make_stage(raw_dir)
    staged_csv <- file.path(stage_dir, basename(target))
    fetched <- .ui_fetch_fred_series(series, staged_csv, reason)

    old_latest <- NA
    if (!is.null(inspected)) {
      if (!is.null(inspected$data) && nrow(inspected$data)) {
        old_latest <- max(inspected$data$date)
      } else if (!is.null(inspected$meta)) {
        old_latest <- suppressWarnings(
          as.Date(as.character(inspected$meta$latest_period))
        )
      }
    }
    new_latest <- max(fetched$date)
    if (!is.na(old_latest) && new_latest < old_latest) {
      .ui_stop(
        "FRED series ", series, " ends at ", format(new_latest),
        ", earlier than the cached ", format(old_latest),
        "; refusing to replace newer cache data."
      )
    }

    meta <- .ui_make_meta(
      fetched,
      dataset = "FRED",
      table = NULL,
      series = series,
      years = sort(unique(as.integer(format(fetched$date, "%Y")))),
      request = paste0("GET ", .ui_fred_url(series))
    )
    stage_entries[[length(stage_entries) + 1L]] <- .ui_stage_cache(
      fetched, meta, stage_dir, target
    )
    parts[[name]] <- fetched
  }

  joined <- .ui_join_dallas(
    parts[["dallas_monthly_annualized"]],
    parts[["dallas_yoy"]]
  )
  .ui_commit_cache(stage_entries, raw_dir)
  joined[]
}
