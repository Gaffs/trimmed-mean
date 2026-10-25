# BEA and FRED ingestion for the trimmed-mean project.
#
# Public entry points:
#   fetch_bea_table(table, start_year = 2016, end_year = current, refresh = FALSE)
#   fetch_bea_inputs(start_year = 2016, refresh = FALSE)
#   fetch_dallas_benchmark(refresh = FALSE)
#   write_data_manifest(price, nominal, dallas, path = "data/raw/manifest.csv")
#
# Requires data.table. Live BEA downloads also require bea.R. Cache hashes use
# digest's SHA-256 when digest is installed and fall back to tools::md5sum.
# BEA_API_KEY overrides the single key stored in .bea_api_key at the project
# root. Keys are never printed or written to cache metadata.
#
# BEA values keep their published units: U20404 is a seasonally adjusted
# monthly price index (2017 = 100) and U20405 is seasonally adjusted monthly
# nominal PCE in millions of dollars at annual rates. Both outputs are long
# tables with one row per table/line/series/month.
#
# Cache layout: every data file under data/raw has a sidecar at
# <file>.meta. The sidecar records the request, the actual fetch time (or the
# file mtime when a legacy file was backfilled), first/latest period, row
# count, and a SHA-256 hash (MD5 fallback when digest is unavailable). Chunks
# covering the current or prior calendar year are refetched when their latest
# period is behind the most recent plausible monthly release; older chunks
# stay cached. refresh = TRUE refetches every chunk.
#
# Refreshes are staged under data/raw/.bea-stage-* and only replace the cache
# after every requested chunk for the table has been fetched and validated.
# fetch_bea_inputs stages both tables and commits only after both succeed.

.bea_chunk_width <- 5L
.bea_fred_series <- "PCETRIM12M159SFRBDAL"
.bea_fred_url <- paste0(
  "https://fred.stlouisfed.org/graph/fredgraph.csv?id=",
  .bea_fred_series
)
.bea_missing_tokens <- c(
  "", ".", "..", "...", ".....", "NA", "N/A", "N.A.", "(D)", "(NA)", "--", "\u2014"
)
.bea_meta_format <- "bea-cache-meta-v1"

.bea_project_root <- function() {
  root <- Sys.getenv("BEA_PROJECT_ROOT", unset = "")
  if (!nzchar(root)) {
    root <- getOption("bea.project_root", "")
    if (is.null(root)) root <- ""
  }
  if (!nzchar(root)) {
    wd <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
    root <- wd
    markers <- c(".bea_api_key", "R/bea_data.R", ".git")
    if (!any(file.exists(file.path(wd, markers)))) {
      cur <- wd
      repeat {
        parent <- dirname(cur)
        if (identical(parent, cur)) break
        cur <- parent
        if (any(file.exists(file.path(cur, markers)))) {
          root <- cur
          break
        }
      }
    }
  }
  if (!dir.exists(root)) {
    stop("Project root does not exist: ", root, call. = FALSE)
  }
  normalizePath(root, winslash = "/", mustWork = TRUE)
}

.bea_raw_dir <- function() {
  d <- file.path(.bea_project_root(), "data", "raw")
  if (!dir.exists(d)) {
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
    if (!dir.exists(d)) {
      stop("Could not create data directory: ", d, call. = FALSE)
    }
  }
  d
}

.bea_resolve_path <- function(path) {
  if (!is.character(path) || length(path) != 1L ||
      is.na(path) || !nzchar(path)) {
    stop("`path` must be a single non-empty string.", call. = FALSE)
  }
  path <- path.expand(path)
  if (grepl("^([A-Za-z]:[/\\\\]|[/\\\\])", path)) {
    return(path)
  }
  file.path(.bea_project_root(), path)
}

.bea_redact <- function(text, key = NULL) {
  text <- paste(as.character(text), collapse = " ")
  if (!is.null(key) && nzchar(key)) {
    text <- gsub(key, "[REDACTED]", text, fixed = TRUE)
  }
  text
}

.bea_short <- function(text, max_chars = 800L) {
  text <- paste(as.character(text), collapse = " ")
  if (nchar(text) > max_chars) {
    paste0(substr(text, 1L, max_chars), "...")
  } else {
    text
  }
}

.bea_now_text <- function(time = Sys.time()) {
  format(time, "%Y-%m-%dT%H:%M:%S%z")
}

.bea_parse_time <- function(text) {
  if (length(text) != 1L || is.na(text) || !nzchar(trimws(text))) {
    return(as.POSIXct(NA_character_, tz = "UTC"))
  }
  tryCatch(
    as.POSIXct(trimws(text), format = "%Y-%m-%dT%H:%M:%S%z", tz = "UTC"),
    error = function(e) as.POSIXct(NA_character_, tz = "UTC")
  )
}

.bea_expected_latest_period <- function(now = Sys.Date()) {
  month_start <- as.Date(format(now, "%Y-%m-01"))
  seq(month_start, by = "-1 month", length.out = 2L)[2L]
}

.bea_chunk_recent <- function(years, now = Sys.Date()) {
  current_year <- as.integer(format(now, "%Y"))
  current_year %in% years || (current_year - 1L) %in% years
}

.bea_chunk_expected_latest <- function(years, now = Sys.Date()) {
  max_year <- max(years)
  if (max_year < as.integer(format(now, "%Y"))) {
    return(as.Date(sprintf("%d-12-01", max_year)))
  }
  .bea_expected_latest_period(now)
}

.bea_validate_refresh <- function(refresh) {
  if (!is.logical(refresh) || length(refresh) != 1L || is.na(refresh)) {
    stop("`refresh` must be TRUE or FALSE.", call. = FALSE)
  }
  refresh
}

.bea_assert_monthly_periods <- function(x, what) {
  if (inherits(x, "Date")) {
    dates <- as.Date(x)
    bad <- is.na(dates) | format(dates, "%d") != "01"
    if (any(bad)) {
      stop(
        what, " contains nonmonthly dates (expected the first of a month).",
        call. = FALSE
      )
    }
    return(invisible(TRUE))
  }

  text <- toupper(trimws(as.character(x)))
  monthly <- !is.na(text) & grepl("^[0-9]{4}M[0-9]{1,2}$", text)
  bad <- !monthly
  if (any(monthly)) {
    month <- suppressWarnings(
      as.integer(sub("^[0-9]{4}M", "", text[monthly]))
    )
    bad_month <- is.na(month) | month < 1L | month > 12L
    if (any(bad_month)) {
      bad[which(monthly)[bad_month]] <- TRUE
    }
  }
  if (any(bad)) {
    examples <- unique(text[bad])
    examples <- examples[seq_len(min(5L, length(examples)))]
    stop(
      what, " contains nonmonthly periods; expected YYYYMmm values ",
      "(for example 2026M08), got: ",
      paste(examples, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.bea_date_column <- function(x, label, suffix = "") {
  if (!inherits(x, "Date") && !is.character(x) && !is.factor(x)) {
    stop(
      label, " has an unsupported date column type (",
      paste(class(x), collapse = "/"), ").", suffix,
      call. = FALSE
    )
  }
  dates <- tryCatch(
    if (inherits(x, "Date")) {
      as.Date(x)
    } else {
      as.Date(trimws(as.character(x)))
    },
    error = function(e) NULL
  )
  if (is.null(dates) || anyNA(dates)) {
    stop(label, " contains missing or invalid dates.", suffix, call. = FALSE)
  }
  dates
}

.bea_read_key <- function() {
  key <- trimws(Sys.getenv("BEA_API_KEY", unset = ""))
  if (!nzchar(key)) {
    key_file <- file.path(.bea_project_root(), ".bea_api_key")
    if (!file.exists(key_file)) {
      stop(
        "BEA API key not found. Set BEA_API_KEY or create ", key_file,
        " containing exactly one key.",
        call. = FALSE
      )
    }
    lines <- tryCatch(
      readLines(key_file, warn = FALSE, encoding = "UTF-8"),
      error = function(e) {
        stop(
          "Could not read BEA key file ", key_file, ": ",
          conditionMessage(e),
          call. = FALSE
        )
      }
    )
    lines <- sub("^\ufeff", "", trimws(lines))
    lines <- lines[nzchar(lines) & !startsWith(lines, "#")]
    if (length(lines) == 0L) {
      stop("BEA key file ", key_file, " is empty.", call. = FALSE)
    }
    if (length(lines) > 1L) {
      stop(
        "BEA key file ", key_file,
        " must contain exactly one key; found ", length(lines),
        " non-empty lines.",
        call. = FALSE
      )
    }
    key <- lines[[1L]]
  }
  key <- sub("^\ufeff", "", trimws(key))
  if (nchar(key) >= 2L) {
    first <- substr(key, 1L, 1L)
    last <- substr(key, nchar(key), nchar(key))
    if (first == last && first %in% c("\"", "'")) {
      key <- substr(key, 2L, nchar(key) - 1L)
    }
  }
  key <- gsub("[[:space:]]", "", key)
  if (nchar(key) != 36L) {
    stop(
      "BEA API key is invalid: expected 36 characters (key not shown). ",
      "Check BEA_API_KEY or .bea_api_key.",
      call. = FALSE
    )
  }
  key
}

.bea_table_id <- function(table) {
  if (length(table) != 1L || is.na(table)) {
    stop("`table` must be a single BEA table name (for example U20404).",
         call. = FALSE)
  }
  id <- toupper(trimws(as.character(table)))
  if (!nzchar(id) || !grepl("^[A-Z0-9]+$", id)) {
    stop("`table` must contain only letters and digits (for example U20404).",
         call. = FALSE)
  }
  id
}

.bea_year <- function(x, name) {
  if (length(x) != 1L || is.na(x)) {
    stop("`", name, "` must be a single year.", call. = FALSE)
  }
  value <- suppressWarnings(as.numeric(x))
  if (is.na(value) || value != floor(value) ||
      value < 1900 || value > 2200) {
    stop("`", name, "` must be an integer year between 1900 and 2200.",
         call. = FALSE)
  }
  as.integer(value)
}

.bea_year_chunks <- function(start_year, end_year) {
  starts <- seq.int(start_year, end_year, by = .bea_chunk_width)
  chunks <- lapply(starts, function(start) {
    seq.int(start, min(start + .bea_chunk_width - 1L, end_year))
  })
  # Avoid a one- or two-year tail: re-split the final two chunks into pieces
  # of at least three years each. Every chunk stays within the 3-5 year range.
  n_chunks <- length(chunks)
  if (n_chunks >= 2L && length(chunks[[n_chunks]]) < 3L) {
    combined <- c(chunks[[n_chunks - 1L]], chunks[[n_chunks]])
    midpoint <- floor(length(combined) / 2L)
    chunks[[n_chunks - 1L]] <- combined[seq_len(midpoint)]
    chunks[[n_chunks]] <- combined[(midpoint + 1L):length(combined)]
  }
  chunks
}

.bea_parse_value <- function(x, what = "DataValue") {
  if (is.numeric(x)) {
    out <- as.numeric(x)
    out[is.nan(out)] <- NA_real_
    return(out)
  }
  text <- trimws(as.character(x))
  text <- gsub(",", "", text, fixed = TRUE)
  text[toupper(text) %in% .bea_missing_tokens] <- NA_character_
  out <- suppressWarnings(as.numeric(text))
  bad <- is.na(out) & !is.na(text)
  if (any(bad)) {
    examples <- unique(text[bad])
    examples <- examples[seq_len(min(5L, length(examples)))]
    stop(
      "Could not parse ", what, " as numeric; examples: ",
      paste(examples, collapse = ", "),
      call. = FALSE
    )
  }
  out
}

.bea_parse_period <- function(x, what = "TimePeriod") {
  if (inherits(x, "Date")) {
    return(as.Date(x))
  }
  text <- toupper(trimws(as.character(x)))
  out <- rep(as.Date(NA_character_), length(text))

  annual <- grepl("^[0-9]{4}$", text)
  if (any(annual)) {
    out[annual] <- as.Date(
      paste0(text[annual], "-01-01"),
      format = "%Y-%m-%d"
    )
  }

  monthly <- grepl("^[0-9]{4}M[0-9]{1,2}$", text)
  if (any(monthly)) {
    year <- substr(text[monthly], 1L, 4L)
    month <- suppressWarnings(
      as.integer(sub("^[0-9]{4}M", "", text[monthly]))
    )
    ok <- !is.na(month) & month >= 1L & month <= 12L
    idx <- which(monthly)[ok]
    out[idx] <- as.Date(
      sprintf("%s-%02d-01", year[ok], month[ok]),
      format = "%Y-%m-%d"
    )
  }

  quarterly <- grepl("^[0-9]{4}Q[1-4]$", text)
  if (any(quarterly)) {
    year <- substr(text[quarterly], 1L, 4L)
    quarter <- as.integer(sub("^[0-9]{4}Q", "", text[quarterly]))
    out[quarterly] <- as.Date(
      sprintf("%s-%02d-01", year, (quarter - 1L) * 3L + 1L),
      format = "%Y-%m-%d"
    )
  }

  bad <- is.na(out)
  if (any(bad)) {
    examples <- unique(text[bad])
    examples <- examples[seq_len(min(5L, length(examples)))]
    stop(
      "Could not parse ", what, " as dates; examples: ",
      paste(examples, collapse = ", "),
      call. = FALSE
    )
  }
  out
}

.bea_find_column <- function(columns, candidates) {
  hit <- match(tolower(candidates), tolower(columns))
  hit <- hit[!is.na(hit)]
  if (!length(hit)) return(NA_character_)
  columns[hit[1L]]
}

# Reject duplicate line-date rows unless the duplicates are byte-for-byte
# identical. Exact duplicates are collapsed; differing values, series codes,
# or descriptions are a provenance conflict and stop the ingestion.
.bea_check_duplicate_rows <- function(dt, context) {
  keys <- c("table", "line_number", "date")
  if (nrow(dt) == 0L) return(dt)
  flag <- duplicated(dt, by = keys) | duplicated(dt, by = keys, fromLast = TRUE)
  if (!any(flag)) return(dt)

  dup <- dt[flag]
  value_cols <- setdiff(names(dt), keys)
  variants <- dup[, .(
    variants = nrow(unique(.SD))
  ), by = c("table", "line_number", "date"), .SDcols = value_cols]
  conflict <- variants[variants > 1L]
  if (nrow(conflict)) {
    first <- conflict[1L]
    detail <- dt[
      table == first$table &
        line_number == first$line_number &
        date == first$date,
      value_cols,
      with = FALSE
    ]
    example <- paste(
      apply(as.data.frame(detail), 1L, function(row) {
        paste(names(row), row, sep = "=", collapse = "; ")
      }),
      collapse = " | "
    )
    stop(
      context, " contains conflicting duplicate line-date rows (line ",
      first$line_number, " on ", format(first$date), "; ",
      .bea_short(example), ").",
      call. = FALSE
    )
  }
  unique(dt)
}

.bea_normalize_response <- function(raw, table, years) {
  if (!is.data.frame(raw)) {
    stop(
      "BEA response is not tabular (class: ",
      paste(class(raw), collapse = "/"),
      ").",
      call. = FALSE
    )
  }
  dt <- data.table::as.data.table(raw)
  columns <- names(dt)

  line_col <- .bea_find_column(columns, "LineNumber")
  time_col <- .bea_find_column(columns, c("TimePeriod", "Year"))
  value_col <- .bea_find_column(columns, "DataValue")
  missing <- c(
    if (is.na(line_col)) "LineNumber",
    if (is.na(time_col)) "TimePeriod/Year",
    if (is.na(value_col)) "DataValue"
  )
  if (length(missing)) {
    stop(
      "BEA response for table ", table,
      " is missing required column(s): ", paste(missing, collapse = ", "),
      ". Columns returned: ", paste(columns, collapse = ", "),
      call. = FALSE
    )
  }

  .bea_assert_monthly_periods(
    dt[[time_col]],
    paste0("BEA response for table ", table)
  )

  series_col <- .bea_find_column(columns, c("SeriesCode", "SeriesID"))
  desc_col <- .bea_find_column(columns, c("LineDescription", "Description"))

  line_number <- suppressWarnings(as.integer(trimws(as.character(dt[[line_col]]))))
  if (anyNA(line_number)) {
    bad <- unique(trimws(as.character(dt[[line_col]]))[is.na(line_number)])
    bad <- bad[seq_len(min(5L, length(bad)))]
    stop(
      "BEA response for table ", table,
      " contains missing or non-integer LineNumber values; examples: ",
      paste(bad, collapse = ", "),
      call. = FALSE
    )
  }

  series_code <- if (is.na(series_col)) {
    rep(NA_character_, nrow(dt))
  } else {
    trimws(as.character(dt[[series_col]]))
  }
  series_code[is.na(series_code) | !nzchar(series_code)] <- NA_character_

  description <- if (is.na(desc_col)) {
    rep(NA_character_, nrow(dt))
  } else {
    trimws(as.character(dt[[desc_col]]))
  }
  description[is.na(description) | !nzchar(description)] <- NA_character_

  out <- data.table::data.table(
    table = rep(table, nrow(dt)),
    line_number = line_number,
    series_code = series_code,
    description = description,
    date = .bea_parse_period(dt[[time_col]], "TimePeriod"),
    value = .bea_parse_value(dt[[value_col]], "DataValue")
  )

  out <- out[
    as.integer(format(date, "%Y")) >= min(years) &
      as.integer(format(date, "%Y")) <= max(years)
  ]
  if (nrow(out) == 0L) {
    stop(
      "BEA response for table ", table, " has no observations in years ",
      min(years), "-", max(years), ".",
      call. = FALSE
    )
  }

  out <- .bea_check_duplicate_rows(
    out,
    paste0("BEA response for table ", table)
  )
  data.table::setorder(out, line_number, date, series_code, na.last = TRUE)
  out[]
}

.bea_read_chunk <- function(path, table, years) {
  dt <- tryCatch(
    data.table::fread(path, data.table = TRUE, na.strings = c("NA", "")),
    error = function(e) {
      stop(
        "Could not read cached BEA chunk ", path, ": ", conditionMessage(e),
        ". Delete it or rerun with refresh = TRUE.",
        call. = FALSE
      )
    }
  )
  required <- c(
    "table", "line_number", "series_code", "description", "date", "value"
  )
  missing <- setdiff(required, names(dt))
  if (length(missing)) {
    stop(
      "Cached BEA chunk ", path, " is invalid; missing column(s): ",
      paste(missing, collapse = ", "),
      ". Delete it or rerun with refresh = TRUE.",
      call. = FALSE
    )
  }
  if (nrow(dt) == 0L) {
    stop(
      "Cached BEA chunk ", path,
      " is empty. Delete it or rerun with refresh = TRUE.",
      call. = FALSE
    )
  }
  if (!all(as.character(dt$table) == table) || anyNA(as.character(dt$table))) {
    stop(
      "Cached BEA chunk ", path, " contains a table other than ", table,
      ". Delete it or rerun with refresh = TRUE.",
      call. = FALSE
    )
  }

  line_number <- suppressWarnings(as.integer(dt$line_number))
  if (anyNA(line_number)) {
    stop(
      "Cached BEA chunk ", path, " has invalid line_number values. ",
      "Delete it or rerun with refresh = TRUE.",
      call. = FALSE
    )
  }

  dates <- .bea_date_column(
    dt$date,
    paste0("Cached BEA chunk ", path),
    " Delete it or rerun with refresh = TRUE."
  )
  if (any(format(dates, "%d") != "01")) {
    stop(
      "Cached BEA chunk ", path,
      " has dates that are not the first of a month. ",
      "Delete it or rerun with refresh = TRUE.",
      call. = FALSE
    )
  }

  series_code <- trimws(as.character(dt$series_code))
  series_code[is.na(series_code) | !nzchar(series_code)] <- NA_character_
  description <- trimws(as.character(dt$description))
  description[is.na(description) | !nzchar(description)] <- NA_character_

  out <- data.table::data.table(
    table = rep(table, nrow(dt)),
    line_number = line_number,
    series_code = series_code,
    description = description,
    date = dates,
    value = .bea_parse_value(
      dt$value,
      what = paste0("value in cached chunk ", path)
    )
  )

  out <- out[
    as.integer(format(date, "%Y")) >= min(years) &
      as.integer(format(date, "%Y")) <= max(years)
  ]
  if (nrow(out) == 0L) {
    stop(
      "Cached BEA chunk ", path, " has no rows in years ",
      min(years), "-", max(years), ". Delete it or rerun with refresh = TRUE.",
      call. = FALSE
    )
  }

  out <- .bea_check_duplicate_rows(
    out,
    paste0("Cached BEA chunk ", path)
  )
  data.table::setorder(out, line_number, date, series_code, na.last = TRUE)
  out[]
}

.bea_sidecar_path <- function(path) {
  paste0(path, ".meta")
}

.bea_file_hash <- function(path) {
  sha256 <- NA_character_
  if (requireNamespace("digest", quietly = TRUE)) {
    sha256 <- tryCatch(
      unname(digest::digest(file = path, algo = "sha256")),
      error = function(e) NA_character_
    )
  }
  if (!is.na(sha256) && nzchar(sha256)) {
    return(list(algo = "sha256", sha256 = sha256, md5 = NA_character_))
  }
  md5 <- tryCatch(
    unname(tools::md5sum(path)),
    error = function(e) NA_character_
  )
  list(
    algo = if (is.na(md5)) "none" else "md5",
    sha256 = NA_character_,
    md5 = md5
  )
}

.bea_meta_value <- function(x) {
  if (length(x) == 0L) return("")
  x <- x[!is.na(x)]
  if (!length(x)) return("")
  paste(as.character(x), collapse = ",")
}

.bea_write_meta <- function(meta, path) {
  fields <- c(
    "format", "dataset", "table", "series", "frequency", "years",
    "request", "fetched_at", "first_period", "latest_period", "row_count",
    "hash_algo", "sha256", "md5", "metadata_source", "fetched_at_source"
  )
  values <- vapply(fields, function(field) {
    value <- if (identical(field, "fetched_at")) {
      meta$fetched_at_text
    } else {
      meta[[field]]
    }
    .bea_meta_value(value)
  }, character(1L))
  if (any(grepl("[\r\n]", values))) {
    stop("Cache metadata contains a line break.", call. = FALSE)
  }

  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(
    paste0(basename(path), "."),
    tmpdir = dirname(path),
    fileext = ".tmp"
  )
  on.exit(unlink(tmp), add = TRUE)
  lines <- paste0(fields, ": ", values)
  tryCatch(
    writeLines(lines, tmp, useBytes = TRUE),
    error = function(e) {
      stop("Could not write cache metadata ", path, ": ",
           conditionMessage(e), call. = FALSE)
    }
  )
  if (!file.copy(tmp, path, overwrite = TRUE)) {
    stop("Could not replace cache metadata ", path, call. = FALSE)
  }
  invisible(path)
}

.bea_meta_date <- function(text, what) {
  if (is.na(text) || !nzchar(trimws(text))) {
    return(as.Date(NA_character_))
  }
  value <- suppressWarnings(as.Date(trimws(text)))
  if (is.na(value)) {
    stop("Cache metadata has an invalid ", what, ": ", text, call. = FALSE)
  }
  value
}

.bea_read_meta <- function(path) {
  if (!file.exists(path)) return(NULL)
  raw <- tryCatch(
    read.dcf(path, all = TRUE),
    error = function(e) {
      stop("Could not parse cache metadata ", path, ": ",
           conditionMessage(e), call. = FALSE)
    }
  )
  if (is.null(raw) || nrow(raw) != 1L) {
    stop("Cache metadata ", path, " is empty or malformed.", call. = FALSE)
  }
  values <- as.list(raw[1L, ])
  names(values) <- colnames(raw)
  required <- c(
    "format", "dataset", "table", "series", "frequency", "years",
    "request", "fetched_at", "first_period", "latest_period", "row_count",
    "hash_algo", "metadata_source", "fetched_at_source"
  )
  missing <- setdiff(required, names(values))
  if (length(missing)) {
    stop(
      "Cache metadata ", path, " is missing field(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  years_text <- trimws(values$years)
  years <- if (nzchar(years_text)) {
    suppressWarnings(as.integer(strsplit(years_text, ",", fixed = TRUE)[[1L]]))
  } else {
    integer()
  }
  if (anyNA(years)) {
    stop("Cache metadata ", path, " has invalid years.", call. = FALSE)
  }

  row_count <- suppressWarnings(as.integer(trimws(values$row_count)))
  if (is.na(row_count)) {
    stop("Cache metadata ", path, " has an invalid row_count.", call. = FALSE)
  }

  fetched_at <- .bea_parse_time(values$fetched_at)
  if (is.na(fetched_at)) {
    stop("Cache metadata ", path, " has an invalid fetched_at.", call. = FALSE)
  }

  list(
    format = values$format,
    dataset = values$dataset,
    table = values$table,
    series = values$series,
    frequency = values$frequency,
    years = years,
    request = values$request,
    fetched_at = fetched_at,
    fetched_at_text = values$fetched_at,
    first_period = .bea_meta_date(values$first_period, "first_period"),
    latest_period = .bea_meta_date(values$latest_period, "latest_period"),
    row_count = row_count,
    hash_algo = values$hash_algo,
    sha256 = if ("sha256" %in% names(values)) values$sha256 else "",
    md5 = if ("md5" %in% names(values)) values$md5 else "",
    metadata_source = values$metadata_source,
    fetched_at_source = values$fetched_at_source
  )
}

.bea_make_meta <- function(path, data, dataset, table = NULL, series = NULL,
                           years = integer(), request, fetched_at_text,
                           metadata_source, fetched_at_source) {
  dates <- .bea_date_column(
    data$date,
    paste0("Cache data for ", basename(path))
  )
  if (nrow(data) == 0L) {
    stop("Cannot write metadata for an empty data set.", call. = FALSE)
  }
  hash <- .bea_file_hash(path)
  list(
    format = .bea_meta_format,
    dataset = dataset,
    table = if (is.null(table)) "" else table,
    series = if (is.null(series)) "" else series,
    frequency = "M",
    years = as.integer(years),
    request = request,
    fetched_at = .bea_parse_time(fetched_at_text),
    fetched_at_text = fetched_at_text,
    first_period = min(dates),
    latest_period = max(dates),
    row_count = nrow(data),
    hash_algo = hash$algo,
    sha256 = hash$sha256,
    md5 = hash$md5,
    metadata_source = metadata_source,
    fetched_at_source = fetched_at_source
  )
}

.bea_meta_issues <- function(meta, path, dataset, table = NULL, series = NULL,
                             years = NULL, data) {
  issues <- character()
  add <- function(text) issues <<- c(issues, text)

  if (is.null(meta) || !is.list(meta)) {
    return("metadata is missing")
  }
  if (!identical(meta$format, .bea_meta_format)) {
    add("unsupported metadata format")
  }
  if (!identical(meta$dataset, dataset)) {
    add("dataset does not match the cache filename")
  }
  if (!is.null(table) && !identical(meta$table, table)) {
    add("table does not match the cache filename")
  }
  if (!is.null(series) && !identical(meta$series, series)) {
    add("series does not match the cache filename")
  }
  if (!identical(meta$frequency, "M")) {
    add("frequency is not monthly")
  }
  if (!is.null(years) &&
      !identical(as.integer(meta$years), as.integer(years))) {
    add("requested years do not match the cache filename")
  }
  if (is.na(meta$row_count) || meta$row_count != nrow(data)) {
    add("row_count does not match the cache file")
  }

  dates <- as.Date(data$date)
  if (!length(dates) || anyNA(dates)) {
    add("cache data has invalid dates")
  } else {
    if (is.na(meta$first_period) ||
        as.numeric(meta$first_period) != as.numeric(min(dates))) {
      add("first_period does not match the cache file")
    }
    if (is.na(meta$latest_period) ||
        as.numeric(meta$latest_period) != as.numeric(max(dates))) {
      add("latest_period does not match the cache file")
    }
  }

  if (identical(meta$hash_algo, "sha256") &&
      !is.na(meta$sha256) && nzchar(meta$sha256)) {
    if (!requireNamespace("digest", quietly = TRUE)) {
      add("cannot verify SHA-256 metadata without digest")
    } else {
      actual <- tryCatch(
        unname(digest::digest(file = path, algo = "sha256")),
        error = function(e) NA_character_
      )
      if (is.na(actual)) {
        add("could not hash the cache file")
      } else if (!identical(tolower(actual), tolower(meta$sha256))) {
        add("SHA-256 does not match the cache file")
      }
    }
  } else if (identical(meta$hash_algo, "md5") &&
             !is.na(meta$md5) && nzchar(meta$md5)) {
    actual <- tryCatch(
      unname(tools::md5sum(path)),
      error = function(e) NA_character_
    )
    if (is.na(actual)) {
      add("could not hash the cache file")
    } else if (!identical(tolower(actual), tolower(meta$md5))) {
      add("MD5 does not match the cache file")
    }
  } else {
    add("no verifiable file hash is recorded")
  }

  if (is.na(meta$fetched_at)) {
    add("fetched_at is invalid")
  } else if (as.numeric(difftime(meta$fetched_at, Sys.time(), units = "secs")) >
             86400) {
    add("fetched_at is in the future")
  }
  if (is.na(meta$metadata_source) || !nzchar(meta$metadata_source)) {
    add("metadata_source is missing")
  }
  if (is.na(meta$fetched_at_source) || !nzchar(meta$fetched_at_source)) {
    add("fetched_at_source is missing")
  }
  if (is.na(meta$request) || !nzchar(meta$request)) {
    add("request is missing")
  } else {
    if (!is.null(table) &&
        !grepl(paste0("TableName=", table), meta$request, fixed = TRUE)) {
      add("request table does not match the cache filename")
    }
    if (!is.null(years) &&
        !grepl(
          paste0("Year=", paste(years, collapse = ",")),
          meta$request,
          fixed = TRUE
        )) {
      add("request years do not match the cache filename")
    }
    if (!is.null(series) &&
        !grepl(series, meta$request, fixed = TRUE)) {
      add("request series does not match the cache filename")
    }
  }
  unique(issues)
}

.bea_backfill_meta <- function(path, data, dataset, table = NULL,
                               series = NULL, years = integer(),
                               write = FALSE) {
  mtime <- file.info(path)$mtime
  if (length(mtime) != 1L || is.na(mtime)) mtime <- Sys.time()
  request <- if (identical(dataset, "FRED")) {
    paste0("reconstructed from cache filename; GET ", .bea_fred_url)
  } else {
    paste0(
      "reconstructed from cache filename; ",
      .bea_request_text(table, years)
    )
  }
  meta <- .bea_make_meta(
    path, data,
    dataset = dataset,
    table = table,
    series = series,
    years = years,
    request = request,
    fetched_at_text = .bea_now_text(mtime),
    metadata_source = "backfill:file_mtime",
    fetched_at_source = "file_mtime"
  )
  meta$backfill_needed <- TRUE
  meta$sidecar_written <- FALSE
  if (isTRUE(write)) {
    meta$sidecar_written <- tryCatch({
      .bea_write_meta(meta, .bea_sidecar_path(path))
      TRUE
    }, error = function(e) FALSE)
  }
  meta
}

.bea_write_backfills <- function(backfills) {
  for (item in backfills) {
    try({
      .bea_write_meta(item$meta, .bea_sidecar_path(item$path))
      item$meta$sidecar_written <- TRUE
    }, silent = TRUE)
  }
  invisible(NULL)
}

# A recent chunk is fresh only when it covers the last calendar month whose
# data could plausibly have been published. A chunk ending in an earlier year
# is complete once it reaches December of that year.
.bea_meta_is_fresh <- function(meta, years, now = Sys.Date()) {
  if (!.bea_chunk_recent(years, now)) return(TRUE)
  expected <- .bea_chunk_expected_latest(years, now)
  !is.na(meta$latest_period) && meta$latest_period >= expected
}

.bea_inspect_cache <- function(path, table, years, now = Sys.Date()) {
  if (!file.exists(path)) {
    return(list(status = "missing", reason = "cache file does not exist"))
  }

  read_error <- NULL
  data <- tryCatch(
    .bea_read_chunk(path, table, years),
    error = function(e) {
      read_error <<- conditionMessage(e)
      NULL
    }
  )
  if (is.null(data)) {
    return(list(status = "invalid", reason = read_error))
  }

  meta_path <- .bea_sidecar_path(path)
  if (!file.exists(meta_path)) {
    meta <- .bea_backfill_meta(
      path, data,
      dataset = "BEA NIUnderlyingDetail",
      table = table,
      years = years
    )
    return(list(status = "ok", data = data, meta = meta, reason = NULL))
  }

  meta <- tryCatch(
    .bea_read_meta(meta_path),
    error = function(e) NULL
  )
  if (is.null(meta)) {
    return(list(
      status = "invalid",
      reason = "cache metadata is unreadable",
      data = data
    ))
  }
  issues <- .bea_meta_issues(
    meta, path,
    dataset = "BEA NIUnderlyingDetail",
    table = table,
    years = years,
    data = data
  )
  if (length(issues)) {
    return(list(
      status = "invalid",
      reason = paste(issues, collapse = "; "),
      data = data
    ))
  }
  list(status = "ok", data = data, meta = meta, reason = NULL)
}

.bea_inspect_fred_cache <- function(path) {
  if (!file.exists(path)) {
    return(list(status = "missing", reason = "cache file does not exist"))
  }

  read_error <- NULL
  data <- tryCatch(
    .bea_parse_fred(path, paste0("Cached FRED file ", path)),
    error = function(e) {
      read_error <<- conditionMessage(e)
      NULL
    }
  )
  if (is.null(data)) {
    return(list(status = "invalid", reason = read_error))
  }

  meta_path <- .bea_sidecar_path(path)
  if (!file.exists(meta_path)) {
    meta <- .bea_backfill_meta(
      path, data,
      dataset = "FRED",
      series = .bea_fred_series,
      years = sort(unique(as.integer(format(data$date, "%Y")))),
      write = TRUE
    )
    return(list(status = "ok", data = data, meta = meta, reason = NULL))
  }

  meta <- tryCatch(
    .bea_read_meta(meta_path),
    error = function(e) NULL
  )
  if (is.null(meta)) {
    return(list(
      status = "invalid",
      reason = "cache metadata is unreadable",
      data = data
    ))
  }
  issues <- .bea_meta_issues(
    meta, path,
    dataset = "FRED",
    series = .bea_fred_series,
    data = data
  )
  if (length(issues)) {
    return(list(
      status = "invalid",
      reason = paste(issues, collapse = "; "),
      data = data
    ))
  }
  list(status = "ok", data = data, meta = meta, reason = NULL)
}

.bea_request_text <- function(table, years) {
  paste0(
    "Method=GetData; datasetname=NIUnderlyingDetail; TableName=", table,
    "; Frequency=M; Year=", paste(years, collapse = ","),
    "; ResultFormat=JSON"
  )
}

.bea_chunk_path <- function(raw_dir, table, years) {
  file.path(
    raw_dir,
    sprintf("bea_%s_%d_%d.csv", table, min(years), max(years))
  )
}

.bea_info_from_meta <- function(meta, path, table, years, reused) {
  list(
    path = path,
    table = if (is.null(table)) "" else table,
    year_min = if (length(years)) min(years) else NA_integer_,
    year_max = if (length(years)) max(years) else NA_integer_,
    years = paste(years, collapse = ","),
    fetched_at = meta$fetched_at,
    fetched_at_text = meta$fetched_at_text,
    first_period = meta$first_period,
    latest_period = meta$latest_period,
    row_count = meta$row_count,
    hash_algo = meta$hash_algo,
    hash = if (identical(meta$hash_algo, "sha256")) {
      meta$sha256
    } else {
      meta$md5
    },
    metadata_source = meta$metadata_source,
    fetched_at_source = meta$fetched_at_source,
    reused = reused
  )
}

.bea_files_table <- function(infos) {
  rows <- lapply(infos, function(info) {
    data.table::data.table(
      file = basename(info$path),
      table = info$table,
      year_min = info$year_min,
      year_max = info$year_max,
      fetched_at = info$fetched_at,
      fetched_at_text = info$fetched_at_text,
      first_period = info$first_period,
      latest_period = info$latest_period,
      row_count = info$row_count,
      hash_algo = info$hash_algo,
      hash = info$hash,
      metadata_source = info$metadata_source,
      fetched_at_source = info$fetched_at_source,
      reused = info$reused
    )
  })
  data.table::rbindlist(rows, use.names = TRUE)
}

.bea_provenance <- function(source, files, extra = list()) {
  valid <- which(!is.na(files$fetched_at))
  if (length(valid)) {
    latest <- valid[which.max(files$fetched_at[valid])]
    pulled_at <- files$fetched_at[latest]
    pulled_at_text <- files$fetched_at_text[latest]
    pulled_at_source <- files$fetched_at_source[latest]
  } else {
    pulled_at <- as.POSIXct(NA_character_, tz = "UTC")
    pulled_at_text <- NA_character_
    pulled_at_source <- NA_character_
  }
  c(
    list(
      source = source,
      files = files,
      pulled_at = pulled_at,
      pulled_at_text = pulled_at_text,
      pulled_at_source = pulled_at_source,
      metadata_sources = unique(files$metadata_source),
      cache_files = nrow(files)
    ),
    extra
  )
}

.bea_attach_provenance <- function(dt, provenance) {
  dt <- dt[]
  data.table::setattr(dt, "bea_provenance", provenance)
  dt
}

.bea_write_csv <- function(dt, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(
    paste0(basename(path), "."),
    tmpdir = dirname(path),
    fileext = ".tmp"
  )
  on.exit(unlink(tmp), add = TRUE)
  tryCatch(
    data.table::fwrite(dt, tmp, na = "NA"),
    error = function(e) {
      stop("Could not write ", path, ": ", conditionMessage(e), call. = FALSE)
    }
  )
  if (!file.copy(tmp, path, overwrite = TRUE)) {
    stop("Could not replace cache file ", path, call. = FALSE)
  }
  invisible(path)
}

.bea_atomic_copy <- function(src, dest) {
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(
    paste0(basename(dest), "."),
    tmpdir = dirname(dest),
    fileext = ".tmp"
  )
  on.exit(unlink(tmp), add = TRUE)
  if (!file.copy(src, tmp, overwrite = TRUE)) {
    stop("Could not stage ", dest, call. = FALSE)
  }
  if (!file.copy(tmp, dest, overwrite = TRUE)) {
    stop("Could not replace ", dest, call. = FALSE)
  }
  invisible(dest)
}

.bea_stage_discard <- function(staged) {
  if (is.list(staged) && !is.null(staged$stage_dir) &&
      length(staged$stage_dir) == 1L && nzchar(staged$stage_dir)) {
    unlink(staged$stage_dir, recursive = TRUE, force = TRUE)
  }
  invisible(NULL)
}

.bea_commit_entries <- function(entries, raw_dir) {
  entries <- Filter(Negate(is.null), entries)
  if (!length(entries)) return(invisible(FALSE))

  stage_dirs <- unique(vapply(entries, function(entry) {
    entry$stage_dir
  }, character(1L)))
  backup_dir <- tempfile(".bea-backup-", tmpdir = raw_dir)
  dir.create(backup_dir, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(backup_dir)) {
    stop("Could not create cache backup directory in ", raw_dir, call. = FALSE)
  }
  on.exit(unlink(backup_dir, recursive = TRUE, force = TRUE), add = TRUE)
  on.exit(unlink(stage_dirs, recursive = TRUE, force = TRUE), add = TRUE)

  backups <- vector("list", length(entries))
  for (i in seq_along(entries)) {
    entry <- entries[[i]]
    backup <- list(
      target_exists = file.exists(entry$target),
      meta_exists = file.exists(entry$target_meta),
      target_backup = file.path(backup_dir, sprintf("%03d_target", i)),
      meta_backup = file.path(backup_dir, sprintf("%03d_meta", i))
    )
    if (backup$target_exists &&
        !file.copy(entry$target, backup$target_backup, overwrite = TRUE)) {
      stop("Could not back up cache file ", entry$target, call. = FALSE)
    }
    if (backup$meta_exists &&
        !file.copy(entry$target_meta, backup$meta_backup, overwrite = TRUE)) {
      stop("Could not back up cache metadata ", entry$target_meta,
           call. = FALSE)
    }
    backups[[i]] <- backup
  }

  tryCatch({
    for (i in seq_along(entries)) {
      entry <- entries[[i]]
      .bea_atomic_copy(entry$staged_csv, entry$target)
      .bea_atomic_copy(entry$staged_meta, entry$target_meta)
    }
  }, error = function(err) {
    for (i in rev(seq_along(entries))) {
      entry <- entries[[i]]
      backup <- backups[[i]]
      if (isTRUE(backup$target_exists)) {
        try(file.copy(backup$target_backup, entry$target, overwrite = TRUE),
            silent = TRUE)
      } else {
        try(unlink(entry$target, force = TRUE), silent = TRUE)
      }
      if (isTRUE(backup$meta_exists)) {
        try(file.copy(backup$meta_backup, entry$target_meta, overwrite = TRUE),
            silent = TRUE)
      } else {
        try(unlink(entry$target_meta, force = TRUE), silent = TRUE)
      }
    }
    stop(
      "Could not commit BEA cache update: ", conditionMessage(err),
      call. = FALSE
    )
  })

  invisible(TRUE)
}

.bea_stage_table <- function(table, chunks, refresh, raw_dir,
                             now = Sys.Date()) {
  stage_dir <- NULL
  keep_stage <- FALSE
  on.exit({
    if (!keep_stage && !is.null(stage_dir)) {
      unlink(stage_dir, recursive = TRUE, force = TRUE)
    }
  }, add = TRUE)

  parts <- vector("list", length(chunks))
  infos <- vector("list", length(chunks))
  entries <- list()
  backfills <- list()
  key <- NULL
  key_read <- FALSE

  for (i in seq_along(chunks)) {
    years <- chunks[[i]]
    target <- .bea_chunk_path(raw_dir, table, years)
    inspected <- NULL
    action <- "fetch"
    reason <- "cache missing"

    if (!refresh && file.exists(target)) {
      inspected <- tryCatch(
        .bea_inspect_cache(target, table, years, now = now),
        error = function(e) {
          list(status = "invalid", reason = conditionMessage(e))
        }
      )
      if (identical(inspected$status, "ok")) {
        if (.bea_meta_is_fresh(inspected$meta, years, now)) {
          action <- "reuse"
          parts[[i]] <- inspected$data
          infos[[i]] <- .bea_info_from_meta(
            inspected$meta, target, table, years, reused = TRUE
          )
          if (isTRUE(inspected$meta$backfill_needed) &&
              !isTRUE(inspected$meta$sidecar_written)) {
            backfills[[length(backfills) + 1L]] <- list(
              path = target,
              meta = inspected$meta
            )
          }
        } else {
          reason <- "stale current/prior-year chunk"
        }
      } else {
        reason <- paste0("unusable cache: ", inspected$reason)
      }
    } else if (refresh) {
      reason <- "refresh requested"
    }

    if (identical(action, "reuse")) next

    if (!key_read) {
      key <- .bea_read_key()
      key_read <- TRUE
    }

    fetched_at_text <- NULL
    chunk_dt <- tryCatch(
      {
        dt <- .bea_fetch_chunk(table, years, key)
        fetched_at_text <- .bea_now_text()
        dt
      },
      error = function(e) {
        stop(
          "BEA fetch failed for table ", table, ", years ",
          min(years), "-", max(years), " (", reason, "): ",
          .bea_redact(conditionMessage(e), key),
          call. = FALSE
        )
      }
    )

    old_latest <- NA
    if (!is.null(inspected) && identical(inspected$status, "ok")) {
      old_latest <- inspected$meta$latest_period
    } else if (!is.null(inspected$data) && nrow(inspected$data)) {
      old_latest <- max(inspected$data$date)
    }
    new_latest <- max(chunk_dt$date)
    if (!is.na(old_latest) && new_latest < old_latest) {
      stop(
        "BEA response for table ", table, ", years ", min(years), "-",
        max(years), " ends at ", format(new_latest),
        ", earlier than the cached ", format(old_latest),
        "; refusing to replace a newer cache with older data.",
        call. = FALSE
      )
    }

    if (is.null(stage_dir)) {
      stage_dir <- tempfile(".bea-stage-", tmpdir = raw_dir)
      dir.create(stage_dir, recursive = TRUE, showWarnings = FALSE)
      if (!dir.exists(stage_dir)) {
        stop("Could not create cache staging directory in ", raw_dir,
             call. = FALSE)
      }
    }

    staged_csv <- file.path(stage_dir, basename(target))
    .bea_write_csv(chunk_dt, staged_csv)
    meta <- .bea_make_meta(
      staged_csv, chunk_dt,
      dataset = "BEA NIUnderlyingDetail",
      table = table,
      years = years,
      request = .bea_request_text(table, years),
      fetched_at_text = fetched_at_text,
      metadata_source = "api",
      fetched_at_source = "api"
    )
    staged_meta <- .bea_sidecar_path(staged_csv)
    .bea_write_meta(meta, staged_meta)

    entries[[length(entries) + 1L]] <- list(
      target = target,
      target_meta = .bea_sidecar_path(target),
      staged_csv = staged_csv,
      staged_meta = staged_meta,
      stage_dir = stage_dir,
      table = table,
      years = years
    )
    parts[[i]] <- chunk_dt
    infos[[i]] <- .bea_info_from_meta(meta, target, table, years, reused = FALSE)
  }

  keep_stage <- TRUE
  list(
    table = table,
    parts = parts,
    infos = infos,
    entries = entries,
    backfills = backfills,
    stage_dir = stage_dir
  )
}

.bea_finish_table <- function(staged, start_year, end_year) {
  out <- data.table::rbindlist(staged$parts, use.names = TRUE)
  out <- out[
    as.integer(format(date, "%Y")) >= start_year &
      as.integer(format(date, "%Y")) <= end_year
  ]
  if (nrow(out) == 0L) {
    stop("No BEA observations in the requested range.", call. = FALSE)
  }

  out <- .bea_check_duplicate_rows(
    out,
    paste0("BEA table ", staged$table, " cache")
  )
  data.table::setorder(out, line_number, date, series_code, na.last = TRUE)

  files <- .bea_files_table(staged$infos)
  provenance <- .bea_provenance(
    source = "BEA NIUnderlyingDetail",
    files = files,
    extra = list(
      table = staged$table,
      requested_years = c(start_year, end_year)
    )
  )
  .bea_attach_provenance(out[], provenance)
}

.bea_get_table <- function(spec) {
  bea.R::beaGet(
    spec,
    asTable = TRUE,
    asWide = FALSE,
    iTableStyle = FALSE
  )
}

.bea_fetch_chunk <- function(table, years, key) {
  if (!requireNamespace("bea.R", quietly = TRUE)) {
    stop(
      "Package 'bea.R' is required for live BEA downloads; install it with ",
      "install.packages(\"bea.R\").",
      call. = FALSE
    )
  }

  spec <- list(
    UserID = key,
    Method = "GetData",
    datasetname = "NIUnderlyingDetail",
    TableName = table,
    Frequency = "M",
    Year = paste(years, collapse = ","),
    ResultFormat = "JSON"
  )

  warnings_seen <- character()
  raw <- withCallingHandlers(
    .bea_get_table(spec),
    warning = function(w) {
      warnings_seen <<- c(warnings_seen, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  if (!is.data.frame(raw)) {
    detail <- c(if (is.character(raw)) raw else character(), warnings_seen)
    detail <- detail[nzchar(detail)]
    if (!length(detail)) {
      detail <- paste0(
        "unexpected result class: ",
        paste(class(raw), collapse = "/")
      )
    }
    stop(
      "BEA API returned an error: ",
      .bea_short(.bea_redact(paste(unique(detail), collapse = "; "), key)),
      call. = FALSE
    )
  }

  if (!"DataValue" %in% names(raw) &&
      any(grepl("error", names(raw), ignore.case = TRUE))) {
    description_col <- grep(
      "APIErrorDescription", names(raw),
      ignore.case = TRUE, value = TRUE
    )
    detail <- if (length(description_col)) {
      as.character(raw[[description_col[1L]]])
    } else {
      names(raw)
    }
    detail <- c(detail, warnings_seen)
    stop(
      "BEA API returned an error: ",
      .bea_short(.bea_redact(paste(unique(detail), collapse = "; "), key)),
      call. = FALSE
    )
  }

  raw <- data.table::as.data.table(raw)
  if (nrow(raw) == 0L) {
    detail <- if (length(warnings_seen)) {
      paste(unique(warnings_seen), collapse = "; ")
    } else {
      "no data rows"
    }
    stop(
      "BEA API returned no data: ",
      .bea_short(.bea_redact(detail, key)),
      call. = FALSE
    )
  }

  .bea_normalize_response(raw, table, years)
}

#' Fetch one BEA NIUnderlyingDetail table as a normalized long table.
#'
#' Cached chunks are reused unless `refresh = TRUE`. Chunks covering the
#' current or prior calendar year are automatically refetched when their cache
#' is missing, unusable, or behind the latest plausible monthly release; older
#' chunks stay cached. All fetched chunks are staged and only replace the
#' cache after every requested chunk for the table has succeeded.
#'
#' Chunks are five years wide; the final chunk is shorter when the requested
#' range does not divide evenly. The default range is 2016 through the current
#' year; pass an earlier `start_year` if year-over-year calculations need
#' observations before 2016.
#'
#' @param table BEA table name, for example U20404.
#' @param start_year First year to return.
#' @param end_year Last year to return; defaults to the current year.
#' @param refresh If TRUE, refetch every chunk instead of using cached chunks.
#' @return A data.table with columns table, line_number, series_code,
#'   description, date (first of month), value.
fetch_bea_table <- function(table, start_year = 2016L,
                            end_year = as.integer(format(Sys.Date(), "%Y")),
                            refresh = FALSE) {
  table <- .bea_table_id(table)
  start_year <- .bea_year(start_year, "start_year")
  end_year <- .bea_year(end_year, "end_year")
  if (start_year > end_year) {
    stop("`start_year` cannot be later than `end_year`.", call. = FALSE)
  }
  refresh <- .bea_validate_refresh(refresh)

  chunks <- .bea_year_chunks(start_year, end_year)
  raw_dir <- .bea_raw_dir()
  staged <- .bea_stage_table(table, chunks, refresh, raw_dir)

  committed <- FALSE
  on.exit({
    if (!committed) .bea_stage_discard(staged)
  }, add = TRUE)

  out <- .bea_finish_table(staged, start_year, end_year)
  .bea_commit_entries(staged$entries, raw_dir)
  .bea_write_backfills(staged$backfills)
  committed <- TRUE
  out
}

#' Fetch the price and nominal BEA inputs used by the trimmed-mean charts.
#'
#' U20404 is the price index table and U20405 is the nominal table. Both tables
#' are staged before either cache is committed, so a failure while fetching the
#' second table leaves both existing caches untouched.
#'
#' @param start_year First year to return.
#' @param refresh If TRUE, refetch every cached chunk in both tables.
#' @return A list with `price` and `nominal`, each a normalized long data.table.
fetch_bea_inputs <- function(start_year = 2016L, refresh = FALSE) {
  start_year <- .bea_year(start_year, "start_year")
  refresh <- .bea_validate_refresh(refresh)
  end_year <- as.integer(format(Sys.Date(), "%Y"))
  if (start_year > end_year) {
    stop("`start_year` cannot be later than the current year.", call. = FALSE)
  }
  raw_dir <- .bea_raw_dir()
  chunks <- .bea_year_chunks(start_year, end_year)

  staged_price <- .bea_stage_table("U20404", chunks, refresh, raw_dir)
  staged_nominal <- tryCatch(
    .bea_stage_table("U20405", chunks, refresh, raw_dir),
    error = function(e) {
      .bea_stage_discard(staged_price)
      stop(conditionMessage(e), call. = FALSE)
    }
  )

  committed <- FALSE
  on.exit({
    if (!committed) {
      .bea_stage_discard(staged_price)
      .bea_stage_discard(staged_nominal)
    }
  }, add = TRUE)

  price <- .bea_finish_table(staged_price, start_year, end_year)
  nominal <- .bea_finish_table(staged_nominal, start_year, end_year)
  .bea_commit_entries(
    c(staged_price$entries, staged_nominal$entries),
    raw_dir
  )
  .bea_write_backfills(c(staged_price$backfills, staged_nominal$backfills))
  committed <- TRUE

  list(price = price, nominal = nominal)
}

.bea_download_fred <- function(url, dest) {
  status <- tryCatch(
    suppressWarnings(
      utils::download.file(url, dest, mode = "wb", quiet = TRUE)
    ),
    error = function(e) {
      stop(
        "Failed to download FRED series ", .bea_fred_series, ": ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )
  if (!is.null(status) && !identical(as.integer(status), 0L)) {
    stop(
      "Failed to download FRED series ", .bea_fred_series,
      " from ", url, " (download status ", status, ").",
      call. = FALSE
    )
  }
  if (!file.exists(dest) || is.na(file.info(dest)$size) ||
      file.info(dest)$size == 0) {
    stop(
      "Failed to download FRED series ", .bea_fred_series,
      ": the downloaded file is empty.",
      call. = FALSE
    )
  }
  invisible(dest)
}

.bea_parse_fred <- function(path, context) {
  raw <- tryCatch(
    data.table::fread(path, data.table = TRUE),
    error = function(e) {
      stop(
        context, " could not be parsed: ", conditionMessage(e),
        call. = FALSE
      )
    }
  )
  if (ncol(raw) < 2L) {
    stop(context, " has fewer than two columns.", call. = FALSE)
  }

  dates <- .bea_date_column(raw[[1L]], context)
  if (any(format(dates, "%d") != "01")) {
    stop(
      context, " is not monthly (dates are not the first of a month).",
      call. = FALSE
    )
  }
  if (anyDuplicated(dates)) {
    duplicates <- unique(dates[duplicated(dates)])
    stop(
      context, " contains duplicate observation dates (first: ",
      format(duplicates[1L]), ").",
      call. = FALSE
    )
  }

  dt <- data.table::data.table(
    date = dates,
    dallas_yoy = .bea_parse_value(
      raw[[2L]],
      what = paste0("FRED series ", .bea_fred_series)
    )
  )
  dt <- dt[!is.na(dallas_yoy)]
  if (nrow(dt) == 0L) {
    stop(context, " contains no numeric observations.", call. = FALSE)
  }
  data.table::setorder(dt, date)
  dt[]
}

#' Fetch the Dallas Fed trimmed-mean PCE benchmark from FRED.
#'
#' Reads the public CSV for PCETRIM12M159SFRBDAL and caches the normalized
#' series at data/raw/fred_PCETRIM12M159SFRBDAL.csv with a provenance sidecar.
#' Values are percentage points as published by FRED. Without `refresh = TRUE`
#' an existing valid cache is reused; an invalid cache triggers a fresh
#' download, and the old cache is replaced only after the download parses.
#'
#' @param refresh If TRUE, download even when a cached copy exists.
#' @return A data.table with columns date and dallas_yoy.
fetch_dallas_benchmark <- function(refresh = FALSE) {
  refresh <- .bea_validate_refresh(refresh)
  raw_dir <- .bea_raw_dir()
  path <- file.path(raw_dir, paste0("fred_", .bea_fred_series, ".csv"))

  cache_issue <- NULL
  if (!refresh && file.exists(path)) {
    inspected <- tryCatch(
      .bea_inspect_fred_cache(path),
      error = function(e) {
        list(status = "invalid", reason = conditionMessage(e))
      }
    )
    if (identical(inspected$status, "ok")) {
      provenance <- .bea_provenance(
        source = "FRED",
        files = .bea_files_table(list(.bea_info_from_meta(
          inspected$meta, path, table = NULL,
          years = inspected$meta$years, reused = TRUE
        ))),
        extra = list(series = .bea_fred_series)
      )
      return(.bea_attach_provenance(inspected$data, provenance))
    }
    cache_issue <- inspected$reason
  }

  stage_dir <- tempfile(".bea-stage-", tmpdir = raw_dir)
  dir.create(stage_dir, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(stage_dir)) {
    stop("Could not create cache staging directory in ", raw_dir,
         call. = FALSE)
  }
  on.exit(unlink(stage_dir, recursive = TRUE, force = TRUE), add = TRUE)

  staged_csv <- file.path(stage_dir, basename(path))
  tryCatch(
    .bea_download_fred(.bea_fred_url, staged_csv),
    error = function(e) {
      detail <- conditionMessage(e)
      if (!is.null(cache_issue)) {
        detail <- paste0(
          detail, " (cached copy was unusable: ", cache_issue, ")"
        )
      }
      stop(detail, call. = FALSE)
    }
  )
  fetched_at_text <- .bea_now_text()

  dt <- .bea_parse_fred(
    staged_csv,
    paste0("FRED series ", .bea_fred_series)
  )
  .bea_write_csv(dt, staged_csv)

  meta <- .bea_make_meta(
    staged_csv, dt,
    dataset = "FRED",
    series = .bea_fred_series,
    years = sort(unique(as.integer(format(dt$date, "%Y")))),
    request = paste0("GET ", .bea_fred_url),
    fetched_at_text = fetched_at_text,
    metadata_source = "fred-download",
    fetched_at_source = "fred-download"
  )
  staged_meta <- .bea_sidecar_path(staged_csv)
  .bea_write_meta(meta, staged_meta)

  .bea_commit_entries(
    list(list(
      target = path,
      target_meta = .bea_sidecar_path(path),
      staged_csv = staged_csv,
      staged_meta = staged_meta,
      stage_dir = stage_dir,
      table = "",
      years = meta$years
    )),
    raw_dir
  )

  provenance <- .bea_provenance(
    source = "FRED",
    files = .bea_files_table(list(.bea_info_from_meta(
      meta, path, table = NULL, years = meta$years, reused = FALSE
    ))),
    extra = list(series = .bea_fred_series)
  )
  .bea_attach_provenance(dt, provenance)
}

.bea_manifest_row <- function(dataset, source, identifier, x) {
  if (!is.data.frame(x)) {
    stop(
      "`", dataset, "` must be a data.frame or data.table.",
      call. = FALSE
    )
  }
  if (!"date" %in% names(x)) {
    stop("`", dataset, "` must contain a `date` column.", call. = FALSE)
  }
  dates <- .bea_date_column(x$date, paste0("`", dataset, "`"))

  provenance <- attr(x, "bea_provenance", exact = TRUE)
  pulled_at <- NA_character_
  if (is.list(provenance) &&
      !is.null(provenance$pulled_at_text) &&
      length(provenance$pulled_at_text) == 1L &&
      !is.na(provenance$pulled_at_text) &&
      nzchar(provenance$pulled_at_text)) {
    pulled_at <- provenance$pulled_at_text
  }

  data.table::data.table(
    dataset = dataset,
    source = source,
    identifier = identifier,
    n_rows = nrow(x),
    first_date = if (length(dates)) min(dates) else as.Date(NA_character_),
    latest_date = if (length(dates)) max(dates) else as.Date(NA_character_),
    pulled_at = pulled_at
  )
}

#' Write a manifest describing the inputs and their latest observation dates.
#'
#' `pulled_at` is the latest actual fetch time recorded in the source cache
#' metadata, or the file mtime for a backfilled legacy cache. It is not the
#' time this function runs; it is NA when the inputs did not carry provenance.
#'
#' @return The manifest data.table, invisibly.
write_data_manifest <- function(price, nominal, dallas,
                                path = "data/raw/manifest.csv") {
  manifest <- data.table::rbindlist(
    list(
      .bea_manifest_row(
        "price", "BEA NIUnderlyingDetail", "U20404", price
      ),
      .bea_manifest_row(
        "nominal", "BEA NIUnderlyingDetail", "U20405", nominal
      ),
      .bea_manifest_row(
        "dallas", "FRED", .bea_fred_series, dallas
      )
    ),
    use.names = TRUE
  )

  path <- .bea_resolve_path(path)
  .bea_write_csv(manifest, path)
  invisible(manifest)
}
