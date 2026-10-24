# BEA and FRED ingestion for the trimmed-mean project.
#
# Public entry points:
#   fetch_bea_table(table, start_year = 2016, end_year = current, refresh = FALSE)
#   fetch_bea_inputs(start_year = 2016, refresh = FALSE)
#   fetch_dallas_benchmark()
#   write_data_manifest(price, nominal, dallas, path = "data/raw/manifest.csv")
#
# Requires data.table. Live BEA downloads also require bea.R.
# BEA_API_KEY overrides the single key stored in .bea_api_key at the project
# root. Keys are never printed.
#
# BEA values keep their published units: U20404 is a seasonally adjusted
# monthly price index (2017 = 100) and U20405 is seasonally adjusted monthly
# nominal PCE in millions of dollars at annual rates. Both outputs are long
# tables with one row per table/line/series/month.

.bea_chunk_width <- 5L
.bea_fred_series <- "PCETRIM12M159SFRBDAL"
.bea_fred_url <- paste0(
  "https://fred.stlouisfed.org/graph/fredgraph.csv?id=",
  .bea_fred_series
)
.bea_missing_tokens <- c(
  "", ".", "..", "...", ".....", "NA", "N/A", "N.A.", "(D)", "(NA)", "--", "\u2014"
)

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

  data.table::setorder(out, line_number, date, series_code, na.last = TRUE)
  out <- unique(
    out,
    by = c("table", "line_number", "series_code", "date")
  )
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
  if (!all(as.character(dt$table) == table)) {
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

  data.table::setorder(out, line_number, date, series_code, na.last = TRUE)
  out <- unique(
    out,
    by = c("table", "line_number", "series_code", "date")
  )
  out[]
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
#' Cached chunks are reused unless `refresh = TRUE`. Chunks are five years
#' wide; the final chunk is shorter when the requested range does not divide
#' evenly. The default range is 2016 through the current year; pass an earlier
#' `start_year` if year-over-year calculations need observations before 2016.
#'
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
  if (!is.logical(refresh) || length(refresh) != 1L || is.na(refresh)) {
    stop("`refresh` must be TRUE or FALSE.", call. = FALSE)
  }

  chunks <- .bea_year_chunks(start_year, end_year)
  raw_dir <- .bea_raw_dir()
  key <- NULL
  parts <- vector("list", length(chunks))

  for (i in seq_along(chunks)) {
    years <- chunks[[i]]
    path <- file.path(
      raw_dir,
      sprintf(
        "bea_%s_%d_%d.csv",
        table, min(years), max(years)
      )
    )

    if (!refresh && file.exists(path)) {
      parts[[i]] <- .bea_read_chunk(path, table, years)
      next
    }

    if (is.null(key)) {
      key <- .bea_read_key()
    }

    chunk_dt <- tryCatch(
      .bea_fetch_chunk(table, years, key),
      error = function(e) {
        stop(
          "BEA fetch failed for table ", table, ", years ",
          min(years), "-", max(years), ": ",
          .bea_redact(conditionMessage(e), key),
          call. = FALSE
        )
      }
    )
    .bea_write_csv(chunk_dt, path)
    parts[[i]] <- chunk_dt
  }

  out <- data.table::rbindlist(parts, use.names = TRUE)
  out <- out[
    as.integer(format(date, "%Y")) >= start_year &
      as.integer(format(date, "%Y")) <= end_year
  ]
  if (nrow(out) == 0L) {
    stop("No BEA observations in the requested range.", call. = FALSE)
  }

  data.table::setorder(out, line_number, date, series_code, na.last = TRUE)
  out <- unique(
    out,
    by = c("table", "line_number", "series_code", "date")
  )
  out[]
}

#' Fetch the price and nominal BEA inputs used by the trimmed-mean charts.
#'
#' U20404 is the price index table and U20405 is the nominal table.
#'
#' @return A list with `price` and `nominal`, each a normalized long data.table.
fetch_bea_inputs <- function(start_year = 2016L, refresh = FALSE) {
  end_year <- as.integer(format(Sys.Date(), "%Y"))
  list(
    price = fetch_bea_table(
      "U20404",
      start_year = start_year,
      end_year = end_year,
      refresh = refresh
    ),
    nominal = fetch_bea_table(
      "U20405",
      start_year = start_year,
      end_year = end_year,
      refresh = refresh
    )
  )
}

#' Fetch the Dallas Fed trimmed-mean PCE benchmark from FRED.
#'
#' Reads the public CSV for PCETRIM12M159SFRBDAL and caches the normalized
#' series at data/raw/fred_PCETRIM12M159SFRBDAL.csv. Values are percentage
#' points as published by FRED. Delete the cache file to force a fresh download.
#'
#' @return A data.table with columns date and dallas_yoy.
fetch_dallas_benchmark <- function() {
  raw_dir <- .bea_raw_dir()
  path <- file.path(raw_dir, paste0("fred_", .bea_fred_series, ".csv"))

  if (!file.exists(path)) {
    tmp <- tempfile(
      paste0("fred_", .bea_fred_series, "_"),
      tmpdir = raw_dir,
      fileext = ".csv"
    )
    on.exit(unlink(tmp), add = TRUE)

    status <- tryCatch(
      suppressWarnings(
        utils::download.file(.bea_fred_url, tmp, mode = "wb", quiet = TRUE)
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
        " from ", .bea_fred_url, " (download status ", status, ").",
        call. = FALSE
      )
    }

    raw <- tryCatch(
      data.table::fread(tmp, data.table = TRUE),
      error = function(e) {
        stop(
          "Could not parse FRED CSV for ", .bea_fred_series, ": ",
          conditionMessage(e),
          call. = FALSE
        )
      }
    )
    if (ncol(raw) < 2L) {
      stop(
        "FRED CSV for ", .bea_fred_series,
        " has fewer than two columns.",
        call. = FALSE
      )
    }

    dates <- .bea_date_column(
      raw[[1L]],
      paste0("FRED CSV for ", .bea_fred_series)
    )
    if (any(format(dates, "%d") != "01")) {
      stop(
        "FRED series ", .bea_fred_series,
        " is not monthly (dates are not the first of a month).",
        call. = FALSE
      )
    }
    if (anyDuplicated(dates)) {
      stop(
        "FRED CSV for ", .bea_fred_series,
        " contains duplicate observation dates.",
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
      stop(
        "FRED series ", .bea_fred_series,
        " contains no numeric observations.",
        call. = FALSE
      )
    }
    data.table::setorder(dt, date)
    .bea_write_csv(dt, path)
    return(dt[])
  }

  dt <- tryCatch(
    data.table::fread(path, data.table = TRUE, na.strings = c("NA", "")),
    error = function(e) {
      stop(
        "Could not read cached FRED file ", path, ": ",
        conditionMessage(e),
        ". Delete it to re-download.",
        call. = FALSE
      )
    }
  )
  required <- c("date", "dallas_yoy")
  missing <- setdiff(required, names(dt))
  if (length(missing)) {
    stop(
      "Cached FRED file ", path, " is invalid; missing column(s): ",
      paste(missing, collapse = ", "),
      ". Delete it to re-download.",
      call. = FALSE
    )
  }
  if (nrow(dt) == 0L) {
    stop(
      "Cached FRED file ", path, " is empty. Delete it to re-download.",
      call. = FALSE
    )
  }

  dates <- .bea_date_column(
    dt$date,
    paste0("Cached FRED file ", path),
    " Delete it to re-download."
  )
  if (any(format(dates, "%d") != "01")) {
    stop(
      "Cached FRED file ", path,
      " has dates that are not the first of a month. Delete it to re-download.",
      call. = FALSE
    )
  }

  out <- data.table::data.table(
    date = dates,
    dallas_yoy = .bea_parse_value(
      dt$dallas_yoy,
      what = paste0("cached FRED series ", .bea_fred_series)
    )
  )
  out <- out[!is.na(dallas_yoy)]
  if (nrow(out) == 0L) {
    stop(
      "Cached FRED file ", path, " has no numeric observations. ",
      "Delete it to re-download.",
      call. = FALSE
    )
  }
  data.table::setorder(out, date)
  out[]
}

.bea_manifest_row <- function(dataset, source, identifier, x, pulled_at) {
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
#' @return The manifest data.table, invisibly.
write_data_manifest <- function(price, nominal, dallas,
                                path = "data/raw/manifest.csv") {
  pulled_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  manifest <- data.table::rbindlist(
    list(
      .bea_manifest_row(
        "price", "BEA NIUnderlyingDetail", "U20404",
        price, pulled_at
      ),
      .bea_manifest_row(
        "nominal", "BEA NIUnderlyingDetail", "U20405",
        nominal, pulled_at
      ),
      .bea_manifest_row(
        "dallas", "FRED", .bea_fred_series,
        dallas, pulled_at
      )
    ),
    use.names = TRUE
  )

  path <- .bea_resolve_path(path)
  .bea_write_csv(manifest, path)
  invisible(manifest)
}
