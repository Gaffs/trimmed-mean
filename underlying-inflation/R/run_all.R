# Standalone underlying-inflation workflow.
#
#   Rscript underlying-inflation/R/run_all.R [--refresh]   (from the repo root)
#   Rscript R/run_all.R [--refresh]                        (from the subproject)
#
# Sources only the subproject modules in R/ and writes the subproject's
# output/data and output/figures directories. --refresh is forwarded to both
# the BEA and FRED fetchers.

.uir_stop <- function(...) {
  stop(..., call. = FALSE)
}

.uir_assert <- function(condition, ...) {
  if (!isTRUE(condition)) {
    .uir_stop(...)
  }
  invisible(TRUE)
}

.uir_short <- function(x, n = 8L) {
  x <- as.character(x)
  if (length(x) <= n) {
    return(paste(x, collapse = ", "))
  }
  paste0(paste(x[seq_len(n)], collapse = ", "), ", ... (",
         length(x) - n, " more)")
}

.uir_format_month <- function(x) {
  format(as.Date(x), "%Y-%m")
}

.uir_month_id <- function(x) {
  as.integer(format(as.Date(x), "%Y")) * 12L +
    as.integer(format(as.Date(x), "%m"))
}

.uir_is_absolute <- function(path) {
  grepl("^([A-Za-z]:[/\\\\]|[/\\\\])", path)
}

.uir_expected_components <- 163L
.uir_expected_series <- c(
  "headline", "core", "dallas", "market_ex_housing_tm16"
)
.uir_expected_measures <- c(
  "yoy", "six_month_annualized", "three_month_annualized"
)
.uir_expected_starts <- structure(
  as.Date(c("1988-01-01", "1987-07-01", "1987-04-01")),
  names = .uir_expected_measures
)
.uir_measure_columns <- c(
  yoy = "yoy",
  six_month_annualized = "annualized_6m",
  three_month_annualized = "annualized_3m"
)
.uir_start_columns <- c(
  yoy = "yoy_start",
  six_month_annualized = "six_month_annualized_start",
  three_month_annualized = "three_month_annualized_start"
)
.uir_preferred_columns <- c(
  "date", "monthly", "index", "yoy", "annualized_6m", "annualized_3m"
)
.uir_expected_data_files <- c(
  "preferred_measure.csv", "inflation_rates.csv", "run_summary.csv"
)
.uir_expected_figure_files <- c(
  "01_year_on_year.png",
  "02_six_month_annualized.png",
  "03_three_month_annualized.png"
)
.uir_png_signature <- as.raw(c(
  0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a
))

.uir_script_file <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  hit <- grep("^--file=", args, value = TRUE)
  if (!length(hit)) {
    return(NA_character_)
  }
  path <- sub("^--file=", "", hit[[1L]])
  path <- gsub('^["\']|["\']$', "", trimws(path))
  if (!nzchar(path)) {
    return(NA_character_)
  }
  path <- path.expand(path)
  if (!.uir_is_absolute(path)) {
    path <- file.path(getwd(), path)
  }
  tryCatch(
    normalizePath(path, winslash = "/", mustWork = FALSE),
    error = function(e) NA_character_
  )
}

.uir_is_subproject <- function(path) {
  if (!is.character(path) || length(path) != 1L || is.na(path) ||
      !nzchar(path) || !dir.exists(path)) {
    return(FALSE)
  }
  all(file.exists(c(
    file.path(path, "R", "bea_fred_data.R"),
    file.path(path, "R", "market_ex_housing_tm16.R"),
    file.path(path, "R", "plots.R"),
    file.path(path, "config", "components.csv")
  )))
}

.uir_project_root <- function() {
  candidates <- character()
  script <- .uir_script_file()
  if (!is.na(script)) {
    candidates <- c(candidates, dirname(dirname(script)))
  }
  candidates <- c(
    candidates, getwd(), file.path(getwd(), "underlying-inflation")
  )

  current <- tryCatch(
    normalizePath(getwd(), winslash = "/", mustWork = TRUE),
    error = function(e) NULL
  )
  if (!is.null(current)) {
    repeat {
      candidates <- c(
        candidates, current, file.path(current, "underlying-inflation")
      )
      parent <- dirname(current)
      if (identical(parent, current)) {
        break
      }
      current <- parent
    }
  }

  candidates <- unique(candidates[nzchar(candidates)])
  for (candidate in candidates) {
    if (.uir_is_subproject(candidate)) {
      return(normalizePath(candidate, winslash = "/", mustWork = TRUE))
    }
  }
  .uir_stop(
    "Could not locate the underlying-inflation subproject. Run from the ",
    "repository root or from underlying-inflation/, or invoke the script by ",
    "path with Rscript."
  )
}

.uir_parse_args <- function(args) {
  unknown <- setdiff(args, "--refresh")
  if (length(unknown)) {
    .uir_stop(
      "Unknown argument(s): ", paste(unknown, collapse = ", "),
      ". Usage: Rscript underlying-inflation/R/run_all.R [--refresh]"
    )
  }
  "--refresh" %in% args
}

.uir_png_valid <- function(path) {
  connection <- file(path, open = "rb")
  on.exit(close(connection), add = TRUE)
  identical(
    readBin(connection, what = "raw", n = length(.uir_png_signature)),
    .uir_png_signature
  )
}

.uir_assert_monthly <- function(dates, label) {
  dates <- as.Date(dates)
  if (!length(dates)) {
    .uir_stop(label, " contains no dates.")
  }
  if (anyNA(dates)) {
    .uir_stop(label, " contains missing or unparseable dates.")
  }
  if (any(format(dates, "%d") != "01")) {
    .uir_stop(label, " contains dates that are not the first of a month.")
  }
  if (anyDuplicated(dates)) {
    duplicate <- dates[duplicated(dates)][[1L]]
    .uir_stop(label, " contains duplicate month ", .uir_format_month(duplicate), ".")
  }
  ordered <- sort(dates)
  month_ids <- .uir_month_id(ordered)
  if (length(month_ids) > 1L && any(diff(month_ids) != 1L)) {
    gap <- which(diff(month_ids) != 1L)[[1L]]
    .uir_stop(
      label, " is not monthly-contiguous; a month is missing before ",
      .uir_format_month(ordered[gap + 1L]), "."
    )
  }
  invisible(TRUE)
}

.uir_official_index <- function(price, code, expected_description, label) {
  price <- data.table::as.data.table(price)
  required <- c("line_number", "series_code", "description", "date", "value")
  missing <- setdiff(required, names(price))
  if (length(missing)) {
    .uir_stop(
      label, " source table is missing column(s): ",
      paste(missing, collapse = ", "), "."
    )
  }

  rows <- price[toupper(trimws(as.character(series_code))) == code]
  if (nrow(rows) == 0L) {
    .uir_stop("BEA price table contains no series ", code, " for ", label, ".")
  }
  lines <- unique(rows$line_number)
  if (length(lines) != 1L) {
    .uir_stop(
      label, " series ", code, " maps to ", length(lines),
      " BEA lines; expected exactly one."
    )
  }
  descriptions <- unique(trimws(as.character(rows$description)))
  if (length(descriptions) != 1L ||
      !identical(descriptions, expected_description)) {
    .uir_stop(
      label, " series ", code, " description is ",
      .uir_short(descriptions, 4L), "; expected ",
      dQuote(expected_description), "."
    )
  }

  dates <- as.Date(rows$date)
  values <- suppressWarnings(as.numeric(rows$value))
  if (anyNA(dates) || anyNA(values)) {
    .uir_stop(label, " series ", code, " has missing dates or values.")
  }
  if (any(!is.finite(values)) || any(values <= 0)) {
    .uir_stop(label, " series ", code, " must be finite and strictly positive.")
  }
  if (anyDuplicated(dates)) {
    .uir_stop(label, " series ", code, " has duplicate dates.")
  }

  out <- data.table::data.table(date = dates, value = values)
  data.table::setorder(out, date)
  .uir_assert_monthly(out$date, paste0(label, " series ", code))
  if (min(out$date) != as.Date("1987-01-01")) {
    .uir_stop(
      label, " series ", code, " starts in ", .uir_format_month(min(out$date)),
      "; expected 1987-01."
    )
  }
  out[]
}

.uir_index_rate <- function(index_table, lag_months, label) {
  index_table <- data.table::as.data.table(index_table)
  required <- c("date", "value")
  missing <- setdiff(required, names(index_table))
  if (length(missing)) {
    .uir_stop(label, " index is missing column(s): ",
              paste(missing, collapse = ", "), ".")
  }
  dates <- as.Date(index_table$date)
  values <- suppressWarnings(as.numeric(index_table$value))
  if (anyNA(dates) || anyNA(values)) {
    .uir_stop(label, " index has missing dates or values.")
  }
  if (any(!is.finite(values)) || any(values <= 0)) {
    .uir_stop(label, " index must be finite and strictly positive.")
  }
  .uir_assert_monthly(dates, paste0(label, " index"))
  if (is.unsorted(dates)) {
    .uir_stop(label, " index dates are not sorted.")
  }

  month_ids <- .uir_month_id(dates)
  lag_match <- match(month_ids - as.integer(lag_months), month_ids)
  ratio <- values / values[lag_match]
  out <- data.table::data.table(
    date = dates,
    value = 100 * (ratio^(12 / lag_months) - 1)
  )
  bad <- !is.na(out$value) & !is.finite(out$value)
  if (any(bad)) {
    .uir_stop(
      label, " produced non-finite annualized rates at ",
      .uir_format_month(out$date[bad][[1L]]), "."
    )
  }
  out[]
}

.uir_dallas_index <- function(dallas) {
  dallas <- data.table::as.data.table(dallas)
  required <- c("date", "dallas_monthly_annualized", "dallas_yoy")
  missing <- setdiff(required, names(dallas))
  if (length(missing)) {
    .uir_stop(
      "Dallas data is missing column(s): ", paste(missing, collapse = ", "), "."
    )
  }
  dates <- as.Date(dallas$date)
  annualized <- suppressWarnings(as.numeric(dallas$dallas_monthly_annualized))
  yoy <- suppressWarnings(as.numeric(dallas$dallas_yoy))
  if (anyNA(dates) || anyNA(annualized) || anyNA(yoy)) {
    .uir_stop("Dallas data has missing dates or values.")
  }
  if (any(!is.finite(annualized)) || any(!is.finite(yoy))) {
    .uir_stop("Dallas data has non-finite values.")
  }
  .uir_assert_monthly(dates, "Dallas data")
  if (is.unsorted(dates)) {
    .uir_stop("Dallas data dates are not sorted.")
  }

  growth <- 1 + annualized / 100
  if (any(growth <= 0)) {
    .uir_stop("Dallas one-month annualized rates imply a non-positive growth factor.")
  }
  monthly_decimal <- growth^(1 / 12) - 1
  index <- 100 * cumprod(1 + monthly_decimal)
  if (any(!is.finite(index)) || any(index <= 0)) {
    .uir_stop("The Dallas chained index is not finite and positive.")
  }
  data.table::data.table(date = dates, value = index)
}

.uir_preferred_rate <- function(preferred, column) {
  preferred <- data.table::as.data.table(preferred)
  if (!column %in% names(preferred)) {
    .uir_stop("Preferred measure is missing column ", dQuote(column), ".")
  }
  dates <- as.Date(preferred$date)
  values <- suppressWarnings(as.numeric(preferred[[column]]))
  .uir_assert_monthly(dates, paste0("Preferred measure ", column))
  if (is.unsorted(dates)) {
    .uir_stop("Preferred measure ", column, " dates are not sorted.")
  }
  if (any(!is.finite(values) & !is.na(values))) {
    .uir_stop("Preferred measure ", column, " has non-finite values.")
  }
  data.table::data.table(date = dates, value = values)
}

.uir_assemble_measure <- function(measure, series_rates, start, latest) {
  if (!identical(names(series_rates), .uir_expected_series)) {
    .uir_stop(
      "Rate sources for ", measure, " must be named exactly: ",
      paste(.uir_expected_series, collapse = ", "), "."
    )
  }
  start <- as.Date(start)
  latest <- as.Date(latest)
  expected_dates <- seq(start, latest, by = "month")
  expected_text <- format(expected_dates, "%Y-%m")

  pieces <- vector("list", length(series_rates))
  names(pieces) <- names(series_rates)
  for (series_name in names(series_rates)) {
    table <- data.table::as.data.table(series_rates[[series_name]])
    required <- c("date", "value")
    missing <- setdiff(required, names(table))
    if (length(missing)) {
      .uir_stop(
        measure, "/", series_name, " is missing column(s): ",
        paste(missing, collapse = ", "), "."
      )
    }
    dates <- as.Date(table$date)
    values <- suppressWarnings(as.numeric(table$value))
    if (anyNA(dates)) {
      .uir_stop(measure, "/", series_name, " has missing dates.")
    }
    if (any(!is.finite(values) & !is.na(values))) {
      .uir_stop(measure, "/", series_name, " has non-finite values.")
    }
    .uir_assert_monthly(dates, paste0(measure, "/", series_name))
    inside <- data.table::data.table(date = dates, value = values)[
      date >= start & date <= latest
    ]
    data.table::setorder(inside, date)
    if (nrow(inside) != length(expected_dates) ||
        !identical(format(inside$date, "%Y-%m"), expected_text)) {
      .uir_stop(
        measure, "/", series_name, " does not cover every month from ",
        .uir_format_month(start), " to ", .uir_format_month(latest), "."
      )
    }
    if (any(!is.finite(inside$value))) {
      .uir_stop(
        measure, "/", series_name, " has non-finite values from ",
        .uir_format_month(start), " to ", .uir_format_month(latest), "."
      )
    }
    pieces[[series_name]] <- data.table::data.table(
      series = series_name, date = inside$date, value = inside$value
    )
  }

  out <- data.table::rbindlist(pieces, use.names = TRUE)
  out[, series := factor(series, levels = .uir_expected_series)]
  data.table::setorder(out, series, date)
  out[, series := as.character(series)]
  out[]
}

.uir_validate_outputs <- function(data_dir, figures_dir, latest, starts, refresh) {
  latest <- as.Date(latest)
  .uir_assert(dir.exists(data_dir), "Data output directory does not exist: ", data_dir)
  .uir_assert(dir.exists(figures_dir), "Figure output directory does not exist: ", figures_dir)

  data_files <- list.files(data_dir, all.files = FALSE, no.. = TRUE)
  .uir_assert(
    length(data_files) == length(.uir_expected_data_files) &&
      setequal(data_files, .uir_expected_data_files),
    "Data output directory must contain exactly ",
    paste(.uir_expected_data_files, collapse = ", "), "; found: ",
    .uir_short(data_files, 10L), "."
  )

  preferred_path <- file.path(data_dir, "preferred_measure.csv")
  preferred <- data.table::fread(preferred_path, data.table = TRUE)
  .uir_assert(
    identical(names(preferred), .uir_preferred_columns),
    "preferred_measure.csv columns are not exactly ",
    paste(.uir_preferred_columns, collapse = ", "), "."
  )
  preferred_dates <- as.Date(preferred$date)
  .uir_assert_monthly(preferred_dates, "preferred_measure.csv")
  .uir_assert(
    min(preferred_dates) == as.Date("1987-01-01"),
    "preferred_measure.csv starts in ", .uir_format_month(min(preferred_dates)),
    "; expected 1987-01."
  )
  .uir_assert(
    max(preferred_dates) == latest,
    "preferred_measure.csv ends in ", .uir_format_month(max(preferred_dates)),
    "; expected the common latest official month ",
    .uir_format_month(latest), "."
  )
  .uir_assert(
    nrow(preferred) == length(seq(min(preferred_dates), latest, by = "month")),
    "preferred_measure.csv is not a complete monthly grid."
  )
  .uir_assert(
    all(is.finite(preferred$index)) && all(preferred$index > 0),
    "preferred_measure.csv index must be finite and strictly positive."
  )
  latest_preferred <- preferred[preferred_dates == latest]
  .uir_assert(nrow(latest_preferred) == 1L,
              "preferred_measure.csv has no unique row for ", .uir_format_month(latest), ".")
  latest_preferred_values <- c(
    latest_preferred$monthly, latest_preferred$index, latest_preferred$yoy,
    latest_preferred$annualized_6m, latest_preferred$annualized_3m
  )
  .uir_assert(
    all(is.finite(latest_preferred_values)),
    "preferred_measure.csv has non-finite latest values."
  )

  rates_path <- file.path(data_dir, "inflation_rates.csv")
  rates <- data.table::fread(rates_path, data.table = TRUE)
  .uir_assert(
    identical(names(rates), c("measure", "series", "date", "value")),
    "inflation_rates.csv columns must be measure, series, date, value."
  )
  rates[, date := as.Date(date)]
  .uir_assert(!anyNA(rates$date), "inflation_rates.csv has missing dates.")
  .uir_assert(
    setequal(unique(rates$measure), .uir_expected_measures),
    "inflation_rates.csv must contain exactly the three expected measures."
  )
  .uir_assert(
    setequal(unique(rates$series), .uir_expected_series),
    "inflation_rates.csv must contain exactly headline, core, dallas, and ",
    "market_ex_housing_tm16."
  )
  .uir_assert(
    !anyDuplicated(rates, by = c("measure", "series", "date")),
    "inflation_rates.csv has duplicate measure-series-date rows."
  )

  for (measure_name in .uir_expected_measures) {
    start <- as.Date(starts[[measure_name]])
    expected_dates <- seq(start, latest, by = "month")
    expected_text <- format(expected_dates, "%Y-%m")
    measure_rows <- rates[rates$measure == measure_name]
    .uir_assert(
      nrow(measure_rows) ==
        length(expected_dates) * length(.uir_expected_series),
      "inflation_rates.csv ", measure_name,
      " does not cover every series-month."
    )
    for (series_name in .uir_expected_series) {
      one <- measure_rows[measure_rows$series == series_name]
      .uir_assert(
        nrow(one) == length(expected_dates) &&
          identical(format(sort(one$date), "%Y-%m"), expected_text),
        "inflation_rates.csv ", measure_name, "/", series_name,
        " does not cover every month from ", .uir_format_month(start),
        " to ", .uir_format_month(latest), "."
      )
      .uir_assert(
        all(is.finite(one$value)),
        "inflation_rates.csv ", measure_name, "/", series_name,
        " has non-finite values."
      )
    }
    latest_rows <- measure_rows[measure_rows$date == latest]
    .uir_assert(
      nrow(latest_rows) == length(.uir_expected_series) &&
        all(is.finite(latest_rows$value)),
      "inflation_rates.csv ", measure_name,
      " does not have finite values for all four series in ",
      .uir_format_month(latest), "."
    )
  }

  for (measure_name in .uir_expected_measures) {
    column <- .uir_measure_columns[[measure_name]]
    expected <- latest_preferred[[column]][[1L]]
    observed <- rates[
      rates$measure == measure_name &
        rates$series == "market_ex_housing_tm16" &
        rates$date == latest,
      value
    ]
    .uir_assert(
      length(observed) == 1L && abs(observed - expected) <= 1e-10,
      "inflation_rates.csv ", measure_name,
      " does not reproduce the preferred measure's latest value."
    )
  }

  summary_path <- file.path(data_dir, "run_summary.csv")
  summary <- data.table::fread(summary_path, data.table = TRUE)
  .uir_assert(nrow(summary) == 1L, "run_summary.csv must have exactly one row.")
  summary_required <- c(
    "latest_month", "components", .uir_start_columns, "figures", "refresh"
  )
  .uir_assert(
    all(summary_required %in% names(summary)),
    "run_summary.csv is missing required column(s): ",
    paste(setdiff(summary_required, names(summary)), collapse = ", "), "."
  )
  .uir_assert(
    as.integer(summary$components[[1L]]) == .uir_expected_components,
    "run_summary.csv components is ", summary$components[[1L]],
    "; expected ", .uir_expected_components, "."
  )
  .uir_assert(
    identical(
      trimws(as.character(summary$latest_month[[1L]])),
      .uir_format_month(latest)
    ),
    "run_summary.csv latest_month is ", summary$latest_month[[1L]],
    "; expected ", .uir_format_month(latest), "."
  )
  for (measure in .uir_expected_measures) {
    column <- .uir_start_columns[[measure]]
    .uir_assert(
      identical(
        trimws(as.character(summary[[column]][[1L]])),
        .uir_format_month(starts[[measure]])
      ),
      "run_summary.csv ", column, " is ", summary[[column]][[1L]],
      "; expected ", .uir_format_month(starts[[measure]]), "."
    )
  }
  .uir_assert(
    as.integer(summary$figures[[1L]]) == length(.uir_expected_figure_files),
    "run_summary.csv figures is ", summary$figures[[1L]], "; expected 3."
  )
  summary_refresh <- summary$refresh[[1L]]
  if (is.character(summary_refresh)) {
    summary_refresh <- toupper(trimws(summary_refresh)) == "TRUE"
  }
  .uir_assert(
    isTRUE(as.logical(summary_refresh)) == refresh,
    "run_summary.csv refresh flag does not match this run."
  )

  figure_files <- list.files(figures_dir, all.files = FALSE, no.. = TRUE)
  .uir_assert(
    length(figure_files) == length(.uir_expected_figure_files) &&
      setequal(figure_files, .uir_expected_figure_files),
    "Figure directory must contain exactly the three expected PNGs; found: ",
    .uir_short(figure_files, 10L), "."
  )
  png_files <- figure_files[grepl("\\.png$", figure_files, ignore.case = TRUE)]
  .uir_assert(
    length(png_files) == 3L &&
      setequal(png_files, .uir_expected_figure_files),
    "Figure directory must contain exactly three PNG files."
  )
  for (file_name in .uir_expected_figure_files) {
    path <- file.path(figures_dir, file_name)
    info <- file.info(path)
    .uir_assert(
      !is.na(info$size) && info$size > 0,
      "Figure is missing or empty: ", path
    )
    .uir_assert(
      .uir_png_valid(path),
      "Figure is not a valid PNG: ", path
    )
  }

  list(
    data_files = data_files,
    figure_files = figure_files,
    rates_rows = nrow(rates),
    components = as.integer(summary$components[[1L]]),
    latest_month = latest,
    preferred_latest_yoy = as.numeric(latest_preferred$yoy[[1L]])
  )
}

.uir_commit_outputs <- function(stage_data, stage_figures, data_dir, figures_dir,
                                validate) {
  output_dir <- dirname(data_dir)
  backup_root <- tempfile(".backup-", tmpdir = output_dir)
  if (!dir.create(backup_root, recursive = TRUE, showWarnings = FALSE)) {
    .uir_stop("Could not create output backup directory in ", output_dir, ".")
  }
  on.exit(unlink(backup_root, recursive = TRUE, force = TRUE), add = TRUE)

  specs <- list(
    data = list(
      staged = stage_data, target = data_dir,
      backup = file.path(backup_root, "data")
    ),
    figures = list(
      staged = stage_figures, target = figures_dir,
      backup = file.path(backup_root, "figures")
    )
  )
  state <- list(
    data = list(backed_up = FALSE, installed = FALSE),
    figures = list(backed_up = FALSE, installed = FALSE)
  )

  restore <- function() {
    for (name in rev(names(specs))) {
      spec <- specs[[name]]
      item <- state[[name]]
      if (isTRUE(item$installed) && dir.exists(spec$target)) {
        unlink(spec$target, recursive = TRUE, force = TRUE)
      }
      if (isTRUE(item$backed_up) && dir.exists(spec$backup)) {
        file.rename(spec$backup, spec$target)
      }
    }
  }

  tryCatch({
    for (name in names(specs)) {
      spec <- specs[[name]]
      if (dir.exists(spec$target)) {
        if (!file.rename(spec$target, spec$backup)) {
          .uir_stop("Could not move the previous ", name, " output aside.")
        }
        state[[name]]$backed_up <- TRUE
      } else if (file.exists(spec$target)) {
        .uir_stop("Output path is not a directory: ", spec$target)
      }
      if (!file.rename(spec$staged, spec$target)) {
        .uir_stop("Could not install the staged ", name, " output.")
      }
      state[[name]]$installed <- TRUE
    }
    validate(data_dir, figures_dir)
  }, error = function(e) {
    restore()
    .uir_stop("Could not publish outputs: ", conditionMessage(e))
  })
}

.uir_parent_reference <- function(root, latest, actual) {
  candidates <- unique(c(
    file.path(root, "output", "data", "measures_yoy.csv"),
    file.path(dirname(root), "output", "data", "measures_yoy.csv")
  ))
  existing <- candidates[file.exists(candidates)]
  if (!length(existing)) {
    return(NULL)
  }
  path <- existing[[1L]]
  reference <- tryCatch(
    data.table::fread(path, data.table = TRUE),
    error = function(e) NULL
  )
  if (is.null(reference) ||
      !all(c("variant", "date", "yoy") %in% names(reference))) {
    return(NULL)
  }
  dates <- tryCatch(as.Date(reference$date), error = function(e) NULL)
  if (is.null(dates) || anyNA(dates)) {
    return(NULL)
  }
  rows <- which(
    trimws(as.character(reference$variant)) == "tm16_market_ex_housing" &
      dates == as.Date(latest)
  )
  if (length(rows) != 1L) {
    return(NULL)
  }
  expected <- suppressWarnings(as.numeric(reference$yoy[rows[[1L]]]))
  if (!is.finite(expected)) {
    return(NULL)
  }
  list(
    path = path,
    expected = expected,
    difference = actual - expected
  )
}

main <- function(refresh, root) {
  if (!requireNamespace("data.table", quietly = TRUE)) {
    .uir_stop("Package 'data.table' is required.")
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    .uir_stop("Package 'ggplot2' is required.")
  }

  output_dir <- file.path(root, "output")
  if (file.exists(output_dir) && !dir.exists(output_dir)) {
    .uir_stop("Output path is not a directory: ", output_dir)
  }
  if (!dir.exists(output_dir) &&
      !dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)) {
    .uir_stop("Could not create output directory: ", output_dir)
  }
  data_dir <- file.path(output_dir, "data")
  figures_dir <- file.path(output_dir, "figures")

  stage_root <- tempfile(".stage-", tmpdir = output_dir)
  if (!dir.create(stage_root, recursive = TRUE, showWarnings = FALSE)) {
    .uir_stop("Could not create output staging directory in ", output_dir, ".")
  }
  on.exit(unlink(stage_root, recursive = TRUE, force = TRUE), add = TRUE)
  stage_data <- file.path(stage_root, "data")
  stage_figures <- file.path(stage_root, "figures")
  dir.create(stage_data, recursive = TRUE, showWarnings = FALSE)
  dir.create(stage_figures, recursive = TRUE, showWarnings = FALSE)

  inputs <- fetch_bea_inputs(start_year = 1987L, refresh = refresh)
  dallas <- fetch_dallas_data(refresh = refresh)
  if (!is.list(inputs) || !all(c("price", "nominal") %in% names(inputs))) {
    .uir_stop("fetch_bea_inputs() did not return price and nominal tables.")
  }

  components_path <- file.path(root, "config", "components.csv")
  components <- load_component_crosswalk(components_path)
  selected_components <- data.table::uniqueN(
    components[market_based & !housing, component_id]
  )
  .uir_assert(
    selected_components == .uir_expected_components,
    "The component lock selects ", selected_components,
    " market-based, non-housing components; expected ",
    .uir_expected_components, "."
  )

  preferred <- construct_market_ex_housing_tm16(
    inputs$price, inputs$nominal, components_path = components_path
  )
  preferred <- data.table::as.data.table(preferred)
  .uir_assert(
    identical(names(preferred), .uir_preferred_columns),
    "construct_market_ex_housing_tm16() returned unexpected columns: ",
    paste(names(preferred), collapse = ", "), "."
  )
  preferred[, date := as.Date(date)]
  .uir_assert_monthly(preferred$date, "Preferred measure")
  data.table::setorder(preferred, date)
  .uir_assert(
    min(preferred$date) == as.Date("1987-01-01"),
    "Preferred measure starts in ", .uir_format_month(min(preferred$date)),
    "; expected 1987-01."
  )

  benchmark_headline <- .uir_official_index(
    inputs$price, "DPCERG", "Personal consumption expenditures", "Headline PCE"
  )
  benchmark_core <- .uir_official_index(
    inputs$price, "DPCCRG", "PCE excluding food and energy", "Core PCE"
  )

  dallas_index <- .uir_dallas_index(dallas)
  dallas_yoy_rate <- data.table::data.table(
    date = as.Date(dallas$date),
    value = as.numeric(dallas$dallas_yoy)
  )

  preferred_rate_tables <- lapply(
    .uir_measure_columns, function(column) .uir_preferred_rate(preferred, column)
  )
  names(preferred_rate_tables) <- names(.uir_measure_columns)

  start_numbers <- vapply(
    .uir_expected_measures,
    function(measure) {
      table <- preferred_rate_tables[[measure]]
      valid <- table$date[is.finite(table$value)]
      if (!length(valid)) {
        .uir_stop("Preferred measure ", measure, " has no valid rate.")
      }
      as.numeric(min(valid))
    },
    numeric(1L)
  )
  starts <- as.Date(start_numbers, origin = "1970-01-01")
  names(starts) <- .uir_expected_measures
  for (measure in .uir_expected_measures) {
    .uir_assert(
      starts[[measure]] == .uir_expected_starts[[measure]],
      "Preferred ", measure, " starts in ", .uir_format_month(starts[[measure]]),
      "; expected ", .uir_format_month(.uir_expected_starts[[measure]]), "."
    )
  }

  latest <- as.Date(min(
    max(benchmark_headline$date),
    max(benchmark_core$date),
    max(dallas$date)
  ))
  .uir_assert(
    max(preferred$date) >= latest,
    "Preferred measure ends in ", .uir_format_month(max(preferred$date)),
    ", before the common latest official month ",
    .uir_format_month(latest), "."
  )
  .uir_assert(
    max(benchmark_headline$date) >= latest &&
      max(benchmark_core$date) >= latest &&
      max(dallas$date) >= latest,
    "Official inputs do not cover the common latest month."
  )
  .uir_assert(
    latest %in% preferred$date,
    "Preferred measure has no observation for ", .uir_format_month(latest), "."
  )
  for (measure in .uir_expected_measures) {
    .uir_assert(
      latest >= starts[[measure]],
      "The common latest month precedes the ", measure, " start."
    )
  }

  headline_rates <- list(
    yoy = .uir_index_rate(benchmark_headline, 12L, "Headline PCE"),
    six_month_annualized = .uir_index_rate(benchmark_headline, 6L, "Headline PCE"),
    three_month_annualized = .uir_index_rate(benchmark_headline, 3L, "Headline PCE")
  )
  core_rates <- list(
    yoy = .uir_index_rate(benchmark_core, 12L, "Core PCE"),
    six_month_annualized = .uir_index_rate(benchmark_core, 6L, "Core PCE"),
    three_month_annualized = .uir_index_rate(benchmark_core, 3L, "Core PCE")
  )
  dallas_rates <- list(
    yoy = dallas_yoy_rate,
    six_month_annualized = .uir_index_rate(dallas_index, 6L, "Dallas"),
    three_month_annualized = .uir_index_rate(dallas_index, 3L, "Dallas")
  )

  rate_sources <- list(
    yoy = list(
      headline = headline_rates$yoy,
      core = core_rates$yoy,
      dallas = dallas_rates$yoy,
      market_ex_housing_tm16 = preferred_rate_tables$yoy
    ),
    six_month_annualized = list(
      headline = headline_rates$six_month_annualized,
      core = core_rates$six_month_annualized,
      dallas = dallas_rates$six_month_annualized,
      market_ex_housing_tm16 = preferred_rate_tables$six_month_annualized
    ),
    three_month_annualized = list(
      headline = headline_rates$three_month_annualized,
      core = core_rates$three_month_annualized,
      dallas = dallas_rates$three_month_annualized,
      market_ex_housing_tm16 = preferred_rate_tables$three_month_annualized
    )
  )

  rates <- lapply(
    .uir_expected_measures,
    function(measure) {
      .uir_assemble_measure(
        measure, rate_sources[[measure]], starts[[measure]], latest
      )
    }
  )
  names(rates) <- .uir_expected_measures
  for (measure in .uir_expected_measures) {
    .uir_assert(
      min(rates[[measure]]$date) == starts[[measure]] &&
        max(rates[[measure]]$date) == latest,
      measure, " rate table does not span its required start and latest month."
    )
  }

  preferred_out <- preferred[
    date <= latest,
    .(date, monthly, index, yoy, annualized_6m, annualized_3m)
  ]
  preferred_path <- file.path(stage_data, "preferred_measure.csv")
  data.table::fwrite(preferred_out, preferred_path, na = "NA")

  long_rates <- data.table::rbindlist(
    lapply(
      .uir_expected_measures,
      function(measure) {
        table <- rates[[measure]]
        data.table::data.table(
          measure = measure, series = table$series,
          date = table$date, value = table$value
        )
      }
    ),
    use.names = TRUE
  )
  long_rates[, measure := factor(measure, levels = .uir_expected_measures)]
  long_rates[, series := factor(series, levels = .uir_expected_series)]
  data.table::setorder(long_rates, measure, series, date)
  long_rates[, `:=`(
    measure = as.character(measure), series = as.character(series)
  )]
  inflation_rates_path <- file.path(stage_data, "inflation_rates.csv")
  data.table::fwrite(long_rates, inflation_rates_path, na = "NA")

  summary <- data.table::data.table(
    latest_month = .uir_format_month(latest),
    components = as.integer(selected_components),
    yoy_start = .uir_format_month(starts[["yoy"]]),
    six_month_annualized_start =
      .uir_format_month(starts[["six_month_annualized"]]),
    three_month_annualized_start =
      .uir_format_month(starts[["three_month_annualized"]]),
    figures = length(.uir_expected_figure_files),
    refresh = refresh
  )
  summary_path <- file.path(stage_data, "run_summary.csv")
  data.table::fwrite(summary, summary_path, na = "NA")

  figure_paths <- create_comparison_graphs(rates, stage_figures)
  .uir_assert(
    length(figure_paths) == length(.uir_expected_figure_files) &&
      setequal(basename(unname(figure_paths)), .uir_expected_figure_files),
    "create_comparison_graphs() did not return the three expected PNGs."
  )

  .uir_validate_outputs(stage_data, stage_figures, latest, starts, refresh)
  validation <- .uir_commit_outputs(
    stage_data, stage_figures, data_dir, figures_dir,
    function(published_data, published_figures) {
      .uir_validate_outputs(
        published_data, published_figures, latest, starts, refresh
      )
    }
  )

  reference <- .uir_parent_reference(
    root, latest, validation$preferred_latest_yoy
  )
  if (!is.null(reference)) {
    reference_ok <- abs(reference$difference) <= 1e-6
    if (!reference_ok) {
      warning(sprintf(
        paste0(
          "Preferred latest YoY differs from the existing parent reference ",
          "%.6f by %.6g pp."
        ),
        reference$expected, reference$difference
      ), call. = FALSE)
    }
  } else {
    reference_ok <- NA
  }

  cat("Underlying-inflation standalone run complete\n")
  cat("  root: ", root, "\n", sep = "")
  cat("  latest official month: ", .uir_format_month(latest), "\n", sep = "")
  cat("  components: ", validation$components, "\n", sep = "")
  cat(
    "  rate starts: yoy ", .uir_format_month(starts[["yoy"]]),
    ", 6m ", .uir_format_month(starts[["six_month_annualized"]]),
    ", 3m ", .uir_format_month(starts[["three_month_annualized"]]), "\n",
    sep = ""
  )
  cat(sprintf(
    "  preferred latest YoY: %.6f%%\n", validation$preferred_latest_yoy
  ))
  cat(
    "  outputs: ", length(validation$data_files), " CSVs, ",
    length(validation$figure_files), " PNGs\n", sep = ""
  )
  cat("  data: ", data_dir, "\n", sep = "")
  cat("  figures: ", figures_dir, "\n", sep = "")
  cat("  refresh: ", if (refresh) "TRUE" else "FALSE", "\n", sep = "")
  if (!is.null(reference)) {
    cat(sprintf(
      "  parent reference YoY: %.6f (%s; diff %+.2e pp)\n",
      reference$expected,
      if (isTRUE(reference_ok)) "match" else "MISMATCH",
      reference$difference
    ))
  }
  invisible(validation)
}

refresh <- .uir_parse_args(commandArgs(trailingOnly = TRUE))
root <- .uir_project_root()
options(underlying.project_root = root)
source(file.path(root, "R", "bea_fred_data.R"), local = .GlobalEnv)
source(file.path(root, "R", "market_ex_housing_tm16.R"), local = .GlobalEnv)
source(file.path(root, "R", "plots.R"), local = .GlobalEnv)

main(refresh, root)
