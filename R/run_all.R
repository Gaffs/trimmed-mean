# Top-level orchestration for the trimmed-mean pipeline.
#
# Run from the project root:
#   Rscript R/run_all.R            use the automatic BEA cache logic
#   Rscript R/run_all.R --refresh  refetch every BEA chunk and the FRED benchmark
#
# Outputs are built under output/.stage-* and validated before they replace
# output/data and output/figures. A calculation or validation failure leaves the
# previously published outputs untouched.

root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
if (!file.exists(file.path(root, "R", "bea_data.R"))) {
  stop("Run from the project root: Rscript R/run_all.R", call. = FALSE)
}

source(file.path(root, "R", "bea_data.R"))
source(file.path(root, "R", "components.R"))
source(file.path(root, "R", "trimmed_mean.R"))
source(file.path(root, "R", "plots.R"))

.run_expected_components <- 192L
.run_expected_lines <- 194L
.run_expected_variants <- c(
  "tm16", "tm32", "tm48", "tm64", "median",
  "tm16_ex_housing", "tm16_market", "tm16_market_ex_housing",
  "dallas_replication"
)
.run_expected_figures <- c(
  "01_benchmarks_dallas",
  "02_dallas_16_core",
  "03_trim_ladder",
  "04_latest_distribution",
  "05_housing",
  "06_market_based",
  "07_market_ex_housing"
)
.run_min_dallas_months <- 100L
.run_max_dallas_mean_abs_pp <- 0.08
.run_max_dallas_max_abs_pp <- 0.20
.run_freshness_limit_months <- 3L
.run_yoy_limit_pp <- 20
.run_source_groups <- c(
  "Durable goods", "Nondurable goods", "Services", "NPISH"
)
.run_png_signature <- as.raw(c(
  0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a
))

.run_stop <- function(...) {
  stop(..., call. = FALSE)
}

.run_assert <- function(condition, ...) {
  if (!isTRUE(condition)) {
    .run_stop(...)
  }
  invisible(TRUE)
}

.run_month_id <- function(x) {
  as.integer(format(x, "%Y")) * 12L + as.integer(format(x, "%m"))
}

.run_month_start <- function(x) {
  as.Date(format(x, "%Y-%m-01"))
}

.run_format_month <- function(x) {
  format(x, "%Y-%m")
}

.run_format_date <- function(x) {
  format(x, "%Y-%m-%d")
}

.run_parse_month <- function(x, what) {
  text <- trimws(as.character(x))
  out <- rep(as.Date(NA_character_), length(text))
  short <- grepl("^[0-9]{4}-[0-9]{2}$", text)
  full <- grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}", text)
  if (any(short)) {
    out[short] <- as.Date(paste0(text[short], "-01"))
  }
  if (any(full)) {
    out[full] <- as.Date(substr(text[full], 1L, 10L))
  }
  if (anyNA(out)) {
    .run_stop(
      what, " must contain calendar months such as YYYY-MM or YYYY-MM-DD; ",
      "found: ", paste(utils::head(text[is.na(out)], 5L), collapse = ", ")
    )
  }
  out
}

.run_parse_args <- function(args) {
  unknown <- setdiff(args, "--refresh")
  if (length(unknown) > 0L) {
    .run_stop(
      "Unknown argument(s): ", paste(unknown, collapse = ", "),
      ". Usage: Rscript R/run_all.R [--refresh]"
    )
  }
  "--refresh" %in% args
}

.run_assert_file <- function(path, label) {
  if (!file.exists(path)) {
    .run_stop(label, " does not exist: ", path)
  }
  info <- file.info(path)
  if (is.na(info$size) || info$size <= 0) {
    .run_stop(label, " is empty: ", path)
  }
  invisible(info)
}

.run_hash_file <- function(path) {
  .run_assert_file(path, "Hash input")
  if (requireNamespace("digest", quietly = TRUE)) {
    value <- tryCatch(
      unname(digest::digest(file = path, algo = "sha256")),
      error = function(e) NA_character_
    )
    if (!is.na(value) && nzchar(value)) {
      return(list(hash_algo = "sha256", hash = value))
    }
  }
  md5 <- tryCatch(
    unname(tools::md5sum(path)),
    error = function(e) NA_character_
  )
  if (is.na(md5) || !nzchar(md5)) {
    .run_stop("Could not hash ", path)
  }
  list(hash_algo = "md5", hash = md5)
}

.run_file_entry <- function(path, display_path) {
  info <- .run_assert_file(path, "Output file")
  hash <- .run_hash_file(path)
  list(
    path = gsub("\\\\", "/", display_path),
    bytes = as.numeric(info$size),
    hash_algo = hash$hash_algo,
    hash = hash$hash
  )
}

.run_relative_path <- function(path, root) {
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  prefix <- paste0(root, "/")
  if (startsWith(path, prefix)) {
    substring(path, nchar(prefix) + 1L)
  } else {
    path
  }
}

.run_rows <- function(x) {
  if (is.null(x) || nrow(x) == 0L) return(list())
  columns <- names(x)
  unname(lapply(seq_len(nrow(x)), function(i) {
    row <- lapply(columns, function(column) {
      value <- x[[column]][i]
      if (inherits(value, "Date")) {
        return(.run_format_date(value))
      }
      if (inherits(value, "POSIXt")) {
        return(format(value, "%Y-%m-%dT%H:%M:%S%z"))
      }
      value
    })
    names(row) <- columns
    row
  }))
}

.run_provenance <- function(x) {
  provenance <- attr(x, "bea_provenance", exact = TRUE)
  if (is.null(provenance)) {
    return(list(available = FALSE))
  }
  files <- provenance$files
  file_columns <- c(
    "file", "table", "year_min", "year_max", "first_period", "latest_period",
    "row_count", "hash_algo", "hash", "metadata_source", "reused"
  )
  file_rows <- if (is.data.frame(files) && nrow(files) > 0L) {
    columns <- intersect(file_columns, names(files))
    .run_rows(files[, columns, with = FALSE])
  } else {
    list()
  }
  out <- list(
    available = TRUE,
    source = as.character(provenance$source),
    pulled_at = if (is.null(provenance$pulled_at_text)) {
      NA_character_
    } else {
      as.character(provenance$pulled_at_text)
    },
    pulled_at_source = if (is.null(provenance$pulled_at_source)) {
      NA_character_
    } else {
      as.character(provenance$pulled_at_source)
    },
    metadata_sources = as.list(as.character(provenance$metadata_sources)),
    cache_files = as.integer(provenance$cache_files),
    files = file_rows
  )
  if (!is.null(provenance$table)) {
    out$table <- as.character(provenance$table)
  }
  if (!is.null(provenance$requested_years)) {
    out$years <- as.integer(provenance$requested_years)
  }
  if (!is.null(provenance$series)) {
    out$series <- as.character(provenance$series)
  }
  out
}

.run_package_version <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    return(NA_character_)
  }
  tryCatch(
    as.character(utils::packageVersion(package)),
    error = function(e) NA_character_
  )
}

.run_package_versions <- function() {
  packages <- c("data.table", "ggplot2", "jsonlite", "digest", "bea.R")
  stats::setNames(lapply(packages, .run_package_version), packages)
}

.run_git_info <- function(root) {
  unavailable <- list(
    available = FALSE,
    commit = NA_character_,
    branch = NA_character_,
    dirty = NA,
    status = list(),
    status_truncated = NA
  )
  git <- unname(Sys.which("git"))
  if (!nzchar(git)) return(unavailable)

  run <- function(args) {
    tryCatch(
      suppressWarnings(
        system2(git, args, stdout = TRUE, stderr = FALSE)
      ),
      error = function(e) NULL
    )
  }
  commit <- run(c("rev-parse", "HEAD"))
  if (is.null(commit) || length(commit) != 1L ||
      !is.null(attr(commit, "status"))) {
    return(unavailable)
  }
  branch <- run(c("rev-parse", "--abbrev-ref", "HEAD"))
  status <- run(c("status", "--porcelain"))
  status_ok <- !is.null(status) &&
    (is.null(attr(status, "status")) || attr(status, "status") == 0L)
  list(
    available = TRUE,
    commit = trimws(commit),
    branch = if (is.null(branch) || length(branch) != 1L ||
                 !is.null(attr(branch, "status"))) {
      NA_character_
    } else {
      trimws(branch)
    },
    dirty = if (status_ok) length(status) > 0L else NA,
    status = if (status_ok) as.list(utils::head(status, 50L)) else list(),
    status_truncated = if (status_ok) length(status) > 50L else NA
  )
}

.run_lock_dir <- function(root) {
  dir <- Sys.getenv("PCE_LOCK_DIR", unset = "")
  if (!nzchar(dir)) {
    dir <- getOption("pce.lock_dir", "")
    if (is.null(dir)) dir <- ""
  }
  if (!nzchar(dir)) {
    dir <- file.path(root, "config")
  }
  dir <- path.expand(dir)
  if (!grepl("^([A-Za-z]:[/\\\\]|[/\\\\])", dir)) {
    dir <- file.path(root, dir)
  }
  if (!dir.exists(dir)) {
    .run_stop("Lock directory does not exist: ", dir)
  }
  normalizePath(dir, winslash = "/", mustWork = TRUE)
}

.run_read_crosswalk_lock <- function(path) {
  .run_assert_file(path, "Crosswalk lock")
  lock <- tryCatch(
    data.table::fread(path, data.table = TRUE, na.strings = c("NA", "")),
    error = function(e) {
      .run_stop("Could not read crosswalk lock ", path, ": ",
                conditionMessage(e))
    }
  )
  required <- c(
    "component_id", "component_line", "line_number", "line_order",
    "price_line", "is_price_source", "component_description",
    "line_description", "price_series_code", "nominal_series_code",
    "housing", "market_based", "source_group", "dallas_name"
  )
  missing <- setdiff(required, names(lock))
  if (length(missing) > 0L) {
    .run_stop(
      "Crosswalk lock ", path, " is missing required column(s): ",
      paste(missing, collapse = ", "), "."
    )
  }

  for (column in c("line_number", "line_order", "price_line")) {
    value <- suppressWarnings(as.integer(as.character(lock[[column]])))
    if (anyNA(value)) {
      .run_stop("Crosswalk lock ", path, " has non-integer `", column,
                "` values.")
    }
    data.table::set(lock, j = column, value = value)
  }
  for (column in c("is_price_source", "housing", "market_based")) {
    value <- lock[[column]]
    if (is.logical(value)) {
      if (anyNA(value)) {
        .run_stop("Crosswalk lock ", path, " has missing `", column,
                  "` values.")
      }
    } else {
      text <- toupper(trimws(as.character(value)))
      if (anyNA(text) || !all(text %in% c("TRUE", "FALSE"))) {
        .run_stop("Crosswalk lock ", path, " has invalid `", column,
                  "` values.")
      }
      data.table::set(lock, j = column, value = text == "TRUE")
    }
  }
  for (column in c(
    "component_id", "component_line", "component_description",
    "line_description", "nominal_series_code", "source_group", "dallas_name"
  )) {
    value <- trimws(as.character(lock[[column]]))
    if (anyNA(value) || any(!nzchar(value))) {
      .run_stop("Crosswalk lock ", path, " has empty `", column, "` values.")
    }
    data.table::set(lock, j = column, value = value)
  }
  lock$price_series_code <- trimws(as.character(lock$price_series_code))
  lock[!is.na(lock$price_series_code) & !nzchar(lock$price_series_code),
       price_series_code := NA_character_]

  .run_assert(
    nrow(lock) == .run_expected_lines,
    "Crosswalk lock has ", nrow(lock), " expanded lines; expected ",
    .run_expected_lines, "."
  )
  .run_assert(
    data.table::uniqueN(lock$component_id) == .run_expected_components,
    "Crosswalk lock has ", data.table::uniqueN(lock$component_id),
    " components; expected ", .run_expected_components, "."
  )
  .run_assert(
    data.table::uniqueN(lock$line_number) == .run_expected_lines,
    "Crosswalk lock assigns BEA lines more than once."
  )
  .run_assert(
    !any(lock$housing & !lock$market_based),
    "Crosswalk lock flags a housing component as not market-based."
  )
  .run_assert(
    all(lock$source_group %in% .run_source_groups),
    "Crosswalk lock has unknown source_group value(s): ",
    paste(setdiff(unique(lock$source_group), .run_source_groups),
          collapse = ", "), "."
  )
  .run_assert(
    !any(lock$is_price_source & is.na(lock$price_series_code)),
    "Crosswalk lock has a price source line without a price series code."
  )

  lock <- lock[order(lock$component_id, lock$line_order)]
  price_counts <- lock[, .(
    n_price = sum(is_price_source),
    n_price_lines = data.table::uniqueN(price_line)
  ), by = component_id]
  .run_assert(
    all(price_counts$n_price == 1L & price_counts$n_price_lines == 1L),
    "Every crosswalk component must have exactly one price source line."
  )
  canonical <- lock[, .(
    expected_line = paste(line_number, collapse = "+"),
    component_line = component_line[1L]
  ), by = component_id]
  .run_assert(
    all(canonical$expected_line == canonical$component_line),
    "Crosswalk lock component_line does not match its expanded line numbers."
  )
  lock[]
}

.run_resolve_price_series <- function(price, code, label,
                                      expected_description) {
  data <- data.table::as.data.table(price)
  if (!all(c("line_number", "series_code", "description", "date", "value") %in%
           names(data))) {
    .run_stop(
      "BEA price data is missing columns needed to resolve ", label, "."
    )
  }

  normalized_code <- toupper(trimws(as.character(data$series_code)))
  normalized_code <- sub("-M$", "", normalized_code)
  selected <- normalized_code == code
  if (!any(selected)) {
    .run_stop(
      "Could not resolve ", label, " from BEA price series code ", code, "."
    )
  }
  rows <- data[selected]
  line_numbers <- unique(rows$line_number)
  .run_assert(
    length(line_numbers) == 1L,
    label, " series code ", code, " maps to ", length(line_numbers),
    " BEA lines; expected exactly one."
  )
  rows <- rows[order(rows$date)]
  .run_assert(
    !anyNA(rows$date),
    label, " (", code, ") has missing dates."
  )
  .run_assert(
    anyDuplicated(rows$date) == 0L,
    label, " (", code, ") has duplicate date rows; expected exactly one row ",
    "per date."
  )

  dates <- as.Date(rows$date)
  .run_assert(
    all(format(dates, "%d") == "01"),
    label, " (", code, ") contains non-monthly dates."
  )
  descriptions <- unique(.run_normalize_description(rows$description))
  .run_assert(
    length(descriptions) == 1L,
    label, " (", code, ") has multiple normalized descriptions: ",
    paste(descriptions, collapse = " / "), "."
  )
  .run_assert(
    identical(descriptions, expected_description),
    label, " (", code, ") description is '", descriptions,
    "'; expected '", expected_description, "'."
  )

  month_id <- .run_month_id(dates)
  .run_assert(
    anyDuplicated(month_id) == 0L,
    label, " (", code, ") has duplicate calendar months."
  )
  if (length(month_id) > 1L) {
    gaps <- which(diff(month_id) != 1L)
    .run_assert(
      length(gaps) == 0L,
      label, " (", code, ") is not monthly-contiguous; a month is missing ",
      "before ", .run_format_date(dates[gaps[1L] + 1L]), "."
    )
  }
  .run_assert(
    all(is.finite(rows$value)) && all(rows$value > 0),
    label, " (", code, ") has non-finite or non-positive price index values."
  )

  # Exact calendar month join: the prior observation is the row whose month_id
  # is exactly twelve months earlier. This is not a row shift, so gaps would
  # produce NA rather than silently comparing adjacent rows.
  prior_index <- match(month_id - 12L, month_id)
  prior_value <- rows$value[prior_index]
  first_year <- month_id < min(month_id) + 12L
  .run_assert(
    !any(is.na(prior_value) & !first_year),
    label, " (", code, ") is missing the exact month_id - 12 observation for ",
    "at least one month after its first year; cannot compute YoY by calendar ",
    "month."
  )
  yoy <- 100 * (rows$value / prior_value - 1)
  .run_assert(
    all(is.finite(yoy) | first_year),
    label, " (", code, ") produced non-finite year-over-year values."
  )
  usable <- !is.na(yoy)
  .run_assert(any(usable), label, " (", code, ") has no usable YoY values.")
  bad <- which(usable & abs(yoy) >= .run_yoy_limit_pp)
  if (length(bad) > 0L) {
    worst <- bad[which.max(abs(yoy[bad]))]
    .run_stop(
      label, " (", code, ") has an implausible year-over-year change of ",
      sprintf("%+.2f%%", yoy[worst]), " in ",
      .run_format_month(dates[worst]), "; expected abs(YoY) < ",
      .run_yoy_limit_pp, " pp."
    )
  }

  data.table::data.table(
    series = label,
    date = dates,
    month_id = month_id,
    value = as.numeric(yoy)
  )
}

.run_normalize_description <- function(x) {
  gsub("[[:space:]]+", " ", tolower(trimws(as.character(x))))
}

.run_read_prior_summary <- function(root) {
  path <- file.path(root, "output", "data", "run_summary.csv")
  if (!file.exists(path)) {
    return(list(path = path, exists = FALSE, latest = NULL))
  }
  summary <- tryCatch(
    data.table::fread(path, data.table = TRUE),
    error = function(e) {
      .run_stop("Could not read prior run summary ", path, ": ",
                conditionMessage(e))
    }
  )
  if (!"latest_month" %in% names(summary)) {
    .run_stop(
      "Prior run summary ", path, " has no latest_month column; cannot verify ",
      "that the latest month does not regress."
    )
  }
  .run_assert(
    nrow(summary) == 1L,
    "Prior run summary ", path, " has ", nrow(summary),
    " rows; expected exactly one."
  )
  latest <- .run_parse_month(
    summary$latest_month[[1L]],
    paste0("prior run summary ", path, " latest_month")
  )
  list(path = path, exists = TRUE, latest = latest)
}

.run_check_classifications <- function(panel, lock) {
  for (column in c("housing", "market_based")) {
    .run_assert(
      column %in% names(panel) && is.logical(panel[[column]]) &&
        !anyNA(panel[[column]]),
      "Panel classification `", column, "` must be a non-missing logical."
    )
  }
  .run_assert(
    !any(panel$housing & !panel$market_based),
    "Panel flags a housing component as not market-based."
  )
  .run_assert(
    !anyNA(panel$source_group) &&
      all(panel$source_group %in% .run_source_groups),
    "Panel has missing or unknown source_group values."
  )
  .run_assert(
    !anyNA(panel$component_id) && !anyNA(panel$description) &&
      all(nzchar(trimws(as.character(panel$description)))),
    "Panel has missing component identifiers or descriptions."
  )
  .run_assert(
    all(is.finite(panel$price)) && all(is.finite(panel$nominal)) &&
      all(panel$price > 0) && all(panel$nominal >= 0),
    "Panel contains non-finite or out-of-range price/nominal values."
  )
  .run_assert(
    is.logical(lock$housing) && !anyNA(lock$housing) &&
      is.logical(lock$market_based) && !anyNA(lock$market_based),
    "Crosswalk lock classifications must be non-missing logical values."
  )

  panel_meta <- unique(panel[, .(
    component_id, line_number, series_code, description,
    housing, market_based, source_group
  )])
  .run_assert(
    nrow(panel_meta) == .run_expected_components,
    "Panel has ", nrow(panel_meta), " distinct component metadata rows; ",
    "expected ", .run_expected_components, "."
  )
  lock_meta <- unique(lock[, .(
    component_id, component_line, housing, market_based, source_group
  )])
  index <- match(panel_meta$component_id, lock_meta$component_id)
  .run_assert(
    !anyNA(index),
    "Panel components do not match the crosswalk lock."
  )
  .run_assert(
    all(as.character(panel_meta$line_number) ==
          lock_meta$component_line[index]),
    "Panel component line numbers do not match the crosswalk lock."
  )
  .run_assert(
    all(panel_meta$housing == lock_meta$housing[index]) &&
      all(panel_meta$market_based == lock_meta$market_based[index]) &&
      all(panel_meta$source_group == lock_meta$source_group[index]),
    "Panel classifications do not match the crosswalk lock."
  )

  list(
    housing_missing = sum(is.na(panel$housing)),
    market_based_missing = sum(is.na(panel$market_based)),
    invalid_source_groups = sum(
      !panel$source_group %in% .run_source_groups
    ),
    panel_lock_mismatches = sum(
      as.character(panel_meta$line_number) !=
        lock_meta$component_line[index]
    ) + sum(panel_meta$housing != lock_meta$housing[index]) +
      sum(panel_meta$market_based != lock_meta$market_based[index]),
    passed = TRUE
  )
}

.run_build_crosswalk <- function(panel, lock) {
  panel_meta <- unique(panel[, .(
    component_id, line_number, series_code, description,
    housing, market_based, source_group
  )])
  lock <- lock[order(lock$component_id, lock$line_order)]
  component <- lock[, {
    price_codes <- unique(price_series_code[
      is_price_source & !is.na(price_series_code)
    ])
    nominal_codes <- unique(nominal_series_code[!is.na(nominal_series_code)])
    list(
      line_number = component_line[1L],
      price_line = price_line[1L],
      price_series_code = if (length(price_codes) == 1L) {
        price_codes
      } else {
        NA_character_
      },
      nominal_series_code = paste(nominal_codes, collapse = "+"),
      n_lines = .N,
      housing = housing[1L],
      market_based = market_based[1L],
      source_group = source_group[1L],
      dallas_name = dallas_name[1L]
    )
  }, by = component_id]

  crosswalk <- merge(
    component,
    panel_meta[, .(component_id, series_code, description)],
    by = "component_id", all.x = TRUE, sort = FALSE
  )
  .run_assert(
    nrow(crosswalk) == .run_expected_components &&
      data.table::uniqueN(crosswalk$component_id) == .run_expected_components,
    "Crosswalk export has ", nrow(crosswalk), " rows; expected ",
    .run_expected_components, "."
  )
  .run_assert(
    sum(crosswalk$n_lines) == .run_expected_lines,
    "Crosswalk export covers ", sum(crosswalk$n_lines),
    " expanded lines; expected ", .run_expected_lines, "."
  )
  .run_assert(
    !anyNA(crosswalk$series_code) && !anyNA(crosswalk$description) &&
      !anyNA(crosswalk$price_line) && !anyNA(crosswalk$dallas_name),
    "Crosswalk export has missing series, description, price_line, or ",
    "dallas_name values."
  )
  data.table::setcolorder(crosswalk, c(
    "component_id", "line_number", "price_line", "series_code",
    "description", "price_series_code", "nominal_series_code",
    "housing", "market_based", "source_group", "dallas_name", "n_lines"
  ))
  data.table::setorder(crosswalk, component_id)
  crosswalk[]
}

.run_check_dallas_diagnostic <- function(diagnostics) {
  required <- c("date", "official", "reconstructed", "difference_pp")
  missing <- setdiff(required, names(diagnostics))
  if (length(missing) > 0L) {
    .run_stop(
      "Dallas diagnostic is missing column(s): ",
      paste(missing, collapse = ", "), "."
    )
  }
  complete <- diagnostics[
    is.finite(official) & is.finite(reconstructed)
  ]
  .run_assert(
    nrow(complete) >= .run_min_dallas_months,
    "Dallas diagnostic has ", nrow(complete),
    " complete months; at least ", .run_min_dallas_months, " are required."
  )
  complete[, abs_error_pp := abs(difference_pp)]
  mean_abs <- mean(complete$abs_error_pp)
  max_abs <- max(complete$abs_error_pp)
  worst <- complete[which.max(abs_error_pp)]
  worst_month <- .run_format_month(worst$date[[1L]])
  detail <- sprintf(
    "worst month %s (official %.3f pp, reconstructed %.3f pp, difference %+.3f pp)",
    worst_month,
    worst$official[[1L]],
    worst$reconstructed[[1L]],
    worst$difference_pp[[1L]]
  )
  if (!is.finite(mean_abs) || mean_abs >= .run_max_dallas_mean_abs_pp) {
    .run_stop(
      "Dallas diagnostic mean absolute error is ",
      sprintf("%.4f pp", mean_abs), ", not below ",
      .run_max_dallas_mean_abs_pp, " pp; ", detail, "."
    )
  }
  if (!is.finite(max_abs) || max_abs >= .run_max_dallas_max_abs_pp) {
    .run_stop(
      "Dallas diagnostic maximum absolute error is ",
      sprintf("%.4f pp", max_abs), ", not below ",
      .run_max_dallas_max_abs_pp, " pp; ", detail, "."
    )
  }
  list(
    complete_months = as.integer(nrow(complete)),
    mean_abs_error_pp = mean_abs,
    max_abs_error_pp = max_abs,
    worst_month = worst_month,
    worst_official_pp = worst$official[[1L]],
    worst_reconstructed_pp = worst$reconstructed[[1L]],
    worst_difference_pp = worst$difference_pp[[1L]],
    passed = TRUE
  )
}

.run_read_csv <- function(path, label) {
  .run_assert_file(path, label)
  tryCatch(
    data.table::fread(path, data.table = TRUE, na.strings = c("NA", "")),
    error = function(e) {
      .run_stop("Could not read ", label, " ", path, ": ",
                conditionMessage(e))
    }
  )
}

.run_validate_outputs <- function(stage_data, stage_figures, latest, refresh) {
  checks <- list()

  crosswalk <- .run_read_csv(
    file.path(stage_data, "component_crosswalk.csv"),
    "component crosswalk"
  )
  .run_assert(
    nrow(crosswalk) == .run_expected_components,
    "Published crosswalk has ", nrow(crosswalk), " rows; expected ",
    .run_expected_components, "."
  )
  .run_assert(
    data.table::uniqueN(crosswalk$component_id) ==
      .run_expected_components,
    "Published crosswalk component identifiers are not unique."
  )
  for (column in c("price_line", "dallas_name", "n_lines")) {
    .run_assert(
      column %in% names(crosswalk),
      "Published crosswalk is missing `", column, "`."
    )
  }
  .run_assert(
    !anyNA(crosswalk$price_line) &&
      !anyNA(crosswalk$dallas_name) &&
      !any(!nzchar(trimws(crosswalk$dallas_name))),
    "Published crosswalk has missing price_line or dallas_name values."
  )
  .run_assert(
    sum(as.integer(crosswalk$n_lines)) == .run_expected_lines,
    "Published crosswalk covers ", sum(as.integer(crosswalk$n_lines)),
    " expanded lines; expected ", .run_expected_lines, "."
  )
  .run_assert(
    is.logical(crosswalk$housing) && !anyNA(crosswalk$housing) &&
      is.logical(crosswalk$market_based) &&
      !anyNA(crosswalk$market_based),
    "Published crosswalk has missing classification flags."
  )
  checks$crosswalk <- list(
    rows = nrow(crosswalk),
    components = data.table::uniqueN(crosswalk$component_id),
    expanded_lines = sum(as.integer(crosswalk$n_lines)),
    passed = TRUE
  )

  panel <- .run_read_csv(
    file.path(stage_data, "component_panel.csv"),
    "component panel"
  )
  panel[, date := as.Date(date)]
  months <- data.table::uniqueN(panel$date)
  .run_assert(
    nrow(panel) == .run_expected_components * months,
    "Published panel has ", nrow(panel), " rows; expected ",
    .run_expected_components, " x ", months, "."
  )
  .run_assert(
    data.table::uniqueN(panel$component_id) == .run_expected_components,
    "Published panel does not contain ",
    .run_expected_components, " components."
  )
  .run_assert(
    max(panel$date) == latest,
    "Published panel ends on ", .run_format_date(max(panel$date)),
    "; expected ", .run_format_date(latest), "."
  )
  .run_assert(
    all(is.finite(panel$price)) && all(is.finite(panel$nominal)) &&
      all(panel$price > 0) && all(panel$nominal >= 0),
    "Published panel contains invalid price or nominal values."
  )
  .run_assert(
    is.logical(panel$housing) && !anyNA(panel$housing) &&
      is.logical(panel$market_based) && !anyNA(panel$market_based),
    "Published panel has missing classification flags."
  )
  checks$component_panel <- list(
    rows = nrow(panel),
    components = data.table::uniqueN(panel$component_id),
    months = months,
    passed = TRUE
  )

  diagnostics <- .run_read_csv(
    file.path(stage_data, "dallas_diagnostic.csv"),
    "Dallas diagnostic"
  )
  diagnostic_check <- .run_check_dallas_diagnostic(diagnostics)
  checks$dallas_diagnostic <- list(
    rows = nrow(diagnostics),
    complete_months = diagnostic_check$complete_months,
    mean_abs_error_pp = diagnostic_check$mean_abs_error_pp,
    max_abs_error_pp = diagnostic_check$max_abs_error_pp,
    worst_month = diagnostic_check$worst_month,
    passed = TRUE
  )

  summary <- .run_read_csv(
    file.path(stage_data, "run_summary.csv"),
    "run summary"
  )
  .run_assert(
    nrow(summary) == 1L,
    "Published run summary has ", nrow(summary), " rows; expected one."
  )
  summary_latest <- .run_parse_month(
    summary$latest_month[[1L]],
    "published run_summary latest_month"
  )
  .run_assert(
    summary_latest == latest,
    "Published run summary latest_month is ",
    .run_format_month(summary_latest), "; expected ",
    .run_format_month(latest), "."
  )
  .run_assert(
    as.integer(summary$components[[1L]]) == .run_expected_components,
    "Published run summary component count is ",
    summary$components[[1L]], "; expected ", .run_expected_components, "."
  )
  .run_assert(
    as.integer(summary$expanded_lines[[1L]]) == .run_expected_lines,
    "Published run summary expanded line count is ",
    summary$expanded_lines[[1L]], "; expected ", .run_expected_lines, "."
  )
  .run_assert(
    as.integer(summary$figures[[1L]]) == length(.run_expected_figures),
    "Published run summary figure count is ", summary$figures[[1L]],
    "; expected ", length(.run_expected_figures), "."
  )
  .run_assert(
    is.finite(summary$market_based_share[[1L]]) &&
      summary$market_based_share[[1L]] > 0 &&
      summary$market_based_share[[1L]] < 1,
    "Published run summary has an invalid market-based share."
  )
  summary_refresh <- summary$refresh[[1L]]
  if (is.character(summary_refresh)) {
    summary_refresh <- toupper(trimws(summary_refresh)) == "TRUE"
  }
  .run_assert(
    isTRUE(summary_refresh) == refresh,
    "Published run summary refresh flag does not match this run."
  )
  checks$run_summary <- list(
    rows = nrow(summary),
    latest_month = .run_format_month(summary_latest),
    components = as.integer(summary$components[[1L]]),
    expanded_lines = as.integer(summary$expanded_lines[[1L]]),
    figures = as.integer(summary$figures[[1L]]),
    passed = TRUE
  )

  measures <- .run_read_csv(
    file.path(stage_data, "measures_yoy.csv"),
    "measures"
  )
  measures[, date := as.Date(date)]
  .run_assert(
    setequal(unique(measures$variant), .run_expected_variants),
    "Published measures do not contain exactly the expected variants."
  )
  .run_assert(
    nrow(measures) ==
      length(.run_expected_variants) * data.table::uniqueN(measures$date),
    "Published measures are not a complete variant-by-month grid."
  )
  .run_assert(
    max(measures$date) == latest,
    "Published measures end on ",
    .run_format_date(max(measures$date)), "; expected ",
    .run_format_date(latest), "."
  )
  latest_measures <- measures[measures$date == latest]
  .run_assert(
    nrow(latest_measures) == length(.run_expected_variants) &&
      all(is.finite(latest_measures$yoy)),
    "Published measures do not have finite YoY values for every variant in ",
    .run_format_month(latest), "."
  )
  checks$measures <- list(
    rows = nrow(measures),
    variants = data.table::uniqueN(measures$variant),
    latest_month = .run_format_month(max(measures$date)),
    passed = TRUE
  )

  distribution <- .run_read_csv(
    file.path(stage_data, "latest_distribution.csv"),
    "latest distribution"
  )
  distribution[, date := as.Date(date)]
  .run_assert(
    nrow(distribution) == .run_expected_components &&
      data.table::uniqueN(distribution$component_id) ==
        .run_expected_components,
    "Published distribution does not contain ",
    .run_expected_components, " unique components."
  )
  .run_assert(
    all(distribution$date == latest),
    "Published distribution is not restricted to ",
    .run_format_month(latest), "."
  )
  .run_assert(
    all(is.finite(distribution$pi)) &&
      all(distribution$pi > -1) &&
      all(is.finite(distribution$fisher_weight)) &&
      all(distribution$fisher_weight > 0),
    "Published distribution has invalid pi or Fisher weights."
  )
  .run_assert(
    abs(sum(distribution$weight_share) - 1) < 1e-8,
    "Published distribution weight shares do not sum to one."
  )
  checks$distribution <- list(
    rows = nrow(distribution),
    components = data.table::uniqueN(distribution$component_id),
    latest_month = .run_format_month(latest),
    passed = TRUE
  )

  for (stem in .run_expected_figures) {
    path <- file.path(stage_figures, paste0(stem, ".png"))
    .run_assert_file(path, "figure")
    signature <- readBin(path, what = "raw", n = 8L)
    .run_assert(
      length(signature) == 8L && identical(signature, .run_png_signature),
      "Figure is not a valid nonempty PNG: ", path
    )
  }
  checks$figures <- list(
    expected = length(.run_expected_figures),
    produced = length(.run_expected_figures),
    nonempty = TRUE,
    png_signature = TRUE,
    passed = TRUE
  )

  checks
}

.run_collect_passed <- function(x) {
  if (!is.list(x)) return(logical())
  out <- logical()
  if (!is.null(x$passed)) {
    out <- c(out, isTRUE(x$passed))
  }
  for (name in names(x)) {
    if (identical(name, "passed")) next
    out <- c(out, .run_collect_passed(x[[name]]))
  }
  out
}

.run_create_stage <- function(output_dir) {
  stage_root <- tempfile(".stage-", tmpdir = output_dir)
  dir.create(stage_root, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(stage_root)) {
    .run_stop("Could not create output staging directory in ", output_dir)
  }
  complete <- FALSE
  on.exit({
    if (!complete) {
      unlink(stage_root, recursive = TRUE, force = TRUE)
    }
  }, add = TRUE)

  stage_data <- file.path(stage_root, "data")
  stage_figures <- file.path(stage_root, "figures")
  dir.create(stage_data, recursive = TRUE, showWarnings = FALSE)
  dir.create(stage_figures, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(stage_data) || !dir.exists(stage_figures)) {
    .run_stop("Could not create staged data/figures directories.")
  }
  complete <- TRUE
  list(
    root = normalizePath(stage_root, winslash = "/", mustWork = TRUE),
    data = normalizePath(stage_data, winslash = "/", mustWork = TRUE),
    figures = normalizePath(stage_figures, winslash = "/", mustWork = TRUE)
  )
}

.run_assert_final_outputs <- function(data_dir, figures_dir) {
  data_files <- c(
    "component_crosswalk.csv", "component_panel.csv",
    "dallas_diagnostic.csv", "measures_yoy.csv",
    "latest_distribution.csv", "run_summary.csv", "run_metadata.json"
  )
  for (file in data_files) {
    .run_assert_file(file.path(data_dir, file), "Published data file")
  }
  for (stem in .run_expected_figures) {
    .run_assert_file(
      file.path(figures_dir, paste0(stem, ".png")),
      "Published figure"
    )
  }
  invisible(TRUE)
}

.run_commit_outputs <- function(stage_data, stage_figures,
                                data_dir, figures_dir) {
  output_dir <- dirname(data_dir)
  targets <- list(
    data = list(stage = stage_data, target = data_dir),
    figures = list(stage = stage_figures, target = figures_dir)
  )

  backup_root <- tempfile(".backup-", tmpdir = output_dir)
  dir.create(backup_root, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(backup_root)) {
    .run_stop("Could not create output backup directory in ", output_dir)
  }

  moved <- character()
  installed <- character()
  rollback <- function() {
    for (name in rev(names(targets))) {
      target <- targets[[name]]$target
      stage <- targets[[name]]$stage
      backup <- file.path(backup_root, name)
      if (name %in% installed) {
        try(suppressWarnings(file.rename(target, stage)), silent = TRUE)
      }
      if (name %in% moved) {
        try(suppressWarnings(file.rename(backup, target)), silent = TRUE)
      }
    }
    invisible(NULL)
  }
  fail <- function(message) {
    rollback()
    leftovers <- list.files(backup_root, all.files = TRUE, no.. = TRUE)
    if (length(leftovers) == 0L) {
      unlink(backup_root, recursive = TRUE, force = TRUE)
      .run_stop(message)
    }
    .run_stop(
      message, " A backup of the previous outputs was retained at ",
      backup_root, "."
    )
  }

  for (name in names(targets)) {
    stage <- targets[[name]]$stage
    target <- targets[[name]]$target
    if (file.exists(target) && !dir.exists(target)) {
      fail(paste0("Output path is not a directory: ", target))
    }
    if (dir.exists(target)) {
      backup <- file.path(backup_root, name)
      if (!suppressWarnings(file.rename(target, backup))) {
        fail(paste0("Could not move the previous ", name, " output aside."))
      }
      moved <- c(moved, name)
    }
    if (!suppressWarnings(file.rename(stage, target))) {
      fail(paste0("Could not install the staged ", name, " output."))
    }
    installed <- c(installed, name)
  }

  final_ok <- tryCatch({
    .run_assert_final_outputs(data_dir, figures_dir)
    TRUE
  }, error = function(e) {
    fail(paste0(
      "Published outputs failed final validation: ", conditionMessage(e)
    ))
  })
  .run_assert(final_ok, "Internal error: final output validation did not run.")

  unlink(backup_root, recursive = TRUE, force = TRUE)
  invisible(TRUE)
}

main <- function() {
  run_started <- Sys.time()
  refresh <- .run_parse_args(commandArgs(trailingOnly = TRUE))

  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    .run_stop(
      "Package 'jsonlite' is required to write output/data/run_metadata.json."
    )
  }

  prior <- .run_read_prior_summary(root)
  if (!is.null(prior$latest)) {
    current_month <- .run_month_start(Sys.Date())
    .run_assert(
      prior$latest <= current_month,
      "Prior run_summary.csv latest_month ", .run_format_month(prior$latest),
      " is after the current month ", .run_format_month(current_month),
      "; refusing to run with a future-dated summary."
    )
  }
  git_info <- .run_git_info(root)

  message(
    "Fetching BEA inputs", if (refresh) " (refresh)" else " (automatic cache)",
    " and Dallas benchmark", if (refresh) " (refresh)" else " (cache)"
  )
  inputs <- fetch_bea_inputs(start_year = 2016L, refresh = refresh)
  dallas <- fetch_dallas_benchmark(refresh = refresh)

  panel <- prepare_component_panel(inputs$price, inputs$nominal, strict = TRUE)
  measures <- compute_all_variants(panel, long = TRUE)
  weighted_panel <- compute_fisher_weights(panel)

  lock_dir <- .run_lock_dir(root)
  lock_paths <- c(
    bea_line_lock = file.path(lock_dir, "bea_line_lock.csv"),
    crosswalk_lock = file.path(lock_dir, "crosswalk_lock.csv")
  )
  lock <- .run_read_crosswalk_lock(lock_paths[["crosswalk_lock"]])
  classification_check <- .run_check_classifications(panel, lock)
  crosswalk <- .run_build_crosswalk(panel, lock)

  headline <- .run_resolve_price_series(
    inputs$price, "DPCERG", "headline",
    "personal consumption expenditures"
  )
  core <- .run_resolve_price_series(
    inputs$price, "DPCCRG", "core",
    "pce excluding food and energy"
  )
  headline_max_abs <- max(abs(headline$value), na.rm = TRUE)
  core_max_abs <- max(abs(core$value), na.rm = TRUE)

  dallas_data <- data.table::as.data.table(dallas)
  if (!all(c("date", "dallas_yoy") %in% names(dallas_data))) {
    .run_stop("Dallas benchmark must have `date` and `dallas_yoy` columns.")
  }
  dallas_data[, date := as.Date(date)]
  .run_assert(
    !anyNA(dallas_data$date) && anyDuplicated(dallas_data$date) == 0L,
    "Dallas benchmark has missing or duplicate dates."
  )
  .run_assert(
    all(format(dallas_data$date, "%d") == "01"),
    "Dallas benchmark contains non-monthly dates."
  )
  dallas_data <- dallas_data[order(dallas_data$date)]
  if (nrow(dallas_data) > 1L) {
    .run_assert(
      all(diff(.run_month_id(dallas_data$date)) == 1L),
      "Dallas benchmark is not monthly-contiguous."
    )
  }
  .run_assert(
    all(is.finite(dallas_data$dallas_yoy)),
    "Dallas benchmark contains non-finite values."
  )

  benchmarks <- data.table::rbindlist(list(
    headline[, .(series, date, value)],
    core[, .(series, date, value)],
    dallas_data[, .(series = "dallas", date, value = dallas_yoy)]
  ), use.names = TRUE)
  data.table::setorderv(benchmarks, c("series", "date"))
  .run_assert(
    anyDuplicated(benchmarks, by = c("series", "date")) == 0L,
    "Benchmarks have duplicate series-date rows."
  )

  max_price <- max(as.Date(inputs$price$date))
  max_nominal <- max(as.Date(inputs$nominal$date))
  max_dallas <- max(dallas_data$date)
  max_panel <- max(panel$date)
  max_measures <- max(measures$date)
  .run_assert(
    any(!is.na(measures$yoy)),
    "Computed measures contain no year-over-year observations."
  )
  latest_measure <- max(measures$date[!is.na(measures$yoy)])
  benchmark_complete <- benchmarks[
    !is.na(value),
    .(n_series = data.table::uniqueN(series)),
    by = date
  ][n_series == 3L, date]
  .run_assert(
    length(benchmark_complete) > 0L,
    "Benchmarks never have headline, core, and Dallas available together."
  )
  latest_benchmark <- max(benchmark_complete)
  latest_values <- c(
    price = as.numeric(max_price),
    nominal = as.numeric(max_nominal),
    dallas = as.numeric(max_dallas),
    panel = as.numeric(max_panel),
    measures = as.numeric(max_measures),
    latest_measure = as.numeric(latest_measure),
    latest_benchmark = as.numeric(latest_benchmark)
  )
  if (length(unique(latest_values)) != 1L) {
    .run_stop(
      "Strict latest-month equality failed: ",
      paste(
        sprintf("%s=%s", names(latest_values),
                .run_format_date(as.Date(latest_values, origin = "1970-01-01"))),
        collapse = ", "
      ),
      "."
    )
  }
  latest <- max_price

  .run_assert(
    setequal(unique(measures$variant), .run_expected_variants),
    "Computed measures do not contain exactly the expected variants."
  )
  measures_latest <- measures[measures$date == latest]
  .run_assert(
    nrow(measures_latest) == length(.run_expected_variants) &&
      all(is.finite(measures_latest$yoy)),
    "Computed measures do not have finite YoY values for every variant in ",
    .run_format_month(latest), "."
  )
  benchmarks_latest <- benchmarks[benchmarks$date == latest]
  .run_assert(
    nrow(benchmarks_latest) == 3L &&
      setequal(benchmarks_latest$series, c("headline", "core", "dallas")) &&
      all(is.finite(benchmarks_latest$value)),
    "Benchmarks do not have headline, core, and Dallas values in ",
    .run_format_month(latest), "."
  )
  latest_panel <- panel[panel$date == latest]
  .run_assert(
    nrow(latest_panel) == .run_expected_components &&
      data.table::uniqueN(latest_panel$component_id) ==
        .run_expected_components,
    "Latest panel does not contain ", .run_expected_components,
    " components."
  )

  current_month <- .run_month_start(Sys.Date())
  freshness_lag <- .run_month_id(current_month) - .run_month_id(latest)
  .run_assert(
    freshness_lag >= 0L,
    "Latest month ", .run_format_month(latest),
    " is after the current month ", .run_format_month(current_month), "."
  )
  .run_assert(
    freshness_lag <= .run_freshness_limit_months,
    "Latest month ", .run_format_month(latest), " is ", freshness_lag,
    " calendar months behind the current month ",
    .run_format_month(current_month), "; the freshness limit is ",
    .run_freshness_limit_months, " months."
  )

  regression_passed <- TRUE
  if (!is.null(prior$latest)) {
    regression_passed <- latest >= prior$latest
    .run_assert(
      regression_passed,
      "Latest month ", .run_format_month(latest),
      " would regress from the prior run_summary.csv latest_month ",
      .run_format_month(prior$latest), "."
    )
  }

  weighted_latest <- weighted_panel[weighted_panel$date == latest]
  .run_assert(
    nrow(weighted_latest) == .run_expected_components &&
      data.table::uniqueN(weighted_latest$component_id) ==
        .run_expected_components,
    "Latest weighted panel does not contain ",
    .run_expected_components, " unique components."
  )
  .run_assert(
    all(is.finite(weighted_latest$pi)) &&
      all(is.finite(weighted_latest$fisher_weight)) &&
      all(weighted_latest$fisher_weight > 0),
    "Latest weighted panel has missing pi or non-positive Fisher weights."
  )
  weight_sum <- sum(weighted_latest$fisher_weight)
  .run_assert(
    is.finite(weight_sum) && weight_sum > 0,
    "Latest weighted panel Fisher weights do not have a positive sum."
  )

  official <- dallas_data[, .(date, official = dallas_yoy)]
  replication <- measures[
    variant == "dallas_replication",
    .(date, reconstructed = yoy)
  ]
  .run_assert(
    nrow(replication) > 0L,
    "No Dallas replication variant was computed."
  )
  diagnostics <- merge(official, replication, by = "date", sort = TRUE)
  diagnostics[, difference_pp := reconstructed - official]
  diagnostic_check <- .run_check_dallas_diagnostic(diagnostics)

  market_share <- sum(latest_panel$nominal[latest_panel$market_based]) /
    sum(latest_panel$nominal)
  .run_assert(
    is.finite(market_share) && market_share > 0 && market_share < 1,
    "Latest market-based nominal share is invalid."
  )

  manifest_path <- file.path(root, "data", "raw", "manifest.csv")
  manifest <- write_data_manifest(
    inputs$price, inputs$nominal, dallas,
    path = manifest_path
  )
  .run_assert(
    nrow(manifest) == 3L,
    "Data manifest has ", nrow(manifest), " rows; expected three."
  )
  .run_assert(
    all(as.Date(manifest$latest_date) == latest),
    "Data manifest latest dates do not all equal ",
    .run_format_date(latest), "."
  )

  raw_dir <- file.path(root, "data", "raw")
  raw_paths <- list.files(
    raw_dir, pattern = "\\.csv$", full.names = TRUE
  )
  raw_paths <- raw_paths[basename(raw_paths) != "manifest.csv"]
  .run_assert(
    length(raw_paths) > 0L,
    "No raw input CSV files found in ", raw_dir, "."
  )
  raw_entries <- lapply(sort(raw_paths), function(path) {
    .run_file_entry(
      path,
      .run_relative_path(path, root)
    )
  })
  lock_entries <- lapply(unname(lock_paths), function(path) {
    .run_file_entry(path, .run_relative_path(path, root))
  })

  output_dir <- file.path(root, "output")
  if (file.exists(output_dir) && !dir.exists(output_dir)) {
    .run_stop("Output path is not a directory: ", output_dir)
  }
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  .run_assert(dir.exists(output_dir),
              "Could not create output directory: ", output_dir)

  stage <- .run_create_stage(output_dir)
  stage_root <- stage$root
  on.exit({
    if (!is.null(stage_root) && dir.exists(stage_root)) {
      unlink(stage_root, recursive = TRUE, force = TRUE)
    }
  }, add = TRUE)

  clean_benchmarks <- benchmarks[benchmarks$date <= latest]
  clean_measures <- measures[measures$date <= latest]

  data.table::fwrite(
    crosswalk, file.path(stage$data, "component_crosswalk.csv")
  )
  data.table::fwrite(
    panel, file.path(stage$data, "component_panel.csv")
  )
  data.table::fwrite(
    diagnostics, file.path(stage$data, "dallas_diagnostic.csv")
  )
  figure_data <- write_figure_data(
    clean_measures, weighted_latest, output_dir = stage$data
  )
  .run_assert(
    all(file.exists(figure_data)) && all(file.info(figure_data)$size > 0),
    "Figure data CSVs were not written."
  )
  figure_paths <- create_all_figures(
    clean_measures, clean_benchmarks, weighted_latest,
    output_dir = stage$figures
  )
  .run_assert(
    length(figure_paths) == length(.run_expected_figures) &&
      setequal(names(figure_paths), .run_expected_figures),
    "Expected ", length(.run_expected_figures), " named figures; got ",
    length(figure_paths), "."
  )

  summary <- data.table::data.table(
    latest_month = latest,
    components = data.table::uniqueN(panel$component_id),
    market_based_share = market_share,
    figures = length(figure_paths),
    expanded_lines = nrow(lock),
    dallas_complete_months = diagnostic_check$complete_months,
    dallas_mean_abs_error_pp = diagnostic_check$mean_abs_error_pp,
    dallas_max_abs_error_pp = diagnostic_check$max_abs_error_pp,
    refresh = refresh
  )
  data.table::fwrite(summary, file.path(stage$data, "run_summary.csv"))

  output_checks <- .run_validate_outputs(
    stage$data, stage$figures, latest, refresh
  )
  validation <- list(
    components = list(
      expected = .run_expected_components,
      observed = data.table::uniqueN(panel$component_id),
      passed = data.table::uniqueN(panel$component_id) ==
        .run_expected_components
    ),
    expanded_lines = list(
      expected = .run_expected_lines,
      observed = nrow(lock),
      passed = nrow(lock) == .run_expected_lines
    ),
    classifications = classification_check,
    latest_month = list(
      latest = .run_format_date(latest),
      price = .run_format_date(max_price),
      nominal = .run_format_date(max_nominal),
      dallas = .run_format_date(max_dallas),
      panel = .run_format_date(max_panel),
      measures = .run_format_date(max_measures),
      latest_measure = .run_format_date(latest_measure),
      latest_benchmark = .run_format_date(latest_benchmark),
      passed = TRUE
    ),
    freshness = list(
      current_month = .run_format_month(current_month),
      months_behind = as.integer(freshness_lag),
      limit_months = .run_freshness_limit_months,
      passed = freshness_lag <= .run_freshness_limit_months
    ),
    regression = list(
      prior_latest = if (is.null(prior$latest)) {
        NA_character_
      } else {
        .run_format_month(prior$latest)
      },
      latest = .run_format_month(latest),
      passed = regression_passed
    ),
    yoy_plausibility = list(
      limit_pp = .run_yoy_limit_pp,
      headline_max_abs_pp = headline_max_abs,
      core_max_abs_pp = core_max_abs,
      passed = headline_max_abs < .run_yoy_limit_pp &&
        core_max_abs < .run_yoy_limit_pp
    ),
    dallas_diagnostic = diagnostic_check,
    outputs = output_checks
  )
  validation$all_passed <- all(.run_collect_passed(validation))
  .run_assert(
    validation$all_passed,
    "Internal validation collected a failed check."
  )

  summary_hash <- .run_hash_file(file.path(stage$data, "run_summary.csv"))
  manifest_hash <- .run_hash_file(manifest_path)
  output_entries <- c(
    lapply(
      c(
        "component_crosswalk.csv", "component_panel.csv",
        "dallas_diagnostic.csv", "measures_yoy.csv",
        "latest_distribution.csv", "run_summary.csv"
      ),
      function(file) {
        .run_file_entry(
          file.path(stage$data, file),
          file.path("output", "data", file)
        )
      }
    ),
    lapply(.run_expected_figures, function(stem) {
      file <- paste0(stem, ".png")
      .run_file_entry(
        file.path(stage$figures, file),
        file.path("output", "figures", file)
      )
    })
  )

  run_finished <- Sys.time()
  metadata <- list(
    schema = "trimmed-mean-run-metadata-v1",
    latest_month = .run_format_month(latest),
    latest_date = .run_format_date(latest),
    refresh = refresh,
    run = list(
      started_at = format(run_started, "%Y-%m-%dT%H:%M:%S%z"),
      finished_at = format(run_finished, "%Y-%m-%dT%H:%M:%S%z"),
      started_at_utc = format(
        run_started, tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ"
      ),
      duration_seconds = as.numeric(
        difftime(run_finished, run_started, units = "secs")
      ),
      timezone = Sys.timezone(),
      command = paste(
        c("Rscript", "R/run_all.R", if (refresh) "--refresh"),
        collapse = " "
      ),
      r = list(
        version = R.version.string,
        version_number = as.character(getRversion()),
        platform = R.version$platform,
        nickname = R.version$nickname
      ),
      packages = .run_package_versions()
    ),
    rows = list(
      components = data.table::uniqueN(panel$component_id),
      expanded_lines = nrow(lock),
      months = data.table::uniqueN(panel$date),
      panel = nrow(panel),
      crosswalk = nrow(crosswalk),
      measures = nrow(measures),
      benchmarks = nrow(benchmarks),
      diagnostic = nrow(diagnostics),
      diagnostic_complete = diagnostic_check$complete_months
    ),
    validation = validation,
    inputs = list(
      manifest = list(
        path = .run_relative_path(manifest_path, root),
        hash_algo = manifest_hash$hash_algo,
        hash = manifest_hash$hash,
        rows = .run_rows(manifest)
      ),
      raw_files = raw_entries,
      config_locks = lock_entries,
      provenance = list(
        price = .run_provenance(inputs$price),
        nominal = .run_provenance(inputs$nominal),
        dallas = .run_provenance(dallas)
      )
    ),
    prior = list(
      path = .run_relative_path(prior$path, root),
      exists = prior$exists,
      latest_month = if (is.null(prior$latest)) {
        NA_character_
      } else {
        .run_format_month(prior$latest)
      }
    ),
    outputs = list(
      data_dir = "output/data",
      figures_dir = "output/figures",
      figures = as.list(.run_expected_figures),
      files = output_entries
    ),
    run_summary = list(
      path = "output/data/run_summary.csv",
      hash_algo = summary_hash$hash_algo,
      hash = summary_hash$hash,
      values = .run_rows(summary)
    ),
    git = git_info
  )

  metadata_text <- jsonlite::toJSON(
    metadata,
    auto_unbox = TRUE,
    pretty = TRUE,
    digits = NA,
    na = "null",
    null = "null"
  )
  .run_assert(
    !grepl("BEA_API_KEY|UserID|api[_-]?key|Authorization",
           metadata_text, ignore.case = TRUE),
    "Refusing to write run metadata containing credential-like fields."
  )
  metadata_path <- file.path(stage$data, "run_metadata.json")
  writeLines(metadata_text, metadata_path, useBytes = TRUE)
  .run_assert_file(metadata_path, "run metadata")

  metadata_check <- tryCatch(
    jsonlite::fromJSON(
      paste(readLines(metadata_path, warn = FALSE), collapse = "\n"),
      simplifyVector = FALSE
    ),
    error = function(e) {
      .run_stop(
        "run_metadata.json is not valid JSON: ", conditionMessage(e)
      )
    }
  )
  .run_assert(
    identical(metadata_check$latest_date, .run_format_date(latest)),
    "run_metadata.json latest_date does not match the run."
  )
  metadata_refresh <- metadata_check$refresh
  if (is.list(metadata_refresh)) metadata_refresh <- metadata_refresh[[1L]]
  .run_assert(
    isTRUE(metadata_refresh) == refresh,
    "run_metadata.json refresh flag does not match the run."
  )
  .run_assert(
    isTRUE(metadata_check$validation$all_passed),
    "run_metadata.json reports a failed validation."
  )
  .run_assert(
    length(metadata_check$inputs$raw_files) > 0L &&
      length(metadata_check$inputs$config_locks) == 2L,
    "run_metadata.json is missing input hashes."
  )

  .run_commit_outputs(
    stage$data, stage$figures,
    file.path(output_dir, "data"), file.path(output_dir, "figures")
  )
  unlink(stage_root, recursive = TRUE, force = TRUE)
  if (!dir.exists(stage_root)) {
    stage_root <- NULL
  }

  cat(sprintf(
    paste0(
      "Published outputs for %s: %d components, %d expanded lines, ",
      "%d figures%s.\n"
    ),
    .run_format_month(latest),
    data.table::uniqueN(panel$component_id),
    nrow(lock),
    length(figure_paths),
    if (refresh) " (refresh)" else ""
  ))
  cat(sprintf(
    paste0(
      "Dallas diagnostic: %d complete months, mean abs %.4f pp, ",
      "max abs %.4f pp (worst %s).\n"
    ),
    diagnostic_check$complete_months,
    diagnostic_check$mean_abs_error_pp,
    diagnostic_check$max_abs_error_pp,
    diagnostic_check$worst_month
  ))
  invisible(metadata)
}

if (sys.nframe() == 0L) {
  main()
}
