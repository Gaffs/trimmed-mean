# Standalone construction of Employ America's preferred market-based ex-housing
# 16 percent symmetric trimmed mean.
#
# Public entry points:
#   construct_market_ex_housing_tm16(price, nominal, components_path = ...)
#   load_component_crosswalk(path = ...)
#
# `price` and `nominal` are long BEA underlying-detail style tables with
# `line_number`, `series_code`, `description`, `date`, and `value`.  A `table`
# column is optional but, when present, must identify U20404 for `price` and
# U20405 for `nominal`.  Exact duplicate line-date rows are collapsed; conflicting
# duplicates are an error.
#
# The measure is built only from components flagged `market_based == TRUE` and
# `housing == FALSE`.  That filter is applied before Fisher weights are formed.
# Nominal expenditure is summed over the expanded BEA lines of each component
# (notably lines 7+8 and 14+16); price comes from the component's designated
# `price_line`.  The output starts at the first complete calendar month, with a
# floor of 1987-01, and requires every selected component and expanded nominal
# line to be present in every month through the common end date.
#
# Monthly rates are simple price changes.  Fisher weights follow the Dallas Fed
# formula
#
#   Q_it = N_it / P_it
#   w_it = 0.5 * (Q_it * P_i,t-1) / sum_j(Q_jt * P_j,t-1)
#        + 0.5 * N_i,t-1 / sum_j N_j,t-1
#
# over components with a current and an immediately preceding calendar month.
# The trimmed mean retains the exact fractional weight inside the 8 percent
# boundary in each tail.  The chained index is 100 in the first month.  `yoy`,
# `annualized_6m`, and `annualized_3m` use exact calendar lags (12, 6, and 3
# months) and geometric annualization; all returned rates are in percent.
#
# Legacy BEA lines 157 and 158 are dropped before validation.  Historical BEA
# vintages publish them as children of line 156, but the current component lock
# represents that block with line 156 alone.  Unmapped BEA lines are otherwise
# ignored rather than treated as additions to the component inventory.

.uei_expected_lines <- 194L
.uei_expected_components <- 192L
.uei_panel_start <- as.Date("1987-01-01")
.uei_trim_fraction <- 0.08
.uei_identity_tolerance <- 5
.uei_legacy_lines <- c(157L, 158L)

.uei_module_file <- local({
  ofile <- tryCatch(sys.frame(1L)$ofile, error = function(e) NULL)
  if (is.null(ofile) || !nzchar(ofile)) {
    args <- commandArgs(trailingOnly = FALSE)
    hit <- grep("^--file=", args, value = TRUE)
    if (length(hit)) ofile <- sub("^--file=", "", hit[[1L]])
  }
  if (is.null(ofile) || !nzchar(ofile)) {
    NA_character_
  } else {
    normalizePath(ofile, winslash = "/", mustWork = FALSE)
  }
})

.uei_default_components_path <- function() {
  configured <- getOption("underlying_inflation.components_path", NULL)
  if (!is.null(configured)) {
    return(path.expand(configured))
  }
  configured <- Sys.getenv("UNDERLYING_INFLATION_COMPONENTS", "")
  if (nzchar(configured)) {
    return(path.expand(configured))
  }

  candidates <- c(
    "underlying-inflation/config/components.csv",
    "config/components.csv",
    "../underlying-inflation/config/components.csv"
  )
  if (!is.na(.uei_module_file)) {
    candidates <- c(
      file.path(dirname(dirname(.uei_module_file)), "config", "components.csv"),
      candidates
    )
  }
  for (candidate in candidates) {
    if (file.exists(candidate)) {
      return(normalizePath(candidate, winslash = "/", mustWork = TRUE))
    }
  }
  candidates[[1L]]
}

.uei_resolve_components_path <- function(path) {
  if (is.null(path)) {
    path <- .uei_default_components_path()
  }
  if (!is.character(path) || length(path) != 1L || is.na(path) ||
      !nzchar(trimws(path))) {
    stop("`components_path` must be one non-empty file path.", call. = FALSE)
  }
  path <- path.expand(trimws(path))
  if (dir.exists(path)) {
    path <- file.path(path, "components.csv")
  }
  if (!file.exists(path)) {
    stop(sprintf(
      paste0(
        "Component lock not found at %s. `components_path` must point to the ",
        "subproject copy generated from config/crosswalk_lock.csv."
      ),
      path
    ), call. = FALSE)
  }
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

.uei_preview <- function(x, n = 8L) {
  x <- as.character(x)
  if (length(x) <= n) {
    return(paste(x, collapse = ", "))
  }
  paste0(paste(x[seq_len(n)], collapse = ", "), ", ... (",
         length(x) - n, " more)")
}

.uei_integer <- function(x, label, allow_na = FALSE) {
  if (is.factor(x)) {
    x <- as.character(x)
  }
  text <- as.character(x)
  number <- suppressWarnings(as.numeric(text))
  integer <- suppressWarnings(as.integer(number))
  bad <- if (allow_na) {
    !is.na(text) & nzchar(trimws(text)) &
      (is.na(number) | is.na(integer) | number != integer)
  } else {
    is.na(number) | is.na(integer) | number != integer
  }
  if (any(bad)) {
    stop(sprintf(
      "%s must contain whole numbers; invalid value(s): %s.",
      label, .uei_preview(unique(text[bad]))
    ), call. = FALSE)
  }
  if (!allow_na && anyNA(integer)) {
    stop(sprintf("%s must not contain missing values.", label), call. = FALSE)
  }
  integer
}

.uei_logical <- function(x, label) {
  if (is.logical(x)) {
    if (anyNA(x)) {
      stop(sprintf("%s must not contain missing values.", label), call. = FALSE)
    }
    return(x)
  }
  text <- toupper(trimws(as.character(x)))
  bad <- is.na(text) | !text %in% c("TRUE", "FALSE")
  if (any(bad)) {
    stop(sprintf(
      "%s must be TRUE or FALSE; invalid value(s): %s.",
      label, .uei_preview(unique(text[bad]))
    ), call. = FALSE)
  }
  text == "TRUE"
}

.uei_date <- function(x, label) {
  if (inherits(x, "Date")) {
    out <- as.Date(x)
  } else if (inherits(x, "POSIXt")) {
    out <- as.Date(x)
  } else if (is.character(x)) {
    out <- suppressWarnings(as.Date(x))
    if (all(is.na(out)) && any(!is.na(x))) {
      out <- suppressWarnings(as.Date(x, format = "%Y-%m-%d"))
    }
  } else {
    stop(sprintf(
      "%s must be a Date or ISO date character column.", label
    ), call. = FALSE)
  }

  if (anyNA(out)) {
    bad <- unique(as.character(x)[is.na(out)])
    stop(sprintf(
      "%s has missing or unparseable date(s): %s.",
      label, .uei_preview(bad)
    ), call. = FALSE)
  }
  if (any(as.integer(format(out, "%d")) != 1L)) {
    bad <- unique(format(out[as.integer(format(out, "%d")) != 1L], "%Y-%m-%d"))
    stop(sprintf(
      paste0(
        "%s must contain first-of-month dates so that calendar lags are ",
        "unambiguous; invalid date(s): %s."
      ),
      label, .uei_preview(bad)
    ), call. = FALSE)
  }
  out
}

.uei_month_id <- function(x) {
  as.integer(format(x, "%Y")) * 12L + as.integer(format(x, "%m"))
}

#' Load and validate the subproject component lock
#'
#' @param path Path to `components.csv`. The default resolves to the
#'   subproject's `config/components.csv`.
#'
#' @return A data.table with integer line numbers, logical flags, and the
#'   expanded component-line mapping used by
#'   `construct_market_ex_housing_tm16()`.
load_component_crosswalk <- function(path = .uei_default_components_path()) {
  path <- .uei_resolve_components_path(path)
  lock <- tryCatch(
    data.table::fread(
      path, data.table = TRUE, na.strings = c("", "NA"),
      showProgress = FALSE
    ),
    error = function(e) {
      stop(sprintf("Could not read component lock %s: %s", path,
                   conditionMessage(e)), call. = FALSE)
    }
  )
  if (anyDuplicated(names(lock))) {
    stop(sprintf("Component lock %s has duplicate column names.", path),
         call. = FALSE)
  }

  required <- c(
    "component_id", "component_line", "line_number", "line_order",
    "price_line", "is_price_source", "component_description",
    "line_description", "price_series_code", "nominal_series_code",
    "housing", "market_based", "source_group", "dallas_name",
    "identity_parent", "identity_children"
  )
  missing <- setdiff(required, names(lock))
  if (length(missing)) {
    stop(sprintf(
      "Component lock %s is missing required column(s): %s.",
      path, paste(missing, collapse = ", ")
    ), call. = FALSE)
  }

  for (column in c("line_number", "line_order", "price_line")) {
    data.table::set(
      lock, j = column,
      value = .uei_integer(lock[[column]], sprintf("Component lock `%s`", column))
    )
  }
  if ("component_n_lines" %in% names(lock)) {
    data.table::set(
      lock, j = "component_n_lines",
      value = .uei_integer(lock[["component_n_lines"]],
                           "Component lock `component_n_lines`")
    )
  }
  data.table::set(
    lock, j = "identity_parent",
    value = .uei_integer(lock[["identity_parent"]],
                         "Component lock `identity_parent`", allow_na = TRUE)
  )
  for (column in c("is_price_source", "housing", "market_based")) {
    data.table::set(
      lock, j = column,
      value = .uei_logical(lock[[column]], sprintf("Component lock `%s`", column))
    )
  }
  for (column in c(
    "component_id", "component_line", "component_description",
    "line_description", "price_series_code", "nominal_series_code",
    "source_group", "dallas_name"
  )) {
    value <- trimws(as.character(lock[[column]]))
    if (anyNA(value) || any(!nzchar(value))) {
      stop(sprintf(
        "Component lock %s has missing or empty `%s` value(s).", path, column
      ), call. = FALSE)
    }
    data.table::set(lock, j = column, value = value)
  }
  identity_children <- trimws(as.character(lock[["identity_children"]]))
  identity_children[is.na(identity_children)] <- ""
  data.table::set(lock, j = "identity_children", value = identity_children)

  if (nrow(lock) != .uei_expected_lines) {
    stop(sprintf(
      "Component lock %s has %d expanded line rows; expected %d.",
      path, nrow(lock), .uei_expected_lines
    ), call. = FALSE)
  }
  if (data.table::uniqueN(lock$component_id) != .uei_expected_components) {
    stop(sprintf(
      "Component lock %s has %d components; expected %d.",
      path, data.table::uniqueN(lock$component_id),
      .uei_expected_components
    ), call. = FALSE)
  }
  if (data.table::uniqueN(lock$line_number) != nrow(lock)) {
    stop(sprintf(
      "Component lock %s assigns at least one BEA line more than once.", path
    ), call. = FALSE)
  }
  has_parent <- !is.na(lock$identity_parent)
  if (any(has_parent & !nzchar(identity_children))) {
    bad <- lock[has_parent & !nzchar(identity_children)]
    stop(sprintf(
      paste0(
        "Component lock %s has `identity_parent` but no `identity_children` ",
        "for line(s) %s."
      ),
      path, .uei_preview(bad$line_number)
    ), call. = FALSE)
  }
  if (any(!has_parent & nzchar(identity_children))) {
    bad <- lock[!has_parent & nzchar(identity_children)]
    stop(sprintf(
      paste0(
        "Component lock %s has `identity_children` but no `identity_parent` ",
        "for line(s) %s."
      ),
      path, .uei_preview(bad$line_number)
    ), call. = FALSE)
  }

  data.table::setorder(lock, component_id, line_order)

  components <- lock[, .(
    n_lines = .N,
    n_price_source = sum(is_price_source),
    n_price_lines = data.table::uniqueN(price_line),
    n_component_description = data.table::uniqueN(component_description),
    n_housing = data.table::uniqueN(housing),
    n_market_based = data.table::uniqueN(market_based),
    n_source_group = data.table::uniqueN(source_group)
  ), by = component_id]
  bad <- components[
    n_price_source != 1L | n_price_lines != 1L |
      n_component_description != 1L | n_housing != 1L |
      n_market_based != 1L | n_source_group != 1L
  ]
  if (nrow(bad)) {
    stop(sprintf(
      paste0(
        "Component lock %s is inconsistent within component(s): %s. Each ",
        "component needs one price source, one price line, and stable flags."
      ),
      path, .uei_preview(bad$component_id)
    ), call. = FALSE)
  }

  bad <- lock[is_price_source & line_number != price_line]
  if (nrow(bad)) {
    stop(sprintf(
      paste0(
        "Component lock %s has price source line %d but price_line %d for ",
        "component %s."
      ),
      path, bad$line_number[[1L]], bad$price_line[[1L]],
      bad$component_id[[1L]]
    ), call. = FALSE)
  }
  if (any(!lock$price_line %in% lock$line_number)) {
    bad <- lock[!price_line %in% line_number]
    stop(sprintf(
      "Component lock %s has price_line(s) that are not expanded lines: %s.",
      path, .uei_preview(unique(bad$price_line))
    ), call. = FALSE)
  }
  bad <- lock[, .(
    ok = all(price_line %in% line_number)
  ), by = component_id][ok == FALSE]
  if (nrow(bad)) {
    stop(sprintf(
      paste0(
        "Component lock %s assigns a price_line from another component to %s."
      ),
      path, .uei_preview(bad$component_id)
    ), call. = FALSE)
  }
  if (any(lock$housing & !lock$market_based)) {
    bad <- lock[housing & !market_based]
    stop(sprintf(
      "Component lock %s flags housing component(s) as not market-based: %s.",
      path, .uei_preview(unique(bad$component_id))
    ), call. = FALSE)
  }
  if (!all(lock$source_group %in% c(
    "Durable goods", "Nondurable goods", "Services", "NPISH"
  ))) {
    stop(sprintf(
      "Component lock %s has unknown `source_group` value(s): %s.",
      path, .uei_preview(unique(lock$source_group))
    ), call. = FALSE)
  }

  canonical <- lock[, .(
    component_line = component_line[[1L]],
    expected_line = paste(line_number, collapse = "+"),
    contiguous = identical(line_order, seq_len(.N))
  ), by = component_id]
  bad <- canonical[component_line != expected_line | !contiguous]
  if (nrow(bad)) {
    stop(sprintf(
      paste0(
        "Component lock %s has a non-canonical component_line/line_order for ",
        "component(s): %s."
      ),
      path, .uei_preview(bad$component_id)
    ), call. = FALSE)
  }

  if ("component_n_lines" %in% names(lock)) {
    check <- lock[, .(
      n_lines = .N,
      declared = component_n_lines[[1L]],
      n_declared = data.table::uniqueN(component_n_lines)
    ), by = component_id]
    bad <- check[n_lines != declared | n_declared != 1L]
    if (nrow(bad)) {
      stop(sprintf(
        "Component lock %s has incorrect `component_n_lines` for %s.",
        path, .uei_preview(bad$component_id)
      ), call. = FALSE)
    }
  }

  has_parent <- !is.na(lock$identity_parent)
  specs <- unique(lock[has_parent, .(
    identity_parent, identity_children
  )])
  if (nrow(specs)) {
    parsed <- lapply(seq_len(nrow(specs)), function(index) {
      parent <- specs$identity_parent[[index]]
      children_text <- specs$identity_children[[index]]
      children <- .uei_integer(
        strsplit(children_text, "+", fixed = TRUE)[[1L]],
        sprintf("Component lock identity children for parent %d", parent),
        allow_na = FALSE
      )
      if (length(children) < 2L || parent <= 0L || parent %in% children ||
          anyDuplicated(children)) {
        stop(sprintf(
          "Component lock %s has an invalid identity specification: %d = %s.",
          path, parent, children_text
        ), call. = FALSE)
      }
      list(parent = parent, children = children, text = children_text)
    })

    membership <- data.table::rbindlist(lapply(parsed, function(spec) {
      data.table::data.table(
        line_number = spec$children,
        identity_parent = spec$parent,
        identity_children = spec$text
      )
    }))
    duplicate_child <- membership[, .N, by = line_number][N > 1L]
    if (nrow(duplicate_child)) {
      stop(sprintf(
        "Component lock %s places line(s) %s in more than one identity.",
        path, .uei_preview(duplicate_child$line_number)
      ), call. = FALSE)
    }
    bad <- data.table::fsetdiff(
      lock[has_parent, .(line_number, identity_parent, identity_children)],
      membership
    )
    if (nrow(bad)) {
      stop(sprintf(
        paste0(
          "Component lock %s lists line(s) %s under an identity that does ",
          "not include them."
        ),
        path, .uei_preview(bad$line_number)
      ), call. = FALSE)
    }
  }

  lock[]
}

.uei_prepare_input <- function(x, name, expected_table, components) {
  if (!is.data.frame(x)) {
    stop(sprintf("`%s` must be a data.frame or data.table.", name),
         call. = FALSE)
  }
  input <- data.table::copy(data.table::as.data.table(x))
  if (anyDuplicated(names(input))) {
    stop(sprintf("`%s` has duplicate column names.", name), call. = FALSE)
  }

  required <- c("line_number", "series_code", "description", "date", "value")
  missing <- setdiff(required, names(input))
  if (length(missing)) {
    stop(sprintf(
      "`%s` must be a long BEA-style table with columns %s; missing: %s.",
      name, paste(required, collapse = ", "),
      paste(missing, collapse = ", ")
    ), call. = FALSE)
  }
  keep <- c(required, if ("table" %in% names(input)) "table")
  input <- input[, keep, with = FALSE]

  if ("table" %in% names(input)) {
    table_id <- trimws(as.character(input[["table"]]))
    bad <- is.na(table_id) | !table_id %in% expected_table
    if (any(bad)) {
      stop(sprintf(
        "`%s$table` must be %s; found: %s.",
        name, paste(expected_table, collapse = " or "),
        .uei_preview(unique(table_id[bad]))
      ), call. = FALSE)
    }
  }

  data.table::set(
    input, j = "line_number",
    value = .uei_integer(input[["line_number"]], sprintf("`%s$line_number`", name))
  )
  input <- input[!line_number %in% .uei_legacy_lines]
  if (nrow(input) == 0L) {
    stop(sprintf("`%s` has no rows after dropping legacy lines 157/158.", name),
         call. = FALSE)
  }
  for (column in c("series_code", "description")) {
    value <- trimws(as.character(input[[column]]))
    if (anyNA(value) || any(!nzchar(value))) {
      stop(sprintf(
        "`%s$%s` must not contain missing or empty values.", name, column
      ), call. = FALSE)
    }
    data.table::set(input, j = column, value = value)
  }
  data.table::set(
    input, j = "date",
    value = .uei_date(input[["date"]], sprintf("`%s$date`", name))
  )

  value <- input[["value"]]
  if (!is.numeric(value)) {
    if (!is.character(value)) {
      stop(sprintf("`%s$value` must be numeric.", name), call. = FALSE)
    }
    value <- suppressWarnings(as.numeric(value))
    if (anyNA(value)) {
      stop(sprintf("`%s$value` has non-numeric value(s).", name),
           call. = FALSE)
    }
  }
  if (any(!is.finite(value))) {
    stop(sprintf("`%s$value` must contain only finite values.", name),
         call. = FALSE)
  }
  data.table::set(input, j = "value", value = value)

  duplicates <- input[, .N, by = .(line_number, date)][N > 1L]
  if (nrow(duplicates)) {
    conflict <- input[duplicates, on = .(line_number, date), nomatch = 0L][
      ,
      .(
        descriptions = data.table::uniqueN(description),
        codes = data.table::uniqueN(series_code),
        values = data.table::uniqueN(value)
      ),
      by = .(line_number, date)
    ][descriptions > 1L | codes > 1L | values > 1L]
    if (nrow(conflict)) {
      stop(sprintf(
        paste0(
          "`%s` has conflicting duplicate rows for line %d on %s. Duplicate ",
          "line-date rows must be identical."
        ),
        name, conflict$line_number[[1L]],
        format(conflict$date[[1L]], "%Y-%m-%d")
      ), call. = FALSE)
    }
    input <- unique(input, by = c("line_number", "date"))
  }

  metadata <- input[, .(
    n_descriptions = data.table::uniqueN(description),
    n_codes = data.table::uniqueN(series_code)
  ), by = line_number]
  bad <- metadata[n_descriptions > 1L | n_codes > 1L]
  if (nrow(bad)) {
    stop(sprintf(
      paste0(
        "`%s` has more than one description or series code for line(s) %s; ",
        "line identities must be stable across the requested history."
      ),
      name, .uei_preview(bad$line_number)
    ), call. = FALSE)
  }

  code_column <- if (identical(name, "price")) {
    "price_series_code"
  } else {
    "nominal_series_code"
  }
  expected <- data.table::data.table(
    line_number = components$line_number,
    expected_description = components$line_description,
    expected_code = components[[code_column]]
  )
  present <- input[line_number %in% expected$line_number, .(
    description = description[[1L]],
    series_code = series_code[[1L]]
  ), by = line_number]
  compared <- merge(present, expected, by = "line_number", all.x = TRUE,
                    sort = FALSE)
  bad <- compared[
    description != expected_description | series_code != expected_code
  ]
  if (nrow(bad)) {
    show <- utils::head(bad, 8L)
    issues <- sprintf(
      "line %d: data has %s [%s], lock expects %s [%s]",
      show$line_number, show$description, show$series_code,
      show$expected_description, show$expected_code
    )
    stop(sprintf(
      paste0(
        "Current `%s` line descriptions or series codes do not match the ",
        "component lock:\n  %s%s"
      ),
      name, paste(issues, collapse = "\n  "),
      if (nrow(bad) > nrow(show)) {
        sprintf("\n  ... and %d more mismatch(es)", nrow(bad) - nrow(show))
      } else {
        ""
      }
    ), call. = FALSE)
  }

  if (identical(name, "price") && any(input$value <= 0)) {
    bad <- input[value <= 0]
    stop(sprintf(
      "`price$value` must be strictly positive; line %d on %s is %s.",
      bad$line_number[[1L]], format(bad$date[[1L]], "%Y-%m-%d"),
      format(bad$value[[1L]], digits = 12)
    ), call. = FALSE)
  }

  input[]
}

.uei_require_component_lines <- function(input, required, name) {
  present <- unique(input$line_number)
  missing <- required[!line_number %in% present]
  if (nrow(missing)) {
    detail <- sprintf(
      "%s/line %d", missing$component_id, missing$line_number
    )
    stop(sprintf(
      paste0(
        "`%s` is missing %d line(s) required for the market-based ",
        "ex-housing measure: %s."
      ),
      name, nrow(missing), .uei_preview(detail)
    ), call. = FALSE)
  }
  invisible(TRUE)
}

.uei_build_panel <- function(price, nominal, selected) {
  price_map <- unique(selected[, .(
    component_id,
    line_number = price_line
  )])
  nominal_map <- unique(selected[, .(
    component_id,
    line_number
  )])
  if (anyDuplicated(price_map, by = c("component_id", "line_number"))) {
    stop("Internal component lock error: duplicate component price lines.",
         call. = FALSE)
  }

  price_rows <- merge(
    price, price_map, by = "line_number", all = FALSE, sort = FALSE
  )
  nominal_rows <- merge(
    nominal, nominal_map, by = "line_number", all = FALSE, sort = FALSE
  )
  if (nrow(price_rows) == 0L || nrow(nominal_rows) == 0L) {
    stop("No price or nominal rows remain for the selected components.",
         call. = FALSE)
  }
  if (anyDuplicated(price_rows, by = c("component_id", "date"))) {
    stop("Selected price lines produce duplicate component-date rows.",
         call. = FALSE)
  }

  price_coverage <- price_rows[, .(
    price_first = min(date),
    price_last = max(date)
  ), by = component_id]
  nominal_line_coverage <- nominal_rows[, .(
    nominal_first = min(date),
    nominal_last = max(date)
  ), by = .(component_id, line_number)]
  nominal_expected <- unique(nominal_map)
  nominal_line_coverage <- merge(
    nominal_expected, nominal_line_coverage,
    by = c("component_id", "line_number"), all.x = TRUE, sort = FALSE
  )
  missing_nominal <- nominal_line_coverage[is.na(nominal_first)]
  if (nrow(missing_nominal)) {
    stop(sprintf(
      paste0(
        "`nominal` has no observations for required line(s): %s."
      ),
      .uei_preview(sprintf(
        "%s/line %d", missing_nominal$component_id, missing_nominal$line_number
      ))
    ), call. = FALSE)
  }
  nominal_coverage <- nominal_line_coverage[, .(
    nominal_first = max(nominal_first),
    nominal_last = min(nominal_last),
    n_lines = data.table::uniqueN(line_number)
  ), by = component_id]
  expected_counts <- nominal_expected[, .(expected_lines = .N), by = component_id]
  nominal_coverage <- merge(
    nominal_coverage, expected_counts, by = "component_id", all.x = TRUE,
    sort = FALSE
  )
  bad_counts <- nominal_coverage[n_lines != expected_lines]
  if (nrow(bad_counts)) {
    stop(sprintf(
      paste0(
        "`nominal` does not cover every expanded line of component(s) %s."
      ),
      .uei_preview(bad_counts$component_id)
    ), call. = FALSE)
  }

  coverage <- merge(
    price_coverage, nominal_coverage, by = "component_id",
    all = TRUE, sort = FALSE
  )
  missing_coverage <- coverage[is.na(price_first) | is.na(nominal_first)]
  if (nrow(missing_coverage)) {
    stop(sprintf(
      paste0(
        "Selected component(s) lack usable price or nominal coverage: %s."
      ),
      .uei_preview(missing_coverage$component_id)
    ), call. = FALSE)
  }
  coverage[, first_date := pmax(price_first, nominal_first)]
  coverage[, last_date := pmin(price_last, nominal_last)]
  if (any(coverage$first_date > coverage$last_date)) {
    bad <- coverage[first_date > last_date]
    stop(sprintf(
      paste0(
        "Selected component(s) have no overlapping price and nominal months: ",
        "%s."
      ),
      .uei_preview(bad$component_id)
    ), call. = FALSE)
  }

  common_first <- max(coverage$first_date)
  common_last <- min(coverage$last_date)
  start <- max(common_first, .uei_panel_start)
  if (start > common_last) {
    stop(sprintf(
      paste0(
        "No complete monthly panel exists on or after 1987-01: the common ",
        "start is %s and the common end is %s."
      ),
      format(common_first, "%Y-%m"), format(common_last, "%Y-%m")
    ), call. = FALSE)
  }
  months <- seq(start, common_last, by = "month")
  price_rows <- price_rows[date >= start & date <= common_last]
  nominal_rows <- nominal_rows[date >= start & date <= common_last]

  expected_price <- price_map[, .(date = months), by = .(component_id, line_number)]
  missing_price <- data.table::fsetdiff(
    expected_price, price_rows[, .(component_id, line_number, date)]
  )
  if (nrow(missing_price)) {
    stop(sprintf(
      paste0(
        "`price` is incomplete for the selected panel: %d required ",
        "component-month price observation(s) are missing. First: %s line %d ",
        "on %s."
      ),
      nrow(missing_price), missing_price$component_id[[1L]],
      missing_price$line_number[[1L]],
      format(missing_price$date[[1L]], "%Y-%m")
    ), call. = FALSE)
  }

  expected_nominal <- nominal_map[, .(date = months),
                                  by = .(component_id, line_number)]
  missing_nominal <- data.table::fsetdiff(
    expected_nominal, nominal_rows[, .(component_id, line_number, date)]
  )
  if (nrow(missing_nominal)) {
    stop(sprintf(
      paste0(
        "`nominal` is incomplete for the selected panel: %d required ",
        "line-month observation(s) are missing. First: %s line %d on %s."
      ),
      nrow(missing_nominal), missing_nominal$component_id[[1L]],
      missing_nominal$line_number[[1L]],
      format(missing_nominal$date[[1L]], "%Y-%m")
    ), call. = FALSE)
  }

  price_by_component <- price_rows[, .(
    price = value[[1L]]
  ), by = .(component_id, date)]
  nominal_by_component <- nominal_rows[, .(
    nominal = sum(value),
    n_lines = data.table::uniqueN(line_number)
  ), by = .(component_id, date)]
  panel <- merge(
    price_by_component, nominal_by_component,
    by = c("component_id", "date"), all = FALSE, sort = FALSE
  )
  expected_rows <- data.table::uniqueN(selected$component_id) * length(months)
  if (nrow(panel) != expected_rows) {
    stop(sprintf(
      paste0(
        "The aligned component panel has %d rows; expected %d complete ",
        "component-month rows."
      ),
      nrow(panel), expected_rows
    ), call. = FALSE)
  }

  data.table::setorder(panel, component_id, date)
  panel[, month_id := .uei_month_id(date)]
  panel[, month_previous := data.table::shift(month_id), by = component_id]
  bad <- panel[!is.na(month_previous) & month_id - month_previous != 1L]
  if (nrow(bad)) {
    stop(sprintf(
      "Non-contiguous calendar month for %s between %s and %s.",
      bad$component_id[[1L]],
      format(.uei_month_start(bad$month_previous[[1L]]), "%Y-%m"),
      format(bad$date[[1L]], "%Y-%m")
    ), call. = FALSE)
  }

  bad <- panel[nominal < 0]
  if (nrow(bad)) {
    worst <- bad[which.min(nominal)]
    stop(sprintf(
      paste0(
        "Merged nominal expenditure is negative for %s on %s (%s). ",
        "Nominal expenditure must be non-negative."
      ),
      worst$component_id, format(worst$date, "%Y-%m"),
      format(worst$nominal, big.mark = ",", digits = 12)
    ), call. = FALSE)
  }

  panel[, month_previous := NULL]
  panel[]
}

.uei_fisher_weights <- function(panel) {
  work <- data.table::copy(panel)
  data.table::setorder(work, component_id, month_id)
  work[, `:=`(
    price_previous = data.table::shift(price),
    nominal_previous = data.table::shift(nominal)
  ), by = component_id]
  work[, pi := price / price_previous - 1]
  work[, q_prior_price := (nominal / price) * price_previous]
  work[, valid := !is.na(price_previous) & !is.na(nominal_previous) &
        is.finite(pi) & is.finite(q_prior_price) &
        price_previous > 0 & nominal_previous >= 0 & q_prior_price >= 0]

  n_components <- data.table::uniqueN(work$component_id)
  sums <- work[, .(
    n_valid = sum(valid),
    sum_q = sum(q_prior_price[valid]),
    sum_n = sum(nominal_previous[valid])
  ), by = month_id]
  data.table::setorder(sums, month_id)
  sums[, row := seq_len(.N)]

  bad <- sums[
    row > 1L & n_valid != n_components
  ]
  if (nrow(bad)) {
    stop(sprintf(
      paste0(
        "Fisher weights are undefined for %s: only %d of %d selected ",
        "components have a valid current and preceding month."
      ),
      format(.uei_month_start(bad$month_id[[1L]]), "%Y-%m"),
      bad$n_valid[[1L]], n_components
    ), call. = FALSE)
  }
  bad <- sums[
    n_valid > 0L & (
      !is.finite(sum_q) | !is.finite(sum_n) |
        sum_q <= 0 | sum_n <= 0
    )
  ]
  if (nrow(bad)) {
    stop(sprintf(
      paste0(
        "Cannot compute Fisher weights for %s: valid rows have zero or ",
        "non-finite total Q_t * P_{t-1} or prior nominal expenditure."
      ),
      format(.uei_month_start(bad$month_id[[1L]]), "%Y-%m")
    ), call. = FALSE)
  }

  work[sums, on = "month_id", `:=`(sum_q = i.sum_q, sum_n = i.sum_n)]
  work[, fisher_weight := NA_real_]
  work[valid == TRUE, fisher_weight :=
         0.5 * q_prior_price / sum_q + 0.5 * nominal_previous / sum_n]
  work[valid == TRUE, fisher_weight :=
         fisher_weight / sum(fisher_weight), by = month_id]
  bad <- work[
    valid == TRUE & (!is.finite(fisher_weight) | fisher_weight < 0)
  ]
  if (nrow(bad)) {
    stop(sprintf(
      "Fisher weight is non-finite or negative for %s on %s.",
      bad$component_id[[1L]], format(bad$date[[1L]], "%Y-%m")
    ), call. = FALSE)
  }
  work[]
}

.uei_month_start <- function(month_id) {
  year <- (month_id - 1L) %/% 12L
  month <- (month_id - 1L) %% 12L + 1L
  as.Date(sprintf("%04d-%02d-01", year, month))
}

.uei_trimmed_mean <- function(x, w) {
  if (!is.numeric(x) || !is.numeric(w)) {
    stop("Trimmed-mean inputs must be numeric.", call. = FALSE)
  }
  if (length(x) != length(w) || length(x) == 0L) {
    stop("Trimmed-mean inputs must be non-empty and the same length.",
         call. = FALSE)
  }
  if (any(!is.finite(x)) || any(!is.finite(w))) {
    stop("Trimmed-mean inputs must be finite.", call. = FALSE)
  }
  if (any(w < 0)) {
    stop("Fisher weights must be non-negative.", call. = FALSE)
  }
  total <- sum(w)
  if (!is.finite(total) || total <= 0) {
    stop("Fisher weights must have a positive finite sum.", call. = FALSE)
  }
  w <- w / total

  ordering <- order(x)
  x <- x[ordering]
  w <- w[ordering]
  cumulative <- cumsum(w)
  previous <- c(0, cumulative[-length(cumulative)])
  retained <- pmax(
    0,
    pmin(cumulative, 1 - .uei_trim_fraction) -
      pmax(previous, .uei_trim_fraction)
  )
  retained_total <- sum(retained)
  if (!is.finite(retained_total) || retained_total <= 0) {
    stop("No Fisher weight remains after the 8 percent tail trims.",
         call. = FALSE)
  }
  sum(retained * x) / retained_total
}

.uei_compute_series <- function(weighted, months) {
  usable <- weighted[valid == TRUE]
  if (nrow(usable) == 0L) {
    rates <- data.table::data.table(
      month_id = integer(),
      monthly_decimal = numeric()
    )
  } else {
    rates <- usable[, .(
      monthly_decimal = .uei_trimmed_mean(pi, fisher_weight)
    ), by = month_id]
  }
  if (anyDuplicated(rates$month_id)) {
    stop("More than one trimmed mean was computed for a calendar month.",
         call. = FALSE)
  }

  grid <- data.table::data.table(
    date = months,
    month_id = .uei_month_id(months)
  )
  result <- merge(grid, rates, by = "month_id", all.x = TRUE, sort = TRUE)
  if (nrow(result) > 1L && anyNA(result$monthly_decimal[-1L])) {
    stop(sprintf(
      paste0(
        "Could not compute a monthly trimmed mean for %s; a later month ",
        "would otherwise carry the previous index forward."
      ),
      format(result$date[-1L][is.na(result$monthly_decimal[-1L])][[1L]], "%Y-%m")
    ), call. = FALSE)
  }

  result[, monthly := 100 * monthly_decimal]
  result[, index := 100 * cumprod(
    1 + data.table::fifelse(is.na(monthly_decimal), 0, monthly_decimal)
  )]
  lag_12 <- match(result$month_id - 12L, result$month_id)
  lag_6 <- match(result$month_id - 6L, result$month_id)
  lag_3 <- match(result$month_id - 3L, result$month_id)
  result[, yoy := 100 * (index / index[lag_12] - 1)]
  result[, annualized_6m := 100 * ((index / index[lag_6])^(12 / 6) - 1)]
  result[, annualized_3m := 100 * ((index / index[lag_3])^(12 / 3) - 1)]

  result[, date := as.Date(date)]
  result[, .(
    date, monthly, index, yoy, annualized_6m, annualized_3m
  )]
}

.uei_verify_identities <- function(nominal, components, start, end) {
  groups <- unique(components[
    !is.na(identity_parent),
    .(parent = identity_parent, children_text = identity_children)
  ])
  if (nrow(groups) == 0L) {
    return(invisible(TRUE))
  }
  groups[, children := lapply(children_text, function(text) {
    as.integer(strsplit(text, "+", fixed = TRUE)[[1L]])
  })]
  present <- unique(nominal$line_number)
  months <- seq(start, end, by = "month")

  for (index in seq_len(nrow(groups))) {
    parent <- groups$parent[[index]]
    children <- groups$children[[index]]
    if (!parent %in% present || !all(children %in% present)) {
      next
    }

    parent_rows <- nominal[
      line_number == parent & date >= start & date <= end,
      .(date, parent_value = value)
    ]
    child_rows <- nominal[
      line_number %in% children & date >= start & date <= end,
      .(
        child_value = sum(value),
        n_children = data.table::uniqueN(line_number)
      ),
      by = date
    ]
    if (nrow(parent_rows) != length(months) ||
        nrow(child_rows) != length(months) ||
        any(child_rows$n_children != length(children))) {
      stop(sprintf(
        paste0(
          "Cannot verify the nominal identity %d = %s over %s to %s: the ",
          "parent or a child line has missing months."
        ),
        parent, paste(children, collapse = " + "),
        format(start, "%Y-%m"), format(end, "%Y-%m")
      ), call. = FALSE)
    }

    checked <- merge(parent_rows, child_rows, by = "date", all = FALSE,
                     sort = TRUE)
    if (nrow(checked) != length(months)) {
      stop(sprintf(
        "Cannot verify the nominal identity %d = %s: date alignment failed.",
        parent, paste(children, collapse = " + ")
      ), call. = FALSE)
    }
    checked[, residual := parent_value - child_value]
    bad <- checked[
      is.na(residual) |
        abs(residual) > .uei_identity_tolerance +
          1e-8 * pmax(abs(parent_value), abs(child_value))
    ]
    if (nrow(bad)) {
      worst <- bad[which.max(abs(residual))]
      stop(sprintf(
        paste0(
          "Nominal accounting identity failed: line %d = %s.\n",
          "  Largest absolute residual: %s in %s (tolerance %s).\n",
          "  Failing months: %d of %d."
        ),
        parent, paste(children, collapse = " + "),
        format(worst$residual, big.mark = ",", digits = 12),
        format(worst$date, "%Y-%m"),
        format(.uei_identity_tolerance, big.mark = ",", digits = 0),
        nrow(bad), nrow(checked)
      ), call. = FALSE)
    }
  }
  invisible(TRUE)
}

#' Construct the market-based ex-housing 16 percent symmetric trimmed mean
#'
#' @param price Long BEA U20404-style price table with `line_number`,
#'   `series_code`, `description`, `date`, and `value`.
#' @param nominal Long BEA U20405-style nominal table with the same columns.
#'   Nominal values may be negative at the expanded-line level (line 16 is an
#'   employee-reimbursement offset), but the merged component nominal must be
#'   non-negative.
#' @param components_path Path to the subproject `components.csv`. The default
#'   resolves to `underlying-inflation/config/components.csv`.
#'
#' @return A data.table with one row per calendar month and columns `date`,
#'   `monthly`, `index`, `yoy`, `annualized_6m`, and `annualized_3m`.
#'   `monthly`, `yoy`, `annualized_6m`, and `annualized_3m` are in percent;
#'   `index` is a chained level equal to 100 in the first month. The first month
#'   has `NA` monthly, year-over-year, and annualized values because it has no
#'   earlier index. Year-over-year needs an exact 12-month lag and the
#'   annualized measures need exact 6- and 3-month lags.
construct_market_ex_housing_tm16 <- function(
    price, nominal, components_path = .uei_default_components_path()) {
  components <- load_component_crosswalk(components_path)

  price <- .uei_prepare_input(price, "price", "U20404", components)
  nominal <- .uei_prepare_input(nominal, "nominal", "U20405", components)

  selected <- components[market_based & !housing]
  if (nrow(selected) == 0L) {
    stop("The component lock has no market-based, non-housing components.",
         call. = FALSE)
  }

  .uei_require_component_lines(
    price,
    unique(selected[, .(component_id, line_number = price_line)]),
    "price"
  )
  .uei_require_component_lines(
    nominal,
    unique(selected[, .(component_id, line_number)]),
    "nominal"
  )

  panel <- .uei_build_panel(price, nominal, selected)
  months <- seq(min(panel$date), max(panel$date), by = "month")
  .uei_verify_identities(
    nominal, components, min(months), max(months)
  )
  weighted <- .uei_fisher_weights(panel)
  .uei_compute_series(weighted, months)
}
