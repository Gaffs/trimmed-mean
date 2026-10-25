# Underlying-inflation comparison figures.
#
# Standalone ggplot2/data.table plotting module; source this file directly.
# Dependencies are ggplot2, data.table, and grid (all already used elsewhere
# in this project).
#
# create_comparison_graphs(rates, output_dir) writes exactly three PNGs at
# 1800 x 1000 pixels (9 x 5 inches at 200 dpi):
#   01_year_on_year.png
#   02_six_month_annualized.png
#   03_three_month_annualized.png
#
# `rates` is a list of three long data frames/data.tables, one per
# transformation, each with columns:
#   series  headline, core, dallas, or market_ex_housing_tm16
#   date    Date, POSIXct, or ISO-8601 character; one observation per month
#   value   numeric percent, possibly NA before a series' first observation
# Elements may be named yoy/12m, six/6m, and three/3m (output stems also
# work); an unnamed list is read in the order above. A single long table is
# also accepted when it has a measure/rate/window/transformation column with
# those three values. Every chart runs from the earliest non-missing
# observation to the latest one, and all four series in a rate table must
# reach the same latest month.
#
# Only the four requested series are drawn. Recession shading is
# intentionally omitted because this module fetches no external data.

if (!requireNamespace("data.table", quietly = TRUE)) {
  stop("Package 'data.table' is required.", call. = FALSE)
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  stop("Package 'ggplot2' is required.", call. = FALSE)
}

.uip_source_caption <- paste0(
  "Source: BEA; Dallas Fed/FRED; calculations following Employ America; ",
  "independent reconstruction."
)

.uip_required_series <- c(
  "headline", "core", "dallas", "market_ex_housing_tm16"
)

.uip_series_labels <- c(
  headline = "Headline PCE",
  core = "Core PCE",
  dallas = "Dallas Fed Trimmed Mean",
  market_ex_housing_tm16 = "Market-Based Ex-Housing 16% TM"
)

.uip_series_colours <- c(
  "Headline PCE" = "#6F747C",
  "Core PCE" = "#0B7A5A",
  "Dallas Fed Trimmed Mean" = "#2C5F9E",
  "Market-Based Ex-Housing 16% TM" = "#C05621"
)

.uip_rate_order <- c(
  "year_on_year", "six_month_annualized", "three_month_annualized"
)

.uip_rate_measure_columns <- c("measure", "rate", "window", "transformation")

.uip_classify_measure <- function(x) {
  if (grepl("six|6", x)) {
    "six_month_annualized"
  } else if (grepl("three|3", x)) {
    "three_month_annualized"
  } else if (grepl("yoy|year|12", x)) {
    "year_on_year"
  } else {
    NA_character_
  }
}

.uip_colours <- c(
  background = "#F9F7F5",
  grid = "#DEDCD9",
  axis_line = "#8A8783",
  axis_text = "#5D5C5B",
  caption = "#9B9A98",
  zero_line = "#C9C6C1"
)

.uip_stop <- function(...) {
  stop(..., call. = FALSE)
}

.uip_month_year <- function(date) {
  date <- as.Date(date)
  paste0(
    month.name[as.integer(format(date, "%m"))],
    " ",
    format(date, "%Y")
  )
}

.uip_measure_specs <- function() {
  list(
    year_on_year = list(
      filename = "01_year_on_year.png",
      title = paste0(
        "Year-over-Year PCE Inflation:\n",
        "Headline, Core, and Trimmed Means"
      ),
      change = "12-month percent change",
      y_label = "Percent change"
    ),
    six_month_annualized = list(
      filename = "02_six_month_annualized.png",
      title = paste0(
        "Six-Month Annualized PCE Inflation:\n",
        "Headline, Core, and Trimmed Means"
      ),
      change = "Annualized percent change over six months (6m/6m)",
      y_label = "Annualized percent change"
    ),
    three_month_annualized = list(
      filename = "03_three_month_annualized.png",
      title = paste0(
        "Three-Month Annualized PCE Inflation:\n",
        "Headline, Core, and Trimmed Means"
      ),
      change = "Annualized percent change over three months (3m/3m)",
      y_label = "Annualized percent change"
    )
  )
}

.uip_output_dir <- function(output_dir) {
  if (!is.character(output_dir) || length(output_dir) != 1L ||
      is.na(output_dir) || !nzchar(output_dir)) {
    .uip_stop("`output_dir` must be a single non-empty string.")
  }
  if (!dir.exists(output_dir) &&
      !dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)) {
    .uip_stop(sprintf("Could not create output directory: %s", output_dir))
  }
  output_dir
}

.uip_as_date <- function(x, key) {
  if (inherits(x, "Date")) {
    out <- as.Date(x)
  } else if (inherits(x, "POSIXt")) {
    tz <- attr(x, "tzone")
    if (length(tz) != 1L || is.na(tz) || !nzchar(tz)) {
      tz <- ""
    }
    out <- as.Date(x, tz = tz)
  } else if (is.character(x) || is.factor(x)) {
    out <- tryCatch(
      as.Date(as.character(x)),
      error = function(e) rep(as.Date(NA), length(x))
    )
  } else {
    .uip_stop(sprintf(
      "`rates$%s$date` must be a Date, POSIXct, or ISO-8601 character column.",
      key
    ))
  }
  if (anyNA(out)) {
    .uip_stop(sprintf(
      "`rates$%s$date` contains missing or unparseable dates.",
      key
    ))
  }
  out
}

.uip_rate_keys <- function(rates) {
  nm <- names(rates)
  blank <- if (is.null(nm)) rep(TRUE, length(rates)) else {
    is.na(nm) | !nzchar(nm)
  }
  if (all(blank)) {
    return(.uip_rate_order)
  }
  if (any(blank)) {
    .uip_stop(
      "Name every element of `rates`, or leave all three elements unnamed."
    )
  }

  normalized <- gsub("[^a-z0-9]+", "_", tolower(trimws(nm)))
  keys <- vapply(
    normalized, .uip_classify_measure, character(1), USE.NAMES = FALSE
  )

  if (anyNA(keys)) {
    .uip_stop(paste0(
      "Could not identify the transformation for `rates` element(s): ",
      paste(sprintf("`%s`", nm[is.na(keys)]), collapse = ", "),
      ". Use names containing yoy/year/12m, six/6m, or three/3m."
    ))
  }
  if (anyDuplicated(keys) > 0L) {
    .uip_stop("`rates` names do not identify three distinct transformations.")
  }
  missing <- setdiff(.uip_rate_order, keys)
  if (length(missing) > 0L) {
    .uip_stop(sprintf(
      "`rates` is missing transformation(s): %s.",
      paste(sprintf("`%s`", missing), collapse = ", ")
    ))
  }
  keys
}

.uip_split_rate_table <- function(rates) {
  measure_columns <- intersect(.uip_rate_measure_columns, names(rates))
  if (length(measure_columns) == 0L) {
    .uip_stop(paste0(
      "A single `rates` data frame must include a transformation column (",
      paste(sprintf("`%s`", .uip_rate_measure_columns), collapse = ", "),
      "), or pass a named list of three tables."
    ))
  }
  measure_column <- measure_columns[[1L]]
  measure <- tolower(trimws(as.character(rates[[measure_column]])))
  if (anyNA(measure) || any(!nzchar(measure))) {
    .uip_stop(sprintf(
      "`rates$%s` must not contain missing or empty values.",
      measure_column
    ))
  }
  keys <- vapply(
    measure, .uip_classify_measure, character(1), USE.NAMES = FALSE
  )
  if (anyNA(keys)) {
    examples <- unique(measure[is.na(keys)])
    examples <- examples[seq_len(min(5L, length(examples)))]
    .uip_stop(paste0(
      "Could not identify the transformation for `rates$", measure_column,
      "` value(s): ", paste(sprintf("`%s`", examples), collapse = ", "),
      ". Use values containing yoy/year/12m, six/6m, or three/3m."
    ))
  }
  present <- unique(keys)
  missing <- setdiff(.uip_rate_order, present)
  if (length(missing) > 0L) {
    .uip_stop(sprintf(
      "`rates` is missing transformation(s): %s.",
      paste(sprintf("`%s`", missing), collapse = ", ")
    ))
  }
  if (length(present) != length(.uip_rate_order)) {
    .uip_stop("`rates` must contain exactly three transformations.")
  }

  out <- lapply(.uip_rate_order, function(key) {
    rows <- which(keys == key)
    rates[rows, ]
  })
  names(out) <- .uip_rate_order
  out
}

.uip_prepare_rate_table <- function(x, key) {
  if (!is.data.frame(x)) {
    .uip_stop(sprintf(
      "`rates$%s` must be a data frame or data.table.",
      key
    ))
  }
  if (anyDuplicated(names(x)) > 0L) {
    .uip_stop(sprintf("`rates$%s` must have unique column names.", key))
  }
  missing_columns <- setdiff(c("series", "date", "value"), names(x))
  if (length(missing_columns) > 0L) {
    .uip_stop(sprintf(
      "`rates$%s` is missing required column(s): %s.",
      key, paste(sprintf("`%s`", missing_columns), collapse = ", ")
    ))
  }

  dt <- data.table::copy(data.table::as.data.table(x))
  if (nrow(dt) == 0L) {
    .uip_stop(sprintf("`rates$%s` has no rows.", key))
  }

  parsed_series <- tolower(trimws(as.character(dt[["series"]])))
  if (anyNA(parsed_series) || any(!nzchar(parsed_series))) {
    .uip_stop(sprintf(
      "`rates$%s$series` must not contain missing or empty values.",
      key
    ))
  }
  dt[, series := parsed_series]

  parsed_date <- .uip_as_date(dt[["date"]], key)
  dt[, date := parsed_date]

  parsed_value <- dt[["value"]]
  if (!is.numeric(parsed_value)) {
    .uip_stop(sprintf("`rates$%s$value` must be numeric.", key))
  }
  if (any(!is.finite(parsed_value) & !is.na(parsed_value))) {
    .uip_stop(sprintf(
      "`rates$%s$value` must contain only finite values or NA.",
      key
    ))
  }
  dt[, value := as.numeric(parsed_value)]

  normalized_date <- as.Date(sprintf(
    "%04d-%02d-01",
    as.integer(format(dt[["date"]], "%Y")),
    as.integer(format(dt[["date"]], "%m"))
  ))
  dt[, date := normalized_date]

  duplicates <- dt[, .N, by = c("series", "date")][N > 1L]
  if (nrow(duplicates) > 0L) {
    example <- duplicates[1L]
    .uip_stop(sprintf(
      "`rates$%s` has more than one observation for series `%s` in %s.",
      key, example[["series"]], .uip_month_year(example[["date"]])
    ))
  }

  extras <- setdiff(unique(dt[["series"]]), .uip_required_series)
  if (length(extras) > 0L) {
    warning(sprintf(
      "`rates$%s` contains non-required series (%s); they will not be plotted.",
      key, paste(extras, collapse = ", ")
    ), call. = FALSE)
    dt <- dt[series %in% .uip_required_series]
  }

  missing_series <- setdiff(.uip_required_series, unique(dt[["series"]]))
  if (length(missing_series) > 0L) {
    .uip_stop(sprintf(
      "`rates$%s` is missing required series: %s.",
      key, paste(sprintf("`%s`", missing_series), collapse = ", ")
    ))
  }

  present <- dt[!is.na(value)]
  empty_series <- setdiff(.uip_required_series, unique(present[["series"]]))
  if (length(empty_series) > 0L) {
    .uip_stop(sprintf(
      "`rates$%s` has no non-missing values for series: %s.",
      key, paste(sprintf("`%s`", empty_series), collapse = ", ")
    ))
  }

  latest_by <- present[, .(latest = max(date)), by = series]
  latest_by <- latest_by[match(.uip_required_series, latest_by[["series"]])]
  if (anyNA(latest_by[["latest"]])) {
    .uip_stop(sprintf(
      "`rates$%s` is missing a latest observation for at least one series.",
      key
    ))
  }
  if (data.table::uniqueN(latest_by[["latest"]]) != 1L) {
    detail <- paste(
      sprintf(
        "%s through %s",
        latest_by[["series"]],
        .uip_month_year(latest_by[["latest"]])
      ),
      collapse = "; "
    )
    .uip_stop(sprintf(
      "`rates$%s` series do not share the same latest date (%s). Align the data before plotting.",
      key, detail
    ))
  }
  latest <- latest_by[["latest"]][1L]

  out <- present[, .(series, date, value)]
  data.table::setorderv(out, c("series", "date"))

  list(
    data = out,
    start = min(out[["date"]]),
    latest = latest
  )
}

.uip_prepare_rates <- function(rates) {
  if (is.data.frame(rates)) {
    rates <- .uip_split_rate_table(rates)
  }
  if (!is.list(rates) || length(rates) != 3L) {
    .uip_stop("`rates` must be a list of exactly three long data frames.")
  }

  keys <- .uip_rate_keys(rates)
  prepared <- vector("list", 3L)
  names(prepared) <- keys
  for (i in seq_along(keys)) {
    prepared[[keys[[i]]]] <- .uip_prepare_rate_table(rates[[i]], keys[[i]])
  }
  prepared[.uip_rate_order]
}

.uip_year_breaks <- function(start, end) {
  start_year <- as.integer(format(start, "%Y"))
  end_year <- as.integer(format(end, "%Y"))
  span <- end_year - start_year + 1L
  step <- if (span <= 12L) {
    1L
  } else if (span <= 45L) {
    2L
  } else {
    5L
  }
  first <- start_year + ((step - start_year %% step) %% step)
  years <- seq.int(first, end_year, by = step)
  breaks <- as.Date(sprintf("%04d-01-01", years))
  breaks <- breaks[breaks >= start & breaks <= end]
  if (length(breaks) == 0L) {
    breaks <- start
  }
  breaks
}

.uip_y_labels <- function(x) {
  if (all(abs(x - round(x)) < 1e-8)) {
    sprintf("%d%%", as.integer(round(x)))
  } else {
    sprintf("%.1f%%", x)
  }
}

.uip_subtitle <- function(spec, start, latest) {
  if (start == latest) {
    sprintf("%s, %s.", spec[["change"]], .uip_month_year(latest))
  } else {
    sprintf(
      "%s, %s to %s.",
      spec[["change"]], .uip_month_year(start), .uip_month_year(latest)
    )
  }
}

.uip_theme <- function(base_size = 11) {
  ggplot2::theme_minimal(base_size = base_size, base_family = "sans") +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(
        fill = .uip_colours[["background"]], colour = NA
      ),
      panel.background = ggplot2::element_rect(
        fill = .uip_colours[["background"]], colour = NA
      ),
      panel.grid.major = ggplot2::element_line(
        colour = .uip_colours[["grid"]], linewidth = 0.25
      ),
      panel.grid.minor = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(
        colour = .uip_colours[["axis_line"]], linewidth = 0.35
      ),
      axis.ticks = ggplot2::element_line(
        colour = .uip_colours[["axis_line"]], linewidth = 0.3
      ),
      axis.ticks.length = grid::unit(2.5, "pt"),
      axis.text = ggplot2::element_text(
        colour = .uip_colours[["axis_text"]], size = ggplot2::rel(0.9)
      ),
      axis.text.x = ggplot2::element_text(
        margin = ggplot2::margin(t = 4)
      ),
      axis.text.y = ggplot2::element_text(
        margin = ggplot2::margin(r = 4)
      ),
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_text(
        size = ggplot2::rel(0.9),
        colour = .uip_colours[["axis_text"]],
        margin = ggplot2::margin(r = 8)
      ),
      plot.title = ggplot2::element_text(
        face = "bold",
        size = ggplot2::rel(1.95),
        colour = "#111111",
        hjust = 0,
        lineheight = 0.95,
        margin = ggplot2::margin(b = 5)
      ),
      plot.subtitle = ggplot2::element_text(
        size = ggplot2::rel(1.02),
        colour = "#333333",
        hjust = 0,
        lineheight = 1.05,
        margin = ggplot2::margin(b = 6)
      ),
      plot.caption = ggplot2::element_text(
        face = "italic",
        size = ggplot2::rel(0.66),
        colour = .uip_colours[["caption"]],
        hjust = 0,
        margin = ggplot2::margin(t = 8)
      ),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.margin = ggplot2::margin(10, 22, 9, 26),
      legend.position = "top",
      legend.justification = "left",
      legend.direction = "horizontal",
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(
        size = ggplot2::rel(0.95), colour = "#222222"
      ),
      legend.key.width = grid::unit(2.2, "lines"),
      legend.key.height = grid::unit(0.9, "lines"),
      legend.spacing.x = grid::unit(0.8, "lines"),
      legend.box.spacing = grid::unit(2, "pt"),
      legend.box.margin = ggplot2::margin(0, 0, 4, 0),
      legend.background = ggplot2::element_blank(),
      legend.key = ggplot2::element_blank()
    )
}

.uip_comparison_plot <- function(rate, spec) {
  data <- data.table::copy(rate[["data"]])
  labels <- .uip_series_labels
  data[, series_label := factor(
    unname(labels[series]),
    levels = unname(labels)
  )]
  if (anyNA(data[["series_label"]])) {
    .uip_stop("Could not map every series to a display label.")
  }

  start <- rate[["start"]]
  latest <- rate[["latest"]]
  axis_start <- start
  axis_end <- latest
  if (axis_start == axis_end) {
    axis_start <- axis_start - 31
    axis_end <- axis_end + 31
  }

  values <- data[["value"]]
  lo <- min(c(0, values), na.rm = TRUE)
  hi <- max(c(0, values), na.rm = TRUE)
  if (!is.finite(lo) || !is.finite(hi)) {
    .uip_stop("Cannot draw a chart with no finite values.")
  }
  if (hi == lo) {
    hi <- lo + 1
  }
  padding <- max(0.25, 0.05 * (hi - lo))
  y_breaks <- pretty(c(lo - padding, hi + padding), n = 6L)
  y_limits <- range(y_breaks)

  ggplot2::ggplot(
    data,
    ggplot2::aes(x = date, y = value, colour = series_label)
  ) +
    ggplot2::geom_hline(
      yintercept = 0,
      colour = .uip_colours[["zero_line"]],
      linewidth = 0.35
    ) +
    ggplot2::geom_line(
      linewidth = 0.8,
      lineend = "round",
      na.rm = TRUE
    ) +
    ggplot2::scale_colour_manual(
      values = .uip_series_colours,
      breaks = unname(labels),
      name = NULL,
      guide = ggplot2::guide_legend(
        nrow = 1,
        byrow = TRUE,
        override.aes = list(linewidth = 0.9)
      )
    ) +
    ggplot2::scale_x_date(
      limits = c(axis_start, axis_end),
      breaks = .uip_year_breaks(axis_start, axis_end),
      labels = function(x) format(x, "%Y"),
      expand = ggplot2::expansion(mult = c(0.004, 0.012))
    ) +
    ggplot2::scale_y_continuous(
      limits = y_limits,
      breaks = y_breaks,
      labels = .uip_y_labels,
      expand = ggplot2::expansion(mult = c(0.01, 0.03))
    ) +
    ggplot2::labs(
      title = spec[["title"]],
      subtitle = .uip_subtitle(spec, start, latest),
      caption = .uip_source_caption,
      x = NULL,
      y = spec[["y_label"]]
    ) +
    .uip_theme()
}

#' Create the three underlying-inflation comparison figures
#'
#' @param rates A list of three long data frames, one per transformation, or a
#'   single long table with a transformation column. Each observation has
#'   columns `series`, `date`, and `value`; see the file header.
#' @param output_dir Directory for the three PNG files.
#'
#' @return A named character vector of PNG paths, in chart order.
create_comparison_graphs <- function(rates, output_dir) {
  output_dir <- .uip_output_dir(output_dir)
  prepared <- .uip_prepare_rates(rates)
  specs <- .uip_measure_specs()

  latest <- do.call(c, lapply(prepared, function(x) x[["latest"]]))
  if (data.table::uniqueN(latest) > 1L) {
    detail <- paste(
      sprintf("%s through %s", names(prepared), .uip_month_year(latest)),
      collapse = "; "
    )
    warning(sprintf(
      "The three rate tables end in different months (%s); each chart is labeled with its own latest month.",
      detail
    ), call. = FALSE)
  }

  paths <- character(length(specs))
  names(paths) <- names(specs)

  for (key in names(specs)) {
    spec <- specs[[key]]
    path <- file.path(output_dir, spec[["filename"]])
    ggplot2::ggsave(
      filename = path,
      plot = .uip_comparison_plot(prepared[[key]], spec),
      width = 9,
      height = 5,
      units = "in",
      dpi = 200,
      bg = .uip_colours[["background"]],
      device = "png"
    )
    paths[[key]] <- path
  }

  paths
}
