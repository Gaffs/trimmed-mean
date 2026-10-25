# Employ America trimmed-mean figures.
#
# Public entry points:
#   create_all_figures(measures, benchmarks, weighted_panel,
#                      output_dir = "output/figures")
#   write_figure_data(measures, weighted_panel,
#                     output_dir = "output/figures")
#
# `measures` is the long output of compute_all_variants(long = TRUE) with
# columns variant, date, monthly, index, and yoy; monthly and yoy are percent.
# `benchmarks` is long with series (headline, core, dallas), date, and value;
# value is year-over-year percent. `weighted_panel` is a latest-month
# distribution from compute_fisher_weights() with component_id, pi, and
# fisher_weight, plus description and/or date when available. A full dated
# panel is accepted and reduced to its latest usable month.

.plots_source_caption <- paste0(
  "Source: BEA; Dallas Fed; calculations following Employ America (2026). ",
  "Independent reconstruction; see component crosswalk."
)

.plots_colors <- c(
  dallas = "#29679B",
  core = "#008A6A",
  headline = "#64748B",
  ea16 = "#40B2FF",
  navy = "#2E2A73",
  green = "#008A6A",
  grey = "#64748B",
  background = "#F9F7F5",
  grid = "#DFDEDE",
  axis_text = "#5D5C5B",
  axis_line = "#7C7B7A",
  caption = "#9B9A98",
  note = "#8390A2"
)

.plots_required_variants <- c(
  "tm16", "tm48", "median",
  "tm16_ex_housing", "tm16_market", "tm16_market_ex_housing"
)

.plots_start_month <- 2018L * 12L + 1L
.plots_y_limit <- 30

.plots_stop <- function(...) {
  stop(..., call. = FALSE)
}

.plots_trim_spec <- function(variant) {
  spec <- trim_specifications()[[variant]]
  if (is.null(spec)) {
    .plots_stop(sprintf("Unknown trim variant `%s`.", variant))
  }
  spec
}

.plots_trim_percent <- function(variant) {
  spec <- .plots_trim_spec(variant)
  100 * (spec$lower + spec$upper)
}

.plots_trim_label <- function(variant) {
  sprintf("%.0f%%", .plots_trim_percent(variant))
}

.plots_trim_boundaries <- function() {
  tm16 <- .plots_trim_spec("tm16")
  dallas <- .plots_trim_spec("dallas_replication")
  list(
    low_16 = tm16$lower,
    high_16 = 1 - tm16$upper,
    low_dallas = dallas$lower,
    high_dallas = 1 - dallas$upper
  )
}

.plots_status_levels <- function() {
  c(
    "Kept by both",
    sprintf("Kept by %s, trimmed by Dallas", .plots_trim_label("tm16")),
    "Trimmed by both"
  )
}

.plots_assert_dt <- function(x, name) {
  if (!data.table::is.data.table(x)) {
    .plots_stop(sprintf("`%s` must be a data.table.", name))
  }
  if (anyDuplicated(names(x)) > 0L) {
    .plots_stop(sprintf("`%s` must have unique column names.", name))
  }
  if (nrow(x) == 0L) {
    .plots_stop(sprintf("`%s` must have at least one row.", name))
  }
  invisible(TRUE)
}

.plots_require_columns <- function(x, columns, name) {
  missing <- setdiff(columns, names(x))
  if (length(missing) > 0L) {
    .plots_stop(sprintf(
      "`%s` is missing required column(s): %s.",
      name, paste(sprintf("`%s`", missing), collapse = ", ")
    ))
  }
  invisible(TRUE)
}

.plots_date <- function(x, name) {
  if (!inherits(x, "Date")) {
    .plots_stop(sprintf("`%s` must be a Date column.", name))
  }
  out <- as.Date(x)
  if (anyNA(out)) {
    .plots_stop(sprintf("`%s` must not contain missing values.", name))
  }
  out
}

.plots_month_id <- function(x) {
  as.integer(data.table::year(x)) * 12L + as.integer(data.table::month(x))
}

.plots_month_date <- function(id) {
  year <- (id - 1L) %/% 12L
  month <- (id - 1L) %% 12L + 1L
  as.Date(sprintf("%04d-%02d-01", year, month))
}

.plots_month_name <- function(date) {
  paste0(month.name[as.integer(format(date, "%m"))], " ", format(date, "%Y"))
}

.plots_date_labels <- function(x) {
  paste0(
    month.abb[as.integer(format(x, "%m"))],
    "-",
    substr(format(x, "%Y"), 3L, 4L)
  )
}

.plots_date_breaks <- function(start, end) {
  seq(from = start, to = end, by = "6 months")
}

.plots_output_dir <- function(output_dir) {
  if (!is.character(output_dir) || length(output_dir) != 1L ||
      is.na(output_dir) || !nzchar(output_dir)) {
    .plots_stop("`output_dir` must be a single non-empty string.")
  }
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }
  if (!dir.exists(output_dir)) {
    .plots_stop(sprintf("Could not create output directory: %s", output_dir))
  }
  output_dir
}

.plots_prepare_measures <- function(measures,
                                    required_variants = character()) {
  .plots_assert_dt(measures, "measures")
  .plots_require_columns(
    measures,
    c("variant", "date", "monthly", "index", "yoy"),
    "measures"
  )

  variant <- tolower(trimws(as.character(measures[["variant"]])))
  if (anyNA(variant) || any(!nzchar(variant))) {
    .plots_stop("`measures$variant` must not contain missing or empty values.")
  }
  date <- .plots_date(measures[["date"]], "measures$date")

  for (column in c("monthly", "index", "yoy")) {
    value <- measures[[column]]
    if (!is.numeric(value)) {
      .plots_stop(sprintf("`measures$%s` must be numeric.", column))
    }
    if (any(!is.finite(value) & !is.na(value))) {
      .plots_stop(sprintf(
        "`measures$%s` must contain only finite values or NA.", column
      ))
    }
  }

  out <- data.table::data.table(
    variant = variant,
    date = date,
    monthly = as.numeric(measures[["monthly"]]),
    index = as.numeric(measures[["index"]]),
    yoy = as.numeric(measures[["yoy"]])
  )
  out[, month := .plots_month_id(date)]
  out[, date := .plots_month_date(month)]

  if (anyDuplicated(out, by = c("variant", "month")) > 0L) {
    .plots_stop(
      "`measures` must have at most one row per variant and calendar month."
    )
  }

  missing <- setdiff(required_variants, unique(out[["variant"]]))
  if (length(missing) > 0L) {
    .plots_stop(sprintf(
      "`measures` is missing required variant(s): %s.",
      paste(sprintf("`%s`", missing), collapse = ", ")
    ))
  }

  data.table::setcolorder(
    out,
    c("variant", "date", "month", "monthly", "index", "yoy")
  )
  out[]
}

.plots_prepare_benchmarks <- function(benchmarks) {
  .plots_assert_dt(benchmarks, "benchmarks")
  .plots_require_columns(
    benchmarks,
    c("series", "date", "value"),
    "benchmarks"
  )

  series <- tolower(trimws(as.character(benchmarks[["series"]])))
  if (anyNA(series) || any(!nzchar(series))) {
    .plots_stop("`benchmarks$series` must not contain missing or empty values.")
  }
  date <- .plots_date(benchmarks[["date"]], "benchmarks$date")
  value <- benchmarks[["value"]]
  if (!is.numeric(value)) {
    .plots_stop("`benchmarks$value` must be numeric.")
  }
  if (any(!is.finite(value) & !is.na(value))) {
    .plots_stop("`benchmarks$value` must contain only finite values or NA.")
  }

  out <- data.table::data.table(
    series = series,
    date = date,
    value = as.numeric(value)
  )
  out[, month := .plots_month_id(date)]
  out[, date := .plots_month_date(month)]

  if (anyDuplicated(out, by = c("series", "month")) > 0L) {
    .plots_stop(
      "`benchmarks` must have at most one row per series and calendar month."
    )
  }

  required <- c("dallas", "core", "headline")
  missing <- setdiff(required, unique(out[["series"]]))
  if (length(missing) > 0L) {
    .plots_stop(sprintf(
      "`benchmarks` is missing required series: %s.",
      paste(sprintf("`%s`", missing), collapse = ", ")
    ))
  }

  data.table::setorderv(out, c("series", "date"))
  out[]
}

.plots_prepare_distribution <- function(weighted_panel, fallback_date) {
  .plots_assert_dt(weighted_panel, "weighted_panel")
  .plots_require_columns(
    weighted_panel,
    c("component_id", "pi", "fisher_weight"),
    "weighted_panel"
  )

  component_id <- weighted_panel[["component_id"]]
  if (!is.atomic(component_id)) {
    .plots_stop("`weighted_panel$component_id` must be an atomic vector.")
  }
  if (anyNA(component_id)) {
    .plots_stop("`weighted_panel$component_id` must not contain missing values.")
  }

  pi <- weighted_panel[["pi"]]
  weight <- weighted_panel[["fisher_weight"]]
  if (!is.numeric(pi)) {
    .plots_stop("`weighted_panel$pi` must be numeric.")
  }
  if (!is.numeric(weight)) {
    .plots_stop("`weighted_panel$fisher_weight` must be numeric.")
  }
  if (any(!is.finite(pi) & !is.na(pi))) {
    .plots_stop("`weighted_panel$pi` must contain only finite values or NA.")
  }
  if (any(!is.finite(weight) & !is.na(weight))) {
    .plots_stop(
      "`weighted_panel$fisher_weight` must contain only finite values or NA."
    )
  }
  if (any(weight < 0, na.rm = TRUE)) {
    .plots_stop("`weighted_panel$fisher_weight` must be non-negative.")
  }
  if (any(pi <= -1, na.rm = TRUE)) {
    .plots_stop("`weighted_panel$pi` must be greater than -1.")
  }

  description <- if ("description" %in% names(weighted_panel)) {
    as.character(weighted_panel[["description"]])
  } else {
    rep(NA_character_, nrow(weighted_panel))
  }

  dist <- data.table::data.table(
    component_id = component_id,
    description = description,
    pi = as.numeric(pi),
    fisher_weight = as.numeric(weight)
  )

  has_date <- "date" %in% names(weighted_panel)
  if (has_date) {
    date <- .plots_date(weighted_panel[["date"]], "weighted_panel$date")
    dist[, date := date]
    usable <- !is.na(dist[["pi"]]) &
      !is.na(dist[["fisher_weight"]]) &
      dist[["fisher_weight"]] > 0
    if (!any(usable)) {
      .plots_stop(
        "`weighted_panel` has no date with usable pi and fisher_weight values."
      )
    }
    latest <- max(dist[["date"]][usable])
    dist <- dist[dist[["date"]] == latest]
    dist[, date := NULL]
    dist_date <- latest
  } else {
    dist_date <- .plots_date(fallback_date, "fallback_date")
  }

  keep <- !is.na(dist[["pi"]]) &
    !is.na(dist[["fisher_weight"]]) &
    dist[["fisher_weight"]] > 0
  dropped <- sum(!keep)
  if (dropped > 0L) {
    warning(sprintf(
      "Dropped %d component%s from the distribution because pi or fisher_weight was missing or non-positive.",
      dropped, if (dropped == 1L) "" else "s"
    ), call. = FALSE)
  }
  dist <- dist[keep]
  if (nrow(dist) == 0L) {
    .plots_stop("No usable components remain in `weighted_panel`.")
  }
  if (anyDuplicated(dist[["component_id"]]) > 0L) {
    .plots_stop(
      "`weighted_panel` must have at most one row per component in the latest month."
    )
  }

  annualized <- 100 * ((1 + dist[["pi"]])^12 - 1)
  if (any(!is.finite(annualized))) {
    .plots_stop("Annualized component inflation is not finite.")
  }

  total_weight <- sum(dist[["fisher_weight"]])
  if (!is.finite(total_weight) || total_weight <= 0) {
    .plots_stop("`weighted_panel$fisher_weight` must have a positive sum.")
  }

  dist[, weight_share := fisher_weight / total_weight]
  data.table::setorderv(dist, c("pi", "component_id"))
  dist[, annualized_pct := 100 * ((1 + pi)^12 - 1)]

  cumulative <- cumsum(dist[["weight_share"]])
  cumulative[length(cumulative)] <- 1
  before <- c(0, cumulative[-length(cumulative)])
  dist[, `:=`(
    cum_before = before,
    cumulative_weight_pct = 100 * cumulative
  )]

  # Compare each component's cumulative-weight interval with the retained
  # intervals; boundary components are credited to every rule that keeps any
  # of their weight rather than being classified by their center alone.
  trim_bounds <- .plots_trim_boundaries()
  overlap_16 <- pmax(
    0,
    pmin(cumulative, trim_bounds$high_16) - pmax(before, trim_bounds$low_16)
  )
  overlap_dallas <- pmax(
    0,
    pmin(cumulative, trim_bounds$high_dallas) -
      pmax(before, trim_bounds$low_dallas)
  )

  tolerance <- 1e-12
  dist[, `:=`(
    kept_16 = overlap_16 > tolerance,
    kept_dallas = overlap_dallas > tolerance
  )]
  status_levels <- .plots_status_levels()
  dist[, status := data.table::fcase(
    kept_16 & kept_dallas, status_levels[[1L]],
    kept_16 & !kept_dallas, status_levels[[2L]],
    default = status_levels[[3L]]
  )]
  dist[, status := factor(status, levels = status_levels)]
  dist[, date := as.Date(dist_date)]

  n_components <- data.table::uniqueN(dist[["component_id"]])
  if (n_components != nrow(dist)) {
    .plots_stop("`weighted_panel` component identifiers are not unique.")
  }

  cutoff_value <- function(boundary) {
    index <- which(cumulative >= boundary)[1L]
    if (is.na(index)) dist[["annualized_pct"]][nrow(dist)] else {
      dist[["annualized_pct"]][index]
    }
  }
  trim16_label <- .plots_trim_label("tm16")
  cutoffs <- data.table::data.table(
    boundary = c(
      sprintf("%s low", trim16_label),
      "Dallas low",
      "Dallas high",
      sprintf("%s high", trim16_label)
    ),
    position = 100 * c(
      trim_bounds$low_16, trim_bounds$low_dallas,
      trim_bounds$high_dallas, trim_bounds$high_16
    ),
    value = c(
      cutoff_value(trim_bounds$low_16),
      cutoff_value(trim_bounds$low_dallas),
      cutoff_value(trim_bounds$high_dallas),
      cutoff_value(trim_bounds$high_16)
    )
  )

  list(
    data = dist[],
    date = dist_date,
    n_components = n_components,
    cutoffs = cutoffs
  )
}

.plots_latest_common_month <- function(measures, benchmarks) {
  required_series <- c("dallas", "core", "headline")
  benchmark_complete <- benchmarks[
    benchmarks[["series"]] %in% required_series &
      !is.na(benchmarks[["value"]]),
    .(n_series = data.table::uniqueN(series)),
    by = month
  ][n_series == length(required_series), month]

  measure_complete <- measures[
    measures[["variant"]] %in% .plots_required_variants &
      !is.na(measures[["yoy"]]),
    .(n_variants = data.table::uniqueN(variant)),
    by = month
  ][n_variants == length(.plots_required_variants), month]

  common <- intersect(benchmark_complete, measure_complete)
  common <- common[common >= .plots_start_month]
  if (length(common) == 0L) {
    .plots_stop(
      "Benchmarks and measures have no complete common month on or after 2018-01-01."
    )
  }
  max(common)
}

.plots_spec <- function(series_key, label, source, colour,
                        linetype = "solid") {
  n <- length(series_key)
  if (n == 0L ||
      length(label) != n ||
      length(source) != n ||
      length(colour) != n ||
      length(linetype) != n) {
    .plots_stop("Chart series metadata must have matching lengths.")
  }
  if (anyDuplicated(label) > 0L || anyDuplicated(series_key) > 0L) {
    .plots_stop("Chart series keys and labels must be unique.")
  }
  if (any(!source %in% c("measure", "benchmark"))) {
    .plots_stop("Chart series `source` must be `measure` or `benchmark`.")
  }
  data.table::data.table(
    series_key = series_key,
    label = label,
    source = source,
    colour = colour,
    linetype = linetype
  )
}

.plots_time_series_data <- function(spec, measures, benchmarks,
                                    start_month, latest_month) {
  parts <- vector("list", nrow(spec))

  for (i in seq_len(nrow(spec))) {
    series_key <- spec[["series_key"]][i]
    if (identical(spec[["source"]][i], "measure")) {
      part <- measures[
        measures[["variant"]] == series_key &
          measures[["month"]] >= start_month &
          measures[["month"]] <= latest_month,
        c("date", "yoy")
      ]
      data.table::setnames(part, "yoy", "value")
    } else {
      part <- benchmarks[
        benchmarks[["series"]] == series_key &
          benchmarks[["month"]] >= start_month &
          benchmarks[["month"]] <= latest_month,
        c("date", "value")
      ]
    }

    part <- part[!is.na(part[["value"]])]
    if (nrow(part) == 0L) {
      .plots_stop(sprintf(
        "No observations are available for chart series `%s`.",
        spec[["label"]][i]
      ))
    }
    part[, series := spec[["label"]][i]]
    parts[[i]] <- part
  }

  out <- data.table::rbindlist(parts, use.names = TRUE)
  out[, series := factor(series, levels = spec[["label"]])]
  out[]
}

.plots_theme <- function(base_size = 10.5) {
  ggplot2::theme_minimal(base_size = base_size, base_family = "sans") +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(
        fill = .plots_colors[["background"]], colour = NA
      ),
      panel.background = ggplot2::element_rect(
        fill = .plots_colors[["background"]], colour = NA
      ),
      panel.grid.major = ggplot2::element_line(
        colour = .plots_colors[["grid"]], linewidth = 0.25
      ),
      panel.grid.minor = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(
        colour = .plots_colors[["axis_line"]], linewidth = 0.35
      ),
      axis.ticks = ggplot2::element_line(
        colour = .plots_colors[["axis_line"]], linewidth = 0.3
      ),
      axis.ticks.length = grid::unit(2.5, "pt"),
      axis.text = ggplot2::element_text(
        colour = .plots_colors[["axis_text"]], size = ggplot2::rel(0.9)
      ),
      axis.text.x = ggplot2::element_text(
        margin = ggplot2::margin(t = 3)
      ),
      axis.text.y = ggplot2::element_text(
        margin = ggplot2::margin(r = 3)
      ),
      axis.title = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(
        face = "bold",
        size = ggplot2::rel(2.0),
        colour = "#000000",
        hjust = 0,
        lineheight = 0.92,
        margin = ggplot2::margin(b = 6)
      ),
      plot.subtitle = ggplot2::element_text(
        size = ggplot2::rel(1.12),
        colour = "#000000",
        hjust = 0,
        lineheight = 1.0,
        margin = ggplot2::margin(b = 5)
      ),
      plot.caption = ggplot2::element_text(
        face = "italic",
        size = ggplot2::rel(0.68),
        colour = .plots_colors[["caption"]],
        hjust = 0,
        margin = ggplot2::margin(t = 7)
      ),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.margin = ggplot2::margin(8, 24, 8, 28),
      legend.position = "top",
      legend.justification = "left",
      legend.direction = "horizontal",
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(
        size = ggplot2::rel(0.94), colour = "#000000"
      ),
      legend.key.width = grid::unit(2.1, "lines"),
      legend.key.height = grid::unit(0.9, "lines"),
      legend.spacing.x = grid::unit(0.9, "lines"),
      legend.box.spacing = grid::unit(2, "pt"),
      legend.box.margin = ggplot2::margin(0, 0, 5, 0),
      legend.background = ggplot2::element_blank(),
      legend.key = ggplot2::element_blank()
    )
}

.plots_time_series_plot <- function(data, spec, start_date, end_date,
                                    title, subtitle) {
  values <- data[["value"]]
  ymax <- max(1, ceiling(max(values, na.rm = TRUE) + 0.25))
  ymin <- min(0, floor(min(values, na.rm = TRUE)))
  if (ymax <= ymin) {
    ymax <- ymin + 1
  }
  breaks <- seq.int(ymin, ymax, by = 1L)
  labels <- sprintf("%.1f%%", breaks)
  colours <- stats::setNames(spec[["colour"]], spec[["label"]])
  linetypes <- stats::setNames(spec[["linetype"]], spec[["label"]])

  ggplot2::ggplot(
    data,
    ggplot2::aes(x = date, y = value, colour = series, linetype = series)
  ) +
    ggplot2::geom_line(
      linewidth = 0.65, lineend = "round", na.rm = TRUE
    ) +
    ggplot2::scale_colour_manual(
      values = colours,
      breaks = spec[["label"]],
      name = NULL,
      guide = ggplot2::guide_legend(
        override.aes = list(linewidth = 0.8)
      )
    ) +
    ggplot2::scale_linetype_manual(
      values = linetypes,
      breaks = spec[["label"]],
      name = NULL
    ) +
    ggplot2::scale_x_date(
      limits = c(start_date, end_date),
      breaks = .plots_date_breaks(start_date, end_date),
      labels = .plots_date_labels,
      expand = ggplot2::expansion(mult = c(0.005, 0.005))
    ) +
    ggplot2::scale_y_continuous(
      limits = c(ymin, ymax),
      breaks = breaks,
      labels = labels,
      expand = ggplot2::expansion(mult = c(0, 0.02))
    ) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      caption = .plots_source_caption,
      x = NULL,
      y = NULL
    ) +
    .plots_theme()
}

.plots_distribution_plot <- function(prepared) {
  dist <- data.table::copy(prepared[["data"]])
  cutoffs <- prepared[["cutoffs"]]
  n_components <- prepared[["n_components"]]

  status_levels <- .plots_status_levels()
  dist[, status := factor(status, levels = status_levels)]
  # True annualized values remain in the data and CSV; only the drawn bar
  # height is capped so off-scale components do not spill outside the panel.
  dist[, bar_ymax := pmax(
    pmin(annualized_pct, .plots_y_limit),
    -.plots_y_limit
  )]
  status_colours <- stats::setNames(
    c(
      .plots_colors[["dallas"]],
      .plots_colors[["ea16"]],
      .plots_colors[["grey"]]
    ),
    status_levels
  )

  trim16_label <- .plots_trim_label("tm16")
  label_rows <- data.table::data.table(
    x = cutoffs[["position"]],
    y = data.table::fifelse(
      cutoffs[["boundary"]] %in% c(
        sprintf("%s low", trim16_label),
        sprintf("%s high", trim16_label)
      ),
      35.4, 31.9
    ),
    label = sprintf("%s: %+.1f%%", cutoffs[["boundary"]], cutoffs[["value"]])
  )

  off_scale <- dist[["annualized_pct"]] < -.plots_y_limit |
    dist[["annualized_pct"]] > .plots_y_limit
  n_off_scale <- sum(off_scale)
  off_trimmed <- if (n_off_scale > 0L) {
    all(
      as.character(dist[["status"]][off_scale]) == status_levels[[3L]]
    )
  } else {
    FALSE
  }

  component_word <- if (n_components == 1L) "component" else "components"
  plot_title <- sprintf(
    "What Gets Trimmed In vs Out, %s",
    .plots_month_name(prepared[["date"]])
  )
  plot_subtitle <- sprintf(
    "%s PCE %s, sorted ascending by inflation.\nX-axis is cumulative expenditure weight; bars size and position by weight.",
    format(n_components, big.mark = ","),
    component_word
  )

  plot <- ggplot2::ggplot(dist) +
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = 100 * cum_before,
        xmax = cumulative_weight_pct,
        ymin = 0,
        ymax = bar_ymax,
        fill = status
      ),
      colour = NA
    ) +
    ggplot2::geom_vline(
      xintercept = cutoffs[["position"]],
      linetype = "dashed",
      colour = .plots_colors[["grey"]],
      linewidth = 0.35
    ) +
    ggplot2::geom_hline(
      yintercept = 0,
      colour = "#333333",
      linewidth = 0.35
    ) +
    ggplot2::geom_text(
      data = label_rows,
      ggplot2::aes(x = x, y = y, label = label),
      inherit.aes = FALSE,
      size = 3.0,
      colour = "#3E3D3D",
      hjust = 0.5,
      vjust = 0
    ) +
    ggplot2::scale_x_continuous(
      limits = c(0, 100),
      breaks = seq(0, 100, by = 20),
      expand = c(0, 0)
    ) +
    ggplot2::scale_y_continuous(
      breaks = seq(-.plots_y_limit, .plots_y_limit, by = 10),
      labels = function(x) sprintf("%d", x),
      expand = c(0, 0)
    ) +
    ggplot2::scale_fill_manual(
      values = status_colours,
      limits = status_levels,
      breaks = status_levels,
      drop = FALSE,
      name = NULL,
      guide = ggplot2::guide_legend(
        override.aes = list(colour = "#333333", linewidth = 0.3)
      )
    ) +
    ggplot2::labs(
      title = plot_title,
      subtitle = plot_subtitle,
      x = "Cumulative expenditure weight (%), bars sorted by ascending inflation, sized by weight",
      y = "Monthly inflation, annualized (%)",
      caption = .plots_source_caption
    ) +
    ggplot2::coord_cartesian(
      ylim = c(-.plots_y_limit, .plots_y_limit),
      clip = "off"
    ) +
    .plots_theme() +
    ggplot2::theme(
      axis.title = ggplot2::element_text(
        size = ggplot2::rel(0.9),
        colour = .plots_colors[["axis_text"]]
      ),
      axis.title.x = ggplot2::element_text(
        margin = ggplot2::margin(t = 6)
      ),
      axis.title.y = ggplot2::element_text(
        margin = ggplot2::margin(r = 6)
      ),
      legend.box.margin = ggplot2::margin(0, 0, 32, 0),
      legend.key.width = grid::unit(1.2, "lines"),
      legend.key.height = grid::unit(0.8, "lines"),
      legend.text = ggplot2::element_text(size = ggplot2::rel(0.9)),
      plot.subtitle = ggplot2::element_text(
        size = ggplot2::rel(1.0),
        colour = "#000000",
        hjust = 0,
        lineheight = 1.0,
        margin = ggplot2::margin(b = 2)
      )
    )

  if (n_off_scale > 0L) {
    note <- sprintf(
      "%d %s off-scale%s",
      n_off_scale,
      if (n_off_scale == 1L) "component" else "components",
      if (off_trimmed) "\n(always trimmed at the tails)" else ""
    )
    plot <- plot + ggplot2::geom_text(
      data = data.table::data.table(
        x = 99.3,
        y = -(.plots_y_limit - 1),
        label = note
      ),
      ggplot2::aes(x = x, y = y, label = label),
      inherit.aes = FALSE,
      size = 2.7,
      colour = .plots_colors[["note"]],
      fontface = "italic",
      hjust = 1,
      vjust = 0,
      lineheight = 0.9
    )
  }

  plot
}

#' Create the seven Employ America trimmed-mean figures
#'
#' @param measures Long output from `compute_all_variants(long = TRUE)`.
#' @param benchmarks Long benchmark data with `series`, `date`, and `value`.
#' @param weighted_panel Latest-month output from `compute_fisher_weights()`,
#'   or a full dated panel.
#' @param output_dir Directory for the PNG files.
#'
#' @return A named character vector of PNG paths, in chart order.
create_all_figures <- function(measures, benchmarks, weighted_panel,
                               output_dir = "output/figures") {
  output_dir <- .plots_output_dir(output_dir)
  measures_dt <- .plots_prepare_measures(
    measures,
    required_variants = .plots_required_variants
  )
  benchmark_data <- .plots_prepare_benchmarks(benchmarks)

  latest_month <- .plots_latest_common_month(measures_dt, benchmark_data)
  start_date <- .plots_month_date(.plots_start_month)
  latest_date <- .plots_month_date(latest_month)

  distribution <- .plots_prepare_distribution(
    weighted_panel,
    fallback_date = latest_date
  )
  n_components <- distribution[["n_components"]]
  component_word <- if (n_components == 1L) "Component" else "Components"
  trim16_label <- .plots_trim_label("tm16")
  trim48_label <- .plots_trim_label("tm48")
  ea16_label <- sprintf(
    "Employ America %s Trimmed Mean (%s %s)",
    trim16_label,
    format(n_components, big.mark = ","),
    component_word
  )

  spec_benchmarks <- .plots_spec(
    series_key = c("dallas", "core", "headline"),
    label = c("Dallas Fed TM", "Core PCE", "Headline PCE"),
    source = rep("benchmark", 3L),
    colour = c(
      .plots_colors[["dallas"]],
      .plots_colors[["core"]],
      .plots_colors[["headline"]]
    ),
    linetype = rep("solid", 3L)
  )
  spec_dallas_16 <- .plots_spec(
    series_key = c("dallas", "tm16", "core"),
    label = c("Dallas Fed TM", ea16_label, "Core PCE"),
    source = c("benchmark", "measure", "benchmark"),
    colour = c(
      .plots_colors[["dallas"]],
      .plots_colors[["green"]],
      .plots_colors[["grey"]]
    ),
    linetype = c("solid", "solid", "dashed")
  )
  spec_trim_ladder <- .plots_spec(
    series_key = c("tm16", "tm48", "median", "core"),
    label = c(
      sprintf("%s TM", trim16_label),
      sprintf("%s TM", trim48_label),
      "Median PCE", "Core PCE"
    ),
    source = c("measure", "measure", "measure", "benchmark"),
    colour = c(
      .plots_colors[["navy"]],
      .plots_colors[["green"]],
      .plots_colors[["ea16"]],
      .plots_colors[["grey"]]
    ),
    linetype = c("solid", "solid", "solid", "dashed")
  )
  spec_housing <- .plots_spec(
    series_key = c("tm16", "tm16_ex_housing", "core"),
    label = c(
      sprintf("%s TM", trim16_label),
      sprintf("%s TM, Ex-Housing", trim16_label),
      "Core PCE"
    ),
    source = c("measure", "measure", "benchmark"),
    colour = c(
      .plots_colors[["ea16"]],
      .plots_colors[["green"]],
      .plots_colors[["grey"]]
    ),
    linetype = c("solid", "solid", "dashed")
  )
  spec_market <- .plots_spec(
    series_key = c("tm16", "tm16_market", "core"),
    label = c(
      sprintf("%s TM", trim16_label),
      sprintf("%s TM, Market-Based", trim16_label),
      "Core PCE"
    ),
    source = c("measure", "measure", "benchmark"),
    colour = c(
      .plots_colors[["ea16"]],
      .plots_colors[["green"]],
      .plots_colors[["grey"]]
    ),
    linetype = c("solid", "solid", "dashed")
  )
  spec_market_ex_housing <- .plots_spec(
    series_key = c(
      "tm16", "tm16_ex_housing", "tm16_market_ex_housing", "core"
    ),
    label = c(
      sprintf("%s TM", trim16_label),
      sprintf("%s TM, Ex-Housing", trim16_label),
      sprintf("%s TM, Market-Based Ex-Housing", trim16_label),
      "Core PCE"
    ),
    source = c("measure", "measure", "measure", "benchmark"),
    colour = c(
      .plots_colors[["ea16"]],
      .plots_colors[["navy"]],
      .plots_colors[["green"]],
      .plots_colors[["grey"]]
    ),
    linetype = c("solid", "solid", "solid", "dashed")
  )

  chart_specs <- list(
    "01_benchmarks_dallas" = list(
      spec = spec_benchmarks,
      title = "How Does the Dallas Fed Trimmed Mean Compare\nto Headline and Core PCE",
      subtitle = "Monthly PCE Inflation, 12-Month % Change"
    ),
    "02_dallas_16_core" = list(
      spec = spec_dallas_16,
      title = sprintf(
        "Does a Symmetric %s Trim Tell\na Different Story Than Dallas?",
        trim16_label
      ),
      subtitle = sprintf(
        "Dallas Fed vs %s Symmetric vs Core PCE, 12-Month %% Change",
        trim16_label
      )
    ),
    "03_trim_ladder" = list(
      spec = spec_trim_ladder,
      title = "Does Wider Trimming\nMove the Signal?",
      subtitle = sprintf(
        "%s and %s Trimmed Means, the Median, and Core PCE, 12-Month %% Change",
        trim16_label, trim48_label
      )
    ),
    "05_housing" = list(
      spec = spec_housing,
      title = "How Much Does Housing Matter\nfor the Trimmed Mean?",
      subtitle = sprintf(
        "%s Trim With vs Without Shelter Lines vs Core PCE, 12-Month %% Change",
        trim16_label
      )
    ),
    "06_market_based" = list(
      spec = spec_market,
      title = "How Much Do Imputed and\nNon-Market Items Matter?",
      subtitle = sprintf(
        "%s Trim With vs Without Market-Based Exclusions vs Core PCE, 12-Month %% Change",
        trim16_label
      )
    ),
    "07_market_ex_housing" = list(
      spec = spec_market_ex_housing,
      title = "How Much Do Housing and Market-Based\nExclusions Matter Together?",
      subtitle = sprintf(
        "%s Trim With Both Sets of Exclusions vs Core PCE, 12-Month %% Change",
        trim16_label
      )
    )
  )

  plots <- vector("list", 7L)
  names(plots) <- c(
    "01_benchmarks_dallas",
    "02_dallas_16_core",
    "03_trim_ladder",
    "04_latest_distribution",
    "05_housing",
    "06_market_based",
    "07_market_ex_housing"
  )

  for (stem in names(chart_specs)) {
    chart <- chart_specs[[stem]]
    chart_data <- .plots_time_series_data(
      chart[["spec"]],
      measures_dt,
      benchmark_data,
      .plots_start_month,
      latest_month
    )
    plots[[stem]] <- .plots_time_series_plot(
      chart_data,
      chart[["spec"]],
      start_date,
      latest_date,
      chart[["title"]],
      chart[["subtitle"]]
    )
  }
  plots[["04_latest_distribution"]] <- .plots_distribution_plot(distribution)
  plots <- plots[names(plots)]

  files <- paste0(names(plots), ".png")
  paths <- file.path(output_dir, files)
  names(paths) <- names(plots)

  for (i in seq_along(paths)) {
    ggplot2::ggsave(
      filename = paths[[i]],
      plot = plots[[i]],
      width = 9,
      height = 5,
      units = "in",
      dpi = 200,
      bg = .plots_colors[["background"]],
      device = "png"
    )
  }

  paths
}

#' Write the chart inputs as CSV files
#'
#' @param measures Long output from `compute_all_variants(long = TRUE)`.
#' @param weighted_panel Latest-month output from `compute_fisher_weights()`,
#'   or a full dated panel.
#' @param output_dir Directory for the CSV files.
#'
#' @return A named character vector with the measures and distribution paths.
write_figure_data <- function(measures, weighted_panel,
                              output_dir = "output/figures") {
  output_dir <- .plots_output_dir(output_dir)
  measures_dt <- .plots_prepare_measures(measures)
  usable <- measures_dt[!is.na(measures_dt[["yoy"]])]
  fallback_date <- if (nrow(usable) > 0L) {
    max(usable[["date"]])
  } else {
    max(measures_dt[["date"]])
  }
  distribution <- .plots_prepare_distribution(
    weighted_panel,
    fallback_date = fallback_date
  )

  measures_out <- measures_dt[, .(variant, date, monthly, index, yoy)]
  distribution_out <- distribution[["data"]][, .(
    date = date,
    component_id = component_id,
    description = description,
    pi = pi,
    annualized_pct = annualized_pct,
    fisher_weight = fisher_weight,
    weight_share = weight_share,
    cumulative_weight_start_pct = 100 * cum_before,
    cumulative_weight_pct = cumulative_weight_pct,
    kept_16 = kept_16,
    kept_dallas = kept_dallas,
    status = as.character(status)
  )]

  measures_path <- file.path(output_dir, "measures_yoy.csv")
  distribution_path <- file.path(output_dir, "latest_distribution.csv")
  data.table::fwrite(measures_out, measures_path, na = "NA")
  data.table::fwrite(distribution_out, distribution_path, na = "NA")

  c(measures = measures_path, distribution = distribution_path)
}
