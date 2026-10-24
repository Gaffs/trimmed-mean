# Dallas Fed-style trimmed mean methodology.
#
# Panel conventions:
#   date          Date, one observation per component per calendar month
#   component_id  stable component identifier
#   price         strictly positive price / price index
#   nominal       non-negative nominal expenditure
#   housing       optional logical flag, TRUE for housing components
#   market_based  optional logical flag, TRUE for market-based components
#
# `pi` is a simple monthly inflation rate as a decimal (0.01 == 1%).
# `fisher_weight` is the normalized Fisher weight
#
#   Q_it = N_it / P_it
#   w_it = 0.5 * (Q_it * P_i,t-1) / sum_j(Q_jt * P_j,t-1)
#        + 0.5 * N_i,t-1 / sum_j N_j,t-1
#
# over components with both a current and an immediately preceding calendar
# month. The monthly and year-over-year values returned by compute_variant()
# are in percent; `annualized` is 100 * ((1 + monthly / 100)^12 - 1), also in
# percent, and `index` is a chained level with base 100.
#
# The median follows the official mytrim 50/50 limiting rule: after sorting
# monthly pi ascending, it is the first component whose cumulative Fisher
# weight reaches 0.50. There is no averaging across the 50/50 boundary, and
# zero-weight components are kept in the cumulative crossing.
#
# compute_variant() emits a complete monthly grid. Months present in the panel
# keep their observed date; months missing inside the panel span use month-end
# when every observed date is month-end, otherwise the first day of the month.

.assert_scalar_logical <- function(x, name) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop(sprintf("`%s` must be a single non-missing logical (TRUE or FALSE).", name),
         call. = FALSE)
  }
  invisible(TRUE)
}

.check_trim_fraction <- function(x, name) {
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x)) {
    stop(sprintf("`%s` must be a single finite number.", name), call. = FALSE)
  }
  if (x < 0) {
    stop(sprintf("`%s` must be non-negative.", name), call. = FALSE)
  }
  invisible(TRUE)
}

.validate_trim <- function(lower, upper, allow_median = FALSE) {
  .check_trim_fraction(lower, "lower")
  .check_trim_fraction(upper, "upper")

  if (allow_median && lower == 0.5 && upper == 0.5) {
    return(list(lower = lower, upper = upper, median = TRUE))
  }
  if (lower + upper >= 1) {
    stop("`lower + upper` must be less than 1 for a trimmed mean.",
         call. = FALSE)
  }
  list(lower = lower, upper = upper, median = FALSE)
}

.year_month <- function(x) {
  as.integer(data.table::year(x)) * 12L + as.integer(data.table::month(x))
}

.annualize_decimal <- function(x) {
  100 * ((1 + x)^12 - 1)
}

.complete_month_grid <- function(dates) {
  dates <- as.Date(dates)
  month_id <- .year_month(dates)
  month_range <- seq.int(min(month_id), max(month_id))
  years <- (month_range - 1L) %/% 12L
  months <- (month_range - 1L) %% 12L + 1L
  grid_date <- as.Date(sprintf("%04d-%02d-01", years, months))

  if (all(as.integer(format(dates + 1, "%d")) == 1L)) {
    next_id <- month_range + 1L
    next_years <- (next_id - 1L) %/% 12L
    next_months <- (next_id - 1L) %% 12L + 1L
    grid_date <- as.Date(sprintf(
      "%04d-%02d-01",
      next_years,
      next_months
    )) - 1
  }

  grid <- data.table::data.table(month_id = month_range, date = grid_date)
  observed <- unique(data.table::data.table(month_id = month_id, date = dates))
  grid[observed, on = "month_id", date := i.date]
  grid
}

.assert_panel <- function(panel) {
  if (!data.table::is.data.table(panel)) {
    stop("`panel` must be a data.table; use data.table::as.data.table(panel) first.",
         call. = FALSE)
  }
  if (anyDuplicated(names(panel)) > 0L) {
    stop("`panel` must have unique column names.", call. = FALSE)
  }

  required <- c("date", "component_id", "price", "nominal")
  missing <- setdiff(required, names(panel))
  if (length(missing) > 0L) {
    stop(sprintf("`panel` is missing required column(s): %s.",
                 paste(sprintf("`%s`", missing), collapse = ", ")),
         call. = FALSE)
  }
  if (nrow(panel) == 0L) {
    stop("`panel` must have at least one row.", call. = FALSE)
  }
  if (!inherits(panel[["date"]], "Date")) {
    stop("`panel$date` must be a Date column.", call. = FALSE)
  }
  if (anyNA(panel[["date"]])) {
    stop("`panel$date` must not contain missing values.", call. = FALSE)
  }
  if (anyNA(panel[["component_id"]])) {
    stop("`panel$component_id` must not contain missing values.", call. = FALSE)
  }
  if (!is.atomic(panel[["component_id"]])) {
    stop("`panel$component_id` must be an atomic vector.", call. = FALSE)
  }
  if (!is.numeric(panel[["price"]]) || !is.numeric(panel[["nominal"]])) {
    stop("`panel$price` and `panel$nominal` must be numeric.", call. = FALSE)
  }
  if (any(!is.finite(panel[["price"]])) || any(!is.finite(panel[["nominal"]]))) {
    stop("`panel$price` and `panel$nominal` must contain only finite values.",
         call. = FALSE)
  }
  if (any(panel[["price"]] <= 0)) {
    stop("`panel$price` must be strictly positive.", call. = FALSE)
  }
  if (any(panel[["nominal"]] < 0)) {
    stop("`panel$nominal` must be non-negative.", call. = FALSE)
  }

  for (flag in c("housing", "market_based")) {
    if (flag %in% names(panel) && !is.logical(panel[[flag]])) {
      stop(sprintf("`panel$%s` must be a logical column when present.", flag),
           call. = FALSE)
    }
  }

  key <- data.table::data.table(date = panel[["date"]],
                                component_id = panel[["component_id"]])
  if (anyDuplicated(key) > 0L) {
    stop("`panel` must have at most one row per date and component_id.",
         call. = FALSE)
  }

  month_id <- .year_month(panel[["date"]])
  component_month <- data.table::data.table(
    component_id = panel[["component_id"]],
    month_id = month_id
  )
  if (anyDuplicated(component_month) > 0L) {
    stop("`panel` must have at most one row per component per calendar month.",
         call. = FALSE)
  }

  month_dates <- unique(data.table::data.table(
    month_id = month_id,
    date = panel[["date"]]
  ))
  if (anyDuplicated(month_dates[["month_id"]]) > 0L) {
    stop("All rows in a calendar month must share the same `date`.",
         call. = FALSE)
  }

  invisible(TRUE)
}

.require_flag <- function(panel, column, context) {
  if (!column %in% names(panel)) {
    stop(sprintf("%s requires a `%s` column in `panel`; join the component classification first.",
                 context, column),
         call. = FALSE)
  }
  if (!is.logical(panel[[column]])) {
    stop(sprintf("`panel$%s` must be a logical column.", column), call. = FALSE)
  }
  if (anyNA(panel[[column]])) {
    stop(sprintf("`panel$%s` contains missing values and cannot be used for classification.",
                 column),
         call. = FALSE)
  }
  invisible(TRUE)
}

.filter_panel <- function(panel, exclude_housing, market_only) {
  .assert_scalar_logical(exclude_housing, "exclude_housing")
  .assert_scalar_logical(market_only, "market_only")

  out <- panel
  if (exclude_housing) {
    .require_flag(out, "housing", "`exclude_housing = TRUE`")
    out <- out[!out[["housing"]]]
  }
  if (market_only) {
    .require_flag(out, "market_based", "`market_only = TRUE`")
    out <- out[out[["market_based"]]]
  }

  if (nrow(out) == 0L) {
    active <- c(
      if (exclude_housing) "exclude_housing = TRUE",
      if (market_only) "market_only = TRUE"
    )
    stop(sprintf("No observations remain after applying %s.",
                 paste(active, collapse = " and ")),
         call. = FALSE)
  }
  out
}

.validate_xw <- function(x, w) {
  if (!is.numeric(x)) {
    stop("`x` must be numeric.", call. = FALSE)
  }
  if (!is.numeric(w)) {
    stop("`w` must be numeric.", call. = FALSE)
  }
  if (length(x) != length(w)) {
    stop("`x` and `w` must have the same length.", call. = FALSE)
  }
  if (length(x) == 0L) {
    stop("`x` and `w` must not be empty.", call. = FALSE)
  }
  if (any(!is.finite(x))) {
    stop("`x` must contain only finite values (no NA/NaN/Inf).", call. = FALSE)
  }
  if (any(!is.finite(w))) {
    stop("`w` must contain only finite values (no NA/NaN/Inf).", call. = FALSE)
  }
  if (any(w < 0)) {
    stop("`w` must be non-negative.", call. = FALSE)
  }

  total <- sum(w)
  if (!is.finite(total) || total <= 0) {
    stop("`w` must have a finite, positive sum.", call. = FALSE)
  }
  w / total
}

#' Weighted trimmed mean with exact fractional-boundary weights
#'
#' @param x Numeric values.
#' @param w Non-negative weights; normalized internally.
#' @param lower,upper Fractions of total weight trimmed from each tail. With
#'   `lower = upper = 0.5`, returns the official mytrim 50/50 median (the first
#'   value whose cumulative weight reaches 0.50, without adjacent averaging).
#'
#' @return A numeric scalar.
trimmed_mean <- function(x, w, lower, upper) {
  trim <- .validate_trim(lower, upper, allow_median = TRUE)
  if (trim$median) {
    return(.weighted_median(x, w))
  }

  w <- .validate_xw(x, w)
  lower <- trim$lower
  upper <- trim$upper

  ord <- order(x)
  x <- x[ord]
  w <- w[ord]

  cumulative <- cumsum(w)
  previous <- c(0, cumulative[-length(cumulative)])
  retained <- pmin(cumulative, 1 - upper) - pmax(previous, lower)
  retained <- pmax(retained, 0)

  total <- sum(retained)
  if (!is.finite(total) || total <= 0) {
    stop("No weight remains after trimming; check `lower`, `upper`, and `w`.",
         call. = FALSE)
  }
  sum(retained * x) / total
}

.weighted_median <- function(x, w) {
  w <- .validate_xw(x, w)

  ord <- order(x)
  x <- x[ord]
  w <- w[ord]
  cumulative <- cumsum(w)

  # Official mytrim 50/50 limiting definition: the first component whose
  # cumulative weight reaches 0.50. Zero-weight rows stay in the ordering.
  index <- which(cumulative >= 0.5)[1L]
  if (is.na(index)) {
    return(x[length(x)])
  }
  x[index]
}

.distribution_detail <- function(x, w, lower, upper) {
  trim <- .validate_trim(lower, upper, allow_median = TRUE)
  w <- .validate_xw(x, w)
  if (any(x < -1)) {
    stop("`x` must be at least -1 to annualize monthly inflation rates.",
         call. = FALSE)
  }

  ord <- order(x)
  x <- x[ord]
  w <- w[ord]
  n <- length(x)

  cumulative <- cumsum(w)
  before <- c(0, cumulative[-n])
  lower_index <- which(cumulative >= trim$lower)[1L]
  if (is.na(lower_index)) lower_index <- n
  upper_index <- which(cumulative >= 1 - trim$upper)[1L]
  if (is.na(upper_index)) upper_index <- n

  retained <- numeric(n)
  weight_used <- numeric(n)
  category <- rep("retained", n)

  if (lower_index > 1L) {
    category[seq_len(lower_index - 1L)] <- "lower_trimmed"
  }
  if (upper_index < n) {
    category[(upper_index + 1L):n] <- "upper_trimmed"
  }

  if (trim$median) {
    # The official 50/50 median is a limiting rule: the crossing component
    # carries unit effective mass even though no original weight is retained.
    weight_used[lower_index] <- 1
    category[lower_index] <- "median"
  } else {
    retained <- pmax(
      0,
      pmin(cumulative, 1 - trim$upper) - pmax(before, trim$lower)
    )
    weight_used <- retained
    if (lower_index == upper_index) {
      category[lower_index] <- "retained"
    } else {
      category[lower_index] <- "lower_cutoff"
      category[upper_index] <- "upper_cutoff"
    }
  }

  lower_cutoff_pi <- x[lower_index]
  upper_cutoff_pi <- x[upper_index]
  lower_cutoff_annualized <- .annualize_decimal(lower_cutoff_pi)
  upper_cutoff_annualized <- .annualize_decimal(upper_cutoff_pi)

  out <- data.table::data.table(
    rank = seq_len(n),
    input_index = ord,
    pi = x,
    annualized_pi = .annualize_decimal(x),
    weight = w,
    cumulative_weight_before = before,
    cumulative_weight = cumulative,
    retained_weight = retained,
    retained_fraction = data.table::fifelse(
      w > 0,
      retained / w,
      NA_real_
    ),
    weight_used = weight_used,
    category = category,
    lower_cutoff_pi = lower_cutoff_pi,
    upper_cutoff_pi = upper_cutoff_pi,
    lower_cutoff_annualized = lower_cutoff_annualized,
    upper_cutoff_annualized = upper_cutoff_annualized
  )

  total_used <- sum(weight_used)
  estimate <- if (total_used > 0) {
    sum(weight_used * x) / total_used
  } else {
    NA_real_
  }
  attr(out, "trimmed_mean") <- estimate
  attr(out, "median") <- trim$median
  attr(out, "retained_weight_total") <- sum(retained)
  attr(out, "weight_used_total") <- total_used
  attr(out, "lower_cutoff_pi") <- lower_cutoff_pi
  attr(out, "upper_cutoff_pi") <- upper_cutoff_pi
  attr(out, "lower_cutoff_annualized") <- lower_cutoff_annualized
  attr(out, "upper_cutoff_annualized") <- upper_cutoff_annualized
  out
}

#' Detail the sorted distribution behind a trimmed mean
#'
#' @param x Monthly simple inflation rates as decimals, each at least `-1`.
#' @param w Non-negative Fisher weights; normalized internally.
#' @param lower,upper Fractions trimmed from each tail. With both equal to
#'   `0.5`, uses the official mytrim 50/50 median rule.
#'
#' @return A data.table sorted by `pi` with `rank`, `input_index`, annualized
#'   rates, cumulative weights, retained weights, effective weights, boundary
#'   categories, and cutoff values. `pi` and cutoff `pi` values are decimals;
#'   annualized values are percent; weights, cumulative weights, and retained
#'   weights are fractions. The estimate is stored in the `trimmed_mean`
#'   attribute. For the median, `retained_weight` is zero by construction and
#'   `weight_used` places unit mass on the selected component.
trimmed_mean_detail <- function(x, w, lower, upper) {
  .distribution_detail(x, w, lower, upper)
}

#' Compute monthly simple inflation and Fisher weights
#'
#' @param panel A data.table with `date`, `component_id`, `price`, and
#'   `nominal` columns.
#'
#' @return `panel` with normalized `fisher_weight` and decimal `pi` columns.
#'   Components without the immediately preceding calendar month receive `NA`
#'   for both columns; weights are normalized over the remaining components.
compute_fisher_weights <- function(panel) {
  .assert_panel(panel)

  out <- data.table::copy(panel)
  if ("pi" %in% names(out)) {
    data.table::set(out, j = "pi", value = NULL)
  }
  if ("fisher_weight" %in% names(out)) {
    data.table::set(out, j = "fisher_weight", value = NULL)
  }

  work <- data.table::data.table(
    row_id = seq_len(nrow(panel)),
    date = panel[["date"]],
    component_id = panel[["component_id"]],
    price = panel[["price"]],
    nominal = panel[["nominal"]]
  )
  work[, month_id := .year_month(date)]

  previous <- work[, .(
    component_id,
    month_id,
    price_previous = price,
    nominal_previous = nominal
  )]
  data.table::setkey(previous, component_id, month_id)

  work[, month_previous := month_id - 1L]
  work[previous,
       on = .(component_id, month_previous = month_id),
       `:=`(price_previous = i.price_previous,
            nominal_previous = i.nominal_previous)]

  work[, pi := price / price_previous - 1]
  work[, q_prior_price := (nominal / price) * price_previous]
  work[, valid := is.finite(pi) &
        is.finite(q_prior_price) &
        is.finite(nominal_previous) &
        nominal_previous >= 0 &
        q_prior_price >= 0]

  work[, `:=`(
    sum_q_prior_price = sum(q_prior_price[valid]),
    sum_nominal_previous = sum(nominal_previous[valid]),
    n_valid = sum(valid)
  ), by = date]

  bad <- unique(work[
    n_valid > 0L & (
      !is.finite(sum_q_prior_price) |
        !is.finite(sum_nominal_previous) |
        sum_q_prior_price <= 0 |
        sum_nominal_previous <= 0
    ),
    .(date, n_valid, sum_q_prior_price, sum_nominal_previous)
  ])
  if (nrow(bad) > 0L) {
    stop(sprintf(
      "Cannot compute Fisher weights for %s: valid rows have zero or non-finite total Q_t * P_{t-1} or prior nominal expenditure.",
      paste(format(bad[["date"]], "%Y-%m"), collapse = ", ")
    ), call. = FALSE)
  }

  work[, fisher_weight := data.table::fifelse(
    valid,
    0.5 * q_prior_price / sum_q_prior_price +
      0.5 * nominal_previous / sum_nominal_previous,
    NA_real_
  )]
  work[valid == TRUE,
       fisher_weight := fisher_weight / sum(fisher_weight),
       by = date]

  data.table::setorderv(work, "row_id")
  data.table::set(out, j = "pi", value = work[["pi"]])
  data.table::set(out, j = "fisher_weight", value = work[["fisher_weight"]])
  out
}

#' Prepare one month's sorted component distribution
#'
#' @param panel A data.table with `date`, `component_id`, `price`, `nominal`,
#'   and optionally `housing` and `market_based` logical columns.
#' @param date A single `Date` present in the panel.
#' @param lower,upper Fractions trimmed from each tail. With both equal to
#'   `0.5`, uses the official mytrim 50/50 median rule.
#' @param exclude_housing,market_only Apply the same classification filters as
#'   `compute_variant()` before computing Fisher weights.
#'
#' @return A data.table with one sorted row per component with non-missing
#'   `pi` and Fisher weight, and columns `date`, `component_id`, `rank`, `pi`,
#'   `annualized_pi`, `weight`,
#'   `cumulative_weight_before`, `cumulative_weight`, `retained_weight`,
#'   `retained_fraction`, `weight_used`, `category`, the lower and upper
#'   cutoff rates in monthly and annualized form. `pi` is a decimal;
#'   `annualized_pi` and annualized cutoff values are percent; weights,
#'   cumulative weights, and retained weights are fractions. The estimate is
#'   stored in the `trimmed_mean` attribute. `retained_weight` is the fraction
#'   of the original Fisher weight retained; for the median it is zero and
#'   `weight_used` places unit mass on the selected component.
prepare_distribution <- function(panel, date, lower, upper,
                                 exclude_housing = FALSE, market_only = FALSE) {
  .assert_panel(panel)
  if (!inherits(date, "Date") || length(date) != 1L || is.na(date)) {
    stop("`date` must be a single non-missing Date.", call. = FALSE)
  }
  .assert_scalar_logical(exclude_housing, "exclude_housing")
  .assert_scalar_logical(market_only, "market_only")
  .validate_trim(lower, upper, allow_median = TRUE)

  filtered <- .filter_panel(panel, exclude_housing, market_only)
  weighted_panel <- compute_fisher_weights(filtered)
  target_date <- as.Date(date)

  rows <- weighted_panel[
    weighted_panel[["date"]] == target_date &
      !is.na(weighted_panel[["pi"]]) &
      !is.na(weighted_panel[["fisher_weight"]])
  ]
  if (nrow(rows) == 0L) {
    stop(sprintf(
      "No usable component distribution for date %s; the date must be present in the filtered panel and have a preceding month.",
      format(target_date, "%Y-%m-%d")
    ), call. = FALSE)
  }

  detail <- trimmed_mean_detail(
    rows[["pi"]],
    rows[["fisher_weight"]],
    lower,
    upper
  )
  data.table::set(detail, j = "date", value = rep(target_date, nrow(detail)))
  data.table::set(
    detail,
    j = "component_id",
    value = rows[["component_id"]][detail[["input_index"]]]
  )
  data.table::set(detail, j = "input_index", value = NULL)

  data.table::setcolorder(detail, c(
    "date", "component_id", "rank", "pi", "annualized_pi", "weight",
    "cumulative_weight_before", "cumulative_weight", "retained_weight",
    "retained_fraction", "weight_used", "category", "lower_cutoff_pi",
    "upper_cutoff_pi", "lower_cutoff_annualized",
    "upper_cutoff_annualized"
  ))
  detail
}

#' Compute one trimmed-mean variant
#'
#' @param panel A data.table with `date`, `component_id`, `price`, `nominal`,
#'   and optionally `housing` and `market_based` logical columns.
#' @param lower,upper Fractions of total weight trimmed from each tail. With
#'   `lower = upper = 0.5`, uses the official mytrim 50/50 median rule.
#' @param exclude_housing Drop components with `housing == TRUE`.
#' @param market_only Keep only components with `market_based == TRUE`.
#'
#' @return A data.table with `date`, `monthly`, `index`, `yoy`, and
#'   `annualized`. `monthly`, `yoy`, and `annualized` are in percent;
#'   `annualized` is `100 * ((1 + monthly / 100)^12 - 1)`. `index` is a
#'   chained level with the first panel month set to 100. The result is a
#'   complete monthly grid over the panel span: months without a usable rate
#'   have `NA` monthly rates and carry the previous index forward.
compute_variant <- function(panel, lower, upper,
                            exclude_housing = FALSE, market_only = FALSE) {
  .assert_panel(panel)
  .assert_scalar_logical(exclude_housing, "exclude_housing")
  .assert_scalar_logical(market_only, "market_only")
  trim <- .validate_trim(lower, upper, allow_median = TRUE)

  filtered <- .filter_panel(panel, exclude_housing, market_only)
  weighted_panel <- compute_fisher_weights(filtered)

  rates <- weighted_panel[
    !is.na(weighted_panel[["pi"]]) &
      !is.na(weighted_panel[["fisher_weight"]])
  ]
  if (nrow(rates) == 0L) {
    stop("No component-month has both a preceding calendar month and a valid Fisher weight; at least two consecutive monthly observations for the same component are required.",
         call. = FALSE)
  }

  monthly <- rates[, .(
    monthly = 100 * trimmed_mean(pi, fisher_weight, trim$lower, trim$upper)
  ), by = date]
  monthly[, month_id := .year_month(date)]
  monthly[, date := NULL]

  all_dates <- .complete_month_grid(panel[["date"]])
  result <- monthly[all_dates, on = "month_id"]
  data.table::setorderv(result, "month_id")

  monthly <- result[["monthly"]]
  index <- 100 * cumprod(data.table::fifelse(
    is.na(monthly),
    1,
    1 + monthly / 100
  ))
  data.table::set(result, j = "index", value = index)
  data.table::set(
    result,
    j = "annualized",
    value = .annualize_decimal(monthly / 100)
  )

  month_id <- result[["month_id"]]
  previous_year <- match(month_id - 12L, month_id)
  data.table::set(
    result,
    j = "yoy",
    value = 100 * (result[["index"]] / result[["index"]][previous_year] - 1)
  )

  result[, .(date, monthly, index, yoy, annualized)]
}

#' Compute the standard and diagnostic trimmed-mean variants
#'
#' @param panel A data.table containing `housing` and `market_based` logical
#'   columns in addition to the columns required by `compute_variant()`.
#'   Both classification columns are required because the result includes
#'   housing- and market-based variants.
#' @param long Return one long data.table instead of a named list.
#'
#' @return A named list with `tm16`, `tm32`, `tm48`, `tm64`, `median`,
#'   `tm16_ex_housing`, `tm16_market`, `tm16_market_ex_housing`, and the
#'   diagnostic `dallas_replication`, or one long data.table with a `variant`
#'   column when `long = TRUE`. Every variant has the `compute_variant()`
#'   columns, including `annualized`.
#'
#' @details `dallas_replication` applies the official 24% lower / 31% upper
#'   trim to the monthly distribution for diagnostic comparison. It does not
#'   fetch or replace the external official benchmark.
compute_all_variants <- function(panel, long = FALSE) {
  .assert_panel(panel)
  .assert_scalar_logical(long, "long")
  .require_flag(panel, "housing", "`compute_all_variants()`")
  .require_flag(panel, "market_based", "`compute_all_variants()`")

  specs <- list(
    tm16 = list(lower = 0.08, upper = 0.08,
                exclude_housing = FALSE, market_only = FALSE),
    tm32 = list(lower = 0.16, upper = 0.16,
                exclude_housing = FALSE, market_only = FALSE),
    tm48 = list(lower = 0.24, upper = 0.24,
                exclude_housing = FALSE, market_only = FALSE),
    tm64 = list(lower = 0.32, upper = 0.32,
                exclude_housing = FALSE, market_only = FALSE),
    median = list(lower = 0.5, upper = 0.5,
                  exclude_housing = FALSE, market_only = FALSE),
    tm16_ex_housing = list(lower = 0.08, upper = 0.08,
                           exclude_housing = TRUE, market_only = FALSE),
    tm16_market = list(lower = 0.08, upper = 0.08,
                       exclude_housing = FALSE, market_only = TRUE),
    tm16_market_ex_housing = list(lower = 0.08, upper = 0.08,
                                  exclude_housing = TRUE, market_only = TRUE),
    dallas_replication = list(lower = 0.24, upper = 0.31,
                              exclude_housing = FALSE, market_only = FALSE)
  )

  result <- lapply(specs, function(spec) {
    compute_variant(
      panel,
      lower = spec$lower,
      upper = spec$upper,
      exclude_housing = spec$exclude_housing,
      market_only = spec$market_only
    )
  })

  if (!long) {
    return(result)
  }

  long_result <- data.table::rbindlist(result, idcol = "variant", use.names = TRUE)
  data.table::setcolorder(
    long_result,
    c("variant", "date", "monthly", "index", "yoy", "annualized")
  )
  long_result
}
