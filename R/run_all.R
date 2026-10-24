root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
if (!file.exists(file.path(root, "R", "bea_data.R"))) {
  stop("Run from the project root: Rscript R/run_all.R", call. = FALSE)
}

source(file.path(root, "R", "bea_data.R"))
source(file.path(root, "R", "components.R"))
source(file.path(root, "R", "trimmed_mean.R"))
source(file.path(root, "R", "plots.R"))

refresh <- "--refresh" %in% commandArgs(trailingOnly = TRUE)
inputs <- fetch_bea_inputs(start_year = 2016L, refresh = refresh)
dallas <- fetch_dallas_benchmark()
write_data_manifest(inputs$price, inputs$nominal, dallas)

panel <- prepare_component_panel(inputs$price, inputs$nominal, strict = TRUE)
measures <- compute_all_variants(panel, long = TRUE)
weighted_panel <- compute_fisher_weights(panel)

benchmark_prices <- inputs$price[line_number %in% c(1L, 374L)]
benchmark_prices <- benchmark_prices[order(line_number, date)]
benchmark_prices[, value := 100 * (value / data.table::shift(value, 12L) - 1), by = line_number]
benchmark_prices[, series := data.table::fifelse(line_number == 1L, "headline", "core")]
benchmarks <- data.table::rbindlist(list(
  benchmark_prices[, .(series, date, value)],
  dallas[, .(series = "dallas", date, value = dallas_yoy)]
))

output_data <- file.path(root, "output", "data")
output_figures <- file.path(root, "output", "figures")
dir.create(output_data, recursive = TRUE, showWarnings = FALSE)
dir.create(output_figures, recursive = TRUE, showWarnings = FALSE)

crosswalk <- unique(panel[, .(
  component_id, line_number, series_code, description,
  housing, market_based, source_group
)])
data.table::fwrite(crosswalk, file.path(output_data, "component_crosswalk.csv"))
data.table::fwrite(panel, file.path(output_data, "component_panel.csv"))
write_figure_data(measures, weighted_panel, output_data)
figure_paths <- create_all_figures(
  measures, benchmarks, weighted_panel,
  output_dir = output_figures
)

official_dallas <- dallas[, .(date, official = dallas_yoy)]
replication <- measures[variant == "dallas_replication", .(date, reconstructed = yoy)]
diagnostics <- merge(official_dallas, replication, by = "date")
diagnostics[, difference_pp := reconstructed - official]
data.table::fwrite(diagnostics, file.path(output_data, "dallas_diagnostic.csv"))

latest <- min(
  max(benchmarks[!is.na(value), date]),
  max(measures[!is.na(yoy), date])
)
latest_panel <- panel[date == latest]
market_share <- latest_panel[, sum(nominal[market_based]) / sum(nominal)]
summary <- data.table::data.table(
  latest_month = latest,
  components = data.table::uniqueN(panel$component_id),
  market_based_share = market_share,
  figures = length(figure_paths)
)
data.table::fwrite(summary, file.path(output_data, "run_summary.csv"))

stopifnot(
  length(figure_paths) == 7L,
  all(file.exists(figure_paths)),
  all(file.info(figure_paths)$size > 0),
  latest == as.Date("2026-08-01") || latest > as.Date("2026-08-01")
)
cat(sprintf(
  "Created %d figures through %s using %d components.\n",
  length(figure_paths), format(latest, "%Y-%m"), summary$components
))
