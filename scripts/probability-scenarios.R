library(hector.rcmip)
library(tidyverse)
library(hector)
library(future, exclude = "run")
library(furrr)
library(data.table, exclude = c("between", "first", "last", "transpose"))
library(fs)
library(here)

plan(future.callr::callr)

outdir <- dir_create(here("output"))

scenarios <- c(
  "piControl", "esm-piControl", "1pctCO2", "1pctCO2-4xext",
  "abrupt-4xCO2", "abrupt-2xCO2", "abrupt-0p5xCO2", "historical",
  "ssp119", "ssp585"
  ## "esm-hist", "esm-ssp119", "esm-ssp585"
)

all_results <- future_map(
  scenarios,
  possibly(run_probability, NULL, quiet = FALSE),
  n = 1000,
  .progress = TRUE
)

results_dt <- all_results %>%
  map("results") %>%
  do.call(c, .) %>%
  map(setDT) %>%
  rbindlist()

results_summary <- results_dt %>%
  .[, .(Mean = mean(value),
        SD = sd(value),
        q025 = quantile(value, 0.025),
        q05 = quantile(value, 0.05),
        q10 = quantile(value, 0.1),
        q25 = quantile(value, 0.25),
        q50 = quantile(value, 0.50),
        q75 = quantile(value, 0.75),
        q90 = quantile(value, 0.9),
        q95 = quantile(value, 0.95),
        q975 = quantile(value, 0.975)),
    .(scenario, year, variable)]

write_csv(results_summary, path(outdir, "probability-results.csv"))

results_long <- results_summary %>%
  melt(
    id.vars = c("scenario", "year", "variable"),
    variable.name = "stat"
  )

write_csv(results_long, path(outdir, "probability-results-long.csv"))
