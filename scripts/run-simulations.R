#!/usr/bin/env Rscript
options(conflicts.policy = "depends.ok")
conflictRules("testthat", exclude = c("matches", "is_null", "equals",
                                      "not", "is_less_than"))

library(drake, exclude = c("gather", "expand"))
library(here)
library(readr)

devtools::load_all(here())

expose_imports("hector.rcmip")

outdir <- dir_create(here("output"))

scenarios <- c(
  "piControl", "esm-piControl", "1pctCO2", "1pctCO2-4xext",
  "abrupt-4xCO2", "abrupt-2xCO2", "abrupt-0p5xCO2",
  "historical", "historical-cmip5",
  "ssp119", "ssp585",
  paste0("rcp", c("26", "45", "60", "85"))
)
models <- c(cmip6_params()[["model"]], "default")

rcmip_infile <- here("inst", "rcmip-inputs.fst")
if (!file.exists(rcmip_infile)) generate_rcmip_inputs()

plan <- drake_plan(
  # Individual runs
  out_files = target(
    run_scenario(scenario, model),
    transform = cross(scenario = !!scenarios, model = !!models)
  ),
  # Probability runs
  probability_params = read_csv(file_in(
    "data-raw/brick-posteriors/emissions_17k_posteriorSamples.csv"
  ), col_types = cols(.default = "d")),
  isamps = sample.int(nrow(probability_params), 1000),
  probability_param_draws = probability_params[isamps, ],
  probability_run = target(
    run_with_param(
      scenario,
      probability_param_draws[["S.temperature"]],
      probability_param_draws[["diff.temperature"]],
      probability_param_draws[["alpha.temperature"]],
      isamp = isamps
    ),
    dynamic = map(probability_param_draws, isamps),
    transform = map(scenario = !!scenarios)
  )
)

### Make plan
options(clustermq.scheduler = "multicore")
make(plan, parallelism = "clustermq", jobs = parallel::detectCores())
