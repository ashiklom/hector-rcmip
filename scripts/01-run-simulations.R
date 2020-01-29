#!/usr/bin/env Rscript
options(conflicts.policy = "depends.ok")
conflictRules("testthat", exclude = c("matches", "is_null", "equals",
                                      "not", "is_less_than"))

library(drake, exclude = c("gather", "expand"))
library(here)
library(readr)
library(fs)
library(dplyr, exclude = c("between", "first", "last"))
library(fst)
library(data.table)

.datatable.aware <- TRUE #nolint

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

# Do the runs
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

# Write the outputs into a concise format
get_probability_files <- function(outdir) {
  tibble::tibble(
    directory = fs::dir_ls(outdir),
    files = purrr::map(directory, dir_ls, glob = "*.csv") %>%
      purrr::map(as.character)
  ) %>%
    tidyr::unnest(files) %>%
    dplyr::transmute(
      scenario = fs::path_file(directory),
      file = files
    )
}

read_probability_scenario <- function(scenario_files) {
  .datatable.aware <- TRUE
  outdir = path(
    "output", "zz-raw-output",
    "probability-processed"
  )
  fs::dir_create(outdir)
  dat <- rbindlist(lapply(scenario_files[["file"]], fread))
  setDT(dat)
  scenario <- unique(dat[["scenario"]])
  values <- dat[, .(year, variable, value)]
  fst::write_fst(values, fs::path(outdir, paste0(scenario, ".fst")))
  rm(values)
  params <- unique(dat[, .(isamp, param_ecs, param_diffusivity, param_volscl)])
  rm(dat)
  fst::write_fst(params, fs::path(outdir, paste0("params.", scenario, ".fst")))
}

plan <- bind_plans(plan, drake_plan(
  probability_files_df = get_probability_files(path(
    outdir, "zz-raw-output", "probability"
  )),
  probability_processed = target(
    read_probability_scenario(dplyr::filter(
      probability_files_df, scenario == s
    )),
    transform = map(s = !!scenarios),
    # Without this, we run out of memory when trying to read multiple files
    # Might be able to get around it on beefier systems
    hpc = FALSE
  )
))

### Make plan
options(clustermq.scheduler = "multicore")
make(plan, parallelism = "clustermq", jobs = parallel::detectCores())
