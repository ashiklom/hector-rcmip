library(drake, exclude = c("gather", "expand"))
library(tidyverse)
library(here)
library(glue, exclude = "collapse")
library(fs)
library(data.table, exclude = c("between", "first", "last", "transpose"))

# Make `data.table` bracket notation work.
.datatable.aware <- TRUE #nolint

stopifnot(
  requireNamespace("reshape2", quietly = TRUE),
  requireNamespace("git2r", quietly = TRUE),
  requireNamespace("fst", quietly = TRUE),
  requireNamespace("readxl", quietly = TRUE),
  # For parallel execution
  requireNamespace("clustermq", quietly = TRUE),
  # Needed for dynamic branching
  packageVersion("drake") > "7.7.0"
)

devtools::load_all(here())
expose_imports("hector.rcmip")

outdir <- dir_create(here("output"))
figdir <- dir_create(here("figures"))

# Git commit hash corresponding to Hector RCMIP version
hector_version <- "62381e7"

scenarios <- c(
  "piControl", "esm-piControl", "1pctCO2", "1pctCO2-4xext",
  "abrupt-4xCO2", "abrupt-2xCO2", "abrupt-0p5xCO2",
  "historical", "historical-cmip5",
  "ssp119", "ssp585",
  paste0("rcp", c("26", "45", "60", "85"))
)

models <- c(cmip6_params()[["model"]], "default")

do_scenario <- function(scenario, cmip6_model) {
  core <- run_scenario(scenario, cmip6_model)
  rcmip_outputs(core, dates = 1750:2100) %>%
    dplyr::mutate(rcmip_scenario = scenario,
                  cmip6_model = cmip6_model)
}

### Scenario outputs -- single runs
plan <- drake_plan(
  out = target(
    do_scenario(scenario, model),
    transform = cross(scenario = !!scenarios, model = !!models)
  ),
  all_results = target(
    bind_rows(out) %>%
      filter(year >= 1850, year <= 2100),
    transform = combine(out)
  ),
  everything_plot = ggplot(all_results) +
    aes(x = year, y = value, color = cmip6_model) +
    geom_line() +
    facet_grid(
      vars(variable),
      vars(rcmip_scenario),
      scales = "free_y"
    ) +
    scale_color_brewer(type = "q") +
    theme_bw() +
    theme(
      strip.text.y = element_text(angle = 0),
      axis.text.x = element_text(angle = 90),
      legend.position = "bottom"
    )
)

### Post-processing scenario outputs
plan <- bind_plans(plan, drake_plan(
  scenario_df = rcmip_inputs() %>%
    distinct(Model, Scenario, Region),
  rcmip_vars = readxl::read_excel(
    file_in("data-raw/rcmip-data-submission-template-v3-1-0.xlsx"),
    sheet = "variable_definitions"
  ) %>%
    select(Variable, Unit),
  all_results_rcmip_format = all_results %>%
    rename(Scenario = rcmip_scenario, Variable = variable) %>%
    select(-scenario) %>%
    pivot_wider(names_from = "year", values_from = "value") %>%
    left_join(scenario_df, "Scenario") %>%
    left_join(rcmip_vars, "Variable") %>%
    left_join(distinct(meta_model, cmip6_model, ClimateModel),
              c("Scenario" = "rcmip_scenario", "cmip6_model")) %>%
    select(ClimateModel, Model, Scenario, Region, Variable, Unit,
           dplyr::matches("[[:digit:]]{4}")),
  cmip_params_form = cmip6_params() %>%
    mutate(
      "Model Configuration Description" = glue(
        "Hector calibrated against {model}. ",
        "ECS = {S} degC, ",
        "ocean heat diffusivity = {diff} cm2/s, ",
        "volcanic scaling factor = {volscl}, ",
        "aerosol scaling factor = {alpha}."
      )
    ) %>%
    select(cmip6_model = model, ECS = S, `Model Configuration Description`) %>%
    add_row(
      cmip6_model = "default", ECS = 3.0,
      `Model Configuration Description` = paste0(
        "Hector defaults. ",
        "ECS = 3.0 degC, ",
        "ocean heat diffusivity = 2.3 cm2/s, ",
        "volcanic scaling factor = 1.0, ",
        "aerosol scaling factor = 1.0"
      )
    ),
  meta_model = all_results %>%
    distinct(cmip6_model) %>%
    left_join(cmip_params_form, "cmip6_model") %>%
    mutate(
      "Climate Model Name" = "hector",
      "Climate Model Version" = hector_version,
      "Climate Model Configuration Label" = if_else(
        cmip6_model == "default",
        "DEFAULT",
        sprintf("CMIP6-%s-CALIB", cmip6_model)
      ),
      ClimateModel = paste(`Climate Model Name`, `Climate Model Version`,
                           `Climate Model Configuration Label`,
                           sep = "|"),
      "Project" = "",
      "Name of Person" = "Alexey Shiklomanov",
      "Literature Reference" = "https://doi.org/10.5194/gmd-8-939-2015"
    ),
  meta_model_write = meta_model %>%
    distinct(
      ClimateModel,
      `Climate Model Name`,
      `Climate Model Version`,
      `Climate Model Configuration Label`,
      `ECS`,
      `Model Configuration Description`,
      `Project`,
      `Name of Person`,
      `Literature Reference`
    )
))

### Probability runs
fast_bind <- function(x) {
  x <- purrr::map(x, data.table::setDT)
  data.table::rbindlist(x)
}

plan <- bind_plans(plan, drake_plan(
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
      probability_param_draws[["alpha.temperature"]]
    ) %>%
      mutate(isamp = isamps),
    dynamic = map(probability_param_draws, isamps),
    transform = map(scenario = !!scenarios)
  ),
  probability_summaries = target(
    fast_bind(readd(probability_run))[, .(
      scenario = scenario,
      Mean = mean(value),
      SD = sd(value),
      q025 = quantile(value, 0.025),
      q05 = quantile(value, 0.05),
      q10 = quantile(value, 0.1),
      q25 = quantile(value, 0.25),
      q50 = quantile(value, 0.50),
      q75 = quantile(value, 0.75),
      q90 = quantile(value, 0.9),
      q95 = quantile(value, 0.95),
      q975 = quantile(value, 0.975)
    ), .(year, variable)],
    transform = map(probability_run),
    format = "fst_dt"
  ),
  probability_summary = target(
    bind_rows(probability_summaries),
    transform = combine(probability_summaries),
    format = "fst_dt",
    hpc = FALSE
  ),
  probability_long = melt(
    probability_summary,
    id.vars = c("scenario", "year", "variable"),
    variable.name = "stat"
  )
))

### Formatting probability output
plan <- bind_plans(plan, drake_plan(
 probability_formatted = probability_long %>%
  filter(year >= 1850, year <= 2100) %>%
  reshape2::dcast(scenario + variable + stat ~ year, value.var = "value") %>%
  as_tibble() %>%
  rename(Variable = variable) %>%
  mutate(
    ClimateModel = sprintf("hector|%s|HISTCALIB-%s", hector_version, stat),
    Scenario = str_remove(scenario, "-p$")
    ) %>%
  left_join(scenario_df, "Scenario") %>%
  left_join(rcmip_vars, "Variable") %>%
  mutate(Region = "World") %>%
  select(ClimateModel, Model, Scenario, Region, Variable, Unit,
         dplyr::matches("[[:digit:]]{4}")),
 probability_meta = probability_formatted %>%
   distinct(ClimateModel) %>%
   mutate(
     stat = case_when(
       grepl("-Mean$", ClimateModel) ~ "mean",
       grepl("-SD$", ClimateModel) ~ "standard deviation",
       grepl("-q025$", ClimateModel) ~ "2.5% quantile",
       grepl("-q05$", ClimateModel) ~ "5% quantile",
       grepl("-q10$", ClimateModel) ~ "10% quantile",
       grepl("-q25$", ClimateModel) ~ "25% quantile",
       grepl("-q50$", ClimateModel) ~ "50% quantile",
       grepl("-q75$", ClimateModel) ~ "75% quantile",
       grepl("-q90$", ClimateModel) ~ "90% quantile",
       grepl("-q95$", ClimateModel) ~ "95% quantile",
       grepl("-q975$", ClimateModel) ~ "97.5% quantile",
       TRUE ~ NA_character_
     )
   ) %>%
   transmute(
     ClimateModel = ClimateModel,
     `Climate Model Name` =
       gsub("^(.*)\\|(.*)\\|(.*)$", "\\1", ClimateModel),
     `Climate Model Version` =
       gsub("^(.*)\\|(.*)\\|(.*)$", "\\2", ClimateModel),
     `Climate Model Configuration Label` =
       gsub("^(.*)\\|(.*)\\|(.*)$", "\\3", ClimateModel),
     `ECS` = NA_real_,
     `Model Configuration Description` = glue(
       "Hector with ECS, ocean heat diffusivity, and aerosol scaling factor ",
       "calibrated against historical observations: ",
       "Parameter uncertainty ensemble {stat} (1000 simulations)."
     ),
     `Project` = unique(meta_model[["Project"]]),
     `Name of Person` = unique(meta_model[["Name of Person"]]),
     `Literature Reference` = unique(meta_model[["Literature Reference"]])
   )
))

### Final output files
plan <- bind_plans(plan, drake_plan(
  your_data_csv = all_results_rcmip_format %>%
    bind_rows(probability_formatted) %>%
    write_csv(file_out(!!path(outdir, "your_data.csv"))),
  meta_model_tsv = meta_model_write %>%
    bind_rows(probability_meta) %>%
    write_tsv(file_out(!!path(outdir, "meta_model.tsv")))
))

### Diagnostic plots
scenario_plot <- function(dat, scenario) {
  dat_sub <- dat %>%
    pivot_longer(dplyr::matches("[[:digit:]]{4}"),
                 names_to = "year", values_to = "value",
                 names_ptypes = list(year = numeric())) %>%
    filter(Scenario == scenario)

  dat_scalar <- dat_sub %>%
    filter(!grepl("HISTCALIB", ClimateModel)) %>%
    mutate(
      calib_model = gsub("^.*\\|.*\\|", "", ClimateModel) %>%
        gsub("^CMIP6-", "", .) %>%
        gsub("-CALIB$", "", .)
    )

  dat_prob <- dat_sub %>%
    filter(grepl("HISTCALIB", ClimateModel)) %>%
    mutate(stat = gsub(".*\\|HISTCALIB-(.*)", "\\1", ClimateModel)) %>%
    select(-ClimateModel) %>%
    pivot_wider(names_from = "stat", values_from = "value")

  fcol <- "gray25"
  plt <- ggplot(dat_prob) +
    aes(x = year) +
    geom_ribbon(aes(ymin = q025, ymax = q975),
                fill = fcol, alpha = 0.2) +
    geom_ribbon(aes(ymin = q05, ymax = q95),
                fill = fcol, alpha = 0.2) +
    geom_ribbon(aes(ymin = q10, ymax = q90),
                fill = fcol, alpha = 0.2) +
    geom_ribbon(aes(ymin = q25, ymax = q75),
                fill = fcol, alpha = 0.2) +
    geom_line(aes(y = Mean, color = "probability", linetype = "probability")) +
    geom_line(aes(y = value, color = calib_model, linetype = calib_model),
              data = dat_scalar) +
    facet_wrap(vars(Variable), scale = "free_y") +
    ggtitle(scenario) +
    theme_bw()
  print(plt)
}
plan <- bind_plans(plan, drake_plan(
  diagnostic_plots = {
    pdf(file_out("figures/final-scenario-plots.pdf"), width = 16, height = 9.5)
    walk(scenarios, scenario_plot, dat = your_data_csv)
    dev.off()
  }
))

### Make plan
options(clustermq.scheduler = "multicore")
make(plan, parallelism = "clustermq", jobs = parallel::detectCores())
