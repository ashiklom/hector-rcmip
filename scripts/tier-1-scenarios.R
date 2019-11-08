library(drake, exclude = c("gather", "expand"))
library(tidyverse)
library(here)
library(glue, exclude = "collapse")
library(fs)

devtools::load_all(here())
expose_imports("hector.rcmip")

options(nwarnings = 150)

outdir <- dir_create(here("output"))
figdir <- dir_create(here("figures"))

scenarios <- c(
  "piControl", "esm-piControl", "1pctCO2", "1pctCO2-4xext",
  "abrupt-4xCO2", "abrupt-2xCO2", "abrupt-0p5xCO2", "historical",
  "ssp119", "ssp585"
  ## "esm-hist", "esm-ssp119", "esm-ssp585"
)

models <- cmip6_params() %>% pull(model) %>% c("default")

do_scenario <- function(scenario, cmip6_model) {
  core <- run_scenario(scenario, cmip6_model)
  rcmip_outputs(core, dates = 1750:2100) %>%
    dplyr::mutate(rcmip_scenario = scenario,
                  cmip6_model = cmip6_model)
}

plan <- drake_plan(
  out = target(
    do_scenario(scenario, model),
    transform = cross(scenario = !!scenarios, model = !!models)
  ),
  all_results = target(
    bind_rows(out),
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
    ),
  scenario_df = rcmip_inputs() %>%
    distinct(Model, Scenario, Region),
  rcmip_vars = rcmip_inputs() %>%
    distinct(Variable, Unit),
  all_results_rcmip_format = all_results %>%
    rename(Scenario = rcmip_scenario, Variable = variable) %>%
    select(-scenario) %>%
    filter(year >= 1850) %>%
    pivot_wider(names_from = "year", values_from = "value") %>%
    left_join(scenario_df, "Scenario") %>%
    left_join(rcmip_vars, "Variable") %>%
    left_join(distinct(meta_model, rcmip_scenario, cmip6_model, ClimateModel),
              c("Scenario" = "rcmip_scenario", "cmip6_model")) %>%
    select(ClimateModel, Model, Scenario, Region, Variable, Unit,
           matches("[[:digit:]]{4}")) %>%
    write_csv(file_out(!!path(outdir, "your_data.csv"))),
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
    distinct(rcmip_scenario, cmip6_model) %>%
    left_join(cmip_params_form, "cmip6_model") %>%
    mutate(
      "Climate Model Name" = "hector",
      "Climate Model Version" = substr(git2r::revparse_single("../hector", "HEAD")$sha, 0, 8),
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
    ) %>%
    write_tsv(file_out(!!path(outdir, "meta_model.tsv")))
)
make(plan)
