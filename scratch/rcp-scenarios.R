library(tidyverse)
library(hector.rcmip)

rcin <- rcmip_inputs()

rcps <- paste0("rcp", c("26", "45", "60", "85"))
models <- "default"
runs <- crossing(
  Scenario = rcps,
  cmip6_model = models
)

do_scenario <- function(scenario, cmip6_model) {
  core <- run_scenario(scenario, cmip6_model)
  rcmip_outputs(core, dates = 1750:2100) %>%
    dplyr::mutate(rcmip_scenario = scenario,
                  cmip6_model = cmip6_model)
}

results <- runs %>%
  mutate(results = map2(Scenario, cmip6_model, do_scenario))

results_df <- results %>%
  select(-cmip6_model) %>%
  unnest(results)
