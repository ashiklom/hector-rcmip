library(drake, exclude = c("gather", "expand"))
library(tidyverse)
library(here)

devtools::load_all(here())
expose_imports("hector.rcmip")

options(nwarnings = 150)

dir.create("figures", showWarnings = FALSE)

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
    )
)
make(plan)
