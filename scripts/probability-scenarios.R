library(hector.rcmip)
library(tidyverse)
library(hector)

posteriors <- read_csv("data-raw/brick-posteriors/emissions_17k_posteriorSamples.csv")

run_with_param <- function(core, pS, pdiff, palpha, .pb = NULL) {
  if (!is.null(.pb)) .pb$tick()
  hector::reset(core)
  hector::setvar(core, NA, ECS(), pS, getunits(ECS()))
  hector::setvar(core, NA, DIFFUSIVITY(), pdiff, getunits(DIFFUSIVITY()))
  hector::setvar(core, NA, VOLCANIC_SCALE(), palpha, getunits(VOLCANIC_SCALE()))
  hector::run(core)
  dplyr::mutate(
    rcmip_outputs(core, dates = 1750:2100),
    param_ecs = pS,
    param_diffusivity = pdiff,
    param_volscl = palpha
  )
}

hc <- system.file("input", "hector_rcp45.ini", package = "hector") %>%
  newcore(suppresslogging = TRUE)
p1 <- posteriors[1,]
d1 <- run_with_param(hc, p1[[1]], p1[[2]], p1[[3]])

n <- 1000
pb <- progress::progress_bar$new(total = n)
post_inputs <- posteriors %>%
  sample_n(n, replace = FALSE) %>%
  mutate(
    results = pmap(list(
      pS = S.temperature,
      pdiff = diff.temperature,
      palpha = alpha.temperature
    ), run_with_param, core = hc, .pb = pb)
  )

outputs <- post_inputs %>%
  unnest(results)

out_summary <- outputs %>%
  select(S.temperature:alpha.temperature, year, variable, value) %>%
  group_by(year, variable) %>%
  summarize(
    Mean = mean(value),
    SD = sd(value),
    lo = quantile(value, 0.025),
    hi = quantile(value, 0.975)
  ) %>%
  ungroup()

out_summary %>%
  filter(variable %in% c("Heat Uptake|Ocean", "Atmospheric Concentrations|CO2",
                         "Carbon Sequestration", "Ocean Air Temperature Change",
                         "Radiative Forcing|Anthropogenic", "Radiative Forcing|Anthropogenic|CO2",
                         "Surface Air Temperature Change")) %>%
  ggplot() +
  aes(x = year, y = Mean, ymin = lo, ymax = hi) +
  geom_ribbon(fill = "gray70", alpha = 0.8) +
  geom_line(color = "black") +
  facet_wrap(vars(variable), scales = "free_y") +
  ylab("Mean and 95% CI") +
  theme_bw() +
  theme(axis.title.x = element_blank())

ggsave("figures/probability.png", width = 9, height = 7)
