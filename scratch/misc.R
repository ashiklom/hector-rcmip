rcmip_inputs() %>%
  filter(Scenario == "abrupt-2xCO2") %>%
  distinct(Variable, value)

picontrol <- run_scenario("piControl")
pctco2 <- run_scenario("1pctCO2")
abrupt <- run_scenario("abrupt-2xCO2")
purrr::map_dfr(list(picontrol, pctco2, abrupt), fetchvars2) %>%
  rplot()

library(tidyverse)
devtools::load_all()
abrupt <- run_scenario("abrupt-4xCO2")
## out <- rcmip_outputs(abrupt, dates = 1750:2100)
v <- c(hector::ATMOSPHERIC_CO2(), hector::LAND_CFLUX(), hector::OCEAN_CFLUX())
out <- hector::fetchvars(abrupt, 1845:1865, v)
ggplot(out) +
  aes(x = year, y = value, color = scenario) +
  geom_line() +
  geom_point() +
  facet_grid(vars(variable), scales = "free_y")

write_csv(out, "~/Downloads/hector-earthc.csv")

compare <- bind_rows(
  deepocean = read_csv("~/Downloads/hector-deepocean.csv"),
  deepocean_timestep = read_csv("~/Downloads/hector-deepocean-timestep.csv"),
  earthc = read_csv("~/Downloads/hector-earthc.csv"),
  .id = "config"
)

compare %>%
  filter(config != "deepocean_timestep") %>%
  ggplot() +
  aes(x = year, y = value, color = config) +
  geom_line() +
  geom_point() +
  facet_grid(vars(variable), scales = "free_y")

ggsave("~/Downloads/abrupt-co2.png")

ggplot2::ggsave("figures/control-1pct-abrupt.png", width = 7, height = 7)

rplot(run_scenario(scenario))

plot(value ~ year, co2)

rf_funs <- lsf.str("package:hector") %>%
  grep("^RF_", ., value = TRUE)
rf_vars <- map_chr(rf_funs, exec)

results <- fetchvars2(picontrol, c(rf_vars, "Tgav"))
results %>%
  filter(abs(value) > 0) %>%
  ggplot() +
  aes(x = year, y = value) +
  geom_line() +
  facet_wrap(vars(variable), scales = "free_y")

tier1_results <- tier1_inputs %>%
  distinct(Model, Scenario) %>%
  mutate(
    result = map(Scenario, run_concentration_scenario,
                 input = tier1_inputs)
  )

warnings()

tier1_results %>%
  unnest(result) %>%
  filter(year <= 2100) %>%
  ggplot() +
  aes(x = year, y = value, color = Scenario) +
  geom_line() +
  facet_wrap(vars(variable), scales = "free_y") +
  scale_color_brewer(type = "qual") +
  theme_bw()

ggsave("figures/rcmip-tier1.png", width = 7, height = 7)

input_data <- tier1_inputs %>%
  filter(Scenario == Scenario[[1]])
scenario <- input_data[["Scenario"]][1]

tier1_inputs %>%
  distinct(Variable, Unit) %>%
  filter(grepl("N2O", Variable)) %>%
  print(n = Inf)

tier1_inputs %>%
  filter(grepl("Emissions\\|CH4", Variable)) %>%
  distinct(Variable, Unit)

tier1_inputs %>%
  distinct(Scenario, Variable) %>%
  count(Variable, sort = TRUE) %>%
  ## filter(grepl("^Emissions", Variable)) %>%
  print(n = Inf)

pivot_cols <- input_sub %>%
  dplyr::select(-year, -value, -Variable) %>%
  colnames()


input_wide_fill <- input_wide %>%
  mutate_if(
    is.double,
    ~.x[!is.na(.x)][]
  )


add_wide <- tibble::tibble(
  !!!as.list(dplyr::select(
    input_wide[1,], -year, -`Atmospheric Concentrations|CO2`
  )),
  year = seq(1750, min(input_wide[["year"]]) - 1),
  `Atmospheric Concentrations|CO2` = 276
)
input_wide2 <- bind_rows(add_wide, input_wide)
input_sub2 <- input_wide2 %>%
  pivot_longer(-c(pivot_cols, "year"), names_to = "Variable", values_to = "value")

ggplot(input_sub) +
  aes(x = year, y = value) +
  geom_point() +
  facet_wrap(vars(Variable), scales = "free_y")


sn <- run_scenario("abrupt-4xCO2")
rplot(sn) +
  ggplot2::xlim(c(NA, 2100))

##################################################

scenario <- "1pctCO2"
core <- run_scenario(scenario)
out <- rcmip_outputs(core, 1750:2100)
rplot(out) + ggplot2::xlim(1840, 1860)

hector::fetchvars(s, 2000:2010, "rh", "PgC year-1")

fetchvars2(core, hector::LAND_CFLUX(), 1750:2100)
fetchvars2(core, hector::OCEAN_CFLUX())
x <- fetchvars2(core, c(hector::LAND_CFLUX(), hector::OCEAN_CFLUX()), 1750:2100)
rplot(x)

hector::LAND_CFLUX()
hector::OCEAN_CFLUX()

input_sub <- rcmip_inputs() %>%
  dplyr::filter(Scenario == !!scenario)

input_sub %>%
  mutate(Variable = str_remove(Variable, "Atmospheric Concentrations\\|")) %>%
  filter(Variable == "CO2", year < 2000) %>%
  ggplot() +
  aes(x = year, y = value) +
  geom_line()
  ## facet_wrap(vars(Variable), scales = "free_y")
