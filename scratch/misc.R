rcmip_inputs() %>%
  filter(Scenario == "abrupt-2xCO2") %>%
  distinct(Variable, value)

picontrol <- run_scenario("piControl")
pctco2 <- run_scenario("1pctCO2")
abrupt <- run_scenario("abrupt-2xCO2")
purrr::map_dfr(list(picontrol, pctco2, abrupt), fetchvars2) %>%
  rplot()

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
s <- run_scenario(scenario)
out <- rcmip_outputs(s)
rplot(out)

input_sub <- rcmip_inputs() %>%
  dplyr::filter(Scenario == !!scenario)

input_sub %>%
  mutate(Variable = str_remove(Variable, "Atmospheric Concentrations\\|")) %>%
  filter(Variable == "CO2", year < 2000) %>%
  ggplot() +
  aes(x = year, y = value) +
  geom_line()
  ## facet_wrap(vars(Variable), scales = "free_y")
