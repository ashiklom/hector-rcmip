# devtools::install()
library(hector)
library(tidyverse)
library(hector.rcmip)
library(hectortools)

options(nwarnings = 150)

dir.create("figures", showWarnings = FALSE)

tier1 <- tibble(Scenario = c(
  "piControl",
  "esm-piControl",
  "1pctCO2",
  "1pctCO2-4xext",
  "abrupt-4xCO2",
  "abrupt-2xCO2",
  "abrupt-0p5xCO2",
  "historical",
  "ssp119",
  "ssp585",
  "esm-hist",
  "esm-ssp119",
  "esm-ssp585"
))

out <- list()
for (i in seq_len(nrow(tier1))) {
  s <- tier1[i, ][[1]]
  tryCatch(
    out[[s]] <- run_scenario(s),
    error = function(e) {
      message("Scenario ", s, " failed to run with error:\n",
              conditionMessage(e))
    }
  )
}

results <- map_dfr(out, fetchvars2)

rplot(results) +
  geom_hline(yintercept = 0, linetype = "dashed")

ggsave("figures/tier1-results.png", width = 7, height = 7)

picontrol <- run_scenario("piControl")
esm_picontrol <- run_scenario("esm-piControl")
rplot(run_scenario("abrupt-4xCO2"))

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

if (FALSE) {

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

}
