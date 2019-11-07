library(hector.rcmip)
library(tidyverse)

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
  "ssp585"
  ## "esm-hist",
  ## "esm-ssp119",
  ## "esm-ssp585"
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

results <- map_dfr(out, rcmip_outputs, dates = seq(1750, 2100))
rplot(results)
ggsave("figures/tier1-results-rcmip-vars.png", width = 10, height = 8)

results %>%
  filter(variable == "Carbon Sequestration",
         year > 1845, year < 1880) %>%
  ggplot() +
  aes(x = year, y = value, color = scenario) +
  geom_line()

rplot(results) +
  geom_hline(yintercept = 0, linetype = "dashed")

core <- out[[1]]
results_rcmip <- map_dfr(out, rcmip_outputs)

ggsave("figures/tier1-results.png", width = 7, height = 7)

