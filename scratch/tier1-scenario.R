# devtools::install()
library(hector)
library(tidyverse)
library(hector.rcmip)
library(hectortools)

s <- run_scenario("abrupt-2xCO2")

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

results <- map_dfr(out, fetchvars2)

rplot(results) +
  geom_hline(yintercept = 0, linetype = "dashed")

ggsave("figures/tier1-results.png", width = 7, height = 7)

