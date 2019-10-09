library(tidyverse)
library(fs)
library(here)

## dir_ls(here("data-raw"))

conc_long <- here("data-raw", "rcmip-concentrations-annual-means-v1-0-0.csv") %>%
  read_csv() %>%
  pivot_longer(
    matches("[[:digit:]]{4}"),
    names_to = "year",
    values_to = "value",
    names_ptypes = list(year = numeric())
  )

emiss_long <- here("data-raw", "rcmip-emissions-annual-means-v1-0-0.csv") %>%
  read_csv() %>%
  pivot_longer(
    matches("[[:digit:]]{4}"),
    names_to = "year",
    values_to = "value",
    names_ptypes = list(year = numeric())
  )

rf_long <- here("data-raw", "rcmip-radiative-forcing-annual-means-v1-0-0.csv") %>%
  read_csv() %>%
  pivot_longer(
    matches("[[:digit:]]{4}"),
    names_to = "year",
    values_to = "value",
    names_ptypes = list(year = numeric())
  )

## all_long <- reduce(list(conc_long, emiss_long, rf_long), full_join)
all_long <- bind_rows(conc_long, emiss_long, rf_long)

all_vars <- all_long %>%
  distinct(Variable) %>%
  mutate(
    varlist = str_split(Variable, fixed("|")),
    nvar = lengths(varlist),
    vartype = map_chr(varlist, 1),
    gas = map_chr(varlist, tail, 1),
    other = map(varlist, ~head(tail(.x, -1), -1)) %>%
      map_chr(paste0, collapse = "|") %>%
      na_if("")
  ) %>%
  select(-varlist, -nvar)

all_long2 <- all_long %>%
  left_join(all_vars, "Variable") %>%
  select(Model:Mip_Era, vartype:other, everything())

# How many differen models?
all_long2 %>%
  select(-year, -value, -Variable) %>%
  distinct()

all_long2 %>%
  distinct(vartype, gas) %>%
  mutate(x = 1) %>%
  pivot_wider(names_from = "vartype", values_from = "x") %>%
  print(n = Inf) %>%
  mutate_if(is.numeric, negate(is.na)) %>%
  pivot_longer(-gas) %>%
  ggplot() +
  aes(x = name, y = fct_rev(gas), fill = value) +
  labs(fill = "Has data", y = "Component") +
  geom_tile() +
  cowplot::theme_cowplot() +
  theme(axis.title.x = element_blank())

ggsave("~/Downloads/components.png")
