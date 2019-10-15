library(tidyverse)
library(hector.rcmip)

all_long <- rcmip_inputs()

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

all_long2 %>%
  ## distinct(vartype, gas, Variable, Unit) %>%
  distinct(Variable, Unit) %>%
  filter(grepl("oc", Variable, ignore.case = TRUE)) %>%
  print(n = Inf)

# How many differen models?
all_long2 %>%
  distinct(Model, Scenario) %>%
  print(n = Inf)

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
  theme(axis.title.x = element_blank())

ggsave("~/Downloads/components.png")
