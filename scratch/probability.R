scenario <- "historical"

hist_results <- run_probability("historical", n = 1000)
hist_results_full <- unnest(hist_results, results)

hc <- system.file("input", "hector_rcp45.ini", package = "hector") %>%
  newcore(suppresslogging = TRUE)

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
