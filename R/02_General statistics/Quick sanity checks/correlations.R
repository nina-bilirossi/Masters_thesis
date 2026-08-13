data |>
  filter(year == 2017) |>                    # filter for specific year
  summarise(correlation = cor(pop_rur_unw, state_pop,    # correlation per group
                              use = "complete.obs"))


data |>
  filter(year == 2017) |>                    # filter for specific year
  summarise(correlation = cor(pop_m_unw, state_pop,    # correlation per group
                              use = "complete.obs"))
