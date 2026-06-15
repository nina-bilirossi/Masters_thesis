library(dplyr)
library(tidyr)
library(tidyverse)

ilo_df <- data.frame(
  Age = c("15-24", "25-34", "35-44", "45-54", "55-64", "65+"),
  Male = c(79.463,	76.061,	79.886,	81.258,	85.674,	94.199))


# Bar chart
ggplot(ilo_df, aes(x = Age, y = Male)) +
  geom_bar(stat = "identity", fill = "forestgreen") +
  labs(
    title = "" ,#"Share of Workers Outside the Formal Sector by Age",
    x = "Age",
    y = "% Share"
  ) +
  theme_minimal()
