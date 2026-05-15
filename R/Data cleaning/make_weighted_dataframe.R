library(dplyr)
library(tidyr)

df <- read.csv("/Users/ninabilirossi/Desktop/MSC THESIS/Data works/Code/Outputs/final material/regression_dataframe_deeper.csv")
colnames(df)


pop <- df |> dplyr::select(pop_tot_unw, STATE, pop_workingage_unw, state_pop)

# Identify columns:
# - ending with "_unw"
# - NOT containing "pop"
cols_to_weight <- names(df) %>%
  .[grepl("_unw$", .) & !grepl("pop", ., ignore.case = TRUE)]

# Create weighted versions with suffix "_w"
df <- df %>%
  mutate(
    across(
      all_of(cols_to_weight),
      ~ .x * state_pop,
      .names = "{sub('_unw$', '_w', .col)}"
    )
  )

# Check new column names
colnames(df)