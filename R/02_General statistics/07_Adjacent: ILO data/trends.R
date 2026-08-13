library(dplyr)
library(ggplot2)
library(stringr)
library(patchwork)

inf_df <- read.csv("./Data/ILOSTAT/ILO data/EMP_NIFL_SEX_EDU_GEO_RT_A-filtered-2026-03-05.csv")
emp_df <- read.csv("./Data/ILOSTAT/ILO data/EMP_DWAP_SEX_EDU_RT_A-filtered-2026-03-05-2.csv")
inf_acti <- read.csv("./Data/ILOSTAT/ILO data/EMP_NIFL_SEX_ECO_RT_A-filtered-2026-03-05.csv")
emp_acti <- read.csv("./Data/ILOSTAT/ILO data/EMP_TEMP_SEX_AGE_ECO_NB_A-filtered-2026-03-05-2.csv")


inf_df <- inf_df |>  filter(time <2017)
emp_df <- emp_df |>  filter(time <2017)

# ----------------
# 1. Informality
# 1.1  Informality by gender

inf_df_gender <- inf_df |>
  filter(classif1.label == "Education (Aggregate levels): Total",
         classif2.label == "Area type: National",
         sex.label != "Other",
         #sex.label != "Total"
         )

inf_gender <- ggplot(inf_df_gender, aes(x = time, y = obs_value, color = sex.label)) +
  geom_line() +
  geom_point() +
  labs(
    x = "Year",
    y = "Informal employment (%)",
    title = "Informal employment by gender",
    color = "Gender"
  ) +
  theme_minimal()

# 1.2 Informality by education level

inf_df_edu <- inf_df |>
  filter(sex.label == "Total",
         classif2.label == "Area type: National",
         classif1.label != "Education (Aggregate levels): Level not stated",
         str_detect(classif1.label, "\\(Aggregate levels\\)"))

inf_edu <- ggplot(inf_df_edu, aes(x = time, y = obs_value, color = classif1.label)) +
  geom_line() +
  geom_point() +
  labs(
    x = "Year",
    y = "Informal employment (%)",
    title = "Informal employment by education",
    color = "Education"
  ) +
  theme_minimal()

# 1.2 Informality by rural/urban

inf_df_rur <- inf_df |>
  filter(sex.label == "Total",
         #classif2.label == "Area type: National",
         classif1.label == "Education (Aggregate levels): Total")

inf_geo <- ggplot(inf_df_rur, aes(x = time, y = obs_value, color = classif2.label)) +
  geom_line() +
  geom_point() +
  labs(
    x = "Year",
    y = "Informal employment (%)",
    title = "Informal employment by geography",
  ) +
  theme_minimal()

# ----------------
# 1. Employment
# 1.1  Employment by gender

emp_df_gender <- emp_df |>
  filter(classif1.label == "Education (Aggregate levels): Total",
         sex.label != "Other")

emp_gender <- ggplot(emp_df_gender, aes(x = time, y = obs_value, color = sex.label)) +
  geom_line() +
  geom_point() +
  labs(
    x = "Year",
    y = "Employment ratio",
    title = "Employment ratio by gender",
    color = "Gender"
  ) +
  theme_minimal()

# 1.2 Employment by education level

emp_df_edu <- emp_df |>
  filter(sex.label == "Total",
        classif1.label != "Education (Aggregate levels): Level not stated",
         str_detect(classif1.label, "\\(Aggregate levels\\)"))

emp_edu <- ggplot(emp_df_edu, aes(x = time, y = obs_value, color = classif1.label)) +
  geom_line() +
  geom_point() +
  labs(
    x = "Year",
    y = "Employment ratio",
    title = "Employment by education",
    color = "Education"
  ) +
  theme_minimal()


# ---------
# Economic activities

emp_acti_tot <- emp_acti |>
  filter(time >2016, sex.label == "Total",
         classif1.label == "Age (Youth, adults): 15-64",
         str_detect(classif2.label, "Broad sector")
         )

emp_acti_plot <- ggplot(emp_acti_tot, aes(x = time, y = obs_value, color = classif2.label)) +
  geom_line() +
  geom_point() +
  labs(
    x = "Year",
    y = "Employment (thousands)",
    title = "Employment by sector",
    color = "Economic activity"
  ) +
  theme_minimal()

emp_acti_plot

inf_acti_tot <- inf_acti |>
  filter(time>2016, sex.label == "Total",
         # classif1.label == "Age (Youth, adults): 15-64",
         str_detect(classif1.label, "Broad sector")
  )

inf_acti_plot <- ggplot(inf_acti_tot, aes(x = time, y = obs_value, color = classif1.label)) +
  geom_line() +
  geom_point() +
  labs(
    x = "Year",
    y = "Informal employment rate (%)",
    title = "Informality by sector",
    color = "Economic activity"
  ) +
  theme_minimal()

inf_acti_plot
# ----------------
# Plotting

emp_edu + inf_edu

emp_gender + inf_gender

