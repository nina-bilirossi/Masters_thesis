library(tidyverse)
library(ggplot2)
library(patchwork)  # for combining plots

# ── Your data ──────────────────────────────────────────────────────────────────
df <- data

colnames(data)

# ── Reshape to long format ─────────────────────────────────────────────────────
df_long <- df %>%
  rename(
    female = s_casual_w_worker_PS_f_unw,
    male   = s_casual_w_worker_PS_m_unw,
    overall = s_casual_w_worker_PS_unw
  ) %>%
  pivot_longer(
    cols      = c(female, male, overall),
    names_to  = "gender",
    values_to = "informality_rate"
  ) |>  select(gender, STATE, year, informality_rate, pop_f_unw, pop_m_unw, state_pop)

head(df_long)

df <- df_long

uts_to_drop <- c(
  "ANDAMAN & NICOBAR",
  "CHANDIGARH",
  "DADRA & NAGAR HAVELI & DAMAN & DIU",
  "DELHI",
  #"JAMMU AND KASHMIR",  
  "LADAKH",
  "PUDUCHERRY"
)

df_long <- df %>%
  filter(!STATE %in% uts_to_drop)
df <- df_long

# ── 1. National trends (average across states) ────────────────────────────────
national <- df %>%
  group_by(year, gender) %>%
  summarise(rate = mean(informality_rate, na.rm = TRUE), .groups = "drop")

national_ci <- df_long %>%
  group_by(year, gender) %>%
  summarise(
    rate  = weighted.mean(informality_rate, w = state_pop, na.rm = TRUE),
    lower = quantile(informality_rate, 0.1, na.rm = TRUE),
    upper = quantile(informality_rate, 0.9, na.rm = TRUE),
    .groups = "drop"
  )

outliers <- df_long %>%
  left_join(national_ci %>% select(year, gender, lower, upper),
            by = c("year", "gender")) %>%
  filter(informality_rate < lower | informality_rate > upper)

p1 <- ggplot(national_ci, aes(x = year, colour = gender, fill = gender, group = gender)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.15, colour = NA) +
  geom_line(aes(y = rate), linewidth = 1.2) +
  geom_point(aes(y = rate), size = 3) +
  geom_point(data = outliers, aes(y = informality_rate),
             size = 2, alpha = 0.7, shape = 21,
             stroke = 0.4, fill = "white") +
  scale_colour_manual(values = c("male" = "#185FA5", "female" = "#D4537E")) +
  scale_fill_manual(values  = c("male" = "#185FA5", "female" = "#D4537E")) +
  scale_x_continuous(breaks = unique(df_long$year)) +
  scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  labs(title    = "National informality rate over time",
       subtitle = "Lines = population-weighted average, shading = 90% state range, points = outlier states",
       x = NULL, y = "Informality rate", colour = NULL, fill = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")

p1
# ── 2. Gender gap evolution (female − male, per year) ─────────────────────────
gap_national <- national %>%
  pivot_wider(names_from = gender, values_from = rate) %>%
  mutate(gap = female - male)

p2 <- ggplot(gap_national, aes(x = year, y = gap, fill = gap > 0)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_hline(yintercept = 0, linewidth = 0.4, colour = "grey40") +
  scale_fill_manual(values = c("TRUE" = "#639922", "FALSE" = "#D85A30")) +
  scale_x_continuous(breaks = unique(df$year)) +
  labs(title = "Gender gap (female − male) over time",
       x = NULL, y = "Percentage points") +
  theme_minimal(base_size = 12)

p2

length(unique(df_long$STATE))

gap_ci <- df_long %>%
  pivot_wider(names_from = gender, values_from = informality_rate) %>%
  mutate(gap = female - male) %>%
  group_by(year) %>%
  summarise(
    gap_avg = weighted.mean(gap, w = state_pop, na.rm = TRUE),
    lower   = quantile(gap, 0.1, na.rm = TRUE),
    upper   = quantile(gap, 0.9, na.rm = TRUE),
    .groups = "drop"
  )

gap_outliers <- df_long %>%
  pivot_wider(names_from = gender, values_from = informality_rate) %>%
  mutate(gap = female - male) %>%
  left_join(gap_ci %>% select(year, lower, upper), by = "year") %>%
  filter(gap < lower | gap > upper)

p2 <- ggplot(gap_ci, aes(x = year)) +
  geom_hline(yintercept = 0, linewidth = 0.4, colour = "grey40", linetype = "dashed") +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#888780", alpha = 0.15) +
  geom_line(aes(y = gap_avg), linewidth = 1.2, colour = "#2C2C2A") +
  geom_point(aes(y = gap_avg), size = 3, colour = "#2C2C2A") +
  geom_point(data = gap_outliers, aes(y = gap),
             size = 2, alpha = 0.7, shape = 21,
             colour = "#2C2C2A", fill = "white", stroke = 0.4) +
  #geom_text_repel(data = gap_outliers, aes(y = gap, label = STATE),
  #                size = 2.5, colour = "#2C2C2A",
  #                nudge_x = 0.15, min.segment.length = 0.2,
  #                segment.size = 0.3, show.legend = FALSE) +
  scale_x_continuous(breaks = unique(df_long$year)) +
  scale_y_continuous(labels = scales::label_number(suffix = " pp")) +
  labs(title    = "Gender gap (female − male) over time",
       #subtitle = "Line = population-weighted average, shading = 90% state range, points = outlier states",
       x = NULL, y = "Percentage points") +
  theme_minimal(base_size = 12)

p2
library(ggrepel)

gap_states <- df_long %>%
  pivot_wider(names_from = gender, values_from = informality_rate) %>%
  mutate(gap = female - male)

p2_2 <- ggplot() +
  geom_hline(yintercept = 0, linewidth = 0.4, colour = "grey40", linetype = "dashed") +
  geom_line(data = gap_states, aes(x = year, y = gap, group = STATE, colour = STATE),
            linewidth = 0.5, alpha = 0.6) +
  geom_ribbon(data = gap_ci, aes(x = year, ymin = lower, ymax = upper),
              fill = "#2C2C2A", alpha = 0.10) +
  geom_line(data = gap_ci, aes(x = year, y = gap_avg),
            linewidth = 1.4, colour = "#2C2C2A") +
  geom_point(data = gap_ci, aes(x = year, y = gap_avg),
             size = 3, colour = "#2C2C2A") +
  geom_text_repel(data = gap_outliers, aes(x = year, y = gap, label = STATE),
                  size = 2.5, colour = "#2C2C2A",
                  nudge_x = 0.15, min.segment.length = 0.2,
                  segment.size = 0.3, show.legend = FALSE) +
  scale_colour_manual(values = colorRampPalette(
    c("#185FA5", "#1D9E75", "#D4537E", "#BA7517", "#534AB7", "#D85A30")
  )(n_distinct(gap_states$STATE))) +
  scale_x_continuous(breaks = unique(df_long$year)) +
  scale_y_continuous(labels = scales::label_number(suffix = " pp")) +
  labs(title    = "Gender gap (female − male) by state over time",
       subtitle = "Coloured lines = individual states, bold = population-weighted average, shading = 95% range",
       x = NULL, y = "Percentage points", colour = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "right",
        legend.text = element_text(size = 7),
        legend.key.height = unit(0.5, "cm"))

p2_2

# ── 3. Gender gap by state (averaged across all years) ────────────────────────
gap_state <- df %>%
  pivot_wider(names_from = gender, values_from = informality_rate) %>%
  mutate(gap = female - male) %>%
  group_by(STATE) %>%
  summarise(gap_avg = mean(gap, na.rm = TRUE), .groups = "drop") %>%
  mutate(state = fct_reorder(STATE, gap_avg))

p3 <- ggplot(gap_state, aes(x = gap_avg, y = state, fill = gap_avg > 0)) +
  geom_col(show.legend = FALSE) +
  geom_vline(xintercept = 0, linewidth = 0.4, colour = "grey40") +
  scale_fill_manual(values = c("TRUE" = "#639922", "FALSE" = "#D85A30")) +
  labs(title = "Average informality gender gap by state",
       x = "Percentage points (female − male)", y = NULL) +
  theme_minimal(base_size = 11)
p3
# ── 4. State trends — small multiples ─────────────────────────────────────────
p4 <- ggplot(df, aes(x = year, y = informality_rate,
                     colour = gender)) +
  geom_line(linewidth = 0.8) +
  scale_x_continuous(breaks = unique(df$year)) +
  scale_y_continuous(labels = scales::label_percent()) +
  scale_colour_manual(values = c("male" = "#185FA5", "female" = "#D4537E", "overall" = "grey40")) +
  facet_wrap(~ STATE, ncol = 5, scales = "free") +
  labs(title = "Informality rate by state")+
  theme_minimal(base_size = 9) +
  theme(legend.position = "top",
        strip.text = element_text(size = 7))
p4
# ── 5. State clustering by trend slope ────────────────────────────────────────
slopes <- df %>%
  group_by(STATE, gender) %>%
  summarise(slope = coef(lm(informality_rate ~ year))[["year"]], .groups = "drop") %>%
  pivot_wider(names_from = gender, values_from = slope) %>%
  rename(slope_m = male, slope_f = female) %>%
  mutate(cluster = case_when(
    slope_m < -0.5 & slope_f < -0.5 ~ "Both declining",
    slope_m >  0.3 | slope_f >  0.3 ~ "Rising",
    TRUE                             ~ "Stable"
  ))

p5 <- ggplot(slopes, aes(x = slope_m, y = slope_f, colour = cluster)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey70") +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey70") +
  geom_point(size = 3, alpha = 0.85) +
  geom_text(aes(label = STATE), size = 2.5, vjust = -0.8, show.legend = FALSE) +
  scale_colour_manual(values = c(
    "Both declining" = "#1D9E75",
    "Rising"         = "#D85A30",
    "Stable"         = "#888780"
  )) +
  labs(title = "State clusters by trend slope",
       x = "Male slope (pp/year)", y = "Female slope (pp/year)", colour = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")

# ── Combine and print ──────────────────────────────────────────────────────────
(p1 | p2) / p3
p4
p5
