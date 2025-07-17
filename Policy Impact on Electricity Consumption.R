library(tidyverse)
library(lubridate)
library(dplyr)
library(readxl)
library(fixest)
library(broom)
library(ggplot2)

# Data Preparation
df <- read_excel("Electricity EU.xlsx")
num_countries <- df %>% distinct(Country) %>% nrow()  
date_seq <- seq(as.Date("2018-01-01"), as.Date("2025-03-01"), by = "month")
df <- df %>% select(-Date)
df$Date <- rep(date_seq, times = num_countries)
df <- df %>% relocate(Date, .after = Country)
# Check for missing data
sum(is.na(df$Electricity_Consumption))
sum(is.na(df$Price))
head(df)
tail(df)

# 1. TWFE: 27 Countries
df <- df %>%
  mutate(
    treated = if_else(Country %in% c("Spain", "Greece", "Italy"), 1, 0),
    post = if_else(Date >= as.Date("2022-06-01"), 1, 0),
    treat_post = treated * post
  )

twfe_model <- feols(
  Electricity_Consumption ~ treat_post + Price | Country + Date,
  data = df,
  cluster = ~Country
)
summary(twfe_model)

# 2. Placebo: 27 Countries
df <- df %>%
  mutate(
    post_placebo = if_else(Date >= as.Date("2021-06-01"), 1, 0),
    treat_post_placebo = treated * post_placebo
  )

placebo_model <- feols(
  Electricity_Consumption ~ treat_post_placebo + Price | Country + Date,
  data = df,
  cluster = ~Country
)
summary(placebo_model)

# 3. Parallel Trends Plot: 27 Countries
agg_data <- df %>%
  group_by(treated, Date) %>%
  summarise(avg_consumption = mean(Electricity_Consumption, na.rm = TRUE), .groups = "drop")
ggplot(agg_data, aes(x = Date, y = avg_consumption, color = factor(treated))) +
  geom_line(size = 1) +
  geom_vline(xintercept = as.Date("2022-06-01"), linetype = "dashed", color = "black") +
  labs(
    title = "Parallel Trends: Electricity Consumption (Pre- and Post-Policy)",
    subtitle = "Pre-treatment (before June 2022) checks parallel trends assumption",
    x = "Date",
    y = "Average Electricity Consumption",
    color = "Group"
  ) +
  scale_color_manual(values = c("#1f77b4", "#ff7f0e"), labels = c("Control", "Treated")) +
  theme_minimal()

# 4. TWFE: Neighboring Countries
countries <- c("Spain", "Greece", "Italy", "France", "Portugal", "Bulgaria", "Switzerland", "Austria", "Slovenia")
df_subset <- df %>%
  filter(Country %in% countries) %>%
  mutate(
    treated = if_else(Country %in% c("Spain", "Greece", "Italy"), 1, 0),
    post = if_else(Date >= as.Date("2022-06-01"), 1, 0),
    treat_post = treated * post
  )

twfe_model <- feols(
  Electricity_Consumption ~ treat_post + Price | Country + Date,
  data = df_subset,
  cluster = ~Country
)
summary(twfe_model)

# 5. Placebo: Neighboring Countries
df_subset <- df_subset %>%
  mutate(
    post_placebo = if_else(Date >= as.Date("2021-06-01"), 1, 0),
    treat_post_placebo = treated * post_placebo
  )

placebo_model <- feols(
  Electricity_Consumption ~ treat_post_placebo + Price | Country + Date,
  data = df_subset,
  cluster = ~Country
)
summary(placebo_model)

# 6. Parallel Trends Plot: Neighboring Countries
agg_data <- df_subset %>%
  group_by(treated, Date) %>%
  summarise(avg_consumption = mean(Electricity_Consumption, na.rm = TRUE), .groups = "drop")
ggplot(agg_data, aes(x = Date, y = avg_consumption, color = factor(treated))) +
  geom_line(size = 1) +
  geom_vline(xintercept = as.Date("2022-06-01"), linetype = "dashed", color = "black") +
  labs(
    title = "Parallel Trends: Electricity Consumption (Neighboring Countries)",
    subtitle = "Pre-treatment (before June 2022) checks parallel trends assumption",
    x = "Date",
    y = "Average Electricity Consumption",
    color = "Group"
  ) +
  scale_color_manual(values = c("#1f77b4", "#ff7f0e"), labels = c("Control", "Treated")) +
  theme_minimal()

# 7. Event Study: 27 Countries
df <- df %>%
  mutate(
    rel_time = as.numeric(floor((interval(as.Date("2022-06-01"), Date) %/% months(1)))),
    rel_time_factor = factor(rel_time)
  )

event_study_model <- feols(
  Electricity_Consumption ~ i(rel_time_factor, treated, ref = -1) + Price | Country + Date,
  data = df,
  cluster = ~Country
)
summary(event_study_model)

event_study_coefs <- tidy(event_study_model) %>%
  filter(str_detect(term, "rel_time_factor")) %>%
  mutate(
    rel_time = as.numeric(str_extract(term, "-?\\d+")),
    estimate = estimate,
    std.error = std.error,
    conf.low = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error
  )

ggplot(event_study_coefs, aes(x = rel_time, y = estimate)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "red") +
  labs(
    title = "Event Study: Policy Effect on Electricity Consumption (27 Countries)",
    subtitle = "Pre- and Post-Policy Effects, Normalized to t = -1",
    x = "Months Relative to Policy (June 2022)",
    y = "Estimated Treatment Effect"
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(event_study_coefs$rel_time), max(event_study_coefs$rel_time), by = 3))

# 8. Event Study: Neighboring Countries
df_subset <- df %>%
  filter(Country %in% countries) %>%
  mutate(
    treated = if_else(Country %in% c("Spain", "Greece", "Italy"), 1, 0),
    rel_time = as.numeric(floor((interval(as.Date("2022-06-01"), Date) %/% months(1)))),
    rel_time_factor = factor(rel_time)
  )

event_study_model_subset <- feols(
  Electricity_Consumption ~ i(rel_time_factor, treated, ref = -1) + Price | Country + Date,
  data = df_subset,
  cluster = ~Country
)
summary(event_study_model_subset)

event_study_coefs_subset <- tidy(event_study_model_subset) %>%
  filter(str_detect(term, "rel_time_factor")) %>%
  mutate(
    rel_time = as.numeric(str_extract(term, "-?\\d+")),
    estimate = estimate,
    std.error = std.error,
    conf.low = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error
  )

ggplot(event_study_coefs_subset, aes(x = rel_time, y = estimate)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "red") +
  labs(
    title = "Event Study: Policy Effect on Electricity Consumption (Neighboring Countries)",
    subtitle = "Pre- and Post-Policy Effects, Normalized to t = -1",
    x = "Months Relative to Policy (June 2022)",
    y = "Estimated Treatment Effect"
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(event_study_coefs_subset$rel_time), max(event_study_coefs_subset$rel_time), by = 3))
