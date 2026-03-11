# ====================================================
# Script Name: BeeClim_analysis_AB.R
# Project: BumblebeeClim 2025-26
# Author: Alex Beauchemin
# Date Created: 2026-03-04
# Last Modified: 2026-03-04
# Description: This script includes the BumblebeeClim analysis.
# Dependencies: 
# ====================================================

#### SETUP ----
# Load required libraries
library(tidyverse)

# Add site coordinates
lat <- 69.583
lon <- -139.03

# Load compiled datasets
beeclim_2025 <- read_csv("./data/clean/beeclim_2025.csv")
beeclim_ECCC <- read_csv("./data/clean/beeclim_ECCC.csv")


#### ANALYSIS 1: Can we detect a difference in temporal patterning of bumblebee activity between warm and cool days

# Change timezone to show local time
attr(beeclim_ECCC$datetime_LST, "tzone") <- "America/Whitehorse"

# Calculate average daily temperature and wind
beeclim_ECCC <-  beeclim_ECCC %>%
  mutate(date = floor_date(datetime_LST, "day"))
mean_temp_wind <- beeclim_ECCC %>%
  group_by(date) %>%
  summarize(mean_temp = mean(temperature_C, na.rm = TRUE),
            mean_wind = mean(wind_speed_kmh, na.rm = TRUE))

# Visually assess threshold wind speed for bumblebee activity 
ggplot(beeclim_ECCC, aes(x = wind_speed_kmh, y = detections_above_th2)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_classic() +
  ylim(0,1000) +
  xlim(0,50) # bumblebee activity does not appear to drop off dramatically until wind speeds are >20 km/h


# Filter for winds < 20, days with sun altitude always > 0, retain warmest days
ideal_bee_days <- mean_temp_wind %>%
  filter(mean_wind < 20)

ideal_bee_days_2024 <- ideal_bee_days %>%
  filter(date >= as.Date("2024-07-01") &
           date <= as.Date("2024-08-30"))
ideal_bee_days_2025 <- ideal_bee_days %>%
  filter(date >= as.Date("2025-06-01") &
           date <= as.Date("2025-08-30"))

ggplot(ideal_bee_days_2024, aes(x = date)) +
  geom_point(aes(y = mean_temp), colour = "darkred") +
  geom_line(aes(y = mean_temp), colour = "darkred") +
  geom_point(aes(y = mean_wind), colour = "lightblue") +
  geom_line(aes(y = mean_wind), colour = "lightblue") +
  theme_classic() 


ggplot(ideal_bee_days_2025, aes(x = date)) +
  geom_point(aes(y = mean_temp), colour = "darkred") +
  geom_line(aes(y = mean_temp), colour = "darkred") +
  geom_point(aes(y = mean_wind), colour = "lightblue") +
  geom_line(aes(y = mean_wind), colour = "lightblue") +
  theme_classic() 


# Test the following:
# Warm 2024 days: 2024-08-07, 2024-07-25, 2024-07-19, 2024-07-13, 2024-07-24
# Warm 2025 days: 2025-07-05, 2025-08-03, 2025-06-14, 2025-07-16, 2025-07-14

# Cold 2024 days: 2024-07-02, 2024-07-09, 2024-07-28, 2024-07-22, 2024-07-05
# Cold 2025 days: 2025-07-09, 2025-07-18, 2025-08-06, 2025-06-25, 2025-07-12
hot_dates <- as.Date(c("2024-08-07", "2024-07-25", "2024-07-19", "2024-07-13", "2024-07-24", "2025-07-05", "2025-08-03", "2025-06-14", "2025-07-16", "2025-07-14"))
cold_dates <- as.Date(c("2024-07-02", "2024-07-09", "2024-07-28", "2024-07-22", "2024-07-05", "2025-07-09", "2025-07-18", "2025-08-06", "2025-06-25", "2025-07-12"))

# Select dates
hot_days <- beeclim_ECCC %>%
  filter(as.Date(date) %in% hot_dates)
cold_days <- beeclim_ECCC %>%
  filter(as.Date(date) %in% cold_dates)


# Visualize
ggplot(hot_days, aes(x = factor(time_of_day), y = detections_above_th2)) +
  geom_boxplot() +
  geom_point() +
  theme_classic() +
  ylim(0,750)

ggplot(cold_days, aes(x = factor(time_of_day), y = detections_above_th2)) +
  geom_boxplot() +
  geom_point() +
  ylim(0,750)

ggplot(cold_days, aes(x = time_of_day, y = detections_above_th2)) +
  geom_point() +
  geom_smooth() +
  ylim(0,750)



# Visualization farm

daily_time_dist <- beeclim_ECCC %>%
  filter(as.Date(date) %in% c(hot_dates, cold_dates)) %>%
  mutate(day_type = ifelse(as.Date(date) %in% hot_dates, "hot", "cold")) %>%
  group_by(date) %>%
  mutate(total_detections = sum(detections_above_th2), .groups = "drop") %>%
  mutate(prop_activity = detections_above_th2 / total_detections)

ggplot(daily_time_dist,
       aes(x = factor(time_of_day), y = prop_activity, colour = day_type)) +
  geom_boxplot() +
  ylim(0, 0.4)
  #geom_point(alpha=0.2)

beeclim_ECCC_time_bin <- beeclim_ECCC %>%
  mutate(
    hour = as.numeric(format(datetime_LST, "%H")),
    time_bin = cut(hour,
                   breaks = c(0,4,8,12,16,20,24),
                   include.lowest = TRUE)
  )

daily_time_dist_time_bin <- beeclim_ECCC_time_bin %>%
  filter(as.Date(date) %in% c(hot_dates, cold_dates)) %>%
  mutate(day_type = ifelse(as.Date(date) %in% hot_dates, "hot", "cold")) %>%
  group_by(date) %>%
  mutate(total_detections = sum(detections_above_th2), .groups = "drop") %>%
  group_by(date, time_bin, day_type) %>%
  summarize(prop_activity = sum(detections_above_th2) / first(total_detections))

ggplot(daily_time_dist_time_bin,
       aes(x = factor(time_bin), y = prop_activity, colour = day_type)) +
  geom_boxplot() +
  geom_point() +
  theme_classic()



beeclim_ECCC_time_bin_prop <- beeclim_ECCC_time_bin %>%
  left_join(mean_temp_wind, by = "date") %>%
  group_by(date) %>%
  mutate(total_detections = sum(detections_above_th2), .groups = "drop") %>%
  group_by(date, time_bin, mean_temp, mean_wind, total_detections) %>%
  summarize(prop_activity = sum(detections_above_th2) / first(total_detections), .groups = "drop")

beeclim_ECCC_time_bin_detections <- beeclim_ECCC_time_bin %>%
  left_join(mean_temp_wind, by = "date") %>%
  group_by(date) %>%
  mutate(total_detections = sum(detections_above_th2), .groups = "drop") %>%
  group_by(date, time_bin, mean_temp, mean_wind, total_detections) %>%
  summarize(bin_detections = sum(detections_above_th2), .groups = "drop")

bee_model_data <- beeclim_ECCC_time_bin_prop %>%
  filter(!is.nan(prop_activity)) %>%
  filter(mean_wind < 20) %>%
  filter(prop_activity < 1)

mod <- lm(prop_activity ~ mean_temp * time_bin, data = bee_model_data)
summary(mod)

hist(bee_model_data$prop_activity)


library(emmeans)

slopes <- emtrends(mod, ~ time_bin, var = "mean_temp")
summary(slopes, infer = TRUE)

plot(slopes)

as.data.frame(slopes)

ggplot(as.data.frame(slopes), aes(time_bin, mean_temp.trend)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2) +
  labs(y = "Temperature slope", x = "Time of day")

as.data.frame(slopes) %>%
  ggplot(aes(time_bin, mean_temp.trend)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylab("Temperature slope (Δ proportion activity per °C)") +
  xlab("Time of day") +
  theme_classic()

pairs(slopes)

newdata <- expand.grid(
  mean_temp = seq(min(bee_model_data$mean_temp),
                  max(bee_model_data$mean_temp),
                  length.out = 100),
  time_bin = levels(bee_model_data$time_bin)
)

pred <- predict(mod, newdata = newdata, se.fit = TRUE)

newdata$fit <- pred$fit
newdata$se <- pred$se.fit

newdata <- newdata %>%
  mutate(
    lower = fit - 1.96 * se,
    upper = fit + 1.96 * se
  )

ggplot(newdata, aes(x = mean_temp, y = fit, colour = time_bin)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = time_bin),
              alpha = 0.2, colour = NA) +
  facet_wrap(~time_bin) +
  labs(
    x = "Mean temperature (°C)",
    y = "Predicted proportion of daily activity",
    colour = "Time of day",
    fill = "Time of day"
  ) +
  theme_classic()



ggplot() +
  geom_point(data = bee_model_data,
             aes(x = mean_temp, y = prop_activity, colour = time_bin),
             alpha = 0.4) +
  geom_line(data = newdata,
            aes(x = mean_temp, y = fit, colour = time_bin),
            linewidth = 1.2) +
  geom_ribbon(data = newdata,
              aes(x = mean_temp, ymin = lower, ymax = upper, fill = time_bin),
              alpha = 0.2, colour = NA) +
  facet_wrap(~time_bin) +
  labs(
    x = "Mean temperature (°C)",
    y = "Proportion of daily activity",
    colour = "Time of day",
    fill = "Time of day"
  ) +
  theme_classic()

sig_bins <- c("(4,8]", "(8,12]", "(12,16]")

newdata_sig <- newdata %>%
  filter(time_bin %in% sig_bins)

bee_model_sig <- bee_model_data %>%
  filter(time_bin %in% sig_bins)

ggplot() +
  geom_point(data = bee_model_sig,
             aes(mean_temp, prop_activity, colour = time_bin),
             alpha = 0.5) +
  geom_line(data = newdata_sig,
            aes(mean_temp, fit, colour = time_bin),
            colour = "black",
            linewidth = 1.2) +
  geom_ribbon(data = newdata_sig,
              aes(mean_temp, ymin = lower, ymax = upper, fill = time_bin),
              alpha = 0.5) +
  facet_wrap(~time_bin, ncol = 1) +
  theme_classic() +
  labs(
    x = "Daily mean temperature (°C)",
    y = "Proportion of daily activity"
  ) +
  theme(legend.position = "none")


midday_bee <- beeclim_ECCC_time_bin_prop %>%
  filter(prop_activity != "NaN") %>%
  filter(time_bin == "(12,16]") %>%
  filter(mean_wind < 20)

early_morning_bee <- beeclim_ECCC_time_bin_prop %>%
  filter(prop_activity != "NaN") %>%
  filter(time_bin == "(4,8]") %>%
  filter(mean_wind < 20)

morning_bee <- beeclim_ECCC_time_bin_prop %>%
  filter(prop_activity != "NaN") %>%
  filter(time_bin == "(8,12]") %>%
  filter(mean_wind < 20)

evening_bee <- beeclim_ECCC_time_bin_prop %>%
  filter(prop_activity != "NaN") %>%
  filter(time_bin == "(16,20]") %>%
  filter(mean_wind < 20)

night_bee <- beeclim_ECCC_time_bin_prop %>%
  filter(prop_activity != "NaN") %>%
  filter(prop_activity < 1) %>%
  filter(time_bin %in% c("(20,24]", "(0,4]")) %>%
  filter(mean_wind < 20)

ggplot(midday_bee, aes(x = mean_temp, y = prop_activity)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_classic() 
  
ggplot(morning_bee, aes(x = mean_temp, y = prop_activity)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_classic() 

ggplot(evening_bee, aes(x = mean_temp, y = prop_activity)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_classic()

ggplot(night_bee, aes(x = mean_temp, y = prop_activity)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_classic()

ggplot(early_morning_bee, aes(x = mean_temp, y = prop_activity)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_classic()

#### Exploratory visualizations ----

# Histograms for distribution of response variable
hist(beeclim_2025$bumblebee_buzz_hourly)
hist(beeclim_2025$bumblebee_buzz_hourly[
  beeclim_2025$bumblebee_buzz_hourly < 20
], breaks = 100)


# Plotting variables over 24-hour window
ggplot(beeclim_2025, 
aes(x = time_of_day)) +
  geom_smooth(method = "loess", aes(y = temperature_C), colour = "orange2") +
  geom_point(aes(y = bumblebee_buzz_hourly/30), colour = "black", alpha = 0.2) +
  geom_smooth(method = "loess", aes(y = bumblebee_buzz_hourly/30), colour = "gold") +
  geom_smooth(method = "loess", aes(y = solar_radiation/35), colour = "blue", alpha = 0.2) +
  ylim(0,25) +
  theme_classic()
   
# Plotting variables over summer
ggplot(beeclim_2025, 
       aes(x = time_index)) +
  geom_point(aes(y = temperature_C), colour = "black", alpha = 0.2) +
  geom_point(aes(y = bumblebee_buzz_hourly/30), colour = "orange2", alpha = 0.2) +
  geom_smooth(method = "loess", aes(y = bumblebee_buzz_hourly/30), colour = "gold") +
  ylim(-3,35) +
  theme_classic()


# Identify starting point temperature for Tmin
ggplot(beeclim_2025, 
       aes(x = temperature_C, y = bumblebee_buzz_hourly)) +
  geom_point() +
  geom_smooth(method = "lm", colour = "orange2") +
  ylim(0,1000) +
  xlim(-5,15) +
  theme_classic()

ggplot(beeclim_2025, 
       aes(x = temperature_C, y = bumblebee_buzz_hourly)) +
  geom_point() +
  geom_smooth(method = "lm", colour = "orange2") +
  ylim(0,1000) +
  xlim(-5,20) +
  theme_classic()

ggplot(beeclim_2025, 
       aes(x = temperature_C, y = bumblebee_buzz_hourly)) +
  geom_point() +
  geom_smooth(method = "lm", colour = "orange2") +
  ylim(0,1000) +
  xlim(-5,30) +
  theme_classic()

# Assess correlation factors
cor(beeclim_2025$temperature_C, beeclim_2025$doy)
cor(beeclim_2025$temperature_C, beeclim_2025$solar_radiation)
cor(beeclim_2025$temperature_C, beeclim_2025$time_of_day_hour)
cor(beeclim_2025$temperature_C, beeclim_2025$altitude)
