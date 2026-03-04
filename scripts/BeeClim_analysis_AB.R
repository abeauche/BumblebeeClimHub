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

# Load compiled datasets
beeclim_2025 <- read_csv("/Volumes/IGUTCHAQ/projects/BumblebeeClim/data/clean/QHI_bumblebee_nest_2025.csv")


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

