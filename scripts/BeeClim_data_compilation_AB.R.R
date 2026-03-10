# ====================================================
# Script Name: BeeClim_data_compilation_AB.R
# Project: BumblebeeClim 2025-26
# Author: Alex Beauchemin
# Date Created: 2026-03-04
# Last Modified: 2026-03-09
# Description: This script compiles dataset relevant to the BumblebeeClim analysis.
# Dependencies: 
# ====================================================

#### SETUP ----
# Load required libraries
library(tidyverse)
library(suncalc)

# Add site coordinates
lat <- 69.583
lon <- -139.03

# Load compiled datasets
beeclim_2025 <- read_csv("/Volumes/IGUTCHAQ/projects/BumblebeeClim/data/clean/QHI_bumblebee_nest_2025.csv")

# Calculate sun altitude
sun_pos <- getSunlightPosition(
  date = beeclim_2025$datetime_utc,
  lat = lat,
  lon = lon
)

# sun_pos$altitude gives altitude in radians, convert to degrees
sun_pos$altitude <- sun_pos$altitude * (180/pi)

# Join with beeclim_2025
beeclim_2025 <- beeclim_2025 %>%
  left_join(sun_pos, by = c("datetime_utc"="date"))

# Save dataset
write_csv(beeclim_2025, "./data/clean/beeclim_2025.csv")
