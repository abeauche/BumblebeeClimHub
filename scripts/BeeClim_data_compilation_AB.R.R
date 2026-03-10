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
library(data.table)
library(hms)

# Add site coordinates
lat <- 69.583
lon <- -139.03

# Load raw datasets



#### PART 1: Compiling bumblebee recognizer predictions ----

## Load raw datasets
ARUQ0_2024_pred_raw <- read_csv("/Volumes/IGUTCHAQ/projects/BumblebeeClim/data/raw/2024/QHI24_ARUQ00_pred.csv")
ARUQ0_2025_pred_raw <- read_csv("/Volumes/IGUTCHAQ/projects/BumblebeeClim/data/raw/2025/QHI25_ARUQ00_pred.csv")
location_mapping <- read_csv("/Volumes/IGUTCHAQ/projects/BumblebeeClim/data/raw/location_mapping_TundraBUZZ.csv")

## Merge prediction datasets
# List ARUQ prediction datasets
ARUQ_list <- mget(ls(pattern = "ARUQ\\d+_202\\d_pred_raw"))

# Bind datasets together
ARUQ_pred_raw <- bind_rows(ARUQ_list)
rm(list = ls(pattern = "ARUQ\\d+_202\\d_pred_raw")) # remove individual datasets to reduce memory use
rm(ARUQ_list)
head(ARUQ_pred_raw)

## Clean ARUQ_pred_raw dataset
# Extract aru_id and clean file structure naming
setDT(ARUQ_pred_raw)
ARUQ_pred_raw[, file := sub("^.*[\\\\/]", "", file)] # remove unneeded file structure
ARUQ_pred_raw[, datetime := stringr::str_extract(file, "\\d{8}_\\d{6}")] # extract datetime string
is_aruq <- grepl("^ARUQ", ARUQ_pred_raw$file) # create list of files named with ARUQ structure
ARUQ_pred_raw[is_aruq, aru_number := as.integer(sub("ARUQ(\\d+).*", "\\1", file))] # extract aru number from ARUQ files
ARUQ_pred_raw[!is_aruq, aru_number := as.integer(sub(".*-(\\d+)_.*", "\\1", file))] # extract aru number from QHI25- files
ARUQ_pred_raw[, aru_id := paste0("ARUQ", aru_number)] # create consistent aru_id
ARUQ_pred_raw <- ARUQ_pred_raw[!aru_id %in% c("ARUQ18", "ARUQ28")] # filter out incorrect aru_ids
ARUQ_pred_raw[, aru_id := factor(aru_id)] # change aru_id to factor

# Convert datetime to POSIXct
ARUQ_pred_raw[, datetime := ISOdatetime(
  year = as.integer(substr(datetime,1,4)),
  month = as.integer(substr(datetime,5,6)),
  day = as.integer(substr(datetime,7,8)),
  hour = as.integer(substr(datetime,10,11)),
  min = as.integer(substr(datetime,12,13)),
  sec = as.integer(substr(datetime,14,15)),
  tz = "America/Whitehorse"
)] ### This step is taking a while

# Save as data.frame
ARUQ_pred_df <- as.data.frame(ARUQ_pred_raw)
head(ARUQ_pred_df)

# Merge to replace aru_id with location_id
ARUQ_pred_df_mapped <- ARUQ_pred_df %>%
  left_join(location_mapping, by = "aru_id") %>% # BEEBOX corresponds to the bumblebee nest location
  select(-c(aru_id, polcam_id,tomst_id,site))  # remove aru_id, now using location_id

# Remove obsolete objects 
rm(ARUQ_pred_raw)
rm(ARUQ_pred_df)

# Mutate datetime to POSIXct format
ARUQ_pred_df_mapped_datetime <- ARUQ_pred_df_mapped %>% 
  mutate(datetime = as.POSIXct(datetime, format="%Y%m%d_%H%M%S", tz="America/Whitehorse")  # Convert to POSIXct
  )

# Remove obsolete objects 
rm(ARUQ_pred_df_mapped_datetime)

# Save compiled dataset as .csv
# write_csv(ARUQ_pred_df_mapped, "/Volumes/IGUTCHAQ/projects/BumblebeeClim/data/raw/2025/QHI_BEEBOX_pred_clean.csv")
# write_csv(ARUQ_pred_df_mapped_datetime, "/Volumes/IGUTCHAQ/projects/BumblebeeClim/data/raw/2025/QHI_BEEBOX_pred_datetime_clean.csv")



#### PART 2: Aggregate and filter bumblebee detections ----

# ARUQ_pred_df_mapped_datetime <- read_csv("/Volumes/IGUTCHAQ/projects/BumblebeeClim/data/raw/2025/QHI_BEEBOX_pred_datetime_clean.csv")

# Change timezone to show local time
attr(ARUQ_pred_df_mapped_datetime$datetime, "tzone") <- "America/Whitehorse"

# Define threshold and filter data
threshold_1 <- 1.74 # threshold optimized using recognier scores
threshold_2 <- 8 # high precision score

# Aggregate number of detections per recording period according to both thresholds
setDT(ARUQ_pred_df_mapped_datetime)
ARUQ_bumblebee_detections <- ARUQ_pred_df_mapped_datetime[, .(
  detections_above_th1 = sum(BUZZ > threshold_1),
  detections_above_th2 = sum(BUZZ > threshold_2)
), by = .(location_id, datetime)]

# Remove obsolete objects
rm(ARUQ_pred_df_mapped_datetime)

# Write .csv
# write_csv(ARUQ_bumblebee_detections, "/Volumes/IGUTCHAQ/projects/BumblebeeClim/data/raw/2025/ARUQ_bumblebee_detections.csv")

# Aggregate bumblebee detections per hour, filter for beebox
hourly_BEEBOX_detections <- ARUQ_bumblebee_detections %>%
  filter(location_id == "BEEBOX") %>%
  mutate(datetime = floor_date(datetime, "hour")) %>%  # round down to start of the hour
  group_by(location_id, datetime) %>%
  summarise(
    detections_above_th1 = sum(detections_above_th1, na.rm = TRUE),
    detections_above_th2 = sum(detections_above_th2, na.rm = TRUE),
    .groups = "drop"
  )

# Create time_of_day column with local time
hourly_BEEBOX_detections <- hourly_BEEBOX_detections %>%
  mutate(
    time_of_day = as_hms(datetime)  # extract the time (HH:MM:SS) from datetime
  )

# Write .csv
# write_csv(hourly_BEEBOX_detections, "/Volumes/IGUTCHAQ/projects/BumblebeeClim/data/raw/2025/hourly_BEEBOX_detections.csv")


#### PART 3: Combine bumblebee detections with climate data ----



#### In progress ----
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
