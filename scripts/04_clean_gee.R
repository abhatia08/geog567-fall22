# SETUP ----

## 1. Load required packages ----
library(plyr)
library(jsonlite)
library(data.table)
library(tidyverse)
library(ggplot2)
library(sf)
library(tmap)
library(labelled)

## 2. Declare `here`
here::i_am("scripts/04_clean_GEE.R")

## 3. Run Util.R ----
source(here::here("scripts", "util.R"))

## 4. Create necessary directories ----
ensure_directory(here::here("derived_data"))

# 04. GEE Data ----

## 1. Read in data ----
## County fire average days data
county_data_days <-
  read_csv(here::here("source_data", "GEE_output", "FireDaysAverage_10252022.csv"))

## County average ratio burned
county_data_burnedarea <-
  read_csv(here::here(
    "source_data",
    "GEE_output",
    "CountyBurnAverage_10252022.csv"
  ))

## County fire average intensity data
county_data_intensity <-
  read_csv(here::here(
    "source_data",
    "GEE_output",
    "County_Fire_Intensity_10242022.csv"
  ))

## Aerosol data
county_aerosol_data <-
  read_csv(
    here::here(
      "source_data",
      "GEE_output",
      "MCD_Aerosol_Optical_Depth_11032022.csv"
    )
  )

## County shapefile
CA_county <-
  st_read(here::here("source_data", "CA_counties", "CA_Counties_TIGER2016.shp"))

## 2. Clean data ----
county_data_days <- rename(county_data_days, mean_days = mean)
county_data_burnedarea <-
  rename(county_data_burnedarea, burned_ratio = mean)
county_data_intensity <-
  rename(county_data_intensity, mean_intensity = mean)
county_aerosol_data <-
  rename(county_aerosol_data, mean_aerosol = mean)

merged_data_fire <-
  merge(county_data_days,
        county_data_burnedarea,
        by = c("GEOID", "year"))
merged_data_fire <-
  merge(merged_data_fire, county_data_intensity, by = c("GEOID", "year"))
merged_data_full <-
  merge(merged_data_fire, county_aerosol_data, by = c("GEOID", "year"))
merged_data_full$upd_geoid <-
  paste0("0", as.character(merged_data_full$GEOID))
merged_data_full <- merged_data_full[, 2:7]

## 3. Write data to directory ----
readr::write_csv(merged_data_full,
                 here::here("derived_data", "GEE_county_data_11082022.csv"))

## 4. Plot data ----
grouped_data <-
  merged_data_full %>% group_by(upd_geoid) %>% summarise(
    mean_days = mean(mean_days),
    mean_intensity = mean(mean_intensity),
    mean_aerosol = mean(mean_aerosol),
    burned_ratio = mean(burned_ratio)
  )

CA_county_merged <-
  merge(CA_county, grouped_data, by.x = "GEOID", by.y = "upd_geoid")

tmap_mode("view")

tm_shape(CA_county_merged) + tm_polygons("burned_ratio", style = "jenks")
