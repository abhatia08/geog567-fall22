# SETUP ----

## 1. Load required packages ----
library(plyr)
library(data.table)
library(tidyverse)
library(here)
library(fs)
library(janitor)
library(labelled)
library(tidycensus)
library(readxl)

## 2. Declare `here`
here::i_am("scripts/05_merge_data.R")

## 3. Run Util.R ----
source(here::here("scripts", "util.R"))

## 4. If the "derived data" folder doesn't exist, source scripts 01-04 ----
if (!dir.exists(here::here("derived_data"))) {
  source(here::here("scripts", "01_clean_pop.R"))
  source(here::here("scripts", "02_clean_ahrf.R"))
  source(here::here("scripts", "03_clean_asthma.R"))
}

# 01. MERGING CLEANED DATA ----

## 1. Load the cleaned data files ----

pop_df <- read_csv(here::here("derived_data", "pop_data.csv"))

ahrf_df <- read_csv(here::here("derived_data", "ahrf_subset.csv"))

asthma_df <- read_csv(here::here("derived_data", "asthma_df.csv"))

gee_df <-
  read_csv(here::here("derived_data", "GEE_county_data_11082022.csv"))

## 2. Left join everything to GEE data
## Renaming to make it easier
gee_df <-
  gee_df %>% rename(fips = upd_geoid) %>% filter(year > 2014)
asthma_df <- asthma_df %>% rename (name = county)

analytic_df <- gee_df %>% left_join(ahrf_df, by = c("fips"))

analytic_df <- analytic_df %>% left_join(pop_df, by = c("fips"))


analytic_df <-
  analytic_df %>% left_join(asthma_df, by = c("year", "name"))

## 3. Sort Columns ----
analytic_df <-
  analytic_df %>% dplyr::select(
    year,
    fips,
    name,
    ed_rate,
    mean_days,
    burned_ratio,
    mean_intensity,
    mean_aerosol,
    n_pop_2018,
    p_hosp_permil,
    p_bedsper1000,
    p_mds1000,
    p_medicare_bene1000,
    p_medicare_elig1000,
    p_poverty,
    p5younger,
    p65older,
    p_nonwhite
  )



## 4. Write to directory ----
readr::write_csv(analytic_df,
                 here::here("derived_data", "analytic_df.csv"))
