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

## 2. Declare `here`
here::i_am("scripts/01_clean_pop.R")

## 3. Run Util.R ----
source(here::here("scripts", "util.R"))

## 4. Create necessary directories ----
ensure_directory(here::here("derived_data"))

# 01. POPULATION DATA (Percent non-white Hispanic + Percent white) ----

## Download and reshape the 2018 NCHS bridged race population file. 
## https://www.cdc.gov/nchs/nvss/bridged_race/data_documentation.htm#vintage2018%20

## Note that the code to clean and reshape this data is based on mkiang's repo:
## https://github.com/mkiang/county_preparedness/blob/master/code/01_get_population_data.R

## 1. Download the data ----

utils::download.file(url = "https://www.cdc.gov/nchs/nvss/bridged_race/pcen_v2018_y18.txt.zip",
                     destfile = here::here(
                       "source_data",
                       basename(
                         "https://www.cdc.gov/nchs/nvss/bridged_race/pcen_v2018_y18.txt.zip"
                       )
                     ))

## 2. Reshape the 2018 NCHS bridged race population file ----
orig_pop_df <-
  readr::read_fwf(here::here(
    "source_data",
    basename(
      "https://www.cdc.gov/nchs/nvss/bridged_race/pcen_v2018_y18.txt.zip"
    )
  ),
  readr::fwf_widths(
    c(4, 4, 1, 5, 2, 1, 1, 8),
    c(
      "series",
      "year",
      "month",
      "fips",
      "age",
      "racesex",
      "hispanic",
      "pop"
    )
  ))  %>%
  dplyr::filter(year == 2018, month == 7)

pop_df <- orig_pop_df %>%
  dplyr::select(age, fips, pop) %>%
  dplyr::mutate(age = (cut(
    age,
    c(0, seq(5, 85, 5), Inf),
    include.lowest = TRUE,
    right = FALSE,
    labels = FALSE
  ) - 1) * 5) %>%
  dplyr::group_by(fips, age) %>%
  dplyr::summarize(pop = sum(pop)) %>%
  dplyr::ungroup()

## 3. Write population data to directory ----
readr::write_csv(pop_df,
                 here::here("derived_data", "population_by_age.csv"))

## 4. Get percentage of non-NHW population ----
nhw_df <- orig_pop_df %>%
  dplyr::select(age, fips, pop, racesex, hispanic) %>%
  dplyr::filter(racesex %in% 1:2, hispanic == 1) %>%
  dplyr::mutate(age = (cut(
    age,
    c(0, seq(5, 85, 5), Inf),
    include.lowest = TRUE,
    right = FALSE,
    labels = FALSE
  ) - 1) * 5) %>%
  dplyr::group_by(fips, age) %>%
  dplyr::summarize(nhw_pop = sum(pop)) %>%
  dplyr::ungroup()

nonwhite_df <- pop_df %>%
  dplyr::left_join(nhw_df) %>%
  dplyr::group_by(fips) %>%
  dplyr::summarize(pop = sum(pop),
                   nhw_pop = sum(nhw_pop)) %>%
  dplyr::transmute(
    fips = fips,
    p_nonwhite = (pop - nhw_pop) / pop * 100,
    p_white = nhw_pop / pop * 100
  )
## 5. Write data to directory ----

readr::write_csv(nonwhite_df,
                 here::here("derived_data",
                            "percent_nonwhite_pop.csv"))




