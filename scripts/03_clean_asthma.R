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
setwd("geog567-fall22")
here::i_am("scripts/03_clean_asthma.R")

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

utils::download.file(url = "https://data.chhs.ca.gov/dataset/01f456c3-db34-44f2-a52c-6811bef8ba6d/resource/0b88f23c-7781-474f-8687-15bd1f206265/download/lifetime-asthma-prevalence-by-county-2015_2018.xlsx",
                     destfile = here::here(
                       "source_data",
                       basename(
                         "https://data.chhs.ca.gov/dataset/01f456c3-db34-44f2-a52c-6811bef8ba6d/resource/0b88f23c-7781-474f-8687-15bd1f206265/download/lifetime-asthma-prevalence-by-county-2015_2018.xlsx"
                       )
                     ))

## 2. Load and clean data ----
raw_df <- readxl::read_excel(
  here::here(
    "source_data",
    "lifetime-asthma-prevalence-by-county-2015_2018.xlsx"
  )
)

# ## 3. Write population data to directory ----
# readr::write_csv(pop_df,
#                  here::here("derived_data", "asthma_prevalence.csv"))
