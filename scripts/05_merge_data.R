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