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
here::i_am("scripts/03_clean_asthma.R")

## 3. Run Util.R ----
source(here::here("scripts", "util.R"))

## 4. Create necessary directories ----
ensure_directory(here::here("derived_data"))

# 03. ASTHMA DATA ----

## Download and reshape the CHHS data on Asthma hospitalization, visits, prevalence.
## https://data.chhs.ca.gov/

## 3.1. Prevalence Data ----
### 1. Download the data ----
curl::curl_download(
  url = "https://data.chhs.ca.gov/dataset/01f456c3-db34-44f2-a52c-6811bef8ba6d/resource/0b88f23c-7781-474f-8687-15bd1f206265/download/lifetime-asthma-prevalence-by-county-2015_2018.xlsx",
  destfile = here::here(
    "source_data",
    "lifetime-asthma-prevalence-by-county-2015_2018.xlsx"
  )
)


### 2. Read in downloaded excel file data ----
raw_df1 <-
  read_excel(
    here::here(
      "source_data",
      "lifetime-asthma-prevalence-by-county-2015_2018.xlsx"
    )
  )

### 3. Clean the data ----
clean_df1 <- raw_df1 %>%
  clean_names() %>%
  filter(strata != "Child vs. adult") %>%
  filter(age_group == "All ages") %>%
  filter(county != "California") %>% 
  dplyr::select(-c(
    counties_grouped,
    x95_percent_confidence_interval,
    comment,
    strata,
    age_group
  )) %>%
  rename(asthma_prevalence = lifetime_prevalence) 

### 4. Pivot the data from long to wide ----
clean_df1 <- clean_df1 %>%
  pivot_wider(names_from = years,
              values_from = asthma_prevalence) %>%
  clean_names() %>%
  rename(asthma_prev_1516 = x2015_2016, asthma_prev_1718 = x2017_2018)


## 3.2. Death Data ----
### 1. Download the data ----
curl::curl_download(
  url = "https://data.chhs.ca.gov/dataset/5ea28f96-7bb6-4c18-9c0d-020484fab181/resource/0fe2c650-e4e2-4de8-b2aa-5a14e01e9e61/download/asthma-deaths-by-county-2014_2019.csv",
  destfile = here::here("source_data",
                        "asthma-deaths-by-county-2014_2019.csv")
)

### 2. Read in downloaded csv file----
raw_df2 <-
  read_csv(
    here::here("source_data",
               "asthma-deaths-by-county-2014_2019.csv"),
    locale = locale(encoding = "UTF-8")
  )

### 3. Clean the data ----
clean_df2 <- raw_df2 %>%
  clean_names() %>%
  filter(strata != "Child vs. adult") %>%
  filter(age_group == "All ages") %>%
  filter(county != "California") %>% 
  dplyr::select(-c(comment, strata, age_group, number_of_deaths))

### Remove the \x96 string from years column
clean_df2$years <- gsub("\x96", "-", clean_df2$years)

### 4. Pivot the data from long to wide ----
clean_df2 <- clean_df2 %>%
  pivot_wider(names_from = years,
              values_from = age_adjusted_mortality_rate) %>%
  clean_names() %>%
  rename(asthma_mort_1416 = x2014_2016, asthma_mort_1719 = x2017_2019)


## 3.3. ED Visit Data ----
### 1. Download the data ----
curl::curl_download(
  url = "https://data.chhs.ca.gov/dataset/34f3464e-b2eb-4f74-9ef9-f378711aa0f5/resource/94d84508-8046-40a7-b8d9-0ed73b13b697/download/asthma-emergency-department-visit-rates-by-county-2015_2019.csv",
  destfile = here::here(
    "source_data",
    "asthma-emergency-department-visit-rates-by-county-2015_2019.csv"
  )
)

### 2. Read in downloaded csv file----
raw_df3 <-
  read_csv(
    here::here(
      "source_data",
      "asthma-emergency-department-visit-rates-by-county-2015_2019.csv"
    ),
    locale = locale(encoding = "UTF-8")
  )

### 3. Clean the data ----
clean_df3 <- raw_df3 %>%
  clean_names() %>%
  filter(strata == "Total population") %>%
  filter(age_group == "All ages") %>%
  filter(county != "California") %>%
  dplyr::select(-c(comment, strata, age_group, number_of_ed_visits, strata_name)) %>%
  rename(years = year)

### 4. Pivot the data from long to wide ----
clean_df3 <- clean_df3 %>%
  pivot_wider(names_from = years,
              values_from = age_adjusted_ed_visit_rate) %>%
  clean_names() 

colnames(clean_df3) <- gsub("x", "asthma_edrate_", colnames(clean_df3))

## 3.4. Merged Asthma Dataset ----

### 1. Left Join all data ----
asthma_df <- clean_df1 %>%
  left_join(clean_df2, by = "county") %>%
  left_join(clean_df3, by = "county")

### 2. Only retain ED rates and pivot to Long ----
asthma_df <- asthma_df %>%
  pivot_longer(
    cols = c(
      "asthma_edrate_2015", "asthma_edrate_2016", "asthma_edrate_2017", "asthma_edrate_2018", "asthma_edrate_2019"),
               names_to = "year",
               values_to = "ed_rate")

asthma_df$year <- gsub("asthma_edrate_", "", asthma_df$year)

asthma_df <- asthma_df[,-c(2:5)]


### 2. Write data to directory ----
readr::write_csv(asthma_df,
                 here::here("derived_data", "asthma_df.csv"))
