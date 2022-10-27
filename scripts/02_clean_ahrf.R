# SETUP ----

## 1. Load required packages ----
library(plyr)
library(jsonlite)
library(data.table)
library(tidyverse)
library(tidygeocoder)
library(usmap)
library(here)
library(fs)
library(janitor)
library(tidycensus)
library(labelled)

## 2. Declare `here`
here::i_am("scripts/02_clean_ahrf.R")

## 3. Run Util.R ----
source(here::here("scripts", "util.R"))

## 4. Create necessary directories ----
ensure_directory(here::here("derived_data"))


# 02. AHRF DATA  ----
## Note that this code is almost entirely from jjchern's ahrf repo:
##  https://github.com/mkiang/ahrf/blob/master/data-raw/prep_county.R

## 1. Import sources ----

RAW_SRC <- here::here("source_data",
                      "AHRF_2018-2019",
                      "DATA",
                      "AHRF2019.asc")
DOC_SRC <- here::here(
  "source_data",
  "AHRF_2018-2019",
  "DOC",
  "AHRF 2018-2019 Technical Documentation.xlsx"
)

## 2. Check if file is unzipped ----
if (!fs::file_exists(RAW_SRC)) {
  utils::unzip(here::here("source_data", "AHRF_2018-2019.zip"),
               exdir = here::here("source_data"))
}



## 3. Get the FWF layout ----
bgn_line <- readxl::read_excel(DOC_SRC) %>%
  dplyr::pull(`...1`) %>%
  grepl("F00001", .) %>%
  which()

## 4. Import the excel as a df of FWF info ----
ahrf_county_layout <- readxl::read_excel(
  DOC_SRC,
  col_names = c(
    "field",
    "col_col",
    "year_of_data",
    "var_label",
    "characteristics",
    "source",
    "date_on"
  ),
  skip = bgn_line
) %>%
  dplyr::filter(grepl("^F[0-9]", field)) %>%
  tidyr::separate(col_col, c("col_start", "col_end")) %>%
  dplyr::mutate_at(c("col_start", "col_end"), as.integer)

## 5. Import the county-level AHRF file ----
ahrf_county <- readr::read_fwf(
  file = RAW_SRC,
  col_positions = readr::fwf_positions(
    start = ahrf_county_layout$col_start,
    end = ahrf_county_layout$col_end,
    col_names = ahrf_county_layout$field
  )
)

labelled::var_label(ahrf_county) <- ahrf_county_layout %>%
  dplyr::select(field, var_label) %>%
  tibble::deframe() %>%
  as.list()

## 6. Extract scaling factor ----
ahrf_county_layout <- ahrf_county_layout %>%
  dplyr::mutate(scaling_factor = stringr::str_extract(characteristics, "\\(.[0-1]{1,2}\\)")) %>%
  dplyr::mutate(scaling_factor = as.numeric(gsub("\\(|\\)", "", scaling_factor)))

## 7. Rescale columns ----
for (s in unique(ahrf_county_layout$scaling_factor)) {
  if (!is.na(s)) {
    ahrf_county <- ahrf_county %>%
      dplyr::mutate_at(dplyr::vars(
        ahrf_county_layout %>%
          dplyr::filter(scaling_factor == s) %>%
          dplyr::pull(field)
      ),
      function(x)
        as.numeric(x) * s)
  }
}

## 8. Filter ahrf_county to only include fips codes that start with 06 ----
ahrf_county <- ahrf_county %>%
  dplyr::filter(stringr::str_detect(F00011, "06"))


## 9. Subset data to keep relevant variables ----
## Note: This subset of variables should be build on as the analysis gets deeper.
ahrf_list <- c(
  "fips_st" = "F00011",
  "fips_ct" = "F00012",
  "name" = "F00010",
  "n_pop_2017" = "F13182-17",
  # Health system data
  "n_hospital_beds_2017" = "F08921-17",
  "n_hospitals_2017" = "F08868-17",
  "n_active_mds_2017" = "F08857-17",
  "n_medicare_bene_2017" = "F15288-17",
  "n_medicare_eligible_2018" = "F13191-18",
  #SDoH vars
  "n_people_below_poverty_level_2017" =  "F14419-13",
  # Health data
  "n_deaths_resp_1517" = "F11936-15",
  "n_deaths_resp_1416" = "F11936-14",
  "n_deaths_resp_1315" = "F11936-13"
)

ahrf_subset <- ahrf_county %>%
  dplyr::select(ahrf_list) 

ahrf_subset <- ahrf_subset %>% 
  dplyr::mutate(fips = paste0(fips_st, fips_ct)) %>%
  dplyr::group_by(fips, name) %>%
  dplyr::mutate_at(dplyr::vars(-dplyr::group_cols()), function(x)
    as.numeric(x)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(p_poverty = ((100 * n_people_below_poverty_level_2017) /
                               n_pop_2017)) %>%
  dplyr::select(-fips_st,-fips_ct,-n_people_below_poverty_level_2017) %>%
  dplyr::select(fips,
                name,
                dplyr::everything())

## 10. Write data to directory ----
readr::write_csv(ahrf_subset,
                 here::here("derived_data", "ahrf_subset.csv"))
## Writing data dictionary to directory
readr::write_csv(ahrf_county_layout,
                 here::here("derived_data", "ahrf_dictionary.csv"))
