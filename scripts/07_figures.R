# SETUP ----

## 1. Load required packages ----
library(data.table)
library(tidyverse)
library(here)
library(fs)
library(labelled)
library(patchwork)
library(ggpubr)
library(sf)
library(randomForest)
library(caret)


## 2. Declare `here` ----
here::i_am("scripts/07_analysis.R")

## 3. Run Util.R ----
source(here::here("scripts", "util.R"))

## 4. If the "predictions.csv" file doesn't exist, source scripts 06.analysis.R ----
if (!file.exists(here::here("derived_data", "predictions.csv"))) {
  source(here::here("scripts", "06_analysis.R"))
} 

## 5. Load data ----
df <- read_csv(here::here("derived_data", "predictions.csv"))
counties <- st_read(here::here("source_data", "CA_counties", "CA_counties_TIGER2016.shp"))
xgb_model <- readRDS(here::here("derived_data", "xgboostmodel.RDS"))
load(here::here("derived_data", "rf_model.RData"))

# 07. FIGURES ----

## 1. Feature Importance ----
xgb_features <- caret::varImp(xgb_model)

png(here::here("figures", "xgbfeatureimp.png"),
    width = 600,
    height = 600)
plot(xgb_features, top = 13)
dev.off()

png(here::here("figures", "rffeatureimp.png"),
    width = 600,
    height = 600)
randomForest::varImpPlot(rf_final_model)
dev.off()

## 2. Create a scatterplot of edrate_actual vs. edrate_predicted ----
scatterplot <- ggplot(df, aes(x = edrate_actual, y = edrate_pred)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  theme_bw() +
  labs(x = "Actual ED Rate", y = "Predicted ED Rate") +
  theme(legend.position = "none") 

scatterplot

## 3. Create county map of predicted ED rate in 2020 against actual ED rate for 2019 ----
### 3a. Create a data frame with the county name, fips code, edrate_actual for only the year 2019 ----
df_2019 <- df %>%
  filter(year == 2019) %>%
  dplyr::select(c(name, fips, edrate_actual)) %>%
  rename(edrate_actual_2019 = edrate_actual)

### 3b. Create a data frame with the county name, fips code, edrate_predicted for only the year 2020 ----
df_2020 <- df %>%
  filter(year == 2020) %>%
  dplyr::select(c(name, fips, edrate_pred)) %>%
  rename(edrate_pred_2020 = edrate_pred)

### 3c. Merge the two data frames together ----
df_2019_2020 <- merge(df_2019, df_2020, by = c("fips", "name"))

### 3d. Merge the data frame with the county shapefile ----
df_2019_2020 <- counties %>%
  left_join(df_2019_2020, by = c("GEOID" = "fips"))

### 3c. Plot maps ----
actual_map <- ggplot(df_2019_2020, aes(fill = edrate_actual_2019)) +
  geom_sf() +
  scale_fill_viridis_c(option = "magma") +
  theme_bw() +
  labs(title = "Actual ED Rate in 2019", x = "", y = "") +
  theme(legend.position = c(0.85, 0.8)) +
  theme(legend.background = element_blank(),
        legend.title = element_blank()) 

### Create a choropleth map of predicted ED rate in 2020 
pred_map <- ggplot(df_2019_2020, aes(fill = edrate_pred_2020)) +
  geom_sf() +
  scale_fill_viridis_c(option = "magma") +
  theme_bw() +
  labs(title = "Predicted ED Rate in 2020", x = "", y = "") +
  theme(legend.position = c(0.85, 0.8)) +
  theme(legend.background = element_blank(),
        legend.title = element_blank()) 

actual_map + pred_map

### 3d. Save combined plot in ---- 
ggsave(here::here("figures", "2019vs2020pred.png"), width = 12, height = 8)

## 4. Choropleth of all SDoH variables
### 4a. Create a data frame with the relevant vars  ----
df_mean <- df %>%
  group_by(fips, name) %>%
  summarise(p_hosp_permill = mean(p_hosp_permil),
            p_bedsper1000 = mean(p_bedsper1000),
            p_mds1000 = mean(p_mds1000),
            p_medicare_bene1000 = mean(p_medicare_bene1000),
            p_poverty = mean(p_poverty),
            p_nonwhite = mean(p_nonwhite))

### 4b. Merge the data frame with the county shapefile ----
df_mean <- counties %>%
  left_join(df_mean, by = c("GEOID" = "fips"))

### 4c. Plot maps ----
hosp_map <- ggplot(df_mean, aes(fill = p_hosp_permill)) +
  geom_sf() +
  scale_fill_viridis_c(option = "magma") +
  theme_bw() +
  labs(title = "Hospitals per million individuals", x = "", y = "") +
  theme(legend.position = c(0.85, 0.8)) +
  theme(legend.background = element_blank(),
        legend.title = element_blank()) +
  theme(plot.title = element_text(size = 10))

beds_map <- ggplot(df_mean, aes(fill = p_bedsper1000)) +
  geom_sf() +
  scale_fill_viridis_c(option = "magma") +
  theme_bw() +
  labs(title = "Hospital Beds per 1000 individuals", x = "", y = "") +
  theme(legend.position = c(0.85, 0.8)) +
  theme(legend.background = element_blank(),
        legend.title = element_blank()) +
  theme(plot.title = element_text(size = 10))

mds_map <- ggplot(df_mean, aes(fill = p_mds1000)) +
  geom_sf() +
  scale_fill_viridis_c(option = "magma") +
  theme_bw() +
  labs(title = "MDs per 1000 individuals", x = "", y = "") +
  theme(legend.position = c(0.85, 0.8)) +
  theme(legend.background = element_blank(),
        legend.title = element_blank()) +
  theme(plot.title = element_text(size = 10))

medicare_map <- ggplot(df_mean, aes(fill = p_medicare_bene1000)) +
  geom_sf() +
  scale_fill_viridis_c(option = "magma") +
  theme_bw() +
  labs(title = "Medicare Beneficiaries per 1000 individuals", x = "", y = "") +
  theme(legend.position = c(0.85, 0.8)) +
  theme(legend.background = element_blank(),
        legend.title = element_blank()) +
  theme(plot.title = element_text(size = 10))

poverty_map <- ggplot(df_mean, aes(fill = p_poverty)) +
  geom_sf() +
  scale_fill_viridis_c(option = "magma") +
  theme_bw() +
  labs(title = "% of Population below Poverty", x = "", y = "") +
  theme(legend.position = c(0.85, 0.8)) +
  theme(legend.background = element_blank(),
        legend.title = element_blank()) +
  theme(plot.title = element_text(size = 10))

nonwhite_map <- ggplot(df_mean, aes(fill = p_nonwhite)) +
  geom_sf() +
  scale_fill_viridis_c(option = "magma") +
  theme_bw() +
  labs(title = "% of Population that is Non-white", x = "", y = "") +
  theme(legend.position = c(0.85, 0.8)) +
  theme(legend.background = element_blank(),
        legend.title = element_blank()) +
  theme(plot.title = element_text(size = 10))



hosp_map + beds_map + mds_map + medicare_map + poverty_map + nonwhite_map

### 4d. Save combined plot in ---- 
ggsave(here::here("figures", "all_sdoh_maps.png"), width = 12, height = 8)

