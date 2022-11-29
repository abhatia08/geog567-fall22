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
library(RColorBrewer)

## 2. Declare `here` ----
here::i_am("scripts/07_figures.R")

## 3. Run Util.R ----
source(here::here("scripts", "util.R"))

## 4. If the "predictions.csv" file doesn't exist, source scripts 06.analysis.R ----
if (!file.exists(here::here("derived_data", "predictions.csv"))) {
  source(here::here("scripts", "06_analysis.R"))
}

## 5. Load data ----
analytic_df <-
  read_csv(here::here("derived_data", "predictions.csv"))

counties <-
  st_read(here::here("source_data", "CA_counties", "CA_counties_TIGER2016.shp"))

xgb_model <- readRDS(here::here("derived_data", "xgboostmodel.RDS"))
load(here::here("derived_data", "rf_model.RData"))

# 07. FIGURES ----

## 1. Feature Importance ----

## 1.1 Export xgb feature importance
xgb_features <- caret::varImp(xgb_model)
features <- as.data.frame(xgb_features$importance)
## Rename to xgb_imp
colnames(features) <- "xgb_imp"
## convert rownames to column
features$feature <- rownames(features)

## 1.2. Export rf feature importance
rf_features <- as.data.frame(varImp(rf_final_model))
## Rename to rf_imp
colnames(rf_features) <- "rf_imp"
## convert rownames to column
rf_features$feature <- rownames(rf_features)

## 1.3 Wrangling
# Left join to get both xgb and rf feature importance
features <-
  left_join(features, rf_features, by = "feature")
features <- features %>% select (c("feature", "xgb_imp", "rf_imp"))
# Convert to long format
features <-
  features %>% gather(key = "model", value = "importance",-feature)
# Rename xgb_imp to XGBoost and rf_imp to Random Forest
features$model <-
  recode(features$model, "xgb_imp" = "XGBoost", "rf_imp" = "Random Forest")

## Create feature categories
features <- features %>%
  mutate(category = ifelse(
    feature %in% c("p65older", "p5younger", "p_poverty", "p_nonwhite"),
    "Demographics",
    ifelse(
      feature %in% c(
        "p_medicare_elig1000",
        "p_medicare_bene1000",
        "p_mds1000",
        "p_hosp_permil",
        "p_bedsper1000"
      ),
      "Health System",
      "Remote Sensing"
    )
  ))

## Rename features to more readable names
features$feature <-
  recode(
    features$feature,
    "p65older" = "% Population over 65 years",
    "p5younger" = "% Population below 5 years",
    "p_poverty" = "% Population below poverty",
    "p_nonwhite" = "% Population that is Non-white",
    "p_medicare_elig1000" = "# Medicare eligible per 1000 individuals",
    "p_medicare_bene1000" = "# Medicare beneficiaries per 1000 individuals",
    "p_mds1000" = "# MDs per 1000 individuals",
    "p_hosp_permil" = "# Hospitals per million individuals",
    "p_bedsper1000" = "# Hospital beds per 1000 individuals",
    "mean_intensity" = "Intensity of burning (Mean)",
    "mean_aerosol" = "Aerosol Optical Depth (Mean)",
    "mean_days" = "Number of days with fire (Mean)",
    "burned_ratio" = "Ratio of burned area to total area (mean)"
  )

## Compute average imporatnce for each feature (for plotting)
features <- features %>%
  group_by(feature, category) %>%
  summarise(average_importance = mean(importance)) %>%
  ungroup() %>%
  left_join(features, by = c("feature", "category"))


## 1.4 Plot Feature importance

feature_imp <- features %>%
  ggplot(aes(x = importance, y = feature, color = model)) +
  geom_point(position = position_dodge(0.5)) +
  geom_linerange(aes(xmin = 0, xmax = importance),
                 linetype = "solid",
                 position = position_dodge(.5)) +
  geom_vline(xintercept = 0,
             linetype = "solid",
             color = "grey70") +
  scale_x_continuous(limits = c(-5, 100)) +
  labs(x = "Importance", y = "Feature", color = "Model") +
  theme_bw() +
  theme(legend.position = "bottom",
        text = element_text(family = "serif")) +
  scale_color_manual(values = c("XGBoost" = "#2E86AB", "Random Forest" = "#ED6A5A")) +
  guides(color = guide_legend(title = NULL)) +
  scale_y_discrete(limits = features$feature[order(features$average_importance, decreasing = FALSE)])

## 1.5 Save plot
png(
  here::here("figures", "feature_importance.png"),
  width = 8,
  height = 8,
  units = "in",
  res = 300
)
feature_imp
dev.off()


## 2. Scatterplot of edrate_actual vs. edrate_predicted ----

## Create a DF with actual and predicted values for only 2015-2019 and drop counties with missing values or 0 ED visits
df_scatter <- analytic_df %>%
  filter(year %in% 2015:2019) %>%
  filter(!is.na(edrate_actual)) %>%
  filter(edrate_actual != 0)

## Calculate RMSE for scatterplot, ignoring NAs (XGB)
rmse_xgb <-
  sqrt(mean((
    df_scatter$edrate_actual - df_scatter$edrate_pred
  ) ^ 2,
  na.rm = TRUE))

## Calculate RMSE for scatterplot, ignoring NAs (RF)
rmse_rf <-
  sqrt(mean((
    df_scatter$edrate_actual - df_scatter$edrate_pred_rf
  ) ^ 2,
  na.rm = TRUE))


## Scatterplot of actual vs. predicted with RMSE

scatter_xgb <- df_scatter %>%
  ggplot(aes(x = edrate_actual, y = edrate_pred)) +
  geom_point(alpha = 0.5, color = "#2E86AB") +
  geom_abline(intercept = 0,
              slope = 1,
              color = "black") +
  labs(
    x = "Observed ED Rate",
    y = "Predicted ED Rate",
    title = "Plot of Observed vs. Predicted ED Rates (XGBoost Model)"
  ) +
  theme_bw() +
  theme(text = element_text(family = "serif"),
        plot.title = element_text(hjust = 0.5)) +
  annotate(
    "text",
    x = 60,
    y = 30,
    label = paste0("RMSE = ", round(rmse_xgb, 2)),
    size = 5,
    family = "serif"
  ) +
  scale_x_continuous(breaks = seq(0, max(df_scatter$edrate_actual), by = 20),
                     limits = c(0, max(df_scatter$edrate_actual))) +
  scale_y_continuous(breaks = seq(0, max(df_scatter$edrate_pred), by = 20),
                     limits = c(0, max(df_scatter$edrate_pred)))

## Scatterplot of actual vs. predicted with RMSE (RF)
scatter_rf <- df_scatter %>%
  ggplot(aes(x = edrate_actual, y = edrate_pred_rf)) +
  geom_point(alpha = 0.5, color = "#ED6A5A") +
  geom_abline(intercept = 0,
              slope = 1,
              color = "black") +
  labs(
    x = "Observed ED Rate",
    y = "Predicted ED Rate",
    title = "Plot of Observed vs. Predicted ED Rates (Random Forest)"
  ) +
  theme_bw() +
  theme(text = element_text(family = "serif"),
        plot.title = element_text(hjust = 0.5)) +
  annotate(
    "text",
    x = 60,
    y = 30,
    label = paste0("RMSE = ", round(rmse_rf, 2)),
    size = 5,
    family = "serif"
  ) +
  scale_x_continuous(breaks = seq(0, max(df_scatter$edrate_actual), by = 20),
                     limits = c(0, max(df_scatter$edrate_actual))) +
  scale_y_continuous(breaks = seq(0, max(df_scatter$edrate_pred), by = 20),
                     limits = c(0, max(df_scatter$edrate_pred)))

## Save plot
png(
  here::here("figures", "scatterplot_compare.png"),
  width = 12,
  height = 8,
  units = "in",
  res = 300
)

(scatter_rf | scatter_xgb)

dev.off()

## Clear memory before mapping except for analytic_df and counties
rm(list = ls()[!ls() %in% c("analytic_df", "counties")])


## 3. Choropleth of predicted ED rate in 2020 against actual ED rates for 2015-2019 ----
### 3a. Create a facet plot of actual values
### Subset analytic_df to only include 2015-2019
df_actual <- analytic_df %>%
  filter(year %in% 2015:2019) %>%
  mutate(edrate_actual = ifelse(edrate_actual == 0, NA, edrate_actual)) %>%  # This is done to stretch the palette.
  select(c("fips", "name", "year", "edrate_actual")) %>%
  left_join(counties, by = c("fips" = "GEOID"))


### Create a facet wrap by year, plotting actual ED rates using ggplot geom_sf
actual_plot <- df_actual %>%
  ggplot(aes(fill = edrate_actual, geometry = geometry)) +
  geom_sf() +
  facet_wrap(~ year, ncol = 2) +
  labs(title = "Observed",
       fill = "ED Rate") +
  theme_bw() +
  theme(
    text = element_text(family = "serif"),
    plot.title = element_text(hjust = 0.5),
    legend.position = "none"
  ) +
  scale_fill_viridis_c(option = "viridis",
                       direction = -1,
                       na.value = "grey95") +
  guides(fill = guide_colorbar(title.position = "top")) +
  coord_sf(datum = NA)

### 3b. Create a plot of predicted values in 2020
### Subset analytic_df to only include 2015-2019
df_pred <- analytic_df %>%
  filter(year == 2020) %>%
  mutate(edrate_pred = ifelse(edrate_pred == 0, NA, edrate_pred)) %>%  # This is done to stretch the palette.
  select(c("fips", "name", "year", "edrate_pred")) %>%
  left_join(counties, by = c("fips" = "GEOID"))

### Create a facet wrap by year, plotting actual ED rates using ggplot geom_sf
pred_plot <- df_pred %>%
  ggplot(aes(fill = edrate_pred, geometry = geometry)) +
  geom_sf() +
  labs(title = "Predicted (2020)",
       fill = "ED Rate") +
  theme_bw() +
  theme(text = element_text(family = "serif"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom") +
  scale_fill_viridis_c(option = "viridis",
                       direction = -1,
                       na.value = "grey95") +
  guides(fill = guide_colorbar(title.position = "top")) +
  coord_sf(datum = NA)

### 3c. Print combined plot
choropleth <-
  (actual_plot) | (pred_plot + plot_layout(guides = 'keep'))

choropleth <-
  choropleth + plot_layout(widths = c(2, 1)) +
  plot_annotation(title = 'Observed ED Rates (2015-2019) vs. Predicted ED rates (2020)',
                  subtitle = 'ED Rates corresponding to visits per 10,000 persons',
                  caption = 'Note: Counties with 0 ED visits are not depicted')

### 3d. Save combined plot
png(
  here::here("figures", "edrate_choropleth.png"),
  width = 8,
  height = 8,
  units = "in",
  res = 300
)

choropleth &
  theme(
    text = element_text('serif'),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
  )

dev.off()

## Clear memory before mapping except for analytic_df and counties
rm(list = ls()[!ls() %in% c("analytic_df", "counties")])


## 4. Choropleth of all SDoH variables ----

### 4a. Create a data frame with the relevant vars and add geometry
df_mean <- analytic_df %>%
  group_by(fips, name) %>%
  summarise(
    p65older = mean(p65older),
    p5younger = mean(p5younger),
    p_hosp_permil = mean(p_hosp_permil),
    p_bedsper1000 = mean(p_bedsper1000),
    p_mds1000 = mean(p_mds1000),
    p_medicare_bene1000 = mean(p_medicare_bene1000),
    p_medicare_elig1000 = mean(p_medicare_elig1000),
    p_poverty = mean(p_poverty),
    p_nonwhite = mean(p_nonwhite)
  ) %>% ungroup () %>%
  left_join(counties, by = c("fips" = "GEOID"))

## 4b. Label variables
labelled::var_label(df_mean) <- list(
  p65older = "% Population over 65 years",
  p5younger = "% Population below 5 years",
  p_poverty = "% Population below poverty",
  p_nonwhite = "% Population that is Non-white",
  p_medicare_elig1000 = "# Medicare eligible per 1000",
  p_medicare_bene1000 = "# Medicare beneficiaries per 1000",
  p_mds1000 = "# MDs per 1000",
  p_hosp_permil = "# Hospitals per million",
  p_bedsper1000 = "# Hospital beds per 1000"
)

### 4c. Plot maps

# Vector of all vars
sdoh_vec <- c(
  "p65older",
  "p5younger",
  "p_poverty",
  "p_nonwhite",
  "p_medicare_elig1000",
  "p_medicare_bene1000",
  "p_mds1000",
  "p_hosp_permil",
  "p_bedsper1000"
)

# Loop over sdoh vector to produce maps
for (i in sdoh_vec) {
  assign(
    paste0(i, "_map"),
    df_mean %>%
      ggplot(aes(fill = !!sym(i), geometry = geometry)) +
      geom_sf() +
      theme_bw() +
      theme(
        text = element_text(family = "serif"),
        legend.position = c(0.75, 0.80),
        legend.background = element_blank(),
        legend.key.size = unit(0.8, 'cm'),
        legend.key.height = unit(0.8, 'cm'),
        legend.key.width = unit(0.8, 'cm'),
        legend.title = element_text(size=11),
        legend.text = element_text(size=11)
      ) +
      scale_fill_viridis_c(option = "magma",
                           direction = -1,
                           na.value = "grey95") +
      guides(fill = guide_colorbar(
        title.position = "top",
        title = labelled::var_label(df_mean[[i]])
      )) +
      coord_sf(datum = NA)
  )
}

### 4d. Combine Plots
sdoh_maps <- (p65older_map | p5younger_map | p_poverty_map) /
  (p_nonwhite_map | p_medicare_elig1000_map | p_medicare_bene1000_map) /
  (p_mds1000_map | p_hosp_permil_map | p_bedsper1000_map)


### 4e. Save combined plots
png(
  here::here("figures", "sdoh_choropleth.png"),
  width = 15,
  height = 15,
  units = "in",
  res = 300
)

sdoh_maps &
    theme(
      text = element_text('serif')
    )

dev.off()


## Clear memory before mapping except for analytic_df and counties
rm(list = ls()[!ls() %in% c("analytic_df", "counties")])

## 5. Choropleth of remote sensing variables (Mean)----
### 5a. Create a data frame with the relevant vars and add geometry
df_mean <- analytic_df %>%
  group_by(fips, name) %>%
  summarise(
    mean_intensity = mean(mean_intensity),
    mean_aerosol = mean(mean_aerosol),
    mean_days = mean(mean_days),
    burned_ratio = mean(burned_ratio)
  ) %>% ungroup () %>%
  left_join(counties, by = c("fips" = "GEOID"))

## 5b. Label variables
labelled::var_label(df_mean) <- list(
  mean_intensity = "Intensity of burning (Mean)",
  mean_aerosol = "Aerosol Optical Depth (Mean)",
  mean_days = "Number of days with fire (Mean)",
  burned_ratio = "Ratio of burned area to total area (mean)"
)

### 5c. Plot maps

# Vector of all vars
fire_vec <- c("mean_intensity",
              "mean_aerosol",
              "mean_days",
              "burned_ratio")


# Loop over fire vector to produce maps
for (i in fire_vec) {
  assign(
    paste0(i, "_map"),
    df_mean %>%
      ggplot(aes(
        fill = !!sym(i), geometry = geometry
      )) +
      geom_sf() +
      theme_bw() +
      theme(
        text = element_text(family = "serif"),
        legend.position = c(0.75, 0.80),
        legend.background = element_blank(),
        legend.key.size = unit(1.3, 'cm'),
        legend.key.height = unit(1.3, 'cm'),
        legend.key.width = unit(1.3, 'cm'),
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 13)
      ) +
      scale_fill_gradientn(colours = brewer.pal(5, "OrRd"),
                           na.value = "grey95") +
      guides(fill = guide_colorbar(
        title.position = "top",
        title = labelled::var_label(df_mean[[i]])
      )) +
      coord_sf(datum = NA)
  )
}


### 5d. Combine Plots
fire_maps <-
  (mean_intensity_map |
     mean_aerosol_map) / (mean_days_map | burned_ratio_map)

### 4e. Save combined plots
png(
  here::here("figures", "fire_choropleth.png"),
  width = 15,
  height = 15,
  units = "in",
  res = 300
)

fire_maps &
  theme(text = element_text('serif'))

dev.off()


## Clear memory before mapping except for analytic_df and counties
rm(list = ls()[!ls() %in% c("analytic_df", "counties")])

## 6. Choropleth of remote sensing variables (Time-series)----
### 6a. Create a data frame with the relevant vars and add geometry
df_timeseries <- analytic_df %>%
  select(
    c(
      "fips",
      "name",
      "year",
      "mean_intensity",
      "mean_aerosol",
      "mean_days",
      "burned_ratio"
    )
  ) %>% ungroup () %>%
  left_join(counties, by = c("fips" = "GEOID"))

## 6b. Label variables
labelled::var_label(df_timeseries) <- list(
  mean_intensity = "Intensity of burning (Mean)",
  mean_aerosol = "Aerosol Optical Depth (Mean)",
  mean_days = "Number of days with fire (Mean)",
  burned_ratio = "Ratio of burned area to total area (mean)"
)

### 6c. Plot maps

# Vector of all vars
fire_vec <- c("mean_intensity",
              "mean_aerosol",
              "mean_days",
              "burned_ratio")

# Loop over fire vector to produce maps and facet wrap by year
for (i in fire_vec) {
  assign(
    paste0(i, "_map"),
    df_timeseries %>%
      ggplot(aes(
        fill = !!sym(i), geometry = geometry
      )) +
      geom_sf() +
      theme_bw() +
      theme(
        text = element_text(family = "serif"),
        plot.title = element_text(size = 20),
        legend.background = element_blank(),
        legend.key.size = unit(1.4, 'cm'),
        legend.key.height = unit(1.4, 'cm'),
        legend.key.width = unit(1, 'cm'),
        legend.text = element_text(size = 12),
        legend.position = "left",
        legend.title = element_blank(),
        strip.text = element_text(size = 15)
      ) +
      scale_fill_gradientn(colours = brewer.pal(5, "OrRd"),
                           na.value = "grey95") +
      guides(fill = guide_colorbar(
        title.position = "top",
        title = labelled::var_label(df_timeseries[[i]])
      )) +
      labs(title = labelled::var_label(df_timeseries[[i]])) +
      coord_sf(datum = NA) +
      facet_wrap(~ year, ncol = 6)
  )
}

### 6d. Combine Plots
fire_maps <-
  (mean_intensity_map / mean_days_map / burned_ratio_map / mean_aerosol_map)

# fire_maps
### 6e. Save combined plots
png(
  here::here("figures", "fire_timeseries.png"),
  width = 15,
  height = 15,
  units = "in",
  res = 300
)

fire_maps &
  theme(text = element_text('serif'))

dev.off()

## Clear memory before mapping except for analytic_df and counties
rm(list = ls()[!ls() %in% c("analytic_df", "counties")])
