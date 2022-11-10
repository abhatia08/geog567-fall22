# SETUP ----

## 1. Load required packages ----
library(data.table)
library(tidyverse)
library(here)
library(fs)
library(labelled)
library(caret)
library(parallel)
library(randomForest)
library(MASS)
library(xgboost)
library(DiagrammeR)
library(wesanderson)


## 2. Declare `here` ----
here::i_am("scripts/06_analysis.R")

## 3. Run Util.R ----
source(here::here("scripts", "util.R"))

## 4. If the "analytic_df.csv" file doesn't exist, source scripts 05_merge_data.R ----
if (!file.exists(here::here("derived_data", "analytic_df.csv"))) {
  source(here::here("scripts", "05_merge_data.R"))
} else {
  analytic_df <-
    read_csv(here::here("derived_data", "analytic_df.csv"))
}

# 06. ANALYSIS ----

## NOTE: If the models are not already trained, the chunk below will run to save the files. Else, it will load them in accordingly
if (!file.exists(here::here("derived_data", "xgboostmodel.RDS"))) {
  ## 6.1 Pre-processing ----
  
  ### 1. Subset out years 2015-2019 for model training and keep 2020 for predictions ----
  df_2019 <-
    analytic_df %>% filter(year < 2020) %>% filter(!is.na(ed_rate)) %>% filter(ed_rate > 0) %>% dplyr::select(c(-year,-name,-fips,-n_pop_2018))# Dropping the weird outliers (Alpine)
  df_2020 <- analytic_df %>% filter(year == 2020)
  
  ### 2. Set seed ----
  set.seed(8125)
  
  ### 3. Split data ----
  sample <-
    sample.int(n = nrow(df_2019),
               size = floor(.8 * nrow(df_2019)),
               replace = F)
  train <- df_2019[sample, ]
  test <- df_2019[-sample, ]
  
  train_y <- train[, 'ed_rate']
  train_y <- unlist(train_y)
  train_x <- train[, names(train) != 'ed_rate']
  
  test_y <- test[, 'ed_rate']
  test_y <- unlist(test_y)
  test_x <- test[, names(test) != 'ed_rate']
  
  
  ## 6.2 Random Forest Model ----
  ## NOTE: Tuning commented out to save time
  
  #### 1. Model Fit ----
  # rf_model <-
  #   randomForest(
  #     x = train_x,
  #     y = train_y,
  #     xtest = test_x,
  #     ytest = test_y,
  #     mtry = 5,
  #     importance = TRUE,
  #     ntree = 10000
  #   )
  #
  #### 2. Model tuning ----
  #
  # ### Find the best number of iterations
  # rf_model$train_rmse <- sqrt(rf_model$mse)
  # rf_model$test_rmse <- sqrt(rf_model$test$mse)
  #
  # rf_res_df <-
  #   data.frame(
  #     TRAINING_ERROR = rf_model$train_rmse,
  #     VALIDATION_ERROR = rf_model$test_rmse,
  #     # Don't confuse this with the test data set.
  #     ITERATION = c(1:10000)
  #   ) %>%
  #   mutate(MIN = VALIDATION_ERROR == min(VALIDATION_ERROR))
  #
  # best_nrounds <- rf_res_df %>%
  #   filter(MIN) %>%
  #   pull(ITERATION)
  
  best_nrounds <- 5000
  
  rf_final_model <-
    randomForest(
      x = train_x,
      y = train_y,
      mtry = 5,
      importance = TRUE,
      ntree = best_nrounds
    )
  
  #### 3, Save Random Forest to disk ----
  save(rf_final_model, file = here::here("derived_data", "rf_model.RData"))
  
  #### 4. Make Predictions ----
  
  y_pred_rf = predict(rf_final_model, test_x)
  rf_test_mse = mean(((y_pred_rf - test_y) ^ 2))
  rf_test_rmse = sqrt(rf_test_mse)
  
  rf_benchmark <- rf_test_rmse
  
  #### 5. Save test predictions ----
  
  ## Ensemble predictions
  y_ens <- (y_pred_rf * 0.5) + (y_pred_xgb * 0.5)
  ens_test_mse = mean(((y_ens - test_y) ^ 2))
  ens_test_rmse = sqrt(ens_test_mse)
  
  
  
  ## 6.3 Extreme Gradient Boosted Model  ----
  ## NOTE: THIS TAKES A LONG TIME, COMMENTED OUT THE TUNING SO THAT YOU DONT HAVE TO RE-TRAIN A MODEL.
  
  ##### Recreating training data
  # train_y <- df_2019[,'ed_rate']
  # train_y <- unlist(train_y)
  # train_x <- df_2019[, names(df_2019) !='ed_rate']
  
  #### 1. Tuning the learning rate (eta) -----
  # nrounds <- 1000
  #
  # xgb_tune_grid1 = expand.grid(
  #   nrounds = seq(from = 200, to = nrounds, by = 50),
  #   eta = c(0.001, 0.005, 0.01, 0.1),
  #   max_depth = c(5, 6, 7, 8, 9),
  #   gamma = 0,
  #   colsample_bytree = 1,
  #   min_child_weight = 1,
  #   subsample = 1
  # )
  #
  # xgb_trcontrol = caret::trainControl(
  #   method = "cv",
  #   number = 5,
  #   verboseIter = TRUE,
  #   returnData = FALSE,
  #   returnResamp = "all",
  #   allowParallel = TRUE
  # )
  #
  # xgb_tune_1 = caret::train(
  #   x = as.matrix(train_x),
  #   y = train_y,
  #   trControl = xgb_trcontrol,
  #   tuneGrid = xgb_tune_grid_1,
  #   method = "xgbTree",
  #   verbosity = 0
  # )
  #
  # xgb_tune_1$bestTune
  # tuneplot(xgb_tune_1)
  # # Based on this, we pick an eta of 0.005, and will tune the tree depth, and other hyperparameters
  
  xgb_eta <- 0.005
  
  #### 2. Tuning max depth and child weight ----
  # xgb_tune_grid_2 <- expand.grid(
  #   nrounds = seq(from = 50, to = nrounds, by = 50),
  #   eta = xgb_eta,
  #   max_depth = ifelse(
  #     xgb_tune_1$bestTune$max_depth == 5,
  #     c(xgb_tune_1$bestTune$max_depth:4),
  #     xgb_tune_1$bestTune$max_depth - 1:xgb_tune_1$bestTune$max_depth + 1
  #   ),
  #   gamma = 0,
  #   colsample_bytree = 1,
  #   min_child_weight = c(1, 2, 3),
  #   subsample = 1
  # )
  #
  # xgb_tune_2 <- caret::train(
  #   x = as.matrix(train_x),
  #   y = train_y,
  #   trControl = xgb_trcontrol,
  #   tuneGrid = xgb_tune_grid_2,
  #   method = "xgbTree",
  #   verbose = TRUE,
  #   verbosity = 0
  # )
  
  # xgb_tune_2$bestTune
  # tuneplot(xgb_tune_2)
  # # Looks like the min_child_weight = 2 for an eta = 0.005, with a tree-depth of 5
  
  xgb_treedepth <- 5
  xgb_min_child_weight <- 2
  
  #### 3. Tuning column and row sampling ----
  #
  # xgb_tune_grid_3 <- expand.grid(
  #   nrounds = seq(from = 50, to = nrounds, by = 50),
  #   eta = xgb_eta,
  #   max_depth = xgb_treedepth,
  #   gamma = 0,
  #   colsample_bytree = c(0.4, 0.6, 0.8, 1.0),
  #   min_child_weight = xgb_min_child_weight,
  #   subsample = c(0.5, 0.75, 1.0)
  # )
  #
  # xgb_tune_3 <- caret::train(
  #   x = as.matrix(train_x),
  #   y = train_y,
  #   trControl = xgb_trcontrol,
  #   tuneGrid = xgb_tune_grid_3,
  #   method = "xgbTree",
  #   verbose = TRUE,
  #   verbosity = 0
  # )
  #
  # xgb_tune_3$bestTune
  # tuneplot(xgb_tune_3, probs = .95)
  # # Colsample = 0.4, subsample = 1 are the best parameters here
  
  xgb_colsample <- 0.4
  xgb_subsample <- 1
  
  
  #### 4. Tuning loss function gamma ----
  #
  # xgb_tune_grid_4 <- expand.grid(
  #   nrounds = seq(from = 50, to = nrounds, by = 50),
  #   eta = xgb_eta,
  #   max_depth = xgb_treedepth,
  #   gamma = c(0, 0.05, 0.1, 0.5, 0.7, 0.9, 1.0),
  #   colsample_bytree = xgb_colsample,
  #   min_child_weight = xgb_min_child_weight,
  #   subsample = xgb_subsample
  # )
  #
  # xgb_tune_4 <- caret::train(
  #   x = as.matrix(train_x),
  #   y = train_y,
  #   trControl = xgb_trcontrol,
  #   tuneGrid = xgb_tune_grid_4,
  #   method = "xgbTree",
  #   verbose = TRUE,
  #   verbosity = 0
  # )
  #
  # xgb_tune_4$bestTune
  # tuneplot(xgb_tune_4)
  #
  # # Gamma = 1
  xgb_gamma <- 1
  
  #### 5. Tune the learning rate slightly ----
  # #### Tweak the learning rate at the end to lower it slightly
  #
  # xgb_tune_grid_5 <- expand.grid(
  #   nrounds = seq(from = 100, to = 10000, by = 100),
  #   eta = c(0.001, 0.002, 0.003, 0.004, 0.005, 0.01),
  #   max_depth = xgb_treedepth,
  #   gamma = xgb_gamma,
  #   colsample_bytree = xgb_colsample,
  #   min_child_weight = xgb_min_child_weight,
  #   subsample = xgb_subsample
  # )
  #
  # xgb_tune_5 <- caret::train(
  #   x = as.matrix(train_x),
  #   y = train_y,
  #   trControl = xgb_trcontrol,
  #   tuneGrid = xgb_tune_grid_5,
  #   method = "xgbTree",
  #   verbose = TRUE,
  #   verbosity = 0
  # )
  #
  # xgb_tune_5$bestTune
  # tuneplot(xgb_tune_5)
  # # Eta = 0.003, nrounds = 4500
  xgb_eta <- 0.003
  xgb_nrounds <- 4500
  
  #### 6. Final Model ----
  #### Fit
  xgb_final_grid <- expand.grid(
    nrounds = xgb_nrounds,
    eta = xgb_eta,
    max_depth = xgb_treedepth,
    gamma = xgb_gamma,
    colsample_bytree = xgb_colsample,
    min_child_weight = xgb_min_child_weight,
    subsample = xgb_subsample
  )
  
  xgb_trcontrol = caret::trainControl(
    method = "cv",
    number = 5,
    verboseIter = TRUE,
    returnData = FALSE,
    returnResamp = "all",
    allowParallel = TRUE
  )
  
  xgb_model <- caret::train(
    x = as.matrix(train_x),
    y = train_y,
    trControl = xgb_trcontrol,
    tuneGrid = xgb_final_grid,
    method = "xgbTree",
    verbose = TRUE,
    verbosity = 0
  )
  
  #### 7. Model Evaluation ----
  y_pred_xgb = predict(xgb_model, test_x)
  xgb_test_mse = mean(((y_pred_xgb - test_y) ^ 2))
  xgb_test_rmse = sqrt(xgb_test_mse)
  
  xgb_benchmark <- xgb_test_rmse
  
  #### 8. Save xgboost model ----
  saveRDS(xgb_model, here::here("derived_data", "xgboostmodel.RDS"))
  
} else {
  xgb_model <- readRDS(here::here("derived_data", "xgboostmodel.RDS"))
  # read in rf_model.RData as well
  load(here::here("derived_data", "rf_model.RData"))
  
}
## 6.5 Final Predictions ----
## Finally, since both models performed somewhat-similarly, we will make ensemble predictions

### 1. Make predictions
pred_df <- analytic_df[,-4]
y_pred_rf = predict(rf_final_model, newdata = pred_df)
y_pred_xgb = predict(xgb_model, pred_df)
y_ens = (y_pred_rf + y_pred_xgb) / 2

predictions <- analytic_df[, 1:3]
predictions$edrate_pred <- round(y_ens, 1)


### 2. Reload data and join in predicted values, and sort

predictions_df <-
  analytic_df %>% left_join(predictions, by = c("fips", "year", "name")) 


predictions_df$edrate_pred[predictions_df$name == "Alpine"] <-
  0 # Alpine is an anomaly with no hospitals


### 2. Sort columns
predictions_df <-
  predictions_df %>% rename(edrate_actual = ed_rate) %>%
  dplyr::select(fips,
                name,
                year,
                edrate_actual,
                edrate_pred,
                dplyr::everything())


### 3. Write data
readr::write_csv(analytic_df, here::here("derived_data", "predictions.csv"))


