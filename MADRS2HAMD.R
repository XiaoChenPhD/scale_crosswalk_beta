# do the conversion from the MADRS to HAMD
#
# Xiao Chen 240604
# Modified 240729
# Use FOURD Screen data, lower missing rate
# Modified 240820
# carry over the screen data to the baseline data for FOURD dataset if the baseline data are missing
# Modified 240822
# carry over ALL screen data to baseline data
# chenxiaophd@gmail.com

# clear everything
rm(list = ls())
cat("\014")

# load packages
library(openxlsx)
library(tidyr)
library(dplyr)
library(ggplot2)
library(mirt)
library(equateIRT)
library(Metrics)
library(pROC)
library(equate)
library(caret)
library(mice)
library(randomForest)
library(e1071)

# initialization
work_dir <- "/Users/ChenXiao/Library/CloudStorage/OneDrive-Personal/Documents/scale_crosswalk/Analysis"
code_dir <- "/Users/ChenXiao/Documents/My_Documents/scale_crosswalk/scale_crosswalk_alpha"
svm_kernal <- "linear"
benchmark_filename <- "benchmarks_M2H_ALL_carryover.RData" # the file name to save the benchmark results
model_baseline_filename <- "model_baseline_M2H_ALL_carryover.RData"
model_current_prefix <- "data_model_M2H_ALL_carryover_"

# read in data
file_path <- file.path(work_dir, "FOURD_CARTBIND_dataset.xlsx")
data_FOURD <- read.xlsx(file_path, sheet = "FOURD")
data_CARTBIND <- read.xlsx(file_path, sheet = "CARTBIND")
source(file.path(code_dir, "organize_data.R"))
source(file.path(code_dir, "fill_in_results_main.R"))

# recode some NAs as 0
data_FOURD$BENZO[is.na(data_FOURD$BENZO)] <- 0
data_FOURD$ANTIDEP[is.na(data_FOURD$ANTIDEP)] <- 0
data_FOURD$ANTIDEP_COMB[is.na(data_FOURD$ANTIDEP_COMB)] <- 0

# To address the high missing rate of the baseline time point in FOURD dataset, 
# check if they are NA, if yes, carry over the screen data
data_FOURD$hrsd1s_b <- ifelse(is.na(data_FOURD$hrsd1s_b), data_FOURD$hrsd1s_s, data_FOURD$hrsd1s_b)
data_FOURD$hrsd2s_b <- ifelse(is.na(data_FOURD$hrsd2s_b), data_FOURD$hrsd2s_s, data_FOURD$hrsd2s_b)
data_FOURD$hrsd3s_b <- ifelse(is.na(data_FOURD$hrsd3s_b), data_FOURD$hrsd3s_s, data_FOURD$hrsd3s_b)
data_FOURD$hrsd4s_b <- ifelse(is.na(data_FOURD$hrsd4s_b), data_FOURD$hrsd4s_s, data_FOURD$hrsd4s_b)
data_FOURD$hrsd5s_b <- ifelse(is.na(data_FOURD$hrsd5s_b), data_FOURD$hrsd5s_s, data_FOURD$hrsd5s_b)
data_FOURD$hrsd6s_b <- ifelse(is.na(data_FOURD$hrsd6s_b), data_FOURD$hrsd6s_s, data_FOURD$hrsd6s_b)
data_FOURD$hrsd7s_b <- ifelse(is.na(data_FOURD$hrsd7s_b), data_FOURD$hrsd7s_s, data_FOURD$hrsd7s_b)
data_FOURD$hrsd8s_b <- ifelse(is.na(data_FOURD$hrsd8s_b), data_FOURD$hrsd8s_s, data_FOURD$hrsd8s_b)
data_FOURD$hrsd9s_b <- ifelse(is.na(data_FOURD$hrsd9s_b), data_FOURD$hrsd9s_s, data_FOURD$hrsd9s_b)
data_FOURD$hrsd10s_b <- ifelse(is.na(data_FOURD$hrsd10s_b), data_FOURD$hrsd10s_s, data_FOURD$hrsd10s_b)
data_FOURD$hrsd11s_b <- ifelse(is.na(data_FOURD$hrsd11s_b), data_FOURD$hrsd11s_s, data_FOURD$hrsd11s_b)
data_FOURD$hrsd12s_b <- ifelse(is.na(data_FOURD$hrsd12s_b), data_FOURD$hrsd12s_s, data_FOURD$hrsd12s_b)
data_FOURD$hrsd13s_b <- ifelse(is.na(data_FOURD$hrsd13s_b), data_FOURD$hrsd13s_s, data_FOURD$hrsd13s_b)
data_FOURD$hrsd14s_b <- ifelse(is.na(data_FOURD$hrsd14s_b), data_FOURD$hrsd14s_s, data_FOURD$hrsd14s_b)
data_FOURD$hrsd15s_b <- ifelse(is.na(data_FOURD$hrsd15s_b), data_FOURD$hrsd15s_s, data_FOURD$hrsd15s_b)
data_FOURD$hrsd16s_b <- ifelse(is.na(data_FOURD$hrsd16s_b), data_FOURD$hrsd16s_s, data_FOURD$hrsd16s_b)
data_FOURD$hrsd17s_b <- ifelse(is.na(data_FOURD$hrsd17s_b), data_FOURD$hrsd17s_s, data_FOURD$hrsd17s_b)
data_FOURD$hrsd_total_b <- ifelse(is.na(data_FOURD$hrsd_total_b), data_FOURD$hrsd_total_s, data_FOURD$hrsd_total_b)

# also carry over the madrs screen data to the baseline data in FOURD
data_FOURD$madrs1_b <- ifelse(is.na(data_FOURD$madrs1_b), data_FOURD$madrs1_s, data_FOURD$madrs1_b)
data_FOURD$madrs2_b <- ifelse(is.na(data_FOURD$madrs2_b), data_FOURD$madrs2_s, data_FOURD$madrs2_b)
data_FOURD$madrs3_b <- ifelse(is.na(data_FOURD$madrs3_b), data_FOURD$madrs3_s, data_FOURD$madrs3_b)
data_FOURD$madrs4_b <- ifelse(is.na(data_FOURD$madrs4_b), data_FOURD$madrs4_s, data_FOURD$madrs4_b)
data_FOURD$madrs5_b <- ifelse(is.na(data_FOURD$madrs5_b), data_FOURD$madrs5_s, data_FOURD$madrs5_b)
data_FOURD$madrs6_b <- ifelse(is.na(data_FOURD$madrs6_b), data_FOURD$madrs6_s, data_FOURD$madrs6_b)
data_FOURD$madrs7_b <- ifelse(is.na(data_FOURD$madrs7_b), data_FOURD$madrs7_s, data_FOURD$madrs7_b)
data_FOURD$madrs8_b <- ifelse(is.na(data_FOURD$madrs8_b), data_FOURD$madrs8_s, data_FOURD$madrs8_b)
data_FOURD$madrs9_b <- ifelse(is.na(data_FOURD$madrs9_b), data_FOURD$madrs9_s, data_FOURD$madrs9_b)
data_FOURD$madrs10_b <- ifelse(is.na(data_FOURD$madrs10_b), data_FOURD$madrs10_s, data_FOURD$madrs10_b)
data_FOURD$total_madrs_b <- ifelse(is.na(data_FOURD$total_madrs_b), data_FOURD$total_madrs_s, data_FOURD$total_madrs_b)

# carryover the CARTBIND data
data_CARTBIND$hrsd1s_b2 <- ifelse(is.na(data_CARTBIND$hrsd1s_b2), data_CARTBIND$hrsd1s_b1, data_CARTBIND$hrsd1s_b2)
data_CARTBIND$hrsd2s_b2 <- ifelse(is.na(data_CARTBIND$hrsd2s_b2), data_CARTBIND$hrsd2s_b1, data_CARTBIND$hrsd2s_b2)
data_CARTBIND$hrsd3s_b2 <- ifelse(is.na(data_CARTBIND$hrsd3s_b2), data_CARTBIND$hrsd3s_b1, data_CARTBIND$hrsd3s_b2)
data_CARTBIND$hrsd4s_b2 <- ifelse(is.na(data_CARTBIND$hrsd4s_b2), data_CARTBIND$hrsd4s_b1, data_CARTBIND$hrsd4s_b2)
data_CARTBIND$hrsd5s_b2 <- ifelse(is.na(data_CARTBIND$hrsd5s_b2), data_CARTBIND$hrsd5s_b1, data_CARTBIND$hrsd5s_b2)
data_CARTBIND$hrsd6s_b2 <- ifelse(is.na(data_CARTBIND$hrsd6s_b2), data_CARTBIND$hrsd6s_b1, data_CARTBIND$hrsd6s_b2)
data_CARTBIND$hrsd7s_b2 <- ifelse(is.na(data_CARTBIND$hrsd7s_b2), data_CARTBIND$hrsd7s_b1, data_CARTBIND$hrsd7s_b2)
data_CARTBIND$hrsd8s_b2 <- ifelse(is.na(data_CARTBIND$hrsd8s_b2), data_CARTBIND$hrsd8s_b1, data_CARTBIND$hrsd8s_b2)
data_CARTBIND$hrsd9s_b2 <- ifelse(is.na(data_CARTBIND$hrsd9s_b2), data_CARTBIND$hrsd9s_b1, data_CARTBIND$hrsd9s_b2)
data_CARTBIND$hrsd10s_b2 <- ifelse(is.na(data_CARTBIND$hrsd10s_b2), data_CARTBIND$hrsd10s_b1, data_CARTBIND$hrsd10s_b2)
data_CARTBIND$hrsd11s_b2 <- ifelse(is.na(data_CARTBIND$hrsd11s_b2), data_CARTBIND$hrsd11s_b1, data_CARTBIND$hrsd11s_b2)
data_CARTBIND$hrsd12s_b2 <- ifelse(is.na(data_CARTBIND$hrsd12s_b2), data_CARTBIND$hrsd12s_b1, data_CARTBIND$hrsd12s_b2)
data_CARTBIND$hrsd13s_b2 <- ifelse(is.na(data_CARTBIND$hrsd13s_b2), data_CARTBIND$hrsd13s_b1, data_CARTBIND$hrsd13s_b2)
data_CARTBIND$hrsd14s_b2 <- ifelse(is.na(data_CARTBIND$hrsd14s_b2), data_CARTBIND$hrsd14s_b1, data_CARTBIND$hrsd14s_b2)
data_CARTBIND$hrsd15s_b2 <- ifelse(is.na(data_CARTBIND$hrsd15s_b2), data_CARTBIND$hrsd15s_b1, data_CARTBIND$hrsd15s_b2)
data_CARTBIND$hrsd16s_b2 <- ifelse(is.na(data_CARTBIND$hrsd16s_b2), data_CARTBIND$hrsd16s_b1, data_CARTBIND$hrsd16s_b2)
data_CARTBIND$hrsd17s_b2 <- ifelse(is.na(data_CARTBIND$hrsd17s_b2), data_CARTBIND$hrsd17s_b1, data_CARTBIND$hrsd17s_b2)
data_CARTBIND$hrsd_total_b2 <- ifelse(is.na(data_CARTBIND$hrsd_total_b2), data_CARTBIND$hrsd_total_b1, data_CARTBIND$hrsd_total_b2)

# build data frames to store the benchmarking metrics
column_names <- c('b', 't30', 't30_b', 'f1', 'f1_b', 'f2', 'f2_b', 
                  'f3', 'f3_b', 'delta', 'delta_b')
row_names <- c('Leucht', 'percentile', 
               'linear regression', 'linear regression item', 
               'random forest', 'random forest item', 
               'svm regression', 'svm regression item')
df_rmse <- as.data.frame(matrix(nrow = length(row_names), ncol = length(column_names)))
colnames(df_rmse) <- column_names
rownames(df_rmse) <- row_names
df_mae <- as.data.frame(matrix(nrow = length(row_names), ncol = length(column_names)))
colnames(df_mae) <- column_names
rownames(df_mae) <- row_names

# the main loop, looping across all time points
for (flag in c(1,2,3,4,5,6)){
  results <- organize_data(flag, data_FOURD, data_CARTBIND)
  data_FOURD_renamed <- results$data_FOURD_renamed
  data_CARTBIND_renamed <- results$data_CARTBIND_renamed
  
  # conbine selected variables from THREE-D and CARTBIND, do multiple imputation
  # and split them into training and test sample
  set.seed(1)
  # multiple imputation to deal with missing values
  data_CARTBIND_FOURD <- rbind(data_CARTBIND_renamed, data_FOURD_renamed)
  data_CARTBIND_FOURD$sex[data_CARTBIND_FOURD$sex == 9999] <- NA
  imp <- mice(data_CARTBIND_FOURD, m = 5, method = 'pmm')
  data_binded_imp <- complete(imp)
  # hrsd17 are all 0s, so manually replace NAs as 0
  data_binded_imp$hrsd17s[is.na(data_binded_imp$hrsd17s)] <- 0
  
  # split the data, 1/3 as validation sample, 2/3 as training sample
  index <- createDataPartition(data_binded_imp$hrsd_total, p = 2/3, list = FALSE)
  # Create the derivative (training) sample
  derivative_sample <- data_binded_imp[index, ]
  # Create the validation sample
  validation_sample <- data_binded_imp[-index, ]
  
  
  ################################################################################################
  # Leucht et al., 2018 method
  # if to convert delta values, the conversion table is a little different
  if (flag == 6){
    conversion_table <- read.xlsx(file.path(work_dir, "coversion_table_delta_Leucht2018.xlsx"))
  } else {
    conversion_table <- read.xlsx(file.path(work_dir, "coversion_table_Leucht2018.xlsx"))
  }
  
  predictions <- c()
  for (i in seq_along(validation_sample$madrs_total)){
    if (flag == 6){
      if (validation_sample$madrs_total[[i]] < -8){
        # the original conversion table only listed delta_MADRS >= -8
        predictions <- c(predictions, -5)
      }else if (validation_sample$madrs_total[[i]] > 37){
        # the original conversion table only listed delta_MADRS <= 37
        predictions <- c(predictions, 27)
      }else{
        predictions <- c(predictions, conversion_table$HAMD[match(validation_sample$madrs_total[[i]], conversion_table$MADRS)])
      }
    }else{
      if (validation_sample$madrs_total[[i]] > 0 & validation_sample$madrs_total[[i]] < 3){
        # The original conversion table did not list madrs = 1 or madrs = 2, I arbitrarily set it as 2
        predictions <- c(predictions, 2)
      }else{
        predictions <- c(predictions, conversion_table$HAMD[match(validation_sample$madrs_total[[i]], conversion_table$MADRS)])
      }
    }
    }
  
  rmse_b <- NA
  mae_b <- NA
  rmse <- rmse(validation_sample$hrsd_total, predictions)
  mae <- mae(validation_sample$hrsd_total, predictions)
  
  model_flag <- 'Leucht'
  temp <- fill_in_results_main(flag, model_flag, df_rmse, df_mae, rmse, rmse_b, mae, mae_b)
  df_rmse <- temp$df_rmse
  df_mae <- temp$df_mae
  
  
  ################################################################################################
  # Percentile method
  table_hrsd_total <- table(derivative_sample$hrsd_total)
  y <- as.freqtab(table_hrsd_total)
  table_madrs_total <- table(derivative_sample$madrs_total)
  x <- as.freqtab(table_madrs_total)
  
  # do presmooth
  x_smooth <- presmoothing(x, smooth = "log", degrees = list(3, 1))
  y_smooth <- presmoothing(y, smooth = "log", degrees = list(3, 1))
  equating_result <- equate(x_smooth, y_smooth, type = "equipercentile")
  predictions <- equate(validation_sample$madrs_total, y = equating_result)
  # benchmarking
  rmse <- rmse(validation_sample$hrsd_total, predictions)
  mae <- mae(validation_sample$hrsd_total, predictions)
  
  rmse_b <- NA
  mae_b <- NA
  # use baseline model to do prediction
  if(flag != 1){
    load(file.path(work_dir, model_baseline_filename))
    predictions_b <- equate(validation_sample$madrs_total, y = equating_result_b)
    rmse_b <- rmse(validation_sample$hrsd_total, predictions_b)
    mae_b <- mae(validation_sample$hrsd_total, predictions_b)
  }
  
  model_flag <- 'percentile'
  temp <- fill_in_results_main(flag, model_flag, df_rmse, df_mae, rmse, rmse_b, mae, mae_b)
  df_rmse <- temp$df_rmse
  df_mae <- temp$df_mae
  
  
  ################################################################################################
  # Multiple variable linear regression
  model_lm <- lm(hrsd_total ~ madrs_total, data = derivative_sample)
  coefficients <- coef(model_lm)
  
  predictions <- predict(model_lm, newdata = validation_sample)
  rmse <- rmse(validation_sample$hrsd_total, predictions)
  mae <- mae(validation_sample$hrsd_total, predictions)
  
  rmse_b <- NA
  mae_b <- NA
  if(flag != 1){
    load(file.path(work_dir, model_baseline_filename))
    predictions_b <- predict(model_lm_b, newdata = validation_sample)
    rmse_b <- rmse(validation_sample$hrsd_total, predictions_b)
    mae_b <- mae(validation_sample$hrsd_total, predictions_b)
  }
  
  model_flag <- 'linear regression'
  temp <- fill_in_results_main(flag, model_flag, df_rmse, df_mae, rmse, rmse_b, mae, mae_b)
  df_rmse <- temp$df_rmse
  df_mae <- temp$df_mae
  
  
  ################################################################################################
  # multiple linear regression with item scores as inputs
  model_lm_item <- lm(hrsd_total ~ madrs1 + madrs2 + madrs3 + madrs4 + madrs5 
                      + madrs6 + madrs7 + madrs8 + madrs9 + madrs10, data = derivative_sample)
  coefficients <- coef(model_lm_item)
  predictions <- predict(model_lm_item, newdata = validation_sample)
  rmse <- rmse(validation_sample$hrsd_total, predictions)
  mae <- mae(validation_sample$hrsd_total, predictions)
  
  rmse_b <- NA
  mae_b <- NA
  if(flag != 1){
    load(file.path(work_dir, model_baseline_filename))
    predictions_b <- predict(model_lm_item_b, newdata = validation_sample)
    rmse_b <- rmse(validation_sample$hrsd_total, predictions_b)
    mae_b <- mae(validation_sample$hrsd_total, predictions_b)
  }
  
  model_flag <- 'linear regression item'
  temp <- fill_in_results_main(flag, model_flag, df_rmse, df_mae, rmse, rmse_b, mae, mae_b)
  df_rmse <- temp$df_rmse
  df_mae <- temp$df_mae
  
  
  ################### Random Forest regression ##################################
  model_rf <- randomForest(hrsd_total ~ madrs_total, data = derivative_sample, ntree = 500)
  predictions <- predict(model_rf, newdata = validation_sample)
  
  rmse <- rmse(validation_sample$hrsd_total, predictions)
  mae <- mae(validation_sample$hrsd_total, predictions)
  
  rmse_b <- NA
  mae_b <- NA
  if(flag != 1){
    load(file.path(work_dir, model_baseline_filename))
    predictions_b <- predict(model_rf_b, newdata = validation_sample)
    rmse_b <- rmse(validation_sample$hrsd_total, predictions_b)
    mae_b <- mae(validation_sample$hrsd_total, predictions_b)
  }
  
  model_flag <- 'random forest'
  temp <- fill_in_results_main(flag, model_flag, df_rmse, df_mae, rmse, rmse_b, mae, mae_b)
  df_rmse <- temp$df_rmse
  df_mae <- temp$df_mae
  
  
  ################### Random Forest regression item ##################################
  model_rf_item <- randomForest(hrsd_total ~ madrs1 + madrs2 + madrs3 + madrs4 + madrs5 
                                + madrs6 + madrs7 + madrs8 + madrs9 + madrs10, data = derivative_sample, ntree = 500)
  predictions <- predict(model_rf_item, newdata = validation_sample)
  v_imp <- importance(model_rf_item)
  
  rmse <- rmse(validation_sample$hrsd_total, predictions)
  mae <- mae(validation_sample$hrsd_total, predictions)
  
  
  rmse_b <- NA
  mae_b <- NA
  if(flag != 1){
    load(file.path(work_dir, model_baseline_filename))
    predictions_b <- predict(model_rf_item_b, newdata = validation_sample)
    rmse_b <- rmse(validation_sample$hrsd_total, predictions_b)
    mae_b <- mae(validation_sample$hrsd_total, predictions_b)
  }
  
  model_flag <- 'random forest item'
  temp <- fill_in_results_main(flag, model_flag, df_rmse, df_mae, rmse, rmse_b, mae, mae_b)
  df_rmse <- temp$df_rmse
  df_mae <- temp$df_mae
  
  
  ######################### svm regression #####################################
  x <- select(derivative_sample, c(madrs_total))
  x_test <- select(validation_sample, c(madrs_total))
  y <- derivative_sample$hrsd_total
  
  model_svm <- svm(x, y, type = "eps-regression", kernel = "linear", cost = 1)
  predictions <- predict(model_svm, x_test)
  weights <- t(model_svm$coefs) %*% model_svm$SV
  rmse <- rmse(validation_sample$hrsd_total, predictions)
  mae <- mae(validation_sample$hrsd_total, predictions)
  
  rmse_b <- NA
  mae_b <- NA
  if(flag != 1){
    load(file.path(work_dir, model_baseline_filename))
    predictions_b <- predict(model_svm_b, newdata = x_test)
    rmse_b <- rmse(validation_sample$hrsd_total, predictions_b)
    mae_b <- mae(validation_sample$hrsd_total, predictions_b)
  }
  
  model_flag <- 'svm regression'
  temp <- fill_in_results_main(flag, model_flag, df_rmse, df_mae, rmse, rmse_b, mae, mae_b)
  df_rmse <- temp$df_rmse
  df_mae <- temp$df_mae
  
  
  ######################### svm regression item #################################
  x <- select(derivative_sample, c(madrs1, madrs2, madrs3, madrs4, madrs5, madrs6, 
                                   madrs7, madrs8, madrs9, madrs10))
  y <- derivative_sample$hrsd_total
  x_test <- select(validation_sample, c(madrs1, madrs2, madrs3, madrs4, madrs5, madrs6, 
                                        madrs7, madrs8, madrs9, madrs10))
  
  model_svm_item <- svm(x, y, type = "eps-regression", kernel = "linear", cost = 1)
  predictions <- predict(model_svm_item, x_test)
  weights <- t(model_svm_item$coefs) %*% model_svm_item$SV
  rmse <- rmse(validation_sample$hrsd_total, predictions)
  mae <- mae(validation_sample$hrsd_total, predictions)
  
  rmse_b <- NA
  mae_b <- NA
  if(flag != 1){
    load(file.path(work_dir, model_baseline_filename))
    predictions_b <- predict(model_svm_item_b, newdata = x_test)
    rmse_b <- rmse(validation_sample$hrsd_total, predictions_b)
    mae_b <- mae(validation_sample$hrsd_total, predictions_b)
  }
  
  model_flag <- 'svm regression item'
  temp <- fill_in_results_main(flag, model_flag, df_rmse, df_mae, rmse, rmse_b, mae, mae_b)
  df_rmse <- temp$df_rmse
  df_mae <- temp$df_mae
  
  # save the baseline models
  if(flag == 1){
    equating_result_b <- equating_result
    model_lm_b <- model_lm
    model_lm_item_b <- model_lm_item
    model_rf_b <- model_rf
    model_rf_item_b <- model_rf_item
    model_svm_b <- model_svm
    model_svm_item_b <- model_svm_item
    file_path <- file.path(work_dir, model_baseline_filename)
    save(equating_result_b, model_lm_b, model_lm_item_b, model_rf_b, model_rf_item_b, 
         model_svm_b, model_svm_item_b, file = file_path)
  }
  
  # save the current models and organized data
  file_path <- file.path(work_dir, paste(model_current_prefix, as.character(flag), ".RData", sep = ""))
  save(list = c("equating_result", 
                "model_lm", "model_lm_item", 
                "model_rf", "model_rf_item",
                "model_svm", "model_svm_item"), file = file_path)
}

# save the final benchmarking table
save(df_rmse, df_mae, file = file.path(work_dir, benchmark_filename))
