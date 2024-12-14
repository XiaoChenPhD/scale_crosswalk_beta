# make the conversion tables
#
# Xiao Chen
# 241214
# chenxiaophd@gmail.com


# clear everything
rm(list = ls())
cat("\014")

output_dir <- paste("/Users/ChenXiao/Library/CloudStorage/OneDrive-Personal/Documents", 
                    "/scale_crosswalk/Analysis/major_revision/conversion", sep = "")
data_dir <- "/Users/ChenXiao/Documents/project_codes/scale_crosswalk_beta/scale_crosswalk_shiny/data"
column_names <- c('Baseline', 'T30', 'Follow-up 1', 'Follow-up 2', 'Follow-up 3')
df_conversion_H2M <- as.data.frame(matrix(nrow = 53, ncol = length(column_names)))
colnames(df_conversion_H2M) <- column_names
df_conversion_M2H <- as.data.frame(matrix(nrow = 61, ncol = length(column_names)))
colnames(df_conversion_M2H) <- column_names

################# HRSD to MADRS #########################
predictor_data <- seq(0, 52)

# rTMS equipercentile model
for (flag in c(1,2,3,4,5)){
  load(file.path(data_dir, paste("data_model_ALL_carryover_", as.character(flag), ".RData", sep = "")))
  responses <- equate(predictor_data, y = equating_result)
  # round the predicted values and make sure they are in the meaningful ranges
  responses <- round(responses, digits = 0)
  responses[responses > 60] <- 60
  if (flag == 6){
    responses[responses < -60] <- -60
  } else {
    responses[responses < 0] <- sprintf("%.2f", 0)
  }
  df_conversion_H2M[ ,flag] <- responses
}
write.csv(df_conversion_H2M, paste(output_dir, "/rTMS_percentile_H2M.csv", sep = ""), row.names = FALSE)

# linear regression
for (flag in c(1,2,3,4,5)){
  load(file.path(data_dir, paste("data_model_ALL_carryover_", as.character(flag), ".RData", sep = "")))
  
  df_predictor <- data.frame(hrsd_total = predictor_data)
  responses <- predict(model_lm, newdata = df_predictor)
  # round the predicted values and make sure they are in the meaningful ranges
  responses <- round(responses, digits = 0)
  responses[responses > 60] <- 60
  if (flag == 6){
    responses[responses < -60] <- -60
  } else {
    responses[responses < 0] <- sprintf("%.2f", 0)
  }
  df_conversion_H2M[ ,flag] <- responses
}
write.csv(df_conversion_H2M, paste(output_dir, "/rTMS_LR_H2M.csv", sep = ""), row.names = FALSE)

# RF regression
for (flag in c(1,2,3,4,5)){
  load(file.path(data_dir, paste("data_model_ALL_carryover_", as.character(flag), ".RData", sep = "")))
  
  df_predictor <- data.frame(hrsd_total = predictor_data)
  responses <- predict(model_rf, newdata = df_predictor)
  # round the predicted values and make sure they are in the meaningful ranges
  responses <- round(responses, digits = 0)
  responses[responses > 60] <- 60
  if (flag == 6){
    responses[responses < -60] <- -60
  } else {
    responses[responses < 0] <- sprintf("%.2f", 0)
  }
  df_conversion_H2M[ ,flag] <- responses
}
write.csv(df_conversion_H2M, paste(output_dir, "/rTMS_RF_H2M.csv", sep = ""), row.names = FALSE)

# SVR
for (flag in c(1,2,3,4,5)){
  load(file.path(data_dir, paste("data_model_ALL_carryover_", as.character(flag), ".RData", sep = "")))
  
  df_predictor <- data.frame(hrsd_total = predictor_data)
  responses <- predict(model_svm, newdata = df_predictor)
  # round the predicted values and make sure they are in the meaningful ranges
  responses <- round(responses, digits = 0)
  responses[responses > 60] <- 60
  if (flag == 6){
    responses[responses < -60] <- -60
  } else {
    responses[responses < 0] <- sprintf("%.2f", 0)
  }
  df_conversion_H2M[ ,flag] <- responses
}
write.csv(df_conversion_H2M, paste(output_dir, "/rTMS_SVR_H2M.csv", sep = ""), row.names = FALSE)

###################### MADRS to HRSD ##################
predictor_data <- seq(0, 60)

# rTMS equipercentile model
for (flag in c(1,2,3,4,5)){
  load(file.path(data_dir, paste("data_model_M2H_ALL_carryover_", as.character(flag), ".RData", sep = "")))
  responses <- equate(predictor_data, y = equating_result)
  # round the predicted values and make sure they are in the meaningful ranges
  responses <- round(responses, digits = 0)
  responses[responses > 52] <- 52
  if (flag == 6){
    responses[responses < -52] <- -52
  } else {
    responses[responses < 0] <- sprintf("%.2f", 0)
  }
  df_conversion_M2H[ ,flag] <- responses
}
write.csv(df_conversion_M2H, paste(output_dir, "/rTMS_percentile_M2H.csv", sep = ""), row.names = FALSE)

# linear regression
for (flag in c(1,2,3,4,5)){
  load(file.path(data_dir, paste("data_model_M2H_ALL_carryover_", as.character(flag), ".RData", sep = "")))
  df_predictor <- data.frame(madrs_total = predictor_data)
  responses <- predict(model_lm, newdata = df_predictor)
  # round the predicted values and make sure they are in the meaningful ranges
  responses <- round(responses, digits = 0)
  responses[responses > 52] <- 52
  if (flag == 6){
    responses[responses < -52] <- -52
  } else {
    responses[responses < 0] <- sprintf("%.2f", 0)
  }
  df_conversion_M2H[ ,flag] <- responses
}
write.csv(df_conversion_M2H, paste(output_dir, "/rTMS_LR_M2H.csv", sep = ""), row.names = FALSE)

# random forest
for (flag in c(1,2,3,4,5)){
  load(file.path(data_dir, paste("data_model_M2H_ALL_carryover_", as.character(flag), ".RData", sep = "")))
  df_predictor <- data.frame(madrs_total = predictor_data)
  responses <- predict(model_rf, newdata = df_predictor)
  # round the predicted values and make sure they are in the meaningful ranges
  responses <- round(responses, digits = 0)
  responses[responses > 52] <- 52
  if (flag == 6){
    responses[responses < -52] <- -52
  } else {
    responses[responses < 0] <- sprintf("%.2f", 0)
  }
  df_conversion_M2H[ ,flag] <- responses
}
write.csv(df_conversion_M2H, paste(output_dir, "/rTMS_RF_M2H.csv", sep = ""), row.names = FALSE)

# SVR
for (flag in c(1,2,3,4,5)){
  load(file.path(data_dir, paste("data_model_M2H_ALL_carryover_", as.character(flag), ".RData", sep = "")))
  df_predictor <- data.frame(madrs_total = predictor_data)
  responses <- predict(model_svm, newdata = df_predictor)
  # round the predicted values and make sure they are in the meaningful ranges
  responses <- round(responses, digits = 0)
  responses[responses > 52] <- 52
  if (flag == 6){
    responses[responses < -52] <- -52
  } else {
    responses[responses < 0] <- sprintf("%.2f", 0)
  }
  df_conversion_M2H[ ,flag] <- responses
}
write.csv(df_conversion_M2H, paste(output_dir, "/rTMS_SVR_M2H.csv", sep = ""), row.names = FALSE)

############## delta scores ###################
# HRSD to MADRS
column_names <- c('percentile', 'linear regression', 'random forest', 'svm regression')
predictor_data <- seq(-27, 5)
df_delta_H2M <- as.data.frame(matrix(nrow = length(predictor_data), ncol = length(column_names)))
colnames(df_delta_H2M) <- column_names

load(file.path(data_dir, "data_model_ALL_carryover_6.RData"))

# rTMS equipercentile model
responses <- equate(predictor_data, y = equating_result)
# round the predicted values and make sure they are in the meaningful ranges
responses <- round(responses, digits = 0)
responses[responses > 60] <- 60
responses[responses < -60] <- -60
df_delta_H2M[ ,1] <- responses
# linear regression
df_predictor <- data.frame(hrsd_total = predictor_data)
responses <- predict(model_lm, newdata = df_predictor)
# round the predicted values and make sure they are in the meaningful ranges
responses <- round(responses, digits = 0)
responses[responses > 60] <- 60
responses[responses < -60] <- -60
df_delta_H2M[ ,2] <- responses
# RF
df_predictor <- data.frame(hrsd_total = predictor_data)
responses <- predict(model_rf, newdata = df_predictor)
# round the predicted values and make sure they are in the meaningful ranges
responses <- round(responses, digits = 0)
responses[responses > 60] <- 60
responses[responses < -60] <- -60
df_delta_H2M[ ,3] <- responses
# SVR
df_predictor <- data.frame(hrsd_total = predictor_data)
responses <- predict(model_svm, newdata = df_predictor)
# round the predicted values and make sure they are in the meaningful ranges
responses <- round(responses, digits = 0)
responses[responses > 60] <- 60
responses[responses < -60] <- -60
df_delta_H2M[ ,4] <- responses
write.csv(df_delta_H2M, paste(output_dir, "/delta_H2M.csv", sep = ""), row.names = FALSE)


# and vise versa
column_names <- c('percentile', 'linear regression', 'random forest', 'svm regression')
predictor_data <- seq(-37, 8)
df_delta_M2H <- as.data.frame(matrix(nrow = length(predictor_data), ncol = length(column_names)))
colnames(df_delta_M2H) <- column_names

load(file.path(data_dir, "data_model_M2H_ALL_carryover_6.RData"))

# rTMS equipercentile model
responses <- equate(predictor_data, y = equating_result)
# round the predicted values and make sure they are in the meaningful ranges
responses <- round(responses, digits = 0)
responses[responses > 52] <- 52
responses[responses < -52] <- -52
df_delta_M2H[ ,1] <- responses
# linear regression
df_predictor <- data.frame(madrs_total = predictor_data)
responses <- predict(model_lm, newdata = df_predictor)
# round the predicted values and make sure they are in the meaningful ranges
responses <- round(responses, digits = 0)
responses[responses > 52] <- 52
responses[responses < -52] <- -52
df_delta_M2H[ ,2] <- responses
# RF
df_predictor <- data.frame(madrs_total = predictor_data)
responses <- predict(model_rf, newdata = df_predictor)
# round the predicted values and make sure they are in the meaningful ranges
responses <- round(responses, digits = 0)
responses[responses > 52] <- 52
responses[responses < -52] <- -52
df_delta_M2H[ ,3] <- responses
# SVR
df_predictor <- data.frame(madrs_total = predictor_data)
responses <- predict(model_svm, newdata = df_predictor)
# round the predicted values and make sure they are in the meaningful ranges
responses <- round(responses, digits = 0)
responses[responses > 52] <- 52
responses[responses < -52] <- -52
df_delta_M2H[ ,4] <- responses
write.csv(df_delta_M2H, paste(output_dir, "/delta_M2H.csv", sep = ""), row.names = FALSE)
