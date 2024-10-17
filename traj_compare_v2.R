# compare the real clinical scores and predicted clinical scores from different models
#
# Xiao Chen
# 240515
#
# Modified 240708
# Only include the final predicting method
# Xiao Chen
# chenxiaophd@gmail.com

# clear everything
rm(list = ls())
cat("\014")

library(openxlsx)
library(tidyr)
library(dplyr)
library(ggplot2)
library(equate)
library(randomForest)
library(e1071)

data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

work_dir <- "/Users/ChenXiao/Library/CloudStorage/OneDrive-Personal/Documents/scale_crosswalk/Analysis"
output_dir <- "/Users/ChenXiao/Library/CloudStorage/OneDrive-Personal/Documents/scale_crosswalk/all_carryover_term_updated"
column_names <- c('subj_id', 
                  'Real_Baseline', 'Pharmacotherapy Equipercentile Model_Baseline', 'rTMS Equipercentile Model_Baseline', 
                  'Linear Regression_Baseline', 'RF Regression_Baseline','SVR_Baseline',
                  'Real_T30', 'Pharmacotherapy Equipercentile Model_T30', 'rTMS Equipercentile Model_T30', 
                  'Linear Regression_T30', 'RF Regression_T30','SVR_T30', 
                  'Real_Follow-up 1', 'Pharmacotherapy Equipercentile Model_Follow-up 1', 'rTMS Equipercentile Model_Follow-up 1', 
                  'Linear Regression_Follow-up 1', 'RF Regression_Follow-up 1', 'SVR_Follow-up 1',
                  'Real_Follow-up 2', 'Pharmacotherapy Equipercentile Model_Follow-up 2', 'rTMS Equipercentile Model_Follow-up 2', 
                  'Linear Regression_Follow-up 2', 'RF Regression_Follow-up 2', 'SVR_Follow-up 2',
                  'Real_Follow-up 3', 'Pharmacotherapy Equipercentile Model_Follow-up 3', 'rTMS Equipercentile Model_Follow-up 3', 
                  'Linear Regression_Follow-up 3', 'RF Regression_Follow-up 3', 'SVR_Follow-up 3')
col_num = 6 # the column number at each time point
custom_colors <- c("Pharmacotherapy Equipercentile Model" = "#6C946F", "Linear Regression" = "#D4BDAC", "rTMS Equipercentile Model" = "#FFA823", 
                   "RF Regression" = "#DC0083", "SVR" = "#536493",
                   "Real" = "#3AA6B9")

# load the data at time point 1 to get the subj list
load(file.path(work_dir, "data_model_M2H_ALL_carryover_1_imputed_data.RData"))
data_full <- as.data.frame(matrix(nrow = nrow(data_binded_imp), ncol = length(column_names)))
colnames(data_full) <- column_names
data_full$subj_id <- data_binded_imp$subj_id

data_full_b <- as.data.frame(matrix(nrow = nrow(data_binded_imp), ncol = length(column_names)))
colnames(data_full_b) <- column_names
data_full_b$subj_id <- data_binded_imp$subj_id

#################### use true hrsd results to predict the madrs results ###############################
for (flag in c(1,2,3,4,5)){
  load(file.path(work_dir, paste("data_model_ALL_carryover_", as.character(flag), ".RData", sep = "")))
  load(file.path(work_dir, "model_baseline_ALL_carryover.RData"))
  load(file.path(work_dir, paste("data_model_ALL_carryover_", as.character(flag), "_imputed_data.RData", sep = "")))
  # real madrs data
  data_full[, (flag-1)*col_num + 2] <- data_binded_imp$madrs_total
  data_full_b[, (flag-1)*col_num + 2] <- data_binded_imp$madrs_total
  
  # Leucht_2018 method
  conversion_table <- read.xlsx(file.path(work_dir, "coversion_table_Leucht2018.xlsx"))
  predictions <- c()
  for (i in seq_along(data_binded_imp$hrsd_total)){
    if (data_binded_imp$hrsd_total[[i]] < -5){
      # the original conversion table only listed delta_HAMD >= -5
      predictions <- c(predictions, -8)
    }else{
      predictions <- c(predictions, 
                       conversion_table$MADRS[match(data_binded_imp$hrsd_total[[i]], conversion_table$HAMD)])
    }
  }
  data_full[,(flag-1)*col_num + 3] <- predictions
  data_full_b[,(flag-1)*col_num + 3] <- predictions
  
  # percentile method
  predictions <- c()
  predictions <- equate(data_binded_imp$hrsd_total, y = equating_result)
  data_full[,(flag-1)*col_num + 4] <- predictions
  
  predictions <- c()
  predictions <- equate(data_binded_imp$hrsd_total, y = equating_result_b)
  data_full_b[,(flag-1)*col_num + 4] <- predictions
  
  # linear regression
  predictions <- c()
  predictions <- predict(model_lm, newdata = data_binded_imp)
  data_full[,(flag-1)*col_num + 5] <- predictions
  
  predictions <- c()
  predictions <- predict(model_lm_b, newdata = data_binded_imp)
  data_full_b[,(flag-1)*col_num + 5] <- predictions
  
  # random forest
  predictions <- c()
  predictions <- predict(model_rf, newdata = data_binded_imp)
  data_full[,(flag-1)*col_num + 6] <- predictions
  
  predictions <- c()
  predictions <- predict(model_rf_b, newdata = data_binded_imp)
  data_full_b[,(flag-1)*col_num + 6] <- predictions
  
  # svm
  predictions <- c()
  cost <- 1
  x_test <- select(data_binded_imp, c(hrsd_total))
  predictions <- predict(model_svm, x_test)
  data_full[,(flag-1)*col_num + 7] <- predictions
  
  predictions <- c()
  cost <- 1
  x_test <- select(data_binded_imp, c(hrsd_total))
  predictions <- predict(model_svm_b, x_test)
  data_full_b[,(flag-1)*col_num + 7] <- predictions
}

# organize data using timed models
data_long <- data_full %>%
  pivot_longer(
    cols = -subj_id, # Exclude the first column (assumed to be 'id' in this example)
    names_to = c("Group", ".value"),
    names_sep = "_"
  ) %>% pivot_longer(
    cols = c("Baseline","T30","Follow-up 1","Follow-up 2","Follow-up 3"),
    names_to = "Time",
    values_to = "Value"
  )
df_summary <- data_summary(data_long, varname="Value", 
                           groupnames=c("Time", "Group"))
df_summary$Group <- as.factor(df_summary$Group)
df_summary$Time <- factor(df_summary$Time, levels = c("Baseline","T30","Follow-up 1","Follow-up 2","Follow-up 3"))

# organize results from baseline models
data_long_b <- data_full_b %>%
  pivot_longer(
    cols = -subj_id, # Exclude the first column (assumed to be 'id' in this example)
    names_to = c("Group", ".value"),
    names_sep = "_"
  ) %>% pivot_longer(
    cols = c("Baseline","T30","Follow-up 1","Follow-up 2","Follow-up 3"),
    names_to = "Time",
    values_to = "Value"
  )
df_summary_b <- data_summary(data_long_b, varname="Value", 
                           groupnames=c("Time", "Group"))
df_summary_b$Group <- as.factor(df_summary_b$Group)
df_summary_b$Time <- factor(df_summary_b$Time, levels = c("Baseline","T30","Follow-up 1","Follow-up 2","Follow-up 3"))

#plot timed model results
ggplot(df_summary, aes(x= Time, y=Value, group=Group, color=Group)) + 
  geom_line(size = 1) + xlab("Time") + ylab("MADRS score") +
  geom_point(size = 3) +
  scale_y_continuous(limits = c(14, 30), breaks = seq(15, 30, by = 5)) +
  scale_color_manual(values = custom_colors) +
  theme_classic(base_size = 20) + 
  theme(legend.text = element_text(size = 12), axis.text.x = element_text(angle = 45, hjust = 0.95))

ggsave(file.path(output_dir, "timed_model_legend.png"), 
       width = 8, height = 6, dpi = 1200)

# plot baseline models results
ggplot(df_summary_b, aes(x= Time, y=Value, group=Group, color=Group)) + 
  geom_line(size = 1) + xlab("Time") + ylab("MADRS score") +
  geom_point(size = 3) +
  scale_y_continuous(limits = c(14, 30), breaks = seq(15, 30, by = 5)) +
  scale_color_manual(values = custom_colors) +
  theme_classic(base_size = 20) + 
  theme(legend.text = element_text(size = 12), axis.text.x = element_text(angle = 45, hjust = 0.95))

ggsave(file.path(output_dir, "baseline_model_v2.png"), 
       width = 8, height = 6, dpi = 1200)


#################### use true madrs results to predict the hrsd results ###############################
for (flag in c(1,2,3,4,5)){
  load(file.path(work_dir, paste("data_model_M2H_ALL_carryover_", as.character(flag), ".RData", sep = "")))
  load(file.path(work_dir, "model_baseline_M2H_ALL_carryover.RData"))
  load(file.path(work_dir, paste("data_model_M2H_ALL_carryover_", as.character(flag), "_imputed_data.RData", sep = "")))
  # real hrsd data
  data_full[, (flag-1)*col_num + 2] <- data_binded_imp$hrsd_total
  data_full_b[, (flag-1)*col_num + 2] <- data_binded_imp$hrsd_total
  
  # Leucht_2018 method
  conversion_table <- read.xlsx(file.path(work_dir, "coversion_table_Leucht2018.xlsx"))
  predictions <- c()
  for (i in seq_along(data_binded_imp$madrs_total)){
    if (data_binded_imp$madrs_total[[i]] < -8){
      # the original conversion table only listed delta_MADRS >= -8
      predictions <- c(predictions, -5)
    }else if (data_binded_imp$madrs_total[[i]] > 0 & data_binded_imp$madrs_total[[i]] < 3){
      # The original conversion table did not list madrs = 1 or madrs = 2, I arbitrarily set it as 2
      predictions <- c(predictions, 2)
    }else{
      predictions <- c(predictions, conversion_table$HAMD[match(data_binded_imp$madrs_total[[i]], conversion_table$MADRS)])
    }
  }
  data_full[,(flag-1)*col_num + 3] <- predictions
  data_full_b[,(flag-1)*col_num + 3] <- predictions
  
  # percentile method
  predictions <- c()
  predictions <- equate(data_binded_imp$madrs_total, y = equating_result)
  data_full[,(flag-1)*col_num + 4] <- predictions
  
  predictions <- c()
  predictions <- equate(data_binded_imp$madrs_total, y = equating_result_b)
  data_full_b[,(flag-1)*col_num + 4] <- predictions
  
  # linear regression
  predictions <- c()
  predictions <- predict(model_lm, newdata = data_binded_imp)
  data_full[,(flag-1)*col_num + 5] <- predictions
  
  predictions <- c()
  predictions <- predict(model_lm_b, newdata = data_binded_imp)
  data_full_b[,(flag-1)*col_num + 5] <- predictions
  
  # random forest
  predictions <- c()
  predictions <- predict(model_rf, newdata = data_binded_imp)
  data_full[,(flag-1)*col_num + 6] <- predictions
  
  predictions <- c()
  predictions <- predict(model_rf_b, newdata = data_binded_imp)
  data_full_b[,(flag-1)*col_num + 6] <- predictions
  
  # svm
  predictions <- c()
  cost <- 1
  x_test <- select(data_binded_imp, c(madrs_total))
  predictions <- predict(model_svm, x_test)
  data_full[,(flag-1)*col_num + 7] <- predictions
  
  predictions <- c()
  cost <- 1
  x_test <- select(data_binded_imp, c(madrs_total))
  predictions <- predict(model_svm_b, x_test)
  data_full_b[,(flag-1)*col_num + 7] <- predictions
  
}

# organize data using timed models
data_long <- data_full %>%
  pivot_longer(
    cols = -subj_id, # Exclude the first column (assumed to be 'id' in this example)
    names_to = c("Group", ".value"),
    names_sep = "_"
  ) %>% pivot_longer(
    cols = c("Baseline","T30","Follow-up 1","Follow-up 2","Follow-up 3"),
    names_to = "Time",
    values_to = "Value"
  )
df_summary <- data_summary(data_long, varname="Value", 
                           groupnames=c("Time", "Group"))
df_summary$Group <- as.factor(df_summary$Group)
df_summary$Time <- factor(df_summary$Time, levels = c("Baseline","T30","Follow-up 1","Follow-up 2","Follow-up 3"))

# organize results from baseline models
data_long_b <- data_full_b %>%
  pivot_longer(
    cols = -subj_id, # Exclude the first column (assumed to be 'id' in this example)
    names_to = c("Group", ".value"),
    names_sep = "_"
  ) %>% pivot_longer(
    cols = c("Baseline","T30","Follow-up 1","Follow-up 2","Follow-up 3"),
    names_to = "Time",
    values_to = "Value"
  )
df_summary_b <- data_summary(data_long_b, varname="Value", 
                             groupnames=c("Time", "Group"))
df_summary_b$Group <- as.factor(df_summary_b$Group)
df_summary_b$Time <- factor(df_summary_b$Time, levels = c("Baseline","T30","Follow-up 1","Follow-up 2","Follow-up 3"))

#plot timed model results
ggplot(df_summary, aes(x= Time, y=Value, group=Group, color=Group)) + 
  geom_line(size = 1) + xlab("Time") + ylab("HRSD score") +
  geom_point(size = 3) +
  scale_y_continuous(limits = c(10, 25), breaks = seq(10, 25, by = 5)) +
  scale_color_manual(values=custom_colors) +
  theme_classic(base_size = 20) + 
  theme(legend.text = element_text(size = 12), axis.text.x = element_text(angle = 45, hjust = 0.95))

ggsave(file.path(output_dir, "timed_model_MADRS2HAMD_v2.png"), width = 8, height = 6, dpi = 1200)

# plot baseline models results
ggplot(df_summary_b, aes(x= Time, y=Value, group=Group, color=Group)) + 
  geom_line(size = 1) + xlab("Time") + ylab("HRSD score") +
  geom_point(size = 3) +
  scale_y_continuous(limits = c(10, 25), breaks = seq(10, 25, by = 5)) +
  scale_color_manual(values=custom_colors) +
  theme_classic(base_size = 20) + 
  theme(legend.text = element_text(size = 12), axis.text.x = element_text(angle = 45, hjust = 0.95))

ggsave(file.path(output_dir, "baseline_model_MADRS2HAMD_v2.png"), width = 8, height = 6, dpi = 1200)
