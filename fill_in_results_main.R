fill_in_results_main <- function(flag, model_flag, df_rmse, df_mae, rmse, rmse_b, mae, mae_b) {
  # fill in the benchmark result tables of the HAMD2MADRS_alpha.R
  # these tables are df_mae and df_rmse
  #
  # Written by Xiao Chen 240604
  # chenxiaophd@gmail.com
  if(flag == 1){
    df_rmse[model_flag,'b'] <- rmse
  }else if(flag == 2){
    df_rmse[model_flag,'t30'] <- rmse
    df_rmse[model_flag,'t30_b'] <- rmse_b
  }else if(flag == 3){
    df_rmse[model_flag,'f1'] <- rmse
    df_rmse[model_flag,'f1_b'] <- rmse_b
  }else if(flag == 4){
    df_rmse[model_flag,'f2'] <- rmse
    df_rmse[model_flag,'f2_b'] <- rmse_b
  }else if(flag == 5){
    df_rmse[model_flag,'f3'] <- rmse
    df_rmse[model_flag,'f3_b'] <- rmse_b
  }else if(flag == 6){
    df_rmse[model_flag,'delta'] <- rmse
    df_rmse[model_flag,'delta_b'] <- rmse_b
  }
  
  if(flag == 1){
    df_mae[model_flag,'b'] <- mae
  }else if(flag == 2){
    df_mae[model_flag,'t30'] <- mae
    df_mae[model_flag,'t30_b'] <- mae_b
  }else if(flag == 3){
    df_mae[model_flag,'f1'] <- mae
    df_mae[model_flag,'f1_b'] <- mae_b
  }else if(flag == 4){
    df_mae[model_flag,'f2'] <- mae
    df_mae[model_flag,'f2_b'] <- mae_b
  }else if(flag == 5){
    df_mae[model_flag,'f3'] <- mae
    df_mae[model_flag,'f3_b'] <- mae_b
  }else if(flag == 6){
    df_mae[model_flag,'delta'] <- mae
    df_mae[model_flag,'delta_b'] <- mae_b
  }
  return(list(df_rmse = df_rmse, df_mae = df_mae))
}