
##############################################################################
######## Functions to apply logisit regression on application City Cash ######
##############################################################################

gen_app_bgm_citycash <- function(df,scoring_df,products,period,all_df,
    prev_amount,amount_tab,t_income,disposable_income_adj,base_dir){
  
  # Load rdata
  load(file.path(base_dir,"rdata","citycash_gbm_app.rdata"))
  
  # Set fields
  df$self_approve <- df$self_approval
  df$cession_total <- df$amount_cession_total
  df$city_pop_cat <- 
    ifelse(is.na(df$city_pop),"no_info",
    ifelse(df$city_pop<=2000,"0_2k",
    ifelse(df$city_pop<=50000,"2k_50k",
    ifelse(df$city_pop<=200000,"50k_200k",
    ifelse(df$city_pop<=1000000,"200k_1M","1M+")))))
  df$city_dwel_type <- paste(df$dwelling_type,df$city_pop_cat,sep="|")
  df$flag_location <- 
    ifelse(is.na(df$risky_address),NA,
    ifelse(df$risky_address==1,"1","0"))
  df$payment_method <- df$payment_method2
  
  # Set missing data
  na_cols <- c("status_work","hear_about_us","education","city_dwel_type",
     "age","cession_total","self_approve","gender","amount_drawn_total",
     "distance_office","on_address","marital_status","purpose",
     "outs_overdue_ratio_total","flag_location","status_finished_total",
     "current_status_active_total","has_viber","status_active_total",
     "total_income","cred_count_total","ownership","codebtor_tot",
     "guarantor_tot","monthly_installment_total","household_children",
     "household_total","city_pop_cat","source_entity_count_total",
     "payment_method","city_pop","experience_employer")
  for (col in na_cols) {
    df[is.na(df[,col]),col] <- -999
  }
  
  # Factor certain variables
  asfac <- c("ownership","education","marital_status","has_viber",
    "status_work","purpose","hear_about_us","payment_method","gender",
    "self_approve","current_status_active_total","status_active_total",
    "status_finished_total","flag_location","city_pop_cat",
    "city_dwel_type")
  df[,c(asfac)] <- lapply(df[,c(asfac)], factor)
  
  if(df$total_income<100 | is.na(df$total_income)){
    ratio_tab <- 0.08
  } else {
    ratio_tab <- all_df$installment_amount/t_income
  }
  ratio_tab <- as.numeric(ratio_tab)
  df$ratio_installment_income <- ifelse(is.na(ratio_tab),-999,ratio_tab)

  apply_gbm <- suppressMessages(predict(citycash_gbm_app,df,type = "response"))
  pd <- round(apply_gbm,3)
  score <- gen_group_scores(pd,all_df$office_id,0,0,0)
  return(list(pd,score))
}


