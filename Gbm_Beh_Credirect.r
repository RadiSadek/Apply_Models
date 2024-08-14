
#########################################################################
######## Functions to apply logisit regression on repeat Credirect ######
#########################################################################

gen_beh_gbm_credirect <- function(df,scoring_df,products,df_Log_beh_Credirect,
    period,all_df,prev_amount,amount_tab,
    t_income,disposable_income_adj,crit_po,
    flag_new_credirect_old_city,api_df,base_dir){
  
  # Load rdata
  load(file.path(base_dir,"rdata","credirect_repeat_gbm.rdata"))
  
  #  Cut and bin those necessary
  df$max_delay <- df$max_delay[[1]]
  api_df$payment_method <- as.numeric(api_df$payment_method)
  df$API_payment_method <- api_df$payment_method

  device_type <- ifelse(grepl("Android",api_df$user_agent),"Android",
     ifelse(grepl("iPhone",api_df$user_agent),"iPhone",
     ifelse(grepl("Win64",api_df$user_agent),"Windows",
     ifelse(grepl("Windows",api_df$user_agent),"Windows",
     ifelse(grepl("Linux",api_df$user_agent),"Linux","other")))))
  df$API_device <- device_type
  
  df$API_hour_app <- as.numeric(substring(api_df$partial_app_at,12,13))
  
  amount_diff_loc <- ifelse(!is.na(api_df$amount),
     as.numeric(api_df$amount) - prev_amount$amount,NA)
  df$API_amount_diff <- amount_diff_loc
  
  api_df$period <- as.numeric(api_df$period)
  df$API_installments <- api_df$period
  
  df$API_referral_source <- api_df$referral_source
  
  df$flag_location <- df$risky_address
  
  df$cession_total <- df$amount_cession_total
  
  df$city_pop_cat <- 
    ifelse(is.na(df$city_pop),"no_info",
    ifelse(df$city_pop<=2000,"0_2k",
    ifelse(df$city_pop<=50000,"2k_50k",
    ifelse(df$city_pop<=200000,"50k_200k",
    ifelse(df$city_pop<=1000000,"200k_1M","1M+")))))

  df$refinance_previous <- ifelse(is.na(df$days_diff_last_credit),1,
    ifelse(df$days_diff_last_credit<=2,1,0))
  
  # Set missing data
  na_cols <- c("max_delay","API_installments","credits_cum",
      "outs_overdue_ratio_total","days_diff_last_credit","city_pop",
      "API_amount_diff","source_entity_count_total","prev_other_brand",
      "education","API_device","status_work","age","API_payment_method",
      "purpose","ratio_nb_payments_prev","hear_about_us",
      "status_active_total","cred_count_total","API_hour_app",
      "status_finished_total","API_referral_source",
      "experience_employer","city_pop_cat","refinance_ratio","on_address",
       "cession_total","phone_plan","has_viber","other_bank_accounts",
       "flag_high_last_paid","marital_status","ownership","household_children",
       "dwelling_type","household_total","flag_location","gender",
       "leasing","refinance_previous")
  for (col in na_cols) {
    df[is.na(df[,col]),col] <- -999
  }
  
  # Factor certain variables
  asfac <- c("flag_location","gender","education","marital_status","ownership",
    "flag_high_last_paid","leasing","status_work","purpose","dwelling_type",
    "phone_plan","hear_about_us","status_active_total","API_device",
    "status_finished_total","city_pop_cat","has_viber","refinance_previous",
    "API_referral_source","prev_other_brand","API_payment_method")
  df[,c(asfac)] <- lapply(df[,c(asfac)], factor)
  
  # Apply model
  for(i in 1:nrow(scoring_df)){
    
    period_tab <- as.numeric(scoring_df$period[i])
    amount_tab <- as.numeric(scoring_df$amount[i])
    
    apply_gbm <- suppressMessages(predict(new_model,df,type = "response"))
    scoring_df$score[i] <- apply_gbm
    scoring_df$score[i] <- gen_group_scores(scoring_df$score[i],
        all_df$office_id,1,1,2)
    scoring_df$pd[i] <- round(apply_gbm,3)
    
    # Compute flag of disposable income
    product_tab <- subset(products, products$product_id==all_df$product_id & 
       products$period==as.numeric(period_tab) &
       products$amount==as.numeric(amount_tab))
    scoring_df$color[i] <- ifelse(scoring_df$score[i]=="Bad", 1, 
       ifelse(scoring_df$score[i]=="Indeterminate", 2,
       ifelse(scoring_df$score[i]=="Good 1", 3,
       ifelse(scoring_df$score[i]=="Good 2", 4,
       ifelse(scoring_df$score[i]=="Good 3", 5,
       ifelse(scoring_df$score[i]=="Good 4",6,NA))))))
    
    if(crit_po!=0){
      scoring_df$installment_amount[i] <- product_tab$installment_amount
    }
  }
  return(scoring_df)
  print("here")
}


  
  