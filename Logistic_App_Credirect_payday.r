
##############################################################################
######## Functions to apply logisit regression on application Credirect ######
##############################################################################

gen_app_credirect_payday <- function(df,scoring_df,products,
  df_Log_Credirect_App_payday,period,all_df,prev_amount,amount_tab,
  t_income,disposable_income_adj,flag_credit_next_salary,api_df,base_dir){
  
  
  # Load rdata
  load(file.path(base_dir,"rdata","credirect_payday_coeffs.rdata"))
  names(coefficients) <- c("coeff")
  
  # Cut and bin
  df$age_cut <- 
    ifelse(df$age<=20,"18_20","21+")
  df$age <- as.factor(df$age_cut)
  
  df$gender <- as.factor(df$gender)
  
  df$ownership_cut <- ifelse(df$ownership %in% c(1), "1", 
    ifelse(is.na(df$ownership), "not_1", "not_1"))
  df$ownership <- as.factor(df$ownership_cut)
  
  df$education_cut <- 
    ifelse(is.na(df$education),"2_3",
    ifelse(df$education %in% c(2,3),"2_3",df$education))
  df$education <- as.factor(df$education_cut)
  
  df$marital_status_cut <- ifelse(is.na(df$marital_status), "other",
    ifelse(df$marital_status %in% c(1,3,4), "1_3_4","other"))
  df$marital_status <- as.factor(df$marital_status_cut)
  
  df$purpose_cut <- ifelse(is.na(df$purpose), "other",
    ifelse(df$purpose %in% c(4,5),"2_5",
    ifelse(df$purpose %in% c(6),"6","other")))
  df$purpose <- as.factor(df$purpose_cut)
  
  df$status_work_cut <- ifelse(is.na(df$status_work), "other",
    ifelse(df$status_work %in% c(4,5,9,10,12), "4_5_9_10_12","other"))
  df$status_work <- as.factor(df$status_work_cut)
  
  df$status_finished_total_cut <- ifelse(is.na(df$status_finished_total),
    "0_71_72_73",ifelse(df$status_finished_total %in% c(0,71,72,73),"0_71_72_73",
    "74_75"))
  df$status_finished_total <- as.factor(df$status_finished_total_cut)
  
  df$outs_overdue_ratio_total_cut <- ifelse(
    is.na(df$outs_overdue_ratio_total),"other",
    ifelse(df$outs_overdue_ratio_total==-999,"other",
    ifelse(df$outs_overdue_ratio_total<=0.01,"0_0.01",
    ifelse(df$outs_overdue_ratio_total<=0.05,"other","0.05_1"))))
  df$outs_overdue_ratio_total <- as.factor(df$outs_overdue_ratio_total_cut)
  
  df$has_viber_cut <- 
    ifelse(is.na(df$has_viber), "has_viber",
    ifelse(df$has_viber=="False", "no_viber","has_viber"))
  df$has_viber <- as.factor(df$has_viber_cut)
  
  api_df$amount <- as.numeric(api_df$amount)
  df$API_amount_cut <- 
    ifelse(is.na(api_df$amount),"300_800",
    ifelse(api_df$amount<=150,"less_150",
    ifelse(api_df$amount<=300,"200_300",
    ifelse(api_df$amount<=800,"300_800","800+"))))
  df$API_amount <- as.factor(df$API_amount_cut)
  
  api_df$payment_method <- as.numeric(api_df$payment_method)
  df$API_payment_method_cut <- ifelse(is.na(api_df$payment_method), "other",
    ifelse(api_df$payment_method==2, "2",
    ifelse(api_df$payment_method==3, "other", "other")))
  df$API_payment_method <- as.factor(df$API_payment_method_cut)
  
  df$city_pop_cut <- 
    ifelse(is.na(df$city_pop),"other",
    ifelse(df$city_pop<=2000,"other",
    ifelse(df$city_pop<=300000,"other","300k+")))
  df$city_pop <- as.factor(df$city_pop_cut)
  
  df$phone_plan_cut <- 
    ifelse(is.na(df$phone_plan),"other",
    ifelse(df$phone_plan==2,"2","other"))
  df$phone_plan <- as.factor(df$phone_plan_cut)
  
  df$leasing_cut <- 
    ifelse(is.na(df$leasing),"other",
    ifelse(df$leasing %in% c(2,3),"2_3","other"))
  df$leasing <- as.factor(df$leasing_cut)
  
  device_type <- ifelse(grepl("Android",api_df$user_agent),"Android",
    ifelse(grepl("iPhone",api_df$user_agent),"iPhone",
    ifelse(grepl("Win64",api_df$user_agent),"Windows",
    ifelse(grepl("Windows",api_df$user_agent),"Windows",
    ifelse(grepl("Linux",api_df$user_agent),"Linux","other")))))
  df$API_device_cut <- 
    ifelse(is.na(device_type),"other",
    ifelse(device_type %in% c("Windows"),"Windows","other"))
  df$API_device <- as.factor(df$API_device_cut)
  
  
  # Apply logisic regression
  for(i in 1:nrow(scoring_df)){
    
    period_tab <- as.numeric(scoring_df$period[i])
    amount_tab <- as.numeric(scoring_df$amount[i])
    
    apply_logit <- gen_apply_model(df,coefficients)
    scoring_df$score[i] <- apply_logit
    scoring_df$score[i] <- gen_group_scores(scoring_df$score[i],
           all_df$office_id,0,1,1)
    scoring_df$pd[i] <- round(apply_logit,3)
  
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
    
  }
  return(scoring_df)
}

