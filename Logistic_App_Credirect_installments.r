
##############################################################################
######## Functions to apply logisit regression on application Credirect ######
##############################################################################

gen_app_credirect_installments <- function(df,scoring_df,products,
  df_Log_Credirect_App_installments,period,all_df,prev_amount,amount_tab,
  t_income,disposable_income_adj,flag_credit_next_salary,api_df,base_dir){
  
  
  # Load rdata
  load(file.path(base_dir,"rdata","credirect_installments_coeffs.rdata"))
  names(coefficients) <- c("coeff")
  
  # Cut and bin
  df$age <- ifelse(df$age<=20,"20_less","more_20")
  df$age <- as.factor(df$age)
  
  df$gender <- as.factor(df$gender)
  
  df$ownership_cut <- ifelse(df$ownership %in% c(1), "1", 
    ifelse(is.na(df$ownership), "not_1", "not_1"))
  df$ownership <- as.factor(df$ownership_cut)
  
  df$education <- ifelse(is.na(df$education),"2",
    ifelse(df$education==1,"1",
    ifelse(df$education %in% c(3,4), "3_4", "2")))
  df$education <- as.factor(df$education)
  
  df$marital_status_cut <- ifelse(is.na(df$marital_status), "other",
    ifelse(df$marital_status %in% c(1,3), "1_3","other"))
  df$marital_status <- as.factor(df$marital_status_cut)
  
  df$experience_employer_cut <- 
    ifelse(is.na(df$experience_employer),"less_96",
    ifelse(df$experience_employer<=96,"less_96","more_96"))
  df$experience_employer <- as.factor(df$experience_employer_cut)
  
  df$purpose_cut <- ifelse(is.na(df$purpose), "other",
    ifelse(df$purpose %in% c(2,5),"2_5",
    ifelse(df$purpose %in% c(6),"6","other")))
  df$purpose <- as.factor(df$purpose_cut)
  
  df$status_work_cut <- ifelse(is.na(df$status_work), "other",
    ifelse(df$status_work %in% c(4,5,9,10,12), "4_5_9_10_12","other"))
  df$status_work <- as.factor(df$status_work_cut)
  
  df$outs_overdue_ratio_total_cut <- ifelse(
    is.na(df$outs_overdue_ratio_total),"other",
    ifelse(df$outs_overdue_ratio_total==-999,"other",
    ifelse(df$outs_overdue_ratio_total<=0.01,"other",
    ifelse(df$outs_overdue_ratio_total>=0.3,"0.3_1","0.05_0.3"))))
  df$outs_overdue_ratio_total <- as.factor(df$outs_overdue_ratio_total_cut)
  
  df$has_viber_cut <- 
    ifelse(is.na(df$has_viber),"has_viber",
    ifelse(df$has_viber==0,"no_viber","has_viber"))
  df$has_viber <- as.factor(df$has_viber_cut)
  
  api_df$period <- as.numeric(api_df$period)
  df$API_period_cut <- 
    ifelse(is.na(api_df$period),"more_7",
    ifelse(api_df$period==3,"3",
    ifelse(api_df$period<=6,"4_6","more_7")))
  df$API_period <- as.factor(df$API_period_cut)
  
  api_df$amount <- as.numeric(api_df$amount)
  df$API_amount_cut <- 
    ifelse(is.na(api_df$amount),"more_400",
    ifelse(api_df$amount<=250,"less_250",
    ifelse(api_df$amount<=350,"300_350","more_400")))
  df$API_amount <- as.factor(df$API_amount_cut)
  
  api_df$payment_method <- as.numeric(api_df$payment_method)
  api_df$payment_method <- ifelse(is.na(api_df$payment_method),
    as.numeric(df$payment_method2),api_df$payment_method)
  df$API_payment_method_cut <- ifelse(is.na(api_df$payment_method), "other",
    ifelse(api_df$payment_method==2, "2",
    ifelse(api_df$payment_method==3, "other", "other")))
  df$API_payment_method <- as.factor(df$API_payment_method_cut)
  
  df$API_referral_source_cut <- ifelse(is.na(api_df$referral_source), "other",
    ifelse(api_df$referral_source=="facebook","facebook","other"))
  df$API_referral_source <- as.factor(df$API_referral_source_cut)
  
  df$city_pop_cut <- 
    ifelse(is.na(df$city_pop),"other",
    ifelse(df$city_pop<=50000,"other","50k+"))
  df$city_pop <- as.factor(df$city_pop_cut)
  
  df$phone_plan_cut <- 
    ifelse(is.na(df$phone_plan),"other",
    ifelse(df$phone_plan==2,"2","other"))
  df$phone_plan <- as.factor(df$phone_plan_cut)
  
  df$leasing_cut <- 
    ifelse(is.na(df$leasing),"other",
    ifelse(df$leasing %in% c(2,3),"2_3","other"))
  df$leasing <- as.factor(df$leasing_cut)
  
  df$other_bank_accounts_cut <- 
    ifelse(is.na(df$other_bank_accounts),"other",
    ifelse(df$other_bank_accounts==1,"no_bank","other"))
  df$other_bank_accounts <- as.factor(df$other_bank_accounts_cut)
  
  # Compute PD
  apply_logit <- gen_apply_model(df,coefficients)
  scoring_df$score <- apply_logit
  for(i in 1:nrow(scoring_df)){
    scoring_df$score[i] <- gen_group_scores(scoring_df$score[i],
      all_df$office_id,0,1,0)
  }
  scoring_df$pd <- round(apply_logit,3)
  
  # Compute color of score
  scoring_df$color <- ifelse(scoring_df$score=="Bad", 1, 
     ifelse(scoring_df$score=="Indeterminate", 2,
     ifelse(scoring_df$score=="Good 1", 3,
     ifelse(scoring_df$score=="Good 2", 4,
     ifelse(scoring_df$score=="Good 3", 5,
     ifelse(scoring_df$score=="Good 4",6,NA))))))
    
  return(scoring_df)
}

