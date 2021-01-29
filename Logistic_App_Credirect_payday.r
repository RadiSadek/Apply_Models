
##############################################################################
######## Functions to apply logisit regression on application Credirect ######
##############################################################################

gen_app_credirect_payday <- function(df,scoring_df,products,
  df_Log_Credirect_App_payday,period,all_df,prev_amount,amount_tab,
  t_income,disposable_income_adj,flag_credit_next_salary){
  
  # Cut and bin
  df$age_cut <- ifelse(df$age<=22,"22_less",
     ifelse(df$age<=29,"23_29",
     ifelse(df$age<=35,"30_35",
     ifelse(df$age<=50,"36_50","more_50"))))
  df$age <- as.factor(df$age_cut)
  
  df$education_cut <- ifelse(is.na(df$education),"2",
     ifelse(df$education %in% c(3,4), "3_4",
     ifelse(df$education==1, "1", "2")))
  df$education <- as.factor(df$education_cut)
  
  df$gender <- as.factor(df$gender)
  
  df$ownership_cut <- ifelse(df$ownership %in% c(1), "1", "not_1")
  df$ownership <- as.factor(df$ownership_cut)
  
  df$status_work_cut <- ifelse(is.na(df$status_work), "other",
      ifelse(df$status_work %in% c(4,9), "4_9",
      ifelse(df$status_work %in% c(5),"5","other")))   
  df$status_work <- as.factor(df$status_work_cut)
  
  df$purpose_cut <- ifelse(is.na(df$purpose), "other",
      ifelse(df$purpose %in% c(5), "5", "other"))
  df$purpose <- as.factor(df$purpose_cut)
  
  df$experience_employer_cut <- ifelse(is.na(df$experience_employer), "2_more",
      ifelse(df$experience_employer<=1,"0_1","2_more"))
  df$experience_employer <- as.factor(df$experience_employer_cut)
  
  df$status_finished_total_cut <- ifelse(is.na(df$status_finished_total),
     "0_71_72_73",
     ifelse(df$status_finished_total %in% c(0,71,72,73),"0_71_72_73",
     ifelse(df$status_finished_total %in% c(74),"74","75")))
  df$status_finished_total <- as.factor(df$status_finished_total_cut)
  
  df$status_active_total_cut <- ifelse(is.na(df$status_active_total),
     "other",
     ifelse(df$status_active_total %in% c(0,-1,71),"other", 
     ifelse(df$status_active_total %in% c(72,73),"72_73", "74_75")))
  df$status_active_total <- as.factor(df$status_active_total_cut)
  
  df$source_entity_count_total_cut <- ifelse(is.na(df$source_entity_count_total),
    "other",
    ifelse(df$source_entity_count_total==0, "0_more_6",
    ifelse(df$source_entity_count_total %in% c(1,4,5),"other",
    ifelse(df$source_entity_count_total %in% c(2,3) , "other","0_more_6"))))
  df$source_entity_count_total <- as.factor(df$source_entity_count_total_cut)
  
  df$outs_overdue_ratio_total_cut <- ifelse(
    is.na(df$outs_overdue_ratio_total), "other",
    ifelse(df$outs_overdue_ratio_total==-999,"other",
    ifelse(df$outs_overdue_ratio_total==0,"other",
    ifelse(df$outs_overdue_ratio_total>0 & df$outs_overdue_ratio_total<=0.02,
      "other",
    ifelse(df$outs_overdue_ratio_total>0.02 & df$outs_overdue_ratio_total<=0.06,
      "0.02_0.06","more_0.06")))))
  df$outs_overdue_ratio_total <- as.factor(df$outs_overdue_ratio_total_cut)
  
  df$viber_registered_cut <- ifelse(is.na(df$has_viber), "other",
    ifelse(df$has_viber==0, "False",
    ifelse(df$has_viber==1, "other", "other")))
  df$viber_registered <- as.factor(df$viber_registered_cut)
  
  df$whatsapp_registered_cut <- "other"
  df$whatsapp_registered <- as.factor(df$whatsapp_registered_cut)
  
  # Apply logisic regression
  for(i in 1:nrow(scoring_df)){
    
    period_tab <- as.numeric(scoring_df$period[i])
    amount_tab <- as.numeric(scoring_df$amount[i])
    
    apply_logit <- predict(df_Log_Credirect_App_payday, 
                           newdata=df, type="response")
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

