
#########################################################################
######## Functions to apply logisit regression on repeat Credirect ######
#########################################################################

gen_beh_credirect <- function(df,scoring_df,products,df_Log_beh_Credirect,
                             period,all_df,prev_amount,amount_tab,
                             t_income,disposable_income_adj,crit_po,
                             flag_new_credirect_old_city){
  #  Cut and bin
  df$education_cut <- ifelse(is.na(df$education), "2_3",
      ifelse(df$education==1,"1",
                                    ifelse(df$education==4,"4","2_3")))
  df$education <- as.factor(df$education_cut)

  df$status_work_cut <- ifelse(is.na(df$status_work), "other",
     ifelse(df$status_work %in% c(1,4,5),"1_4_5","other"))
  df$status_work <- as.factor(df$status_work_cut)
  
  df$purpose_cut <- ifelse(is.na(df$purpose), "other",
     ifelse(df$purpose %in% c(2,4,5),"2_4_5","other"))
  df$purpose <- as.factor(df$purpose_cut)
  
  df$age_cut <- ifelse(df$age<=23,"23_less",
     ifelse(df$age<50,"24_50","51_more"))
  df$age <- as.factor(df$age_cut)
  
  df$experience_employer_cut <- ifelse(is.na(df$experience_employer),"more_3",
     ifelse(df$experience_employer<=2,"0_2","more_3"))
  df$experience_employer <- as.factor(df$experience_employer_cut)
  
  df$new_cred_old_city <- flag_new_credirect_old_city
  df$new_cred_old_city <- as.factor(df$new_cred_old_city)
  
  df$max_delay_cut <- ifelse(is.na(df$max_delay),"5_55",
     ifelse(df$max_delay<=4,"less_4",
     ifelse(df$max_delay<=55,"5_55","more_56")))
  df$max_delay <- as.factor(df$max_delay_cut)
  
  df$days_diff_last_credit_cut <- ifelse(is.na(df$days_diff_last_credit), 
     "0_1",
     ifelse(df$days_diff_last_credit %in% c(0,1),"0_1","more_2"))
  df$days_diff_last_credit <- as.factor(df$days_diff_last_credit_cut)
  
  df$status_active_total_cut <- ifelse(is.na(df$status_active_total),"other",
     ifelse(df$status_active_total==0,"0","other"))
  df$status_active_total <- as.factor(df$status_active_total_cut)
  
  df$status_finished_total_cut <- ifelse(is.na(df$status_finished_total),"not_75",
     ifelse(df$status_finished_total==75,"75","not_75"))
  df$status_finished_total <- as.factor(df$status_finished_total_cut)
  
  df$source_entity_count_total_cut <- ifelse(
    is.na(df$source_entity_count_total), "9_less",
    ifelse(df$source_entity_count_total<=9,"9_less","10_more"))
  df$source_entity_count_total <- as.factor(
    df$source_entity_count_total_cut)
  
  df$outs_overdue_ratio_total_cut <- ifelse(
    is.na(df$outs_overdue_ratio_total), "0_0.16",
    ifelse(df$outs_overdue_ratio_total==-999,"0_0.16",
    ifelse(df$outs_overdue_ratio_total==0,"0",
    ifelse(df$outs_overdue_ratio_total<=0.16,"0_0.16","more_0.16"))))
  df$outs_overdue_ratio_total <- as.factor(df$outs_overdue_ratio_total_cut)
  
  # df$viber_registered_cut <- ifelse(is.na(df$viber_registered), "other",
  #   ifelse(df$viber_registered=="False", "False",
  #   ifelse(df$viber_registered=="True", "other", "other")))
  # df$viber_registered <- as.factor(df$viber_registered_cut)
  df$viber_registered_cut <- ifelse(is.na(df$viber_registered), "other",
     ifelse(df$viber_registered=="False", "other",
     ifelse(df$viber_registered=="True", "other", "other")))
  df$viber_registered <- as.factor(df$viber_registered_cut)
  
  # Apply model
  for(i in 1:nrow(scoring_df)){
    
    period_tab <- as.numeric(scoring_df$period[i])
    amount_tab <- as.numeric(scoring_df$amount[i])
    
    amount_diff_loc <- amount_tab - prev_amount$amount
    df$amount_diff <- ifelse(amount_diff_loc<250,"less_200","more_250")
    df$amount_diff <- as.factor(df$amount_diff)
    
    apply_logit <- predict(df_Log_beh_Credirect, newdata=df, type="response")
    scoring_df$score[i] <- apply_logit
    scoring_df$score[i] <- gen_group_scores(scoring_df$score[i],
         all_df$office_id,1,1,0)
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
    
    if(crit_po!=0){
      scoring_df$installment_amount[i] <- product_tab$installment_amount
    }
    
    
  }
  return(scoring_df)
}


  
  