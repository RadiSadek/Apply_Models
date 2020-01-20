
##############################################################################
######## Functions to apply logisit regression on application Credirect ######
##############################################################################

gen_app_credirect <- function(df,scoring_df,products,df_Log_beh,period,
                             all_df,prev_amount,amount_tab,
                             t_income,disposable_income_adj){
  # Cut and bin
  df$age_cut <- ifelse(df$age<=28,"28_less","29_more")
  df$age <- as.factor(df$age_cut)
  df$education <- ifelse(is.na(df$education), "2_3", 
       ifelse(df$education %in% c(2,3), "2_3", df$education))
  df$education <- as.factor(df$education)
  df$marital_status_cut <- ifelse(is.na(df$marital_status), "2_5",
       ifelse(df$marital_status %in% c(1,3,4), "1_3_4", "2_5"))
  df$marital_status <- as.factor(df$marital_status_cut)
  df$experience_employer_cut <- ifelse(is.na(df$experience_employer), "more_4",
       ifelse(df$experience_employer<=1,"0_1",
       ifelse(df$experience_employer<=3,"2_3","more_4")))
  df$experience_employer <- as.factor(df$experience_employer_cut)
  df$purpose_cut <- ifelse(is.na(df$purpose), "1_6",
       ifelse(df$purpose %in% c(1,6),"1_6",
       ifelse(df$purpose==0, "1_6", "not_1_6")))
  df$purpose <- as.factor(df$purpose_cut)
  df$status_work_cut <- ifelse(is.na(df$status_work), "other",
       ifelse(df$status_work==5, "5",
       ifelse(df$status_work %in% c(4,6,9), "4_6_9", "other")))
  df$status_work <- as.factor(df$status_work_cut)
  df$status_finished_total_cut <- ifelse(is.na(df$status_finished_total),
       "0_71_72",
       ifelse(df$status_finished_total %in% c(0,71,72),"0_71_72",
       ifelse(df$status_finished_total %in% c(73,74),"73_74","75")))
  df$status_finished_total <- as.factor(df$status_finished_total_cut)
  df$status_active_total_cut <- ifelse(is.na(df$status_active_total),
       "no_credit_no_delay",
       ifelse(df$status_active_total %in% c(-1,0), "no_credit_no_delay", 
       ifelse(df$status_active_total %in% c(71,72,73),"71_72_73","74_75")))
  df$status_active_total <- as.factor(df$status_active_total_cut)
  df$source_entity_count_total_cut <- ifelse(
    is.na(df$source_entity_count_total),"0_1_5_6",
    ifelse(df$source_entity_count_total==0, "0_1_5_6",
    ifelse(df$source_entity_count_total %in% c(1,5,6),"0_1_5_6",
    ifelse(df$source_entity_count_total %in% c(2:4),"2_4","more_7"))))
  df$source_entity_count_total <- as.factor(df$source_entity_count_total_cut)
  
  df$outs_overdue_ratio_total_cut <- ifelse(is.na(
    df$outs_overdue_ratio_total), "0_0.03",
    ifelse(df$outs_overdue_ratio_total==-999,"0_0.03",
    ifelse(df$outs_overdue_ratio_total==0,"0",
    ifelse(df$outs_overdue_ratio_total<=0.03,"0_0.03",
    ifelse(df$outs_overdue_ratio_total<=0.08,"0.03_0.08","more_0.08")))))
  df$outs_overdue_ratio_total <- as.factor(df$outs_overdue_ratio_total_cut)
  
  # Apply logisic regression
  for(i in 1:nrow(scoring_df)){
    
    period_tab <- as.numeric(scoring_df$period[i])
    amount_tab <- as.numeric(scoring_df$amount[i])
    
    apply_logit <- predict(df_Log_Credirect_App, newdata=df, type="response")
    scoring_df$score[i] <- apply_logit
    scoring_df$score[i] <- ifelse(flag_credit_next_salary==1,
         gen_group_scores(scoring_df$score[i],0,1,1),
         gen_group_scores(scoring_df$score[i],0,1,0))
    
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

