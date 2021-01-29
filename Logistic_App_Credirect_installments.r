
##############################################################################
######## Functions to apply logisit regression on application Credirect ######
##############################################################################

gen_app_credirect_installments <- function(df,scoring_df,products,
  df_Log_Credirect_App_installments,period,all_df,prev_amount,amount_tab,
  t_income,disposable_income_adj,flag_credit_next_salary){
  
  # Cut and bin
  df$age_cut <- ifelse(df$age<=24,"24_less",
       ifelse(df$age<=39,"25_39",
       ifelse(df$age<=49,"40_49","more_50")))
  df$age <- as.factor(df$age_cut)
  
  df$education_cut <- ifelse(is.na(df$education),"2",
      ifelse(df$education==1,"1",
      ifelse(df$education %in% c(3,4), "3_4", "2")))
  df$education <- as.factor(df$education_cut)
  
  df$marital_status_cut <- ifelse(is.na(df$marital_status), "not_1",
      ifelse(df$marital_status %in% c(2,3,4,5), "not_1", df$marital_status))
  df$marital_status <- as.factor(df$marital_status_cut)
  
  df$status_work_cut <- ifelse(is.na(df$status_work), "other",
      ifelse(df$status_work %in% c(4,5), "4_5","other"))
  df$status_work <- as.factor(df$status_work_cut)
  
  df$status_finished_total_cut <- ifelse(is.na(df$status_finished_total),
     "0_71_72_73",
     ifelse(df$status_finished_total %in% c(0,71,72,73),"0_71_72_73", "74_75"))
  df$status_finished_total <- as.factor(df$status_finished_total_cut)
  
  df$source_entity_count_total_cut <- 
    ifelse(is.na(df$source_entity_count_total),"more_2",
    ifelse(df$source_entity_count_total==0, "0",
    ifelse(df$source_entity_count_total==1, "1","more_2")))
  df$source_entity_count_total <- as.factor(df$source_entity_count_total_cut)
  
  df$outs_overdue_ratio_total_cut <- ifelse(
    is.na(df$outs_overdue_ratio_total), "0_0.01",
    ifelse(df$outs_overdue_ratio_total==-999,"0_0.01",
    ifelse(df$outs_overdue_ratio_total<=0.01,"0_0.01",
    ifelse(df$outs_overdue_ratio_total<=0.08,"0.01_0.08",
    ifelse(df$outs_overdue_ratio_total<=1,"0.08_1","more_0.08")))))
  df$outs_overdue_ratio_total <- as.factor(df$outs_overdue_ratio_total_cut)
  
  df$viber_registered_cut <- ifelse(is.na(df$has_viber), "other",
     ifelse(df$has_viber==0, "False",
     ifelse(df$has_viber==1, "other", "other")))
  df$viber_registered <- as.factor(df$viber_registered_cut)

  # Apply logisic regression
  for(i in 1:nrow(scoring_df)){
    
    period_tab <- as.numeric(scoring_df$period[i])
    amount_tab <- as.numeric(scoring_df$amount[i])
    
    # Compute correct maturity for each amount and product_id
    current_maturity <- ifelse(period==1, period_tab*7/30, 
                               ifelse(period==2, period_tab*14/30, period_tab))
    df$maturity_cut <- ifelse(current_maturity<=2,"2",
             ifelse(current_maturity<=3,"3",
             ifelse(current_maturity<=9,"4_9","more_10")))
    df$maturity <- as.factor(df$maturity_cut)
    
    
    apply_logit <- predict(df_Log_Credirect_App_installments, newdata=df, 
                           type="response")
    
    scoring_df$score[i] <- apply_logit
    scoring_df$score[i] <- gen_group_scores(scoring_df$score[i],
          all_df$office_id,0,1,0)
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

