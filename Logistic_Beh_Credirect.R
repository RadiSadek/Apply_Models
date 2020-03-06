
#########################################################################
######## Functions to apply logisit regression on repeat Credirect ######
#########################################################################

gen_beh_credirect <- function(df,scoring_df,products,df_Log_beh,period,
                             all_df,prev_amount,amount_tab,
                             t_income,disposable_income_adj){
  #  Cut and bin
  df$age_cut <- ifelse(df$age<=31,"31_less","32_more")
  df$age <- as.factor(df$age_cut)
  df$gender <- as.factor(df$gender)
  df$education <- ifelse(is.na(df$education), "not_4", 
        ifelse(df$education %in% c(4), "4", "not_4"))
  df$education <- as.factor(df$education)
  df$status_work_cut <- ifelse(is.na(df$status_work), "other",
       ifelse(df$status_work %in% c(1,3,4,6,9,5), "1_3_4_5_6_9", "other"))
  df$status_work <- as.factor(df$status_work_cut)
  df$days_diff_last_credit_cut <- ifelse(is.na(df$days_diff_last_credit), 
       "less_3", ifelse(df$days_diff_last_credit<=3,"less_3","4_more"))
  df$days_diff_last_credit <- as.factor(df$days_diff_last_credit_cut)
  df$prev_online <- ifelse(is.na(df$prev_online),0,df$prev_online)
  df$prev_online <- as.factor(df$prev_online)
  df$max_delay_cut <- ifelse(df$max_delay<=2,"less_2",
        ifelse(df$max_delay<=13,"3_13",
        ifelse(df$max_delay<=48,"14_48","more_48")))
  df$max_delay <- as.factor(df$max_delay_cut)
  df$credits_cum_cut <- ifelse(df$credits_cum<=5,"5_less","6_more")
  df$credits_cum <- as.factor(df$credits_cum_cut)
  df$status_active_total_cut <- ifelse(is.na(df$status_active_total),"other",
        ifelse(df$status_active_total==0,"0","other"))
  df$status_active_total <- as.factor(df$status_active_total_cut)
  df$status_finished_total_cut <- ifelse(is.na(df$status_finished_total),
        "not_75", ifelse(df$status_finished_total==75,"75","not_75"))
  df$status_finished_total <- as.factor(df$status_finished_total_cut)
  df$source_entity_count_total_cut <- ifelse(
    is.na(df$source_entity_count_total), "11_less",
    ifelse(df$source_entity_count_total<=11,"11_less","12_more"))
  df$source_entity_count_total <- as.factor(
    df$source_entity_count_total_cut)
  df$outs_overdue_ratio_total_cut <- ifelse(
    is.na(df$outs_overdue_ratio_total), "0.15_less",
    ifelse(df$outs_overdue_ratio_total==-999,"0.15_less",
    ifelse(df$outs_overdue_ratio_total<=0.15,"0.15_less","more_0.15")))
  df$outs_overdue_ratio_total <- as.factor(df$outs_overdue_ratio_total_cut)
  
  for(i in 1:nrow(scoring_df)){
    
    period_tab <- as.numeric(scoring_df$period[i])
    amount_tab <- as.numeric(scoring_df$amount[i])
    
    amount_diff_loc <- amount_tab - prev_amount$amount
    df$amount_diff <- ifelse(amount_diff_loc>=200,"200_more","less_200")
    df$amount_diff <- as.factor(df$amount_diff)
    
    apply_logit <- predict(df_Log_beh_Credirect, newdata=df, type="response")
    scoring_df$score[i] <- gen_group_scores(scoring_df$score[i],
                                            all_df$office_id,1,1,0)
    
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


  
  