
#########################################################################
######## Functions to apply logisit regression on repeat City Cash ######
#########################################################################

gen_beh_citycash <- function(df,scoring_df,products,df_Log_beh,period,
                             all_df,prev_amount,amount_tab,
                             t_income,disposable_income_adj){
  # Cut and bin
  df$ownership <- ifelse(df$ownership %in% c("2","4"),"2_4","1_3")
  df$education <- ifelse(df$education %in% c("3","4"),"3_4",
      ifelse(df$education==1,"1","2"))
  df$status_work <- ifelse(df$status_work==5, "5","other")
  df$purpose <- ifelse(df$purpose==4, "4", "not_4")
  df$household_children <- ifelse(df$household_children==0 | 
       df$household_children==1,"0_1",
       ifelse(df$household_children==2,"2","3_more"))
  df$age <- ifelse(df$age<=30,"30_less",
       ifelse(df$age<=50,"31_50",
       ifelse(df$age<=55,"51_55",
       ifelse(df$age<=60,"56_60","61_more"))))
  df$maturity <- ifelse(df$maturity<=2.5, "2.5_less",
       ifelse(df$maturity<=4, "2.5_4",
       ifelse(df$maturity<=6,"4_6","6_more")))
  df$experience_employer <- ifelse(is.na(df$experience_employer), -999, 
       df$experience_employer)
  df$experience_employer <- ifelse(df$experience_employer<=100,"missing_0_100",
       ifelse(df$experience_employer<=156,"100_156","more_156"))
  df$on_address <- ifelse(is.na(df$on_address), -999, df$on_address)
  df$on_address <- ifelse(df$on_address<=24,"missing_less_24","more_24")
  df$credits_cum <- ifelse(df$credits_cum>7,"more_7",
       ifelse(df$credits_cum>4,"5_6",
       ifelse(df$credits_cum>3,"4","3_less")))
  df$days_diff_last_credit <- ifelse(is.na(df$days_diff_last_credit),-999, 
       df$days_diff_last_credit)
  df$days_diff_last_credit <- ifelse(df$days_diff_last_credit<=4, "4_less",
       "more_5")
  df$max_delay <- ifelse(is.na(df$max_delay),60, df$max_delay)
  df$max_delay <- ifelse(df$max_delay<=30,"less_30",
       ifelse(df$max_delay<=90,"31_90","more_90"))
  df$amount_diff <- ifelse(df$amount_diff<=-250,"less_m250",
       ifelse(df$amount_diff<=-100,"m250_m100",
       ifelse(df$amount_diff<=0,"m100_0",
       ifelse(df$amount_diff<=200,"0_200","more_200"))))

  # Treat NAs
  df$gender <- ifelse(is.na(df$gender),1,df$gender)
  df$education <- ifelse(is.na(df$education),"2",df$education)
  df$status_work <- ifelse(is.na(df$status_work),"other",df$status_work)
  df$purpose <- ifelse(is.na(df$purpose),"not_4",df$purpose)
  df$household_children <- ifelse(is.na(df$household_children),"0_1",
       df$household_children)
  df$age <- ifelse(is.na(df$age),"31_50", df$age)
  df$maturity <- ifelse(is.na(df$maturity),"2.5_4",df$maturity)
  df$credits_cum <- ifelse(is.na(df$credits_cum),"3_less",df$credits_cum)
  df$amount_diff <- ifelse(is.na(df$amount_diff),"0_200",df$amount_diff)
  df$flag_high_last_paid <- ifelse(is.na(df$flag_high_last_paid),"0",
      df$flag_high_last_paid)
  
  # Re-factor fields 
  df$ownership <- as.factor(df$ownership)
  df$education <- as.factor(df$education)
  df$gender <- as.factor(df$gender)
  df$status_work <- as.factor(df$status_work)
  df$purpose <- as.factor(df$purpose)
  df$household_children <- as.factor(df$household_children)
  df$age <- as.factor(df$age)
  df$maturity <- as.factor(df$maturity)
  df$experience_employer <- as.factor(df$experience_employer)
  df$on_address  <- as.factor(df$on_address)
  df$credits_cum <- as.factor(df$credits_cum)
  df$days_diff_last_credit <- as.factor(df$days_diff_last_credit)
  df$max_delay <- as.factor(df$max_delay)
  df$amount_diff <- as.factor(df$amount_diff)
  df$flag_high_last_paid <- as.factor(df$flag_high_last_paid)
  
  for(i in 1:nrow(scoring_df)){
    
    period_tab <- as.numeric(scoring_df$period[i])
    amount_tab <- as.numeric(scoring_df$amount[i])
    
    if(df$total_income<100 | is.na(df$total_income)){
      ratio_tab <- 0.08}
    else {
      ratio_tab <- products[products$period == period_tab & 
            products$amount == amount_tab & 
            products$product_id == all_df$product_id, ]$installment_amount/
            t_income}
    
    if (ratio_tab>=3) {ratio_tab <- 3}
    
    acceptable_installment_amount <- products$installment_amount[
      products$period==period_tab & products$amount==amount_tab]
    
    amount_diff_loc <- amount_tab - prev_amount$amount
    df$amount_diff <- ifelse(amount_diff_loc<=-250, "less_m250",
         ifelse(amount_diff_loc<=-100, "m250_m100",
         ifelse(amount_diff_loc<=0, "m100_0",
         ifelse(amount_diff_loc<=200, "0_200", "more_200"))))
    
    df$ratio_installment_income <- ifelse(ratio_tab<=0.07,"0.07_less",
         ifelse(ratio_tab<=0.09,"0.07_0.09","more_0.09"))
    df$ratio_installment_income <- as.factor(df$ratio_installment_income)
    
    # Compute correct maturity for each amount and product_id
    current_maturity <- ifelse(period==1, period_tab*7/30, 
         ifelse(period==2, period_tab*14/30, period_tab))
    df$maturity <- ifelse(current_maturity<=2.5, "2.5_less",
         ifelse(current_maturity<=4, "2.5_4",
         ifelse(current_maturity<=6,"4_6","6_more")))
    
    # Apply logistic model to each amount and installment
    apply_logit <- predict(df_Log_beh, newdata=df, type="response")
    scoring_df$score[i] <- apply_logit
    scoring_df$score[i] <- gen_group_scores(scoring_df$score[i],
                                            all_df$office_id,1,0,0)
    
    # Compute flag of disposable income
    product_tab <- subset(products, products$product_id==all_df$product_id & 
        products$period==as.numeric(period_tab) &
        products$amount==as.numeric(amount_tab))
    
    scoring_df$color[i] <- ifelse(
      product_tab$installment_amount>=disposable_income_adj, 1, 0)
    
    scoring_df$color[i] <- 
          ifelse(scoring_df$color[i]==1 | scoring_df$score[i]=="Bad", 1, 
          ifelse(scoring_df$score[i]=="Indeterminate", 2,
          ifelse(scoring_df$score[i]=="Good 1", 3,
          ifelse(scoring_df$score[i]=="Good 2", 4,
          ifelse(scoring_df$score[i]=="Good 3", 5,
          ifelse(scoring_df$score[i]=="Good 4",6,NA))))))
    
  }
  return(scoring_df)
}

