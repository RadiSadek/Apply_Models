
##############################################################################
######## Functions to apply logisit regression on application City Cash ######
##############################################################################

gen_app_citycash <- function(df,scoring_df,products,df_Log_beh,period,
                             all_df,prev_amount,amount_tab,
                             t_income,disposable_income_adj){
  # Cut and bin
  df$ownership <- ifelse(is.na(df$ownership),-999,df$ownership)
  df$ownership <- ifelse(df$ownership %in% c(1,3,-999), "1_3", "2_4")
  df$ownership <- as.factor(df$ownership)
  df$education <- ifelse(is.na(df$education),-999,df$education)
  df$education <- ifelse(df$education %in% c(2,3,-999),"2_3", df$education)
  df$education <- as.factor(df$education)
  df$marital_status <- ifelse(is.na(df$marital_status), -999, 
     df$marital_status)
  df$marital_status <- ifelse(df$marital_status %in% c(2,3,-999),"2_3", 
     ifelse(df$marital_status %in% c(1,4), "1_4", "5"))      
  df$marital_status <- as.factor(df$marital_status)
  df$gender <- as.factor(df$gender)
  df$status_work <- ifelse(is.na(df$status_work),-999,df$status_work)
  df$status_work <- ifelse(df$status_work %in% c(5,9), "5_9",
     ifelse(df$status_work %in% c(2,3,7,8,11,-999),
     "2_3_7_8_11","1_4_6_10"))
  df$status_work <- as.factor(df$status_work)
  df$purpose <- ifelse(is.na(df$purpose),-999,df$purpose)
  df$purpose <- ifelse(df$purpose %in% c(2,3), "2_3","other")
  df$purpose <- as.factor(df$purpose)
  df$age <- ifelse(df$age<=32,"32_less","32_more")
  df$age <- as.factor(df$age)
  df$experience_employer <- ifelse(is.na(df$experience_employer), 12, 
        df$experience_employer)
  df$experience_employer_cut <- ifelse(df$experience_employer<=5,"0_5",
        ifelse(df$experience_employer<=72,"6_72","more_72"))
  df$experience_employer <- as.factor(df$experience_employer_cut)
  df$maturity <- ifelse(df$maturity<=3,"3_less",
        ifelse(df$maturity<=4,"4","5_more"))
  df$maturity <- as.factor(df$maturity)
  df$status_active_total <- ifelse(is.na(df$status_active_total), -999, 
        df$status_active_total)
  df$status_active_total <- ifelse(df$status_active_total==0,"0",
        ifelse(df$status_active_total %in% 
        c(-1,-999,71,72,73),"no_cred_ckr_71_72_73","74_75"))
  df$status_active_total <- as.factor(df$status_active_total)
  df$cred_count_total <- ifelse(is.na(df$cred_count_total), -999, 
        df$cred_count_total)
  df$cred_count_total <- ifelse(df$cred_count_total==1,"1","not_1")
  df$cred_count_total <- as.factor(df$cred_count_total)
  
  # Apply logisic regression
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
    
    acceptable_installment_amount <- products$installment_amount[
      products$period==period_tab & products$amount==amount_tab]
    
    if (ratio_tab>=3) {ratio_tab <- 3}
    
    ratio_tab <- as.numeric(ratio_tab)
    df$ratio_installment_income <- ifelse(is.na(ratio_tab), 999, 
           ratio_tab)
    df$ratio_installment_income <- ifelse(df$ratio_installment_income<=0.05,
          "0.05_less",
          ifelse(df$ratio_installment_income<=0.08,"0.05_0.08","more_0.08"))
    df$ratio_installment_income <- as.factor(df$ratio_installment_income)
    
    # Compute correct maturity for each amount and product_id
    current_maturity <- ifelse(period==1, period_tab*7/30, 
                               ifelse(period==2, period_tab*14/30, period_tab))
    df$maturity <- ifelse(current_maturity<=3,"3_less",
                          ifelse(current_maturity<=4,"4","5_more"))
    df$maturity <- as.factor(df$maturity)
    
    # Apply logistic model to each amount and installment
    apply_logit <- predict(df_Log_CityCash_App, newdata=df, type="response")
    scoring_df$score[i] <- apply_logit
    scoring_df$score[i] <- gen_group_scores(scoring_df$score[i],0,0,0)
    
    # Compute flag of disposable income
    product_tab <- subset(products, products$product_id==all_df$product_id & 
                            products$period==as.numeric(period_tab) &
                            products$amount==as.numeric(amount_tab))
    criteria_color <- ifelse(0.1*all_df$installment_amount<5 & 
                               all_df$installment_amount<=50, 5, -999)
    
    scoring_df$color[i] <- ifelse(
      product_tab$installment_amount>=disposable_income_adj, 1,
       ifelse(all_df$installment_amount>50,
             (ifelse(acceptable_installment_amount>
                       (1.1*all_df$installment_amount), 
              1, 0)),
       ifelse(criteria_color==5, 
       ifelse((acceptable_installment_amount-all_df$installment_amount)>5, 
              1, 0),
       ifelse(acceptable_installment_amount>(1.1*all_df$installment_amount), 
              1, 0))))
    
    scoring_df$color[i] <- ifelse(scoring_df$color[i]==1 | 
              scoring_df$score[i]=="Bad", 1, 
              ifelse(scoring_df$score[i]=="Indeterminate", 2,
              ifelse(scoring_df$score[i]=="Good 1", 3,
              ifelse(scoring_df$score[i]=="Good 2", 4,
              ifelse(scoring_df$score[i]=="Good 3", 5,
              ifelse(scoring_df$score[i]=="Good 4", 6, NA))))))
  }
  return(scoring_df)
}


  
  