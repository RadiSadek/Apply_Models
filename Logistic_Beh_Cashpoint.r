
#########################################################################
######## Functions to apply logisit regression on repeat City Cash ######
#########################################################################

gen_beh_cashpoint <- function(df,scoring_df,products,df_Log_beh_CityCash,period,
                             all_id,all_df,prev_amount,amount_tab,
                             t_income,disposable_income_adj,crit_po,base_dir){
  
  # Load rdata
  load(file.path(base_dir,"rdata","cashpoint_repeat_coeffs.rdata"))
  names(coefficients) <- c("coeff")
  
  # Cut and bin
  df$age_cut <- 
    ifelse(df$age<=35,"35_less",
    ifelse(df$age<=52,"36_52",
    ifelse(df$age<=60,"53_60","more_60")))
  df$age <- as.factor(df$age_cut)
  
  df$gender <- as.factor(df$gender)
  
  df$education_cut <- ifelse(is.na(df$education),"other",
     ifelse(df$education==1,"1","other"))
  df$education <- as.factor(df$education_cut)
  
  df$experience_employer_cut <- 
    ifelse(is.na(df$experience_employer),"0_36",
    ifelse(df$experience_employer<=36,"0_36","36+"))
  df$experience_employer <- as.factor(df$experience_employer_cut)
  
  df$on_address_cut <- ifelse(is.na(df$on_address),"more_12",
     ifelse(df$on_address<=12,"less_12","more_12"))
  df$on_address <- as.factor(df$on_address_cut)
  
  df$purpose_cut <- ifelse(is.na(df$purpose), "other",
     ifelse(df$purpose %in% c(2),"other",
     ifelse(df$purpose %in% c(5),"other",
     ifelse(df$purpose %in% c(4),"4",
     ifelse(df$purpose %in% c(6),"other","other")))))
  df$purpose <- as.factor(df$purpose_cut)
  
  df$status_work_cut <- ifelse(is.na(df$status_work), "other",
     ifelse(df$status_work %in% c(5,9,10,12), "5_9_10_12","other"))
  df$status_work <- as.factor(df$status_work_cut)
  
  df$cred_count_total_cut <- ifelse(is.na(df$cred_count_total), "more_2",
     ifelse(df$cred_count_total %in% c(0:1),"0_1","more_2"))
  df$cred_count_total <- as.factor(df$cred_count_total_cut)
  
  df$outs_overdue_ratio_total_cut <- ifelse(
    is.na(df$outs_overdue_ratio_total),"more_0.05",
    ifelse(df$outs_overdue_ratio_total==-999,"more_0.05",
    ifelse(df$outs_overdue_ratio_total<=0.05,"0_0.05",
    ifelse(df$outs_overdue_ratio_total<=0.2,"more_0.05","more_0.05"))))
  df$outs_overdue_ratio_total <- as.factor(df$outs_overdue_ratio_total_cut)
  
  df$maturity_cut <- ifelse(df$maturity<=5,"0_5","5_more")
  df$maturity <- as.factor(df$maturity_cut)
  
  df$credits_cum_cut <- ifelse(is.na(df$credits_cum),"3_6",
     ifelse(df$credits_cum %in% c(1,2),"1_2",
     ifelse(df$credits_cum %in% c(3:6),"3_6",
     ifelse(df$credits_cum %in% c(7:10),"7_10","11+"))))
  df$credits_cum <- as.factor(df$credits_cum_cut)
  
  df$max_delay_cut <- 
    ifelse(is.na(df$max_delay),"31_180",
    ifelse(df$max_delay<=30,"0_30",
    ifelse(df$max_delay<=180,"31_180","180+")))
  df$max_delay <- as.factor(df$max_delay_cut)
  
  df$ratio_nb_payments_prev_cut <- 
    ifelse(is.na(df$ratio_nb_payments_prev),"more_0.25",
    ifelse(df$ratio_nb_payments_prev<=0.1,"0_0.1",
    ifelse(df$ratio_nb_payments_prev<=0.25,"0.1_0.25","more_0.25")))
  df$ratio_nb_payments_prev <- as.factor(df$ratio_nb_payments_prev_cut)
  
  df$days_diff_last_credit_cut <- 
    ifelse(is.na(df$days_diff_last_credit),"more_2",
    ifelse(df$days_diff_last_credit<=2,"less_2","more_2"))
  df$days_diff_last_credit <- as.factor(df$days_diff_last_credit_cut)
  
  scoring_df <- scoring_df[which(scoring_df$amount == df$amount & 
      scoring_df$period == df$installments),][1,]
  
  for(i in 1:nrow(scoring_df)){
    
    period_tab <- as.numeric(scoring_df$period[i])
    amount_tab <- as.numeric(scoring_df$amount[i])
    
    if(df$total_income<100 | is.na(df$total_income)){
      ratio_tab <- 0.1}
    else {
      ratio_tab <- products[products$period == period_tab & 
            products$amount == amount_tab & 
            products$product_id == all_df$product_id, ]$installment_amount/
            t_income}
    
    if (ratio_tab>=3) {ratio_tab <- 3}
    ratio_tab <- as.numeric(ratio_tab)
    df$ratio_installment_income <- ifelse(is.na(ratio_tab), 999,ratio_tab)
    df$ratio_installment_income_cut <- 
      ifelse(df$ratio_installment_income<=0.05,"0_0.05","more_0.05")
    df$ratio_installment_income<- as.factor(df$ratio_installment_income_cut)
    
    acceptable_installment_amount <- products$installment_amount[
      products$period==period_tab & products$amount==amount_tab]
    
    amount_diff_loc <- amount_tab - prev_amount$amount
    df$amount_diff <- ifelse(is.na(amount_diff_loc),"less_150",
           ifelse(amount_diff_loc<=150,"less_150","200_more"))
    
    # Apply logistic model to each amount and installment
    apply_logit <- gen_apply_model(df,coefficients)
    scoring_df$score[i] <- apply_logit
    scoring_df$score[i] <- gen_group_scores(scoring_df$score[i],
                                            all_df$office_id,1,0,0)
    scoring_df$pd[i] <- round(apply_logit,3)
    
    # Compute flag of disposable income
    product_tab <- subset(products, products$product_id==all_df$product_id & 
        products$period==as.numeric(period_tab) &
        products$amount==as.numeric(amount_tab))
    
    scoring_df$color[i] <- 0
    
    scoring_df$color[i] <- 
          ifelse(scoring_df$score[i]=="Bad", 1, 
          ifelse(scoring_df$score[i]=="Indeterminate", 2,
          ifelse(scoring_df$score[i]=="Good 1", 3,
          ifelse(scoring_df$score[i]=="Good 2", 4,
          ifelse(scoring_df$score[i]=="Good 3", 5,
          ifelse(scoring_df$score[i]=="Good 4",6,NA))))))
    
    if(crit_po!=0){
      scoring_df$installment_amount[i] <- product_tab$installment_amount
    }
  }
  
  return(list(scoring_df$pd,scoring_df$score))
}

