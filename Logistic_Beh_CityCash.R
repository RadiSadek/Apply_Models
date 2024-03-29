
#########################################################################
######## Functions to apply logisit regression on repeat City Cash ######
#########################################################################

gen_beh_citycash <- function(df,scoring_df,products,df_Log_beh_CityCash,period,
                             all_id,all_df,prev_amount,amount_tab,
                             t_income,disposable_income_adj,crit_po,base_dir){
  
  # Load rdata
  load(file.path(base_dir,"rdata","citycash_repeat_coeffs.rdata"))
  names(coefficients) <- c("coeff")
  
  # Cut and bin
  df$age_cut <- 
    ifelse(df$age<=24,"24_less",
    ifelse(df$age<=36,"25_36",
    ifelse(df$age<=52,"37_52",
    ifelse(df$age<=60,"53_60","more_60"))))
  df$age <- as.factor(df$age_cut)
  
  df$gender <- as.factor(df$gender)
  
  df$education_cut <- ifelse(is.na(df$education),"2",
    ifelse(df$education==1,"1",
    ifelse(df$education %in% c(3),"3_4",
    ifelse(df$education %in% c(4),"3_4","2"))))
  df$education <- as.factor(df$education_cut)
  
  df$marital_status_cut <- ifelse(is.na(df$marital_status), "other",
     ifelse(df$marital_status %in% c(1,4), "other",
    ifelse(df$marital_status %in% c(2,3),"other",
     ifelse(df$marital_status %in% c(5),"5","other"))))
  df$marital_status <- as.factor(df$marital_status_cut)
  
  df$status_work_cut <- ifelse(is.na(df$status_work), "other",
     ifelse(df$status_work %in% c(5,9,10,12), "5_9_10_12","other"))
  df$status_work <- as.factor(df$status_work_cut)
  
  df$on_address_cut <- ifelse(is.na(df$on_address),"24_360",
     ifelse(df$on_address<=12,"less_12",
     ifelse(df$on_address<=360,"24_360","360+")))
  df$on_address <- as.factor(df$on_address_cut)
  
  df$experience_employer_cut <- 
    ifelse(is.na(df$experience_employer),"12_120",
    ifelse(df$experience_employer<=3,"0_3",
    ifelse(df$experience_employer<=12,"3_12",
    ifelse(df$experience_employer<=120,"12_120","120+"))))
  df$experience_employer <- as.factor(df$experience_employer_cut)
  
  df$purpose_cut <- ifelse(is.na(df$purpose), "other",
     ifelse(df$purpose %in% c(2),"2",
     ifelse(df$purpose %in% c(5),"5",
     ifelse(df$purpose %in% c(4),"4",
     ifelse(df$purpose %in% c(6),"6","other")))))
  df$purpose <- as.factor(df$purpose_cut)
  
  df$self_approval <- as.factor(df$self_approval)
  
  df$credits_cum_cut <- ifelse(is.na(df$credits_cum),"3_4",
     ifelse(df$credits_cum==1,"1",
     ifelse(df$credits_cum==2,"2",
     ifelse(df$credits_cum %in% c(3,4),"3_4",
     ifelse(df$credits_cum %in% c(5,6),"5_6",
     ifelse(df$credits_cum %in% c(7:10),"7_10","11+"))))))
  df$credits_cum <- as.factor(df$credits_cum_cut)
  
  df$flag_high_last_paid <- as.factor(df$flag_high_last_paid)
  
  df$ratio_nb_payments_prev_cut <- 
     ifelse(is.na(df$ratio_nb_payments_prev),"0_0.7",
     ifelse(df$ratio_nb_payments_prev<=0.7,"0_0.7",
     ifelse(df$ratio_nb_payments_prev<=0.9,"0.7_0.9","0.9_1")))
  df$ratio_nb_payments_prev <- as.factor(df$ratio_nb_payments_prev_cut)
  
  df$max_delay_cut <- 
    ifelse(is.na(df$max_delay),"16_36",
    ifelse(df$max_delay<=4,"0_4",
    ifelse(df$max_delay<=9,"5_9",
    ifelse(df$max_delay<=15,"10_15",
    ifelse(df$max_delay<=36,"16_36",
    ifelse(df$max_delay<=360,"36_360","360+"))))))
  df$max_delay <- as.factor(df$max_delay_cut)
  
  df$source_entity_count_total_cut <- 
    ifelse(is.na(df$source_entity_count_total),"more_2",
    ifelse(df$source_entity_count_total<=1, "0_1","more_2"))
  df$source_entity_count_total <- as.factor(df$source_entity_count_total_cut)
  
  df$outs_overdue_ratio_total_cut <- ifelse(
    is.na(df$outs_overdue_ratio_total),"more_0.05",
    ifelse(df$outs_overdue_ratio_total==-999,"more_0.05",
    ifelse(df$outs_overdue_ratio_total<=0.05,"0_0.05",
    ifelse(df$outs_overdue_ratio_total<=0.2,"more_0.05","more_0.05"))))
  df$outs_overdue_ratio_total <- as.factor(df$outs_overdue_ratio_total_cut)
  
  df$prev_other_brand <- as.factor(df$prev_other_brand)
  
  df$refinance_ratio_cut <- ifelse(is.na(df$refinance_ratio),"less_1",
     ifelse(df$refinance_ratio<1,"less_1","1"))
  df$refinance_ratio <- as.factor(df$refinance_ratio_cut)
  
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
    df$amount_diff <- ifelse(is.na(amount_diff_loc),"0_150",
           ifelse(amount_diff_loc<0,"less_0",
           ifelse(amount_diff_loc>150,"200_more","0_150")))
    
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
  return(scoring_df)
}

