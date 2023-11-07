
##############################################################################
######## Functions to apply logisit regression on application City Cash ######
##############################################################################

gen_app_citycash <- function(df,scoring_df,products,df_Log_CityCash_App,period,
                             all_df,prev_amount,amount_tab,
                             t_income,disposable_income_adj,base_dir){
  
  # Load rdata
  load(file.path(base_dir,"rdata","citycash_app.rdata"))

  # Cut and bin
  df$age_cut <- 
    ifelse(df$age<=20,"20_less",
    ifelse(df$age<=30,"21_30",
    ifelse(df$age<=45,"31_45",
    ifelse(df$age<=50,"45_58","58_more"))))
  df$age <- as.factor(df$age_cut)
  df$gender <- as.factor(df$gender)
  df$education_cut <- 
    ifelse(is.na(df$education),"2",
    ifelse(df$education==1,"1",
    ifelse(df$education %in% c(3),"3",
    ifelse(df$education %in% c(4),"4","2"))))
  df$education <- as.factor(df$education_cut)
  df$experience_employer_cut <- 
    ifelse(is.na(df$experience_employer),"less_60",
    ifelse(df$experience_employer<=60,"less_60","more_60"))
  df$experience_employer <- as.factor(df$experience_employer_cut)
  df$purpose_cut <- 
    ifelse(is.na(df$purpose), "other",
    ifelse(df$purpose %in% c(2),"2",
    ifelse(df$purpose %in% c(6),"6","other")))
  df$purpose <- as.factor(df$purpose_cut)
  df$status_work_cut <- ifelse(is.na(df$status_work), "other",
    ifelse(df$status_work %in% c(5,9,10,12), "5_9_10_12","other"))
  df$status_work <- as.factor(df$status_work_cut)
  df$on_address_cut <- ifelse(is.na(df$on_address),"24_360",
    ifelse(df$on_address<=12,"less_12",
    ifelse(df$on_address<=360,"24_360","360+")))
  df$on_address <- as.factor(df$on_address_cut)
  df$outs_overdue_ratio_total_cut <- 
    ifelse(is.na(df$outs_overdue_ratio_total),"other",
    ifelse(df$outs_overdue_ratio_total==-999,"other",
    ifelse(df$outs_overdue_ratio_total==0,"0",
    ifelse(df$outs_overdue_ratio_total<=0.2,"0.01_0.2","other"))))
  df$outs_overdue_ratio_total <- as.factor(df$outs_overdue_ratio_total_cut)
  df$status_finished_total_cut <- ifelse(is.na(df$status_finished_total),
     "0_71_72",ifelse(df$status_finished_total %in% c(0,71,72),"0_71_72",
     "73_74_75"))
  df$status_finished_total <- as.factor(df$status_finished_total_cut)
  df$flag_location_cut <- 
    ifelse(is.na(df$risky_address), "0",
    ifelse(df$risky_address==1,"1","0"))
  df$flag_location <- as.factor(df$flag_location_cut)
  df$self_approve_cut <- ifelse(is.na(df$self_approval), "0","1")
  df$self_approve <- as.factor(df$self_approve_cut)
  
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
    df$ratio_installment_income_cut <- 
      ifelse(df$ratio_installment_income<=0.05,"0_0.05",
      ifelse(df$ratio_installment_income<=0.06,"0.05_0.06","more_0.06"))
    df$ratio_installment_income<- as.factor(df$ratio_installment_income_cut)
    
    # Apply logistic model to each amount and installment
    apply_logit <- predict(df_Log_CityCash_App, newdata=df, type="response")
    scoring_df$score[i] <- apply_logit
    scoring_df$score[i] <- gen_group_scores(scoring_df$score[i],
                                            all_df$office_id,0,0,0)
    scoring_df$pd[i] <- round(apply_logit,3)
    
    # Compute flag of disposable income
    product_tab <- subset(products, products$product_id==all_df$product_id & 
                            products$period==as.numeric(period_tab) &
                            products$amount==as.numeric(amount_tab))
    
    scoring_df$color[i] <- 0
    
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


