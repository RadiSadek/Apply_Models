
##########################################################################
######## Functions to apply logistic regression for PTC (Credirect) ######
##########################################################################

# Apply ptc model to flex (repeat)
gen_ptc_credirect_flex <- function(all_df,all_id,application_id,all_credits,
                                   db_name){
  
  all_df$AGE <- as.factor(
    ifelse(all_df$age<=21,"18-21",
    ifelse(all_df$age<=40,"22-40","41+")))
  
  all_df$TIME_ON_ADDRESS <- as.factor(
    ifelse(is.na(all_df$on_address),"0-10Years/NA",
    ifelse(all_df$on_address<=120,"0-10Years/NA","10+Years")))
  
  all_df$credits_cum_credirect <- nrow(subset(all_id,all_id$company_id==2))
  all_df$CONSECUTIVE_LOAN<-as.factor(
    ifelse(all_df$credits_cum_credirect==1,"2nd",
    ifelse(all_df$credits_cum_credirect<=2,"3rd",
    ifelse(all_df$credits_cum_credirect<=6,"4th-7th","8th+"))))
  
  all_df$DAYS_SINCE_LAST <- as.factor(
    ifelse(is.na(gen_days_since_app_ptc(all_id,application_id)),"1-15",
    ifelse(gen_days_since_app_ptc(all_id,application_id)<=0,"0/16-90",
    ifelse(gen_days_since_app_ptc(all_id,application_id)<=15,"1-15",
    ifelse(gen_days_since_app_ptc(all_id,application_id)<=90,"0/16-90",
    ifelse(gen_days_since_app_ptc(all_id,application_id)<=180,"91-180",
           "181+"))))))
  
  all_df$YIELD <- gen_avg_profit(db_name,all_id[all_id$id==application_id,])/
    all_df$amount
  all_df$YIELD <- as.factor(
    ifelse(all_df$YIELD<=1.05,"100-105%",
    ifelse(all_df$YIELD<=1.25,"105-125%",
    ifelse(all_df$YIELD<=1.5,"125-150%",
    ifelse(all_df$YIELD<=1.8,"150-180%","180%+")))))
  
  all_df$DAYS_DELAY <- as.factor(ifelse(all_df$days_delay<=60,"0-60","61+"))
  
  if("src_ent_fin" %in% names(all_df)){
  all_df$NONBANK_CREDITORS <- as.factor(
    ifelse(is.na(all_df$src_ent_fin),"0-1",
    ifelse(all_df$src_ent_fin<=1,"0-1",
    ifelse(all_df$src_ent_fin<=4,"2-4",
    ifelse(all_df$src_ent_fin<=6,"5-6","7+")))))
  } else {
    all_df$NONBANK_CREDITORS <-"0-1"
  }
  
  if("ckr_fin_fin" %in% names(all_df)){
  all_df$NONBANK_DELAY_FINISHED <- as.factor(
    ifelse(is.na(all_df$ckr_fin_fin),"NoCredit/0-180DaysDelay",
    ifelse(all_df$ckr_fin_fin %in% c(0,71:73),"NoCredit/0-180DaysDelay",
       "181+DaysDelay")))
  } else {
    all_df$NONBANK_DELAY_FINISHED <-"NoCredit/0-180DaysDelay"
  }
  
  # Apply model
  return(predict(credirect_flex_model,newdata=all_df,type="response"))
  
}
 
# Apply ptc model to gratis
gen_ptc_credirect_gratis <- function(all_df,all_id,application_id,all_credits,
                                     db_name){
  
  all_df$AGE <- as.factor(
    ifelse(all_df$age<=21,"18-21",
    ifelse(all_df$age<45,"22-44","45+")))
  
  all_df$EDUCATION <- as.factor(
    ifelse(is.na(all_df$education),"HighSchool/lower",
    ifelse(all_df$education<=2,"HighSchool/lower","University/college")))
  
  all_df$MARITAL_STATUS <- as.factor(
    ifelse(is.na(all_df$marital_status),"Relationship/Widowed",
    ifelse(all_df$marital_status %in% c(2,3),"Single/Divorced",
         "Relationship/Widowed")))
  
  all_df$EMPLOYMENT_CONTRACT <- as.factor(
    ifelse(is.na(all_df$status_work),"IndefiniteLabourContract",
    ifelse(all_df$status_work==2,"IndefiniteLabourContract","Other")))
  
  if("src_ent_fin" %in% names(all_df)){
  all_df$NONBANK_CREDITORS <- as.factor(
    ifelse(is.na(all_df$src_ent_fin),"2-3",
    ifelse(all_df$src_ent_fin<=1,"0-1",
    ifelse(all_df$src_ent_fin<=3,"2-3",
    ifelse(all_df$src_ent_fin<=6,"4-6","7+")))))
   } else {
     all_df$NONBANK_CREDITORS <- "2-3"
   }
  
  if("ckr_fin_fin" %in% names(all_df)){
  all_df$NONBANK_DELAY_FINISHED <- as.factor(
    ifelse(is.na(all_df$ckr_fin_fin),"NoDelay/NoCredit",
    ifelse(all_df$ckr_fin_fin==0,"NoDelay/NoCredit",
    ifelse(all_df$ckr_fin_fin %in% c(71:74),"31-360DaysDelay",
            "361+DaysDelay"))))
  } else {
    all_df$NONBANK_DELAY_FINISHED <- "NoDelay/NoCredit"
  }
  
  if("overdue_principal" %in% names(all_df)){
  all_df$OVERDUE_PRINCIPAL <- as.factor(
    ifelse(is.na(all_df$overdue_principal),"0-150",
    ifelse(all_df$overdue_principal <=150,"0-150","150+")))
  } else {
    all_df$OVERDUE_PRINCIPAL <- "0-150"
  }
  
  if("src_ent_bank" %in% names(all_df)){
  all_df$BANK_CREDITORS <- as.factor(
    ifelse(is.na(all_df$src_ent_bank),"1-3",
    ifelse(all_df$src_ent_bank==0,"0/4+",
    ifelse(all_df$src_ent_bank<=3,"1-3","0/4+"))))
  } else {
    all_df$BANK_CREDITORS <- "1-3"
  }
  
  if("ckr_act_bank" %in% names(all_df)){
  all_df$BANK_DELAY_ACTIVE <- as.factor(
    ifelse(is.na(all_df$ckr_act_bank),"0-30Days/NoCredit",
    ifelse(all_df$ckr_act_bank<=70,"0-30Days/NoCredit","31+Days")))
  } else {
    all_df$BANK_DELAY_ACTIVE <- "0-30Days/NoCredit"
  }
  
  all_df$PREVIOUS_APPS <- as.factor(
    ifelse(gen_prev_apps_ptc(all_credits,application_id,db_name)==1,
      "Yes","No"))

  all_df$ACTIVE_CITYCASH <- as.factor(
    ifelse(gen_active_other_brand(all_id,application_id,1)==1,"Yes","No"))
  
  # Apply model
  return(predict(credirect_gratis_model,newdata=all_df,type="response"))
  
}

# Apply ptc model to first terminated installments
gen_ptc_credirect_first_installments <- function(all_df,all_id,application_id,
                                                 all_credits,db_name){
  
  all_df$AGE <- as.factor(
    ifelse(all_df$age<40,"18-39",
    ifelse(all_df$age<=50,"40-50","51+")))
  
  if("cred_count_fin" %in% names(all_df)){
  all_df$NONBANK_CREDITS <- as.factor(
    ifelse(is.na(all_df$cred_count_fin),"NoCredits",
    ifelse(all_df$cred_count_fin<=0,"NoCredits",
    ifelse(all_df$cred_count_fin<=4,"1-4Credits","5+Credits"))))
  } else {
    all_df$NONBANK_CREDITS <- "NoCredits"
  }
  
  if("status_finished_total" %in% names(all_df)){
  all_df$MAX_DELAY_FINISHED <- as.factor(
    ifelse(is.na(all_df$status_finished_total),"0-360DaysDelay/NoCredit",
    ifelse(all_df$status_finished_total<=74,"0-360DaysDelay/NoCredit", 
         "361+DaysDelay")))
  } else {
    all_df$MAX_DELAY_FINISHED <- "0-360DaysDelay/NoCredit"
  }
  
  all_df$DAYS_DELAY <- as.factor(
    ifelse(all_df$days_delay<=90,"0-90",     
    ifelse(all_df$days_delay<=180,"91-180","181+")))
  
  all_df$PREVIOUS_APPS <-as.factor(
    ifelse(gen_prev_apps_ptc(all_credits,application_id,db_name)==1,"Yes","No"))
  
  all_df$ACTIVE_CITYCASH <- as.factor(
    ifelse(gen_active_other_brand(all_id,application_id,1)==1,"Yes","No"))
  
  all_df$PROFIT_FROM_LAST <- as.factor(
    ifelse(gen_profit_id(db_name,all_id,application_id)<=150,"0-150",
    ifelse(gen_profit_id(db_name,all_id,application_id)<=250,"150-250",
    ifelse(gen_profit_id(db_name,all_id,application_id)<=500,"250-500",
    ifelse(gen_profit_id(db_name,all_id,application_id)<=1000,"500-1000",
           "1000+")))))
  
  # Apply model
  return(predict(credirect_consumer_new_model,newdata=all_df,type="response"))
  
}

# Apply ptc model to repeat terminated installments
gen_ptc_credirect_repeat_installments <- function(all_df,all_id,application_id,
                                                  all_credits,db_name){
  
  all_df$AGE <- as.factor(
    ifelse(all_df$age<30,"18-29",
    ifelse(all_df$age<45,"30-44","45+")))
  
  all_df$YIELD <- gen_avg_profit(db_name,all_id[all_id$id==application_id,])/
    all_df$amount
  all_df$YIELD <- as.factor(
    ifelse(all_df$YIELD<=1.1,"100-110%",
    ifelse(all_df$YIELD<=1.3,"110-130%",
    ifelse(all_df$YIELD<=1.8,"130-180%",
    ifelse(all_df$YIELD<=2.1,"180-210%","210+%")))))
  
  all_df$credits_cum_credirect <- nrow(subset(all_id,all_id$company_id==2))
  all_df$CONSECUTIVE_LOAN <- as.factor(
    ifelse(all_df$credits_cum_credirect==1,"2nd",
    ifelse(all_df$credits_cum_credirect==2,"3rd",
    ifelse(all_df$credits_cum_credirect<=4,"4th-5th",
    ifelse(all_df$credits_cum_credirect<=8,"6th-9th","10th+")))))
  
  all_df$DAYS_SINCE_LAST <- as.factor(
    ifelse(is.na(gen_days_since_app_ptc(all_id,application_id)),
           "15-60DaysOrImmediate",
    ifelse(gen_days_since_app_ptc(all_id,application_id)<=0,
           "15-60DaysOrImmediate",
    ifelse(gen_days_since_app_ptc(all_id,application_id)<=14,"1-14Days",
    ifelse(gen_days_since_app_ptc(all_id,application_id)<=60,
          "15-60DaysOrImmediate","60+Days")))))
  
  all_df$ACTIVE_CITYCASH <- as.factor(
    ifelse(gen_active_other_brand(all_id,application_id,1)==0,"No","Yes"))
  
  has_refinance <- nrow(subset(all_id,
    all_id$sub_status==123 & all_id$company_id==2))
  all_df$LIFETIME_REFINANCE <- as.factor(
    ifelse(is.na(has_refinance),"0",ifelse(has_refinance==0,"0","1+")))
  
  if("src_ent_fin" %in% names(all_df)){
  all_df$NONBANK_CREDITORS <- as.factor(
    ifelse(is.na(all_df$src_ent_fin),"0-2",
    ifelse(all_df$src_ent_fin<=2,"0-2",
    ifelse(all_df$src_ent_fin<=4,"3-4","5+"))))
  } else {
    all_df$NONBANK_CREDITORS <- "0-2"
  }
  
  status_active_bank_prev <- gen_query_ckr(all_df,
    all_credits,1,1,0,db_name)$status_active
  status_active_fin_prev <- gen_query_ckr(all_df,
    all_credits,2,1,0,db_name)$status_active
  all_df$status_active_total_prev <-
    ifelse(status_active_fin_prev>status_active_bank_prev, 
           status_active_fin_prev,status_active_bank_prev)
  
  if("status_active_total_prev" %in% names(all_df)){
  all_df$MAX_DELAY_ACTIVE_CHANGE <- as.factor(
    ifelse(is.na(all_df$status_active_total_prev),"Same/Improved",
    ifelse(all_df$status_active_total_prev<all_df$status_active_total,
           "Worsened","Same/Improved"))) 
  } else {
    all_df$MAX_DELAY_ACTIVE_CHANGE <- "Same/Improved"
  }
  
  # Apply model
  return(predict(credirect_consumer_repeat_model,newdata=all_df,
      type="response"))
  
}
