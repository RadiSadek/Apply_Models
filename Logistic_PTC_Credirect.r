
##############################################################
######## Functions to apply logistic regression for PTC ######
##############################################################

# Apply ptc model to flex (repeat)
gen_ptc_credirect_flex <- function(all_df,all_id,application_id){
  
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
  
  diff_time <- difftime(
      all_id$signed_at[all_id$id==application_id],
      all_id$deactivated_at[all_id$id==
      max(subset(all_id,all_id$id!=application_id & all_id$company_id==2)$id)],
      units = c("days"))
  all_df$DAYS_SINCE_LAST <- as.factor(
    ifelse(is.na(diff_time),"1-15",
    ifelse(diff_time<=0,"0/16-90",
    ifelse(diff_time<=15,"1-15",
    ifelse(diff_time<=90,"0/16-90",
    ifelse(diff_time<=180,"91-180","181+"))))))
  
  all_df$YIELD <- gen_avg_profit(db_name,all_id[all_id$id==application_id,])/
    all_df$amount
  all_df$YIELD <- as.factor(
    ifelse(all_df$YIELD<=1.05,"100-105%",
    ifelse(all_df$YIELD<=1.25,"105-125%",
    ifelse(all_df$YIELD<=1.5,"125-150%",
    ifelse(all_df$YIELD<=1.8,"150-180%","180%+")))))
  
  all_df$DAYS_DELAY <- as.factor(ifelse(all_df$days_delay<=60,"0-60","61+"))
  
  all_df$NONBANK_CREDITORS<-as.factor(
    ifelse(is.null(all_df$src_ent_fin),"0-1",
    ifelse(is.na(all_df$src_ent_fin),"0-1",
    ifelse(all_df$src_ent_fin<=1,"0-1",
    ifelse(all_df$src_ent_fin<=4,"2-4",
    ifelse(all_df$src_ent_fin<=6,"5-6","7+"))))))
  
  all_df$NONBANK_DELAY_FINISHED<-as.factor(
    ifelse(is.null(all_df$ckr_fin_fin),"NoCredit/0-180DaysDelay",
     ifelse(is.na(all_df$ckr_fin_fin),"NoCredit/0-180DaysDelay",
     ifelse(all_df$ckr_fin_fin%in%c(0,71:73),"NoCredit/0-180DaysDelay",
       "181+DaysDelay"))))
  
  # Apply model
  return(predict(credirect_flex_model,newdata=all_df,type="response"))
  
}
 
# Apply ptc model to gratis
gen_ptc_credirect_gratis <- function(all_df,all_id,application_id){
  
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
  
  all_df$NONBANK_CREDITORS <- as.factor(
    ifelse(is.null(all_df$src_ent_fin),"0-1",
    ifelse(is.na(all_df$src_ent_fin),"2-3",
    ifelse(all_df$src_ent_fin<=1,"0-1",
    ifelse(all_df$src_ent_fin<=3,"2-3",
    ifelse(all_df$src_ent_fin<=6,"4-6","7+"))))))
  
  all_df$NONBANK_DELAY_FINISHED <- as.factor(
    ifelse(is.null(all_df$ckr_fin_fin),"NoDelay/NoCredit",
    ifelse(is.na(all_df$ckr_fin_fin),"NoDelay/NoCredit",
    ifelse(all_df$ckr_fin_fin==0,"NoDelay/NoCredit",
    ifelse(all_df$other_status_finished%in%c(71:74),"31-360DaysDelay",
            "361+DaysDelay")))))
  
  overdue_principal <- 
    ifelse(is.na(all_df$outs_overdue_bank),0,all_df$outs_overdue_bank) + 
    ifelse(is.na(all_df$outs_overdue_fin),0,all_df$outs_overdue_fin)
  all_df$OVERDUE_PRINCIPAL <- as.factor(
    ifelse(is.null(overdue_principal),"0-150",
    ifelse(is.na(overdue_principal),"0-150",
    ifelse(overdue_principal<=150,"0-150","150+"))))
  
  all_df$BANK_CREDITORS <- as.factor(
    ifelse(is.null(all_df$src_ent_bank),"1-3",
    ifelse(is.na(all_df$src_ent_bank),"1-3",
    ifelse(all_df$src_ent_bank==0,"0/4+",
    ifelse(all_df$src_ent_bank<=3,"1-3","0/4+")))))
  
  all_df$BANK_DELAY_ACTIVE <- as.factor(
    ifelse(is.null(all_df$ckr_act_bank),"0-30Days/NoCredit",
    ifelse(is.na(all_df$ckr_act_bank),"0-30Days/NoCredit",
    ifelse(all_df$ckr_act_bank<=70,"0-30Days/NoCredit","31+Days"))))
  
  # Apply model
  return(predict(credirect_gratis_model,newdata=all_df,type="response"))
  
}

# Apply ptc model to first terminated installments
gen_ptc_credirect_first_installments <- function(all_df,all_id,application_id){
  
  all_df$AGE <- as.factor(
    ifelse(all_df$age<40,"18-39",
    ifelse(all_df$age<=50,"40-50","51+")))
  
  all_df$NONBANK_CREDITS <- as.factor(
    ifelse(is.null(all_df$cred_count_fin),"NoCredits",
    ifelse(is.na(all_df$cred_count_fin),"NoCredits",
    ifelse(all_df$cred_count_fin<=0,"NoCredits",
    ifelse(all_df$cred_count_fin<=4,"1-4Credits","5+Credits")))))
  
  all_df$MAX_DELAY_FINISHED <- as.factor(
    ifelse(is.null(all_df$status_finished_total),"0-360DaysDelay/NoCredit",
    ifelse(is.na(all_df$status_finished_total),"0-360DaysDelay/NoCredit",
    ifelse(all_df$status_finished_total<=74,"0-360DaysDelay/NoCredit", 
         "361+DaysDelay"))))
  
  all_df$DAYS_DELAY<-as.factor(
    ifelse(all_df$days_delay<=90,"0-90",     
    ifelse(all_df$days_delay<=180,"91-180","181+")))
  
}
