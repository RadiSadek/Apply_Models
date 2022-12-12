
##########################################################################
######## Functions to apply logistic regression for PTC (City Cash) ######
##########################################################################

# Apply ptc model to citycash
gen_ptc_citycash <- function(all_df){
  
  all_df$education <- ifelse(is.na(all_df$education), "other",
      ifelse(all_df$education==1,"1",
      ifelse(all_df$education==2,"other","other")))   
  all_df$education <- as.factor(all_df$education)
  
  all_df$gender <- ifelse(all_df$gender==2,0,1)
  all_df$gender <- as.factor(all_df$gender)
  
  # For age, only the name of the bin is erroneous, not the model per se 
  all_df$age <- 
    ifelse(is.na(all_df$age),"less_40",
    ifelse(all_df$age<=40,"less_40",
    ifelse(all_df$age<=60,"55_60","more_60")))
  all_df$age<- as.factor(all_df$age)
  
  all_df$on_address_cut <- ifelse(is.na(all_df$on_address),"more_48",
     ifelse(all_df$on_address<=48,"1_48","more_48"))
  all_df$on_address <- as.factor(all_df$on_address_cut)
  
  all_df$cession_total <- ifelse(is.na(all_df$cession_bank),0,
    all_df$cession_bank) + 
    ifelse(is.na(all_df$cession_fin),0,all_df$cession_fin)
  all_df$cession_total <- ifelse(is.na(all_df$cession_total),"0_500",
    ifelse(all_df$cession_total<=500,"0_500","more_500"))
  all_df$cession_total <- as.factor(all_df$cession_total)
  
  all_df$source_entity_count_total <- 
    ifelse(is.na(all_df$source_entity_count_total),"1_2_3",
    ifelse(all_df$source_entity_count_total %in% c(0),"0",
    ifelse(all_df$source_entity_count_total %in% c(1,2,3),"1_2_3",
    ifelse(all_df$source_entity_count_total %in% c(4),"4",
    ifelse(all_df$source_entity_count_total %in% c(5,6),"5_6",
    ifelse(all_df$source_entity_count_total>=7,"more_7","other"))))))
  all_df$source_entity_count_total <- as.factor(
    all_df$source_entity_count_total)
  
  all_df$days_delay <- ifelse(all_df$days_delay<=90,"0_90","more_90")
  all_df$days_delay <- as.factor(all_df$days_delay)
  
  all_df$credits_cum <- ifelse(all_df$credits_cum<=1,"1",
     ifelse(all_df$credits_cum<=4,"2_4",ifelse(all_df$credits_cum<=6,"5_6",
     ifelse(all_df$credits_cum<=9,"7_9","more_10"))))
  all_df$credits_cum <- as.factor(all_df$credits_cum)
  
  all_df$ratio_nb_payments <- 
    ifelse(all_df$ratio_nb_payments<=0.05,"0_0.05",
    ifelse(all_df$ratio_nb_payments<=0.2,"0.05_0.2",
    ifelse(all_df$ratio_nb_payments<=0.8,"0.2_0.8","more_0.8")))
  all_df$ratio_nb_payments <- as.factor(all_df$ratio_nb_payments)
  
  all_df$profit_avg <-ifelse(all_df$profit_avg<=100,"0_100",
    ifelse(all_df$profit_avg<=500,"150_500","more_500"))
  all_df$profit_avg <- as.factor(all_df$profit_avg)
  
  all_df$ratio_rej <- ifelse(all_df$ratio_rej<=0.25,"0_0.25","more_0.25")
  all_df$ratio_rej <- as.factor(all_df$ratio_rej)
  
  # Apply model
  return(predict(df_Log_CityCash_PTC,newdata=all_df, type="response"))
  
}
 
