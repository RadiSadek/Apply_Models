
#########################################################################
######## Functions to apply logisit regression on repeat Credirect ######
#########################################################################

gen_app_citycash_fraud <- function(df,scoring_df,base_dir){
  
  # Load rdata
  load(file.path(base_dir,"rdata","citycash_app_fraud.rdata"))
  
  df$city_pop_cat <- 
    ifelse(is.na(df$city_pop),"no_info",
    ifelse(df$city_pop<=2000,"0_2k",
    ifelse(df$city_pop<=50000,"2k_50k",
    ifelse(df$city_pop<=200000,"50k_200k",
    ifelse(df$city_pop<=1000000,"200k_1M","1M+")))))
  df$self_approve <- df$self_approval
  df$flag_location <- df$risky_address
  df$flag_location2 <- NA

  # Set missing data
  na_cols <- c("age","gender",
     "ownership","education","leasing",
     "marital_status","status_work","on_address","experience_employer",
     "purpose","dwelling_type","phone_plan","household_children",
     "household_total","self_approve","flag_location",
     "source_entity_count_total","cred_count_total","outs_overdue_ratio_total",
     "status_active_total","status_finished_total",
     "hear_about_us","city_pop","city_pop_cat")
  for (col in na_cols) {
    df[is.na(df[,col]),col] <- -999
  }
  
  # Factor certain variables
  asfac <- c("gender","ownership","education","leasing","marital_status",
             "status_work","purpose","dwelling_type","phone_plan",
             "self_approve","flag_location", "hear_about_us",
             "city_pop_cat")
  df[,c(asfac)] <- lapply(df[,c(asfac)], factor)

  # Apply model if not most are empty
  if(length(which((df[,c("ownership","marital_status","status_work",
      "hear_about_us","city_pop")] == -999)))>=4){
    pd <- NA
  } else {
    pd <- round(predict(gbm_citycash_fraud,df,type = "response"),3)
  }

  return(pd)
}


