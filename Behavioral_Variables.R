
####################################################################
######## Functions to compute variables for repeat customers  ######
####################################################################

# Function to get company id of previous credits
get_company_id_prev <- function(db_name,all_credits){
  products_desc_all <- gen_query(con, 
      gen_get_company_id_query(db_name))
  all_credits <- merge(all_credits, products_desc_all, 
       by.x = "product_id", by.y = "id", all.x = TRUE)
  return(all_credits)
}
# Function to get if previous credit is credirect or not 
gen_prev_online <- function(db_name,all_credits,all_df,app_id){
  all_credits_prev_online <- all_credits[order(all_credits$date),]
  all_credits_prev_online <- subset(all_credits_prev_online,
       all_credits_prev_online$status %in% c(4,5))
  all_credits_prev_online <- subset(all_credits_prev_online, 
       all_credits_prev_online$id<app_id)
  all_df$prev_online <- ifelse(all_credits_prev_online[
    rev(order(all_credits_prev_online$date)),][1,11]==2, 1, 0)
  return(all_df)
}
                            
# Function to get if last credit is eventually a hidden refinance
gen_ratio_last_amount_paid <- function(db_name,all_credits,all_df,
     application_id,products_desc,nrow_all_id,cash_flow,total_amount){

  all_credits <- all_credits[rev(order(all_credits$date)),]
  all_credits_actives <- subset(all_credits, all_credits$status==4 & 
       all_credits$id!=application_id)
  all_credits_terminated <- subset(all_credits, all_credits$status==5 & 
       all_credits$id!=application_id & 
       all_credits$company_id==products_desc$company_id)
  
  if(nrow(all_credits_actives)>=1){
    list_ids_prod_current <- unique(all_credits_actives$product_id)
    if(length(list_ids_prod_current)>1){
      list_ids_prod_current  <- unique(all_credits_actives$product_id)[1]
      for(i in 2:length(unique(all_credits_actives$product_id))){
        list_ids_prod_current <- paste(list_ids_prod_current,
           unique(all_credits_actives$product_id)[i],sep=",")
      } 
    }
    all_credits_actives <- subset(all_credits_actives, 
          all_credits_actives$company_id==products_desc$company_id)
    all_credits_actives <- all_credits_actives[rev(order(
      all_credits_actives$date)),]
    all_credits_actives <- all_credits_actives[!duplicated(
      all_credits_actives$client_id),]
    
    if(nrow(all_credits_actives)>=1){
      cash_flow_active <- gen_query(con, 
            gen_paid_amount_query(all_credits_actives$id,db_name))
      total_amount_active <- gen_query(con, 
            gen_last_cred_amount_query(all_credits_actives$id,db_name))
      
      # if actives and same company as application
      all_df$flag_high_last_paid <- ifelse(sum(cash_flow_active$amount)/
            total_amount_active$final_credit_amount<0.5, 1, 0)
      
    } else if (nrow(all_credits_terminated)>=1){
      
      # if actives and diff company as application and terminated
      all_credits_terminated <- all_credits_terminated[rev(
        order(all_credits_terminated$date)),]
      all_credits_terminated <- all_credits_terminated[!duplicated(
        all_credits_terminated$client_id),]
      cash_flow_terminated <- gen_query(con,
             gen_last_paid_amount_query(all_credits_terminated$id, db_name))
      total_amount_terminated <- gen_query(con, 
            gen_last_cred_amount_query(all_credits_terminated$id,db_name))
      all_df$flag_high_last_paid <- ifelse(all_df$days_diff_last_credit %in% 
            c(0,1,2,NA) & sum(cash_flow_terminated$amount)/
            total_amount_terminated$final_credit_amount>=0.5, 1, 0)
    } else {
      
      # if actives and different company as application and no terminated
      all_df$flag_high_last_paid <- NA
    }
  } else if (nrow_all_id<=1) {
    
    # if no actives and no terminated
    all_df$flag_high_last_paid <- NA
  } else {
    
    # if no actives and terminated
    all_df$flag_high_last_paid <- ifelse(all_df$days_diff_last_credit %in% 
      c(0,1,2) & sum(cash_flow$amount)/total_amount$final_credit_amount>=0.5, 
      1, 0)}
  
  return(all_df)
}

# Function to compute variables for repeat customers
gen_other_rep <- function(nrow_all_id,all_id,all_df,flag_credirect,
                          data_plan_main_select_def,application_id){
  
  if (nrow_all_id>1){
    all_id <- all_id[order(all_id$date),]
    all_id$credits_cum[1] <- 0
    all_id$days_diff_last_credit <- NA
    all_df$max_delay <- ifelse(!(is.na(data_plan_main_select_def)), 
        data_plan_main_select_def[1], ifelse(flag_credirect==0, 60, 10))
    for (i in 2:nrow(all_id)){
      all_id$credits_cum[i] <- 1 + all_id$credits_cum[i-1]
      all_id$days_diff_last_credit[i] <- difftime(all_id$date[i], 
        all_id$deactivated_at[i-1], units=c("days"))
    }
    all_id$days_diff_last_credit <- ifelse(all_id$days_diff_last_credit<0, NA, 
        all_id$days_diff_last_credit)
    all_id$days_diff_last_credit <- round(all_id$days_diff_last_credit, 0)
    all_id <- subset(all_id, all_id$id==application_id)
    all_id <- all_id[,c("credits_cum","days_diff_last_credit")]
    all_df <- cbind(all_df, all_id)
  } else {
    all_df$credits_cum <- 0
    all_df$days_diff_last_credit <- NA
    all_df$max_delay <- NA
  }
  return(all_df)
}

# Function to order by credits for repeat customers
gen_variables_for_rep <- function(all_id){
  list_ids <- unique(all_id$id)[1]
  for(i in 2:length(unique(all_id$id))){
    list_ids <- paste(list_ids, unique(all_id$id)[i], sep=",")
  }
  all_id$date <- ifelse(is.na(all_id$signed_at), 
                        all_id$created_at, all_id$signed_at)
  all_id <- all_id[ , -which(names(all_id) %in% c("signed_at","created_at"))]
  all_id <- all_id[order(all_id$date),]
  
  return(all_id)
  
}

# Function to get last amount paid of previous credit
gen_last_paid <- function(all_id){
  var <- gen_variables_for_rep(all_id)
  return(gen_query(con, 
     gen_last_paid_amount_query(var$id[nrow(var)-1],db_name)))
}

# Function to get total amount paid of previous credit for refinance
gen_total_last_paid <- function(var,db_name){
  result <- gen_query(con,
    gen_total_paid_amount_query(var,db_name))
  result <- sum(result[,2])
  return(result)
}

# Function to get number of unique payment days of previous credit
gen_prev_paid_days <- function(all_id){
  var <- gen_variables_for_rep(all_id)
  result <- gen_query(con, 
     gen_all_payments_query(var$id[nrow(var)-1],db_name))
  return(length(unique(result$pay_date)))
}

# Function to get total amount (with taxes) of previous credit
gen_last_total_amount <- function(all_id){
  var <- gen_variables_for_rep(all_id)
  return(total_amount <- gen_query(con, 
     gen_last_cred_amount_query(var$id[nrow(var)-1],db_name)))
}

# Function to get amount of previous credit
gen_last_prev_amount <- function(all_id){
  var <- gen_variables_for_rep(all_id)
  return(prev_amount <- gen_query(con, 
     gen_prev_amount_query(db_name,var)))
}

# Function to select id of all previous credits
gen_select_relevant_ids <- function(all_id_max_delay,nrow_all_id_max_delay){
  var <- unique(all_id_max_delay$id)[1]
  if(nrow_all_id_max_delay>1){
    for(i in 2:length(unique(all_id_max_delay$id))){
      var <- paste(var,unique(all_id_max_delay$id)[i], sep=",")}
  }
  return(var)
}

# Function to select ids only of relevant credits for max delay
gen_select_relevant_ids_max_delay <- function(db_name,all_actives_past,
                                              all_id_max_delay){
  string_id <- all_actives_past$id[1]
  if(nrow(all_actives_past)>1){
    for(i in 2:nrow(all_actives_past)){
      string_id <- paste(string_id,all_actives_past$id[i],sep=",")
    }
  }
  data_plan_main_actives_past <- suppressWarnings(dbFetch(dbSendQuery(con, 
      gen_plan_main_actives_past_query(db_name,string_id))))
  data_plan_main_actives_past$date_diff <- difftime(Sys.time(), 
      data_plan_main_actives_past$pay_day, units=c("days"))
  agg_passed_installments <- as.data.frame(aggregate(
    data_plan_main_actives_past$date_diff, 
    by=list(data_plan_main_actives_past$application_id), FUN=max))
  agg_passed_installments <- subset(agg_passed_installments, 
    agg_passed_installments$x<30)
  all_id_max_delay <- all_id_max_delay[!(all_id_max_delay$id %in% 
    agg_passed_installments$Group.1),]
  
  return(all_id_max_delay)
}

# Get maximum previous installment amount
gen_prev_max_installment <- function(db_name,input,all_df,application_id,crit,
                                     flag_cashpoint){
  
  input <- input[order(input$signed_at),]
  if(crit==0){
    input <- input[input$id!=application_id,]
  }
  all_df$period <- gen_query(con,
     gen_products_query_desc(db_name,all_df[1,]))$period
  
  for(i in 1:nrow(input)){

    input$installment_amount[i] <- gen_query(con,
      gen_max_pmt_main(db_name,input$id[i]))$max_pmt
    input$period[i] <- gen_query(con,
      gen_products_query_desc(db_name,input[i,]))$period
    
    if(input$period[i]!=all_df$period){
      input$installment_amount[i] <- gen_correct_max_installment_po(
        input$period[i],all_df$period,input$installment_amount[i])
    }
  }
  
  # Apply additional criteria to repeat Cashpoint
  if(flag_cashpoint==1){
    if(all_df$period==3){
      array <- gen_query(con, 
          gen_set_installment_query(db_name,66,600))$installment_amount
      max_install <- array[which.min(abs(array - quantile(array,0.5)))]
    } else if(all_df$period==2){
      array <- gen_query(con, 
          gen_set_installment_query(db_name,72,600))$installment_amount
      max_install <- array[which.min(abs(array - quantile(array,0.5)))]
    } else {
      array <- gen_query(con, 
          gen_set_installment_query(db_name,71,600))$installment_amount
      max_install <- array[which.min(abs(array - quantile(array,0.5)))]
    }
    prev_installment_amount <- max(input$installment_amount,max_install)
  } else {
    prev_installment_amount <- max(input$installment_amount)
  }
  return(prev_installment_amount)
}

# Function to compute installment ratio 
gen_installment_ratio <- function(db_name,all_id,all_df,application_id,crit,
     flag_cashpoint,max_prev_amount,scoring_df){
  
  # Join DPD of past credits
  all_id_here <- all_id[all_id$status %in% c(4,5) & 
                        !(all_id$big_company_id %in% c(4)),]
  if(crit==0){
    all_id_here <- all_id_here[all_id_here$id!=application_id,]
  }
  
  # Joint company ID to all_df
  all_df$company_id <- gen_query(con,
    gen_products_query_desc(db_name,all_df))$company_id

  # Subset into active and terminated
  all_id_local <- subset(all_id_here,all_id_here$status %in% c(5) & 
      all_id_here$company_id==all_df$company_id)
  all_id_local2 <- subset(all_id_here,all_id_here$status %in% c(4) & 
      all_id_here$company_id==all_df$company_id)
  
  # Get highest score
  highest_score <-  
    ifelse("Good 4" %in% names(table(scoring_df$score)),5,
    ifelse("Good 3" %in% names(table(scoring_df$score)),4,  
    ifelse("Good 2" %in% names(table(scoring_df$score)),3,   
    ifelse("Good 1" %in% names(table(scoring_df$score)),2,
    ifelse("Indeterminate" %in% names(table(scoring_df$score)),1,1)))))  

  if(nrow(all_id_local)>0 | nrow(all_id_local2)>0){
    
    # Compute optimized previous installment amount
    if(max_prev_amount<=500){
      allowed_ratio_active <- c(0.6,1.8,2,2.5,3)[highest_score]
      allowed_ratio_term <- c(2,2,2,2.5,3)[highest_score]
    } else {
      allowed_ratio_active <- c(0.6,1,1.1,1.3,1.5)[highest_score]
      allowed_ratio_term <- c(1,1.1,1.2,1.3,1.5)[highest_score]
    }
    
    final_prev_installment_amount <-
      ifelse(nrow(all_id_local2)>0,
             allowed_ratio_active*
               gen_prev_max_installment(db_name,all_id_local2,
                  all_df,application_id,crit,flag_cashpoint),
      ifelse(nrow(all_id_local)>0 & nrow(all_id_local2)==0,
             allowed_ratio_term*
               gen_prev_max_installment(db_name,all_id_local,
                  all_df,application_id,crit,flag_cashpoint),
             allowed_ratio_term*
               gen_prev_max_installment(db_name,all_id_local,
                  all_df,application_id,crit,flag_cashpoint)))
  } else {
    final_prev_installment_amount <- Inf
  }
  return(final_prev_installment_amount)
}

# Get step according to number of paid installments at termination (Credirect)
gen_prev_deactiv_date <- function(db_name,all_df,all_id,application_id){
  
  all_id_local <- subset(all_id,all_id$company_id==gen_query(con,
    gen_products_query_desc(db_name,all_df))$company_id)
  all_id_local <- subset(all_id_local,all_id_local$status %in% c(4,5))
  
  # Remove if current application_id is too soon
  all_id_local$difftime <- difftime(Sys.time(),all_id_local$signed_at,
                                    units = c("days"))
  all_id_local <- all_id_local[!(all_id_local$status=4 & 
                                 all_id_local$difftime<=1),]

  if(nrow(all_id_local)>0){
    all_id_local <- all_id_local[order(all_id_local$deactivated_at),]
    all_id_local <- all_id_local[nrow(all_id_local),]
    all_id_local <- merge(all_id_local,
      gen_query(con,gen_products_query_desc(db_name,all_id_local))[,
      c("id","type")],by.x = "product_id",by.y = "id",all.x = TRUE)
    all_id_local$next_salary <- ifelse(all_id_local$type==4,1,0)
    if(is.na(all_id_local$deactivated_at)){
      all_id_local$deactivated_at <- Sys.time()
    }
    
    # Get number of passed installments at termination of previous
    passed_install_at_pay <- as.numeric(gen_query(con,
       gen_passed_install_before_query(db_name,
       all_id_local$id,all_id_local$deactivated_at))$passed_installments)
    
    if(all_id_local$type!=4 & all_id_local$status!=4){
      passed_install_at_pay  <- passed_install_at_pay + 
        gen_ratio_unpassed_installment(db_name,all_id_local$signed_at,
         all_id_local$deactivated_at,all_id_local$id)
    }
    
  } else {
    passed_install_at_pay <- NA
  }
  return(cbind(passed_install_at_pay,all_id_local$next_salary))
}

# Compute if number of varnat >=2 and before 6 months
gen_nb_varnat <- function(all_credits){
  
  all_credits_local <- subset(all_credits,all_credits$sub_status==122)
  if(nrow(all_credits_local)>0){
    all_credits_local$time_passed <- difftime(Sys.time(),
       all_credits_local$deactivated_at,units=c("days"))
    all_credits_local <- subset(all_credits_local,
       all_credits_local$time_passed<360)
    nrow_varnat <- nrow(all_credits_local)
  } else {
    nrow_varnat <- 0
  }
  return(ifelse(nrow_varnat>=2,1,0))
}

# Compute flag if has active or hidden active
gen_flag_if_curr_active <- function(all_id,application_id){
  
  all_df_local <- get_company_id_prev(db_name,all_df)
  all_id_here <- all_id[all_id$id!=application_id,]
  all_id_local_active <- all_id_here[all_id_here$status %in% c(4) & 
      all_id_here$company_id==all_df_local$company_id,]
  all_id_local_term <- all_id_here[all_id_here$status %in% c(5) & 
      all_id_here$company_id==all_df_local$company_id,]
  if(nrow(all_id_local_term)>0){
    all_id_local_term <- all_id_local_term[rev(order(
      all_id_local_term$deactivated_at)),]
    all_id_local_term$time_to_now <- round(difftime(
     as.Date(substring(Sys.time(),1,10)),
     as.Date(substring(all_id_local_term$deactivated_at,1,10)),units=c("days")),
     2)
  }
  
  return(cbind(ifelse(nrow(all_id_local_active)>0,1,0),
    ifelse(nrow(all_id_local_term[all_id_local_term$time_to_now<1,])>0,1,0)))
}

# Get time since last credit even for past applications
gen_all_days_since_credit <- function(db_name,all_credits,all_df){
  
  all_credits_here <- all_credits[order(all_credits$date),]
  
  # Get last terminated credit
  last_credit <- subset(all_credits_here,all_credits_here$status %in% c(4,5))
  last_credit$deactivated_at <- as.POSIXlt(last_credit$deactivated_at)
  last_credit <- last_credit[rev(order(last_credit$deactivated_at)),]
  last_credit <- last_credit[1,]
  
  # Get all applications which are not yet credits 
  all_credits_here <- subset(all_credits_here,
                             all_credits_here$status %in% c(1,2,3))
  
  if(nrow(all_credits_here)>0){
    
    for(i in 1:nrow(all_credits_here)){
      all_credits_here$difftime[i] <- 
        ifelse(!is.na(last_credit$deactivated_at[1]),
               
          # If last credit has been terminated (no active credit) : 
          # we check time difference between application and last deactivation
          floor(difftime(all_credits_here$date[i],last_credit$deactivated_at,
             units = c("days"))),
          
          # If last credit is still active : 
          # we check if application has been made in the last 7 days 
          ifelse(floor(difftime(Sys.time(),all_credits_here$date[i],
             units = c("days")))<=7,0,8)
          )
    }
    
    # We check when was the last credit terminated deactivated
    time_since <- ifelse(!is.na(last_credit$deactivated_at),
       floor(difftime(Sys.time(),last_credit$deactivated_at,units = c("days"))),
       0)
    
    # Filter those applications which are deemed quick after termination or 
    # during active credit 
    all_credits_here <- subset(all_credits_here,
                               all_credits_here$difftime %in% c(0:1))
    
    flag_app_quickly <- ifelse(nrow(all_credits_here)>0 & time_since<=7,1,0)
  } else {
    flag_app_quickly <- 0
  }
  
  return(flag_app_quickly)
}

# Get ratio of number of refinanced credits of past credits
gen_ratio_refinance_previous <- function(db_name,all_id){
  
  all_id_here_term <- subset(all_id,all_id$status %in% c(5))
  all_id_here_active <- subset(all_id,all_id$status %in% c(4))

  # Set correctly time of deactivation of previous credits 
  if(nrow(all_id_here_active)>0){
    all_id_here_active$deactivated_at <- as.Date(as.POSIXlt(Sys.time(),
      format = '%d%b%Y:%H:%M:%S'),origin = "1970-01-01")
  }
  if(nrow(all_id_here_term)>0){
    all_id_here_term$deactivated_at <- as.Date(all_id_here_term$deactivated_at,
      origin = "1970-01-01")
  }
  all_id_here <- rbind(all_id_here_term,all_id_here_active)
  all_id_here$created_at <- as.Date(all_id_here$created_at,
                                    origin = "1970-01-01")

  for(i in 1:nrow(all_id_here)){
    select <- subset(all_id_here,
        all_id_here$created_at<all_id_here$created_at[i])
    all_id_here$diff_time[i] <- round(suppressWarnings(
      max(difftime(select$deactivated_at,all_id_here$created_at[i],
      units = c("days")))))
  }
  
  all_id_here$days_diff_last_credit <- ifelse(all_id_here$diff_time>=-1,1,0)
  
  return(round(sum(all_id_here$days_diff_last_credit)/nrow(all_id_here),2))

}

# Compute average profit
gen_avg_profit <- function(db_name,all_id){
  
  all_id$amount_paid <- NA
  all_id$amount <- NA
  for(i in 1:nrow(all_id)){
    all_id$amount[i] <- gen_query(con,
     gen_last_cred_amount_query (all_id$id[i],db_name))$amount
    all_id$amount_paid[i] <- gen_query(con,
     gen_all_payments_with_ref_query(all_id$id[i],db_name))$amount_paid
  }
  all_id$profit <- ifelse(is.na(all_id$amount_paid),0,all_id$amount_paid)
  return(round(mean(all_id$profit),3))
}

# Compute profit on last id (for PTC)
gen_profit_id <- function(db_name,all_id,application_id){
  
  all_id$amount_paid <- NA
  all_id$amount <- NA
  all_id <- all_id[all_id$id==application_id,]
  all_id$amount <- gen_query(con,
    gen_last_cred_amount_query (all_id$id,db_name))$amount
  all_id$amount_paid <- gen_query(con,
    gen_all_payments_with_ref_query(all_id$id,db_name))$amount_paid

  return(all_id$amount_paid-all_id$amount)
}

# Compute ratio rejected applications
gen_ratio_rej <- function(db_name,all_credits){
  
  # Joint company ID to all_df
  all_credits <- merge(all_credits,
    gen_query(con,gen_get_company_id_query(db_name)),by.x = "product_id",
    by.y = "id",all.x = TRUE)
  
  # Return result
  return(round(nrow(subset(all_credits,all_credits$status==2))/
    nrow(all_credits),3))
  
}

# Function to get if previous credit is credirect or not 
gen_prev_other_brand <- function(db_name,all_id,all_df,application_id){
  
  # Joint company ID to all_df
  all_df$company_id <- gen_query(con,
     gen_products_query_desc(db_name,all_df))$company_id
  
  all_id <- all_id[all_id$id!=application_id,]
  all_id <- all_id[rev(order(all_id$signed_at)),]
  if(nrow(all_id)>0){
    all_df$prev_other_brand <- ifelse(all_id$company_id[1]!=all_df$company_id,
      1,0)
  } else {
    all_df$prev_other_brand <- NA
  }

  return(all_df$prev_other_brand)
}
