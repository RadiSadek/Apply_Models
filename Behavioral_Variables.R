
####################################################################
######## Functions to compute variables for repeat customers  ######
####################################################################

# Function to get company id of previous credits
get_company_id_prev <- function(db_name,all_credits){
  products_desc_all <- suppressWarnings(fetch(dbSendQuery(con, 
      gen_get_company_id_query(db_name)), n=-1))
  all_credits <- merge(all_credits, products_desc_all, 
       by.x = "product_id", by.y = "id", all.x = TRUE)
  return(all_credits)
}
# Function to get if previous credit is credirect or not 
gen_prev_online <- function(db_name,all_credits,all_df){
  all_credits_prev_online <- all_credits[order(all_credits$date),]
  all_credits_prev_online <- subset(all_credits_prev_online, 
       all_credits_prev_online$id<application_id)
  all_df$prev_online <- ifelse(all_credits_prev_online[
    rev(order(all_credits_prev_online$date)),][1,10]==2, 1, 0)
  return(all_df)
}
                            
# Function to get if last credit is eventually a hidden refinance
gen_ratio_last_amount_paid <- function(db_name,all_credits,all_df){

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
      cash_flow_active <- suppressWarnings(fetch(dbSendQuery(con, 
            gen_paid_amount_query(all_credits_actives$id,db_name)), n=-1))
      total_amount_active <- suppressWarnings(fetch(dbSendQuery(con, 
            gen_last_cred_amount_query(all_credits_actives$id,db_name)), n=-1))
      
      # if actives and same company as application
      all_df$flag_high_last_paid <- ifelse(sum(cash_flow_active$amount)/
            total_amount_active$final_credit_amount<0.5, 1, 0)
      
    } else if (nrow(all_credits_terminated)>=1){
      
      # if actives and diff company as application and terminated
      all_credits_terminated <- all_credits_terminated[rev(
        order(all_credits_terminated$date)),]
      all_credits_terminated <- all_credits_terminated[!duplicated(
        all_credits_terminated$client_id),]
      cash_flow_terminated <- suppressWarnings(fetch(dbSendQuery(con, 
             gen_last_paid_amount_query(all_credits_terminated$id, db_name)), 
                                                     n=-1))
      total_amount_terminated <- suppressWarnings(fetch(dbSendQuery(con, 
            gen_last_cred_amount_query(all_credits_terminated$id,db_name)), 
                                                        n=-1))
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
                          data_plan_main_select_def){
  
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
  return(suppressWarnings(fetch(dbSendQuery(con, 
     gen_last_paid_amount_query(var$id[nrow(var)-1],db_name)), n=-1)))
}

# Function to get total amount (with taxes) of previous credit
gen_last_total_amount <- function(all_id){
  var <- gen_variables_for_rep(all_id)
  return(total_amount <- suppressWarnings(fetch(dbSendQuery(con, 
     gen_last_cred_amount_query(var$id[nrow(var)-1],db_name)), n=-1)))
}

# Function to get amount of previous credit
gen_last_prev_amount <- function(all_id){
  var <- gen_variables_for_rep(all_id)
  return(prev_amount <- suppressWarnings(fetch(dbSendQuery(con, 
     gen_prev_amount_query(db_name,var)), n=-1)))
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
gen_select_relevant_ids_max_delay <- function(db_name,all_actives,
                                              all_id_max_delay){
  
  data_plan_main_actives_past <- suppressWarnings(fetch(dbSendQuery(con, 
      gen_plan_main_actives_past_query(db_name,all_actives)), n=-1))
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

