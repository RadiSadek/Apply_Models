
##########################################################################
## Apply correction to prior approval credits (terminated - refinanced) ##
##########################################################################

# Function to apply correction depending if terminated or refinance
gen_correction_po_fct <- function(con,db_name,all_df,all_id,
      scoring_df,products,period,application_id,scoring_decision){
  
  # Read offers of terminated or refinance
  po <- gen_query(con,
                  gen_po_terminated_query(db_name,all_df$client_id))
  company_id <- gen_query(con,
      gen_get_company_id_query(db_name))
  po <- merge(po,company_id,by.x = "product_id",by.y = "id",all.x = TRUE)
  all_df <- merge(all_df,company_id,by.x = "product_id",by.y = "id",
      all.x = TRUE)
  po <- subset(po,po$company_id==all_df$company_id)
  all_df <- all_df[,-which(names(all_df) %in% c("company_id"))]
  po <- po[,-which(names(po) %in% c("company_id"))]
  
  string_sql_update <- gen_prev_ids_ref_cor(con,db_name,all_df,all_id)[[2]]
  if(length(string_sql_update[!is.na(string_sql_update)])>0){
    po_ref <- gen_query(con,
                        gen_po_refinance_query(db_name,string_sql_update))
  } else {
    po_ref <- subset(as.data.frame(NA),!is.na(NA))
  }
  
  # Subset only recent
  po$final_time <- 
    ifelse(is.na(po$deleted_at),0,
    ifelse(!is.na(po$deleted_at) &
    substring(po$deleted_at,12,20)!="04:00:00",
    difftime(Sys.time(),po$deleted_at,units=c("days")),999))
  po_ref$final_time <- ifelse(is.na(po_ref$deleted_at),0,
      ifelse(substring(po_ref$deleted_at,12,20)!="04:00:00",
      difftime(Sys.time(),po_ref$deleted_at,units=c("days")),999))
  po_ref <- po_ref[order(po_ref$final_time),]
  po_ref <- po_ref[1,]
  
  po <- subset(po,po$final_time<=7)
  po_ref <- subset(po_ref,po_ref$final_time<=14)
  

  # Apply correction depending on whether there is a term or ref offer (or both)
  if(nrow(po_ref)>0 & nrow(po)>0){
    po_ref <- po_ref[rev(order(po_ref$deleted_at)),]
    po_ref <- po_ref[1,]
    po <- po[rev(order(po$deleted_at)),]
    po <- po[1,]
    if(po$credit_amount>po_ref$max_amount){
      scoring_df <- gen_correction_po(con,db_name,all_df,all_id,
          scoring_df,products,period,application_id)
    } else {
      scoring_df <- gen_correction_po_ref(con,db_name,all_df,all_id,
                                          scoring_df,products,period)
    }
  } else if(nrow(po)>0){
    scoring_df <- gen_correction_po(con,db_name,all_df,all_id,
                                    scoring_df,products,period,application_id)
  } else if(nrow(po_ref)>0){
    scoring_df <- gen_correction_po_ref(con,db_name,all_df,all_id,
                                        scoring_df,products,period)
  } else {
    scoring_df <- scoring_df
  }
  return(list(scoring_df,scoring_decision))
}

# Function to correct scoring table for clients with po terminated
gen_correction_po <- function(con,db_name,all_df,all_id,
                              scoring_df,products,period,application_id){

  # Read credits with already an offer for terminated prior approval
  po <- gen_query(con,
           gen_po_terminated_query(db_name,all_df$client_id))
  
  if(nrow(po)>=1){
    
    # Check if any credit after offer (of same company)
    company_id <- gen_query(con,
        gen_get_company_id_query(db_name))
    po <- merge(po,company_id,by.x = "product_id",by.y = "id",all.x = TRUE)
    all_df_local <- merge(all_df,company_id,by.x = "product_id",
        by.y = "id",all.x = TRUE)
    po <- subset(po,po$company_id==all_df_local$company_id)
    
    if(nrow(po)>=1){
      
      # Check if has credit after po offer
      po <- po[rev(order(po$deleted_at)),]
      po <- po[!duplicated(po$client_id),]
      last_po <- subset(po,!is.na(po$deleted_at))
      if(nrow(last_po)>0){
        last_po_date <- last_po$deleted_at
      } else {
        last_po_date <- NA
      }
      all_id_local <- subset(all_id,all_id$status %in% c(4,5))
      all_id_local$difftime <- round(as.numeric(difftime(
         all_id_local$created_at,last_po_date,units = c("days"))),2)
      all_id_local <- subset(all_id_local,
         all_id_local$company_id==po$company_id & 
         all_id_local$difftime>=0 & 
         all_id_local$id!=application_id)
      po$final_time <- 
        ifelse(is.na(po$deleted_at),0,
        ifelse(!is.na(po$deleted_at) &
        substring(po$deleted_at,12,20)!="04:00:00",
        difftime(Sys.time(),po$deleted_at,units=c("days")),999))
            
      # Correct scoring for terminated prior approval
      if(nrow(all_id_local)==0 & po$final_time<=4){
        
        # Arrange installment amount according to period
        period_po <- gen_query(con,
           gen_products_query_desc(db_name,po))$period
        if(period_po!=period){
          po$installment_amount <- gen_correct_max_installment_po(period_po,
            period,po$installment_amount)
        }
        
        # Correct score according to data in PO 
        scoring_df <- merge(scoring_df,
            products[,c("amount","period","installment_amount")],
            by.x = c("amount","period"),by.y = c("amount","period"),
            all.x = TRUE)
        scoring_df$color <- ifelse(
          scoring_df$amount<=po$credit_amount & 
            scoring_df$installment_amount<=po$installment_amount,3,
            scoring_df$color)
        if(all_id$company_id[all_id$id==application_id]!=2){
        scoring_df$color <- ifelse(scoring_df$amount>po$credit_amount & 
            scoring_df$score!="NULL",1,scoring_df$color)
        }
        scoring_df <-  scoring_df[,-which(names(scoring_df) %in% 
                                            c("installment_amount"))]}
    }
 }
  scoring_df <- scoring_df[order(scoring_df$period),]
  scoring_df <- scoring_df[order(scoring_df$amount),]
  return(scoring_df)
}

# Function to correct scoring table for clients with po refinances
gen_correction_po_ref <- function(con,db_name,all_df,all_id,
                              scoring_df,products,period){
  
  string_sql_update <- gen_prev_ids_ref_cor(con,db_name,all_df,all_id)[[2]]
  input <- gen_prev_ids_ref_cor(con,db_name,all_df,all_id)[[1]]
  
  # Append installment amount and period
  scoring_df <- merge(scoring_df,
     products[,c("amount","period","installment_amount")],
    by.x = c("amount","period"),by.y = c("amount","period"),
     all.x = TRUE)
  
  if(nrow(input)>0){
    
    # Read credits with already an offer for terminated prior approval
    po_ref <- gen_query(con,
                gen_po_refinance_query(db_name,string_sql_update))
    
    if(nrow(po_ref)>0){
      po_ref$final_time <- ifelse(is.na(po_ref$deleted_at),0,
        ifelse(substring(po_ref$deleted_at,12,20)!="04:00:00",
        difftime(Sys.time(),po_ref$deleted_at,units=c("days")),999))
      po_ref <- po_ref[order(po_ref$final_time),]
      po_ref <- po_ref[1,]
      
      if(po_ref$final_time<=7 & !is.na(po_ref$max_installment)){
        scoring_df$color <- ifelse(
          scoring_df$amount<=po_ref$max_amount & 
          scoring_df$installment_amount<=po_ref$max_installment,3,
          scoring_df$color)
        
        scoring_df$color <- ifelse(scoring_df$amount>po_ref$max_amount,1,
                                   scoring_df$color)
      }
      
      if(po_ref$final_time<=7 & is.na(po_ref$max_installment)){
        
        unique_amounts <- unique(scoring_df$amount[
          scoring_df$amount<=po_ref$max_amount])
        count_not_bad <- vector(mode = "double",length(unique_amounts))
        
        # Allow scoring dataframe accoring to PO offer
        for(i in 1:length(unique_amounts)){
          amount_df <- scoring_df[scoring_df$amount==unique_amounts[i],]
          count_not_bad[i] <- nrow(amount_df[amount_df$color>=2,])
        }
        correct_df <- as.data.frame(cbind(unique_amounts,count_not_bad))
        if(max(correct_df$count_not_bad)>0){
          max_amount_ok <- max(correct_df$unique_amounts[
          correct_df$count_not_bad>0])
          for(i in 1:nrow(scoring_df)){
            if(scoring_df$amount[i]<=po_ref$max_amount){
              subs <- scoring_df[scoring_df$amount==max_amount_ok,]
              scoring_df$color[i] <- subs$color[
                subs$period==scoring_df$period[i]]}
          }
        } else {
          for(i in 1:nrow(scoring_df)){
            if(scoring_df$amount[i]<=po_ref$max_amount){
              if(correct_df[correct_df$unique_amounts==scoring_df$amount[i],
              2]==0 & 
              scoring_df$period[i]==unique(scoring_df$period)
              [[ceiling(length(unique(scoring_df$period))/2)]]){
              scoring_df$color[i] <- 3}
              }}
        }
        
        # Restrict scoring dataframe to max amount and max installment amount
        scoring_df$color <- ifelse(scoring_df$amount>po_ref$max_amount,1,
            scoring_df$color)
        
        if(!is.na(po_ref$max_installment)){
          scoring_df$color <- ifelse(
            scoring_df$amount==po_ref$max_amount & 
            scoring_df$installment_amount>po_ref$max_installment &
            scoring_df$score!="NULL",1,scoring_df$color)}
        }
   }
  }
  return(scoring_df)
}

# Function to check if early paid previous credit 
gen_corection_early_repaid <- function(con,db_name,scoring_df,all_df,all_id,
   flag_credit_next_salary,flag_cashpoint){
  
  # Identify last credit of same company_id
  all_df <- get_company_id_prev(db_name,all_df)
  all_id_here <- all_id[all_id$company_id==all_df$company_id & 
                          all_id$status %in% c(4,5),]
  all_id_here <- all_id_here[rev(order(all_id_here$deactivated_at)),]
  all_id_here <- all_id_here[1,]
  
  # Get time since deactivation of last
  time_since <- difftime(Sys.time(),all_id_here$deactivated_at,
                         units = c("days"))
  time_since <- ifelse(is.na(all_id_here$deactivated_at),0,
    ifelse(as.Date(Sys.time())-2==substring(all_id_here$deactivated_at,1,10),1,
    ifelse(as.Date(Sys.time())-1==substring(all_id_here$deactivated_at,1,10),1,
    ifelse(as.Date(Sys.time())-0==substring(all_id_here$deactivated_at,1,10),1,
           0))))
  
  # Get total installments, passed installments of last credit
  tot_installments <- gen_query(con,
    gen_last_cred_amount_query(
    all_id_here$id,db_name))$installments
  passed_installments <- gen_query(con,
    gen_passed_install_before_query(
    db_name,all_id_here$id,Sys.time()))$passed_installments
  
  # Get period of last credit (monthly,weekly...)
  period <- gen_query(con,
    gen_products_query_desc(db_name,all_id_here))$period
  
  # Apply conditions
  if(flag_cashpoint==1){
    if(!is.na(time_since) & time_since==1 & !is.na(all_id_here$signed_at) & 
       all_id_here$signed_at>="2023-08-01 00:00:00"){
      po <- gen_query(con,gen_po_terminated_query(db_name,all_df$client_id))
      company_id <- gen_query(con,gen_get_company_id_query(db_name))
      po <- merge(po,company_id,by.x = "product_id",by.y = "id",all.x = TRUE)
      po <- subset(po,po$company_id==all_df$company_id)
      po$final_time <-difftime(Sys.Date(),as.Date(po$created_at),
          units=c("days"))
      po <- subset(po,po$final_time<=2)
      if(nrow(po)==0){
        scoring_df$color <- ifelse(!(scoring_df$score %in% c("NULL")),1,
                                   scoring_df$color)
      }
    }
  } else {
    if(!is.na(time_since) & time_since==1){
      if((flag_credit_next_salary==1 & passed_installments==0) |
         (flag_credit_next_salary!=1 & period==3 & 
          passed_installments<2 & tot_installments<4) |
         (flag_credit_next_salary!=1 & passed_installments<4 & 
          tot_installments>=4)){
        scoring_df$color <- ifelse(!(scoring_df$score %in% c("NULL")),1,
                                   scoring_df$color)
      }
    }
  }

  return(scoring_df)
}

