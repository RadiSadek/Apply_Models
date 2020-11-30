
#############################################################
######## Apply restrictions on final scoring dataframe ######
#############################################################

# Function to apply restrictions for City Cash applications
gen_restrict_citycash_app <- function(scoring_df){
  
  score_df_800 <- subset(scoring_df,scoring_df$amount>800)
  criteria_800 <- length(names(table(score_df_800$score))
     [names(table(score_df_800$score)) %in% c("Good 4")])
  score_df_600 <- subset(scoring_df,scoring_df$amount>600)
  criteria_600 <- length(names(table(score_df_600$score))
    [names(table(score_df_600$score)) %in% c("Good 2","Good 3","Good 4")])
  score_df_400 <- subset(scoring_df,scoring_df$amount>400)
  criteria_400 <- length(names(table(score_df_400$score))
    [names(table(score_df_400$score)) %in% c("Good 1","Good 2",
    "Good 3","Good 4")])
  
  scoring_df$color <- ifelse(scoring_df$score %in% c("NULL"),scoring_df$color,
    ifelse(scoring_df$amount>1000,1,
    ifelse(criteria_800==0 & scoring_df$amount>800,1,
    ifelse(criteria_600==0 & scoring_df$amount>600,1,
    ifelse(criteria_400==0 & scoring_df$amount>400,1,scoring_df$color)))))

  return(scoring_df)
}

# Function to apply restrictions for City Cash repeats
gen_restrict_citycash_beh <- function(scoring_df,prev_amount){
  
  criteria <- length(names(table(scoring_df$score))
    [names(table(scoring_df$score)) %in% 
        c("Good 1","Good 2","Good 3","Good 4")])
  scoring_df$color <- ifelse(scoring_df$score %in% c("NULL"),scoring_df$color,
    ifelse(criteria==0 & scoring_df$amount>prev_amount$amount,1,
           scoring_df$color))
  
  return(scoring_df)
}

# Function to apply restrictions for Credirect applications
gen_restrict_credirect_app <- function(scoring_df,all_df,
     flag_credit_next_salary,flag_new_credirect_old_city){

  if(flag_credit_next_salary==1){
    scoring_df$color <- 
      ifelse(scoring_df$score %in% c("Good 4") & scoring_df$amount>800,1,
      ifelse(scoring_df$score %in% c("Good 3","Good 2","Good 1",
                                     "Indeterminate") &
             scoring_df$amount>600,1,
             scoring_df$color))
  } else {
    scoring_df$color <- 
      ifelse(scoring_df$score %in% c("Good 4") & scoring_df$amount>1000,1,
      ifelse(scoring_df$score %in% c("Good 3","Good 2","Good 1",
                                     "Indeterminate") &
             scoring_df$amount>600,1,
             scoring_df$color))
  }
  if(all_df$age<21){
    scoring_df$color <- ifelse(scoring_df$amount>300 & scoring_df$color>=2,1,
        scoring_df$color)
  }
  
  # Apply filter for new Credirects but old City Cash
  if(flag_new_credirect_old_city==1){
    scoring_df$color <- ifelse(scoring_df$score %in% 
     c("Indeterminate","Good 1","Good 2"), 1, scoring_df$color)
  } 
  return(scoring_df)
}

# Function to apply restrictions for Credirect behavioral
gen_restrict_credirect_beh <- function(scoring_df,all_df,all_id,
       flag_credit_next_salary){

  # Get company ID to filter past credits only for Credirect and credit amounts
  all_df_local <- get_company_id_prev(db_name,all_df)
  all_id_local <- all_id[all_id$status %in% c(4,5) & 
                         all_id$company_id==all_df_local$company_id,]
  all_id_local <- subset(all_id_local, is.na(all_id_local$sub_status) | 
      all_id_local$sub_status %in% c(123,126,128))
  
  # Get nb passed installments at deactivation
  if(nrow(all_id_local)>0){
    for(i in 1:nrow(all_id_local)){
      all_id_local$amount[i] <- fetch(dbSendQuery(con,
       gen_last_cred_amount_query(all_id_local$id[i],db_name)), n=-1)$amount
    }
  }
  
  ratio_passed_installments_prev <- ifelse(nrow(all_id_local)==0,-999,
     ifelse(is.na(gen_prev_deactiv_date(db_name,all_df_local,all_id_local)),999,
        gen_prev_deactiv_date(db_name,all_df_local,all_id_local)))

  # Apply policy rules for Credirect Installments
  if(flag_credit_next_salary==0){
    
       scoring_df$allowed_amount_app <- 
         ifelse(scoring_df$score %in% c("NULL","Bad","Indeterminate"),0,
         ifelse(scoring_df$score %in% c("Good 4"),1000,600))
    
       if(ratio_passed_installments_prev>0.5){
         scoring_df$allowed_amount_rep <- 
           ifelse(scoring_df$score %in% c("Bad","Indeterminate","Good 1","NULL"),
                  max(all_id_local$amount) + 0,
           ifelse(scoring_df$score %in% c("Good 2"),
                  max(all_id_local$amount) + 200,
           ifelse(scoring_df$score %in% c("Good 3"),
                  max(all_id_local$amount) + 400, 
                  max(all_id_local$amount) + 1000)))
         for (i in 1:nrow(scoring_df)){
           scoring_df$allowed_amount[i] <- max(scoring_df$allowed_amount_rep[i],
                                               scoring_df$allowed_amount_app[i])
         }
         scoring_df$color <- ifelse(scoring_df$amount>scoring_df$allowed_amount,
                1,scoring_df$color)
       } else if(ratio_passed_installments_prev!=-999) {
         for (i in 1:nrow(scoring_df)){
           scoring_df$allowed_amount[i] <- max(scoring_df$allowed_amount_app[i],
             max(all_id_local$amount))}
       } else {
         scoring_df$allowed_amount <- scoring_df$allowed_amount_app
      }
      scoring_df$color <- ifelse(scoring_df$amount>scoring_df$allowed_amount,
        1,scoring_df$color)
    
  # Apply policy rules for Pay Day
  } else {
      scoring_df$allowed_amount_app <- 
        ifelse(scoring_df$score %in% c("NULL","Bad","Indeterminate"),0,
        ifelse(scoring_df$score %in% c("Good 4"),800,600))
      
      if(ratio_passed_installments_prev!=-999){
        scoring_df$allowed_amount_rep <- 
          ifelse(scoring_df$score %in% c("Bad","Indeterminate","Good 1","NULL"),
             max(all_id_local$amount) + 0,
          ifelse(scoring_df$score %in% c("Good 2"),
             max(all_id_local$amount) + 200,
          ifelse(scoring_df$score %in% c("Good 3"),
             max(all_id_local$amount) + 400, 
             max(all_id_local$amount) + 1000)))
        
        for (i in 1:nrow(scoring_df)){
          scoring_df$allowed_amount[i] <- max(scoring_df$allowed_amount_rep[i],
                                              scoring_df$allowed_amount_app[i])}
      } else {
        
        scoring_df$allowed_amount <- scoring_df$allowed_amount_app
        
      }
      scoring_df$allowed_amount <- ifelse(scoring_df$allowed_amount>800,800,
                                          scoring_df$allowed_amount)
      scoring_df$color <- ifelse(scoring_df$amount>scoring_df$allowed_amount,
                                 1,scoring_df$color)
  }

  return(scoring_df)
}

# Function to apply restrictions for Big Fin applications
gen_restrict_big_fin_app <- function(scoring_df){
  scoring_df$color <- 
    ifelse(scoring_df$score %in% c("NULL"),scoring_df$color,
    ifelse(scoring_df$score %in% c("Good 3","Good 4"),scoring_df$color,1))
  scoring_df <- subset(scoring_df,scoring_df$amount<=3000)
  return(scoring_df)
}

# Readjust score if necessary for certain cases
gen_adjust_score <- function(scoring_df,crit){
  for(i in 1:nrow(scoring_df)){
    if(scoring_df$score[i] %in% crit){
      scoring_df$color[i] <- 1} 
  }
  return(scoring_df)
}

# Function to apply restrictions to Credirect Installments - Refinance
gen_restrict_beh_refinance <- function(db_name,all_df,all_id,
    scoring_df,flag_active,application_id,flag_credirect){
  
  all_df_local <- get_company_id_prev(db_name,all_df)
  all_id_local <- all_id[all_id$status %in% c(4,5) & 
                         all_id$company_id==all_df_local$company_id,]
  all_id_local <- all_id_local[all_id_local$id!=application_id,]
  all_id_local <- all_id_local[rev(order(all_id_local$deactivated_at)),]
  all_id_local_raw <- all_id_local
  all_id_local <- all_id_local[1,]
  
  # Compute passed paid installments and total terminated credits
  installments <- suppressWarnings(fetch(
    dbSendQuery(con, gen_last_cred_amount_query(all_id_local$id[1],db_name)),
    n=-1))$installments
  
  passed_paid_installments <- ifelse(flag_active[1,1]==1,
    suppressWarnings(fetch(dbSendQuery(con, 
    gen_passed_paid_install_before_query(db_name,all_id_local$id[1],
    Sys.time())), n=-1))$passed_installments / installments, 
    suppressWarnings(fetch(dbSendQuery(con, 
    gen_passed_paid_install_before_query(db_name,all_id_local$id[1],
    as.Date(substring(Sys.time()-3*3600*24,1,10)))), 
    n=-1))$passed_installments / installments)

  total_terminated <- nrow(subset(all_id_local_raw,all_id_local_raw$status==5))

  # Apply for City Cash
  if(flag_credirect==0){
    for(i in 1:nrow(scoring_df)){
      if(total_terminated==0 & passed_paid_installments<0.5){
        scoring_df$color <- 1
      } else if(all_df$max_delay>180){
        scoring_df$color <- 1
      } else if(passed_paid_installments>=0.5){
        scoring_df$color <- 
          ifelse(scoring_df$score %in% c("NULL"),scoring_df$color,
          ifelse(scoring_df$score %in% c("Bad","Indeterminate"),1,
                 scoring_df$color))
      } else if(total_terminated>0 & passed_paid_installments<0.5 & 
                passed_paid_installments>=0.45){
        scoring_df$color <- 
          ifelse(scoring_df$score %in% c("NULL"),scoring_df$color,
          ifelse(scoring_df$score %in% c("Bad","Indeterminate","Good 1"),1,
                scoring_df$color))
      } else if(total_terminated>0 & passed_paid_installments<0.45 & 
                passed_paid_installments>=0.4){
        scoring_df$color <- 
          ifelse(scoring_df$score %in% c("NULL"),scoring_df$color,
          ifelse(scoring_df$score %in% c("Bad","Indeterminate","Good 1",
                                         "Good 2"),1,
                        scoring_df$color))
      } else if(total_terminated>0 & passed_paid_installments<0.4 & 
                passed_paid_installments>=0.3){
        scoring_df$color <- 
          ifelse(scoring_df$score %in% c("NULL"),scoring_df$color,
                 ifelse(scoring_df$score %in% c("Bad","Indeterminate","Good 1",
                                                "Good 2","Good 3"),1,
                        scoring_df$color))
      } else if(total_terminated>0 & passed_paid_installments<0.3){
        scoring_df$color <- 
          ifelse(scoring_df$score %in% c("NULL"),scoring_df$color,
                 ifelse(scoring_df$score %in% c("Bad","Indeterminate","Good 1",
                                                "Good 2","Good 3","Good 4"),1,
                       scoring_df$color))
      } else {
        scoring_df$color <- scoring_df$color
      }
    }
  } else {
    if(all_df$max_delay>180){
      scoring_df$color <- 1
    } else {
      scoring_df$color <- scoring_df$color
    }
  }
  
  return(scoring_df)
}

# Function to apply restrictions to Credirect Installments - Refinance
gen_restrict_credirect_refinance <- function(db_name,all_id,scoring_df,
    application_id){
  
  result <- scoring_df
  all_id_local <- all_id[all_id$company_id==2 & all_id$status==4,]
  all_id_local <- all_id_local[all_id_local$id!=application_id,]
  
  # Apply restrictions if applicable 
  if(nrow(all_id_local)>0){
    
    # Check if client has current active refinance offer
    string_sql <- all_id_local$id[1]
    if(nrow(all_id_local)>1){
      for(i in 2:nrow(all_id_local)){
        string_sql <- paste(string_sql,all_id_local$id[i],sep=",")}
    }
    check_active_refs_office <- suppressWarnings(fetch(dbSendQuery(con,
      gen_po_active_refinance_query(db_name,string_sql)), n=-1))
    
    if(nrow(check_active_refs_office)==0){
      all_id_local <- all_id_local[rev(order(all_id_local$signed_at)),]
      all_id_local <- all_id_local[1,]
      
      # Read total amount, tax amount and discount amount of previous
      prev_amount_local <- suppressWarnings(fetch(dbSendQuery(con,
        gen_total_amount_curr_query(db_name,all_id_local$id)), 
        n=-1))$final_credit_amount
      prev_paid_amount_local <- sum(suppressWarnings(fetch(dbSendQuery(con,
        gen_paid_amount_query(all_id_local$id,db_name)), n=-1))$amount)
      discount_amount <- suppressWarnings(fetch(dbSendQuery(con,
        gen_discount_amount(db_name,all_id_local$id)), n=-1))$discount_amount
      tax_amount <- suppressWarnings(fetch(dbSendQuery(con,
        gen_taxes_amount(db_name,all_id_local$id)), n=-1))$tax_amount
      
      # Correct total amount, tax amount and discount amount of previous
      discount_amount <- ifelse(length(discount_amount)==0,0,
        ifelse(is.na(discount_amount),0,discount_amount))
      tax_amount <- ifelse(length(tax_amount)==0,0,
        ifelse(is.na(tax_amount),0,tax_amount))
      prev_paid_amount_local <- ifelse(length(prev_paid_amount_local)==0,0,
        ifelse(is.na(prev_paid_amount_local),0,prev_paid_amount_local))
      
      # Check if left to pay is lower than the requested amount
      result$left_to_pay <- prev_amount_local - prev_paid_amount_local - 
        discount_amount +  tax_amount 
      result$color <- ifelse(result$color>1 & result$amount<result$left_to_pay,
                             1,result$color)
      result <- result[ , -which(names(result) %in% c("left_to_pay"))]
    }
  }
  return(result)
}


