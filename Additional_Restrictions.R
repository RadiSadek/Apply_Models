
#############################################################
######## Apply restrictions on final scoring dataframe ######
#############################################################

# Function to apply restrictions for City Cash applications
gen_restrict_citycash_app <- function(scoring_df,products,all_df,all_id){

  scoring_df$color <- 
    ifelse(scoring_df$score %in% c("NULL"),scoring_df$color,
    ifelse(scoring_df$score %in% c("Good 3","Good 4") & 
           scoring_df$amount>1000,1,
    ifelse(scoring_df$score %in% c("Good 1","Good 2") & 
          scoring_df$amount>800,1,
           
    ifelse(scoring_df$score %in% c("Indeterminate") & scoring_df$amount>600 & 
           is.na(flag_bad_office(all_df$office_id)),1,
    ifelse(scoring_df$score %in% c("Indeterminate") & scoring_df$amount>600 & 
           flag_bad_office(all_df$office_id)!=1 & scoring_df$pd<0.4,1,
    ifelse(scoring_df$score %in% c("Indeterminate") & scoring_df$amount>400 & 
           flag_bad_office(all_df$office_id)!=1 & scoring_df$pd>=0.4,1,
    ifelse(scoring_df$score %in% c("Indeterminate") & scoring_df$amount>600 & 
           flag_bad_office(all_df$office_id)==1 & scoring_df$pd<0.34,1,
    ifelse(scoring_df$score %in% c("Indeterminate") & scoring_df$amount>400 & 
           flag_bad_office(all_df$office_id)==1 & scoring_df$pd>=0.34,1,
           
           scoring_df$color))))))))
    
                             
  # Check if installment ratio is OK
  if(!("installment_amount" %in% names(scoring_df))){
    scoring_df <- merge(scoring_df,products[,c("amount","period",
      "installment_amount")],
    by.x = c("amount","period"),by.y = c("amount","period"),all.x = TRUE)
  }
  
  # Restrict otkazani ot RO
  if(!is.na(all_df$office_id) & all_df$office_id==215){
     scoring_df$color <- ifelse(scoring_df$score %in% 
      c("Bad","Indeterminate","Good 1","Good 2","Good 3"),1, scoring_df$color)
  }

  return(scoring_df)
}

# Function to apply restrictions for City Cash applications
gen_restrict_cashpoint_app <- function(scoring_df,all_df,flag_beh,
                                       flag_parallel){
  
  scoring_df$color <- 
    ifelse(scoring_df$score %in% c("NULL"),scoring_df$color,
    ifelse(scoring_df$score %in% c("Good 4") & scoring_df$amount>1000,1,
    ifelse(scoring_df$score %in% c("Good 3") & scoring_df$amount>800,1,
    ifelse(scoring_df$score %in% c("Good 2") & scoring_df$amount>700,1,
    ifelse(scoring_df$score %in% c("Good 1") & scoring_df$amount>600,1,
    ifelse(scoring_df$score %in% c("Indeterminate") & scoring_df$amount>400,1,
           scoring_df$color))))))
              
  # No Indeterminates
  scoring_df$color <- ifelse(scoring_df$score %in% c("Bad","Indeterminate"),
    1, scoring_df$color)
  
  # Treat if has parallel and depending on DPD
  if(all_df$product_id!=103){
    scoring_df$color <- 
      ifelse(scoring_df$score %in% c("Bad","Indeterminate",
       "Good 1","Good 2","Good 3","Good 4") & flag_parallel[[1]]==1 & 
       !is.na(flag_parallel[[2]]) & flag_parallel[[2]]>=180,1, 
      ifelse(scoring_df$score %in% c("Bad","Indeterminate",
       "Good 1","Good 2") & flag_parallel[[1]]==1 & !is.na(flag_parallel[[2]]) & 
       flag_parallel[[2]]>=90,1,scoring_df$color))
  } 

  # No Gratis and not Good 4
  if(all_df$product_id %in% c(68,90)){
    scoring_df$color <- ifelse(scoring_df$score %in% c("Indeterminate",
      "Good 1","Good 2","Good 3"),1,scoring_df$color) 
  }

  return(scoring_df)
}

# Function to apply restrictions for FinMag applications 
gen_restrict_finmag_app <- function(scoring_df,all_df,all_id,flag_beh){
  
  # Set lower limit
  scoring_df$color <- 
     ifelse(scoring_df$score %in% c("NULL"),scoring_df$color,
     ifelse(scoring_df$amount>3000,1,scoring_df$color))
  
  # Accept only Good 4
  if(flag_beh==0){
    scoring_df$color <- 
      ifelse(is.na(scoring_df$pd),scoring_df$color,
             ifelse(scoring_df$pd>0.3,1,scoring_df$color))
  } else {
    scoring_df$color <- 
      ifelse(is.na(scoring_df$pd),scoring_df$color,
             ifelse(scoring_df$pd>0.1,1,scoring_df$color))
  }
  
  # Accept only with bezsro4en trudov dogovor
  if(is.na(all_df$status_work) | all_df$status_work!=2){
    scoring_df$color <- 
      ifelse(is.na(scoring_df$pd),scoring_df$color,1)
  }
  
  return(scoring_df)
}

# Function to apply restrictions for City Cash repeats
gen_restrict_citycash_beh <- function(scoring_df,prev_amount,products,all_id,
         all_df,db_name,application_id,crit,flag_cashpoint,flag_parallel){
  
  # Compute allowed installment if application
  if_new <- gen_restrict_citycash_app(scoring_df,products,all_df,all_id)
  if_new <- subset(if_new,if_new$score %in% c("Indeterminate",
     "Good 1","Good 2","Good 3","Good 4") & if_new$color!=1)
  if_new_amount <- suppressWarnings(max(if_new$amount))
  if_new_installment <- suppressWarnings(max(if_new$installment_amount))
  
  # Check if has Good 1 at least somewhere in table
  criteria <- length(names(table(scoring_df$score))
    [names(table(scoring_df$score)) %in% 
        c("Good 1","Good 2","Good 3","Good 4")])
  scoring_df$color <- ifelse(scoring_df$score %in% c("NULL"),scoring_df$color,
    ifelse(criteria==0 & scoring_df$amount>prev_amount$amount,1,
           scoring_df$color))
  
  # Limit based on maximum amount differential
  for(i in 1:nrow(all_id)){
    all_id$amount[i] <- gen_query(con,
        gen_big_sql_query(db_name,all_id$id[i]))$amount
  }
  max_prev_amount <- max(all_id$amount[ 
        all_id$company_id==all_id$company_id[all_id$id==application_id] & 
        all_id$status %in% c(4,5)])
  if(nrow(all_id[all_id$company_id==all_id$company_id[
    all_id$id==application_id] & all_id$status %in% c(4),])>0){
    max_prev_amount_spec <- max(all_id$amount[ 
      all_id$company_id==all_id$company_id[all_id$id==application_id] & 
        all_id$status %in% c(4)])
  } else {
    max_prev_amount_spec <- max(all_id$amount[ 
      all_id$company_id==all_id$company_id[all_id$id==application_id] & 
        all_id$status %in% c(5)])
  }
  
  # Check if installment ratio is OK
  if(!("installment_amount" %in% names(scoring_df))){
    scoring_df <- merge(scoring_df,products[,c("amount","period",
       "installment_amount")],
       by.x = c("amount","period"),by.y = c("amount","period"),all.x = TRUE)
  }
  allowed_installment <- gen_installment_ratio(db_name,all_id,all_df,
      application_id,crit,flag_cashpoint,max_prev_amount_spec,scoring_df)
  for(i in 1:nrow(scoring_df)){
    if(scoring_df$installment_amount[i]>allowed_installment){
      scoring_df$color[i] <- 1
    }
  }
  
  scoring_df$color <- ifelse(scoring_df$score %in% c("Good 4") & 
     scoring_df$amount>(max_prev_amount+600),1,
     ifelse(scoring_df$score %in% c("Good 1","Good 2","Good 3",
     "Indeterminate") & scoring_df$amount>(max_prev_amount+400),1,
     scoring_df$color))
  
  # Correct if sum is less than if client is new
  scoring_df$color <- ifelse(scoring_df$score %in% c("Good 4","Good 1",
     "Good 2","Good 3","Indeterminate") & scoring_df$amount<=if_new_amount & 
     scoring_df$installment_amount<=if_new_installment,3,scoring_df$color)

  # Correct if has cession in Credirect
  # if(gen_cession_credirect(all_id)==1){
  #   scoring_df$color <- ifelse(scoring_df$score %in% c("Bad","Indeterminate",
  #   "Good 1","Good 2"),1, scoring_df$color)
  # }
  
  # Restrict otkazani ot RO
  if(!is.na(all_df$office_id) & all_df$office_id==215){
    scoring_df$color <- ifelse(scoring_df$score %in% 
     c("Bad","Indeterminate","Good 1","Good 2"),1, scoring_df$color)
  }

  return(scoring_df)
}

# Function to apply restrictions for Credirect applications
gen_restrict_credirect_app <- function(scoring_df,all_df,
     flag_credit_next_salary,flag_new_credirect_old_city,fraud_flag,
     flag_risky_address){

  if(flag_credit_next_salary==1){
    scoring_df$color <- 
      ifelse(scoring_df$score %in% c("Good 4") & scoring_df$amount>1000,1,
      ifelse(scoring_df$score %in% c("Good 3","Good 2","Good 1",
                                     "Indeterminate") &
             scoring_df$amount>1000,1,
             scoring_df$color))
  } else {
    scoring_df$color <- 
      ifelse(scoring_df$score %in% c("Good 4") & scoring_df$amount>1400,1,
      ifelse(scoring_df$score %in% c("Good 3","Good 2","Good 1",
                                     "Indeterminate") &
             scoring_df$amount>1000,1,
             scoring_df$color))
  }
  if(all_df$age<21){
    scoring_df$color <- ifelse(scoring_df$amount>500 & scoring_df$color>=2,1,
        scoring_df$color)
  }
  
  # Apply filter for new Credirects but old City Cash
  if(flag_new_credirect_old_city==1){
    scoring_df$color <- ifelse(scoring_df$score %in% 
     c("Indeterminate","Good 1","Good 2"), 1, scoring_df$color)
  }
  
  # Apply filter if fraud flag is positive
  if(!is.na(fraud_flag) & fraud_flag==1){
    scoring_df$color <- ifelse(scoring_df$score %in% 
     c("Indeterminate"), 1, scoring_df$color)
  }
  
  # Apply filter is location is deemed risky
  if(!is.na(flag_risky_address$flag_risky_address) & 
     flag_risky_address$flag_risky_address==1){
    scoring_df$color <- ifelse(scoring_df$score %in% 
     c("Indeterminate","Good 1"), 1, scoring_df$color)
  }
  
  return(scoring_df)
}

# Function to apply restrictions for Cashpoint 
gen_restrict_cashpoint_beh <- function(scoring_df,all_df,all_id,application_id,
     prev_amount,flag_parallel,db_name){
  
  days_delay <- gen_query(con,
      gen_plan_main_select_query(db_name,max(all_id$id[all_id$company_id==5 & 
       all_id$status  %in% c(4,5)])))$max_delay
  days_delay <- ifelse(is.na(days_delay),0,days_delay)
  
  # Compute allowed step 
  scoring_df$prev_amount <- prev_amount$amount
  scoring_df$diff_amount <- scoring_df$amount - prev_amount$amount
  
  if(all_df$product_id!=103){
    if(days_delay<=30){
      scoring_df$color <- ifelse(scoring_df$diff_amount>400,1,scoring_df$color)
    } else if(days_delay<=59){
      scoring_df$color <- ifelse(scoring_df$diff_amount>200,1,scoring_df$color)
    } else if(days_delay<=90){
      scoring_df$color <- ifelse(scoring_df$diff_amount>0,1,scoring_df$color)
    } else if(days_delay<=180){
      scoring_df$color <- ifelse(scoring_df$amount>(0.6*scoring_df$prev_amount),
        1,scoring_df$color)
    } else{
      scoring_df$color <- ifelse(scoring_df$amount>400,1,scoring_df$color)
    }
  } else {
    nb_term <- subset(all_id,all_id$company_id==5 & all_id$product_id==103)
    if(nrow(nb_term)<=2){
      scoring_df$color <- ifelse(scoring_df$diff_amount>0,1,scoring_df$color)
    } else{
      scoring_df$color <- ifelse(scoring_df$diff_amount>100,1,scoring_df$color)
    }
  }
 
  # No Indeterminates
  scoring_df$color <- ifelse(scoring_df$score %in% c("Bad","Indeterminate"),
     1, scoring_df$color)  
  
  # Gratis only for Good 4
  if(all_df$product_id %in% c(68,90)){
    scoring_df$color <- ifelse(scoring_df$score %in% c("Indeterminate",
        "Good 1","Good 2","Good 3"),1,scoring_df$color) 
  }
  
  # Treat those with parallel credits
  if(all_df$product_id!=103){
    scoring_df$color <- 
      ifelse(scoring_df$score %in% c("Bad","Indeterminate",
       "Good 1","Good 2","Good 3","Good 4") & flag_parallel[[1]]==1 & 
       !is.na(flag_parallel[[2]]) & flag_parallel[[2]]>=180,1, 
      ifelse(scoring_df$score %in% c("Bad","Indeterminate","Good 1","Good 2") 
       & flag_parallel[[1]]==1 & !is.na(flag_parallel[[2]]) & 
       flag_parallel[[2]]>=90,1,scoring_df$color))
  }
  
  return(scoring_df)
}

# Function to apply restrictions for Credirect behavioral
gen_restrict_credirect_beh <- function(scoring_df,all_df,all_id,application_id,
       flag_credit_next_salary){

  # Get company ID to filter past credits only for Credirect and credit amounts
  all_df_local <- get_company_id_prev(db_name,all_df)
  all_id_local <- all_id[all_id$status %in% c(5) & 
                         all_id$company_id==all_df_local$company_id,]
  all_id_local_active <- all_id[all_id$status %in% c(4) & 
                           all_id$company_id==all_df_local$company_id,]
  all_id_local <- subset(all_id_local, all_id_local$sub_status %in% 
                         c(123,126,128))
  
  # Get amounts of previous credits
  if(nrow(all_id_local)>0){
    for(i in 1:nrow(all_id_local)){
      all_id_local$amount[i] <- gen_query(con,
       gen_last_cred_amount_query(all_id_local$id[i],db_name))$amount}
  }
  if(nrow(all_id_local_active)>0){
    for(i in 1:nrow(all_id_local_active)){
      all_id_local_active$amount[i] <- gen_query(con,
       gen_last_cred_amount_query(all_id_local_active$id[i],db_name))$amount}
  }
  
  # Get nb passed installments at deactivation & if prev is until next salary
  prev_vars <- gen_prev_deactiv_date(db_name,all_df,all_id,application_id)
  passed_install_at_pay <- prev_vars[1]
  prev_next_salary <- prev_vars[2]
  passed_install_at_pay <- ifelse(is.na(passed_install_at_pay),0,
    passed_install_at_pay)
  prev_next_salary <- ifelse(is.na(prev_next_salary),0,prev_next_salary)
  
  # Define maximum step with previous
  if(passed_install_at_pay==0){
    max_step_installments <- c(200,300,500,600,800)
  } else if(passed_install_at_pay==1){
    max_step_installments <- c(500,700,1000,1000,1300)
  } else if(passed_install_at_pay==2){
    max_step_installments <- c(600,900,1200,1200,1600)
  } else if(passed_install_at_pay==3){
    max_step_installments <- c(700,1000,1400,1400,1700)
  } else {
    max_step_installments <- c(700,1000,1400,1400,1700)
  }
  max_step_payday <- c(500,800,1100,1100,1200)
  if(prev_next_salary==0){
    max_step <- max_step_installments
  } else {
    max_step <- max_step_payday
  }
  
  # Apply policy rules for Credirect Installments
  if(flag_credit_next_salary==0){
    
    scoring_df$allowed_amount_app <- 
      ifelse(scoring_df$score %in% c("NULL","Bad","Indeterminate"),0,
             ifelse(scoring_df$score %in% c("Good 4"),1400,800))
    
    # If has at least 1 terminated 
    if(nrow(all_id_local)>0){

      scoring_df$allowed_amount_rep <- 
        ifelse(scoring_df$score %in% c("Bad","NULL"),
               max(all_id_local$amount) + 0,
        ifelse(scoring_df$score %in% c("Indeterminate"),
               max(all_id_local$amount) + max_step[1], 
        ifelse(scoring_df$score %in% c("Good 1"),
               max(all_id_local$amount) + max_step[2],
        ifelse(scoring_df$score %in% c("Good 2"),
               max(all_id_local$amount) + max_step[3],
        ifelse(scoring_df$score %in% c("Good 3"),
               max(all_id_local$amount) + max_step[4], 
               max(all_id_local$amount) + max_step[5])))))
      
      for (i in 1:nrow(scoring_df)){
        scoring_df$allowed_amount[i] <- max(scoring_df$allowed_amount_rep[i],
                scoring_df$allowed_amount_app[i])
      }
      scoring_df$color <- ifelse(scoring_df$amount>scoring_df$allowed_amount,
                1,scoring_df$color)
    } 
    
    # If only has at least 1 active (no terminated)
    else if(nrow(all_id_local_active)>0){
      for (i in 1:nrow(scoring_df)){
        scoring_df$allowed_amount[i] <- max(scoring_df$allowed_amount_app[i],
                  max(all_id_local_active$amount))}
    
    # Precaution condition
    } else {
      scoring_df$allowed_amount <- scoring_df$allowed_amount_app
    }
    
    scoring_df$color <- ifelse(scoring_df$amount>scoring_df$allowed_amount,
                               1,scoring_df$color)
    
  # Apply policy rules for Pay Day
  } else {
      scoring_df$allowed_amount_app <- 
        ifelse(scoring_df$score %in% c("NULL","Bad","Indeterminate"),0,
        ifelse(scoring_df$score %in% c("Good 4"),1000,1000))
      
      if(nrow(all_id_local)>0){
        scoring_df$allowed_amount_rep <- 
         ifelse(scoring_df$score %in% c("Indeterminate"),
             max(all_id_local$amount) + max_step[1], 
          ifelse(scoring_df$score %in% c("Good 1"),
             max(all_id_local$amount) + max_step[2],
          ifelse(scoring_df$score %in% c("Good 2"),
             max(all_id_local$amount) + max_step[3],
          ifelse(scoring_df$score %in% c("Good 3"),
             max(all_id_local$amount) + max_step[4], 
             max(all_id_local$amount) + max_step[5]))))
        
        for (i in 1:nrow(scoring_df)){
          scoring_df$allowed_amount[i] <- max(scoring_df$allowed_amount_rep[i],
                                              scoring_df$allowed_amount_app[i])}
      } else {
        
        scoring_df$allowed_amount <- scoring_df$allowed_amount_app
        
      }
      scoring_df$allowed_amount <- ifelse(scoring_df$allowed_amount>1000,1000,
                                          scoring_df$allowed_amount)
      scoring_df$color <- ifelse(scoring_df$amount>scoring_df$allowed_amount,
                                 1,scoring_df$color)
  }
  
  # Set hard max credit amounts
  scoring_df$color <- 
     ifelse(scoring_df$score %in% c("Indeterminate") & scoring_df$amount>2000,1,
     ifelse(scoring_df$score %in% c("Good 1") & scoring_df$amount>4000,1,
     ifelse(scoring_df$score %in% c("Good 2","Good 3","Good 4") & 
            scoring_df$amount>6000,1,scoring_df$color)))

  return(scoring_df)
}

# Funciton to apply restrictions for FinMag behavioral
gen_restrict_finmag_beh <- function(scoring_df,prev_amount,products,
       all_id,all_df,db_name,application_id){
  
  # Get company ID to filter past credits only for Credirect and credit amounts
  all_df_local <- get_company_id_prev(db_name,all_df)
  all_id_local <- all_id[all_id$status %in% c(5) & 
      all_id$company_id==all_df_local$company_id,]
  all_id_local_active <- all_id[all_id$status %in% c(4) & 
      all_id$company_id==all_df_local$company_id,]
  all_id_local <- subset(all_id_local, all_id_local$sub_status %in% 
                           c(123,126,128))
  
  # Get amounts of previous credits
  if(nrow(all_id_local)>0){
    for(i in 1:nrow(all_id_local)){
      all_id_local$amount[i] <- gen_query(con,
        gen_last_cred_amount_query(all_id_local$id[i],db_name))$amount}
  }
  if(nrow(all_id_local_active)>0){
    for(i in 1:nrow(all_id_local_active)){
      all_id_local_active$amount[i] <- gen_query(con,
        gen_last_cred_amount_query(all_id_local_active$id[i],db_name))$amount}
  }
  
  # Get nb passed installments at deactivation & if prev is until next salary
  prev_vars <- gen_prev_deactiv_date(db_name,all_df,all_id,application_id)
  passed_install_at_pay <- prev_vars[1]
  passed_install_at_pay <- ifelse(is.na(passed_install_at_pay),0,
        passed_install_at_pay)
  
  # Define maximum step with previous
  if(passed_install_at_pay==0){
    max_step <- c(100,200,300,400,500)
  } else if(passed_install_at_pay==1){
    max_step <- c(400,600,800,900,1000)
  } else if(passed_install_at_pay==2){
    max_step <- c(500,800,1100,1200,1400)
  } else if(passed_install_at_pay==3){
    max_step <- c(600,900,1200,1400,1700)
  } else {
    max_step <- c(700,1000,1300,1500,2000)
  }
  
  # Apply policy rules for Credirect Installment
  scoring_df$allowed_amount_app <- 3000
    
    # If has at least 1 terminated 
  if(nrow(all_id_local)>0){
      
      scoring_df$allowed_amount_rep <- 
        ifelse(scoring_df$score %in% c("Bad","NULL"),
               max(all_id_local$amount) + 0,
        ifelse(scoring_df$score %in% c("Indeterminate"),
               max(all_id_local$amount) + max_step[1], 
        ifelse(scoring_df$score %in% c("Good 1"),
               max(all_id_local$amount) + max_step[2],
        ifelse(scoring_df$score %in% c("Good 2"),
               max(all_id_local$amount) + max_step[3],
        ifelse(scoring_df$score %in% c("Good 3"),
               max(all_id_local$amount) + max_step[4], 
               max(all_id_local$amount) + max_step[5])))))
      
      for (i in 1:nrow(scoring_df)){
        scoring_df$allowed_amount[i] <- max(scoring_df$allowed_amount_rep[i],
             scoring_df$allowed_amount_app[i])
      }
      scoring_df$color <- ifelse(scoring_df$amount>scoring_df$allowed_amount,
                                 1,scoring_df$color)
    } 
    
  # If only has at least 1 active (no terminated)
  else if(nrow(all_id_local_active)>0){
      for (i in 1:nrow(scoring_df)){
        scoring_df$allowed_amount[i] <- max(scoring_df$allowed_amount_app[i],
                                            max(all_id_local_active$amount))}
      
  # Precaution condition
    } else {
      scoring_df$allowed_amount <- scoring_df$allowed_amount_app
  }
    
    scoring_df$color <- ifelse(scoring_df$amount>scoring_df$allowed_amount,
                               1,scoring_df$color)
    
  # Accept only with bezsro4en trudov dogovor
  if(is.na(all_df$status_work) | all_df$status_work!=2){
      scoring_df$color <- 
        ifelse(is.na(scoring_df$pd),scoring_df$color,1)
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

# Function to apply restrictions for Big Fin repeats
gen_restrict_big_fin_rep <- function(scoring_df,prev_amount){
  scoring_df$color <- 
    ifelse(scoring_df$score %in% c("NULL"),scoring_df$color,
    ifelse(scoring_df$score %in% c("Good 2","Good 3","Good 4"),
           scoring_df$color,1))
    
  criteria <- length(names(table(scoring_df$score))
    [names(table(scoring_df$score)) %in% 
    c("Good 3","Good 4")])
  scoring_df$color <- ifelse(scoring_df$score %in% c("NULL"),scoring_df$color,
    ifelse(criteria==0 & scoring_df$amount>prev_amount$amount,1,
    scoring_df$color))
  
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

# Function to apply restrictions to refinances
gen_restrict_beh_refinance <- function(db_name,all_df,all_id,
    scoring_df,flag_active,application_id,flag_credirect,flag_cashpoint){
  
  # Apply restrictions
  if(flag_credirect==1 & all_df$max_delay>180){
    scoring_df$color <- ifelse(scoring_df$color>1 & scoring_df$score!=
     "NULL",1,scoring_df$color)
  } else {
    scoring_df$color <- scoring_df$color
  }
    
  filter_company <- ifelse(flag_credirect==1,2,
                    ifelse(flag_cashpoint==1,5,1))
  all_id_local <- all_id[all_id$company_id==filter_company,]
  all_id_local_active <- subset(all_id_local,all_id_local$status==4)
  all_id_local <- all_id_local[all_id_local$id!=application_id,]
  
  # Apply restrictions if applicable 
  if(nrow(all_id_local)>0){
    
    # Check if client has current active refinance offer
    string_sql <- all_id_local$id[1]
    if(nrow(all_id_local)>1){
      for(i in 2:nrow(all_id_local)){
        string_sql <- paste(string_sql,all_id_local$id[i],sep=",")}
    }
    
    # Active PO refinance offers
    check_active_refs_office <- gen_query(con,
        gen_po_active_refinance_query(db_name,string_sql))
    
    # Terminated PO refinance offers
    check_term_refs_office <- gen_query(con,
        gen_po_refinance_query(db_name,string_sql))
    check_term_refs_office <- subset(check_term_refs_office,
        substring(check_term_refs_office$deleted_at,12,20)!="04:00:00")
    
    # Apply criteria if no relevant offer
    if(nrow(check_term_refs_office)>0){
      check_term_refs_office$difftime <- difftime(Sys.time(),
        check_term_refs_office$deleted_at,units = c("days"))
      check_term_refs_office <- subset(check_term_refs_office,
        check_term_refs_office$difftime<=3)
      check_term_refs_office <- check_term_refs_office[rev(
        order(check_term_refs_office$deleted_at)),]
      if(nrow(check_term_refs_office)>0){
        check_term_refs_office <- check_term_refs_office[1,]
        all_id_local_left <- subset(all_id_local,
          all_id_local$signed_at>=check_term_refs_office$deleted_at)
      } else {
        all_id_local_left <- as.data.frame(NA)
      }
    } else {
      all_id_local_left <- as.data.frame(NA)
    }
    
    # Application for refinance is rejected if no offer for refinance
    if(nrow(check_active_refs_office)==0 & nrow(check_term_refs_office)==0){
      scoring_df$color <- ifelse(scoring_df$color>1 & scoring_df$score!=
        "NULL",1,scoring_df$color)
    }
    
    # Application for refinance is rejected if offer but credit after offer
    if(nrow(check_active_refs_office)==0 & 
       nrow(check_term_refs_office)>0 & nrow(all_id_local_left)>0){
       scoring_df$color <- ifelse(scoring_df$color>1 & scoring_df$score!=
             "NULL",1,scoring_df$color)
    }
    
     # If Credirect : Reject if product is not CreDirect Потребителски - Рефинанс
     if(!(all_df$product_id %in% c(48,95,77)) & flag_credirect==1){
       scoring_df$color <- ifelse(scoring_df$color>1 & scoring_df$score!=
                                    "NULL",1,scoring_df$color)
    }
    
  }
  
  # Application for refinance is rejected if dpd >300 days
  if(nrow(all_id_local_active)>0){
    string_sql <- all_id_local_active$id[1]
    if(nrow(all_id_local_active)>1){
      for(i in 2:nrow(all_id_local_active)){
        string_sql <- paste(string_sql,all_id_local_active$id[i],sep=",")}
    }
    max_dpd <- max(gen_query(con,
        gen_plan_main_select_query(db_name,string_sql))$max_delay)
    if(max_dpd>=300){
      scoring_df$color <- ifelse(scoring_df$color>1 & scoring_df$score!=
                                   "NULL",1,scoring_df$color)
    }
  }

  return(scoring_df)
}

