
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

# Function to apply restrictions for City Cash applications
gen_restrict_cashpoint_app <- function(scoring_df){
  
  score_df_800 <- subset(scoring_df,scoring_df$amount>800)
  criteria_800 <- length(names(table(score_df_800$score))
     [names(table(score_df_800$score)) %in% c("Good 4")])
  
  score_df_600 <- subset(scoring_df,scoring_df$amount>600)
  criteria_600 <- length(names(table(score_df_600$score))
     [names(table(score_df_600$score)) %in% c("Good 3","Good 4")])
  
  score_df_400 <- subset(scoring_df,scoring_df$amount>400)
  criteria_400 <- length(names(table(score_df_400$score))
     [names(table(score_df_400$score)) %in% c("Good 2",
     "Good 3","Good 4")])
  
  score_df_0 <- subset(scoring_df,scoring_df$amount>0)
  criteria_0 <- length(names(table(score_df_0$score))
     [names(table(score_df_0$score)) %in% c("Good 1","Good 2",
     "Good 3","Good 4")])
  
  scoring_df$color <- ifelse(scoring_df$score %in% c("NULL"),scoring_df$color,
    ifelse(scoring_df$amount>1000,1,
    ifelse(criteria_800==0 & scoring_df$amount>800,1,
    ifelse(criteria_600==0 & scoring_df$amount>600,1,
    ifelse(criteria_400==0 & scoring_df$amount>400,1,
    ifelse(criteria_0==0 & scoring_df$amount>0,1,scoring_df$color))))))

  return(scoring_df)
}


# Function to apply restrictions for City Cash repeats
gen_restrict_citycash_beh <- function(scoring_df,prev_amount,products,all_id,
                                      all_df,db_name){
  
  # Check if has Good 1 at least somewhere in table
  criteria <- length(names(table(scoring_df$score))
    [names(table(scoring_df$score)) %in% 
        c("Good 1","Good 2","Good 3","Good 4")])
  scoring_df$color <- ifelse(scoring_df$score %in% c("NULL"),scoring_df$color,
    ifelse(criteria==0 & scoring_df$amount>prev_amount$amount,1,
           scoring_df$color))
  
  # Check if installment ratio is OK
  if(!("installment_amount" %in% names(scoring_df))){
    scoring_df <- merge(scoring_df,products[,c("amount","period",
       "installment_amount")],
       by.x = c("amount","period"),by.y = c("amount","period"),all.x = TRUE)
  }
  allowed_installment <- gen_installment_ratio(db_name,all_id,all_df)
  for(i in 1:nrow(scoring_df)){
    if(scoring_df$installment_amount[i]>allowed_installment){
      scoring_df$color[i] <- 1
    }
  }
  
  
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
  # all_id_local <- subset(all_id_local,all_id_local$id!=application_id)
  # all_id_local_active <- subset(all_id_local_active,
  #                               all_id_local_active$id!=application_id)
  
  # Get amounts of previous credits
  if(nrow(all_id_local)>0){
    for(i in 1:nrow(all_id_local)){
      all_id_local$amount[i] <- fetch(dbSendQuery(con,
       gen_last_cred_amount_query(all_id_local$id[i],db_name)), n=-1)$amount}
  }
  if(nrow(all_id_local_active)>0){
    for(i in 1:nrow(all_id_local_active)){
      all_id_local_active$amount[i] <- fetch(dbSendQuery(con,
       gen_last_cred_amount_query(all_id_local_active$id[i],db_name)), 
       n=-1)$amount}
  }
  
  # Get nb passed installments at deactivation
  max_step_prev <- gen_prev_deactiv_date(db_name,all_df,all_id,
                                         application_id)

  # Apply policy rules for Credirect Installments
  if(flag_credit_next_salary==0){
    
    scoring_df$allowed_amount_app <- 
      ifelse(scoring_df$score %in% c("NULL","Bad","Indeterminate"),0,
             ifelse(scoring_df$score %in% c("Good 4"),1000,600))
    
    # If has at least 1 terminated 
    if(nrow(all_id_local)>0){

      scoring_df$allowed_amount_rep <- 
        ifelse(scoring_df$score %in% c("Bad","Indeterminate","Good 1","NULL"),
               max(all_id_local$amount) + min(0,max_step_prev),
        ifelse(scoring_df$score %in% c("Good 2"),
               max(all_id_local$amount) + min(200,max_step_prev),
        ifelse(scoring_df$score %in% c("Good 3"),
               max(all_id_local$amount) + min(400,max_step_prev), 
               max(all_id_local$amount) + min(1000,max_step_prev))))
      
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
        ifelse(scoring_df$score %in% c("Good 4"),800,600))
      
      if(nrow(all_id_local)>0){
        scoring_df$allowed_amount_rep <- 
          ifelse(scoring_df$score %in% c("Bad","Indeterminate","Good 1","NULL"),
             max(all_id_local$amount) + min(0,max_step_prev),
          ifelse(scoring_df$score %in% c("Good 2"),
             max(all_id_local$amount) + min(200,max_step_prev),
          ifelse(scoring_df$score %in% c("Good 3"),
             max(all_id_local$amount) + min(400,max_step_prev), 
             max(all_id_local$amount) + min(1000,max_step_prev))))
        
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
    scoring_df,flag_active,application_id,flag_credirect){
  
  # Apply restrictions
  if(flag_credirect==1 & all_df$max_delay>180){
    scoring_df$color <- ifelse(scoring_df$color>1 & scoring_df$score!=
     "NULL",1,scoring_df$color)
  } else {
    scoring_df$color <- scoring_df$color
  }
    
  filter_company <- ifelse(flag_credirect==1,2,1)
  all_id_local <- all_id[all_id$company_id==filter_company,]
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
    check_active_refs_office <- suppressWarnings(fetch(dbSendQuery(con,
        gen_po_active_refinance_query(db_name,string_sql)), n=-1))
    
    # Terminated PO refinance offers
    check_term_refs_office <- suppressWarnings(fetch(dbSendQuery(con,
        gen_po_refinance_query(db_name,string_sql)), n=-1))
    check_term_refs_office <- subset(check_term_refs_office,
        substring(check_term_refs_office$deleted_at,12,20)!="04:00:00")
    
    # Apply criteria if no relevant offer
    if(nrow(check_term_refs_office)>0){
      check_term_refs_office$difftime <- difftime(Sys.time(),
        check_term_refs_office$deleted_at,units = c("days"))
      check_term_refs_office <- subset(check_term_refs_office,
        check_term_refs_office$difftime<=120)
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
    
    if(nrow(check_active_refs_office)==0 & nrow(check_term_refs_office)==0){
      scoring_df$color <- ifelse(scoring_df$color>1 & scoring_df$score!=
        "NULL",1,scoring_df$color)
    }
    if(nrow(check_active_refs_office)==0 & 
       nrow(check_term_refs_office)>0 & nrow(all_id_local_left)>0){
       scoring_df$color <- ifelse(scoring_df$color>1 & scoring_df$score!=
             "NULL",1,scoring_df$color)
    }
    
  }
  return(scoring_df)
}

