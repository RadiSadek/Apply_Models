
###################################################
######## Define some extra useful functions  ######
###################################################

# Define function to generate query
gen_query <- function(con,input){
  return(suppressWarnings(dbFetch(dbSendQuery(con,input))))
}

# Define function to get apply cutoffs
gen_group_scores <- function(var,office,flag_beh,flag_credirect,
                             flag_credit_next_salary){
  if(flag_credirect==0 & flag_beh==0){
    if(flag_bad_office(office)==1){
      cutoffs <- cu_app_city_bad_offices
    } else {
      cutoffs <- cu_app_city_norm_offices
    }}
  else if (flag_credirect==1 & flag_beh==0 & flag_credit_next_salary==1){
    cutoffs <- cu_app_cred_flex}
  else if (flag_credirect==1 & flag_beh==0 & flag_credit_next_salary==0){
    cutoffs <- cu_app_cred_user}
  else if (flag_credirect==0 & flag_beh==1){
    cutoffs <- cu_beh_city}
  else {cutoffs <- cu_beh_cred}
  if (var>cutoffs[1]){output="Bad"} 
  else if (var>cutoffs[2]) {output="Indeterminate"} 
  else if (var>cutoffs[3]) {output="Good 1"} 
  else if (var>cutoffs[4]) {output="Good 2"} 
  else if (var>cutoffs[5]) {output="Good 3"} 
  else {output="Good 4"}
  return (output)
}

# Define function to aggregate incomes or expenses
gen_aggregate_income_exp <- function(input){
  names(input)[2] <- "amount"
  if(nrow(input)>0){
    output <- aggregate(input$amount, 
          by=list(input$application_id), FUN=sum)
    names(output) <- c("application_id","amount")
    output <- output$amount
  } else {
    output <- 0}
  
  return(output)
}

# Define function to prepare final data frame to aggregate scoring
gen_final_df <- function(products,application_id){
  
  suppressMessages(suppressWarnings(require("reshape")))
  # Read table with number of payments/number of for city cash
  table_citycash <- table(products$period, products$amount)
  for (i in 1:nrow(table_citycash)){
    for (j in 1:ncol(table_citycash)){
      if (table_citycash[i,j]>0){
        table_citycash[i,j] <- row.names(table_citycash)[i]
      } else {
        table_citycash[i,j] <- NA}}
  }
  
  # Make dataframe of all possible amounts/installments
  vect_citycash_installment <- sort(as.numeric(unique(unlist(table_citycash))))
  vect_citycash_amount <- colnames(table_citycash, do.NULL = TRUE, 
                                   prefix = "col")
  PD_citycash <- matrix("", ncol = length(vect_citycash_installment), 
                        nrow = length(vect_citycash_amount))
  colnames(PD_citycash) <- vect_citycash_installment
  rownames(PD_citycash) <- vect_citycash_amount
  melted <- as.data.frame(melt(t(PD_citycash)))
  names(melted) <- c("period","amount","value")
  melted$value <- as.numeric(melted$value)
  
  # Remove unneccessary rows from melted dataframe
  for(i in 1:nrow(melted)){
    c1 <- as.character(melted$period[i]) 
    c2 <- as.character(melted$amount[i])
    melted$value[i] <- ifelse(is.na(table_citycash[c1,c2]),0,1)
  }
  scoring_df <- subset(melted, melted$value==1)[,1:2]
  names(scoring_df) <- c("period","amount")
  scoring_df$application_id <- application_id
  return(scoring_df)
}

# Gen flag bad office
flag_bad_office <- function(var_off){
  return(ifelse(
    var_off %in% c("121","100","99","51","94","50","32","125","160","34","142",
                   "76","57","133","42","74","136","54","33","149","161","110",
                   "78","28","140","95","139","16","23","36","79","18","163",
                   "58","98","83","130"
    ), 1,
    ifelse(
      var_off %in% c("93","52","27","85","106","55","137","53","80","159","132",
                     "96","120","12","92","73","88","15","11","3","64","165",
                     "56","75","47","124","108","29","46","21","71","167",
                     "107","7","91","5","135","113"
    ), 2,
    ifelse(
      var_off %in% c("25","13","81","128","35","17","2","30","87","162","69",
                     "86","104","1","24","70","59","68","49","97","72","118",
                     "8","168","31","41","84","43","14","134","114","61","9",
                     "4","90","164","10","172","44"
    ), 3, 2
      ))))
}

# Gen flag bad office
flag_real_office <- function(var_off){
  return(ifelse(
    var_off %in% c("6","19","20","37","38","39","40","45","60","62","65","66",
                   "67","89","101","123","126","138","141","143"
       ), 0, 1))
  
}

# Define cutoffs application for credirect fraud
gen_group_scores_fraud <- function(var){
  results <- ifelse(var>cu_app_cred_frauds,1,0)
}

# Define sql string query for writing in DB for PO terminated
gen_sql_string_po_terminated <- function(input,inc){
  return(paste("(",input$id[inc],",",
    input$office_id[inc],",",input$client_id[inc],",",
    input$group[inc],",",input$product_id[inc],",",
    input$application_id[inc],",",input$credit_amount[inc],",",
    input$installment_amount[inc],",",input$credit_amount_updated[inc],",",
    input$installment_amount_updated[inc],",",input$hide_until_date[inc],",",
    input$consultant_id[inc],",",input$active_from[inc],",",
    input$active_to[inc],",'",input$created_at[inc],"',",
    input$updated_at[inc],",",input$deleted_at[inc],")",
    sep=""))
}

# Define sql string query for writing in DB for PO refinanced
gen_sql_string_po_refinance <- function(input,inc){
   return(paste("(",input$application_id[inc],",",
     input$product_id[inc],",",input$min_amount[inc],",",
     input$max_amount[inc],",",input$max_installment[inc],",",
     input$max_amount_updated[inc],",",input$max_installment_updated[inc],",",
     input$ref_application_id[inc],",",input$status[inc],",",
     input$processed_by[inc],",'",input$created_at[inc],"',",
     input$updated_at[inc],",",input$deleted_at[inc],")",
     sep=""))
}

# Correct maximum installment amount of PO 
gen_correct_max_installment_po <- function(period_po,period,installment_amount){
  if(period_po==3 & period==1){
    result <- installment_amount*7/30
  } else if (period_po==3 & period==2){
    result <- installment_amount*14/30
  } else if (period_po==2 & period==3){
    result <- installment_amount*30/14
  } else if (period_po==2 & period==1){
    result <- installment_amount*7/14
  } else if (period_po==1 & period==3){
    result <- installment_amount*30/7
  } else if (period_po==1 & period==2){
    result <- installment_amount*14/7
  }
  return(result)
}

# Function to create column for scoring table for display
gen_final_table_display <- function(scoring_df,flag_credirect){
  scoring_df$display_score <- 
   ifelse(scoring_df$color %in% c(1) & !(scoring_df$score %in% c("NULL")),"No",
   ifelse(scoring_df$score %in% c("NULL"),"NULL","Yes"))
  scoring_df$color <- ifelse(scoring_df$display_score=="No",1,
   ifelse(scoring_df$display_score=="NULL",2, 6))

  return(scoring_df)
}

# Function  to get last credit per company 
gen_if_credit_after_po_terminated <- function(input,table_po,name,company){
 result <- as.data.frame(aggregate(
  input$id[input$client_id %in% table_po$client_id & input$company_id==company],
  by=list(input$client_id[input$client_id %in% table_po$client_id & 
          input$company_id==company]),FUN=max))
 names(result) <- c("client_id",name)
 result <- merge(result,input[,c("id","signed_at")],by.x = name,by.y = "id",
                  all.x = TRUE)
}

# Function to make string for DB update of PO terminated (delete offer)
gen_string_po_terminated <- function(input){
  string_sql_update <- input$id[1]
  if(nrow(input)>1){
    for(i in 2:nrow(input)){
      string_sql_update <- paste(string_sql_update,input$id[i],
                                 sep=",")}}
  return(paste("(",string_sql_update,")",sep=""))
}


# Function to make string for DB update of PO terminated (delete offer)
gen_string_po_refinance <- function(input){
  string_sql_update <- input$application_id[1]
  if(nrow(input)>1){
    for(i in 2:nrow(input)){
      string_sql_update <- paste(string_sql_update,input$application_id[i],
                                 sep=",")}}
  return(paste("(",string_sql_update,")",sep=""))
}

# Function to make string for DB update of PO terminated (update offer)
gen_string_delete_po_terminated <- function(input,var,var_name,db_name){
  iterate_string <- paste("WHEN id = ",input$id[1]," THEN ",var[1],sep="")
  if(nrow(input)>1){
    for(i in 2:nrow(input)){
      iterate_string <- paste(iterate_string,
        paste("WHEN id = ",input$id[i]," THEN ",var[i],sep=""))}
  }
  return(paste("UPDATE ",db_name,".clients_prior_approval_applications SET ",
    var_name," = CASE ",iterate_string," ELSE ",var_name," END;",sep=""))
}


# Function to make string for DB update of PO refinanced (update offer)
gen_string_delete_po_refinance <- function(input,var,var_name,db_name){
  iterate_string <- paste("WHEN application_id = ",input$application_id[1],
          " THEN ",var[1],sep="")
  if(nrow(input)>1){
    for(i in 2:nrow(input)){
      iterate_string <- paste(iterate_string,
          paste("WHEN application_id = ",input$application_id[i],
                " THEN ",var[i],sep=""))}
  }
  return(paste("UPDATE ",db_name,".prior_approval_refinances SET ",
          var_name," = CASE ",iterate_string," ELSE ",var_name," END;",sep=""))
}

# Function to make dataframe out of json data
gen_dataframe_json <- function(input){
  
  return(as.data.frame(do.call(rbind,lapply(input, 
      function(j) as.list(unlist(fromJSON(j, flatten=TRUE)))))))
  
}

# Treat API dataframe
gen_treat_api_df <- function(input){
  
  # Read and correct fields to make nice dataframe 
  if(!(is.na(input[[1]]))){
    api_payment_method <- ifelse(is.null(input$payment.method[[1]]),NA,
      input$payment.method[[1]])                    
    api_amount <- ifelse(is.null(input$amount[[1]]),NA,input$amount[[1]])
    api_referral_source <- ifelse(is.null(input$referral_source[[1]]),NA,
      input$referral_source[[1]])
    api_user_agent <- ifelse(is.null(input$user_agent[[1]]),NA,
      input$user_agent[[1]])
    api_email <- ifelse(is.null(input$client.email[[1]]),NA,
      input$client.email[[1]])
    api_period <- ifelse(is.null(input$period[[1]]),NA,input$period[[1]])
    api_partial_app_at <- ifelse(is.null(input$partial_application_at[[1]]),NA,
      input$partial_application_at[[1]])
    
  } else {
    api_payment_method <- NA
    api_amount <- NA
    api_referral_source <- NA
    api_user_agent <- NA
    api_email <- NA
    api_period <- NA
    api_partial_app_at <- NA
  }
  
  # Finalize output dataframe
  output <- as.data.frame(matrix(NA,ncol=7,nrow=1))
  names(output) <- c("payment_method","amount","referral_source","user_agent",
                     "email","period","partial_app_at")
  output$payment_method <- api_payment_method
  output$amount <- api_amount
  output$referral_source <- api_referral_source
  output$user_agent <- api_user_agent
  output$email <- api_email
  output$period <- api_period
  output$partial_app_at <- api_partial_app_at
  
  return(output)
}

# Get accept or decline reasons 
gen_decline_reason <- function(scoring_df,all_df,level,input){
  
  # Get score of exact amount and installments
  score_here <- scoring_df$color[scoring_df$amount==all_df$amount &
       scoring_df$period==all_df$installments]
  if(length(score_here)==0){
    score_here <- scoring_df$color[scoring_df$amount== unique(scoring_df$amount)
      [which.min(abs(all_df$amount - unique(scoring_df$amount)))] & 
       scoring_df$period==unique(scoring_df$period)
      [which.min(abs(all_df$installments - unique(scoring_df$period)))]]
  }
  
  # Apply accept/reject nomenclature
  if(!is.na(score_here) & !is.na(length(level))){
    if(level==10){
      if(score_here %in% c(2:6)){result <- 1} else {result <- 0}
      result <- cbind(score_here,level)
    } else {
      if(input[1]!=score_here){
        input[1] <- score_here
        result <- cbind(input,level)
      } else {
        result <- input
      }
    }
    
    # Comglomerate for final level
    if(level==83){
      result <- as.data.frame(result)
      result <- sum(result[,names(result)=="level"])
    }
  } else {
    result <- 99
  }
  return(result)
}

# Correct time format 
gen_time_format <- function(input){
  input$created_at2 <- as.POSIXct(Sys.Date())
  input$signed_at2 <- as.POSIXct(Sys.Date())
  input$deactivated_at2 <- as.POSIXct(Sys.Date())
  input$date <- as.POSIXct(Sys.Date())
  for(i in 1:nrow(input)){
    if(input$status[i] %in% c(4,5)){
      input$date[i] <- input$signed_at[i]
      input$signed_at[i] <- input$signed_at[i]
    } else {
      input$date[i] <- input$created_at[i]
    }
    input$signed_at2[i] <- input$signed_at[i]
    input$created_at2[i] <- input$created_at[i]
    input$deactivated_at2[i] <- input$deactivated_at[i]
  }
  input$signed_at <- input$signed_at2
  input$created_at <- input$created_at2
  input$deactivated_at <- input$deactivated_at2
  input <- input[ , -which(names(input) %in% c("signed_at2","created_at2",
                                               "deactivated_at2"))]
  return(input)
}

# Set json file 
gen_setjson <- function(df,all_flags,api_df){
  here <- cbind(df,all_flags,api_df[,c("period","payment_method",
     "amount","email","referral_source")])
  return(toJSON(here))
}

# Set log file
gen_log <- function(application_id,scoring_decision,json_out){
  
  here <- as.data.frame(cbind(application_id,scoring_decision,json_out))
  here <- cbind(here,Sys.time())
  names(here)[ncol(here)] <- "created_at"
  names(here) <- c("application_id","decision","input_params","created_at")
  
  return(here)
}

# Get how many days of play on Credirect's site
gen_play_days <- function(db_name,input){
  
  here <- gen_query(con,gen_play_query(db_name,all_df$client_id))
  here <- subset(here,here$created_at<=all_df$created_at)
  here$number_words <- sapply(strsplit(here$description, " "), length)
  here$nchar <- nchar(here$description)
  here <- subset(here,here$number_words==5 & here$nchar==29)
  if(nrow(here)>0){
    here$days <- substring(here$created_at,1,10)
    here <- here[!duplicated(here$days),]
    days_play <- nrow(here)
  } else {
    days_play <- 0
  }
  
  return(days_play)
}

# Function to get all previous credit ids but for refinance offer check
gen_prev_ids_ref_cor <- function(con,db_name,all_df,all_id){
  
  # Get company ID and all current actives
  all_df_local <- get_company_id_prev(db_name,all_df)
  input <- all_id[all_id$status %in% c(4) & 
                    all_id$company_id==all_df_local$company_id,]
  
  # Get those who were deactivated from not so long 
  input2 <- all_id[all_id$status %in% c(5) & 
                     all_id$company_id==all_df_local$company_id,]
  if(nrow(input2)>0){
    input2$time_since_deactiv <- difftime(Sys.time(),input2$deactivated_at,
                                          units=c("days"))
    input2 <- subset(input2,input2$time_since_deactiv<=3)
    if(nrow(input2)>0){
      input2 <- input2[order(input2$time_since_deactiv),]
      input2 <- input2[1,]
      input2 <- input2[ , -which(names(input2) %in% c("time_since_deactiv"))]
      if(nrow(input)>0){
        input <- rbind(input,input2)
      } else {
        input <- input2}
    }
  }

  string_sql_update <- input$id[1]
  if(nrow(input)>1){
    for(i in 2:nrow(input)){
      string_sql_update <- paste(string_sql_update,input$id[i],sep=",")
    }
  }
  
  return(list(input,string_sql_update))
}

# Compute if previous is third_side
gen_third_side_prev <- function(db_name,all_id,application_id){
  
  # Get from same company_id
  company <- all_id$company_id[all_id$id==application_id]
  all_id <- subset(all_id,all_id$company_id==company)
  
  if(nrow(all_id)>0){
    string_sql_update <- all_id$id[1]
    if(nrow(all_id)>1){
      for(i in 2:nrow(all_id)){
        string_sql_update <- paste(string_sql_update,all_id$id[i],sep=",")
      }
    }
  }
  
  # Get third side
  third_sides <- gen_query(con,gen_thid_side(db_name,string_sql_update))
  return(ifelse(nrow(subset(third_sides,
      !is.na(third_sides$third_side_date)))>0,1,0))
}

# Define function to get apply cutoffs for PTC 
gen_group_scores_ptc <- function(all_df,cutoffs){
  
  all_df$ptc_score <-
    ifelse(is.na(all_df$ptc),"NULL",
    ifelse(all_df$ptc>cutoffs[1],"very_high",
    ifelse(all_df$ptc>cutoffs[2],"high",
    ifelse(all_df$ptc>cutoffs[3],"medium",
    ifelse(all_df$ptc>cutoffs[4],"low","very_low")))))
  return(all_df)
  
}

# Compute flag exclusion
gen_flag_exclusion <- function(all_credits,flag_credirect,risk,all_df){
  
  all_credits <- merge(all_credits,
     gen_query(con,gen_get_company_id_query(db_name)),by.x = "product_id",
     by.y = "id",all.x = TRUE)
  
  if(flag_credirect==0){
    all_credits <- subset(all_credits,all_credits$company_id!=2)
  } 
  flag_exclusion <- ifelse(length(which(names(
    table(all_credits$sub_status)) %in% c(124,133)))>0, 1,
    ifelse(nrow(risk)>0, 1, 0))
  
  # Recorrect for City Restart
  if(all_df$product_id==102){
    flag_exclusion <- 0
  }
  
  return(flag_exclusion)
  
}

# Compute passed days of unpassed installment
gen_ratio_unpassed_installment <- function(db_name,activated,deactivated,id){
  
  # Read credit plan main
  plan <- gen_query(con,gen_get_credits_plan_main_query(db_name,id))
  plan$paid <-  as.Date(deactivated)
  plan_first <- plan[1,]
  plan_first$pay_day <- as.Date(activated)
  plan <- rbind(plan_first,plan)
  plan$difftime <- difftime(plan$paid,plan$pay_day,units = c("days"))
  plan$time_to_installment <- NA
  for(i in 2:(nrow(plan))){
    plan$time_to_installment[i] <- difftime(plan$pay_day[i],plan$pay_day[i-1],
      units = c("days"))
  }
  plan <- subset(plan,plan$difftime<0 & !is.na(plan$time_to_installment))
  if(nrow(plan)>0){
    ratio <- round(1-abs(plan$difftime[1]/plan$time_to_installment[1]))
  } else {
    ratio <- 0
  }
  return(ratio)
}

# Compute days between last app and current terminated (for PTC)
gen_days_since_app_ptc <- function(all_id,application_id){
  
  return(difftime(
    all_id$signed_at[all_id$id==application_id],
    all_id$deactivated_at[all_id$id==
      max(subset(all_id,all_id$id!=application_id & all_id$company_id==2)$id)],
    units = c("days")))
}

# Get if client has prior app to current credit (for PTC)
gen_prev_apps_ptc <- function(all_credits,application_id,db_name){
  
  all_credits <- merge(all_credits,
     gen_query(con,gen_get_company_id_query(db_name)),by.x = "product_id",
     by.y = "id",all.x = TRUE)
  all_credits <- subset(all_credits,all_credits$company_id==2 & 
     all_credits$created_at<all_credits$created_at[
     all_credits$id==application_id])
  
  return(ifelse(nrow(all_credits)>0,1,0))
  
}

# Function to check if client has current active of other company
gen_active_other_brand <- function(all_id,application_id,brand){
  return(ifelse(
    nrow(subset(all_id,all_id$company_id==brand & all_id$status==4))>0,1,0))
  
}

# Apply PTC model to all
gen_ptc <- function(all_df,all_credits,all_id,application_id,
       flag_credirect,flag_credit_next_salary,flag_beh_company,db_name){
  
  if(flag_credirect==0){
    all_df$ptc <- round(gen_ptc_citycash(all_df),3)
    cutoffs <- cu_ptc_citycash
    
  } else {
    if(flag_beh_company==0 & flag_credit_next_salary==1){
      all_df$ptc <- round(gen_ptc_credirect_gratis(all_df,all_id,
         application_id,all_credits,db_name),3)
      cutoffs <- cu_ptc_gratis
    } else if(flag_beh_company==1 & flag_credit_next_salary==1){
      all_df$ptc <- round(gen_ptc_credirect_flex(all_df,all_id,application_id,
         all_credits,db_name) ,3)
      cutoffs <- cu_ptc_flex
    } else if(flag_beh_company==0 & flag_credit_next_salary==0){
      all_df$ptc <- round(gen_ptc_credirect_first_installments(all_df,all_id,
         application_id,all_credits,db_name),3)
      cutoffs <- cu_ptc_consumer_new
    } else {
      all_df$ptc <- round(gen_ptc_credirect_repeat_installments(all_df,all_id,
         application_id,all_credits,db_name),3)
      cutoffs <- cu_ptc_consumer_rep
    }
  }
  all_df <- gen_group_scores_ptc(all_df,cutoffs)
  return(all_df)
}

# Generate MSF score
gen_msf <- function(input,brand){
  
  # Select brand id
  input <- subset(input,input$brand_id==brand)
  
  # Create dataframe
  input <- input[rev(order(input$signed_at)),]
  input <- input[order(input$client_id),]
  clients <- input[!duplicated(input$client_id),c("client_id","signed_at","id",
    "sub_status")]
  names(clients) <- c("client_id","last_signed","last_id","last_status")
  clients$sold <- ifelse(!is.na(clients$last_status) & clients$last_status==124,
    "sold","normal")
  
  # Get total number of credits
  nb_credits <- aggregate(input$credit,by=list(input$client_id),FUN=sum)
  clients <- merge(clients,nb_credits,by.x = "client_id",by.y = "Group.1",
     all.x = TRUE)
  names(clients)[ncol(clients)] <- c("tot_credits")
  
  # Get entry score
  entry_score <- subset(input,!is.na(input$score))
  entry_score <- entry_score[order(entry_score$signed_at),]
  entry_score <- entry_score[order(entry_score$client_id),]
  entry_score <- entry_score[!duplicated(entry_score$client_id),c("client_id",
    "id","score")]
  clients <- merge(clients,entry_score[,c("client_id","score")],
    by.x = "client_id",by.y = "client_id",all.x = TRUE)
  names(clients)[ncol(clients)] <- c("entry_score")
  
  # Get cltv (and removing last credit)
  input_here <- input
  last_credit_here <- aggregate(input_here$id,by=list(input_here$client_id),
    FUN=max)
  input_here <- merge(input_here,last_credit_here,by.x = "client_id",
    by.y = "Group.1",all.x = TRUE)
  names(input_here)[ncol(input_here)] <- c("last_id")
  input_here <- subset(input_here,input_here$id!=input_here$last_id)
  cltv <- aggregate(input_here$profit,by=list(input_here$client_id),FUN=sum)
  clients <- merge(clients,cltv,by.x = "client_id",by.y = "Group.1",
    all.x = TRUE)
  names(clients)[ncol(clients)] <- c("profit")
  clients$profit <- ifelse(is.na(clients$profit),0,clients$profit)
  
  # Create bins
  clients$tot_credits <- clients$tot_credits - 1
  clients$score_bin <-
    ifelse(clients$entry_score%in% c("Bad","Indeterminate"),1,
    ifelse(clients$entry_score=="Good 1",2,
    ifelse(clients$entry_score=="Good 2",3,
    ifelse(clients$entry_score=="Good 3",4,5))))
  clients$score_bin <- ifelse(is.na(clients$entry_score),3,
    ifelse(clients$entry_score=="",3,clients$score_bin))
  clients$frequency <- 
    ifelse(clients$score_bin==1,
    ifelse(clients$tot_credits==0,0,1),ifelse(clients$tot_credits==0,1,0))
  clients$monetary <- ifelse(clients$profit>=1000,1,0)
  
  # Create clients field
  clients$rfm <- clients$score_bin + clients$frequency + clients$monetary
  
  # Make RFM field from 0 to 1000
  clients$rfm <- round((clients$rfm - min(clients$rfm)) /
     (max(clients$rfm) - min(clients$rfm)) * 1000)
  clients$rfm <- ifelse(clients$rfm==0,100,clients$rfm)
  
  # Make RFM field from 0 to 1000
  clients$rfm <- ifelse(is.na(clients$entry_score),clients$rfm,
    ifelse(clients$entry_score=="Indeterminate" & 
           clients$tot_credits>0,clients$rfm-100,clients$rfm))

  # Discretize MSF_score
  clients$rfm_score <- 
    ifelse(clients$rfm<=200,"very_low",
    ifelse(clients$rfm<=400,"low",
    ifelse(clients$rfm<=600,"medium",
    ifelse(clients$rfm<=800,"high","very_high"))))

  # Add more fields
  clients$type <- 2
  clients$brand_id <- brand
  
  return(clients)
}

# Generate RFM score
gen_rfm <- function(input,brand,df_here){

  # Select brand id
  input <- subset(input,input$brand_id==brand)
  df_here <- subset(df,df$brand_id==brand)

  # Get input raw 
  input_raw <- input

  # Create Recency
  ecdf_fun <- function(x,perc) ecdf(x)(perc)
  input$time_since <- round(difftime(Sys.time(),input$last_signed,
    units = c("days")))
  input$recency <- abs(1-ecdf_fun(input$time_since,input$time_since))
  input$recency <- round(10*(input$recency-min(input$recency))/
                           (max(input$recency)-min(input$recency)))

  # Create Frequency
  input$tot_credits <- input$tot_credits + 1
  input$frequency <- ecdf_fun(input$tot_credits,input$tot_credits)
  input$frequency <- round(10*(input$frequency-min(input$frequency))/
     (max(input$frequency)-min(input$frequency)))
  
  # Create Monetary
  df_here$profit <- ifelse(is.na(df_here$deactivated_at) & 
    df_here$signed_at>(Sys.Date()-180),0,df_here$profit)
  df_here$profit <- as.numeric(as.character(df_here$profit))
  profit <- aggregate(df_here$profit,by=list(df_here$client_id),FUN=sum)
  input <- merge(input,profit,by.x = "client_id",by.y = "Group.1",
      all.x = TRUE)
  names(input)[ncol(input)] <- c("profit2")
  input$monetary <- ecdf_fun(input$profit2,input$profit2)
  input$monetary <- round(10*(input$monetary-min(input$monetary))/
                            (max(input$monetary)-min(input$monetary)))
  
  # Create RFM score 
  input$rfm <- input$recency + input$frequency + 2 * input$monetary
  input$rfm <- round((input$rfm - min(input$rfm)) /
                         (max(input$rfm) - min(input$rfm)) * 1000)
  
  # Bin RFM
  input$rfm_score <- 
    ifelse(input$rfm<=200,"very_low",
    ifelse(input$rfm<=400,"low",
    ifelse(input$rfm<=600,"medium",
    ifelse(input$rfm<=800,"high","very_high"))))
  
  # Make final dataframe
  input_raw <- input_raw[,c("client_id","rfm","rfm_score","type","brand_id")]
  input_new <- input[,c("client_id","rfm","rfm_score","type","brand_id")]
  input_new$type <- 1
  input_final <- rbind(input_raw,input_new)
  
  return(input_final)
  
}

# Define sql string query for writing in DB for RFM score 
gen_sql_string_po_rfm <- function(input,inc){
  return(paste("(",input$id[inc],",",
    input$client_id[inc],",",input$rfm[inc],",'",input$rfm_score[inc],"',",
    input$type[inc],",",input$brand_id[inc],",'",input$created_at[inc],"','",
    input$updated_at[inc],"')",sep=""))
}

# Function to make string for DB update of RFM table
gen_sql_string_update_rfm <- function(input,var,var_name,db_name,crit){
  if(crit==0){
    iterate_string <- paste("WHEN id = ",input$id[1]," THEN ",
      var[1],sep="")
  }
  else {
    iterate_string <- paste("WHEN id = ",input$id[1]," THEN '",
      var[1],"'",sep="")
  }
  if(nrow(input)>1){
    for(i in 2:nrow(input)){
      if(crit==0){
        iterate_string <- paste(iterate_string,
          paste("WHEN id = ",input$id[i]," THEN ",var[i],sep=""))
      } else {
        iterate_string <- paste(iterate_string,
          paste("WHEN id = ",input$id[i]," THEN '",var[i],
                "'",sep="")) 
      }
    }
  }
  return(paste("UPDATE ",db_name,".credits_applications_rfm_score SET ",
     var_name," = CASE ",iterate_string," ELSE ",var_name," END;",sep=""))
}

# Generate number of credits per brand - for a list
gen_append_nb_credits <- function(db_name,input){
  
  list_clients <- input$client_id[1]
  if(nrow(input)>1){
    for(i in 2:nrow(input)){
      list_clients <- paste(list_clients,input$client_id[i],sep=",")
    }
  }
  
  # Get number of credits
  get_nb_credits <- gen_query(con,gen_credits_of_clients(db_name,list_clients))
  get_nb_credits <- subset(get_nb_credits,
    !(get_nb_credits$sub_status %in% c(129,122)) | 
    is.na(get_nb_credits$sub_status))
  nb_credits <- as.data.frame((table(
    get_nb_credits$client_id,get_nb_credits$brand_id)))
  input <- merge(input,nb_credits,by.x = c("client_id","company_id"),
    by.y = c("Var1","Var2"),all.x = TRUE)
  names(input)[ncol(input)] <- c("nb_credits")
  input$nb_credits <- ifelse(is.na(input$nb_credits),0,input$nb_credits)
  
  return(input)
  
}

# Generate probability to churn
gen_list_ptc <- function(db_name,input){
  
  list_ids <- input$id[1]
  if(nrow(input)>1){
    for(i in 2:nrow(input)){
      list_ids <- paste(list_ids,input$id[i],sep=",")
    }
  }
  
  # Get probability to churn 
  get_ptc <- gen_query(con,gen_ptc_list_query(db_name,list_ids))
  input <- merge(input,get_ptc,by.x = "id",by.y = "application_id",all.x = TRUE)
  
  return(input)
  
}

# Generate list of flag if pay day or not
gen_flag_payday <- function(db_name,input){
  
  get_type_payday <-  gen_query(con,gen_payday_query(db_name))
  input <- merge(input,get_type_payday,by.x = "product_id",by.y = "id",
                 all.x = TRUE)
  return(input)
}

# Define sql string query for writing in DB for provisions
gen_sql_string_prov <- function(input,inc){
  return(paste("(",input$id[inc],",",
      input$application_id[inc],",",
      input$type[inc],",",input$provision[inc],",'",input$created_at[inc],"','",
      input$updated_at[inc],"')",sep=""))
}

# Check if first is refinanced
gen_if_first_was_ref <- function(db_name,input){
  
  # Create string to get all credits from client
  string_sql <- input$client_id[1]
  if(nrow(input)>1){
    for(i in 2:nrow(input)){
      string_sql<- paste(string_sql,input$client_id[i],
                         sep=",")}}
  
  # Get all credits 
  all_credits <- gen_query(con,gen_all_credits_list_query(db_name,string_sql))
  all_credits <- subset(all_credits,!(all_credits$sub_status %in% c(129,122)) | 
      is.na(all_credits$sub_status))
  all_credits <- merge(all_credits,input[,c("client_id","company_id")],
       by.x = "client_id",by.y = "client_id",all.x = TRUE)
  all_credits <- subset(all_credits,all_credits$company_id==
       all_credits$brand_id)
  
  # Get if first was refinanced
  first <- all_credits[order(all_credits$signed_at),]
  first <- first[order(first$client_id),]
  first <- first[!duplicated(first[c("client_id","brand_id")]),]
  second <- all_credits[!(all_credits$id %in% first$id),]
  second <- second[order(second$signed_at),]
  second <- second[!duplicated(second[c("client_id","brand_id")]),]
  second <- second[order(second$signed_at),]
  get_first <- merge(
    first[,c("client_id","brand_id","created_at","deactivated_at")],
    second[,c("client_id","brand_id","created_at")],
    by.x = c("client_id","brand_id"),by.y = c("client_id","brand_id"),
    all.x = TRUE)
  get_first$ref_first <-
    ifelse(is.na(get_first$deactivated_at),1,
    ifelse(substring(get_first$deactivated_at,1,10)==
           substring(get_first$created_at.y,1,10),1,0))
  input <- merge(input,get_first[,c("client_id","brand_id","ref_first")]
                 ,by.x = c("client_id","company_id"),
                 by.y = c("client_id","brand_id"),all.x = TRUE)

  return(input)
}

# Get latest work data
gen_work_data <- function(db_name,input,all_df){
  
  # Get list of all application_id
  string_sql <- input$id[1]
  if(nrow(input)>1){
    for(i in 2:nrow(input)){
      string_sql <- paste(string_sql,input$id[i],sep=",")
    }
  }
  
  all_works <- gen_query(con,gen_get_work_info_query(db_name,string_sql))
  all_status <- subset(all_works,!is.na(all_works$status_work))
  all_exp <- subset(all_works,!is.na(all_works$experience_employer))
  
  # Get latest non-NA status work and experience employer
  if(nrow(all_status)>0){
    all_df$status_work <- subset(all_status,all_status$application_id==
         max(all_status$application_id))$status_work 
  } else {
    all_df$status_work <- NA
  }
  if(nrow(all_exp)>0){
    all_df$experience_employer <- subset(all_exp,all_exp$application_id==
        max(all_exp$application_id))$experience_employer
  } else {
    all_df$experience_employer <- NA
  }

  return(all_df)

}

# Get if cession in credirect
gen_cession_credirect <- function(all_id){
  
  cession <- subset(all_id,all_id$sub_status==124 & all_id$company_id==2)
  cession_flag <- ifelse(nrow(cession)>0,1,0)
  return(cession_flag)
  
}

# Define function to get apply cutoffs for prescores
gen_group_scores_prescore <- function(var,flag_beh,flag_credirect){
  if(flag_beh==0){
    cutoffs <- cu_app_city_prescore 
  } else {
    cutoffs <- cu_app_city_prescore 
  }
  if (var>cutoffs[1]){output="Bad"} 
  else if (var>cutoffs[2]) {output="Indeterminate"} 
  else if (var>cutoffs[3]) {output="Good 1"} 
  else if (var>cutoffs[4]) {output="Good 2"} 
  else if (var>cutoffs[5]) {output="Good 3"} 
  else {output="Good 4"}
  return (output)
}

# Get flag if parallel
gen_flag_parallel <- function(db_name,all_id,flag_cashpoint){
  
  para <- subset(all_id,all_id$status==4)
  has_para <- ifelse(nrow(para)>0,1,0)
  
  # No parallel if cashpoint
  if(flag_cashpoint==1){
    has_para_city <- subset(para,para$company_id==1)
    if(nrow(has_para_city)>0){
      max_dpd <- gen_query(con,gen_plan_main_select_query(db_name,
        paste(has_para_city$id,collapse=",")))$max_delay
    }
  }
  if(!exists("max_dpd")){max_dpd <- NA}
  return(list(has_para,max_dpd))
}

# Set new names for api scoring
gen_col_names_api <- function(input){
  
  colnames(input)[colnames(input) == "hasViber"] <- "has_viber"
  colnames(input)[colnames(input) == "statusWork"] <- "status_work"
  colnames(input)[colnames(input) == "timeOnAddress"] <- "on_address"
  colnames(input)[colnames(input) == "companyExperience"] <- 
    "experience_employer"
  colnames(input)[colnames(input) == "maritalStatus"] <- "marital_status"
  colnames(input)[colnames(input) == "CKR.active.activeCreditCount"] <- 
    "cred_count_total"
  colnames(input)[colnames(input) == "CKR.history.overduePaymentPeriod"] <- 
    "status_finished_total"
  colnames(input)[colnames(input) == "CKR.active.overduePaymentPeriod"] <- 
    "status_active_total"
  input$outs_overdue_ratio_total <-
    ifelse(!is.na(input$CKR.outstandingOverduePrincipal) & 
           !is.na(input$CKR.outstandingPerformingPrincipal),
           round(as.numeric(input$CKR.outstandingOverduePrincipal)/
                 as.numeric(input$CKR.outstandingPerformingPrincipal),3))
  input$source_entity_count <- NA
  return(input)
}

# Apply general linear models
gen_apply_model <- function(input,coefficients){
  
  input <- as.data.frame(t(input))
  input$vars <- rownames(input)
  names(input) <- c("values","vars")
  input$vars_value <- paste(input$vars,input$values,sep="")
  coefficients$vars <- rownames(coefficients)
  result <- merge(input,coefficients,by.x = "vars_value",by.y = "vars")
  if(nrow(result)>0){
    pd <- 1/(1+exp(-sum(result$coeff,coefficients$coeff
                        [coefficients$vars=="(Intercept)"])))
  } else{
    pd <- 1/(1+exp(-sum(coefficients$coeff
                        [coefficients$vars=="(Intercept)"])))
  }
  return(pd)
}

# Generate table for api scoring
gen_table_api_scoring <- function(score){
  
  vect <- as.data.frame(seq(200,1000,100))
  vect <- cbind(c(1:9),vect,NA)
  names(vect) <- c("id","amount","score")
  for(i in 1:nrow(vect)){
    vect$score[i] <- 
      ifelse(score=="Bad",0,
      ifelse(score=="Indeterminate",ifelse(vect$amount[i]<=500,1,0),
      ifelse(score=="Good 1",ifelse(vect$amount[i]<=600,1,0),
      ifelse(score=="Good 2",ifelse(vect$amount[i]<=800,1,0),
      ifelse(score=="Good 3",ifelse(vect$amount[i]<=1000,1,0),
      ifelse(score=="Good 4",ifelse(vect$amount[i]<=1000,1,0),0))))))
  }
  return(vect)
}

# Generate parallel score if existing
gen_parallel_score <- function(prev_amount,all_id,t_income,criteria_po,
     disposable_income_adj,flag_new_credirect_old_city,base_dir,amount_tab,
     products,scoring_df,df_Log_beh_CityCash,df_Log_beh_Credirect,api_df,period,
     all_df,flag_beh,flag_credirect,flag_cashpoint,flag_finmag){
  
  if(flag_beh==1 & flag_cashpoint==1){
    gen_beh_cashpoint_result <- suppressMessages(
      gen_beh_cashpoint(df,scoring_df,products,df_Log_beh_CityCash,period,
        all_id,all_df,prev_amount,amount_tab,
        t_income,disposable_income_adj,0,base_dir))
    logistic_cashpoint_beh_pd <- gen_beh_cashpoint_result[[1]]
    logistic_cashpoint_beh_score <- gen_beh_cashpoint_result[[2]]
    
    if(flag_otpisan==1 | flag_exclusion==1 | flag_varnat==1 | 
       flag_is_dead==1){
      logistic_cashpoint_beh_score <- "Bad"
    }
    
  }
  
  if(flag_beh==0 & flag_cashpoint==0 & flag_finmag==0 & flag_credirect==0){
    fraud_app_citycash <- suppressMessages(
      gen_app_citycash_fraud(df,scoring_df,base_dir))
    gbm_app_citycash <- suppressMessages(gen_app_bgm_citycash(df,scoring_df,
      products,period,all_df,prev_amount,amount_tab,t_income,
      disposable_income_adj,base_dir))
    gbm_citycash_app_pd <- gbm_app_citycash[[1]]
    gbm_citycash_app_score <- gbm_app_citycash[[2]]
  }
  
  # Check for empty parallel score fields
  if(!exists("gbm_credirect_beh_pd")){
    gbm_credirect_beh_pd <- NA
    gbm_credirect_beh_score <- NA
  }
  if(!exists("gbm_app_citycash")){
    gbm_citycash_app_pd  <- NA
    gbm_citycash_app_score <- NA
  }
  if(!exists("logistic_cashpoint_beh_pd")){
    logistic_cashpoint_beh_pd <- NA
    logistic_cashpoint_beh_score <- NA
  }
  if(!exists("fraud_app_citycash")){
    fraud_app_citycash <- NA
  }
  return(as.data.frame(cbind(gbm_credirect_beh_pd,gbm_credirect_beh_score,
     logistic_cashpoint_beh_pd,logistic_cashpoint_beh_score,
     fraud_app_citycash,gbm_citycash_app_pd,gbm_citycash_app_score)))
}

# Generate PA offer in main scoring
gen_pa_term_citycash <- function(db_name,empty_fields,threshold_empty,
  flag_exclusion,flag_varnat,flag_is_dead,flag_credit_next_salary,
  flag_credirect,flag_beh,all_df,scoring_df,df,products,df_Log_beh_CityCash,
  df_Log_CityCash_App,df_Log_beh_Credirect,df_Log_Credirect_App_installments,
  df_Log_Credirect_App_payday,period,all_id,prev_amount,amount_tab,
  t_income,disposable_income_adj,flag_new_credirect_old_city,api_df,
  flag_judicial,flag_third_side,flag_cashpoint,base_dir,flag_otpisan,
  flag_finmag,flag_cession,flag_bad_ckr_citycash,application_id,
  flag_beh_company,fraud_flag,flag_risky_address,flag_parallel){
  
  # Rescore for City Cash
  flag_finmag <- 0
  flag_credirect <- 1
  all_df$product_id <- df$product_id <- 98
  all_df$total_income <- df$total_income <- 1000
  products <- gen_query(con,gen_products_query(db_name,all_df))
  
  # Generate products table
  scoring_df <- gen_final_df(products,application_id)

  # Generate score
  scoring_df <- gen_apply_score(
    empty_fields,threshold_empty,flag_exclusion,
    flag_varnat,flag_is_dead,flag_credit_next_salary,flag_credirect,
    flag_beh,all_df,scoring_df,df,products,df_Log_beh_CityCash,
    df_Log_CityCash_App,df_Log_beh_Credirect,df_Log_Credirect_App_installments,
    df_Log_Credirect_App_payday,period,all_id,prev_amount,amount_tab,
    t_income,disposable_income_adj,flag_new_credirect_old_city,api_df,
    flag_judicial,0,flag_third_side,flag_cashpoint,base_dir,0,flag_otpisan,
    flag_finmag)

  # Apply policy rules 
  scoring_df <- gen_apply_policy(scoring_df,flag_credirect,flag_cession,
   flag_bad_ckr_citycash,all_df,all_id,flag_beh,prev_amount,products,
   application_id,flag_new_credirect_old_city,flag_credit_next_salary,
   flag_beh_company,flag_cashpoint,0,fraud_flag,flag_risky_address,
   flag_parallel,flag_finmag)
  
  # Join installments if necessary
  if(!("installmesnt_amount" %in% names(scoring_df))){
    scoring_df <- merge(scoring_df,products[,c("amount","period",
      "installment_amount")],by.x = c("amount","period"),
      by.y = c("amount","period"),all.x = TRUE)
  }

  # Subset scoring dataframe according to criteria
  correct_scoring_df <- subset(scoring_df,scoring_df$color!=1 &
   scoring_df$score %in% c("Good 3","Good 4"))
  
  # Get highest amount
  get_max_amount <- suppressWarnings(max(correct_scoring_df$amount))
  
  # Get maximum installment
  if(is.infinite(get_max_amount)){
    get_max_installment <- -Inf	
  } else {	
    get_max_installment <- max(scoring_df$installment_amount[
      scoring_df$color!=1 & scoring_df$amount==get_max_amount])	
  }

  return(list(get_max_amount,get_max_installment))

}

# Generate string to write in PA table for Finmag-CityCash
gen_pa_term_citycash_string <- function(db_name,all_df,check_offer,flag_add){
  
  id_max_query <- paste(
    "SELECT MAX(id) as max_id
  FROM ",db_name,".clients_prior_approval_applications",sep="")
  id_max <- gen_query(con,id_max_query)$max_id + 1 + flag_add
  
  # Buld string 
  pa_str <- all_df[,c("client_id","product_id")]
  pa_str$product_id <- 98
  pa_str$id <- id_max
  pa_str$group <- NA
  pa_str$office_id <- 240
  pa_str$installment_amount <- check_offer[[2]]
  pa_str$credit_amount <- check_offer[[1]]
  pa_str$credit_amount_updated <- NA
  pa_str$application_id <- NA
  pa_str$installment_amount_updated <- NA
  pa_str$hide_until_date <- NA
  pa_str$consultant_id <- NA
  pa_str$active_from <- NA
  pa_str$active_to <- NA
  pa_str$created_at <- Sys.time()
  pa_str$updated_at <- NA
  pa_str$deleted_at <- NA
  
  pa_str[is.na(pa_str)] <- "NULL"
  
  return(gen_sql_string_po_terminated(pa_str,1))
  
}

# Delete if has prior PA city cash offers
gen_pa_term_citycash_string_delete <- function(db_name,all_df){
  
  prods <- gen_query(con,gen_get_company_id_query(db_name))
  delete_query <- paste(
    "SELECT id, client_id FROM ",db_name,".clients_prior_approval_applications 
  WHERE deleted_at IS NULL AND client_id = ",all_df$client,
    " AND product_id IN (98)",sep="")
  to_delete <- gen_query(con,delete_query)
  return(to_delete)
  
}

# Generate string to write in table call_center_offers_suggestions
gen_call_center_offers_citycash_string <- function(db_name,all_df,flag_add){
  
  id_max_query <- paste(
  "SELECT MAX(id) as max_id
  FROM ",db_name,".call_center_offers_suggestions",sep="")
  id_max <- gen_query(con,id_max_query)$max_id + 1 + flag_add
  
  # Buld string 
  pa_str <- all_df[,c("client_id","application_id")]
  pa_str$id <- id_max
  pa_str$ref_application_id <- NA
  pa_str$status <- 1
  pa_str$processed_by <- NA
  pa_str$created_at <- Sys.time()
  pa_str$updated_at <- NA
  pa_str[is.na(pa_str)] <- "NULL"
  
  final_str <- paste("(",pa_str$id[1],",",
        pa_str$client_id[1],",",pa_str$application_id[1],",",
        pa_str$ref_application_id[1],",",pa_str$status[1],",",
        pa_str$processed_by[1],",'",pa_str$created_at[1],"',",
        pa_str$updated_at[1],")",sep="")
  
  return(final_str)
  
}

# Generate credit history
gen_credit_history <- function(db_name, all_df, brand_ids){
  all_df$curr_credit <- 1
  ids <- all_df %>% distinct(client_id)
  df <- gen_query(con, gen_credit_history_query(db_name, ids, brand_ids))
  df <- df %>%
    left_join(all_df[,c("application_id", "curr_credit")], 
              by = c("id"="application_id")) %>%
    mutate(curr_credit = ifelse(is.na(curr_credit), 0, curr_credit)) %>%
    group_by(client_id) %>%
    arrange(client_id, signed_at) %>%
    mutate(credit_flag = 1,
           credit_num = cumsum(credit_flag),
           credit_count = lag(credit_num, default = 0),
           days_delay_cum = cummax(max_days_delay),
           max_dpd_app = lag(days_delay_cum, default = 0)) %>%
    ungroup() %>%
    filter(curr_credit == 1) %>%
    select(id, has_prev_brand_credits, has_prev_credits, credit_count, 
           max_dpd_app) %>%
    mutate(default = as.numeric(max_dpd_app > 90))
  
  df <- merge(all_df, df, by.x = "application_id", by.y = "id", all.x = T)
  df <- df %>% select(-curr_credit)
  return(df)
}

# Generate call center contact history
gen_call_history <- function(db_name, all_df){
  incoming <- gen_query(con, gen_incoming_calls_query(db_name, all_df))
  incoming <- incoming %>%
    left_join(all_df[,c("application_id", "dpd_date")], 
              by = "application_id") %>%
    filter(call_date < dpd_date) %>%
    group_by(application_id) %>%
    summarise(incoming_contact = as.numeric(n() > 0))
  all_df <- merge(all_df, incoming, by = "application_id", all.x = T)
  all_df$incoming_contact[is.na(all_df$incoming_contact)] <- 0
  
  outgoing <- gen_query(con, gen_outgoing_calls_query(db_name, all_df))
  outgoing$created_at <- as.Date(outgoing$created_at)
  
  out_calls <- outgoing %>%
    left_join(all_df[,c("application_id", "dpd_date")], 
              by = "application_id") %>%
    mutate(outgoing_contacts = as.numeric(created_at <= dpd_date),
           call_1w_prior_dpd = as.numeric(created_at >= (dpd_date - days(7)) 
                                          & created_at <= dpd_date),
           call_2w_prior_dpd = as.numeric(created_at >= (dpd_date - days(14)) 
                                          & created_at <= dpd_date),
           call_1m_prior_dpd = as.numeric(created_at >= (dpd_date - days(30)) 
                                          & created_at <= dpd_date)) %>%
    group_by(application_id) %>%
    summarise(outgoing_contacts = as.numeric(sum(outgoing_contacts) > 0),
              call_1w_prior_dpd = sum(call_1w_prior_dpd, na.rm = T), 
              call_2w_prior_dpd = sum(call_2w_prior_dpd, na.rm = T), 
              call_1m_prior_dpd = sum(call_1m_prior_dpd, na.rm = T))
  all_df <- merge(all_df, out_calls, by = "application_id", all.x = T)
  
  last_result <- outgoing %>%
    left_join(all_df[,c("application_id", "dpd_date")], 
              by = "application_id") %>%
    filter(dpd_date >= created_at) %>%
    arrange(application_id, desc(created_at)) %>%
    distinct(application_id, .keep_all = T) %>%
    select(application_id, type) %>%
    rename("last_result" = "type")
  all_df <- merge(all_df, last_result, by = "application_id", all.x = T)
  
  return(all_df)
}

# Generate payment ratio and default installment ratio
gen_payment_ratio <- function(db_name, all_df){
  # Gen paid before
  paid_amount <- gen_query(con, 
                           gen_total_paid_amount_query(all_df$application_id,db_name))
  colnames(paid_amount)[2] <- "paid_amount"
  
  # Gen taxes
  taxes <- gen_query(con, gen_taxes_amount(db_name, all_df$application_id, 
                                           incl_ids = 1))
  
  # Gen plan main
  plan_main <- gen_query(con, gen_plan_main_query(db_name, all_df))
  plan_main <- merge(plan_main, all_df[,c("application_id", "dpd_date")], 
                     by = "application_id", all.x = T)
  
  # Calculate payment ratio
  payment_ratio <- plan_main %>% 
    filter(pay_day <= dpd_date) %>%
    group_by(application_id) %>%
    summarise(due_amount = sum(due_amount, na.rm = T)) %>%
    left_join(paid_amount, by = c("application_id" = "object_id")) %>%
    left_join(taxes, by = "application_id") %>%
    mutate(paid_amount = ifelse(is.na(paid_amount), 0, paid_amount),
           tax_amount = ifelse(is.na(tax_amount), 0, tax_amount),
           paid_amount = paid_amount,
           passed_amount = due_amount+tax_amount,
           repayment_ratio = paid_amount / passed_amount,
           repayment_ratio = ifelse(is.infinite(repayment_ratio) | 
                                      repayment_ratio > 1, 1, repayment_ratio))
  
  all_df <- merge(all_df, payment_ratio[,c("application_id", 
                                           "repayment_ratio")], all.x = T)
  return(all_df)
}

# Gen passed installments before DPD group/All installments ratio
gen_default_inst_ratio <- function(db_name, all_df){
  plan_main <- gen_query(con, gen_plan_main_query(db_name, all_df))
  plan_main <- merge(plan_main, all_df[,c("application_id", "lower_dpd")], 
                     by = "application_id", all.x = T)
  default_installment <- plan_main %>%
    group_by(application_id) %>%
    summarise(default_inst_ratio = 
                min(installment_num[days_delay >= lower_dpd]) / max(installment_num))
  
  all_df <- merge(all_df, default_installment, by = "application_id", all.x = T)
  return(all_df)
}


# Fill factor and numeric NA's with -999
fill_na <- function(column, numeric_value, factor_value) {
  if (is.factor(column)) {
    levels(column) <- c(levels(column), factor_value)
    column[is.na(column)] <- factor_value
  } else if (is.numeric(column)) {
    column[is.na(column)] <- numeric_value
  }
  return(column)
}

# Generate and order dummy columns in GBM style
gen_gbm_dummies <- function(data, feature_names, features) {
  encoded_data <- data.frame(matrix(ncol = 0, nrow = nrow(data)))
  
  for (feature in feature_names) {
    if (is.factor(data[[feature]])) {
      levels_of_feature <- levels(data[[feature]])
      for (level in levels_of_feature) {
        dummy_column_name <- paste0(feature, level)
        encoded_data[[dummy_column_name]] <- as.numeric(data[[feature]] == level)
      }
    } else {
      encoded_data[[feature]] <- data[[feature]]
    }
  }
  
  # In case of missing dummy vars -> create
  missing_features <- features[!features %in% colnames(encoded_data)]
  encoded_data[missing_features] <- 0
  encoded_data <- encoded_data %>% select(all_of(features))
  
  return(encoded_data)
}

# Generate the tree structure for a single tree
gen_tree_structure <- function(model_trees, n){
  tree_structure <- model_trees[[n]]
  
  tree_structure <- data.frame(
    SplitVar = tree_structure[[1]],
    SplitCodePred = tree_structure[[2]],
    LeftNode = tree_structure[[3]],
    RightNode = tree_structure[[4]],
    Prediction = tree_structure[[5]]
    
  )
  return(tree_structure)
}

# Function to get the final prediction for a single tree
gen_terminal_prediction <- function(encoded_data, tree_data) {
  
  current_node <- 0
  
  while (tree_data$SplitVar[current_node + 1] != -1) {
    split_var <- tree_data$SplitVar[current_node + 1]
    split_value <- tree_data$SplitCodePred[current_node + 1]
    feature_value <- encoded_data[[split_var + 1]]
    
    if (feature_value < split_value) {
      current_node <- tree_data$LeftNode[current_node + 1]
    } else {
      current_node <- tree_data$RightNode[current_node + 1]
    }
  }
  return(tree_data$Prediction[current_node + 1])
}

# Softmax function to calculate probabilities from log-odds
gen_softmax <- function(x) {
  exp_x <- exp(x - max(x))  # Subtracting max for numerical stability
  exp_x / sum(exp_x)
}

# Custom multiclass gbm predict function
gen_multiclass_gbm_probs <- function(model_info, data, n_trees, n_classes,
                                     category_labels) {
  
  initial_log_odds <- model_info$initial_pred
  model_trees <- model_info$model_trees
  features <- model_info$features
  feature_names <- colnames(data)[-1]
  
  encoded_data <- gen_gbm_dummies(data, feature_names, features)
  
  # Initialize log-odds accumulation for each class
  log_odds_per_class <- rep(0, n_classes)
  
  log_odds_per_row <- matrix(initial_log_odds, nrow = nrow(encoded_data), 
                             ncol = n_classes)
  
  for (i in 1:n_trees) {
    for (class in 1:n_classes) {
      tree_index <- (i - 1) * n_classes + class
      tree_data <- gen_tree_structure(model_trees, tree_index)
      
      # Get predictions for each row
      row_predictions <- apply(encoded_data, 1, function(row) {
        gen_terminal_prediction(row, tree_data)
      })
      
      # Accumulate the predictions into the log odds matrix
      log_odds_per_row[, class] <- log_odds_per_row[, class] + row_predictions
    }
  }
  
  final_probs <- as.data.frame(t(apply(log_odds_per_row, 1, gen_softmax)))
  colnames(final_probs) <- category_labels
  
  return(final_probs)
}

