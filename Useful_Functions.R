
###################################################
######## Define some extra useful functions  ######
###################################################

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
    var_off %in% c("93","139","27","133","136","53","73","140","125","41",
                   "76","142","95","74","134","132","100","33","92",
                   "137","28","36","25","5","106","120","85","15",
                   "99","50","135","54","51","88","12"
    ), 1,
    ifelse(
      var_off %in% c("52","94","18","107","90","34","81","46","96","64",
                     "83","71","78","72","130","3","32","42","56","124",
                     "69","2","58","47","108","55","31","57","8","118",
                     "68","70","23","110","16","21","86","11","113","104","13"
                     
      ), 2,
      ifelse(
        var_off %in% c("75","79","97","98","7","17","35","4","24","9","91",
                       "30","114","87","61","14","59","84","43","1","80","128",
                       "49","29","10","44","145","121"
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
    input$consultant_id[inc],",'",input$created_at[inc],"',",
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
gen_final_table_display <- function(scoring_df){
  scoring_df$display_score <- 
   ifelse(scoring_df$color %in% c(1),"No",
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
    api_period <- ifelse(is.null(api_df$period[[1]]),NA,api_df$period[[1]])
    
  } else {
    api_payment_method <- NA
    api_amount <- NA
    api_referral_source <- NA
    api_user_agent <- NA
    api_email <- NA
    api_period <- NA
  }
  
  # Finalize output dataframe
  output <- as.data.frame(matrix(NA,ncol=6,nrow=1))
  names(output) <- c("payment_method","amount","referral_source","user_agent",
                     "email","period")
  output$payment_method <- api_payment_method
  output$amount <- api_amount
  output$referral_source <- api_referral_source
  output$user_agent <- api_user_agent
  output$email <- api_email
  output$period <- api_period
  
  return(output)
}

