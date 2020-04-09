
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
gen_final_df <- function(products){
  
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
    var_off %in% c("100","125","95","21","137","76","99","133",
         "41","54","97","33","42","64","78","52","92","53","51","32",
         "25","120", "107","118","28","124","132","18","5","90","34",
         "81","71","80","12"), 1,
    ifelse(
      var_off %in% c("27","96","108","29","69","31","98","86",
         "139","110","93","88","15","35","50","56","68","104","23","91",
         "8","113","130","61","36","4","106","72","73","85","142","140",
         "136","135","105","74","117","134"
      ), 2,
      ifelse(
        var_off %in% c("75","3","58","47","17","94","16","83",
          "9","59","114","13","70","46","55","14","7","57","24","87",
          "79","128","11","30","1","84","43","2","10","49","44"
        ), 3, 2
      ))))
}

# Define cutoffs application for credirect fraud
gen_group_scores_fraud <- function(var){
  results <- ifelse(var>cu_app_cred_frauds,1,0)
}


