

################################################################################
#             New script for generating new daily refinance offers             #
#      Apply Logistic Regression on all products (CityCash and Credirect)      #
#                          Version 1.0 (2020/06/04)                            #
################################################################################



########################
### Initial settings ###
########################

# Libraries
suppressMessages(suppressWarnings(library(RMySQL)))
suppressMessages(suppressWarnings(library(here)))
suppressMessages(suppressWarnings(library(dotenv)))
suppressMessages(suppressWarnings(require("reshape")))


# Defines the directory where custom .env file is located
load_dot_env(file = here('.env'))


#######################
### Manual settings ###
#######################

# Defines the directory where the RScript is located
base_dir <- here('app/Factories/Scoring')


# Define product id
product_id <- NA



#####################
####### MySQL #######
#####################

db_host <- Sys.getenv("DB_HOST", 
                      unset = "localhost", 
                      names = FALSE)
db_port <- strtoi(Sys.getenv("DB_PORT", 
                             unset = "3306", 
                             names = FALSE))
db_name <- Sys.getenv("DB_DATABASE", 
                      unset = "citycash", 
                      names = FALSE)
db_username <- Sys.getenv("DB_USERNAME", 
                          unset = "root", 
                          names = FALSE)
db_password <- Sys.getenv("DB_PASSWORD", 
                          unset = "secret", 
                          names = FALSE)
con <- dbConnect(MySQL(), user=db_username, 
                 password=db_password, dbname=db_name, 
                 host=db_host, port = db_port)
sqlMode <- paste("SET sql_mode=''", sep ="")
suppressWarnings(fetch(dbSendQuery(con, sqlMode), 
                       n=-1))



#################################
####### Load source files #######
#################################

# Load other r files
source(file.path(base_dir,"Refinance.r"))
source(file.path(base_dir,"SQL_queries.r"))
source(file.path(base_dir,"Useful_Functions.r"))



#############################################################
### Generate data of potential credits to offer refinance ###
#############################################################

# Read daily installments and payments
data_sql_daily <- suppressWarnings(dbSendQuery(con, paste("
SELECT application_id, pay_day, 
installment_num
FROM ",db_name,".credits_plan_main",sep="")))
daily <- fetch(data_sql_daily,n=-1)


# Read credit applications 
get_actives_sql <- suppressWarnings(dbSendQuery(con, paste("
SELECT id, status, date, product_id, client_id
FROM ",db_name,".credits_applications 
WHERE status in (4,5)",sep="")))
all_credits <- fetch(get_actives_sql,n=-1)


# Join company ID
company_id <- suppressWarnings(fetch(dbSendQuery(con, 
     gen_get_company_id_query(db_name)), n=-1))
all_credits <- merge(all_credits,company_id,by.x = "product_id",
                     by.y = "id",all.x = TRUE)


# Apply select criteria
select_credits <- subset(all_credits,all_credits$status %in% c(4))
select_credits <- subset(select_credits,select_credits$date>="2020-03-01")


# Remove credits with already an offer 
po_sql_query <- paste(
  "SELECT application_id, created_at, deleted_at, product_id 
   FROM ",db_name,".prior_approval_refinances",sep="")
po <- suppressWarnings(fetch(dbSendQuery(con, po_sql_query), n=-1))
po_raw <- po
daily <- daily[daily$application_id %in% select_credits$id,]
daily <- daily[!(daily$application_id %in% po$application_id),]


# Compute installment ratio
max_installments <- as.data.frame(aggregate(daily$installment_num,
    by=list(daily$application_id),FUN=max))
daily <- merge(daily,max_installments,by.x = "application_id",by.y = "Group.1",
               all.x = TRUE)
names(daily)[ncol(daily)] <- "installments"
daily$installment_ratio <- daily$installment_num / daily$installments


# Get final credit amount
credit_amount_sql <- suppressWarnings(dbSendQuery(con, paste("
SELECT application_id,final_credit_amount, amount as credit_amount
FROM ",db_name,".credits_plan_contract", sep ="")))
credit_amount <- fetch(credit_amount_sql,n=-1)
daily <- merge(daily,credit_amount,by.x = "application_id",
               by.y = "application_id",all.x = TRUE)


# Get eventual taxes
taxes_sql <- suppressWarnings(dbSendQuery(con, paste("
SELECT application_id, amount, paid_amount 
FROM ",db_name,".credits_plan_taxes", sep ="")))
taxes <- fetch(taxes_sql,n=-1)
taxes_raw <- taxes
taxes <- taxes[taxes$application_id %in% daily$application_id,]
taxes_agg <- aggregate(taxes$amount-taxes$paid_amount,
                       by=list(taxes$application_id),FUN=sum)
names(taxes_agg) <- c("application_id","tax_amount")


# Get all payments for each credit
paid <- fetch(suppressWarnings(dbSendQuery(con, paste("
SELECT object_id, amount, pay_date 
FROM ",db_name,".cash_flow
WHERE nomenclature_id in (90,100,101) 
AND deleted_at IS NULL AND object_type=4",sep=""))), n=-1)
paid_raw <- paid
paid <- paid[paid$object_id %in% daily$application_id,]


# Get products periods and amounts of products
products <- fetch(suppressWarnings(dbSendQuery(con, paste("
SELECT product_id, amount 
FROM ",db_name,".products_periods_and_amounts",sep=""))), n=-1)


# Get hitherto payments and ratios
daily <- subset(daily,daily$pay_day<=Sys.time())
daily <- daily[rev(order(daily$pay_day)),]
daily <- daily[order(daily$application_id),]
daily <- daily[!duplicated(daily$application_id),]
paid_agg <- aggregate(paid$amount,by=list(paid$object_id),FUN=sum)
names(paid_agg) <- c("application_id","paid_hitherto")
daily <- merge(daily,paid_agg,by.x = c("application_id"),
               by.y = "application_id",all.x = TRUE)
daily <- merge(daily,taxes_agg,by.x = "application_id",by.y = "application_id",
               all.x = TRUE)
daily$paid_hitherto <- ifelse(is.na(daily$paid_hitherto),0,daily$paid_hitherto)
daily$tax_amount <- ifelse(is.na(daily$tax_amount),0,daily$tax_amount)
daily$left_to_pay <- daily$final_credit_amount + daily$tax_amount - 
  daily$paid_hitherto
daily$paid_ratio <- daily$paid_hitherto / daily$final_credit_amount


# Filter those whose passed installments are lower than 30% and not yet 100%
daily <- subset(daily,daily$installment_ratio>=0.3 & 
                      daily$installment_ratio<1)


# Subset on criteria based on paid ratio (>30%) 
daily <- subset(daily,daily$paid_ratio>=0.3 & daily$paid_ratio<=1)


# Append score
result_df <- daily[!duplicated(daily$application_id),]
result_df <- result_df[,1, drop=FALSE]
result_df$max_amount <- NA
result_df$score_max_amount <- NA
for(i in 1:nrow(result_df)){
  suppressWarnings(tryCatch({
    application_id <- result_df$application_id[i]
    calc <- gen_refinance_fct(con,application_id,product_id)
    result_df$max_amount[i] <- calc[[1]]
    result_df$score_max_amount[i] <- calc[[2]]
    result_df$max_delay[i] <- as.numeric(calc[[3]])
  }, error=function(e){}))
}


# Make final data frame
daily <- merge(daily,result_df,by.x = "application_id",
               by.y = "application_id",all.x = TRUE)


# Select successful offers
daily <- subset(daily,!(is.na(daily$score_max_amount)))


# Rework additional fields
daily <- merge(daily,select_credits[,c("id","date","product_id")],
    by.x = "application_id",by.y = "id",all.x = TRUE)
daily$amount_differential <- daily$max_amount - daily$credit_amount
daily$max_amount <- ifelse(daily$max_amount==-Inf,NA,daily$max_amount)
daily$next_amount_diff <- daily$max_amount - daily$left_to_pay


# Join company ID
daily <- merge(daily,company_id,by.x = "product_id",
    by.y = "id",all.x = TRUE)



#########################################################
### Work on final credit offer amount and write in DB ###
#########################################################

# Choose City Cash credits
daily <- subset(daily,daily$company_id==1)


# Subset max DPD of 200 days
daily <- subset(daily,daily$max_delay<=200)


# Subset based on if next amount is higher than hitherto due amount
daily <- subset(daily,daily$next_amount_diff>0)


# Limit next amount based on score
daily$allowed_step  <- ifelse(daily$score_max_amount %in% c("Good 4"),600,400)
daily$max_amount <- ifelse((daily$max_amount-daily$credit_amount)>
daily$allowed_step,daily$credit_amount + daily$allowed_step,daily$max_amount)


# Get minimum amount to offer
for (i in 1:nrow(daily)){
  local <- products[products$product_id==daily$product_id[i] & 
                    products$amount>=daily$left_to_pay[i],]$amount
  daily$min_amount[i] <- local[which.min(abs(local - daily$left_to_pay[i]))]
}


# Write in Database
daily$ref_application_id <- NA
daily$status <- 1
daily$processed_by <- NA
daily$created_at <- Sys.time()
daily$updated_at <- NA
daily$deleted_at <- NA
daily <- daily[,c("application_id","product_id","min_amount","max_amount",
                  "ref_application_id","status","processed_by",
                  "created_at","updated_at","deleted_at")]


# Replace NAs by NULLs
daily[is.na(daily)] <- "NULL"


# Make result ready for SQL query
string_sql <- gen_sql_string_po_refinance(daily,1)
for(i in 2:nrow(daily)){
  string_sql <- paste(string_sql,gen_sql_string_po_refinance(daily,i),sep=",")
}


# Output real offers
update_prior_query <- paste("INSERT INTO ",db_name,
".prior_approval_refinances VALUES ",
string_sql,";", sep="")


# Write in database
if(nrow(daily)>0){
  suppressMessages(suppressWarnings(dbSendQuery(con,update_prior_query)))
}




###################################
### Updating certain old offers ###
###################################

# Choose credits for updating
po_old <- po_raw
po_old <- subset(po_old,is.na(po_old$deleted_at))


# Remove credits which have izpadejirali
po_old$id <- po_old$application_id
for (i in 1:nrow(po_old)){
  po_old$last_padej[i] <- max(suppressWarnings(fetch(dbSendQuery(con, 
     gen_plan_main_actives_past_query(db_name,po_old[i,])), n=-1))$pay_day)
}
po_to_remove <- subset(po_old,
     po_old$last_padej<=as.Date(substring(Sys.time(),1,10)))
po_old <- po_old[!(po_old$application_id %in% po_to_remove$application_id),]


# Choose credits to update scoring for updating offer 
po_old$time_past <- as.numeric(
  round(difftime(as.Date(substring(Sys.time(),1,10)),
                 as.Date(substring(po_old$created_at,1,10)),units=c("days")),2))
po_old <- subset(po_old,po_old$time_past>0 & po_old$time_past<=360 & 
                 po_old$time_past%%7==0)


# Get company ID and client_id 
po_old <- merge(po_old,company_id,
                by.x = "product_id",by.y = "id",all.x = TRUE)
po_old <- merge(po_old,all_credits[,c("id","client_id")],
                by.x = "application_id",by.y = "id",all.x = TRUE)


# Append score
for(i in 1:nrow(po_old)){
  suppressWarnings(tryCatch({
    application_id <- po_old$application_id[i]
    calc <- gen_refinance_fct(con,application_id,product_id)
    po_old$max_amount[i] <- calc[[1]]
    po_old$score_max_amount[i] <- calc[[2]]
    po_old$max_delay[i] <- as.numeric(calc[[3]])
  }, error=function(e){}))
}


# Change database
po_not_ok <- subset(po_old,is.infinite(po_old$max_amount))
po_ok <- subset(po_old,!(is.infinite(po_old$max_amount)))
po_ok <- subset(po_ok,po_ok$max_delay<=200)
po_ok <- subset(po_ok,po_ok$max_amount>=po_ok$min_amount)
if(nrow(po_to_remove)>0){
  po_not_ok_query <- paste("UPDATE ",db_name,
       ".prior_approval_refinances SET updated_at = '",
       substring(Sys.time(),1,19),"', deleted_at = '",
       substring(Sys.time(),1,19),"'
       WHERE application_id IN",gen_string_po_terminated(po_to_remove), sep="")
  suppressMessages(suppressWarnings(dbSendQuery(con,po_not_ok_query)))
}
if(nrow(po_not_ok)>0){
  po_not_ok_query <- paste("UPDATE ",db_name,
       ".prior_approval_refinances SET updated_at = '",
       substring(Sys.time(),1,19),"', deleted_at = '",
       substring(Sys.time(),1,19),"'
       WHERE application_id IN",gen_string_po_terminated(po_not_ok), sep="")
  suppressMessages(suppressWarnings(dbSendQuery(con,po_not_ok_query)))
}
if(nrow(po_ok)>0){
  po_ok_query <- paste("UPDATE ",db_name,
       ".prior_approval_refinances SET updated_at = '",
       substring(Sys.time(),1,19),"' WHERE application_id IN",
       gen_string_po_terminated(po_ok), sep="")
  suppressMessages(suppressWarnings(dbSendQuery(con,po_ok_query)))
  suppressMessages(suppressWarnings(dbSendQuery(con,
    gen_string_po_refinance(po_ok,po_ok$max_amount,"max_amount",db_name))))
}



#########
## END ##
#########

