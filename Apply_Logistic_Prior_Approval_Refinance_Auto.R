

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
suppressMessages(suppressWarnings(require(gbm)))


# Defines the directory where custom .env file is located
load_dot_env(file = here('.env'))


#######################
### Manual settings ###
#######################

# Defines the directory where the RScript is located
base_dir <- Sys.getenv("SCORING_PATH", unset = "", names = FALSE)


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

# Read credit applications 
get_actives_sql <- paste("
SELECT id, status, date, signed_at, product_id, client_id, third_side_date
FROM ",db_name,".credits_applications 
WHERE status in (4,5)",sep="")
all_credits <- gen_query(con,get_actives_sql)


# Join company ID
company_id <- gen_query(con, 
    gen_get_company_id_query(db_name))
all_credits <- merge(all_credits,company_id,by.x = "product_id",
    by.y = "id",all.x = TRUE)


# Select actives
select <- subset(all_credits,all_credits$status %in% c(4))


# Apply time window criteria
select$time_since <- round(difftime(as.Date(substring(Sys.time(),1,10)),
    select$date,units=c("days")),0)
select <- subset(select,select$time_since<=200)


# Remove credits with already an offer 
po_sql_query <- paste(
  "SELECT application_id, created_at, deleted_at, product_id, min_amount
   FROM ",db_name,".prior_approval_refinances",sep="")
po <- gen_query(con, po_sql_query)
po_raw <- po
select <- select[!(select$id %in% po$application_id),]


# Read daily installments and payments
data_sql_daily <- paste("
SELECT application_id, installment_num, discount_amount
FROM ",db_name,".credits_plan_main")
daily <- gen_query(con,data_sql_daily)
daily <- daily[daily$application_id %in% select$id,]
daily_raw <- daily


# Get installment number
nb_installments <- aggregate(daily$installment_num,
  by=list(daily$application_id),FUN=max)
daily <- merge(daily,nb_installments,by.x = "application_id",by.y = "Group.1",
  all.x = TRUE)
names(daily)[ncol(daily)] <- "nb_installments"
daily <- daily[!duplicated(daily$application_id),]


# Compute paids installment ratio
paid_install_sql <- paste("
SELECT application_id, COUNT(application_id) as installments_paid
FROM ",db_name,".credits_plan_main
WHERE payed_at IS NOT NULL AND pay_day<= '",substring(Sys.time(),1,10),
"' GROUP BY application_id;
", sep ="")
paid_install <- gen_query(con,paid_install_sql)
daily <- merge(daily,paid_install,by.x = "application_id",
   by.y = "application_id",all.x = TRUE)
daily$installments_paid <- as.numeric(daily$installments_paid)
daily$installments_paid <- ifelse(is.na(daily$installments_paid),0,
   daily$installments_paid)
daily$installment_ratio <- round(
  daily$installments_paid / daily$nb_installments,2)
select <- merge(select,
  daily[,c("application_id","installment_ratio")],by.x = "id",
  by.y = "application_id")


# Filter those whose passed installments are lower than 30% and not yet 100%
select <- subset(select,select$installment_ratio>=0.3 & 
  select$installment_ratio<=1)


# Get final credit amount
id_list <- paste(select$id,collapse=",")
credit_amount_sql <- paste("
SELECT application_id,final_credit_amount, amount as credit_amount
FROM ",db_name,".credits_plan_contract WHERE application_id IN (",id_list ,")",
  sep ="")
credit_amount <- gen_query(con,credit_amount_sql)
select <- merge(select,credit_amount,by.x = "id",
   by.y = "application_id",all.x = TRUE)


# Get eventual taxes
taxes_sql <- paste("
SELECT application_id, amount, paid_amount 
FROM ",db_name,".credits_plan_taxes", sep ="")
taxes <- gen_query(con,taxes_sql)
taxes_raw <- taxes
taxes <- taxes[taxes$application_id %in% daily$application_id,]
taxes_agg <- aggregate(taxes$amount,
                       by=list(taxes$application_id),FUN=sum)
names(taxes_agg) <- c("application_id","tax_amount")


# Get discounts
discount_agg <- aggregate(daily_raw$discount_amount,
      list(daily_raw$application_id),FUN=sum)
names(discount_agg) <- c("application_id","discount_amount")


# Read balance 
all_apps <- select$id[1]
if(nrow(select)>1){
  for(i in 2:nrow(select)){
    all_apps <- paste(all_apps,select$id[i],sep=",")}
}
balance_sql <- paste("SELECT id, application_id, pay_day
FROM ",db_name,".credits_plan_daily 
WHERE application_id IN(",all_apps,")",sep="")
balance <- gen_query(con,balance_sql)
balance$today <- substring(Sys.time(),1,10)
balance <- subset(balance,balance$pay_day<=balance$today)
max_id <- as.data.frame(aggregate(balance$id,by=list(balance$application_id),
  FUN=max))
names(max_id) <- c("id","max_id")


# Read daily claim
all_max_id <- max_id$max_id[1]
if(nrow(max_id)>1){
  for(i in 2:nrow(max_id)){
    all_max_id <- paste(all_max_id,max_id$max_id[i],sep=",")}
}
claims_sql <- paste("SELECT daily_id, taxes, penalty, interest, principal
FROM ",db_name,".credits_plan_balance_claims  
WHERE daily_id IN(",all_max_id,")",sep="")
claims <- gen_query(con,claims_sql)
claims$claims <- claims$taxes + claims$penalty + claims$principal + 
  claims$interest
max_id <- merge(max_id,claims[,c("daily_id","claims")],
   by.x = "max_id",by.y = "daily_id",all.x = TRUE)


# Remove balance after
balance_after_sql <- paste("SELECT daily_id,balance_after
FROM ",db_name,".credits_plan_balance_changes   
WHERE daily_id IN(",all_max_id ,")",sep="")
balance_after <- gen_query(con,balance_after_sql)
max_id <- merge(max_id,balance_after,
  by.x = "max_id",by.y = "daily_id",all.x = TRUE)
max_id$balance_after <- ifelse(is.na(max_id$balance_after),0,
  max_id$balance_after)
select <- merge(select,max_id[,c("id","claims","balance_after")],
  by.x = "id",by.y = "id",all.x = TRUE)


# Get all payments for each credit
paid <- gen_query(con, paste("
SELECT object_id, amount, pay_date 
FROM ",db_name,".cash_flow
WHERE nomenclature_id in (90,100,101) 
AND deleted_at IS NULL AND object_type=4",sep=""))
paid_raw <- paid
paid <- paid[paid$object_id %in% daily$application_id,]


# Get products periods and amounts of products
products <- gen_query(con, paste("
SELECT product_id, amount 
FROM ",db_name,".products_periods_and_amounts",sep=""))


# Get hitherto payments and ratios
paid_agg <- aggregate(paid$amount,by=list(paid$object_id),FUN=sum)
names(paid_agg) <- c("application_id","paid_hitherto")
select <- merge(select ,paid_agg,by.x = "id",
  by.y = "application_id",all.x = TRUE)
select <- merge(select,taxes_agg,by.x = "id",
  by.y = "application_id", all.x = TRUE)
select <- merge(select,discount_agg,by.x = "id",
  by.y = "application_id", all.x = TRUE)
select$paid_hitherto <- ifelse(is.na(select$paid_hitherto),0,
  select$paid_hitherto)
select$tax_amount <- ifelse(is.na(select$tax_amount),0,
 select$tax_amount)
select$discount_amount <- ifelse(is.na(select$discount_amount),0,
 select$discount_amount)
select$left_to_pay <- ifelse(select$company_id==2,
  select$claims - select$balance_after, select$final_credit_amount + 
  select$tax_amount - select$paid_hitherto - select$discount_amount)


# Check if client has still VIP status 
names_b4 <- names(select)
is_vip_query <- paste(
  "SELECT client_id, brand_id, is_vip
   FROM ",db_name,".client_brand",sep="")
is_vip <- gen_query(con,is_vip_query)
select <- merge(select,is_vip,
   by.x = c("client_id","company_id"),
   by.y = c("client_id","brand_id"),all.x = TRUE)
select$is_vip <- ifelse(is.na(select$is_vip),0,select$is_vip)
select <- select[,c(names_b4,"is_vip")]


# Check if client has active finstart - no city cash
all_credits$get_brand_company <- paste0(all_credits$big_company_id,
    all_credits$company_id)
select <- select[!(select$client_id %in% 
    subset(all_credits,all_credits$get_brand_company %in% c(44,45,54) & 
    all_credits$status==4)$client_id),]


# Remove Flex credits and other Ipoteki
select <- subset(select,!(select$product_id %in%
  c(25,36,41,43,50,28,26,37,42,44,49,27,55,58,57,56,22,3,53,54,51,65,12,13,
    62,63,61,64,59,60,78,79,80,81,67,68,89,90,93,94)))



#####################
### Score credits ###
#####################

# Append score
if(nrow(select)>0){
result_df <- select[,c("id","client_id"), drop=FALSE]
result_df$max_amount <- NA
result_df$score_max_amount <- NA
result_df$product_id <- NA
result_df$max_installment <- NA
result_df$days_delay <- NA
result_df$office_id <- NA
result_df$third_side <- NA
for(i in 1:nrow(result_df)){
  suppressWarnings(tryCatch({
    application_id <- result_df$id[i]
    if(select$product_id[i]==8 & select$is_vip[i]==0){
      product_id <- 5
    } else {
      product_id <- NA
    }
    calc <- gen_refinance_fct(con,application_id,product_id,db_name)
    result_df$max_amount[i] <- calc[[1]]
    result_df$score_max_amount[i] <- calc[[2]]
    result_df$max_delay[i] <- as.numeric(calc[[3]])
    result_df$product_id[i] <- as.numeric(calc[[4]])
    result_df$max_installment[i] <- as.numeric(calc[[5]])
    result_df$days_delay[i] <- as.numeric(calc[[6]])
    result_df$office_id[i] <- as.numeric(calc[[7]])
    result_df$third_side[i] <- as.numeric(calc[[8]])
  }, error=function(e){}))
}


# Make final data frame
select <- select[,-which(names(select) %in% c("product_id","client_id"))]
select <- merge(select,result_df,by.x = "id",by.y = "id",all.x = TRUE)



#############################
### Apply filter criteria ###
#############################

# Select successful offers
select <- subset(select,!(is.na(select$score_max_amount)))
select <- select[!duplicated(select$id),]


# Subset based on current DPD
select <- subset(select,select$days_delay<=90)


# Subset based on not real offices
select$ok_office <- ifelse(flag_real_office(select$office_id)==1,1,0)
select <- subset(select,select$ok_office==1)


# Subset based on if on third side 
select <- subset(select,select$third_side==0)


# Get number of terminated credits per client
all_credits$nb <- 1
agg_term_citycash <- aggregate(all_credits$nb[
      all_credits$status==5 & all_credits$company_id==1],
    by=list(all_credits$client_id[
      all_credits$status==5 & all_credits$company_id==1]),FUN=sum)
agg_term_credirect <- aggregate(all_credits$nb[
  all_credits$status==5 & all_credits$company_id==2],
  by=list(all_credits$client_id[
    all_credits$status==5 & all_credits$company_id==2]),FUN=sum)
select <- merge(select,agg_term_citycash,
               by.x = "client_id",by.y = "Group.1",all.x = TRUE)
select <- merge(select,agg_term_credirect,
               by.x = "client_id",by.y = "Group.1",all.x = TRUE)
names(select)[ncol(select)-1] <- "nb_term_citycash"
names(select)[ncol(select)] <- "nb_term_credirect"
select$nb_term_citycash <- ifelse(is.na(select$nb_term_citycash),0,
                                 select$nb_term_citycash)
select$nb_term_credirect <- ifelse(is.na(select$nb_term_credirect),0,
                                  select$nb_term_credirect)


# Check if first is refinanced
con <- dbConnect(MySQL(), user=db_username,password=db_password,dbname=db_name,
    host=db_host, port = db_port)
select <- gen_if_first_was_ref(db_name,select)


# Refilter according to aformentioned 2 criteria
select$nb_criteria <- ifelse(select$company_id==1,select$nb_term_citycash,
   select$nb_term_credirect)
select$filter_criteria <- ifelse(select$company_id==2,
  (ifelse(select$nb_criteria==0,0.5,
   ifelse(select$ref_first==1 & select$nb_term_credirect<=2,0.5,
   ifelse(select$score_max_amount %in% c("Good 4"), 0.1,
   ifelse(select$score_max_amount %in% c("Good 3"), 0.1,
   ifelse(select$score_max_amount %in% c("Good 2"), 0.1,
   ifelse(select$score_max_amount %in% c("Good 1"), 0.2,0.3))))))),                               
  (ifelse(select$nb_criteria==0,0.5,
   ifelse(select$score_max_amount %in% c("Good 4"), 0.3,
   ifelse(select$score_max_amount %in% c("Good 3"), 0.4,
   ifelse(select$score_max_amount %in% c("Good 2"), 0.45,0.5))))))
select <- subset(select,select$installment_ratio>=select$filter_criteria)


# Rework additional fields
select$amount_differential <- select$max_amount - select$credit_amount
select$max_amount <- ifelse(select$max_amount==-Inf,NA,select$max_amount)
select$next_amount_diff <- select$max_amount - select$left_to_pay


# Subset based on if next amount is higher than hitherto due amount
select <- 
  rbind(subset(select,select$next_amount_diff>=0 & select$company_id!=2),
        subset(select,select$next_amount_diff>=50 & select$company_id==2))



#########################################################
### Work on final credit offer amount and write in DB ###
#########################################################

# Get minimum amount to offer
if(nrow(select)>0){
for (i in 1:nrow(select)){
  local <- products[products$product_id==select$product_id[i] & 
                    products$amount>=select$left_to_pay[i],]$amount
  select$min_amount[i] <- local[which.min(abs(local - select$left_to_pay[i]))]
}


# Set all credirect offers to one and only product
select$product_id <- ifelse(is.na(select$product_id),select$product_id,
                     ifelse(select$product_id==9,48,select$product_id))


# Write in Database
select$ref_application_id <- NA
select$status <- 1
select$processed_by <- NA
select$created_at <- Sys.time()
select$updated_at <- NA
select$deleted_at <- NA
select$max_amount_updated <- NA
select$max_installment_updated <- NA
select <- select[,c("id","product_id","min_amount","max_amount",
    "max_installment","max_amount_updated","max_installment_updated",
    "ref_application_id","status","processed_by",
    "created_at","updated_at","deleted_at")]
names(select)[1] <- "application_id"


# Replace NAs by NULLs
select[is.na(select)] <- "NULL"


# Make final check 
current <- gen_query(con, paste("
SELECT application_id, max_amount 
FROM ",db_name,".prior_approval_refinances
WHERE deleted_at IS NULL",sep=""))
select <- select[!(select$application_id %in% current$application_id),]


# Make result ready for SQL query
string_sql <- gen_sql_string_po_refinance(select,1)
if(nrow(select)>1){
for(i in 2:nrow(select)){
  string_sql <- paste(string_sql,gen_sql_string_po_refinance(select,i),sep=",")
}}


# Output real offers
update_prior_query <- paste("INSERT INTO ",db_name,
".prior_approval_refinances VALUES ",
string_sql,";", sep="")


# Write in database
  suppressMessages(suppressWarnings(dbSendQuery(con,update_prior_query)))
}}



###################################
### Updating certain old offers ###
###################################

# Choose credits for updating
po_old <- po_raw
po_old <- subset(po_old,is.na(po_old$deleted_at))
po_old$time_past <- as.numeric(
  round(difftime(as.Date(substring(Sys.time(),1,10)),
  as.Date(substring(po_old$created_at,1,10)),units=c("days")),2))
po_old <- subset(po_old,po_old$time_past>0 & po_old$time_past<=360 &
  po_old$time_past%%30==0 & is.na(po_old$deleted_at))


# Remove credits which have izpadejirali
po_old$id <- po_old$application_id
for (i in 1:nrow(po_old)){
  po_old$last_padej[i] <- max(
    gen_query(con,gen_plan_main_actives_past_query(db_name,
     po_old$application_id[i]))$pay_day)
}
po_old$last_padej <- as.Date(po_old$last_padej)
po_to_remove <- subset(po_old,
      po_old$last_padej<=as.Date(substring(Sys.time(),1,10)))
po_old <- po_old[!(po_old$application_id %in% po_to_remove$application_id),]


# Append score
po_old$max_amount <- NA
po_old$score_max_amount <- NA
po_old$max_installment <- NA
po_old$days_delay <- NA
po_old$office_id <- NA
po_old$third_side <- NA
for(i in 1:nrow(po_old)){
  suppressWarnings(tryCatch({
    application_id <- po_old$application_id[i]
    calc <- gen_refinance_fct(con,application_id,product_id,db_name)
    po_old$max_amount[i] <- calc[[1]]
    po_old$score_max_amount[i] <- calc[[2]]
    po_old$max_delay[i] <- as.numeric(calc[[3]])
    po_old$max_installment[i] <- as.numeric(calc[[5]])
    po_old$days_delay[i] <- as.numeric(calc[[6]])
    po_old$office_id[i] <- as.numeric(calc[[7]])
    po_old$third_side[i] <- as.numeric(calc[[8]])
  }, error=function(e){}))
}
po_old$ok_office <- ifelse(flag_real_office(po_old$office_id)==1,1,0)


# Change database
po_not_ok <- subset(po_old,is.infinite(po_old$max_amount) | 
  po_old$max_amount<po_old$min_amount | po_old$days_delay>300 |
  po_old$ok_office==0 | po_old$third_side==1)
po_ok <- po_old[!(po_old$application_id %in% po_not_ok$application_id),]

if(nrow(po_to_remove)>0){
  po_to_remove$max_amount <- -999
  po_not_ok_query <- paste("UPDATE ",db_name,
      ".prior_approval_refinances SET updated_at = '",
      substring(Sys.time(),1,19),"' WHERE application_id IN",
      gen_string_po_terminated(po_to_remove), sep="")
  suppressMessages(suppressWarnings(dbSendQuery(con,po_not_ok_query)))
  suppressMessages(suppressWarnings(dbSendQuery(con,
      gen_string_delete_po_refinance(po_to_remove,po_to_remove$max_amount,
      "max_amount_updated",db_name))))
}

if(nrow(po_not_ok)>0){
  po_not_ok$max_amount <- -999
  po_not_ok_query <- paste("UPDATE ",db_name,
      ".prior_approval_refinances SET updated_at = '",
      substring(Sys.time(),1,19),"' WHERE application_id IN",
      gen_string_po_terminated(po_not_ok), sep="")
  suppressMessages(suppressWarnings(dbSendQuery(con,po_not_ok_query)))
  suppressMessages(suppressWarnings(dbSendQuery(con,
      gen_string_delete_po_refinance (po_not_ok,po_not_ok$max_amount,
      "max_amount_updated",db_name))))
}

if(nrow(po_ok)>0){
  po_ok_query <- paste("UPDATE ",db_name,
    ".prior_approval_refinances SET updated_at = '",
    substring(Sys.time(),1,19),"' WHERE application_id IN",
    gen_string_po_terminated(po_ok), sep="")
  suppressMessages(suppressWarnings(dbSendQuery(con,po_ok_query)))
  suppressMessages(suppressWarnings(dbSendQuery(con,
    gen_string_delete_po_refinance (po_ok,po_ok$max_amount,"max_amount_updated",
    db_name))))
  suppressMessages(suppressWarnings(dbSendQuery(con,
    gen_string_delete_po_refinance (po_ok,po_ok$max_installment,
    "max_installment_updated",db_name))))
}


if(substring(Sys.time(),9,10) %in% c("01")){
  
  po_sql_query <- paste(
    "SELECT application_id, created_at, deleted_at, product_id, min_amount,
    max_amount, max_amount_updated, max_installment_updated
    FROM ",db_name,".prior_approval_refinances",sep="")
  po_all <- gen_query(con, po_sql_query)
  po_all <- subset(po_all,is.na(po_all$deleted_at))
  
  po_all_not_ok <- subset(po_all,po_all$max_amount_updated==-999)
  if(nrow(po_all_not_ok)>0){
    po_all_not_ok_query <- paste("UPDATE ",db_name,
      ".prior_approval_refinances SET status = 4, updated_at = '",
      substring(Sys.time(),1,19),"', deleted_at = '",
      paste(substring(Sys.time(),1,10),"04:00:00",sep=),"'
      WHERE application_id IN ",gen_string_po_refinance(po_all_not_ok), sep="")
    suppressMessages(suppressWarnings(dbSendQuery(con,po_all_not_ok_query)))
  }

  po_all <- subset(po_all,po_all$max_amount_updated!=-999)
  if(nrow(po_all)>0){
    po_all[is.na(po_all)] <- "NULL"
    po_change_query <- paste("UPDATE ",db_name,
      ".prior_approval_refinances SET updated_at = '",
      substring(Sys.time(),1,19),"' WHERE application_id IN",
      gen_string_po_refinance(po_all), sep="")
    suppressMessages(suppressWarnings(dbSendQuery(con,po_change_query)))
    suppressMessages(suppressWarnings(dbSendQuery(con,
      gen_string_delete_po_refinance(po_all,po_all$max_amount_updated,
      "max_amount",db_name))))
    suppressMessages(suppressWarnings(dbSendQuery(con,
      gen_string_delete_po_refinance(po_all,po_all$max_installment_updated,
      "max_installment",db_name))))
  }
}



#################################################
### Check those who didn't amount to a credit ###
#################################################

# Reload offers which were deleted for nothing (and are relevant)
po_sql_query <- paste(
"SELECT application_id, product_id, created_at,deleted_at
FROM ",db_name,".prior_approval_refinances
WHERE LEFT(deleted_at,10) = '",
(as.Date(Sys.time())-3),"'",sep="")
po_reload <- gen_query(con,po_sql_query)
po_reload <- subset(po_reload,
 !(substring(po_reload$deleted_at,12,19)=="04:00:00"))
po_reload <- merge(po_reload,company_id[,c("id","company_id")],
 by.x = "product_id",by.y = "id",all.x = TRUE)
po_reload$difftime <- round(difftime(po_reload$deleted_at,po_reload$created_at,
 units=c("days")),2)
po_reload <- subset(po_reload,po_reload$difftime<90)

# Subset if offer but didn't amount to nothing
po_reload <- merge(po_reload,all_credits[,c("id","client_id","status")],
 by.x = "application_id",by.y = "id",all.x = TRUE)
po_reload <- subset(po_reload,po_reload$status==4)
po_reload <- merge(po_reload,
 all_credits[,c("client_id","signed_at","company_id")],
 by.x = "client_id",by.y = "client_id",all.x = TRUE)
po_reload$difftime2 <- round(difftime(po_reload$signed_at,
 po_reload$deleted_at,units=c("days")),2)
has_credit_after <- subset(po_reload,
 po_reload$difftime2>=-0.3 & po_reload$company_id.x==po_reload$company_id.y)
po_reload <- po_reload[!(po_reload$client_id %in% has_credit_after$client_id),]
po_reload <- po_reload[!duplicated(po_reload$client_id),]

# Check if client has currently odobren
odobren_sql <- paste(
"SELECT client_id, created_at, sub_status, product_id
FROM ",db_name,".credits_applications
WHERE status = 3 AND sub_status <> 114",sep="")
odobren <- gen_query(con,odobren_sql)
odobren <- merge(odobren,company_id,by.x = "product_id",by.y = "id",
  all.x = TRUE)
po_reload <- merge(po_reload[,c("client_id","application_id",
  "created_at","company_id.x")],
  odobren[,c("client_id","company_id","created_at")],
  by.x = c("client_id","company_id.x"),by.y = c("client_id","company_id"),
  all.x = TRUE)
po_reload <- subset(po_reload,is.na(po_reload$created_at.y))

# Write in database
if(nrow(po_reload)>0){
  po_reload_query <- paste("UPDATE ",db_name,
  ".prior_approval_refinances SET updated_at = '",
  substring(Sys.time(),1,19),"', deleted_at = NULL
  WHERE application_id IN",gen_string_po_refinance(po_reload),sep="")
  suppressMessages(suppressWarnings(dbSendQuery(con,po_reload_query)))
}



#######################################################
### Check for special cases and deleted immediately ###
#######################################################

# Read special cases (deceased) 
get_special_sql <- paste("
SELECT id
FROM ",db_name,".clients
WHERE dead_at IS NOT NULL",sep="")
special <- gen_query(con,get_special_sql)

# Read special cases marketing
get_marketing_sql <- paste("
SELECT client_id, brand_id, gdpr_marketing_messages 
FROM ",db_name,".client_brand WHERE gdpr_marketing_messages=1",sep="")
marketing <- gen_query(con,get_marketing_sql)

# Remove special cases if has offer
po_special_sql_query <- paste(
"SELECT a.application_id, a.created_at, a.product_id, a.max_amount, b.brand_id
FROM ",db_name,".prior_approval_refinances a
LEFT JOIN citycash.products b
ON a.product_id = b.id
WHERE a.deleted_at IS NULL",sep="")
po_special <- gen_query(con,po_special_sql_query)
po_special <- merge(po_special,all_credits[,c("id","client_id",
  "third_side_date")],by.x = "application_id",by.y = "id",all.x = TRUE)
po_special <- merge(po_special,marketing,by.x = c("client_id","brand_id"),
   by.y = c("client_id","brand_id"),all.x = TRUE)
marketing <- as.data.frame(subset(po_special,
  po_special$gdpr_marketing_messages==1)[,c("client_id")])
names(marketing) <- c("id")
po_special_raw <- po_special
po_special <- po_special[po_special$client_id %in% rbind(special,marketing)$id,]
po_special_3rd_side <- subset(po_special_raw,
   !is.na(po_special_raw$third_side_date))
po_special_3rd_side <- po_special_3rd_side[,c("application_id","created_at",
   "product_id","max_amount","client_id")]

# Remove Credirect if amount is less than due 
po_special_credirect <- subset(po_special_raw,
  po_special_raw$product_id %in% c(48,77,95,82))

all_apps <- po_special_credirect$application_id[1]
if(nrow(po_special_credirect)>1){
  for(i in 2:nrow(po_special_credirect)){
    all_apps <- paste(all_apps,po_special_credirect$application_id[i],sep=",")}
}
balance_sql <- paste("SELECT id, application_id, pay_day
FROM ",db_name,".credits_plan_daily 
WHERE application_id IN(",all_apps,")",sep="")
balance <- gen_query(con,balance_sql)
balance$today <- substring(Sys.time(),1,10)
balance <- subset(balance,balance$pay_day<=balance$today)
max_id <- as.data.frame(aggregate(balance$id,by=list(balance$application_id),
                                  FUN=max))
names(max_id) <- c("id","max_id")

# Read daily claim
all_max_id <- max_id$max_id[1]
if(nrow(max_id)>1){
  for(i in 2:nrow(max_id)){
    all_max_id <- paste(all_max_id,max_id$max_id[i],sep=",")}
}
claims_sql <- paste("SELECT daily_id, taxes, penalty, interest, principal
FROM ",db_name,".credits_plan_balance_claims  
WHERE daily_id IN(",all_max_id,")",sep="")
claims <- gen_query(con,claims_sql)
claims$claims <- claims$taxes + claims$penalty + claims$principal + 
  claims$interest
max_id <- merge(max_id,claims[,c("daily_id","claims")],
                by.x = "max_id",by.y = "daily_id",all.x = TRUE)

# Remove balance after
balance_after_sql <- paste("SELECT daily_id,balance_after
FROM ",db_name,".credits_plan_balance_changes   
WHERE daily_id IN(",all_max_id ,")",sep="")
balance_after <- gen_query(con,balance_after_sql)
max_id <- merge(max_id,balance_after,
  by.x = "max_id",by.y = "daily_id",all.x = TRUE)
max_id$balance_after <- ifelse(is.na(max_id$balance_after),0,
  max_id$balance_after)
po_special_credirect <- merge(po_special_credirect,
  max_id[,c("id","claims","balance_after")],by.x = "application_id",
  by.y = "id",all.x = TRUE)
po_special_credirect$left_to_pay <- po_special_credirect$claims - 
  po_special_credirect$balance_after
po_special_credirect <- subset(po_special_credirect,
  po_special_credirect$max_amount<po_special_credirect$left_to_pay)
po_special <- rbind(po_special[,c("application_id","created_at",
      "product_id","max_amount","client_id")],po_special_3rd_side,
     po_special_credirect[,c("application_id","created_at",
      "product_id","max_amount","client_id")])
po_special <- po_special[!duplicated(po_special$application_id),]

if(nrow(po_special)>0){
  po_special_query <- paste("UPDATE ",db_name,
     ".prior_approval_refinances SET status = 4, updated_at = '",
     substring(Sys.time(),1,19),"', deleted_at = '",
     paste(substring(Sys.time(),1,10),"04:00:00",sep=),"'
     WHERE application_id IN ",gen_string_po_refinance(po_special), sep="")
  suppressMessages(suppressWarnings(dbSendQuery(con,po_special_query)))
}



######################################
# Recorrect for Credirect brand City #
######################################

po_correct_ref_credirect <- paste("UPDATE ",db_name,
".prior_approval_refinances SET product_id=95, updated_at = '",
substring(Sys.time(),1,19),"' WHERE product_id IN (9,48) 
AND deleted_at IS NULL", sep="")
suppressMessages(suppressWarnings(dbSendQuery(con,po_correct_ref_credirect)))



##########################################################################
### Remove those who were flagged as blacklisted after offer creation  ###
##########################################################################

# Get all clients which are to be removed
po_risk <- gen_query(con,paste(
"SELECT a.application_id, a.created_at, b.client_id
FROM ",db_name,".prior_approval_refinances a LEFT JOIN 
",db_name,".credits_applications b ON a.application_id=b.id 
WHERE a.deleted_at IS NULL",sep=""))
risky1 <- gen_query(con,paste(
  "SELECT id, judge_us_at, dead_at FROM ",db_name,".clients 
WHERE judge_us_at IS NOT NULL OR dead_at IS NOT NULL",sep=""))
risky2 <- gen_query(con,paste(
  "SELECT a.egn, b.id FROM ",db_name,".clients_risk a 
LEFT JOIN clients b ON a.egn = b.egn",sep=""))
risky2 <- subset(risky2,!is.na(risky2$id))
risky3 <- gen_query(con,paste(
  "SELECT client_id, judicial_date FROM ",db_name,".credits_applications  
WHERE judicial_date IS NOT NULL",sep=""))

po_risk <- rbind(po_risk[po_risk$clientid %in% risky1$id,],
  po_risk[po_risk$clientid %in% risky2$id,],
  po_risk[po_risk$clientid %in% risky1$client_id,])

# Remove offers
if(nrow(po_risk)>0){
  
  po_risk <- po_risk[!duplicated(po_risk$id),]
  
  po_change_query <- paste("UPDATE ",db_name,
     ".prior_approval_refinances SET deleted_at = '",
     substring(Sys.time(),1,19),"', updated_at = '",
     substring(Sys.time(),1,19),"' WHERE application_id IN ",
     gen_string_po_refinance(po_risk), sep="")
  suppressMessages(suppressWarnings(dbSendQuery(con,po_change_query)))
}



#########
## END ##
#########

