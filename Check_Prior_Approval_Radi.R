

################################################################################
#         Script for generating  daily offers for terminated credits           #
#      Apply Logistic Regression on all products (CityCash and Credirect)      #
#                          Version 1.0 (2021/07/11)                            #
################################################################################



########################
### Initial settings ###
########################

# Libraries
suppressMessages(suppressWarnings(library(RMariaDB)))
suppressMessages(suppressWarnings(library(DBI)))
suppressMessages(suppressWarnings(library(Rcpp)))
suppressMessages(suppressWarnings(library(here)))
suppressMessages(suppressWarnings(library(dotenv)))
suppressMessages(suppressWarnings(require("reshape")))
suppressMessages(suppressWarnings(library(openxlsx)))
suppressMessages(suppressWarnings(require(gbm)))
suppressMessages(suppressWarnings(library(dplyr)))

# Database
db_name <- "citycash"
con <- dbConnect(RMariaDB::MariaDB(),dbname = "citycash",host ="192.168.2.110",
   port = 3306,user = "userro1",password = "DHng_2pg5zdL0yI9x@")
# db_user <- "root"
# db_password <- "123456"
# db_name <- "citycash_db"
# db_host <- "127.0.0.1"
# df_port <- 3306
# con <- dbConnect(MySQL(), user=db_user, password=db_password, 
#                  dbname=db_name, host=db_host, port = df_port)


# Define work directory
main_dir <- "C:\\Projects\\Apply_Scoring\\"


# Read argument of ID
args <- commandArgs(trailingOnly = TRUE)
#application_id <- args[1]
application_id <- 1694644


# Load other r files
source(paste(main_dir,"Apply_Models\\Terminated_Radi.r",sep=""))
source(paste(main_dir,"Apply_Models\\SQL_queries.r", sep=""))
source(paste(main_dir,"Apply_Models\\Useful_Functions_Radi.r", sep=""))


# Define product id
product_id <- NA



###################################################
### Generate data of potential credits to offer ###
###################################################

# Read credit applications 
get_actives_sql <- paste("
SELECT id, status, date, signed_at, product_id, client_id, 
deactivated_at, sub_status, office_id, consultant_id
FROM ",db_name,".credits_applications 
WHERE id= ",application_id,sep="")
all_credit <- gen_query(con,get_actives_sql)


# Remove certain offices
all_credit <- subset(all_credit,is.na(all_credit$office_id) |
   !(all_credit$office_id %in% c(6,19,20,37,38,39,40,60,62,65,101,126,141,146,
   147,148,158,173,174,176,177,217)))
if(nrow(all_credit)==0){
  quit()
}


# Get company ID
company_id <- gen_query(con, 
    gen_get_company_id_query(db_name))
all_credit <- merge(all_credit,company_id,by.x = "product_id",
    by.y = "id",all.x = TRUE)


# Get last credit amount
credit_amount_sql <- paste("
SELECT application_id , amount as credit_amount
FROM ",db_name,".credits_plan_contract 
WHERE application_id=",application_id,sep ="")
credit_amount <- gen_query(con,credit_amount_sql)
all_credit$credit_amount <- credit_amount$credit_amount


# Get flagged GDPR marketing campaigns
flag_gdpr <- gen_query(con,
  gen_flag_gdpr(db_name,
    all_credit$client_id,all_credit$company_id))$gdpr_marketing_messages
flag_gdpr <- ifelse(identical(flag_gdpr,integer(0)),0,
  ifelse(is.na(flag_gdpr),0,flag_gdpr))


# Get max DPD on current credit
max_dpd <- gen_query(con,
  gen_plan_main_select_query(db_name,application_id))$max_delay



#####################################################
### Apply selection criteria for credits to offer ###
#####################################################

# Subset based on sub_status
if(!(all_credit$sub_status %in% c(123,128))){
  quit()
}


# Subset based on flagged GDPR
if(!is.na(flag_gdpr) & flag_gdpr==1){
  quit()
}


# Select based on product_id
if(all_credit$product_id %in% c(12,13,59:65,53,54,51,22,69,70,91,92,93,94)){
  quit()
}


# Remove according to criteria of passed installments
get_main_sql <- paste(
"SELECT pay_day
FROM ",db_name,".credits_plan_main 
WHERE application_id = ",application_id, sep="")
plan_main <- gen_query(con,get_main_sql)
all_credit$tot_installments <- nrow(plan_main)
all_credit$passed_installments <- length(plan_main[
  plan_main$pay_day<=Sys.time(),])
get_period_sql <- paste(
"SELECT period 
FROM ",db_name,".products 
WHERE id= ",all_credit$product_id,sep="")
all_credit$period <- gen_query(con,get_period_sql)$period
flag_credit_next_salary <- ifelse(all_credit$product_id %in% 
    c(25:28,36,37,41:44,49,50,55:58,78:81),1,0)
if((flag_credit_next_salary==1 & all_credit$passed_installments==0) |
   (flag_credit_next_salary!=1 & all_credit$period==3 & 
    all_credit$passed_installments<2 & all_credit$tot_installments<4 & 
    all_credit$company_id!=5) |
   (flag_credit_next_salary!=1 & all_credit$passed_installments<4 & 
    all_credit$tot_installments>=4 & all_credit$company_id!=5)) {
  quit()
}


# Flag is terminated before maturing
flag_limit_offer <- 0


# Remove if more than 1 "varnat"
get_status_sql <- paste("
SELECT id, status, sub_status, product_id
FROM ",db_name,".credits_applications 
WHERE client_id= ",all_credit$client_id,sep="")
all_credit_status <- gen_query(con,get_status_sql)
all_credit_varnat <- subset(all_credit_status,all_credit_status$sub_status==122)
if(nrow(all_credit_varnat)>1){
  quit()
}


# Remove credits with already an offer of corresponding company
po_sql_query <- paste(
  "SELECT id, client_id, product_id, application_id, created_at, credit_amount,
  installment_amount,deleted_at,updated_at
  FROM ",db_name,".clients_prior_approval_applications WHERE client_id = ",
  all_credit$client_id,sep="")
po <- gen_query(con, po_sql_query)
po_raw <- po
po <- merge(po,company_id,by.x = "product_id",by.y = "id",all.x = TRUE)
po <- subset(po,po$company_id==all_credit$company_id & is.na(po$deleted_at))
if(nrow(po)>0){
  quit()
}


# Remove those who have active credit of corresponding company
all_credit_status <- merge(all_credit_status,company_id,
   by.x = "product_id",by.y = "id",all.x = TRUE)
all_credit_status <- subset(all_credit_status,
  !(all_credit_status$product_id %in% c(69,70,91,92)))
all_credit_active <- subset(all_credit_status,
   all_credit_status$status==4 & 
   all_credit_status$company_id==all_credit$company_id)
if(nrow(all_credit_active)>0){
  quit()
}


# Join if VIP
names_b4 <- names(all_credit)
is_vip_query <- paste(
  "SELECT client_id, brand_id, is_vip
   FROM ",db_name,".client_brand WHERE client_id=",all_credit$client_id,sep="")
is_vip <- gen_query(con,is_vip_query)
all_credit <- merge(all_credit,is_vip,
  by.x = c("client_id","company_id"),
  by.y = c("client_id","brand_id"), all.x = TRUE)
all_credit$is_vip <- ifelse(is.na(all_credit$is_vip),0,all_credit$is_vip)
all_credit <- all_credit[,c(names_b4,"is_vip")]


# Make flag to know if offer amount should be limited
all_credit$limit_offer <-
  ifelse(all_credit$company_id==5 & 
         all_credit$tot_installments>all_credit$passed_installments,1,0)



#####################
### Compute score ###
#####################

if(nrow(all_credit)>0){

# Compute and append score
all_credit <- all_credit[!duplicated(all_credit$client_id),]
all_credit$max_amount <- NA
all_credit$max_installment_amount <- NA
all_credit$score_max_amount <- NA
all_credit$max_delay <- NA
for(i in 1:nrow(all_credit)){
  suppressWarnings(tryCatch({
    if(all_credit$product_id[i]==8 & all_credit$is_vip[i]==0){
      product_id <- 5
    } else if(all_credit$product_id[i]==76 & all_credit$is_vip[i]==0){
      product_id <- 1
    } else if(all_credit$product_id[i]==96 & all_credit$is_vip[i]==0){
      product_id <- 7
    } else if(all_credit$product_id[i]==1 & all_credit$is_vip[i]==1){
      product_id <- 76
    } else if(all_credit$product_id[i]==5 & all_credit$is_vip[i]==1){
      product_id <- 8
    } else if(all_credit$product_id[i]==7 & all_credit$is_vip[i]==1){
      product_id <- 96
    } else if(all_credit$product_id[i] %in% c(48,55:58,77,78:81,95)){
      product_id <- 82
    } else if(all_credit$product_id[i] %in% c(66)){
      product_id <- 85
    } else if(all_credit$product_id[i] %in% c(67)){
      product_id <- 89
    } else if(all_credit$product_id[i] %in% c(68)){
      product_id <- 90
    } else if(all_credit$product_id[i] %in% c(71)){
      product_id <- 83
    } else if(all_credit$product_id[i] %in% c(72)){
      product_id <- 84
    } else if(all_credit$product_id[i] %in% c(75)){
      product_id <- 87
    } else {
      product_id <- NA
    }
    client_id <- all_credit$client_id[i]
    last_id <- all_credit$id[i]
    limit_offer <- all_credit$limit_offer[i]
    calc <- gen_terminated_fct(con,client_id,product_id,last_id,
                               flag_limit_offer,db_name,limit_offer)
    all_credit$max_amount[i] <- calc[[1]]
    all_credit$max_installment_amount[i] <- calc[[2]]
    all_credit$score_max_amount[i] <- calc[[3]]
    all_credit$max_delay[i] <- as.numeric(calc[[4]])
  }, error=function(e){}))
}



##################################
### Reapply selection criteria ###
##################################

# Select based on score and DPD
all_credit <- subset(all_credit,all_credit$max_amount>-Inf & 
                                 all_credit$max_amount<Inf)
if(nrow(all_credit)){
  if(!is.na(all_credit$company_id) & all_credit$company_id!=5){
    all_credit <- subset(all_credit,all_credit$max_delay<=360)
  }
}



###################################################
### Assign to group for Credirect and City Cash ###
###################################################

if(nrow(all_credit)>0){

# # Get number of credits
all_credit <- gen_append_nb_credits(db_name,all_credit)

# Get probability to ptc 
all_credit <- gen_list_ptc(db_name,all_credit)

# Get if pay day or not
all_credit <- gen_flag_payday(db_name,all_credit)
all_credit$payday <- ifelse(all_credit$type==4,1,
    ifelse(all_credit$product_id %in% 
    c(25:28,36,37,41:44,49,50,55:58,67:68,78:81,89:90),1,0))

# Make groups for Credirect
all_credit$ptc_score <- ifelse(is.na(all_credit$ptc_score),"medium",
   all_credit$ptc_score)
all_credit$group <- 
   ifelse(!is.na(all_credit$office_id) & all_credit$office_id==215,120,
   ifelse(all_credit$company_id==2,
   ifelse(all_credit$nb_credits==1,
   ifelse(all_credit$payday==0,
   ifelse(all_credit$ptc_score %in% c("very_high"),101,
   ifelse(all_credit$score_max_amount %in% c("Indeterminate","Good 1"),
          102,103)),
   ifelse(all_credit$score_max_amount %in% c("Indeterminate","Good 1"),
          102,103)),
   ifelse(all_credit$payday==0,
   ifelse(all_credit$ptc_score %in% c("very_low"),101,102),101)),
   ifelse(all_credit$company_id==1,
          ifelse(all_credit$ptc_score %in% c("very_low","low"),NA,
          ifelse(all_credit$ptc_score %in% c("medium"),NA,
          ifelse(all_credit$ptc_score %in% c("high","very_high"),NA,NA))),
   NA)))



#############################################
### Work on final dataset and write in DB ###
#############################################

# Read current database
id_max_query <- paste(
  "SELECT MAX(id) as max_id
  FROM ",db_name,".clients_prior_approval_applications",sep="")
id_max <- gen_query(con,id_max_query)$max_id+1

# Create final dataframe for writing in DB
offers <- all_credit
offers$id <- seq(id_max,id_max+nrow(offers)-1,1)
offers$application_id <- NA
offers$hide_until_date <- NA
offers$created_at <- Sys.time()
offers$updated_at <- NA
offers$deleted_at <- NA
offers$credit_amount_updated <- NA
offers$installment_amount_updated <- NA
offers$active_from <- NA
offers$active_to <- paste0("'",Sys.Date() + 180,"'")
offers <- offers[,c("id","office_id","client_id","group","product_id",
    "application_id","max_amount","max_installment_amount",
    "credit_amount_updated","installment_amount_updated","hide_until_date",
    "consultant_id","active_from","active_to",
    "created_at","updated_at","deleted_at")]
names(offers)[names(offers)=="max_amount"] <- "credit_amount"
names(offers)[names(offers)=="max_installment_amount"] <- "installment_amount"
offers[is.na(offers)] <- "NULL"


# Adjust product ID
offers$product_id <- 
  ifelse(offers$product_id %in% c(43,44,49,50,55,57,58),78,
  ifelse(offers$product_id %in% c(9,48,55:58,77:81,95),82,
  ifelse(offers$product_id %in% c(66),85,
  ifelse(offers$product_id %in% c(67),89,
  ifelse(offers$product_id %in% c(68),90,
  ifelse(offers$product_id %in% c(71),83,
  ifelse(offers$product_id %in% c(72),84,
  ifelse(offers$product_id %in% c(75),87,
         offers$product_id))))))))


# Make result ready for SQL query
string_sql <- gen_sql_string_po_terminated(offers,1)


# Output real offers
update_prior_query <- paste("INSERT INTO ",db_name,
".clients_prior_approval_applications VALUES ",
string_sql,";", sep="")


# Read and write
final_exists <- read.xlsx(paste(main_dir,"Scored_Credits_Terminated.xlsx", 
                                sep=""))
final <- rbind(final_exists,offers[,c("id","client_id","credit_amount")])
write.xlsx(final, paste(main_dir,"Scored_Credits_Terminated.xlsx", sep=""))


# Write in database
if(nrow(offers)>0){
  suppressWarnings(tryCatch({
    #suppressMessages(suppressWarnings(dbSendQuery(con,update_prior_query)))
  }, error=function(e){
    offers$id <- offers$id + 1
    string_sql <- gen_sql_string_po_terminated(offers,1)
    update_prior_query <- paste("INSERT INTO ",db_name,
     ".clients_prior_approval_applications VALUES ",
     string_sql,";", sep="")
    #suppressMessages(suppressWarnings(dbSendQuery(con,update_prior_query)))
  }))

}}}

