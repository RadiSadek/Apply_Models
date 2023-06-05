

################################################################################
#         Script for generating  daily offers for terminated credits           #
#      Apply Logistic Regression on all products (CityCash and Credirect)      #
#                          Version 1.0 (2020/06/23)                            #
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
SELECT id, status, date, created_at, signed_at, product_id, client_id, 
deactivated_at, sub_status, office_id, consultant_id
FROM ",db_name,".credits_applications 
WHERE status IN (1,3,4,5)",sep="")
all_credits_all <- gen_query(con,get_actives_sql)


# Get company ID
company_id <- gen_query(con,
    gen_get_company_id_query(db_name))
all_credits_all <- merge(all_credits_all,company_id,by.x = "product_id",
    by.y = "id",all.x = TRUE)
all_credits <- subset(all_credits_all,all_credits_all$status %in% c(4,5))


# Subset terminated credits
all_credits_raw <- all_credits
all_credits <- subset(all_credits,all_credits$status %in% c(5))
all_credits <- subset(all_credits, is.na(all_credits$sub_status) | 
  all_credits$sub_status %in% c(123,128))


# Subset based on time difference since deactivation
all_credits <- rbind(
  subset(all_credits,all_credits$company_id==2 & 
    substring(all_credits$deactivated_at,1,10)==(as.Date(Sys.time())-2)),
  subset(all_credits,all_credits$company_id==1 & all_credits$sub_status==128 & 
    substring(all_credits$deactivated_at,1,10)==(as.Date(Sys.time())-3)),
  subset(all_credits,all_credits$company_id==1 & all_credits$sub_status==123 & 
    substring(all_credits$deactivated_at,1,10)==(as.Date(Sys.time())-4)))


# Get last credit amount
credit_amount_sql <- paste("
SELECT application_id , amount as credit_amount
FROM ",db_name,".credits_plan_contract", sep ="")
credit_amount <- gen_query(con,credit_amount_sql)
all_credits <- merge(all_credits,credit_amount,by.x = "id",
                     by.y = "application_id",all.x = TRUE)
all_credits <- all_credits[rev(order(all_credits$date)),]
all_credits <- all_credits[order(all_credits$client_id), ]
all_credits <- all_credits[!duplicated(all_credits$client_id),]



#####################################################
### Apply selection criteria for credits to offer ###
#####################################################

# Remove credits with more than 1 "varnat"
all_credits_varnat <- subset(all_credits_raw,all_credits_raw$sub_status==122)
all_credits_varnat$count <- 1
count_varnat <- as.data.frame(aggregate(all_credits_varnat$count,
    by=list(all_credits_varnat$client_id),FUN=sum))
names(count_varnat) <- c("client_id","tot_varnat")
all_credits <- merge(all_credits,count_varnat,by.x = "client_id",
                     by.y = "client_id",all.x = TRUE)
select_credits  <- subset(all_credits,is.na(all_credits$tot_varnat) | 
                          all_credits$tot_varnat<2)


# Remove credits with already an offer of corresponding company
po_sql_query <- paste(
  "SELECT id, client_id, product_id, application_id, created_at, credit_amount,
  installment_amount,deleted_at,updated_at
  FROM ",db_name,".clients_prior_approval_applications",sep="")
po <- gen_query(con,po_sql_query)
po <- merge(po,company_id,by.x = "product_id",by.y = "id",all.x = TRUE)
po_raw <- po
po_select_citycash <- po[is.na(po$deleted_at) & po$company_id==1,]
po_select_credirect <- po[is.na(po$deleted_at) & po$company_id==2,]
select_credits_citycash <- select_credits[!(select_credits$client_id %in% 
    po_select_citycash$client_id) & select_credits$company_id==1,]
select_credits_credirect <- select_credits[!(select_credits$client_id %in% 
    po_select_credirect$client_id) & select_credits$company_id==2,]
select_credits <- rbind(select_credits_citycash,select_credits_credirect)


# Remove those who have active credit of corresponding company
actives_cur_citycash <- subset(all_credits_raw,all_credits_raw$status %in% c(4)
   & all_credits_raw$company_id==1)
actives_cur_credirect <- subset(all_credits_raw,all_credits_raw$status %in% c(4)
   & all_credits_raw$company_id==2)
select_credits_citycash <- subset(select_credits,
   !(select_credits$client_id %in% actives_cur_citycash$client_id) & 
     select_credits$company_id==1)
select_credits_credirect <- subset(select_credits,
   !(select_credits$client_id %in% actives_cur_credirect$client_id) & 
     select_credits$company_id==2)
select_credits <- rbind(select_credits_citycash,select_credits_credirect)


# Join if VIP
names_b4 <- names(select_credits)
is_vip_query <- paste(
  "SELECT client_id, brand_id, is_vip
   FROM ",db_name,".client_brand",sep="")
is_vip <- gen_query(con,is_vip_query)
select_credits <- merge(select_credits,is_vip,
   by.x = c("client_id","company_id"),
   by.y = c("client_id","brand_id"),all.x = TRUE)
select_credits$is_vip <- ifelse(is.na(select_credits$is_vip),0,
   select_credits$is_vip)
select_credits <- select_credits[,c(names_b4,"is_vip")]


# Remove Big Fin and other Ipoteki
select_credits <- subset(select_credits,!(select_credits$product_id %in%
   c(12,13,59:65,53,54,51,22)))


#####################
### Compute score ###
#####################

if(nrow(select_credits)>0){

# Compute and append score
select_credits <- select_credits[!duplicated(select_credits$client_id),]
select_credits$max_amount <- NA
select_credits$max_installment_amount <- NA
select_credits$score_max_amount <- NA
select_credits$max_delay <- NA
for(i in 1:nrow(select_credits)){
  suppressWarnings(tryCatch({
    if(select_credits$product_id[i]==8 & select_credits$is_vip[i]==0){
      product_id <- 5
    } else {
      product_id <- NA
    }
    client_id <- select_credits$client_id[i]
    last_id <- select_credits$id[i]
    calc <- gen_terminated_fct(con,client_id,product_id,last_id,0)
    select_credits$max_amount[i] <- calc[[1]]
    select_credits$max_installment_amount[i] <- calc[[2]]
    select_credits$score_max_amount[i] <- calc[[3]]
    select_credits$max_delay[i] <- as.numeric(calc[[4]])
  }, error=function(e){}))
}



##################################
### Reapply selection criteria ###
##################################

# Select based on score and DPD
select_credits <- subset(select_credits,select_credits$max_amount>-Inf & 
                           select_credits$max_amount<Inf)
select_credits <- subset(select_credits,select_credits$max_delay<=360)



#####################################
### Assign to group for Credirect ###
#####################################

# Get number of credits
select_credits <- gen_append_nb_credits(db_name,select_credits)

# Get probability to ptc 
select_credits <- gen_list_ptc(db_name,select_credits)

# Get if pay day or not
select_credits <- gen_flag_payday(db_name,select_credits)
select_credits$payday <- ifelse(select_credits$type==4,1,
     ifelse(select_credits$product_id %in% 
     c(25:28,36,37,41:44,49,50,55:58,67:68,78:81),1,0))

# Make groups for Credirect
select_credits$group <- 
 ifelse(select_credits$company_id==2,
 ifelse(select_credits$nb_credits==1,
 ifelse(select_credits$payday==0,
 ifelse(select_credits$ptc_score %in% c("very_high"),101,
 ifelse(select_credits$score_max_amount %in% c("Indeterminate","Good 1"),
             102,103)),
 ifelse(select_credits$score_max_amount %in% c("Indeterminate","Good 1"),
             102,103)),
 ifelse(select_credits$payday==0,
 ifelse(select_credits$ptc_score %in% c("very_low"),101,102),101)),NA)



#############################################
### Work on final dataset and write in DB ###
#############################################

# Read current database
id_max <- max(po$id)+1

if(nrow(select_credits)>0){

# Create final dataframe for writing in DB
offers <- select_credits
offers$id <- seq(id_max,id_max+nrow(offers)-1,1)
offers$application_id <- NA
offers$hide_until_date <- NA
offers$created_at <- Sys.time()
offers$updated_at <- NA
offers$deleted_at <- NA
offers$credit_amount_updated <- NA
offers$installment_amount_updated <- NA
offers <- offers[,c("id","office_id","client_id","group","product_id",
    "application_id","max_amount","max_installment_amount",
    "credit_amount_updated","installment_amount_updated","hide_until_date",
    "consultant_id","created_at","updated_at","deleted_at")]
names(offers)[names(offers)=="max_amount"] <- "credit_amount"
names(offers)[names(offers)=="max_installment_amount"] <- "installment_amount"
offers[is.na(offers)] <- "NULL"


# Adjust product ID
offers$product_id <- 
  ifelse(offers$product_id %in% c(43,44,49,50,57,58,55),78,
  ifelse(offers$product_id %in% c(78,79,80,81),78,
  ifelse(offers$product_id %in% c(9),82,
  ifelse(offers$product_id %in% c(48),77,
         offers$product_id))))


# Make result ready for SQL query
string_sql <- gen_sql_string_po_terminated(offers,1)
if(nrow(offers)>1){
  for(i in 2:nrow(offers)){
    string_sql <- paste(string_sql,gen_sql_string_po_terminated(offers,i),
                        sep=",")
  }
}


# Output real offers
update_prior_query <- paste("INSERT INTO ",db_name,
".clients_prior_approval_applications VALUES ",
string_sql,";", sep="")


# Write in database
if(nrow(offers)>0){
  suppressMessages(suppressWarnings(dbSendQuery(con,update_prior_query)))
}}}


###################################
### Updating certain old offers ###
###################################

# Choose credits for updating
po_old <- po_raw
po_old$time_past <- as.numeric(
  round(difftime(as.Date(substring(Sys.time(),1,10)),
  as.Date(substring(po_old$created_at,1,10)),units=c("days")),2))
po_old <- subset(po_old,po_old$time_past>0 & po_old$time_past<=360 &
  po_old$time_past%%30==0 & is.na(po_old$deleted_at))


# See if any new credit created after offer
po_old <- merge(po_old,gen_if_credit_after_po_terminated(
  all_credits_raw,po_old,"last_appl_id_citycash",1),by.x = "client_id",
  by.y = "client_id",all.x = TRUE)
names(po_old)[ncol(po_old)] <- c("signed_at_citycash")
po_old <- merge(po_old,gen_if_credit_after_po_terminated(
  all_credits_raw,po_old,"last_appl_id_credirect",2),by.x = "client_id",
  by.y = "client_id",all.x = TRUE) 
names(po_old)[ncol(po_old)] <- c("signed_at_credirect")
po_old <- merge(po_old,gen_if_credit_after_po_terminated(
  all_credits_raw,po_old,"last_appl_id_cashpoint",5),by.x = "client_id",
  by.y = "client_id",all.x = TRUE) 
names(po_old)[ncol(po_old)] <- c("signed_at_cashpoint")
po_old$last_id <- 
  ifelse(po_old$company_id==1,po_old$last_appl_id_citycash,
  ifelse(po_old$company_id==2,po_old$last_appl_id_credirect,
                              po_old$last_appl_id_cashpoint))
po_old <- po_old[,-which(names(po_old) %in% c("last_appl_id_credirect",
    "last_appl_id_citycash","last_appl_id_cashpoint"))]
po_old$criteria <- ifelse(po_old$company_id==1,
          ifelse(po_old$signed_at_citycash>=po_old$created_at,0,1),
    ifelse(po_old$company_id==2,
          ifelse(po_old$signed_at_credirect>=po_old$created_at,0,1),
          ifelse(po_old$signed_at_cashpoint>=po_old$created_at,0,1)))
po_old <- subset(po_old,po_old$criteria==1)


# Recheck if client still has vip status
names_b4 <- names(po_old)
is_vip <- gen_query(con,is_vip_query)
po_old <- merge(po_old,is_vip,
  by.x = c("client_id","company_id"),
  by.y = c("client_id","brand_id"),all.x = TRUE)
po_old <- po_old[,c(names_b4,"is_vip")]


# Update scoring to selected credits
for(i in 1:nrow(po_old)){
  suppressWarnings(tryCatch({
  if(po_old$product_id[i]==8 & po_old$is_vip[i]==0){
    product_id <- 5
  } else {
    product_id <- NA
  }
  client_id <- po_old$client_id[i]
  last_id <- po_old$last_id[i]
  calc <- gen_terminated_fct(con,client_id,product_id,last_id,0)
  po_old$credit_amount[i] <- calc[[1]]
  po_old$installment_amount[i] <- calc[[2]]
  po_old$max_delay[i] <- as.numeric(calc[[4]])
  }, error=function(e){}))
}


# Change database
po_not_ok <- subset(po_old,is.infinite(po_old$credit_amount))
po_ok <- subset(po_old,!(is.infinite(po_old$credit_amount)))

if(nrow(po_not_ok)>0){
  po_not_ok$credit_amount <- -999
  po_not_ok$installment_amount <- -999
  po_ok_not_query <- paste("UPDATE ",db_name,
       ".clients_prior_approval_applications SET updated_at = '",
       substring(Sys.time(),1,19),"' WHERE id IN",
       gen_string_po_terminated(po_not_ok), sep="")
  suppressMessages(suppressWarnings(dbSendQuery(con,po_ok_not_query)))
  suppressMessages(suppressWarnings(dbSendQuery(con,
     gen_string_delete_po_terminated(po_not_ok,po_not_ok$credit_amount,
     "credit_amount_updated",db_name))))
  suppressMessages(suppressWarnings(dbSendQuery(con,
     gen_string_delete_po_terminated(po_not_ok,po_not_ok$installment_amount,
     "installment_amount_updated",db_name))))
  po_not_ok_credirect <- subset(po_not_ok,po_not_ok$company_id==2)
  if(nrow(po_not_ok_credirect)>0){
    suppressMessages(suppressWarnings(dbSendQuery(con,
       gen_string_delete_po_terminated(po_not_ok_credirect,
        po_not_ok_credirect$credit_amount,"credit_amount",db_name))))
    suppressMessages(suppressWarnings(dbSendQuery(con,
       gen_string_delete_po_terminated(po_not_ok_credirect,
       po_not_ok_credirect$installment_amount,"installment_amount",db_name))))
  }
}

if(nrow(po_ok)>0){
  po_ok_query <- paste("UPDATE ",db_name,
       ".clients_prior_approval_applications SET updated_at = '",
       substring(Sys.time(),1,19),"' WHERE id IN",
       gen_string_po_terminated(po_ok), sep="")
  suppressMessages(suppressWarnings(dbSendQuery(con,po_ok_query)))
  suppressMessages(suppressWarnings(dbSendQuery(con,
    gen_string_delete_po_terminated(po_ok,po_ok$credit_amount,
    "credit_amount_updated",db_name))))
  suppressMessages(suppressWarnings(dbSendQuery(con,
    gen_string_delete_po_terminated(po_ok,po_ok$installment_amount,
    "installment_amount_updated",db_name))))
  po_ok_credirect <- subset(po_ok,po_ok$company_id==2)
  if(nrow(po_ok_credirect)>0){
    suppressMessages(suppressWarnings(dbSendQuery(con,
       gen_string_delete_po_terminated(po_ok_credirect,
       po_ok_credirect$credit_amount,"credit_amount",db_name))))
    suppressMessages(suppressWarnings(dbSendQuery(con,
       gen_string_delete_po_terminated(po_ok_credirect,
       po_ok_credirect$installment_amount,"installment_amount",db_name))))
  }
}


# Update at beginning of month for City Cash
if(substring(Sys.time(),9,10)=="01"){
  
  po_sql_query <- paste(
    "SELECT id, credit_amount, updated_at, installment_amount, product_id, 
  created_at, credit_amount_updated,installment_amount_updated, deleted_at
  FROM ",db_name,".clients_prior_approval_applications
  WHERE deleted_at IS NULL",sep="")
  po_all <- gen_query(con,po_sql_query)
  po_all <- merge(po_all,company_id,by.x = "product_id",
    by.y = "id",all.x = TRUE)
  po_all <- subset(po_all,po_all$company_id==1)

  po_all_not_ok <- subset(po_all,po_all$credit_amount_updated==-999)
  if(nrow(po_all_not_ok)>0){
    po_all_not_ok_query <- paste("UPDATE ",db_name,
      ".clients_prior_approval_applications SET updated_at = '",
      substring(Sys.time(),1,19),"', deleted_at = '",
      paste(substring(Sys.time(),1,10),"04:00:00",sep=),"'
      WHERE id IN",gen_string_po_terminated(po_all_not_ok), sep="")
    suppressMessages(suppressWarnings(dbSendQuery(con,po_all_not_ok_query)))
  }
  
  po_all <- subset(po_all,po_all$credit_amount_updated!=-999)
  if(nrow(po_all)>0){
    po_change_query <- paste("UPDATE ",db_name,
      ".clients_prior_approval_applications SET updated_at = '",
      substring(Sys.time(),1,19),"' WHERE id IN",
      gen_string_po_terminated(po_all), sep="")
    suppressMessages(suppressWarnings(dbSendQuery(con,po_change_query)))
    suppressMessages(suppressWarnings(dbSendQuery(con,
      gen_string_delete_po_terminated(po_all,po_all$credit_amount_updated,
      "credit_amount",db_name))))
    suppressMessages(suppressWarnings(dbSendQuery(con,
      gen_string_delete_po_terminated(po_all,po_all$installment_amount_updated,
      "installment_amount",db_name))))
  }
}



#################################################
### Check those who didn't amount to a credit ###
#################################################

# Reload offers which were deleted for nothing (and are relevant)
po_sql_query <- paste(
  "SELECT id, client_id, product_id, created_at,deleted_at
  FROM ",db_name,".clients_prior_approval_applications
  WHERE `group` IS NULL AND LEFT(deleted_at,10) = '",
  (as.Date(Sys.time())-3),"'",sep="")
po_reload <- gen_query(con,po_sql_query)
po_reload <- subset(po_reload,
  !(substring(po_reload$deleted_at,12,19)=="01:00:00"))
po_reload <- merge(po_reload,company_id[,c("id","company_id")],
  by.x = "product_id",by.y = "id",all.x = TRUE)
po_reload$difftime <- round(difftime(po_reload$deleted_at,po_reload$created_at,
  units=c("days")),2)
po_reload <- subset(po_reload,po_reload$difftime<90)

# Subset if offer but didn't amount to nothing
po_reload <- merge(po_reload,
  all_credits_all[,c("client_id","created_at","company_id")],
  by.x = "client_id",by.y = "client_id",all.x = TRUE)
po_reload$difftime2 <- round(difftime(po_reload$created_at.y,
  po_reload$deleted_at,units=c("days")),2)
has_credit_after <- subset(po_reload,
  po_reload$difftime2>=-0.3 & po_reload$company_id.x==po_reload$company_id.y)
po_reload <- po_reload[!(po_reload$client_id %in% has_credit_after$client_id),]
po_reload <- po_reload[!duplicated(po_reload$client_id),]

if(nrow(po_reload)>0){
  po_reload_query <- paste("UPDATE ",db_name,
   ".clients_prior_approval_applications SET updated_at = '",
   substring(Sys.time(),1,19),"', deleted_at = NULL
   WHERE id IN",gen_string_po_terminated(po_reload),sep="")
  suppressMessages(suppressWarnings(dbSendQuery(con,po_reload_query)))
}



######################################################
### Check for special cases and delete immediately ###
######################################################

# Read special cases (deceased and gdrk marketing clients) 
get_special_sql <- paste("
SELECT id
FROM ",db_name,".clients
WHERE gdpr_marketing_messages=1 OR dead_at IS NOT NULL",sep="")
special <- gen_query(con,get_special_sql)

# Get those who have currently an active in corresponding company
po_active <- subset(po_raw,is.na(po_raw$deleted_at))
all_credits_active <- subset(all_credits_raw,all_credits_raw$status==4)
po_active <- merge(po_active,
  all_credits_active[,c("client_id","signed_at","company_id")],
  by.x = c("client_id","company_id"),by.y = c("client_id","company_id"),
  all.x = TRUE)
po_active <- subset(po_active,!is.na(po_active$signed_at))
po_active <- po_active[c(1:min(nrow(po_active),100)),]

# Remove those who have double entry
doubles_sql <- paste("SELECT * 
FROM ",db_name,".clients_prior_approval_applications
WHERE deleted_at IS NULL", sep="")
doubles <- gen_query(con,doubles_sql)
products_sql <-  paste("SELECT id, brand_id
FROM ",db_name,".products",sep="")
products <- gen_query(con,products_sql)
doubles <- merge(doubles,products,by.x = "product_id",by.y = "id",
    all.x = TRUE)
doubles <- subset(doubles,doubles$brand_id==1)
dups <- doubles[duplicated(doubles$client_id),]
clients_dups <- doubles[doubles$client_id %in% dups$client_id,]
clients_dups <- clients_dups[order(clients_dups$created_at),]
clients_dups <- clients_dups[order(clients_dups$client_id),]
clients_dups <- clients_dups[!duplicated(clients_dups$client_id),]

# Remove special cases if has offer
po_get_special_query <- paste(
  "SELECT id, client_id
  FROM ",db_name,".clients_prior_approval_applications
  WHERE deleted_at IS NULL",sep="")
po_special <- gen_query(con,po_get_special_query)
po_special <- rbind(
  po_special[po_special$client_id %in% special$id,],
  po_special[po_special$client_id %in% po_active$client_id,],
  clients_dups[,c("id","client_id")])

if(nrow(po_special)>0){
  po_special_query <- paste("UPDATE ",db_name,
   ".clients_prior_approval_applications SET updated_at = '",
   substring(Sys.time(),1,19),"', deleted_at = '",
   paste(substring(Sys.time(),1,10),"04:00:00",sep=),"'
   WHERE id IN",gen_string_po_terminated(po_special), sep="")
  suppressMessages(suppressWarnings(dbSendQuery(con,po_special_query)))
}



###########
### End ###
###########

