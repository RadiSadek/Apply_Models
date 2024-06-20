

################################################################################
#         Script for generating  daily offers for terminated credits           #
#      Apply Logistic Regression on all products (CityCash and Credirect)      #
#                          Version 1.0 (2020/06/23)                            #
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
source(file.path(base_dir,"Useful_Functions.r"))
source(file.path(base_dir,"SQL_queries.r"))
source(file.path(base_dir,"Terminated.r"))



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
  subset(all_credits,all_credits$company_id==5 & 
    substring(all_credits$deactivated_at,1,10)==(as.Date(Sys.time())-3)),
  subset(all_credits,all_credits$company_id==1 & all_credits$sub_status==128 & 
    substring(all_credits$deactivated_at,1,10)==(as.Date(Sys.time())-3)),
  subset(all_credits,all_credits$company_id==1 & all_credits$sub_status==123 & 
    substring(all_credits$deactivated_at,1,10)==(as.Date(Sys.time())-4)))


# Remove certain offices
all_credits <- subset(all_credits,is.na(all_credits$office_id) |
   !(all_credits$office_id %in% c(6,19,20,37,38,39,40,60,62,65,101,126,141,146,
   147,148,158,173,174,176,177,217)))


# Get last credit amount
id_list <- paste(all_credits$id,collapse=",")
if(nrow(all_credits)>0){
  credit_amount_sql <- paste("
  SELECT application_id , amount as credit_amount
  FROM ",db_name,".credits_plan_contract WHERE application_id IN (",id_list,")",
         sep ="")
  credit_amount <- gen_query(con,credit_amount_sql)
  all_credits <- merge(all_credits,credit_amount,by.x = "id",
         by.y = "application_id",all.x = TRUE)
}

# Remove duplicates by company_id 
all_credits <- all_credits[rev(order(all_credits$date)),]
all_credits <- all_credits[order(all_credits$client_id),]
all_credits <- all_credits[!duplicated(all_credits[c("client_id",
    "company_id")]),] 



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
  installment_amount,deleted_at,updated_at, `group`, active_from, active_to
  FROM ",db_name,".clients_prior_approval_applications",sep="")
po <- gen_query(con,po_sql_query)
po <- merge(po,company_id,by.x = "product_id",by.y = "id",all.x = TRUE)
po_raw <- po
po_check <- po[is.na(po$deleted_at),]
po_check$has_offer_cur <- 1
select_credits <- merge(select_credits,po_check[,c("client_id","company_id",
  "has_offer_cur")],by.x = c("client_id","company_id"),by.y = c("client_id",
  "company_id"),all.x = TRUE)
select_credits <- subset(select_credits,is.na(select_credits$has_offer_cur))


# Remove those who have active credit of corresponding company
actives_cur <- subset(all_credits_raw,all_credits_raw$status %in% c(4))
actives_cur$has_active_cur <- 1
select_credits <- merge(select_credits,actives_cur[,c("client_id","company_id",
  "has_active_cur")],by.x = c("client_id","company_id"),by.y = c("client_id",
  "company_id"),all.x = TRUE)
select_credits <- subset(select_credits,is.na(select_credits$has_active_cur))


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
   c(12,13,59:65,53,54,51,22,69,70,91,92,93,94)))


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
    } else if(select_credits$product_id[i]==76 & select_credits$is_vip[i]==0){
      product_id <- 1
    } else if(select_credits$product_id[i]==96 & select_credits$is_vip[i]==0){
      product_id <- 7
    } else if(select_credits$product_id[i]==1 & select_credits$is_vip[i]==1){
      product_id <- 76
    } else if(select_credits$product_id[i]==5 & select_credits$is_vip[i]==1){
      product_id <- 8
    } else if(select_credits$product_id[i]==7 & select_credits$is_vip[i]==1){
      product_id <- 96
    } else if(select_credits$product_id[i] %in% c(48,55:58,77,78:81,95)){
      product_id <- 82
    } else if(select_credits$product_id[i] %in% c(66)){
      product_id <- 85
    } else if(select_credits$product_id[i] %in% c(67)){
      product_id <- 89
    } else if(select_credits$product_id[i] %in% c(68)){
      product_id <- 90
    } else if(select_credits$product_id[i] %in% c(71)){
      product_id <- 83
    } else if(select_credits$product_id[i] %in% c(72)){
      product_id <- 84
    } else if(select_credits$product_id[i] %in% c(75)){
      product_id <- 87
    } else {
      product_id <- NA
    }
    client_id <- select_credits$client_id[i]
    last_id <- select_credits$id[i]
    calc <- gen_terminated_fct(con,client_id,product_id,last_id,0,db_name,0)
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



###################################################
### Assign to group for Credirect and City Cash ###
###################################################

# Get number of credits
select_credits <- gen_append_nb_credits(db_name,select_credits)

# Get probability to ptc 
select_credits <- gen_list_ptc(db_name,select_credits)

# Get if pay day or not
select_credits <- gen_flag_payday(db_name,select_credits)
select_credits$payday <- ifelse(select_credits$type==4,1,
     ifelse(select_credits$product_id %in% 
     c(25:28,36,37,41:44,49,50,55:58,67:68,78:81,89:90),1,0))

# Assign group
select_credits$group <- 
 ifelse(!is.na(select_credits$office_id) & select_credits$office_id==215,120,
 ifelse(select_credits$company_id==2,
 ifelse(select_credits$nb_credits==1,
 ifelse(select_credits$payday==0,
 ifelse(select_credits$ptc_score %in% c("very_high"),101,
 ifelse(select_credits$score_max_amount %in% c("Indeterminate","Good 1"),
             102,103)),
 ifelse(select_credits$score_max_amount %in% c("Indeterminate","Good 1"),
             102,103)),
 ifelse(select_credits$payday==0,
 ifelse(select_credits$ptc_score %in% c("very_low"),101,102),101)),NA))



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
offers$active_from <- NA
offers$active_to <- NA
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



#################################
### Updating Cashpoint offers ###
#################################

# Apply selection
po_cp <- po_raw
po_cp <- subset(po_cp,is.na(po_cp$deleted_at) & 
    substring(po_cp$created_at,1,10)==(as.Date(Sys.time())-3) & 
    po_cp$company_id==5)

# Get last ID
all_credits_cp <- all_credits_raw
all_credits_cp <- subset(all_credits_cp,all_credits_cp$status %in% c(5) & 
                         all_credits_cp$company_id==5)
all_credits_cp <- all_credits_cp[rev(order(all_credits_cp$deactivated_at)),]
all_credits_cp <- all_credits_cp[order(all_credits_cp$client_id),]
all_credits_cp <- all_credits_cp[!duplicated(all_credits_cp$client_id),]
colnames(all_credits_cp)[which(names(all_credits_cp) == "id")] <- "last_id"
po_cp <- merge(po_cp,all_credits_cp[,c("client_id","last_id")],
    by.x = "client_id",by.y = "client_id",all.x = TRUE)


# Check if offer is top be updated
if(nrow(po_cp)>0){
  
  po_cp$credit_amount_updated <- NA
  po_cp$installment_amount_updated <- NA
  
  for(i in 1:nrow(po_cp)){
    suppressWarnings(tryCatch({
      client_id <- po_cp$client_id[i]
      last_id <- po_cp$last_id[i]
      calc <- gen_terminated_fct(con,client_id,product_id,last_id,0,db_name,0)
      po_cp$credit_amount_updated[i] <- calc[[1]]
      po_cp$installment_amount_updated[i] <- calc[[2]]
      po_cp$max_delay[i] <- as.numeric(calc[[4]])
    }, error=function(e){}))
  }
  
  # Filter offers
  po_cp <- subset(po_cp,!is.na(po_cp$credit_amount_updated) & 
      po_cp$credit_amount_updated>po_cp$credit_amount)
  
  products_query <- paste(
    "SELECT product_id, amount, period, installment_amount 
  FROM ",db_name,".products_periods_and_amounts WHERE product_id IN (",
    paste(unique(po_cp$product_id),collapse=","),")",sep="")
  products_update <- gen_query(con,products_query)
  max_per_product <- aggregate(products_update$installment_amount,
      by=list(products_update$product_id,products_update$amount),FUN=max)
  for(i in 1:nrow(po_cp)){
    if(po_cp$credit_amount_updated[i]>(po_cp$credit_amount[i]+400)){
      
      po_cp$credit_amount_updated[i] <- po_cp$credit_amount[i] + 400
      po_cp$installment_amount_updated[i] <- 
        max_per_product$x[max_per_product$Group.1==po_cp$product_id[i] & 
          max_per_product$Group.2==po_cp$credit_amount_updated[i]]
    }
  }
  
  if(nrow(po_cp)>0){
    po_change_query <- paste("UPDATE ",db_name,
        ".clients_prior_approval_applications SET updated_at = '",
        substring(Sys.time(),1,19),"' WHERE id IN",
        gen_string_po_terminated(po_cp), sep="")
    suppressMessages(suppressWarnings(dbSendQuery(con,po_change_query)))
    suppressMessages(suppressWarnings(dbSendQuery(con,
        gen_string_delete_po_terminated(po_cp,po_cp$credit_amount_updated,
        "credit_amount",db_name))))
    suppressMessages(suppressWarnings(dbSendQuery(con,
      gen_string_delete_po_terminated(po_cp,po_cp$installment_amount_updated,
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
  !(substring(po_reload$deleted_at,12,19)=="04:00:00"))
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



#################################################
### Correct those for flex credit (Credirect) ###
#################################################

# Read flex gratis period and amounts
flex_gratis <- gen_query(con,paste("
SELECT amount,installment_amount
FROM ",db_name,".products_periods_and_amounts
WHERE product_id = 80",sep=""))

# Correct amount and installment amount to relevant client ids
campaign_180_360 <- gen_query(con,paste("
SELECT client_id, created_at
FROM ",db_name,".call_center_campaigns_clients
WHERE campaign_id IN (302) AND deleted_AT IS NULL",sep=""))
po_updated_c_180_360 <- subset(po_raw,po_raw$company_id==2)
po_updated_c_180_360 <- po_updated_c_180_360[po_updated_c_180_360$client_id 
  %in% campaign_180_360$client_id,]
po_updated_c_180_360 <- 
  po_updated_c_180_360[rev(order(po_updated_c_180_360$created_at)),]
po_updated_c_180_360 <- 
  po_updated_c_180_360[order(po_updated_c_180_360$client_id),]
po_updated_c_180_360 <- 
  po_updated_c_180_360[!duplicated(po_updated_c_180_360$client_id),]
po_updated_c_180_360$credit_amount <-
  ifelse(po_updated_c_180_360$credit_amount==-999,300,
  ifelse(po_updated_c_180_360$credit_amount>1000,1000,
         po_updated_c_180_360$credit_amount))
po_updated_c_180_360 <- po_updated_c_180_360 [ ,
  !names(po_updated_c_180_360 ) %in% c("installment_amount")]
po_updated_c_180_360 <- merge(po_updated_c_180_360,flex_gratis,
   by.x = "credit_amount",by.y = "amount",all.x = TRUE)

# Write in database
if(nrow(po_updated_c_180_360)>0){
  po_change_query <- paste("UPDATE ",db_name,
   ".clients_prior_approval_applications SET deleted_at = NULL,
   product_id = 80, updated_at = '",
   substring(Sys.time(),1,19),"' WHERE id IN",
   gen_string_po_terminated(po_updated_c_180_360), sep="")
  suppressMessages(suppressWarnings(dbSendQuery(con,po_change_query)))
  suppressMessages(suppressWarnings(dbSendQuery(con,
    gen_string_delete_po_terminated(po_updated_c_180_360,
    po_updated_c_180_360$credit_amount,"credit_amount",db_name))))
  suppressMessages(suppressWarnings(dbSendQuery(con,
    gen_string_delete_po_terminated(po_updated_c_180_360,
    po_updated_c_180_360$installment_amount,"installment_amount",db_name))))
}


######################################################
### Check for special cases and delete immediately ###
######################################################

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
"SELECT a.id, a.client_id, b.brand_id
FROM ",db_name,".clients_prior_approval_applications a
LEFT JOIN products b ON a.product_id = b.id WHERE a.deleted_at IS NULL",sep="")
po_special <- gen_query(con,po_get_special_query)
po_special <- merge(po_special,marketing,by.x = c("client_id","brand_id"),
  by.y = c("client_id","brand_id"),all.x = TRUE)
marketing <- subset(po_special,po_special$gdpr_marketing_messages==1)
po_special <- rbind(
  po_special[po_special$client_id %in% special$id,c("id","client_id")],
  po_special[po_special$client_id %in% po_active$client_id,c("id","client_id")],
  marketing[,c("id","client_id")],
  clients_dups[,c("id","client_id")])

if(nrow(po_special)>0){
  po_special_query <- paste("UPDATE ",db_name,
   ".clients_prior_approval_applications SET updated_at = '",
   substring(Sys.time(),1,19),"', deleted_at = '",
   paste(substring(Sys.time(),1,10),"04:00:00",sep=),"'
   WHERE id IN",gen_string_po_terminated(po_special), sep="")
  suppressMessages(suppressWarnings(dbSendQuery(con,po_special_query)))
}


####################################
### Reactivate those from groups ###
####################################

# Get those who meet criteria
po_group <- subset(po_raw,!is.na(po_raw$active_from))
po_group <- subset(po_group,!is.na(po_group$group) & 
    Sys.Date()>po_group$active_from & Sys.Date()>po_group$active_to &
    !is.na(po_group$deleted_at) & po_group$created_at>(Sys.Date()-180))

# Remove those with an active credit 
all_credits_active <- subset(all_credits_raw,all_credits_raw$status==4)
po_group <- merge(po_group,
    all_credits_active[,c("client_id","signed_at","company_id")],
    by.x = c("client_id","company_id"),by.y = c("client_id","company_id"),
    all.x = TRUE)
po_group <- subset(po_group,is.na(po_group$signed_at) | 
                   po_group$signed_at<po_group$created_at)

# Remove those with a terminated credit after creation of offer
po_group <- po_group[ , !names(po_group) %in% c("signed_at")]
all_credits_term <- subset(all_credits_raw,all_credits_raw$status==5)
po_group_check <-  merge(po_group,
  all_credits_term[,c("client_id","signed_at","company_id")],
  by.x = c("client_id","company_id"),by.y = c("client_id","company_id"),
  all.x = TRUE)
po_group_check <- subset(po_group_check,
  po_group_check$signed_at>po_group_check$created_at)
po_group <- po_group[!(po_group$id %in% po_group_check$id),]

# Write in database
if(nrow(po_group)>0){
  po_change_query <- paste("UPDATE ",db_name,
    ".clients_prior_approval_applications SET deleted_at = NULL,
    active_to = NULL, active_from = NULL,
    `group` = NULL, updated_at = '",
    substring(Sys.time(),1,19),"' WHERE id IN",
    gen_string_po_terminated(po_group), sep="")
  suppressMessages(suppressWarnings(dbSendQuery(con,po_change_query)))
}

#  Redelete group for those activated and obsolete 
po_group <- subset(po_raw,!is.na(po_raw$active_from))
po_group <- subset(po_group,!is.na(po_group$group) & 
   Sys.Date()>po_group$active_from & Sys.Date()>po_group$active_to &
   is.na(po_group$deleted_at) & po_group$created_at>(Sys.Date()-180))
if(nrow(po_group)>0){
  po_change_query <- paste("UPDATE ",db_name,
    ".clients_prior_approval_applications SET `group` = NULL, updated_at = '",
     substring(Sys.time(),1,19),"' WHERE id IN",
     gen_string_po_terminated(po_group), sep="")
  suppressMessages(suppressWarnings(dbSendQuery(con,po_change_query)))
}


##################################
### Rearrange for new products ###
##################################

po_rearrange_query <- paste("UPDATE ",db_name,
".clients_prior_approval_applications SET product_id=
IF(product_id=48,82,
IF(product_id=77,82,
IF(product_id=95,82,
IF(product_id=66,85,
IF(product_id=67,89,
IF(product_id=68,90,
IF(product_id=69,92,
IF(product_id=70,91,
IF(product_id=71,83,
IF(product_id=72,84,87))))))))))
WHERE product_id IN (48,77,95,66,67,68,69,70,71,72,75)
AND deleted_at IS NULL", sep="")
suppressMessages(suppressWarnings(dbSendQuery(con,po_rearrange_query)))


####################################
### Recorrect if not valid offer ###
####################################

po_rearrange_query <- paste("UPDATE ",db_name,
".clients_prior_approval_applications
SET deleted_at = '",paste(substring(Sys.time(),1,10),sep=""),
" 04:00:00' WHERE credit_amount <0 AND deleted_at IS NULL",sep="")
suppressMessages(suppressWarnings(dbSendQuery(con,po_rearrange_query)))


###########
### End ###
###########

