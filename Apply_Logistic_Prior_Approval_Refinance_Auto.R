


################################################################################
#                    Joint script for Application to REFINANCES                #
#      Apply Logistic Regression on all products (CityCash and Credirect)      #
#                          Version 6.0 (2020/01/15)                            #
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



#########################
### Command arguments ###
#########################

args <- commandArgs(trailingOnly = TRUE)
application_id <- args[1]
product_id <- args[2]


#######################
### Manual settings ###
#######################

# Defines the directory where the RScript is located
base_dir <- here('app/Factories/Scoring')



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
source(file.path(base_dir,"Logistic_App_CityCash.r"))
source(file.path(base_dir,"Logistic_App_Credirect.r"))
source(file.path(base_dir,"Logistic_Beh_CityCash.r"))
source(file.path(base_dir,"Logistic_Beh_Credirect.r"))
source(file.path(base_dir,"Useful_Functions.r"))
source(file.path(base_dir,"Empty_Fields.r"))
source(file.path(base_dir,"Cutoffs.r"))
source(file.path(base_dir,"SQL_queries.r"))
source(file.path(base_dir,"Disposable_Income.r"))
source(file.path(base_dir,"Behavioral_Variables.r"))
source(file.path(base_dir,"Normal_Variables.r"))
source(file.path(base_dir,"CKR_variables.r"))



########################
####### Settings #######
########################

# Load predefined libraries
rdata <- file.path(base_dir, "rdata", 
                   "all_repeat.rdata")
load(rdata)
rdata4 <- file.path(base_dir, "rdata", 
                    "credirect_repeat.rdata")
load(rdata4)



####################################
### Read database and build data ###
####################################

# Read credits applications
all_df <- suppressWarnings(fetch(dbSendQuery(con, 
              gen_big_sql_query(db_name,application_id)), n=-1))
all_df$date <- ifelse(all_df$status %in% c(4,5), all_df$signed_at, 
                      all_df$created_at)


# Apply some checks to main credit dataframe
if(!is.na(product_id)){
  all_df$product_id <- product_id
}
if(nrow(all_df)>1){
  all_df <- all_df[!duplicated(all_df$application_id),]
}


# Read credits to score from prior approval
po_sql_query <- paste(
"SELECT max_amount, min_amount, product_id FROM ",db_name,
".prior_approval_refinances WHERE application_id=",application_id)
po <- suppressWarnings(fetch(dbSendQuery(con, po_sql_query), n=-1))


# Recorrect amount to get highest possible amount
all_df$amount <- po$max_amount
all_df$product_id <- po$product_id


# Read product's periods and amounts
products  <- suppressWarnings(fetch(dbSendQuery(con, 
   gen_products_query(db_name,all_df)), n=-1))
products_desc <- suppressWarnings(fetch(dbSendQuery(con, 
   gen_products_query_desc(db_name,all_df)), n=-1))


# Read all previous credits or applications of client
all_credits <- suppressWarnings(fetch(dbSendQuery(con, 
      gen_all_credits_query(db_name,all_df)), n=-1))
all_credits$date <- ifelse(all_credits$status %in% c(4,5), 
      all_credits$signed_at, all_credits$created_at)
#all_credits <- rbind(all_credits,
#      all_credits[all_credits$id==application_id,])


# Check if client has a risk profile
risk <- suppressWarnings(fetch(dbSendQuery(con, 
    gen_risky_query(db_name,all_df)), n=-1))


# Read total amount of current credit
total_amount_curr <- suppressWarnings(fetch(dbSendQuery(con, 
    gen_total_amount_curr_query(db_name,application_id)), n=-1))


# Read CKR 
data_ckr_bank <- gen_query_ckr(1)
data_ckr_financial <- gen_query_ckr(2)


# Read all previous active or terminated credits of client
all_id <- subset(all_credits, all_credits$id==application_id | 
    (all_credits$status %in% c(4,5) &
    (all_credits$sub_status!=129 | is.na(all_credits$sub_status)) & 
     all_credits$client_id==all_df$client_id))


# Select for max delay if past AND active: must have at least 30 days of passed
all_id_max_delay <- all_id
all_actives_past <- subset(all_id, all_id$status==4)
if(nrow(all_actives_past)>0){
  all_id_max_delay <- gen_select_relevant_ids_max_delay(db_name,all_actives,
      all_id_max_delay)
}


# Generate sum paid and amounts of previous credit 
nrow_all_id <- nrow(all_id)
if (nrow_all_id>=1){
  cash_flow <- gen_last_paid(rbind(all_id,all_id[all_id$id==max(all_id$id),]))
  total_amount <- gen_last_total_amount(
    rbind(all_id,all_id[all_id$id==max(all_id$id),]))
  prev_amount <- gen_last_prev_amount(
    rbind(all_id,all_id[all_id$id==max(all_id$id),]))
}


# Get correct max days of delay (of relevant previous credits)
all_id_max_delay <- all_id_max_delay[!duplicated(all_id_max_delay$id),]
nrow_all_id_max_delay <- nrow(all_id_max_delay)
if (nrow_all_id_max_delay>=1){
  list_ids_max_delay <- gen_select_relevant_ids(all_id_max_delay,
     nrow_all_id_max_delay)
  data_plan_main_select <- suppressWarnings(fetch(dbSendQuery(con, 
     gen_plan_main_select_query(db_name,list_ids_max_delay)), n=-1))
} 


# Get average expenses according to client's address 
addresses <- suppressWarnings(fetch(dbSendQuery(con, 
  gen_address_query(all_df$client_id,"App\\\\Models\\\\Clients\\\\Client")), 
  n=-1))
if(nrow(addresses)==0){
  addresses <- suppressWarnings(fetch(dbSendQuery(con, 
  gen_address_query(all_df$client_id,
  "App\\\\Models\\\\Credits\\\\Applications\\\\Application")), n=-1))
}


############################################
### Compute and rework additional fields ###
############################################

# Compute flag if credit is up to next salary
flag_credit_next_salary <- ifelse(all_df$product_id %in% 
      c(25:28,36,37,41:44,49,50,55:58), 1, 0)


# Compute flag if product is credirect
flag_credirect <- ifelse(products_desc$company_id==2, 1, 0)


# Compute flag if client has previous otpisan or tsediran
flag_exclusion <- ifelse(length(which(names(
  table(all_credits$sub_status)) %in% c(124,133)))>0, 1,
  ifelse(nrow(risk)>0, 1, 0))


all_df <- cbind(all_df, data_ckr_financial)	
names(all_df)[(ncol(all_df)-8):ncol(all_df)] <- c("ckr_cur_fin","ckr_act_fin",	
  "ckr_fin_fin",	"src_ent_fin","amount_fin","cred_count_fin",	
  "outs_principal_fin","outs_overdue_fin","cession_fin")	
all_df <- cbind(all_df, data_ckr_bank)		
names(all_df)[(ncol(all_df)-8):ncol(all_df)] <- c("ckr_cur_bank",	
  "ckr_act_bank","ckr_fin_bank","src_ent_bank","amount_bank","cred_count_bank",	
  "outs_principal_bank","outs_overdue_bank","cession_bank")


# Set period variable (monthly, twice weekly, weekly)
period <- products_desc$period


# Compute and generate general variables
all_df <- gen_norm_var(period,all_df,products,2)


# Compute and generate variables specific for behavioral model
data_plan_main_select_def <- ifelse(exists("data_plan_main_select"),
          data_plan_main_select,NA)
all_df <- suppressWarnings(
  gen_other_rep(nrow_all_id,all_id,
                all_df,flag_credirect,
                data_plan_main_select_def))
all_df$max_delay <- ifelse(!(is.na(data_plan_main_select_def)), 
   data_plan_main_select_def[1], ifelse(flag_credirect==0, 60, 10))
all_df$credits_cum <- nrow(all_id)
all_df$days_diff_last_credit <- NA


# Get flag if credit is behavioral or not
flag_beh <- ifelse(all_df$credits_cum==0, 0, 1)


# Compute and rework CKR variables, suitable for model application
all_df <- gen_ckr_variables(all_df,flag_beh,flag_credirect)


# Compute if previous is online 
all_credits <- get_company_id_prev(db_name,all_credits)
all_df <- gen_prev_online(db_name, all_credits,all_df,max(all_credits$id)+1)
                             
                              
# Get flag if credit is behavioral but with same company
flag_beh_company <- ifelse(
  nrow(all_credits[all_credits$company_id==
  all_credits$company_id[all_credits$id==application_id],])>1,1,0)


# Compute flag if last paid credit is maybe hidden refinance
all_df$flag_high_last_paid <- ifelse(
 gen_total_last_paid(max(all_id$id),db_name)/total_amount$final_credit_amount>0.5,
  0,1)
                                
# Compute amount differential 
all_df$amount_diff <- ifelse(nrow_all_id<=1, NA, all_df$amount - 
        prev_amount$amount)


# Compute income variables
t_income <- gen_t_income(db_name,application_id,period)
disposable_income_adj <- gen_disposable_income_adj(db_name,application_id,
        all_df,addresses,period)
all_df$total_income <- gen_income(db_name,application_id)


# Read relevant product amounts (not superior to amount of application)
products <- subset(products, products$amount<=all_df$amount)


# Prepare final dataframe
scoring_df <- gen_final_df(products)


# Make back-up dataframe
df <- all_df


# Correct empty and missing value fields (to standard format)
df <- gen_null_to_na(df)


# Get if empty field threshold is passed
empty_fields <- gen_empty_fields(flag_beh,flag_credirect,df)
threshold_empty <- ifelse(flag_credirect==0 & flag_beh==0, 7,
   ifelse(flag_credirect==1 & flag_beh==0, 4,
   ifelse(flag_credirect==0 & flag_beh==1, 8, 5)))


# Adjust count of empty fields accordingly
empty_fields <- ifelse(flag_credirect==1, empty_fields, 
   ifelse(is.na(df$total_income) | df$total_income==0, 
   threshold_empty, empty_fields))


# Readjust fields
df <- gen_norm_var2(df)


# Compute flag exclusion for cession in CKR
flag_cession <- ifelse(flag_credirect==1 & df$amount_cession_total>0, 1, 0)

# Get previous installment amount
closest_period <- products$period[which.min(
  abs(total_amount$installments - products$period))]
closest_amount <- products$amount[which.min(abs(
  prev_amount$amount - products$amount))]
prev_installment_amount <- products$installment_amount[
  products$period==closest_period & products$amount==closest_amount]


############################################################
### Apply model coefficients according to type of credit ###
############################################################

if (empty_fields>=threshold_empty){
  
  scoring_df$score <- "NULL"
  scoring_df$color <- 2
  
} else if (flag_exclusion==1){
  
  scoring_df$score <- "Bad"
  scoring_df$color <- 1
  
} else if (flag_credirect==1 & flag_beh==1 &	
           !is.na(all_df$max_delay) & all_df$max_delay>=180){	
  
  scoring_df$score <- "Bad"	
  scoring_df$color <- 1	

  
} else if (flag_beh==1 & flag_credirect==0){
  scoring_df <- gen_beh_citycash(df,scoring_df,products,df_Log_beh,period,
                     all_df,prev_amount,amount_tab,
                     t_income,disposable_income_adj)
} else if (flag_beh==1 & flag_credirect==1){
  scoring_df <- gen_beh_credirect(df,scoring_df,products,df_Log_beh,period,
                     all_df,prev_amount,amount_tab,
                     t_income,disposable_income_adj)
} else if (flag_beh==0 & flag_credirect==0){
  scoring_df <- gen_app_citycash(df,scoring_df,products,df_Log_beh,period,
                     all_df,prev_amount,amount_tab,
                     t_income,disposable_income_adj)
} else {
  scoring_df <- gen_app_credirect(df,scoring_df,products,df_Log_beh,period,
                     all_df,prev_amount,amount_tab,
                     t_income,disposable_income_adj)
}


######################################
### Generate final output settings ###
######################################

# Compute previous installment amount and if acceptable differential
for(i in 1:nrow(scoring_df)){
  period_tab <- as.numeric(scoring_df$period[i])
  amount_tab <- as.numeric(scoring_df$amount[i])
  product_tab <- subset(products, products$product_id==all_df$product_id & 
     products$period==as.numeric(period_tab) &
     products$amount==as.numeric(amount_tab))
  installment_amount <- products$installment_amount[
    products$period==period_tab & products$amount==amount_tab]
  scoring_df$installment_amount_diff[i] <- ifelse(
    installment_amount>(1.25*prev_installment_amount), 0, 1)
}

# Generate final correction
get_max_amount <- suppressWarnings(max(scoring_df$amount[scoring_df$score %in% 
    c("Indeterminate","Good 1","Good 2","Good 3","Good 4") &
    scoring_df$installment_amount_diff==1 & scoring_df$color!=1]))
final_amount <- ifelse(is.infinite(get_max_amount), -999, 
    ifelse(get_max_amount<po$min_amount, -999, get_max_amount))


# Update clients_prior_approval table
update_prior_ok_query <- paste("UPDATE ",db_name,".prior_approval_refinances
SET max_amount = ",final_amount," WHERE application_id=",application_id, sep="")
update_prior_not_ok_query <- paste("UPDATE ",db_name,".prior_approval_refinances
SET deleted_at = '",substring(Sys.time(),1,19),
"', status=4 WHERE application_id=",application_id, sep="")

if(final_amount==-999){
  suppressMessages(suppressWarnings(dbSendQuery(con, 
      update_prior_not_ok_query)))
} else {
  suppressMessages(suppressWarnings(dbSendQuery(con,
      update_prior_ok_query)))
}



#######
# END #
#######

