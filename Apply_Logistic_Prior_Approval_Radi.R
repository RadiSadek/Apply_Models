

################################################################################
#            Joint script for Application to TERMINATED CREDITS                #
#      Apply Logistic Regression on all products (CityCash and Credirect)      #
#                          Version 6.0 (2020/01/15)                            #
################################################################################



########################
### Initial settings ###
########################

# Library
suppressMessages(suppressWarnings(library(RMySQL)))
suppressMessages(suppressWarnings(library(here)))
suppressMessages(suppressWarnings(library(dotenv)))
suppressMessages(suppressWarnings(require("reshape")))
suppressMessages(suppressWarnings(library(openxlsx)))


# Database
db_user <- "root"
db_password <- "123456"
db_name <- "citycash_db"
db_host <- "127.0.0.1"
df_port <- 3306
con <- dbConnect(MySQL(), user=db_user, password=db_password, 
                 dbname=db_name, host=db_host, port = df_port)


# Define work directory
main_dir <- "C:\\Projects\\Apply_Scoring\\"


# Read argument of ID
args <- commandArgs(trailingOnly = TRUE)
#approval_id <- args[1]
approval_id <- 43933
product_id <- NA


# Set working directory for input (R data for logistic regression) and output #
setwd(main_dir)


# Load other r files
source(paste(main_dir,"Apply_Models\\Additional_Restrictions.r", sep=""))
source(paste(main_dir,"Apply_Models\\Logistic_App_CityCash.r", sep=""))
source(paste(main_dir,"Apply_Models\\Logistic_App_Credirect_installments.r", 
       sep=""))
source(paste(main_dir,"Apply_Models\\Logistic_App_Credirect_payday.r", sep=""))
source(paste(main_dir,"Apply_Models\\Logistic_Beh_CityCash.r", sep=""))
source(paste(main_dir,"Apply_Models\\Logistic_Beh_Credirect.r", sep=""))
source(paste(main_dir,"Apply_Models\\Useful_Functions.r", sep=""))
source(paste(main_dir,"Apply_Models\\Empty_Fields.r", sep=""))
source(paste(main_dir,"Apply_Models\\Cutoffs.r", sep=""))
source(paste(main_dir,"Apply_Models\\SQL_queries.r", sep=""))
source(paste(main_dir,"Apply_Models\\Disposable_Income.r", sep=""))
source(paste(main_dir,"Apply_Models\\Behavioral_Variables.r", sep=""))
source(paste(main_dir,"Apply_Models\\Normal_Variables.r", sep=""))
source(paste(main_dir,"Apply_Models\\CKR_variables.r", sep=""))


# Load predefined libraries
load("rdata\\citycash_repeat.rdata")
load("rdata\\citycash_app.rdata")
load("rdata\\credirect_installments.rdata")
load("rdata\\credirect_payday.rdata")
load("rdata\\credirect_repeat.rdata")
load("rdata\\credirect_app_fraud.rdata")


####################################
### Read database and build data ###
####################################

# Read credits to score from prior approval - terminated

po_sql_query <- paste("SELECT * FROM ",db_name,
".clients_prior_approval_applications WHERE id=",approval_id)
po <- suppressWarnings(fetch(dbSendQuery(con, po_sql_query), n=-1))


# Get last application_id of terminated or active credit
last_id_query <- paste("SELECT id FROM ",db_name,
".credits_applications WHERE client_id=",po$client_id," AND status IN (4,5)
ORDER BY id DESC LIMIT 1", sep="")
last_id <- suppressWarnings(fetch(dbSendQuery(con, last_id_query), n=-1))
application_id <- last_id$id


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


# Recorrect amount to get highest possible amount
all_df$amount <- po$credit_amount
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
# all_credits <- rbind(all_credits, 
#       all_credits[all_credits$id==application_id,])
# application_id <- application_id + 1 
# all_credits[nrow(all_credits),]$id <- application_id 


# Check if client has a risk profile
risk <- suppressWarnings(fetch(dbSendQuery(con, 
    gen_risky_query(db_name,all_df)), n=-1))


# Read total amount of current credit
total_amount_curr <- suppressWarnings(fetch(dbSendQuery(con, 
    gen_total_amount_curr_query(db_name,application_id)), n=-1))


# Read CKR 
data_ckr_bank <- gen_query_ckr(all_df,all_credits,1)
data_ckr_financial <- gen_query_ckr(all_df,all_credits,2)


# Read all previous active or terminated credits of client
all_id <- subset(all_credits, all_credits$id==application_id | 
    (all_credits$status %in% c(4,5) &
    (all_credits$sub_status!=129 | is.na(all_credits$sub_status)) & 
     all_credits$client_id==all_df$client_id))


# Select for max delay if past AND active: must have at least 30 days of passed
all_id_max_delay <- all_id
all_actives_past <- subset(all_id, all_id$status==4)
if(nrow(all_actives_past)>0){
  all_id_max_delay <- gen_select_relevant_ids_max_delay(db_name,
      all_actives_past,all_id_max_delay)
}


# Generate sum paid and amounts of previous credit 
nrow_all_id <- nrow(all_id)
if (nrow_all_id>=1){
  cash_flow <- gen_last_paid(rbind(all_id,all_id[all_id$id==max(all_id$id),]))
  total_amount <- gen_last_total_amount(
    rbind(all_id,all_id[all_id$id==max(all_id$id),]))
  prev_amount <- gen_last_prev_amount(
    rbind(all_id,all_id[all_id$id==max(all_id$id),]))
  prev_paid_days <- gen_prev_paid_days(rbind(all_id[all_id$id==max(all_id$id),],
    all_id[all_id$id==max(all_id$id),]))
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
all_df$ratio_nb_payments_prev <- ifelse(flag_beh==1,prev_paid_days/
   total_amount$installments,NA)


#  Get SEON variables 
all_df$viber_registered <- ifelse(nrow(gen_seon_phones(
  db_name,7,application_id))>=1,gen_seon_phones(db_name,7,application_id),NA)
all_df$whatsapp_registered <- ifelse(nrow(gen_seon_phones(
  db_name,8,application_id))>=1,gen_seon_phones(db_name,8,application_id),NA)


# Compute and rework CKR variables, suitable for model application
all_df <- gen_ckr_variables(all_df,flag_beh,flag_credirect)


# Compute flag of bad CKR for city cash
flag_bad_ckr_citycash <- ifelse(is.na(all_df$amount_fin),0,
      ifelse(all_df$amount_fin==0, 0,
      ifelse(all_df$outs_overdue_fin/all_df$amount_fin>=0.1 & 
             all_df$status_active_total %in% c(74,75), 1, 0)))


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


# Compute flag if new credirect but old citycash
flag_new_credirect_old_city <- ifelse(flag_credirect==1 & flag_beh==1 &
  nrow(all_credits[all_credits$company_id==2 & all_credits$id!=application_id
 & all_credits$status %in% c(4,5),])==0, 1, 0)


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
  scoring_df <- gen_beh_citycash(df,scoring_df,products,df_Log_beh_CityCash,
                     period,all_df,prev_amount,amount_tab,
                     t_income,disposable_income_adj,prev_installment_amount,1)
} else if (flag_beh==1 & flag_credirect==1 & flag_new_credirect_old_city==0){
  scoring_df <- gen_beh_credirect(df,scoring_df,products,df_Log_beh_Credirect,
                     period,all_df,prev_amount,amount_tab,
                     t_income,disposable_income_adj,1)
} else if (flag_new_credirect_old_city==1 & flag_credit_next_salary==1){
  scoring_df <- gen_app_credirect_payday(df,scoring_df,products,
                 df_Log_Credirect_App_payday,period,all_df,prev_amount,
                 amount_tab,t_income,disposable_income_adj,
                 flag_credit_next_salary)
} else if (flag_new_credirect_old_city==1 & flag_credit_next_salary==0){
  scoring_df <- gen_app_credirect_installments(df,scoring_df,products,
                 df_Log_Credirect_App_installments,period,all_df,
                 prev_amount,amount_tab,t_income,disposable_income_adj,
                 flag_credit_next_salary)
} else if (flag_beh==0 & flag_credirect==0){
  scoring_df <- gen_app_citycash(df,scoring_df,products,df_Log_CityCash_App,
                     period,all_df,prev_amount,amount_tab,
                     t_income,disposable_income_adj)
} else if (flag_beh==0 & flag_credirect==1 & flag_credit_next_salary==1){
  scoring_df <- gen_app_credirect_payday(df,scoring_df,products,
                      df_Log_Credirect_App_payday,period,all_df,prev_amount,
                      amount_tab,t_income,disposable_income_adj,
                      flag_credit_next_salary)
} else {
  scoring_df <- gen_app_credirect_installments(df,scoring_df,products,
                      df_Log_Credirect_App_installments,period,all_df,
                      prev_amount,amount_tab,t_income,disposable_income_adj,
                      flag_credit_next_salary)
}



######################################
### Generate final output settings ###
######################################


# Readjust score when applicable
if(flag_cession==1 & flag_credirect==1){
  scoring_df <- gen_adjust_score(scoring_df, c("Bad","Indeterminate","Good 1"))
}
if(flag_bad_ckr_citycash==1 & flag_credirect==0){
  scoring_df <- gen_adjust_score(scoring_df, c("Bad","Indeterminate"))
}
if(flag_beh==0 & flag_credirect==0){
  scoring_df <- gen_restrict_citycash_app(scoring_df)
}
if(flag_beh==1 & flag_credirect==0){
  scoring_df <- gen_restrict_citycash_beh(scoring_df,prev_amount)
}
if(flag_beh==0 & flag_credirect==1){
  scoring_df <- gen_restrict_credirect_app(scoring_df,all_df,
    flag_credit_next_salary)
}
if(flag_beh==1 & flag_credirect==1 & flag_new_credirect_old_city==0){
  scoring_df <- gen_restrict_credirect_beh(scoring_df,all_df,
    flag_credit_next_salary,flag_new_credirect_old_city)
}
if(flag_beh==1 & flag_credirect==1 & flag_new_credirect_old_city==1){
  scoring_df <- gen_restrict_credirect_app(scoring_df,all_df,
    flag_credit_next_salary)
}


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
    installment_amount>(1.3*prev_installment_amount), 0, 1)
}


# Generate final correction
get_max_amount <- suppressWarnings(max(scoring_df$amount[scoring_df$score %in% 
    c("Good 1","Good 2","Good 3","Good 4") &
    scoring_df$installment_amount_diff==1]))
if(is.infinite(get_max_amount)){
  get_max_installment <- -999
} else {
  get_max_installment <- max(scoring_df$installment_amount[
    scoring_df$installment_amount_diff==1 & scoring_df$amount==get_max_amount])
}
final_amount <- ifelse(is.infinite(get_max_amount), -999, get_max_amount)
final_installment <- get_max_installment
df_final <- as.data.frame(cbind(approval_id, application_id,
                                final_amount, final_installment))


# Get and set fields
df_final$flag_high_last_paid <- all_df$flag_high_last_paid
df_final$credits_cum <- all_df$credits_cum
df_final$max_delay <- all_df$max_delay
df_final$prev_online <- all_df$prev_online
df_final$days_diff_last_credit <- all_df$days_diff_last_credit
df_final$flag_credirect <- flag_credirect
df_final$ratio_prev_payments <- all_df$ratio_nb_payments_prev 


# Output results
final_exists <- read.xlsx(paste(main_dir,
     "results\\scored_credits_terminated.xlsx", sep=""))
df_final <- rbind(final_exists, df_final)
write.xlsx(df_final, 
     file=paste(main_dir,"results\\scored_credits_terminated.xlsx", sep=""))



#######
# END #
#######

