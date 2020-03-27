

################################################################################
#               Joint script for Application and Behavioral scoring            #
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
#application_id <- args[1]
application_id <- 575398
product_id <- NA


# Set working directory for input (R data for logistic regression) and output #
setwd(main_dir)


# Load other r files
source(paste(main_dir,"Apply_Models\\Logistic_App_CityCash.r", sep=""))
source(paste(main_dir,"Apply_Models\\Logistic_App_Credirect.r", sep=""))
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
load("rdata\\all_repeat.rdata")
load("rdata\\citycash_app.rdata")
load("rdata\\credirect_app.rdata")
load("rdata\\credirect_repeat.rdata")


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
all_id <- subset(all_id, all_id$date<=all_id$date[all_id$id==application_id])


# Select for max delay if past AND active: must have at least 30 days of passed
all_id_max_delay <- all_id[all_id$id != application_id,]
all_actives_past <- subset(all_id, 
    all_id$id!=application_id & all_id$status==4)
if(nrow(all_actives_past)>0){
  all_id_max_delay <- gen_select_relevant_ids_max_delay(db_name,all_actives,
      all_id_max_delay)
}


# Generate sum paid and amounts of previous credit 
nrow_all_id <- nrow(all_id)
if (nrow_all_id>1){
  cash_flow <- gen_last_paid(all_id)
  total_amount <- gen_last_total_amount(all_id)
  prev_amount <- gen_last_prev_amount(all_id)
  closest_period <- products$period[which.min(
    abs(total_amount$installments - products$period))]
  closest_amount <- products$amount[which.min(abs(
    prev_amount$amount - products$amount))]
  prev_installment_amount <- products$installment_amount[
    products$period==closest_period & products$amount==closest_amount]
}


# Get correct max days of delay (of relevant previous credits)
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


# Get and rename columns for CKR variables
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
all_df <- gen_norm_var(period,all_df,products,1)


# Compute and generate variables specific for behavioral model
data_plan_main_select_def <- ifelse(exists("data_plan_main_select"),
          data_plan_main_select,NA)
all_df <- gen_other_rep(nrow_all_id,all_id,all_df,flag_credirect,
                        data_plan_main_select_def)


# Get flag if credit is behavioral or not
flag_beh <- ifelse(all_df$credits_cum==0, 0, 1)


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
all_df <- gen_prev_online(db_name, all_credits,all_df,application_id)


# Get flag if credit is behavioral but with same company
flag_beh_company <- ifelse(
  nrow(all_credits[all_credits$company_id==
       all_credits$company_id[all_credits$id==application_id],])>1,1,0)


# Compute flag if last paid credit is maybe hidden refinance
all_df <- gen_ratio_last_amount_paid(db_name,all_credits,all_df)


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
                     t_income,disposable_income_adj,prev_installment_amount)
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

# Generate scoring dataframe
scoring_df$created_at <- Sys.time()
scoring_df <- scoring_df[,c("application_id","amount","period","score","color",
                            "created_at")]


# Readjust score when applicable
if(flag_cession==1 & flag_credirect==1){
  scoring_df <- gen_adjust_score(scoring_df, 
        c("Bad","Indeterminate","Good 1"))
}
if(flag_bad_ckr_citycash==1 & flag_credirect==0){
  scoring_df <- gen_adjust_score(scoring_df, c("Bad","Indeterminate"))
}

# Create output dataframe
final <- as.data.frame(cbind(scoring_df$application_id[1],
   scoring_df$score[scoring_df$amount== unique(scoring_df$amount)
            [which.min(abs(all_df$amount - unique(scoring_df$amount)))]
                   & 
   scoring_df$period==unique(scoring_df$period)
            [which.min(abs(all_df$installments - unique(scoring_df$period)))]]))
names(final) <- c("id","score")
# final <- as.data.frame(cbind(
#    scoring_df$application_id[1],
#    scoring_df$score[scoring_df$amount==unique(scoring_df$amount)
#      [which.min(abs(all_df$amount - unique(scoring_df$amount)))]
#                        & 
#    scoring_df$period==unique(scoring_df$period)
#      [which.min(abs(all_df$installments - unique(scoring_df$period)))]],
#    scoring_df$PD[scoring_df$amount==unique(scoring_df$amount)
#      [which.min(abs(all_df$amount - unique(scoring_df$amount)))]
#                     & 
#    scoring_df$period==unique(scoring_df$period)
#      [which.min(abs(all_df$installments - unique(scoring_df$period)))]]))
# names(final) <- c("id","score","PD")
final$flag_beh <- flag_beh
final$flag_credirect <- flag_credirect
final$flag_next_salary <- flag_credit_next_salary
final$flag_exclusion <- flag_exclusion
final$flag_bad_ckr_citycash <- flag_bad_ckr_citycash
final$status_active_total <- all_df$status_active_total
final$status_finished_total <- all_df$status_finished_total
final$outs_overdue_ratio_total <- all_df$outs_overdue_ratio_total
final$amount_fin <- all_df$amount_fin
final$outs_overdue_fin <- all_df$outs_overdue_fin

all_df$installment_amount <- products[
  products$period == unique(products$period)
  [which.min(abs(all_df$installments - unique(products$period)))] & 		
    products$amount == unique(products$amount)
  [which.min(abs(all_df$amount - unique(products$amount)))] & 		
    products$product_id == all_df$product_id, ]$installment_amount

# Read and write
final_exists <- read.xlsx(paste(main_dir,"results\\scored_credits.xlsx", 
                                sep=""))
final <- rbind(final_exists, final)
write.xlsx(final, paste(main_dir,"results\\scored_credits.xlsx", sep=""))



#######
# END #
#######

