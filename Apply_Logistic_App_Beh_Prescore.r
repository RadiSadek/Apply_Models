
################################################################################
#               Joint script for Application and Behavioral PREscoring         #
#      Apply Logistic Regression on all products (CityCash and Credirect)      #
#                          Version 1.0 (2023/10/13)                            #
################################################################################



########################
### Initial settings ###
########################

# Libraries
suppressMessages(suppressWarnings(library(RMySQL)))
suppressMessages(suppressWarnings(library(here)))
suppressMessages(suppressWarnings(library(dotenv)))
suppressMessages(suppressWarnings(require("reshape")))
suppressMessages(suppressWarnings(require(jsonlite)))
suppressMessages(suppressWarnings(require(gbm)))


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
base_dir <- Sys.getenv("SCORING_PATH", unset = "", names = FALSE)



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
source(file.path(base_dir,"Additional_Restrictions.r"))
source(file.path(base_dir,"Addresses.r"))
source(file.path(base_dir,"Adjust_Scoring_Prior_Approval.r"))
source(file.path(base_dir,"Logistic_App_CityCash.r"))
source(file.path(base_dir,"Logistic_App_Credirect_installments.r"))
source(file.path(base_dir,"Logistic_App_Credirect_payday.r"))
source(file.path(base_dir,"Logistic_App_Credirect_Fraud.r"))
source(file.path(base_dir,"Logistic_Beh_CityCash.r"))
source(file.path(base_dir,"Logistic_Beh_Credirect.r"))
source(file.path(base_dir,"Logistic_CityCash_Prescore.r"))
source(file.path(base_dir,"Useful_Functions.r"))
source(file.path(base_dir,"Empty_Fields.r"))
source(file.path(base_dir,"Cutoffs.r"))
source(file.path(base_dir,"SQL_queries.r"))
source(file.path(base_dir,"Disposable_Income.r"))
source(file.path(base_dir,"Behavioral_Variables.r"))
source(file.path(base_dir,"Normal_Variables.r"))
source(file.path(base_dir,"CKR_variables.r"))
source(file.path(base_dir,"Generate_Adjust_Score.r"))
source(file.path(base_dir,"Gbm_Beh_Credirect.r"))
source(file.path(base_dir,"Gbm_App_CityCash.r"))
source(file.path(base_dir,"Logistic_Beh_Cashpoint.r"))



########################
####### Settings #######
########################

# Load Risky Coordinates
risky_address <- read.csv(file.path(base_dir, "risky_coordinates", 
                    "risky_coordinates.csv"),sep=";")
risky_address_credirect <- read.csv(file.path(base_dir, "risky_coordinates", 
                    "risky_coordinates_credirect.csv"),sep=";")


####################################
### Read database and build data ###
####################################

# Read credits applications
all_df <- gen_query(con,gen_big_sql_query(db_name,application_id))
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
products  <- gen_query(con,gen_products_query(db_name,all_df))
products_desc <- gen_query(con,gen_products_query_desc(db_name,all_df))


# Read all previous credits or applications of client
all_credits <- gen_query(con,gen_all_credits_query(db_name,all_df))
all_credits$date <- ifelse(all_credits$status %in% c(4,5), 
      all_credits$signed_at, all_credits$created_at)


# Get status work 
all_df <- gen_work_data(db_name,all_credits,all_df)


# Check if client has a risk profile
risk <- gen_query(con,gen_risky_query(db_name,all_df))


# Check if client was optisan 
flag_otpisan <- ifelse(nrow(subset(all_credits,
  all_credits$sub_status==133))>0,1,0)


# Check number of varnat 
flag_varnat <- gen_nb_varnat(all_credits)


# Read total amount of current credit
total_amount_curr <- gen_query(con,gen_total_amount_curr_query(
  db_name,application_id))


# Read CKR 
data_ckr_bank <- gen_query_ckr(all_df,all_credits,1,0,db_name)
data_ckr_financial <- gen_query_ckr(all_df,all_credits,2,0,db_name)


# Read all previous active or terminated credits of client
all_id <- subset(all_credits, all_credits$id==application_id | 
    (all_credits$status %in% c(4,5) &
    (!(all_credits$sub_status %in% c(129,122,133)) | 
       is.na(all_credits$sub_status)) & 
     all_credits$client_id==all_df$client_id))
all_id <- subset(all_id, all_id$created_at<=
                 all_id$created_at[all_id$id==application_id])


# Select for max delay if past AND active: must have at least 30 days of passed
all_id_max_delay <- all_id[all_id$id != application_id,]
all_actives_past <- subset(all_id, 
    all_id$id!=application_id & all_id$status==4)
if(nrow(all_actives_past)>0){
  all_id_max_delay <- gen_select_relevant_ids_max_delay(db_name,
      all_actives_past,all_id_max_delay)
}


# Generate variables for payments of previous credits
nrow_all_id <- nrow(all_id)
all_id <- all_id[order(all_id$signed_at),]
if (nrow_all_id>1){
  cash_flow <- gen_last_paid(all_id)
  total_amount <- gen_last_total_amount(all_id)
  prev_amount <- gen_last_prev_amount(all_id)
  prev_paid_days <- gen_prev_paid_days(all_id)
}


# Get correct max days of delay (of relevant previous credits)
nrow_all_id_max_delay <- nrow(all_id_max_delay)
if (nrow_all_id_max_delay>=1){
  list_ids_max_delay <- gen_select_relevant_ids(all_id_max_delay,
     nrow_all_id_max_delay)
  data_plan_main_select <- gen_query(con,
     gen_plan_main_select_query(db_name,list_ids_max_delay))
} 


# Get average expenses according to client's address 
addresses <- gen_query(con,
  gen_address_query(all_df$client_id,"App\\\\Models\\\\Clients\\\\Client"))
if(nrow(addresses)==0){
  addresses <- gen_query(con,
  gen_address_query(all_df$client_id,
  "App\\\\Models\\\\Credits\\\\Applications\\\\Application"))
}
address_data <- gen_coordinates(db_name,application_id,all_df)
all_df$city_pop <- address_data$city_pop
all_df$lat <- address_data$lat
all_df$lon <- address_data$lon
all_df$distance_office <- as.numeric(gen_distance_office(db_name,all_df))


# Get if office is self approval
all_df$self_approval <- gen_query(con,
  gen_self_approval_office_query(db_name,all_df$office_id))$self_approve


# Get dataframe of API data
all_credits$day <- substring(all_credits$created_at,1,10)
same_day <- subset(all_credits,all_credits$day>=as.Date(all_df$created_at) - 1)
tryCatch(
  api_df <- gen_dataframe_json(gen_query(con,
    gen_api_data(db_name,
      same_day$id[same_day$created_at==min(same_day$created_at)]))),
  error=function(e) 
  {api_df <- NA})
if(!exists('api_df')){
  api_df <- NA
}
api_df <- gen_treat_api_df(api_df)



############################################
### Compute and rework additional fields ###
############################################

# Compute flag if credit is up to next salary
flag_credit_next_salary <- ifelse(products_desc$type==4,1,
  ifelse(all_df$product_id %in% c(25:28,36,37,41:44,49,50,55:58,67:68,89:90), 
         1, 0))


# Compute flag if product is credirect
flag_credirect <- ifelse(products_desc$company_id==2, 1, 0)


# Compute flag if product is cashpoint
flag_cashpoint <- ifelse(products_desc$company_id==5, 1, 0)


# Compute flag if product is Finmag
flag_finmag <- ifelse(products_desc$company_id==7, 1, 0)


# Compute flag if client has previous otpisan or tsediran
flag_exclusion <- gen_flag_exclusion(all_credits,flag_credirect,risk,all_df)


# Get and rename columns for CKR variables
all_df <- cbind(all_df, data_ckr_financial)
names(all_df)[(ncol(all_df)-11):ncol(all_df)] <- c("ckr_cur_fin","ckr_act_fin",
   "ckr_fin_fin",	"src_ent_fin","amount_fin","cred_count_fin",
   "outs_principal_fin","outs_overdue_fin","cession_fin",
   "monthly_installment_financial","codebtor_fin","guarantor_fin")
all_df <- cbind(all_df, data_ckr_bank)	
names(all_df)[(ncol(all_df)-11):ncol(all_df)] <- c("ckr_cur_bank",
   "ckr_act_bank","ckr_fin_bank","src_ent_bank","amount_bank","cred_count_bank",
   "outs_principal_bank","outs_overdue_bank","cession_bank",
   "monthly_installment_bank","codebtor_bank","guarantor_bank")


# Set period variable (monthly, twice weekly, weekly)
period <- products_desc$period


# Compute and generate general variables
all_df <- gen_norm_var(period,all_df,products,1)


# Compute and generate variables specific for behavioral model
data_plan_main_select_def <- ifelse(exists("data_plan_main_select"),
          data_plan_main_select,NA)
all_df <- gen_other_rep(nrow_all_id,all_id,all_df,flag_credirect,
                        data_plan_main_select_def,application_id)


# Get flag if credit is behavioral or not
flag_beh <- ifelse(all_df$credits_cum==0, 0, 1)
flag_rep <- ifelse(nrow(subset(all_id,all_id$status==5))>0,1,0)


# Correct days since last default if necessary
if(flag_beh==1){
  flag_app_quickly <- gen_all_days_since_credit(db_name,all_credits,all_df)
  all_df$days_diff_last_credit <- 
    ifelse(is.na(all_df$days_diff_last_credit),all_df$days_diff_last_credit,
    ifelse(flag_app_quickly==1 & flag_credirect==1,0,
    all_df$days_diff_last_credit))
} else {
  all_df$days_diff_last_credit <- NA
  flag_app_quickly <- NA
}
if(flag_cashpoint==1){
  all_df$days_diff_last_credit <- 30
}


# Compute ratio of number of payments
all_df$ratio_nb_payments_prev <- ifelse(flag_beh==1,prev_paid_days/	
       total_amount$installments,NA)


# Compute ratio of refinanced
all_df$refinance_ratio <- ifelse(flag_beh==1,
       gen_ratio_refinance_previous(db_name,all_id),NA)


# Compute and rework CKR variables, suitable for model application
all_df <- gen_ckr_variables(all_df,flag_beh,flag_credirect)


# Compute flag of bad CKR for city cash
flag_bad_ckr_citycash <- ifelse(is.na(all_df$amount_fin),0,
    ifelse(all_df$amount_fin==0, 0,
    ifelse(all_df$outs_overdue_fin/all_df$amount_fin>=0.1 & 
           all_df$status_active_total %in% c(74,75), 1, 0)))


# Compute if previous is online 
all_id <- get_company_id_prev(db_name,all_id)
all_df <- gen_prev_online(db_name,all_id,all_df,application_id)


# Get flag if credit is behavioral but with same company
flag_beh_company <- ifelse(
  nrow(all_id[all_id$company_id==
       all_id$company_id[all_id$id==application_id] & 
       !(all_id$big_company_id %in% c(4)),])>1,1,0)


# Compute flag if last paid credit is maybe hidden refinance
all_df <- gen_ratio_last_amount_paid(db_name,all_id,all_df,application_id,
  products_desc,nrow_all_id,cash_flow,total_amount)


# Compute amount differential 
all_df$amount_diff <- ifelse(nrow_all_id<=1, NA, all_df$amount - 
                               prev_amount$amount)


# Get if previous from other brand
all_df$prev_other_brand <- gen_prev_other_brand(db_name,all_id,all_df,
    application_id)


# Compute income variables
t_income <- gen_t_income(db_name,application_id,period)
disposable_income_adj <- gen_disposable_income_adj(db_name,application_id,
        all_df,addresses,period)
all_df$total_income <- gen_income(db_name,application_id)


# Read relevant product amounts (not superior to amount of application)
products <- subset(products, products$amount<=all_df$amount)


# Prepare final dataframe
scoring_df <- gen_final_df(products,application_id)[
  gen_final_df(products,application_id)$period==all_df$installments & 
  gen_final_df(products,application_id)$amount==all_df$amount,]


# Make back-up dataframe
df <- all_df


# Correct empty and missing value fields (to standard format)
df <- gen_null_to_na(df)


# Get if empty field threshold is passed
empty_fields <- sum(is.na(df[,c("age","gender","status_finished_total",
  "outs_overdue_ratio_total")]))
threshold_empty <- 3


# Readjust fields
df <- gen_norm_var2(df)


# Compute flag exclusion for cession in CKR
flag_cession <- ifelse(flag_credirect==1 & df$amount_cession_total>0, 1, 0)


# Compute flag if new credirect but old citycash
flag_new_credirect_old_city <- ifelse(flag_credirect==1 & flag_beh==1 &
 nrow(all_id[all_id$company_id==2 & all_id$id!=application_id
      & all_id$status %in% c(4,5),])==0, 1, 0)


# Compute flag if has current active
if(flag_beh_company==1){
  flag_active <- gen_flag_if_curr_active(all_id,application_id)
} else {
  flag_active <- cbind(NA,NA)
}


# Get flag if client is dead
flag_is_dead <- ifelse(is.na(gen_query(con,
 gen_flag_is_dead(db_name,all_df$client_id))$dead_at),0,1)


# Get flag if client is in a risky address
flag_risky_address <- gen_flag_risky_address(db_name,application_id,
  risky_address,risky_address_credirect,all_df,flag_credirect)
df$risky_address <- flag_risky_address$flag_risky_address


# Compute flag if judicial 
flag_judicial <- ifelse(
    nrow(subset(all_credits,!is.na(all_credits$judicial_date)))>0,1,
  ifelse(!is.na(gen_query(con,gen_flag_judges_us(
    db_name,all_df$client_id))$judge_us_at),1,0))


# Compute flag if third side
flag_third_side <- gen_third_side_prev(db_name,all_id,application_id)


# Compute flag if parallel 
flag_parallel <- gen_flag_parallel(db_name,all_id,flag_cashpoint)



############################################################
### Apply model coefficients according to type of credit ###
############################################################

scoring_df <- gen_apply_score(
  empty_fields,threshold_empty,flag_exclusion,
  flag_varnat,flag_is_dead,flag_credit_next_salary,flag_credirect,
  flag_beh,all_df,scoring_df,df,products,df_Log_beh_CityCash,
  df_Log_CityCash_App,df_Log_beh_Credirect,df_Log_Credirect_App_installments,
  df_Log_Credirect_App_payday,period,all_id,prev_amount,amount_tab,
  t_income,disposable_income_adj,flag_new_credirect_old_city,api_df,
  flag_judicial,0,flag_third_side,flag_cashpoint,base_dir,1,flag_otpisan,
  flag_finmag)


# Build column PD
if(!("pd" %in% names(scoring_df))){
  scoring_df$pd <- NA
}


######################################
### Generate final output settings ###
######################################

# Generate scoring dataframe
scoring_df$created_at <- Sys.time()
scoring_df <- scoring_df[,c("application_id","amount","period","score","color",
                            "pd","created_at")]


# Readjust score when applicable
scoring_df$amount <- ifelse(scoring_df$amount>=3000 & 
   all_df$product_id==22,3000,scoring_df$amount)
scoring_df <- gen_apply_policy(scoring_df,flag_credirect,flag_cession,
   flag_bad_ckr_citycash,all_df,all_id,flag_beh,prev_amount,products,
   application_id,flag_new_credirect_old_city,flag_credit_next_salary,
   flag_beh_company,flag_cashpoint,0,fraud_flag,flag_risky_address,
   flag_parallel,flag_finmag)


# Apply repeat restrictions to refinances and with potential refinance
if(flag_beh_company==1){
  if((max(flag_active)==1 & flag_credirect==0) | 
     (flag_active[1]==1 & flag_credirect==1)){
    scoring_df <- gen_restrict_beh_refinance(db_name,all_df,all_id,
      scoring_df,flag_active,application_id,flag_credirect,flag_cashpoint)
  }
}


# Reselect columns 
scoring_df <- scoring_df[,c("application_id","amount","period","score","color",
                            "pd","created_at")]


# Correct for prior approval offers
scoring_decision <- 10
scoring_df <- gen_correction_po_fct(con,db_name,all_df,all_id,
     scoring_df,products,period,application_id,scoring_decision)[[1]]


# Check if early paid previous credit : no offer for City Cash
if(flag_beh_company==1 & flag_credirect==0){
   scoring_df <- gen_corection_early_repaid(con,db_name,scoring_df,
    all_df,all_id,flag_credit_next_salary,flag_cashpoint)
}


# Reselect columns 
scoring_df <- scoring_df[,c("application_id","amount","period","score","color",
                            "pd","created_at")]


# Finalize display table
scoring_df <- gen_final_table_display(scoring_df,flag_credirect)[,
  c("application_id","score","pd","created_at")]
colnames(scoring_df)[which(names(scoring_df) == "score")] <- "prescore"
scoring_df$id <- gen_query(con,paste("SELECT max(id) AS max_id FROM ",db_name,
  ".credits_applications_prescore",sep=""))$max_id + 1
scoring_df$id <- ifelse(is.na(scoring_df$id),1,scoring_df$id)
scoring_df$pd <- ifelse(is.na(scoring_df$pd),0.999,scoring_df$pd)


# Update table credits applications prescore
write_sql_query <- paste("
  DELETE FROM ",db_name,".credits_applications_prescore WHERE application_id=",
  application_id, sep="")
suppressMessages(dbSendQuery(con,write_sql_query))
append_query <- paste("INSERT INTO ",db_name,
 ".credits_applications_prescore VALUES ",
 paste("(",paste(scoring_df$id,",",scoring_df$application_id,",",
 "'",scoring_df$prescore,"',",scoring_df$pd,",'",scoring_df$created_at,"'",
 sep=""),")",sep=""),sep="")
suppressWarnings(tryCatch({
  suppressMessages(suppressWarnings(dbSendQuery(con,append_query)))
}, error=function(e){
  scoring_df$id <- scoring_df$id + 1
  append_query <- paste("INSERT INTO ",db_name,
   ".credits_applications_prescore VALUES ",
   paste("(",paste(scoring_df$id,",",scoring_df$application_id,",",
   "'",scoring_df$prescore,"',",scoring_df$pd,",'",scoring_df$created_at,"'",
   sep=""),")",sep=""),sep="")
  suppressMessages(suppressWarnings(dbSendQuery(con,append_query)))
}))




#######
# END #
#######

