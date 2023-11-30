

################################################################################
#               Joint script for Application and Behavioral scoring            #
#      Apply Logistic Regression on all products (CityCash and Credirect)      #
#                          Version 7.0 (2020/04/08)                            #
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
source(file.path(base_dir,"Useful_Functions.r"))
source(file.path(base_dir,"Empty_Fields.r"))
source(file.path(base_dir,"Cutoffs.r"))
source(file.path(base_dir,"SQL_queries.r"))
source(file.path(base_dir,"Disposable_Income.r"))
source(file.path(base_dir,"Behavioral_Variables.r"))
source(file.path(base_dir,"Normal_Variables.r"))
source(file.path(base_dir,"CKR_variables.r"))
source(file.path(base_dir,"Generate_Adjust_Score.r"))
source(file.path(base_dir,"Terminated.r"))



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
suppressWarnings(tryCatch({
all_df <- cbind(application_id,
  gen_dataframe_json(gen_query(con,gen_api_score_query(db_name,
   application_id))$request_payload),gen_query(con,gen_api_score_query(db_name,
   application_id))$created_at)
names(all_df)[ncol(all_df)] <- c("created_at")
all_df <- gen_col_names_api(all_df)
all_df$purpose <- NA


# Score according to whether the client is new or repeat in CityCash/Credirect
if(nrow(gen_query(con,gen_client_id_query(db_name,all_df)))>0){
  
  all_df$client_id <- gen_query(con,gen_client_id_query(db_name,all_df))$id
  all_credits <- gen_query(con,gen_all_credits_query(db_name,all_df))
  all_id <- subset(all_credits,(all_credits$status %in% c(4,5) &
   (!(all_credits$sub_status %in% c(129,122,133)) | 
    is.na(all_credits$sub_status)) & all_credits$client_id==all_df$client_id))
  all_df$purpose <- NA
  
  # Score with usual scorecard if repeat client
  if(nrow(all_id)>0){
    last_id <- all_id$id[all_id$id==all_id$id[all_id$signed_at==
        max(all_id$signed_at)]]
    product_id <- NA
    score_calc <- gen_terminated_fct(con,
        all_df$client_id,product_id,last_id,0)[[3]]
    score_calc <- ifelse(is.na(score_calc),"Bad",score_calc)
    scoring_df <- all_df
    scoring_df$score <- score_calc
    scoring_df$pd <- 0.5
    gen_score <- 1
  } else {gen_score <- 0}
} else { gen_score <- 0
}

if(gen_score==0){
  
  # Score if completely new 
  empty_fields <- sum(is.na(all_df[,c("gender","age","education","ownership",
    "status_work","marital_status","outs_overdue_ratio_total")]))
  threshold_empty <- 6
  scoring_df <- all_df
  df <- all_df
  products <- as.data.frame(cbind(NA,NA,NA))
  names(products) <- c("product_id","period","amount")
  all_id <- all_df
  api_df <- as.data.frame(cbind(NA,NA,NA,NA,NA))
  names(api_df) <- c("email","payment_method","amount","referral_source",
     "user_agent")
  scoring_df$amount <- 1000
  scoring_df$installments <- 1
  scoring_df <- gen_apply_score(
    empty_fields,threshold_empty,0,0,0,1,1,0,all_df,scoring_df,df,products,
    df_Log_beh_CityCash,df_Log_CityCash_App,df_Log_beh_Credirect,
    df_Log_Credirect_App_installments,df_Log_Credirect_App_payday,NA,all_id,NA,
    NA,NA,NA,0,api_df,0,0,0,0,base_dir,0)
}



######################################
### Generate final output settings ###
######################################

# Generate scoring dataframe
scoring_df$result_at <- Sys.time()

# Export to json file
scoring_df$id <- application_id
scoring_df$cutoff <- 0.35
setjson_score <- toJSON(scoring_df[,c("score","pd","cutoff")])
setjson_table <- toJSON(gen_table_api_scoring(scoring_df$score))
}, error=function(e){}))

# Treat if error
if(!exists("scoring_df")){
  setjson_score <- toJSON("error")
  setjson_table <- toJSON("error")
  # Write in database
  change_query <- paste("UPDATE ",db_name,
    ".api_scoring SET status = 999, result_at = '",
    substring(Sys.time(),1,19),"', result = '",
    setjson_table,"', result_internal = '",setjson_score,"' WHERE id=",
    application_id,sep="")
  suppressMessages(suppressWarnings(dbSendQuery(con,change_query)))
} else {
  # Write in database
  change_query <- paste("UPDATE ",db_name,
    ".api_scoring SET status = 1, result_at = '",
    substring(Sys.time(),1,19),"', result = '",
    setjson_table,"', result_internal = '",setjson_score,"' WHERE id=",
    application_id,sep="")
  suppressMessages(suppressWarnings(dbSendQuery(con,change_query)))
}



#######
# END #
#######

