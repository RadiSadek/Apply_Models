

################################################################################
#               Joint script for Application and Behavioral scoring            #
#      Apply Logistic Regression on all products (CityCash and Credirect)      #
#                          Version 7.0 (2020/04/08)                            #
################################################################################



########################
### Initial settings ###
########################

# Library
suppressMessages(suppressWarnings(library(RMariaDB)))
suppressMessages(suppressWarnings(library(DBI)))
suppressMessages(suppressWarnings(library(Rcpp)))
#suppressMessages(suppressWarnings(library(RMySQL)))
suppressMessages(suppressWarnings(library(here)))
suppressMessages(suppressWarnings(library(dotenv)))
suppressMessages(suppressWarnings(require("reshape")))
suppressMessages(suppressWarnings(library(openxlsx)))
suppressMessages(suppressWarnings(require(jsonlite)))


# Database
db_name <- "citycash"
con <- dbConnect(RMariaDB::MariaDB(),dbname = "citycash",host ="192.168.2.110",
  port = 3306,user = "userro1",password = "DHng_2pg5zdL0yI9x@")


# Define work directory
base_dir <- "C:/Projects/Apply_Scoring"


# Read argument of ID
args <- commandArgs(trailingOnly = TRUE)
#application_id <- args[1]
application_id <- 33
product_id <- NA


# Set working directory for input (R data for logistic regression) and output #
setwd(base_dir)


# Load other r files
source(paste(base_dir,"/Apply_Models/Additional_Restrictions.r", sep=""))
source(paste(base_dir,"/Apply_Models/Addresses.r", sep=""))
source(paste(base_dir,"/Apply_Models/Adjust_Scoring_Prior_Approval.r", sep=""))
source(paste(base_dir,"/Apply_Models/Logistic_App_CityCash.r", sep=""))
source(paste(base_dir,"/Apply_Models/Logistic_App_Credirect_installments.r", 
       sep=""))
source(paste(base_dir,"/Apply_Models/Logistic_App_Credirect_payday.r", sep=""))
source(paste(base_dir,"/Apply_Models/Logistic_App_Credirect_Fraud.r", sep=""))
source(paste(base_dir,"/Apply_Models/Logistic_Beh_CityCash.r", sep=""))
source(paste(base_dir,"/Apply_Models/Logistic_Beh_Credirect.r", sep=""))
source(paste(base_dir,"/Apply_Models/Useful_Functions_Radi.r", sep=""))
source(paste(base_dir,"/Apply_Models/Empty_Fields.r", sep=""))
source(paste(base_dir,"/Apply_Models/Cutoffs.r", sep=""))
source(paste(base_dir,"/Apply_Models/SQL_queries.r", sep=""))
source(paste(base_dir,"/Apply_Models/Disposable_Income.r", sep=""))
source(paste(base_dir,"/Apply_Models/Behavioral_Variables.r", sep=""))
source(paste(base_dir,"/Apply_Models/Normal_Variables.r", sep=""))
source(paste(base_dir,"/Apply_Models/CKR_variables.r", sep=""))
source(paste(base_dir,"/Apply_Models/Generate_Adjust_Score.r", sep=""))
source(paste(base_dir,"/Apply_Models/Terminated_Radi.r", sep=""))

# Load Risky Coordinates
risky_address <- read.csv("risky_coordinates\\risky_coordinates.csv",sep=";")
risky_address_credirect <- read.csv(
  "risky_coordinates\\risky_coordinates_credirect.csv",sep=";")



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
        all_df$client_id,product_id,last_id,0,db_name,0)[[3]]
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
  
  # Set all variables needed for model 
  all_df$age <- ifelse(is.null(all_df$age),NA,as.numeric(all_df$age))
  all_df$gender <- ifelse(is.null(all_df$gender),NA,all_df$gender)
  all_df$ownership <- ifelse(is.null(all_df$ownership),NA,all_df$ownership)
  all_df$education <- ifelse(is.null(all_df$education),NA,all_df$education)
  all_df$household_total <- ifelse(is.null(all_df$household_total),NA,
    as.numeric(all_df$household_total))
  all_df$purpose <- ifelse(is.null(all_df$purpose),NA,all_df$purpose)
  all_df$marital_status <- ifelse(is.null(all_df$marital_status),NA,
    all_df$marital_status)
  all_df$experience_employer <- ifelse(is.null(all_df$experience_employer),NA,
    as.numeric(all_df$experience_employer))
  all_df$status_work <- ifelse(is.null(all_df$status_work),NA,
    all_df$status_work)
  all_df$status_finished_total <- ifelse(is.null(all_df$status_finished_total),
    NA,all_df$status_finished_total)
  all_df$outs_overdue_ratio_total <- ifelse(is.null(
    all_df$outs_overdue_ratio_total),NA,
    as.numeric(all_df$outs_overdue_ratio_total))
  all_df$has_viber <- ifelse(is.null(all_df$has_viber),NA,all_df$has_viber)
  all_df$city_pop <- ifelse(is.null(all_df$city_pop),NA,
    as.numeric(all_df$city_pop))
  all_df$phone_plan <- ifelse(is.null(all_df$phone_plan),NA,all_df$phone_plan)
  all_df$leasing <- ifelse(is.null(all_df$leasing),NA,all_df$leasing)
  
  # Make additional settings
  df <- all_df
  products <- as.data.frame(cbind(NA,NA,NA))
  names(products) <- c("product_id","period","amount")
  all_id <- all_df
  api_df <- as.data.frame(cbind(NA,NA,NA,NA,NA,NA))
  names(api_df) <- c("email","payment_method","amount","referral_source",
     "user_agent","device_type")
  scoring_df$amount <- 1000
  scoring_df$installments <- 1
  scoring_df <- gen_apply_score(
    empty_fields,threshold_empty,0,0,0,1,1,0,all_df,scoring_df,df,products,
    df_Log_beh_CityCash,df_Log_CityCash_App,df_Log_beh_Credirect,
    df_Log_Credirect_App_installments,df_Log_Credirect_App_payday,NA,all_id,NA,
    NA,NA,NA,0,api_df,0,0,0,0,base_dir,0,0,0)
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
#final_df <- as.data.frame(cbind(0.35,setjson_table))
# names(final_df) <- c("cutOff","scoreTable")
# setjson_table <- toJSON(final_df)
# setjson_table <- gsub("\\\\","",setjson_table)
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
    setjson_table,"' WHERE id=",application_id,sep="")
  change_query <- gsub("\\\\","",change_query,fixed=TRUE)
  suppressMessages(suppressWarnings(dbSendQuery(con,change_query)))
}


#######
# END #
#######

