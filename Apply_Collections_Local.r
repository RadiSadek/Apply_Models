rm(list=ls())
gc()
start_t <- Sys.time()
################################################################################
#                       Script for applying Collections model                  #
#    Apply Multinomial GBM to credits in buckets (CityCash)                    #
#                          Version 1.0 (2024/11/12)                            #
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
suppressMessages(suppressWarnings(require(gbm)))
suppressMessages(suppressWarnings(library(dplyr)))
suppressMessages(suppressWarnings(library(lubridate)))


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
base_dir <- "C:/Projects/Apply_Scoring"


# Set working directory for input (R data for logistic regression) and output #
setwd(base_dir)


# Load other r files
source(paste(base_dir,"/Apply_Models/Useful_Functions_Radi.r", sep=""))
source(paste(base_dir,"/Apply_Models/Empty_Fields.r", sep=""))
source(paste(base_dir,"/Apply_Models/Cutoffs.r", sep=""))
source(paste(base_dir,"/Apply_Models/SQL_queries.r", sep=""))
source(paste(base_dir,"/Apply_Models/Disposable_Income.r", sep=""))
source(paste(base_dir,"/Apply_Models/Behavioral_Variables.r", sep=""))
source(paste(base_dir,"/Apply_Models/Normal_Variables.r", sep=""))
source(paste(base_dir,"/Apply_Models/CKR_variables.r", sep=""))
source(paste(base_dir,"/Apply_Models/Multinomial_GBM_Collections.r", sep=""))

####################################
### Read database and build data ###
####################################

# Get all applications in CityCash DPD buckets
initial_ids <- gen_query(con, gen_bucket_credits_query(db_name, c(1,2)))
initial_ids$dpd_date <- Sys.Date()

# Remove clients with dpd irrelevant to the brand buckets
initial_ids <- initial_ids %>%
  filter((brand_id == 1 & days_delay >= 10) | (brand_id == 2 & days_delay >= 1))

# Add the closest dpd group for model selection
dpds <- c(10, 30, 60, 90, 180, 360)

initial_ids$dpd_group <- sapply(seq_len(nrow(initial_ids)), function(i) {
  current_dpds <- if (initial_ids$brand_id[i] == 2) c(1, dpds) else dpds
  current_dpds[which.min(abs(current_dpds - initial_ids$days_delay[i]))]
})

# Add the closest (lower) dpd group
initial_ids$lower_dpd <- sapply(seq_len(nrow(initial_ids)), function(i) {
  current_dpds <- if (initial_ids$brand_id[i] == 2) c(1, dpds) else dpds
  current_dpds[max(which(current_dpds <= initial_ids$days_delay[i]))]
})

# Read credits applications
all_df <- gen_query(con,gen_big_sql_query(db_name,initial_ids))
all_df <- gen_time_format(all_df)
all_df <- merge(initial_ids, all_df, by = "application_id", all.x = T)
all_df <- gen_genage(all_df)
all_df <- gen_credit_history(db_name, all_df, c(1,2))

# Read & process CKR data
all_credits <- data.frame()
data_ckr_bank <- gen_query_ckr(all_df,all_credits,1,0,1,db_name)
colnames(data_ckr_bank) <- c("application_id", "ckr_cur_bank",
"ckr_act_bank","ckr_fin_bank","src_ent_bank","amount_bank","cred_count_bank",
"outs_principal_bank","outs_overdue_bank","cession_bank",
"monthly_installment_bank","codebtor_bank","guarantor_bank")
all_df <- merge(all_df, data_ckr_bank, by = "application_id", all.x = T)

data_ckr_financial <- gen_query_ckr(all_df,all_credits,2,0,1,db_name)
colnames(data_ckr_financial) <- c("application_id","ckr_cur_fin",
"ckr_act_fin","ckr_fin_fin",	"src_ent_fin","amount_fin","cred_count_fin",
"outs_principal_fin","outs_overdue_fin","cession_fin",
"monthly_installment_financial","codebtor_fin","guarantor_fin")
all_df <- merge(all_df, data_ckr_financial, by = "application_id", all.x = T)

all_df <- gen_ckr_variables(all_df,flag_beh = NA,flag_credirect = NA)

# Gen call history
all_df <- gen_call_history(db_name, all_df)

# Gen scores
scoring <- gen_query(con, gen_scores_query(db_name, all_df))
all_df <- merge(all_df, scoring, by = "application_id", all.x = T)

# Gen payment ratio
all_df <- gen_payment_ratio(db_name, all_df)

# Gen passed installments before DPD group/All installments ratio
all_df <- gen_default_inst_ratio(db_name, all_df)

Sys.time()-start_t

# Apply model ----
start_t <- Sys.time()
ptp <- gen_ptp(all_df, cu_collections_citycash, cu_collections_credirect)
Sys.time()-start_t

ptp <- merge(ptp, all_df[,c("application_id", "bc_id")], 
            by = "application_id", all.x = T)
ptp$id <- ptp$bc_id

# Write to DB ----

if (nrow(ptp) > 0) {
  # Split the dataframe into chunks of batch_size
  batch_size <- 5000
  total_rows <- nrow(ptp)
  batches <- split(ptp, ceiling(seq_along(1:total_rows) / batch_size))
  i <- 0
  
  # Loop through each batch and construct queries
  for (batch in batches) {
    i <- i+1
    col_ptp <- update_multiple_rows(batch, batch$collections_ptp, 
    "collections_ptp", "call_center_buckets_credits", db_name)
    col_cat <- update_multiple_rows(batch, batch$collections_category, 
    "collections_category","call_center_buckets_credits", db_name)
    ids <- paste0(batch$id, collapse = ", ")
    print(paste0("Updated batch ", i))
  }
}

# Export ptp dataframe for testing purporses ----
ptp$exported_at <- Sys.time()
