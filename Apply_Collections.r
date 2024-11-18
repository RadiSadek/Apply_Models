################################################################################
#                       Script for applying Collections model                  #
#    Apply Multinomial GBM to credits in buckets (CityCash)                    #
#                          Version 1.0 (2024/11/12)                            #
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
suppressMessages(suppressWarnings(require(jsonlite)))
suppressMessages(suppressWarnings(require(gbm)))
suppressMessages(suppressWarnings(library(dplyr)))
suppressMessages(suppressWarnings(library(lubridate)))

# Defines the directory where custom .env file is located
load_dot_env(file = here('.env'))


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
source(file.path(base_dir,"Useful_Functions.r"))
source(file.path(base_dir,"Empty_Fields.r"))
source(file.path(base_dir,"Cutoffs.r"))
source(file.path(base_dir,"SQL_queries.r"))
source(file.path(base_dir,"Disposable_Income.r"))
source(file.path(base_dir,"Behavioral_Variables.r"))
source(file.path(base_dir,"Normal_Variables.r"))
source(file.path(base_dir,"CKR_variables.r"))
source(file.path(base_dir,"Multinomial_GBM_Collections.r"))


####################################
### Read database and build data ###
####################################

# Get all applications in CityCash DPD buckets
initial_ids <- gen_query(con, gen_bucket_credits_query(db_name, c(1)))
initial_ids$dpd_date <- Sys.Date()

# Remove clients with dpd irrelevant to the brand buckets
initial_ids <- initial_ids %>%
  filter(brand_id == 1 & days_delay >= 10)

# Add the closest dpd group for model selection
dpds <- c(10, 30, 60, 90, 180, 360)
initial_ids$dpd_group <- sapply(initial_ids$days_delay,
                                function(x) dpds[which.min(abs(dpds - x))])

# Add the closest (lower) dpd group
initial_ids$lower_dpd <- sapply(initial_ids$days_delay, function(x) {
  dpds[max(which(dpds <= x))]
})

# Read credits applications
all_df <- gen_query(con,gen_big_sql_query(db_name,initial_ids))

all_df <- gen_time_format(all_df)
all_df <- merge(initial_ids, all_df, by = "application_id", all.x = T)
all_df <- gen_genage(all_df)
all_df <- gen_credit_history(db_name, all_df, c(1))

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

# Apply model ----
ptp <- gen_ptp(all_df, cu_collections_citycash)
ptp <- merge(ptp, all_df[,c("application_id", "bc_id")], 
            by = "application_id", all.x = T)
ptp$id <- ptp$bc_id

# Write to DB ----
con <- dbConnect(MySQL(), user=db_username, 
                 password=db_password, dbname=db_name, 
                 host=db_host, port = db_port)
sqlMode <- paste("SET sql_mode=''", sep ="")
suppressWarnings(fetch(dbSendQuery(con, sqlMode), 
                       n=-1))

if (nrow(ptp) > 0) {
  # Split the dataframe into chunks of batch_size
  batch_size <- 5000
  total_rows <- nrow(ptp)
  batches <- split(ptp, ceiling(seq_along(1:total_rows) / batch_size))
  
  # Loop through each batch and construct queries
  for (batch in batches) {
    col_ptp <- update_multiple_rows(batch, batch$collections_ptp, 
      "collections_ptp", "call_center_buckets_credits", db_name)
    col_cat <- update_multiple_rows(batch, batch$collections_category, 
      "collections_category","call_center_buckets_credits", db_name)
    ids <- paste0(batch$id, collapse = ", ")
    
    # Send queries
    suppressMessages(suppressWarnings(dbSendQuery(con, col_cat)))
    suppressMessages(suppressWarnings(dbSendQuery(con, col_ptp)))
    suppressMessages(suppressWarnings(dbSendQuery(con,
      paste("UPDATE ",db_name,".call_center_buckets_credits
      SET collections_updated_at = '", substring(Sys.time(),1,19),
      "' WHERE id IN (", ids, ");", sep=""))))
  }
}
