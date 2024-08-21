

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
suppressMessages(suppressWarnings(require(jsonlite)))
suppressMessages(suppressWarnings(require(gbm)))
suppressMessages(suppressWarnings(library(openxlsx)))


# Database
db_name <- "citycash"
con <- dbConnect(RMySQL::MySQL(),dbname = "citycash",host ="192.168.2.110",
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


# Read input file
df <- read.xlsx(
  "C:\\Projects\\Campaign\\20240819-Above-3months\\input\\raw.xlsx")
select_credits <- df[,c("id","client_id","product_id","is_vip")]


# Compute and append score
select_credits <- select_credits[!duplicated(select_credits$client_id),]
select_credits$max_amount <- NA
select_credits$max_installment_amount <- NA
select_credits$score_max_amount <- NA
select_credits$max_delay <- NA
for(i in 1:nrow(select_credits)){
  suppressWarnings(tryCatch({
    gc()
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

