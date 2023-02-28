

############################################################
#        Generate RFM and MSF score for all clients        #
#               Version 1.0 (2023/02/10)                   #
############################################################



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



####################################
### Read database and build data ###
####################################

# Read all clients who have credit
df <- gen_query(con,gen_all_clients(db_name))


# Remove certain credits
df <- subset(df, !(df$sub_status %in% c(129,122)) | is.na(df$sub_status))


# Get all payments
pays <- gen_query(con,gen_all_payments(db_name))
df <- merge(df,pays,by.x = "id",by.y = "object_id",all.x = TRUE)
df$profit <- ifelse(is.na(df$amount_paid),0,df$amount_paid) - df$amount


# Read score
score <- gen_query(con,gen_all_scores(db_name))
df <- merge(df,score,by.x = c("id","amount","installments"), 
   by.y = c("application_id","amount","period"),all.x = TRUE)
df$credit <- 1


# Generate MSF score per brand_id
msf <- rbind(gen_msf(df,1),gen_msf(df,2),gen_msf(df,5))



##############################
### Read current RFM table ###
##############################

# Read current table
rfm_tot <- gen_query(con,paste(
  "SELECT * FROM ",db_name,".credits_applications_rfm_score",sep=""))
colnames(rfm_tot)[which(names(rfm_tot) == "rfm")] <- "rfm_cur"
colnames(rfm_tot)[which(names(rfm_tot) == "rfm_score")] <- "rfm_score_cur"
colnames(rfm_tot)[which(names(rfm_tot) == "type")] <- "type_cur"


# Get ID max
if(nrow(rfm_tot)==0){
  id_max <- 1
} else {
  id_max <- max(rfm_tot$id) + 1
}


# Seperate into msf and rfm
rfm_cur <- subset(rfm_tot,rfm_tot$type_cur==1)
msf_cur <- subset(rfm_tot,rfm_tot$type_cur==2)



#######################################
# MSF : Get those who get to be added #
#######################################

# Merge with current
msf_all <- merge(msf,
  msf_cur[,c("client_id","rfm_cur","rfm_score_cur","type_cur","brand")],
  by.x = c("client_id","brand"),by.y = c("client_id","brand"),all.x = TRUE)


# Create dataframe for new clients 
msf_new <- subset(msf_all,is.na(msf_all$rfm_cur))
msf_new$id_max <- seq(id_max,id_max+nrow(msf_new)-1)
msf_new$created_at <- Sys.time()
msf_new$updated_at <- Sys.time()


# Append new clients to database
if(nrow(msf_new)>10000){
  
  length_df <- seq(1,nrow(msf_new),by=10000)
  
  for(i in 1:(length(length_df)-1)){
    msf_here <- msf_new[c(length_df[i]:(length_df[i+1]-1)),]
    string_sql <- gen_sql_string_po_rfm(msf_here,1)
    if(nrow(msf_here)>1){
      for(j in 2:nrow(msf_here)){
        string_sql <- paste(string_sql,gen_sql_string_po_rfm(msf_here,j),
                            sep=",")
      }
    }
    suppressMessages(suppressWarnings(dbSendQuery(con,paste("INSERT INTO ",
     db_name,".credits_applications_rfm_score VALUES ",string_sql,";", 
     sep=""))))
  }
  msf_here <- msf_new[c(length_df[length(length_df)]:nrow(msf_new)),]
  string_sql <- gen_sql_string_po_rfm(msf_here,1)
  if(nrow(msf_here)>1){
    for(j in 2:nrow(msf_here)){
      string_sql <- paste(string_sql,gen_sql_string_po_rfm(msf_here,j),
                          sep=",")
    }
  }
  suppressMessages(suppressWarnings(dbSendQuery(con,paste("INSERT INTO ",
     db_name,".credits_applications_rfm_score VALUES ",string_sql,";", 
     sep=""))))
} else {
  string_sql <- gen_sql_string_po_rfm(msf_new,1)
  if(nrow(msf_new)>1){
    for(i in 2:nrow(msf_new)){
      string_sql <- paste(string_sql,gen_sql_string_po_rfm(msf_new,i),
                          sep=",")
    }
  }
 suppressMessages(suppressWarnings(dbSendQuery(con,paste("INSERT INTO ",db_name,
   ".credits_applications_rfm_score VALUES ",string_sql,";", sep=""))))
}



#########################################
# MSF : Get those who get to be updated #
#########################################

# Update older clients
msf_update <- subset(msf_all,!is.na(msf_all$rfm_cur) & 
  msf_all$rfm!=msf_all$rfm_cur)
msf_update$updated_at <- Sys.time()


# Update database
suppressMessages(suppressWarnings(dbSendQuery(con,
 gen_sql_string_update_rfm(msf_update,msf_update$rfm,"rfm",db_name,0))))
suppressMessages(suppressWarnings(dbSendQuery(con,
 gen_sql_string_update_rfm(msf_update,msf_update$updated_at,"updated_at",
 db_name,1))))


# End

