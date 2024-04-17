

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

# Load scores
scores <- read.csv(file.path(base_dir,"scores","scores.csv"),sep=";")
scores[scores== ''] <- NA
names(scores) <- c("id","score")



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
score <- gen_query(con,gen_all_scores(db_name,2033221))
df <- merge(df,score,by.x = c("id","amount","installments"), 
   by.y = c("application_id","amount","period"),all.x = TRUE)
df <- merge(df,scores,by.x = "id",by.y = "id",all.x = TRUE)
df$score.x <- as.character(df$score.x)
df$score.y <- as.character(df$score.y)
df$score <- ifelse(is.na(df$score.y),df$score.x,df$score.y)
df$score <- as.character(df$score)
df <- df[,-which(names(df) %in% c("score.y","score.x"))]
df$credit <- 1


# Generate MSF score per brand_id
msf <- rbind(gen_msf(df,1),gen_msf(df,2),gen_msf(df,5))


# Generate final RFM Score
rfm <- rbind(gen_rfm(msf,1,df),gen_rfm(msf,2,df),gen_rfm(msf,5,df))



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



#######################################
# MSF : Get those who get to be added #
#######################################

# Merge with current
rfm_all <- merge(rfm,
  rfm_tot[,c("id","client_id","rfm_cur","rfm_score_cur","type_cur","brand_id")],
  by.x = c("client_id","brand_id","type"),by.y = c("client_id","brand_id",
  "type_cur"),all.x = TRUE)


# Create dataframe for new clients 
rfm_new <- subset(rfm_all,is.na(rfm_all$rfm_cur))
if(nrow(rfm_new)>0){
rfm_new$id <- seq(id_max,id_max+nrow(rfm_new)-1)
rfm_new$created_at <- Sys.time()
rfm_new$updated_at <- Sys.time()


# Append new clients to database
if(nrow(rfm_new)>10000){
  
  length_df <- seq(1,nrow(rfm_new),by=10000)
  
  for(i in 1:(length(length_df)-1)){
    rfm_here <- rfm_new[c(length_df[i]:(length_df[i+1]-1)),]
    string_sql <- gen_sql_string_po_rfm(rfm_here,1)
    if(nrow(rfm_here)>1){
      for(j in 2:nrow(rfm_here)){
        string_sql <- paste(string_sql,gen_sql_string_po_rfm(rfm_here,j),
                            sep=",")
      }
    }
    suppressMessages(suppressWarnings(dbSendQuery(con,paste("INSERT INTO ",
     db_name,".credits_applications_rfm_score VALUES ",string_sql,";", 
     sep=""))))
  }
  rfm_here <- rfm_new[c(length_df[length(length_df)]:nrow(rfm_new)),]
  string_sql <- gen_sql_string_po_rfm(rfm_here,1)
  if(nrow(rfm_here)>1){
    for(j in 2:nrow(rfm_here)){
      string_sql <- paste(string_sql,gen_sql_string_po_rfm(rfm_here,j),
                          sep=",")
    }
  }
  suppressMessages(suppressWarnings(dbSendQuery(con,paste("INSERT INTO ",
     db_name,".credits_applications_rfm_score VALUES ",string_sql,";", 
     sep=""))))
} else {
  string_sql <- gen_sql_string_po_rfm(rfm_new,1)
  if(nrow(rfm_new)>1){
    for(i in 2:nrow(rfm_new)){
      string_sql <- paste(string_sql,gen_sql_string_po_rfm(rfm_new,i),
                          sep=",")
    }
  }
 suppressMessages(suppressWarnings(dbSendQuery(con,paste("INSERT INTO ",db_name,
   ".credits_applications_rfm_score VALUES ",string_sql,";", sep=""))))
}
}


#########################################
# MSF : Get those who get to be updated #
#########################################

# Update older clients
rfm_update <- subset(rfm_all,!is.na(rfm_all$rfm_cur) & 
  rfm_all$rfm!=rfm_all$rfm_cur)
if(nrow(rfm_update)>0){
rfm_update$updated_at <- Sys.time()

# Reconnect to database
con <- dbConnect(MySQL(), user=db_username, 
                 password=db_password, dbname=db_name, 
                 host=db_host, port = db_port)
sqlMode <- paste("SET sql_mode=''", sep ="")
suppressWarnings(fetch(dbSendQuery(con, sqlMode), 
                       n=-1))

# Update database
suppressMessages(suppressWarnings(dbSendQuery(con,
 gen_sql_string_update_rfm(rfm_update,rfm_update$rfm,"rfm",db_name,0))))
suppressMessages(suppressWarnings(dbSendQuery(con,
 gen_sql_string_update_rfm(rfm_update,rfm_update$rfm_score,"rfm_score",db_name,
 1))))
suppressMessages(suppressWarnings(dbSendQuery(con,
 gen_sql_string_update_rfm(rfm_update,rfm_update$updated_at,"updated_at",
 db_name,1))))
}


# End

