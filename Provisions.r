

################################################################################
#               Script to write provisions into database                       #
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
suppressMessages(suppressWarnings(library(dplyr)))

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

# Load Provision file 
load(file.path(base_dir,"provisions","provisions.rdata"))
provisions <- provisions[,c("credit_number","EL","month_provisions",
  "year_provisions")]
names(provisions) <- c("credit_number","provision","month","year")
provisions$provision <- round(provisions$provision,2)



################################################
### Make adjustments and write into database ###
################################################

# Read current table
apps <- gen_query(con,paste("SELECT id,credit_number
  FROM ",db_name,".credits_applications WHERE credit_number IS NOT NULL",
  sep=""))
provisions <- merge(provisions,apps,by.x = c("credit_number"),
  by.y = c("credit_number"),all.x = TRUE)
names(provisions)[ncol(provisions)] <- c("application_id")
provisions <- subset(provisions,!is.na(provisions$application_id))


# Read current table
cur_prov <- gen_query(con,paste("SELECT * FROM ",db_name,".credits_provisions",
                                sep=""))


# Get ID max
if(nrow(cur_prov)==0){
  id_max <- 1
} else {
  id_max <- max(cur_prov$id) + 1
}


# Arrange final dataframe
provisions$id <- seq(id_max,id_max+nrow(provisions)-1)
provisions$type <- 1
provisions$created_at <- paste(
  paste(provisions$year,ifelse(nchar(provisions$month)==1,
  paste("0",provisions$month,sep=""),provisions$month),
  ifelse(provisions$month==2,
    ifelse((provisions$year %% 4) == 0 & 
    (provisions$year %% 100) == 0 & (provisions$year %% 400) == 0,"29","28"),
  ifelse(provisions$month %in% c(4,6,9,11),"30","31")),sep="-"),
  " 00:00:00",sep="")
provisions$updated_at <- Sys.time()


# Write into database
if(nrow(provisions)>10000){
  
  length_df <- seq(1,nrow(provisions),by=10000)
  
  for(i in 1:(length(length_df)-1)){
    provisions_here <- provisions[c(length_df[i]:(length_df[i+1]-1)),]
    string_sql <- gen_sql_string_prov(provisions_here,1)
    if(nrow(provisions_here)>1){
      for(j in 2:nrow(provisions_here)){
        string_sql <- paste(string_sql,gen_sql_string_prov(provisions_here,j),
                            sep=",")
      }
    }
    suppressMessages(suppressWarnings(dbSendQuery(con,paste("INSERT INTO ",
        db_name,".credits_provisions VALUES ",string_sql,";", 
        sep=""))))
  }
  provisions_here <- provisions[c(length_df[length(length_df)]:
                                    nrow(provisions)),]
  string_sql <- gen_sql_string_prov(provisions_here,1)
  if(nrow(provisions_here)>1){
    for(j in 2:nrow(provisions_here)){
      string_sql <- paste(string_sql,gen_sql_string_prov(provisions_here,j),
                          sep=",")
    }
  }
  suppressMessages(suppressWarnings(dbSendQuery(con,paste("INSERT INTO ",
      db_name,".credits_provisions VALUES ",string_sql,";", 
      sep=""))))
} else {
  string_sql <- gen_sql_string_prov(provisions,1)
  if(nrow(provisions)>1){
    for(i in 2:nrow(provisions)){
      string_sql <- paste(string_sql,gen_sql_string_prov(provisions,i),
                          sep=",")
    }
  }
 suppressMessages(suppressWarnings(dbSendQuery(con,paste("INSERT INTO ",db_name,
    ".credits_provisions VALUES ",string_sql,";", sep=""))))
}



#######
# END #
#######

