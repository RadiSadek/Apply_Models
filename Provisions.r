

################################################################################
#               Script to write provisions into database                       #
################################################################################

# Library
suppressMessages(suppressWarnings(library(RMySQL)))
suppressMessages(suppressWarnings(library(here)))
suppressMessages(suppressWarnings(library(dotenv)))
suppressMessages(suppressWarnings(require("reshape")))
suppressMessages(suppressWarnings(library(openxlsx)))
suppressMessages(suppressWarnings(require(jsonlite)))


# Database
db_user <- "root"
db_password <- "123456"
db_name <- "romania"
db_host <- "127.0.0.1"
df_port <- 3306
con <- dbConnect(MySQL(), user=db_user, password=db_password, 
                 dbname=db_name, host=db_host, port = df_port)


# Define work directory
base_dir <- "C:/Projects/Apply_Scoring"


# Set working directory for input (R data for logistic regression) and output #
setwd(base_dir)


# Load other r files
source(paste(base_dir,"/Apply_Models/Useful_Functions.r", sep=""))


# Load Provision file 
provisions <- read.csv("provisions\\provisions_input.csv",sep=";")
provisions <- provisions[,c("id","EL","month_provisions","year_provisions")]
names(provisions) <- c("application_id","provision","month","year")
provisions$provision <- round(provisions$provision,2)



################################################
### Make adjustments and write into database ###
################################################

# Read current table
cur_prov <- gen_query(con,"SELECT * FROM romania.credits_provisions")


# Get ID max
if(nrow(cur_prov)==0){
  id_max <- 1
} else {
  id_max <- max(cur_prov$id) + 1
}


# Arrange final dataframe
provisions$id <- seq(id_max,nrow(provisions),1)
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
    string_sql <- gen_sql_string_prov(provisions,1)
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

