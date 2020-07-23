
#############################################################
######## Apply restrictions on final scoring dataframe ######
#############################################################

# Function to apply restrictions for City Cash applications
gen_restrict_citycash_app <- function(scoring_df){
  
  score_df_800 <- subset(scoring_df,scoring_df$amount>800)
  criteria_800 <- length(names(table(score_df_800$score))
     [names(table(score_df_800$score)) %in% c("Good 4")])
  score_df_600 <- subset(scoring_df,scoring_df$amount>600)
  criteria_600 <- length(names(table(score_df_600$score))
    [names(table(score_df_600$score)) %in% c("Good 2","Good 3","Good 4")])
  score_df_400 <- subset(scoring_df,scoring_df$amount>400)
  criteria_400 <- length(names(table(score_df_400$score))
    [names(table(score_df_400$score)) %in% c("Good 1","Good 2",
    "Good 3","Good 4")])
  
  scoring_df$score <- ifelse(scoring_df$score %in% c("NULL"),scoring_df$score,
    ifelse(scoring_df$amount>1000,"Bad",
    ifelse(criteria_800==0 & scoring_df$amount>800,"Bad",
    ifelse(criteria_600==0 & scoring_df$amount>600,"Bad",
    ifelse(criteria_400==0 & scoring_df$amount>400,"Bad",scoring_df$score)))))
  scoring_df$color <- ifelse(scoring_df$score %in% c("NULL"),scoring_df$color,
    ifelse(scoring_df$score %in% c("Bad"),1,scoring_df$color))
           
  return(scoring_df)
}

# Function to apply restrictions for City Cash repeats
gen_restrict_citycash_beh <- function(scoring_df,prev_amount){
  
  criteria <- length(names(table(scoring_df$score))
    [names(table(scoring_df$score)) %in% 
        c("Good 1","Good 2","Good 3","Good 4")])
  scoring_df$score <- ifelse(scoring_df$score %in% c("NULL"),scoring_df$score,
    ifelse(criteria==0 & scoring_df$amount>prev_amount$amount,"Bad",
           scoring_df$score))
  scoring_df$color <- ifelse(scoring_df$score %in% c("NULL"),scoring_df$color,
    ifelse(scoring_df$score %in% c("Bad"),1,scoring_df$color))

  return(scoring_df)
}

# Function to apply restrictions for Credirect applications
gen_restrict_credirect_app <- function(scoring_df,all_df,
                                       flag_credit_next_salary){

  if(flag_credit_next_salary==1){
    scoring_df$score <- ifelse(scoring_df$score %in% 
            c("Indeterminate"), "Bad", scoring_df$score)
    scoring_df$score <- 
      ifelse(scoring_df$score %in% c("Good 4") & scoring_df$amount>800,"Bad",
      ifelse(scoring_df$score %in% c("Good 3","Good 2","Good 1") &
             scoring_df$amount>600,"Bad",
             scoring_df$score))
  } else {
    scoring_df$score <- ifelse(scoring_df$score %in% 
            c("Indeterminate"), "Bad", scoring_df$score)
    scoring_df$score <- 
      ifelse(scoring_df$score %in% c("Good 4") & scoring_df$amount>1000,"Bad",
      ifelse(scoring_df$score %in% c("Good 3","Good 2","Good 1") &
             scoring_df$amount>600,"Bad",
             scoring_df$score))
  }
  if(all_df$age<21){
    scoring_df$score <- ifelse(scoring_df$amount>300,"Bad",scoring_df$score)
  }
  
  scoring_df$color <- ifelse(scoring_df$score=="Bad", 1, scoring_df$color)
  return(scoring_df)
}

# Function to apply restrictions for Credirect behavioral
gen_restrict_credirect_beh <- function(scoring_df,all_df,
       flag_credit_next_salary,flag_new_credirect_old_city){
  
  # Apply filter for coronavirus effect
  if(flag_new_credirect_old_city==1){
    scoring_df$score <- ifelse(scoring_df$score %in% 
        c("Indeterminate","Good 1"), "Bad", scoring_df$score)
  } else {
    scoring_df$score <- ifelse(scoring_df$score %in% 
        c("Indeterminate"), "Bad", scoring_df$score)
  }

  scoring_df$color <- ifelse(scoring_df$score=="Bad", 1, scoring_df$color)
  return(scoring_df)
}

# Readjust score if necessary for certain cases
gen_adjust_score <- function(scoring_df,crit){
  for(i in 1:nrow(scoring_df)){
    if(scoring_df$score[i] %in% crit){
      scoring_df$score[i] <- "Bad"
      scoring_df$color[i] <- 1} 
  }
  return(scoring_df)
}

