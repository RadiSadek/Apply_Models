
#############################################################
######## Apply restrictions on final scoring dataframe ######
#############################################################

# Function to apply restrictions for City Cash applications
gen_restrict_citycash_app <- function(scoring_df){
  
  if(max(scoring_df$amount)>400){
    score_df <- subset(scoring_df,scoring_df$amount>400)
    criteria <- length(names(table(score_df$score))
      [names(table(score_df$score)) %in% c("Good 2","Good 3","Good 4")])
    if(criteria==0 & nrow(subset(scoring_df,scoring_df$amount<=400))==0){
      scoring_df$score <- "Bad"
      scoring_df$color <- 1}
    if(criteria==0 & nrow(subset(scoring_df,scoring_df$amount<=400))>0){
      scoring_df <- subset(scoring_df,scoring_df$amount<=400)}
  }
  return(scoring_df)
}

# Function to apply restrictions for City Cash repeats
gen_restrict_citycash_beh <- function(scoring_df,prev_amount){
  
  criteria <- length(names(table(scoring_df$score))
    [names(table(scoring_df$score)) %in% 
        c("Good 1","Good 2","Good 3","Good 4")])
  if(criteria==0 & 
     nrow(subset(scoring_df,scoring_df$amount<=prev_amount$amount))>0){
    scoring_df <- subset(scoring_df,scoring_df$amount<=prev_amount$amount)
  }
  if(criteria==0 & 
     nrow(subset(scoring_df,scoring_df$amount<=prev_amount$amount))==0){
    scoring_df$score <- "Bad"
    scoring_df$color <- 1
  }
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

