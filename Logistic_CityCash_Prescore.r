
##############################################################################
######## Functions to apply logisit regression on application City Cash ######
##############################################################################

gen_app_citycash_prescore <- function(df,all_df,base_dir,scoring_df){
  
  # Load rdata
  load(file.path(base_dir,"rdata","citycash_app_prescore_coeffs.rdata"))
  names(coefficients) <- c("coeff")

  # Cut and bin
  df$age_cut <- 
    ifelse(df$age<=20,"20_less",
    ifelse(df$age<=30,"21_30",
    ifelse(df$age<=45,"31_45",
    ifelse(df$age<=58,"45_58","58_more"))))
  df$age <- as.factor(df$age_cut)
  df$gender <- as.factor(df$gender)
  df$status_finished_total_cut <- ifelse(is.na(df$status_finished_total),
    "missing",df$status_finished_total)
  df$status_finished_total_cut <- ifelse(is.na(df$status_finished_total),
    "0_71_72",ifelse(df$status_finished_total %in% c(0,71,72),"0_71_72",
    "73_74_75"))
  df$status_finished_total <- as.factor(df$status_finished_total_cut)
  df$outs_overdue_ratio_total_cut <- ifelse(
    is.na(df$outs_overdue_ratio_total),"other",
    ifelse(df$outs_overdue_ratio_total==-999,"other",
    ifelse(df$outs_overdue_ratio_total==0,"0",
    ifelse(df$outs_overdue_ratio_total<=0.2,"0.01_0.2","other"))))
  df$outs_overdue_ratio_total <- as.factor(df$outs_overdue_ratio_total_cut)
  
  # Apply logisic regression
  apply_logit <- gen_apply_model(df,coefficients)
  scoring_df$score <- gen_group_scores_prescore(apply_logit,0,0)
  scoring_df$pd <- round(apply_logit,3)
  scoring_df$color <- ifelse(scoring_df$score=="Bad", 1, 
      ifelse(scoring_df$score=="Indeterminate", 2,
      ifelse(scoring_df$score=="Good 1", 3,
      ifelse(scoring_df$score=="Good 2", 4,
      ifelse(scoring_df$score=="Good 3", 5,
      ifelse(scoring_df$score=="Good 4", 6,NA))))))
  
  return(scoring_df)
}

