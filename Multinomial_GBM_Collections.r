# Generate PTP and Category
gen_ptp <- function(all_df, collections_cutoffs_citycash, 
                    collections_cutoffs_credirect){
  
  # Load model params
  load(file.path(base_dir,"rdata","collections_gbm_citycash.rdata"))
  load(file.path(base_dir,"rdata","collections_gbm_credirect.rdata"))
  
  # Variables to include
  variables <- c("application_id", "age", "gender", "marital_status",
   "ownership","household_total", "household_children",
   "has_prev_brand_credits","has_prev_credits", "default","credit_count",
   "max_dpd_app","outs_overdue_ratio_total", "source_entity_count_total", 
   "status_finished_total", "status_active_total", "cred_count_total", 
   "amount_cession_total", "incoming_contact", 'last_result', 
   "outgoing_contacts", "call_1w_prior_dpd", "call_2w_prior_dpd", 
   "call_1m_prior_dpd", "score", "repayment_ratio", "default_inst_ratio")

  factor_columns <- c("score", "ownership", "marital_status", "gender", 
       "has_prev_brand_credits", "has_prev_credits", "last_result")
  all_df[factor_columns] <- lapply(all_df[factor_columns], as.factor)
  
  category_labels <- c("0-0.05", "0.05-0.15", "0.15-0.25", "0.25-0.50", 
                       "0.50-0.75", "0.75-1")  
  
  # Model DPD groups
  dpds <- c(10, 30, 60, 90, 180, 360)
  brands <- c(1, 2)
  
  results <- list()
  
  for (brand in brands) {
    dpds_loop <- if (brand == 2) c(1, dpds) else dpds
    for (dpd in dpds_loop) {
      
      # Select the relevant data
      temp_df <- subset(all_df, dpd_group == dpd & brand_id == brand, 
                        select = variables)

      for (col in colnames(temp_df)) {
        temp_df[[col]] <- fill_na(temp_df[[col]], -999, "999")
      }
      
      # Generate multiclass probabilities
      if(brand == 1){
      probs <- gen_multiclass_gbm_probs(
        model_info = collections_gbm_citycash[[paste0("model_dpd", dpd)]],
        data = temp_df, n_trees = 1000, n_classes = 6, 
        category_labels = category_labels)
      }
      if(brand == 2){
        probs <- gen_multiclass_gbm_probs(
          model_info = collections_gbm_credirect[[paste0("model_dpd", dpd)]],
          data = temp_df, n_trees = 1000, n_classes = 6, 
          category_labels = category_labels)
      }
      # Calculate ptp
      coef <- c(0.025, 0.1, 0.2, 0.375, 0.625, 0.875)
      probs$collections_ptp <- with(probs, 
                                    `0-0.05` * coef[1] +
                                      `0.05-0.15` * coef[2] +
                                      `0.15-0.25` * coef[3] +
                                      `0.25-0.50` * coef[4] +
                                      `0.50-0.75` * coef[5] +
                                      `0.75-1` * coef[6])
      probs$collections_ptp <- round(probs$collections_ptp, 3)
      
      # Finalize the result
      temp_output <- data.frame(
        application_id = temp_df[["application_id"]],
        collections_ptp = probs[["collections_ptp"]]
      )
      temp_output$dpd <- dpd
      temp_output$brand <- brand
      
      if(brand == 1){
        cutoffs <- collections_cutoffs_citycash
      }
      if(brand == 2){
        cutoffs <- collections_cutoffs_credirect
      }
      
      labels <- c(1:length(cutoffs))
      temp_output$collections_category <- cut(temp_output$collections_ptp, 
      breaks = cutoffs, labels = labels[-length(labels)],include.lowest = TRUE)
      
      results[[paste0("dpd_", dpd, "_brand_", brand)]] <- temp_output
      
    }
  }
  
  final_output <- do.call(rbind, results)
  
  return(final_output)
}
