
##########################################################################
## Apply correction to prior approval credits (terminated - refinanced) ##
##########################################################################


gen_correction_po <- function(con,db_name,all_df,scoring_df,products,period){

  # Read credits with already an offer for terminated prior approval
  po_sql_query <- paste(
    "SELECT id, client_id, application_id, credit_amount, installment_amount,
     created_at, product_id FROM ",db_name,".clients_prior_approval_applications 
     WHERE deleted_at IS NULL AND client_id=",all_df$client_id,
     sep="")
  po <- suppressWarnings(fetch(dbSendQuery(con,po_sql_query), n=-1))
  
  # Correct scoring for terminated prior approval
  if(nrow(po)==1){
    
    # Arrange installment amount according to period
    period_po <- suppressWarnings(fetch(dbSendQuery(con, 
        gen_products_query_desc(db_name,po)), n=-1))$period
    if(period_po!=period){
        po$installment_amount <- gen_correct_max_installment_po(period_po,
          period,po$installment_amount)
    }
    
    # Correct score according to data in PO 
    scoring_df <- merge(scoring_df,
        products[,c("amount","period","installment_amount")],
        by.x = c("amount","period"),by.y = c("amount","period"),all.x = TRUE)
    scoring_df$score <- ifelse(
        scoring_df$amount<=po$credit_amount & 
        scoring_df$installment_amount<=po$installment_amount &
        scoring_df$score %in% c("Bad","Indeterminate"),"Good 1",
        scoring_df$score)
    scoring_df <-  scoring_df[,-which(names(scoring_df) %in% 
        c("installment_amount"))]
    scoring_df$color <- ifelse(scoring_df$score=="Good 1", 3, scoring_df$color)
  }
  
  scoring_df <- scoring_df[order(scoring_df$period),]
  scoring_df <- scoring_df[order(scoring_df$amount),]
  return(scoring_df)

}
