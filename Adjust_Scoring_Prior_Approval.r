
##########################################################################
## Apply correction to prior approval credits (terminated - refinanced) ##
##########################################################################


gen_correction_po <- function(con,db_name,all_df,all_id,
                              scoring_df,products,period){

  # Read credits with already an offer for terminated prior approval
  po_sql_query <- paste(
    "SELECT id, client_id, application_id, credit_amount, installment_amount,
     deleted_at, created_at, product_id 
     FROM ",db_name,".clients_prior_approval_applications 
     WHERE client_id=",all_df$client_id,
     sep="")
  po <- suppressWarnings(fetch(dbSendQuery(con,po_sql_query), n=-1))
  
  if(nrow(po)>=1){
    
    # Check if any credit after offer (of same company)
    company_id <- suppressWarnings(fetch(dbSendQuery(con, 
        gen_get_company_id_query(db_name)), n=-1))
    po <- merge(po,company_id,by.x = "product_id",by.y = "id",all.x = TRUE)
    all_df_local <- merge(all_df,company_id,by.x = "product_id",
        by.y = "id",all.x = TRUE)
    po <- subset(po,po$company_id==all_df_local$company_id)
    
    if(nrow(po)>=1){
      po <- po[rev(order(po$created_at)),]
      po <- po[!duplicated(po$client_id),]
      all_id_local <- subset(all_id,all_id$status %in% c(4,5))
      all_id_local <- subset(all_id_local,
                             all_id_local$company_id==po$company_id & 
                             all_id_local$created_at>=po$created_at)
      
      
      # Check if last po of same company has been deleted by scoring
      if(!(is.na(po$deleted_at)) & substring(po$deleted_at,12,19)=="04:00:00"){
        flag_corrected_scoring <- 1
      } else if (!(is.na(po$deleted_at)) & po$deleted_at<="2020-07-23"){ 
        flag_corrected_scoring <- 1 } else {
        flag_corrected_scoring <- 0 }
      
      # Correct scoring for terminated prior approval
      if(nrow(all_id_local)==0 & flag_corrected_scoring==0){
        
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
            by.x = c("amount","period"),by.y = c("amount","period"),
            all.x = TRUE)
        scoring_df$score <- ifelse(
          scoring_df$amount<=po$credit_amount & 
            scoring_df$installment_amount<=po$installment_amount &
            scoring_df$score %in% c("Bad","Indeterminate"),"Good corr",
            scoring_df$score)
        scoring_df <-  scoring_df[,-which(names(scoring_df) %in% 
                                            c("installment_amount"))]
        scoring_df$color <- ifelse(scoring_df$score=="Good corr", 3, 
                                   scoring_df$color)
        scoring_df$score <- ifelse(scoring_df$score=="Good corr","Good 1",
                                   scoring_df$score)}
    } 
 }
  
  scoring_df <- scoring_df[order(scoring_df$period),]
  scoring_df <- scoring_df[order(scoring_df$amount),]
  return(scoring_df)

}

gen_correction_po_ref <- function(con,db_name,all_df,all_id,
                              scoring_df,products,period){
  
  # Get comapany ID to filter past credits only for Credirect
  all_df_local <- get_company_id_prev(db_name,all_df)
  input <- all_id[all_id$status %in% c(4) & 
                  all_id$company_id==all_df_local$company_id,]
  
  string_sql_update <- input$id[1]
  if(nrow(input)>1){
    for(i in 1:nrow(input)){
      string_sql_update <- paste(string_sql_update,input$id[i],sep=",")
      }
  }
  
  if(nrow(input)>0){
    # Read credits with already an offer for terminated prior approval
    po_sql_query <- paste(
      "SELECT application_id, max_amount, created_at, updated_at, product_id
     FROM ",db_name,".prior_approval_refinances 
     WHERE deleted_at IS NULL AND application_id IN (", 
      string_sql_update,")",sep="")
    po_ref <- suppressWarnings(fetch(dbSendQuery(con,po_sql_query), n=-1))
    
    if(nrow(po_ref)>0){
      po_ref$final_time <- ifelse(is.na(po_ref$updated_at),po_ref$created_at,
                                  po_ref$updated_at)
      po_ref <- po_ref[order(po_ref$created_at),]
      po_ref <- po_ref[1,]
      max_installments <- max(scoring_df$period)
      if(suppressWarnings(difftime(Sys.time(),po_ref$final_time,c("days")))<=100 
         & all_df$product_id==po_ref$product_id){
        scoring_df$score <- ifelse(
          scoring_df$amount<=po_ref$max_amount & 
            scoring_df$period==max_installments &
            (scoring_df$score %in% c("Bad","Indeterminate") |
               scoring_df$color==1),"Good corr",
          scoring_df$score)
        scoring_df$color <- ifelse(scoring_df$score=="Good corr", 3, 
                                   scoring_df$color)
        scoring_df$score <- ifelse(scoring_df$score=="Good corr","Good 1",
                                   scoring_df$score)
      }
   }
  }
  return(scoring_df)
}
  

