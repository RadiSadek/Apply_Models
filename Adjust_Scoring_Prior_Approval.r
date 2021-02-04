
##########################################################################
## Apply correction to prior approval credits (terminated - refinanced) ##
##########################################################################

# Function to correct scoring table for clients with po terminated
gen_correction_po <- function(con,db_name,all_df,all_id,
                              scoring_df,products,period){

  # Read credits with already an offer for terminated prior approval
  po <- suppressWarnings(fetch(dbSendQuery(con,
           gen_po_terminated_query(db_name,all_df$client_id)), n=-1))
  
  if(nrow(po)>=1){
    
    # Check if any credit after offer (of same company)
    company_id <- suppressWarnings(fetch(dbSendQuery(con, 
        gen_get_company_id_query(db_name)), n=-1))
    po <- merge(po,company_id,by.x = "product_id",by.y = "id",all.x = TRUE)
    all_df_local <- merge(all_df,company_id,by.x = "product_id",
        by.y = "id",all.x = TRUE)
    po <- subset(po,po$company_id==all_df_local$company_id)
    
    if(nrow(po)>=1){
      po <- po[rev(order(po$deleted_at)),]
      po <- po[!duplicated(po$client_id),]
      all_id_local <- subset(all_id,all_id$status %in% c(4,5))
      all_id_local <- subset(all_id_local,
                             all_id_local$company_id==po$company_id & 
                             all_id_local$created_at>=po$created_at)
      po$final_time <- ifelse(!is.na(po$deleted_at) &
                              substring(po$deleted_at,12,20)!="04:00:00",
              difftime(Sys.time(),po$deleted_at,units=c("days")),
              999)
            
      # Correct scoring for terminated prior approval
      if(nrow(all_id_local)==0 & po$final_time<=4){
        
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
        scoring_df$color <- ifelse(
          scoring_df$amount<=po$credit_amount & 
            scoring_df$installment_amount<=po$installment_amount &
            scoring_df$score %in% c("Bad","Indeterminate"),3,
            scoring_df$color)
        scoring_df <-  scoring_df[,-which(names(scoring_df) %in% 
                                            c("installment_amount"))]}
    }
 }
  scoring_df <- scoring_df[order(scoring_df$period),]
  scoring_df <- scoring_df[order(scoring_df$amount),]
  return(scoring_df)
}

# Function to correct scoring table for clients with po refinances
gen_correction_po_ref <- function(con,db_name,all_df,all_id,
                              scoring_df,products,period){
  
  # Get company ID and all current actives
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
    po_ref <- suppressWarnings(fetch(dbSendQuery(con,
                gen_po_refinance_query(db_name,string_sql_update)), n=-1))
    
    if(nrow(po_ref)>0){
      po_ref$final_time <- ifelse(is.na(po_ref$deleted_at),0,
        ifelse(substring(po_ref$deleted_at,12,20)!="04:00:00",
        difftime(Sys.time(),po_ref$deleted_at,units=c("days")),999))
      po_ref <- po_ref[rev(order(po_ref$deleted_at)),]
      po_ref <- po_ref[1,]
      
      if(po_ref$final_time<=7){
        unique_amounts <- unique(scoring_df$amount[
          scoring_df$amount<=po_ref$max_amount])
        count_not_bad <- vector(mode = "double",length(unique_amounts))
        
        for(i in 1:length(unique_amounts)){
          amount_df <- scoring_df[scoring_df$amount==unique_amounts[i],]
          count_not_bad[i] <- nrow(amount_df[amount_df$color>=2,])
        }
        correct_df <- as.data.frame(cbind(unique_amounts,count_not_bad))
        if(max(correct_df$count_not_bad)>0){
          max_amount_ok <- max(correct_df$unique_amounts[
          correct_df$count_not_bad>0])
          for(i in 1:nrow(scoring_df)){
            if(scoring_df$amount[i]<=po_ref$max_amount){
              subs <- scoring_df[scoring_df$amount==max_amount_ok,]
              scoring_df$color[i] <- subs$color[
                subs$period==scoring_df$period[i]]}
          }
        } else {
          for(i in 1:nrow(scoring_df)){
            if(scoring_df$amount[i]<=po_ref$max_amount){
              if(correct_df[correct_df$unique_amounts==scoring_df$amount[i],
              2]==0 & 
              scoring_df$period[i]==unique(scoring_df$period)
              [[ceiling(length(unique(scoring_df$period))/2)]]){
              scoring_df$color[i] <- 3}
              }}
        }
        
        scoring_df$color <- ifelse(scoring_df$amount>po_ref$max_amount,1,
                                   scoring_df$color)}
   }
  }
  return(scoring_df)
}
  
