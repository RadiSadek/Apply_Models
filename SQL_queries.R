
##################################
######## Define SQL queries ######
##################################

# Define big query which reads from credits_applications
gen_big_sql_query <- function(db_name,application_id){
big_sql_query <- paste("SELECT 
",db_name,".credits_applications_clients.application_id, 
",db_name,".credits_applications_clients.ownership,
",db_name,".credits_applications_clients.education,
",db_name,".credits_applications_clients.egn,
",db_name,".credits_applications_clients.household_children,
",db_name,".credits_applications_clients.household_total,
",db_name,".credits_applications_clients.on_address,
",db_name,".credits_applications_clients.marital_status,
",db_name,".credits_applications_clients.has_viber,
",db_name,".credits_applications_clients.dwelling_type,
",db_name,".credits_applications_clients.phone_plan,
",db_name,".credits_applications_data_other.purpose,
",db_name,".credits_applications_data_other.leasing,
",db_name,".credits_applications_data_other.hear_about_us,
",db_name,".credits_applications_data_other.other_bank_accounts,
",db_name,".credits_applications_data_other.payment_method AS payment_method2,
",db_name,".credits_plan_contract.amount,
",db_name,".credits_plan_contract.installments,
",db_name,".credits_applications.signed_at,
",db_name,".credits_applications.created_at,
",db_name,".credits_applications.client_id,
",db_name,".credits_applications.office_id,
",db_name,".credits_applications.status,
",db_name,".credits_applications.sub_status,
",db_name,".credits_applications.product_id,
",db_name,".credits_applications.deactivated_at,
",db_name,".credits_applications.source,
",db_name,".credits_applications.is_online,
",db_name,".credits_applications.third_side_date
FROM ",db_name,".credits_applications_clients
LEFT JOIN ",db_name,".credits_applications_data_other
ON ",db_name,".credits_applications_clients.application_id = ",db_name,
".credits_applications_data_other.application_id
LEFT JOIN ",db_name,".credits_applications_clients_work
ON ",db_name,".credits_applications_clients.application_id = ",db_name,
".credits_applications_clients_work.application_id
LEFT JOIN ",db_name,".credits_plan_contract
ON ",db_name,".credits_applications_clients.application_id = ",db_name,
".credits_plan_contract.application_id
LEFT JOIN ",db_name,".credits_applications
ON ",db_name,".credits_applications_clients.application_id = ",db_name,
".credits_applications.id
WHERE credits_applications_clients.application_id=", application_id, sep="")
return(big_sql_query)
}

# Define query for products periods and amounts
gen_products_query <- function(db_name,all_df){
  return(paste("SELECT * FROM ", db_name, ".products_periods_and_amounts
               WHERE product_id IN (",
               all_df$product_id, ")", sep=""))
}

# Define query for products 
gen_products_query_desc <- function(db_name,all_df){
  return(paste("SELECT id, period, type, brand_id AS company_id FROM ", db_name, 
  ".products WHERE id=", all_df$product_id, sep=""))
}

# Define query for income
gen_income_sql_query <- function(db_name,application_id){
  return(paste("SELECT application_id, amount, sub_type 
  FROM ",db_name,".credits_applications_clients_money_income 
  WHERE deleted_at IS NULL AND application_id=",application_id, sep=""))
}

# Define query for normal expenses 
gen_expenses_sql_query <- function(db_name,all_df){
  return(paste("SELECT application_id, amount 
  FROM ",db_name,".credits_applications_clients_money_expense
  WHERE deleted_at IS NULL AND application_id=",all_df$application_id, sep=""))
}

# Define query for expenses for loans 
gen_loans_sql_query <- function(db_name,all_df){
  return(paste("SELECT application_id, installment
  FROM ",db_name,".credits_applications_clients_money_loans
  WHERE deleted_at IS NULL AND application_id=",all_df$application_id, sep=""))
}

# Define query for getting all credits for client 
gen_all_credits_query <- function(db_name,all_df){
  return(paste("SELECT id, client_id, signed_at, created_at, 
  deactivated_at, status, sub_status, product_id, judicial_date
  FROM ",db_name,".credits_applications 
  WHERE client_id=",all_df$client_id, sep =""))
}

# Define query to get if client is defined as risky 
gen_risky_query <- function(db_name,all_df){
  return(paste("SELECT egn
  FROM ",db_name,".clients_risk
  WHERE egn=",all_df$egn, " AND level = 3", sep =""))
}

# Define query to get if client is defined as risky (for PA offers) 
gen_risky_query_offers <- function(db_name,all_df){
  return(paste("SELECT egn
  FROM ",db_name,".clients_risk
  WHERE egn=",all_df$egn, sep =""))
}

# Define query to get total amount of current application amount
gen_total_amount_curr_query <- function(db_name,application_id){
  return(paste("SELECT final_credit_amount
  FROM ",db_name,".credits_plan_contract 
  WHERE application_id=", application_id, sep =""))
}

# Define query to get the pay days of previous actives credits
gen_plan_main_actives_past_query <- function(db_name,string_id){	
  return(paste("SELECT application_id, pay_day	
  FROM ",db_name,".credits_plan_main WHERE application_id IN (", 	
  string_id,")", sep=""))	
}

# Define query to get total amount of current application amount
gen_prev_amount_query <- function(db_name,all_id){
  return(paste("SELECT amount FROM ",db_name,
  ".credits_plan_contract WHERE application_id=", 
  all_id$id[nrow(all_id)-1], sep=""))
}

# Define query to get the maximum of delay days from previous credits
gen_plan_main_select_query <- function(db_name,list_ids_max_delay){
  return(paste("SELECT MAX(days_delay) AS max_delay FROM ",db_name, 
  ".credits_plan_main WHERE application_id in(",list_ids_max_delay," 
  )", sep=""))
}

# Define query to get paid amount on last day of previous credit
gen_last_paid_amount_query <- function(var,db){
  return(paste("SELECT a.object_id, a.pay_date as last_pay_date, a.amount 
  FROM ",db,".cash_flow a INNER JOIN
  (SELECT object_id, MAX(pay_date) AS pay_date
  FROM ",db,".cash_flow
  WHERE nomenclature_id IN (90,100,101) AND deleted_at IS NULL 
  AND object_type = 4 AND object_id=",var,") AS b
  ON a.object_id=b.object_id AND a.pay_date=b.pay_date", sep =""))
}

# Define query to get last paid amount of previous credit 
gen_paid_amount_query <- function(var,db){		
  return(paste("SELECT object_id, amount 		
  FROM ",db,".cash_flow		
  WHERE nomenclature_id IN (90,100,101) AND deleted_at 
  IS NULL AND object_type = 4	AND object_id=",var, sep =""))		
}

# Define query to get total paid amount of previous credit 
gen_total_paid_amount_query <- function(var,db){		
  return(paste("SELECT object_id, SUM(amount) 		
  FROM ",db,".cash_flow		
  WHERE nomenclature_id IN (90,100,101) AND deleted_at 
  IS NULL AND object_type = 4	AND object_id=",var,
  " GROUP BY object_id",sep =""))		
}

# Define query to get all payments of previous credit 
gen_all_payments_query <- function(var,db){		
  return(paste("SELECT object_id, amount, pay_date	
  FROM ",db,".cash_flow		
  WHERE nomenclature_id IN (90,100,101) AND deleted_at 
  IS NULL AND object_type = 4	AND object_id=",var,sep =""))		
}

# Define query to get the last amount of previous credit
gen_last_cred_amount_query <- function(var,db){
  return(paste("SELECT final_credit_amount, installments, amount
  FROM ",db,".credits_plan_contract 
  WHERE application_id=", var, sep =""))
}

# Define query to get all payments of previous credit 
gen_all_payments_with_ref_query <- function(var,db){		
  return(paste("SELECT SUM(amount) AS amount_paid
  FROM ",db,".cash_flow		
  WHERE nomenclature_id IN (90,100,101,102) AND deleted_at 
  IS NULL AND object_type = 4	AND object_id=",var,sep =""))		
}

# Define query to get expenses according to city
gen_address_query <- function(var,arg){
  return(paste("SELECT 
    c.household, c.child, c.pensioner
    FROM ",db_name,".addresses AS a
    RIGHT JOIN ",db_name,".cities AS c ON c.id=a.city_id
    WHERE a.addressable_type = '",arg,"'
    AND a.addressable_id = ",var," AND a.type = 2", sep =""))
}

# Define query to get coordinates of address (per application)
gen_address_coordinates_query <- function(db_name,application_id){
  return(paste("SELECT lat, lon, type, location_precision, city_id
  FROM ",db_name,".addresses 
  WHERE addressable_type=
  'App\\\\Models\\\\Credits\\\\Applications\\\\Application'
  AND addressable_id=",application_id," AND type IN (1,2,3)",sep =""))
}

# Define query to get coordinates of address (per client)
gen_address_client_coordinates_query <- function(db_name,all_df){
  return(paste("SELECT lat, lon, type, location_precision, city_id
  FROM ",db_name,".addresses 
  WHERE addressable_type=
  'App\\\\Models\\\\Clients\\\\Client'
  AND addressable_id=",all_df$client_id," AND type IN (1,2,3)",sep =""))
}


# Define query to get company id (credirect or city cash) 
gen_get_company_id_query <- function(db_name){
  return(get_company_id_query <- paste("SELECT id, brand_id AS 
     company_id, company_id AS big_company_id 
     FROM ", db_name,".products", sep=""))
}

# Define query to get the CKR status 
gen_query_ckr <- function(all_df,all_credits,type_of,crit,incl_ids=0,db_name){
  
  names_col <- c("reportable_id", 
             "current_status_active","status_active","status_finished",
             "source_entity_count","amount_drawn","cred_count", 
             "outstanding_performing_principal","outstanding_overdue_principal",
             "amount_cession","monthly_installment","codebtor_status",
             "guarantor_status")
  
  client_ids <- all_df %>% distinct(client_id)
  
  query_ckr <- paste("SELECT 
  ",db_name,".ckr_reports.reportable_id,
  ",db_name,".ckr_reports.created_at,
  ",db_name,".ckr_reports.application_id,
  ",db_name,".ckr_report_data.current_status_active, 
  ",db_name,".ckr_report_data.status_active, 
  ",db_name,".ckr_report_data.status_finished,
  ",db_name,".ckr_report_data.source_entity_count,
  ",db_name,".ckr_report_data.amount_drawn,
  ",db_name,".ckr_report_data.cred_count,
  ",db_name,".ckr_report_data.outstanding_performing_principal,
  ",db_name,".ckr_report_data.outstanding_overdue_principal,	
  ",db_name,".ckr_report_data.amount_cession,
  ",db_name,".ckr_report_data.monthly_installment,
  ",db_name,".ckr_report_data.codebtor_status,
  ",db_name,".ckr_report_data.guarantor_status
  FROM ",db_name,".ckr_report_data
  INNER JOIN ",db_name,".ckr_reports
  ON ",db_name,".ckr_reports.id=",db_name,".ckr_report_data.report_id
  WHERE ",db_name,".ckr_report_data.type=",type_of," AND ",db_name,
  ".ckr_reports.reportable_id IN (",paste(client_ids$client_id,
  collapse=","),") AND ",db_name,".ckr_reports.reportable_type=
  'App\\\\Models\\\\Clients\\\\Client'",sep ="")
  
  result_df <- gen_query(con,query_ckr)
  
  if(nrow(result_df)==0 & incl_ids == 0){
    empty_df <- as.data.frame(cbind(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))
    names(empty_df) <- names_col[-1]
    return(empty_df)
  }
  
  if(nrow(result_df)==0 & incl_ids == 1){
    empty_df <- as.data.frame(cbind(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))
    names(empty_df) <- names_col
    return(empty_df)
  }
  
  result_df <- merge(result_df, all_df[,c("client_id", "date")], 
                     by.x = "reportable_id", by.y = "client_id", all.x = T)
  colnames(result_df)[ncol(result_df)] <- "date_curr"
  result_df$date_diff <- difftime(result_df$date_curr, result_df$created_at, 
                                  units=c("days"))
  result_df <- result_df[order(result_df$date_diff),]
  
  result_final <- result_df %>%
    group_by(reportable_id) %>%
    arrange(date_diff) %>%
    filter(!(crit != 0 & row_number() == 1 & n() > 1)) %>%
    summarise(across(everything(), ~ first(na.omit(.)),
                     .names = "final_{.col}"), .groups = "drop") %>%
    rename_with(~ gsub("^final_", "", .), everything())
  
  if (incl_ids == 0) {
    result_final <- result_final %>%
      select(all_of(names_col), -reportable_id)
  }
  
  if (incl_ids == 1){
    result_final <- result_final %>%
      select(all_of(names_col)) %>%
      rename("client_id" = "reportable_id")
    
    result_final <- merge(result_final, all_df[,c("client_id", 
                                       "application_id")], by = "client_id")
    result_final <- result_final %>%
      select(application_id, everything(), -client_id)
  }
  return(result_final)
}

# Define query for SEON phone variables
gen_seon_phones_query <- function(db_name,criteria,var){
  return(paste(
"SELECT b.registered 
FROM ",db_name,".seon_requests a
JOIN ",db_name,".seon_requests_accounts b
ON a.id=b.requests_id
WHERE a.type=1 AND b.type=",criteria," AND application_id=",var,
sep=""))
}

# Define query to get max installment amount per application id
gen_max_pmt_main <- function(db_name,id){
  return(paste(
"SELECT max(pmt_final) AS max_pmt
FROM ",db_name,".credits_plan_main
WHERE application_id=",id,sep=""))
}

# Define query to get passed installments at time of choice
gen_passed_install_before_query <- function(db_name,id,time_choice){
  return(paste(
"SELECT COUNT(application_id) as passed_installments
FROM ",db_name,".credits_plan_main 
WHERE application_id=",id," AND pay_day<='",time_choice,"'",sep=""))
}

# Define query to get passed and paid installments at time of choice
gen_passed_paid_install_before_query <- function(db_name,id,time_choice){
  return(paste(
"SELECT COUNT(application_id) as passed_installments
FROM ",db_name,".credits_plan_main 
WHERE payed_at IS NOT NULL AND application_id=",id," AND pay_day<='",
time_choice,"'",sep=""))
}

# Define read credits plan main (plain and simple)
gen_get_credits_plan_main_query <- function(db_name,id){
  return(paste(
"SELECT pay_day
FROM ",db_name,".credits_plan_main 
WHERE application_id=",id,sep=""))
}

# Read PO terminated data per client_id
gen_po_terminated_query <- function(db_name,input){
  return(paste(
    "SELECT id, client_id, application_id, credit_amount, installment_amount,
     deleted_at, created_at, product_id 
     FROM ",db_name,".clients_prior_approval_applications 
     WHERE client_id=",input,
    sep=""))
}

# Read PO refinance data per client_id
gen_po_refinance_query <- function(db_name,input){
  return(paste(
    "SELECT application_id, max_amount, max_installment,
     deleted_at, created_at, product_id
     FROM ",db_name,".prior_approval_refinances 
     WHERE application_id IN (", input,")",sep=""))
}

# Read if client is dead
gen_flag_is_dead <- function(db_name,input){
  return(paste(
    "SELECT dead_at 
     FROM ",db_name,".clients
     WHERE id=",input,sep=""))
}


# Read PO refinance data per client_id
gen_flag_gdpr <- function(db_name,input,input_brand){
  return(paste(
    "SELECT gdpr_marketing_messages
     FROM ",db_name,".client_brand
     WHERE client_id=",input," AND brand_id=",input_brand,sep=""))
}

# Get discount amount per application_id
gen_discount_amount <- function(db_name,input){
  return(paste(
    "SELECT SUM(discount_amount) AS discount_amount
     FROM ",db_name,".credits_plan_main
     WHERE application_id=",input,sep=""))
}

# Get taxes per credit
gen_taxes_amount <- function(db_name,input){
  return(paste(
    "SELECT SUM(amount) AS tax_amount
     FROM ",db_name,".credits_plan_taxes
     WHERE tax_id NOT IN (4,22) AND 
     application_id=",input,sep=""))
}

# Read ACTIVE PO refinance data per client_id
gen_po_active_refinance_query <- function(db_name,input){
  return(paste(
    "SELECT application_id, max_amount, deleted_at, product_id
     FROM ",db_name,".prior_approval_refinances 
     WHERE deleted_at IS NULL AND application_id IN (", 
    input,")",sep=""))
}

# Read all phone numbers
gen_get_phone_numbers <- function(db_name,all_df){
  return(paste(
    "SELECT number 
    FROM ",db_name,".clients_phones
    WHERE client_id=",all_df$client_id,sep=""))
}

# Read all emails
gen_get_email <- function(db_name,all_df){
  return(paste(
    "SELECT email 
    FROM ",db_name,".clients 
    WHERE id=",all_df$client_id,sep=""))
}

# Get if office is self approval 
gen_self_approval_office_query <- function(db_name,input){
  return(paste(
    "SELECT self_approve
     FROM ",db_name,".structure_offices 
     WHERE id IN (", 
    input,")",sep=""))
}

# Get API data
gen_api_data <- function(db_name,application_id){
  return(paste(
    "SELECT CAST(payload AS CHAR)
     FROM ",db_name,".api_credits_applications
     WHERE application_id IN (", 
    application_id,")",sep=""))
}

# Get play dates on Credirect's site
gen_play_query <- function(db_name,input){
  return(paste(
    "SELECT a.created_at, a.description
    FROM ",db_name,".loyal_program_wallet_logs a
    INNER JOIN citycash.loyal_program_wallets b
    ON a.wallet_id=b.id
    WHERE b.client_id =",input,sep=""))
}

# Define constant installment ratio at fixed amount
gen_set_installment_query <- function(db_name,product,crit){
  return(paste("SELECT * FROM ", db_name, ".products_periods_and_amounts
               WHERE product_id IN (",
               product, ") AND amount=",crit, sep=""))
}

# Read if client is judicial 
gen_flag_judges_us <- function(db_name,input){
  return(paste(
    "SELECT judge_us_at 
     FROM ",db_name,".clients
     WHERE id=",input,sep=""))
}

# Read if client is judicial 
gen_thid_side <- function(db_name,input){
  return(paste(
    "SELECT third_side_date 
     FROM ",db_name,".credits_applications
     WHERE id IN (",input,")",sep=""))
}

# Read all clients 
gen_all_clients <- function(db_name){
  return(paste(
"SELECT 
",db_name,".credits_applications.id, 
",db_name,".credits_applications.signed_at,
",db_name,".credits_applications.deactivated_at,
",db_name,".credits_applications.sub_status,
",db_name,".credits_plan_contract.amount,
",db_name,".credits_plan_contract.installments,
",db_name,".credits_applications.product_id,
",db_name,".credits_applications.client_id,
",db_name,".products.brand_id
FROM ",db_name,".credits_applications
LEFT JOIN ",db_name,".credits_plan_contract
ON ",db_name,".credits_applications.id = ",db_name,
".credits_plan_contract.application_id
LEFT JOIN ",db_name,".products
ON ",db_name,".credits_applications.product_id = ",db_name,
".products.id
WHERE ",db_name,".credits_applications.status IN (4,5)",sep=""))
}

# Get payments of all clients
gen_all_payments <- function(db_name){
  return(paste(
"SELECT object_id, SUM(amount) AS amount_paid 
FROM ",db_name,".cash_flow
WHERE nomenclature_id in (90,100,101,102) 
AND deleted_at IS NULL AND object_type=4
GROUP BY object_id",sep=""))
}

# Get score of all clients
gen_all_scores <- function(db_name,indexx){
  return(paste(
"SELECT application_id, amount, period, score
FROM ",db_name,".credits_applications_scoring WHERE application_id>",
indexx,sep=""))
}

# Get all credits in list of clients
gen_credits_of_clients <- function(db_name,list){
  return(paste(
"SELECT a.client_id, a.product_id, a.sub_status, a.status, b.brand_id 
FROM ",db_name,".credits_applications a
LEFT JOIN ",db_name,".products b
ON a.product_id = b.id
WHERE client_id IN (",list,") AND STATUS IN (4,5)"
    ,sep=""))
}

# Get ptc of credits
gen_ptc_list_query <- function(db_name,list){
  return(paste(
"SELECT application_id, ptc_score
FROM ",db_name,".credits_applications_ptc_score
WHERE application_id IN (",list,")"
    ,sep=""))
}

# Get if next salary
gen_payday_query <- function(db_name){
  return(paste(
"SELECT id, type
FROM ",db_name,".products",sep=""))
}

# Define query for getting all credits for client 
gen_all_credits_list_query <- function(db_name,string_list){
  return(paste("SELECT a.id, a.client_id, a.signed_at, a.created_at, 
  a.deactivated_at, a.status, a.sub_status, b.brand_id
  FROM ",db_name,".credits_applications a
  LEFT JOIN ",db_name,".products b ON a.product_id = b.id 
  WHERE a.client_id IN (",string_list,") AND a.status IN (4,5)",sep =""))
}

# Get status of work and employement
gen_get_work_info_query <- function(db_name,string_list){
   return(paste("SELECT application_id, experience_employer, 
    status AS status_work FROM ",db_name,".credits_applications_clients_work 
    WHERE application_id IN (",string_list,")",sep=""))
}

# Get api scoring data
gen_api_score_query <- function(db_name,application_id){
  return(paste("SELECT CAST(request_payload AS CHAR) AS request_payload, 
  created_at FROM ",db_name,".api_scoring WHERE id IN
  (",application_id,")",sep=""))
}

# Get client_id if has egn
gen_client_id_query <- function(db_name,all_df){
  return(paste("SELECT id FROM ",db_name,".clients WHERE egn = 
  (",all_df$egn,")",sep=""))
}

# Get city pop of current address 
gen_city_pop_query <- function(db_name,id){
  return(paste("SELECT id, population FROM ",db_name,".cities WHERE id = 
  (",id,")",sep=""))
}

# Get coordinates of offices 
gen_office_coordinates <- function(db_name,input){
  return(paste(
    "SELECT name, longitude AS lon, latitude AS lat 
    FROM ",db_name,".structure_offices WHERE longitude IS NOT NULL and id = ",
    input,sep=""))
}
