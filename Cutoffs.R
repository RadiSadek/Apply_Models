

################################################################
######## Define cutoffs for logistical regression models  ######
################################################################


############# City Cash 

# Application City Cash - worst Offices
cu_app_city_bad_offices <- c(0.35,0.32,0.28,0.22,0.15)

# Application City Cash - normal Offices
cu_app_city_norm_offices <- c(0.42,0.35,0.3,0.22,0.15)

# Repeat City Cash
cu_beh_city <- c(0.4,0.3,0.225,0.15,0.075)



############# Credirect 

# Application Credirect PayDay
cu_app_cred_flex <- c(0.36,0.125,0.1,0.07,0.045)

# Application Credirect Installments
cu_app_cred_user <- c(0.68,0.58,0.5,0.425,0.35)

# Repeat all Credirect
cu_beh_cred <- c(0.8,0.4,0.275,0.175,0.1)

# Application Credirect Frauds
cu_app_cred_frauds <- 0.2



########### Cutoffs for probability to churn 

cu_ptc_citycash <- c(0.4,0.35,0.25,0.125)
cu_ptc_gratis <- c(0.51,0.49,0.45,0.38)
cu_ptc_flex <- c(0.25,0.21,0.16,0.11)
cu_ptc_consumer_new  <- c(0.55,0.49,0.41,0.31)
cu_ptc_consumer_rep<- c(0.4,0.3,0.2,0.11)


########## Cutoffs for prescore

cu_app_city_prescore <- c(0.375,0.325,0.3,0.25,0.2)




