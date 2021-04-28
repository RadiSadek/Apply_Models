

################################################################
######## Define cutoffs for logistical regression models  ######
################################################################


############# City Cash 

# Application City Cash - worst Offices
cu_app_city_bad_offices <- c(0.325,0.25,0.2,0.15,0.075)

# Application City Cash - normal Offices
cu_app_city_norm_offices <- c(0.375,0.3,0.225,0.15,0.075)

# Repeat City Cash
cu_beh_city <- c(0.4,0.3,0.225,0.15,0.075)



############# Credirect 

# Application Credirect PayDay
cu_app_cred_flex <- c(0.15,0.125,0.08,0.05,0.035)

# Application Credirect Installments
cu_app_cred_user <- c(0.6,0.535,0.5,0.4,0.325)

# Repeat all Credirect
cu_beh_cred <- c(0.575,0.325,0.225,0.15,0.1)

# Application Credirect Frauds
cu_app_cred_frauds <- 0.2

