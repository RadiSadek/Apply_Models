

################################################################
######## Define cutoffs for logistical regression models  ######
################################################################


############# City Cash 

# Application City Cash - worst Offices
cu_app_city_bad_offices <- c(0.39,0.35,0.3,0.22,0.17)

# Application City Cash - normal Offices
cu_app_city_norm_offices <- c(0.46,0.35,0.3,0.22,0.17)

# Repeat City Cash
cu_beh_city <- c(0.4,0.3,0.225,0.15,0.075)



############# Credirect 

# Application Credirect PayDay
cu_app_cred_flex <- c(0.175,0.125,0.08,0.05,0.0375)

# Application Credirect Installments
cu_app_cred_user <- c(0.65,0.58,0.5,0.4,0.3)

# Repeat all Credirect
cu_beh_cred <- c(0.575,0.325,0.225,0.15,0.1)

# Application Credirect Frauds
cu_app_cred_frauds <- 0.2

