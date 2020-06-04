

######################################################
######## Define cutoffs for statistical models  ######
######################################################


#### Cutoffs after Coronavirus

# # Application City Cash
cu_app_city_bad_offices <- c(0.3,0.25,0.2,0.15,0.075)
cu_app_city_norm_offices <- c(0.35,0.3,0.225,0.15,0.075)

# Application Credirect Flex
cu_app_cred_flex <- c(0.125,0.1,0.08,0.05,0.035)

# Application Credirect Potrebitelski
cu_app_cred_user <- c(0.6,0.55,0.475,0.375,0.325)

# Repeat City Cash
cu_beh_city <- c(0.275,0.25,0.2,0.15,0.1)

# Repeat Credirect
cu_beh_cred <- c(0.32,0.275,0.2,0.125,0.075)

# Application Credirect Frauds
cu_app_cred_frauds <- 0.2



#### Cutoffs before Coronavirus 

# # # Application City Cash
# cu_app_city_bad_offices <- c(0.325,0.25,0.2,0.15,0.075)
# cu_app_city_norm_offices <- c(0.375,0.3,0.225,0.15,0.075)
# 
# # Application Credirect Flex
# cu_app_cred_flex <- c(0.25,0.175,0.125,0.075,0.05)
# 
# # Application Credirect Potrebitelski
# cu_app_cred_user <- c(0.25,0.2,0.175,0.125,0.075)
# 
# # Repeat City Cash
# cu_beh_city <- c(0.3,0.25,0.2,0.15,0.1)

# # Repeat Credirect
# cu_beh_cred <- c(0.35,0.275,0.2,0.125,0.075)


#### Oldest cutoffs

# Old application Credirect
#cu_app_cred_flex <- c(0.3,0.175,0.125,0.075,0.05)
#cu_app_cred_user <- c(0.3,0.225,0.175,0.125,0.075)

# Old City Cash
#cu_app_city <- c(0.375,0.3,0.225,0.15,0.075)
#cu_beh_city <- c(0.3,0.25,0.2,0.15,0.1)