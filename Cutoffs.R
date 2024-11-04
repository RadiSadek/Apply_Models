

################################################################
######## Define cutoffs for logistical regression models  ######
################################################################


############# City Cash 

# Application City Cash - worst Offices
cu_app_city_bad_offices <- c(0.35,0.31,0.28,0.24,0.16)

# Application City Cash - normal Offices
cu_app_city_norm_offices <- c(0.41,0.35,0.3,0.24,0.16)

# Repeat City Cash
cu_beh_city <- c(0.4,0.3,0.225,0.15,0.075)



############# Credirect 

# Application Credirect PayDay
cu_app_cred_flex <- c(0.36,0.125,0.1,0.07,0.045)

# Application Credirect Installments
cu_app_cred_user <- c(0.68,0.58,0.5,0.425,0.35)

# Repeat all Credirect
cu_beh_cred <- c(0.77,0.4,0.25,0.15,0.075)

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


##########  Cutoffs for Collections model for CityCash

cu_collections_citycash_old <- list(
  dpd10 = c(0, 0.5634, 0.6815, 0.7561, 0.8132, 1),
  dpd30 = c(0, 0.2563, 0.3622, 0.4766, 0.6043, 1),
  dpd60 = c(0, 0.1014, 0.1405, 0.2003, 0.2971, 1),
  dpd90 = c(0, 0.0742, 0.1136, 0.1686, 0.2492, 1),
  dpd180 = c(0, 0.0295, 0.0353, 0.0535, 0.1051, 1),
  dpd360 = c(0, 0.0268, 0.0277, 0.0308, 0.0482, 1))

cu_collections_citycash <- list(
  dpd10 = c(0, 0.5635, 0.6815, 0.7562, 0.8133, 1),
  dpd30 = c(0, 0.2563, 0.3622, 0.4767, 0.6044, 1),
  dpd60 = c(0, 0.1015, 0.1406, 0.2004, 0.2971, 1),
  dpd90 = c(0, 0.0743, 0.1136, 0.1687, 0.2493, 1),
  dpd180 = c(0, 0.0471, 0.0704, 0.1036, 0.1661, 1),
  dpd360 = c(0, 0.0306, 0.0428, 0.0608, 0.1032, 1))
