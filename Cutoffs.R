

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

cu_collections_citycash <- list(
  dpd10 = c(0, 0.674, 0.763, 0.810, 0.839, 1),
  dpd30 = c(0, 0.346, 0.491, 0.617, 0.736, 1),
  dpd60 = c(0, 0.137, 0.212, 0.306, 0.442, 1),
  dpd90 = c(0, 0.102, 0.157, 0.234, 0.361, 1),
  dpd180 = c(0, 0.086, 0.120, 0.167, 0.280, 1),
  dpd360 = c(0, 0.060, 0.091, 0.126, 0.189, 1))

cu_collections_credirect <- list(
  dpd1 = c(0, 0.418, 0.586, 0.703, 0.777, 1),
  dpd10 = c(0, 0.460, 0.620, 0.710, 0.772, 1),
  dpd30 = c(0, 0.386, 0.536, 0.653, 0.733, 1),
  dpd60 = c(0, 0.346, 0.477, 0.597, 0.697, 1),
  dpd90 = c(0, 0.373, 0.500, 0.606, 0.698, 1),
  dpd180 = c(0, 0.425, 0.546, 0.631, 0.709, 1),
  dpd360 = c(0, 0.461, 0.567, 0.646, 0.717, 1))
