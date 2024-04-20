library(mgcv)

#-------------------------------#
# Fitting a gam in presentation #
#-------------------------------#

# Formula for each model
# direction 1
formula_1_lag0 <- 
  direction_1 ~ 
  s(air_temp, bs = 'bs', k = 30) + 
  s(hour_weekday, bs = 'cc', k = 60) + 
  s(month_year, bs = 'tp', k = 40) +
  ind +
  holiday

formula_1_lag1 <- 
  direction_1 ~ 
  s(air_temp, bs = 'bs', k = 30) + 
  s(hour_weekday, bs = 'cc', k = 60) + 
  s(month_year, bs = 'tp', k = 40) +
  s(lag1_d1, bs = 'bs', k = 20)
  ind +
  holiday

# direction 2
formula_2_lag0 <-
  direction_2 ~
  s(air_temp, bs = 'bs', k = 30) +
  s(hour_weekday, bs = 'cc', k = 60) +
  s(month_year, bs = 'tp', k = 40) +
  ind +
  holiday

formula_2_lag1 <-
  direction_2 ~
  s(air_temp, bs = 'bs', k = 30) +
  s(hour_weekday, bs = 'cc', k = 60) +
  s(month_year, bs = 'tp', k = 40) +
  s(lag1_d1, bs = 'bs', k = 20)
ind +
  holiday  

# Fit models to data for all station
# Arnulf
m_Arnulf_1_lag0 <- gam(formula = formula_1_lag0, data = df_Arnulf, family = nb, method = "REML")
m_Arnulf_1_lag1 <- gam(formula = formula_1_lag1, data = df_Arnulf, family = nb, method = "REML")
m_Arnulf_2_lag0 <- gam(formula = formula_2_lag0, data = df_Arnulf, family = nb, method = "REML")
m_Arnulf_2_lag1 <- gam(formula = formula_2_lag1, data = df_Arnulf, family = nb, method = "REML")

# Kreuther
m_Kreuther_1_lag0 <- gam(formula = formula_1_lag0, data = df_Kreuther, family = nb, method = "REML")
m_Kreuther_1_lag1 <- gam(formula = formula_1_lag1, data = df_Kreuther, family = nb, method = "REML")
m_Kreuther_2_lag0 <- gam(formula = formula_2_lag0, data = df_Kreuther, family = nb, method = "REML")
m_Kreuther_2_lag1 <- gam(formula = formula_2_lag1, data = df_Kreuther, family = nb, method = "REML")

# Olympia
m_Olympia_1_lag0 <- gam(formula = formula_1_lag0, data = df_Olympia, family = nb, method = "REML")
m_Olympia_1_lag1 <- gam(formula = formula_1_lag1, data = df_Olympia, family = nb, method = "REML")
m_Olympia_2_lag0 <- gam(formula = formula_2_lag0, data = df_Olympia, family = nb, method = "REML")
m_Olympia_2_lag1 <- gam(formula = formula_2_lag1, data = df_Olympia, family = nb, method = "REML")

# Hirsch
m_Hirsch_1_lag0 <- gam(formula = formula_1_lag0, data = df_Hirsch, family = nb, method = "REML")
m_Hirsch_1_lag1 <- gam(formula = formula_1_lag1, data = df_Hirsch, family = nb, method = "REML")
m_Hirsch_2_lag0 <- gam(formula = formula_2_lag0, data = df_Hirsch, family = nb, method = "REML")
m_Hirsch_2_lag1 <- gam(formula = formula_2_lag1, data = df_Hirsch, family = nb, method = "REML")

# Margareten
m_Margareten_1_lag0 <- gam(formula = formula_1_lag0, data = df_Margareten, family = nb, method = "REML")
m_Margareten_1_lag1 <- gam(formula = formula_1_lag1, data = df_Margareten, family = nb, method = "REML")
m_Margareten_2_lag0 <- gam(formula = formula_2_lag0, data = df_Margareten, family = nb, method = "REML")
m_Margareten_2_lag1 <- gam(formula = formula_2_lag1, data = df_Margareten, family = nb, method = "REML")

# Erhardt
m_Erhardt_1_lag0 <- gam(formula = formula_1_lag0, data = df_Erhardt, family = nb, method = "REML")
m_Erhardt_1_lag1 <- gam(formula = formula_1_lag1, data = df_Erhardt, family = nb, method = "REML")
m_Erhardt_2_lag0 <- gam(formula = formula_2_lag0, data = df_Erhardt, family = nb, method = "REML")
m_Erhardt_2_lag1 <- gam(formula = formula_2_lag1, data = df_Erhardt, family = nb, method = "REML")
