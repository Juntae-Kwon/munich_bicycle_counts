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
  s(lag1_d1, bs = 'bs', k = 20) +
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

# plots of partial effects of the model
plot(m_Arnulf_1_lag0, scale = 0, pages = 1, all.terms = TRUE, trans = exp)
plot(m_Margareten_2_lag0, scale = 0, pages = 1, all.terms = TRUE, trans = exp)


#-------------------------------------#
# simulation for prediction intervals #
#-------------------------------------#

# Arnulf
spi_Arnulf_1_lag0 <- sim_pi(m_Arnulf_1_lag0, df_Arnulf, 1, 1000, 0.995, 1)
spi_Arnulf_1_lag1 <- sim_pi(m_Arnulf_1_lag1, df_Arnulf, 1, 1000, 0.995, 1)
spi_Arnulf_2_lag0 <- sim_pi(m_Arnulf_2_lag0, df_Arnulf, 2, 1000, 0.995, 1)
spi_Arnulf_2_lag1 <- sim_pi(m_Arnulf_2_lag1, df_Arnulf, 2, 1000, 0.995, 1)

# Kreuther
spi_Kreuther_1_lag0 <- sim_pi(m_Kreuther_1_lag0, df_Kreuther, 1, 1000, 0.995, 1)
spi_Kreuther_1_lag1 <- sim_pi(m_Kreuther_1_lag1, df_Kreuther, 1, 1000, 0.995, 1)
spi_Kreuther_2_lag0 <- sim_pi(m_Kreuther_2_lag0, df_Kreuther, 2, 1000, 0.995, 1)
spi_Kreuther_2_lag1 <- sim_pi(m_Kreuther_2_lag1, df_Kreuther, 2, 1000, 0.995, 1)

# Olympia
spi_Olympia_1_lag0 <- sim_pi(m_Olympia_1_lag0, df_Olympia, 1, 1000, 0.995, 1)
spi_Olympia_1_lag1 <- sim_pi(m_Olympia_1_lag1, df_Olympia, 1, 1000, 0.995, 1)
spi_Olympia_2_lag0 <- sim_pi(m_Olympia_2_lag0, df_Olympia, 2, 1000, 0.995, 1)
spi_Olympia_2_lag1 <- sim_pi(m_Olympia_2_lag1, df_Olympia, 2, 1000, 0.995, 1)

# Hirsch
spi_Hirsch_1_lag0 <- sim_pi(m_Hirsch_1_lag0, df_Hirsch, 1, 1000, 0.995, 1)
spi_Hirsch_1_lag1 <- sim_pi(m_Hirsch_1_lag1, df_Hirsch, 1, 1000, 0.995, 1)
spi_Hirsch_2_lag0 <- sim_pi(m_Hirsch_2_lag0, df_Hirsch, 2, 1000, 0.995, 1)
spi_Hirsch_2_lag1 <- sim_pi(m_Hirsch_2_lag1, df_Hirsch, 2, 1000, 0.995, 1)

# Margareten
spi_Margareten_1_lag0 <- sim_pi(m_Margareten_1_lag0, df_Margareten, 1, 1000, 0.995, 1)
spi_Margareten_1_lag1 <- sim_pi(m_Margareten_1_lag1, df_Margareten, 1, 1000, 0.995, 1)
spi_Margareten_2_lag0 <- sim_pi(m_Margareten_2_lag0, df_Margareten, 2, 1000, 0.995, 1)
spi_Margareten_2_lag1 <- sim_pi(m_Margareten_2_lag1, df_Margareten, 2, 1000, 0.995, 1)

# Erhardt
spi_Erhardt_1_lag0 <- sim_pi(m_Erhardt_1_lag0, df_Erhardt, 1, 1000, 0.995, 1)
spi_Erhardt_1_lag1 <- sim_pi(m_Erhardt_1_lag1, df_Erhardt, 1, 1000, 0.995, 1)
spi_Erhardt_2_lag0 <- sim_pi(m_Erhardt_2_lag0, df_Erhardt, 2, 1000, 0.995, 1)
spi_Erhardt_2_lag1 <- sim_pi(m_Erhardt_2_lag1, df_Erhardt, 2, 1000, 0.995, 1)


#------------------------#
# obtain strong outliers #
#------------------------#

mark_strong_outliers(spi_Arnulf_1_lag0, 1)



