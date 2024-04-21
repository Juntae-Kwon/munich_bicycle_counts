#' simulation for generating prediction intervals for each observation
#' 
#' @param model (gam) a gam model for either direction 1 or 2
#' @param data  (data frame / data table) whose columns must contain the same ones used in model
#' @param direction (integer) either 1 or 2
#' @param n_sims (integer) the number of simulations
#' @param (numeric) the level of confidence between 0 and 1
#' @param (integer) to fix the seed for reproduction
#' 
#' @return (data frame / data table) containing lower/upper bounds of prediction intervals 

sim_pi <- function(model, data, direction, n_sims, confidence, seed) {
  
  set.seed(seed)
  require(mgcv)
  
  Z <- predict(model, data, type = "lpmatrix") # design matrix with basis function evaluations
  
  B <- rmvn(n_sims, coef(model), model$Vp) # the limiting distribution
  
  mu_mat <- matrix(0, nrow = nrow(data), ncol = n_sims)
  
  for(i in 1:n_sims) {
    eta <- Z %*% B[i, ] # linear predictor
    mu_mat[, i] <- exp(eta) # estimated means
  }
  
  theta <- model$family$getTheta(trans = TRUE) # dispersion parameter
  
  ret <- matrix(
    rnbinom(nrow(data) * n_sims, mu = as.vector(mu_mat), size = theta), # generate random number from NB
    nrow = nrow(data)
  )
  
  ret <- as.data.frame(ret)
  
  alpha = 1 - confidence
  pis <- t(apply(ret, 1, quantile, c(alpha/2, 1-alpha/2), na.rm = TRUE)) # compute quantiles for each observation
  
  if (direction == 1) {
    data <- data %>%
      mutate(
        y_pred = model$family$linkinv(predict(model, data, type = 'link')),
        lb = pis[, 1],
        ub = pis[, 2],
        cov = direction_1 <= ub & direction_1 >= lb)
  } else if(direction == 2) {
    data <- data %>%
      mutate(
        y_pred = model$family$linkinv(predict(model, data, type = 'link')),
        lb = pis[, 1],
        ub = pis[, 2],
        cov = direction_2 <= ub & direction_2 >= lb)
  } else {
    stop("direction must be either 1 or 2")
  }
  
  return(data)
}


#' algorithm to mark strong outliers using the concept of boxplots
#' 
#' @param data (data frame / data table) that includes prediction intervals for each observation
#' @param direction (integer) either 1 or 2
#' 
#' @return (data frame / data table) that contains criteria for strong outliers

mark_strong_outliers <- function(data, direction) {
  
  require(mgcv)
  
  if(direction == 1) {
    # compute the L1 distance
    data <- data %>%
      filter(cov == FALSE) %>%
      mutate(distance = abs(direction_1 - y_pred))
    
    # compute Q1, Q3 and IQR of distance
    Q1 <- quantile(data$distance, 0.25, na.rm = TRUE)
    Q3 <- quantile(data$distance, 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    
    # mutate lower and upper bound as criterion
    data <- data %>%
      mutate(lower_bound_narrow = Q1 - 1.5 * IQR,
             upper_bound_narrow = Q3 + 1.5 * IQR) %>%
      arrange(-distance) %>%
      filter(distance < lower_bound_narrow | distance > upper_bound_narrow)
    
  } else if(direction == 2) {
    # compute the L1 distance
    data <- data %>%
      filter(cov == FALSE) %>%
      mutate(distance = abs(direction_2 - y_pred))
    
    # compute Q1, Q3 and IQR of distance
    Q1 <- quantile(data$distance, 0.25, na.rm = TRUE)
    Q3 <- quantile(data$distance, 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    
    # mutate lower and upper bound as criterion
    data <- data %>%
      mutate(lower_bound_narrow = Q1 - 1.5 * IQR,
             upper_bound_narrow = Q3 + 1.5 * IQR) %>%
      arrange(-distance) %>%
      filter(distance < lower_bound_narrow | distance > upper_bound_narrow)
    
  } else {
    stop("direction must be either 1 or 2")
  }
  
  return(data)
}