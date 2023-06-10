library(tidyverse)
library(boot)
library(quantreg)
library(dineq)

#------------------------------------------------------------------------------
# OLS
#------------------------------------------------------------------------------
ols <- lm(sbp ~ schlyrs + age + age2 + female + black + latinx + southern + 
            mom_ed + dad_ed + y08 + y10 + y12 + y14 + y16 + y18, data = data)
summary(ols)


#Bootstrap ci
boot_lm <- function(data, id){
  fit <- lm(sbp ~ schlyrs + age + age2 + female + black + latinx + southern + 
              mom_ed + dad_ed + y08 + y10 + y12 + y14 + y16 + y18,
            data = data[id, ])
  coef(fit)
  
}

b <- boot(data, boot_lm, 500)
ols_ci <- data.frame("index" = NA,
                     "lower" = NA,
                     "upper" = NA)

for(i in 1:nrow(summary(ols)$coefficient)){
  
  boot <- boot.ci(b, index = i, type = "perc")
  ols_ci[i, "index"] = i
  ols_ci[i, "lower"] = boot$percent[, 4]
  ols_ci[i, "upper"] = boot$percent[, 5]
  
}

ols_est <- cbind(ols_ci[, -1], summary(ols)$coefficient)
rownames(ols_est) <- rownames(summary(ols)$coefficient)
ols_est

#------------------------------------------------------------------------------
# Conditional
#------------------------------------------------------------------------------
#Function
cqr_func <- function(data){
  
  conditional_results <- data.frame()
  
  for(i in seq(0.1, 0.9, by = 0.01)){
    
    i <- round(i, 2)
    
    con <- rq(sbp ~ schlyrs + age + age2 + female + black + latinx + southern + 
                mom_ed + dad_ed + y08 + y10 + y12 + y14 + y16 + y18,
              data = data, tau = i) 
    coef <- summary(con, se = "boot", bsmethod = "mcmb", R = 500) #Bootstrapped SE
    
    #Bootstrap ci
    boot <- boot.rq(cbind(1, data$schlyrs, data$age, data$age2, data$female,
                          data$black, data$latinx, data$southern, data$mom_ed,
                          data$dad_ed, data$y08, data$y10, data$y12, data$y14,
                          data$y16, data$y18),
                    data$sbp, tau = i, R = 500) #Takes a little while to run
    ci <- t(apply(boot$B, 2, quantile, c(0.025, 0.975)))
    
    cqr_est <- cbind(i, ci, coef$coefficient)
    rownames(cqr_est) <- rownames(coef$coefficient)
    
    schlyr_est <- data.frame(t(cqr_est["schlyrs", 1:4]))
    
    conditional_results <- rbind(conditional_results, schlyr_est)
    
  }
  
  names(conditional_results) <- c("quantile", "lower_ci", "upper_ci",
                                  "estimate")
  conditional_results <- conditional_results %>%
    dplyr::select(quantile, lower_ci, estimate, upper_ci)
  return(conditional_results)
  
}

cqr_results <- cqr_func(data)
#Nonunique solutions warning prompted by categorical variables in model

#------------------------------------------------------------------------------
# UQR - RIF
#------------------------------------------------------------------------------
#Function
uqr_func <- function(data){
  
  unconditional_results <- data.frame()
  
  for(i in seq(0.1, 0.9, by = 0.01)){
    
    i <- round(i, 2)
    
    data$rif_sbp <- rif(data$sbp, weights = NULL, method = "quantile",
                        quantile = i)
    uqr <- lm(rif_sbp ~ schlyrs + age + age2 + female + black + latinx +
                southern + mom_ed + dad_ed + y08 + y10 + y12 + y14 + y16 + y18, 
              data = data)
    
    #Bootstrap ci
    boot_uqr <- function(data, id){
      
      fit <- lm(rif_sbp ~ schlyrs + age + age2 + female + black + latinx +
                  southern + mom_ed + dad_ed + y08 + y10 + y12 + y14 + y16 + 
                  y18, data = data[id, ])
      coef(fit)
    }
    
    b <- boot(data, boot_uqr, 500)
    uqr_ci <- data.frame("quantile" = NA,
                         "lower_ci" = NA,
                         "upper_ci" = NA)
    
    
    for(j in 1:nrow(summary(uqr)$coefficient)){
      
      boot <- boot.ci(b, index = j, type = "perc")
      uqr_ci[j, "quantile"] = i
      uqr_ci[j, "lower_ci"] = boot$percent[, 4]
      uqr_ci[j, "upper_ci"] = boot$percent[, 5]
    }
    
    uqr_est <- cbind(uqr_ci, summary(uqr)$coefficient)
    
    uqr_schlyr_est <- data.frame(uqr_est[2, 1:4])
    
    unconditional_results <- rbind(unconditional_results, uqr_schlyr_est)
    
  }
  
  names(unconditional_results) <- c("quantile", "lower_ci", "upper_ci",
                                    "estimate")
  unconditional_results <- unconditional_results %>%
    dplyr::select(quantile, lower_ci, estimate, upper_ci)
  return(unconditional_results)
  
}

uqr_results <- uqr_func(data)


