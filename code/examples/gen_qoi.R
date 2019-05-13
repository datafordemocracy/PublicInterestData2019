# Generate quantities of interest (qoi) for (some) generalized linear models
# Adpated from work shared by Charlotte McClintock and Jan Kropko
# April 2019

# input group variable (grp), data frame (df), model object (mod): gen_qoi(grp, df, mod)
# output qoi (named whatever you want): 
#   predicted probabilites/counts of model outcome for each grp, 
#   along with boostrapped standard errors and credibililty interval bounds

gen_qoi <- function(df, grp, mod){
  grplev <- levels(factor(df[[grp]])) # vector of levels for group var
  grp <-enquo(grp)
  
  # predicted probability/count for glm model
  glm.prob <- sapply(grplev, FUN=function(x){ # get mean predicted probability/count for each level
    mean(predict(mod, type = "response",
                 newdata = mutate(df, !! grp := !! x)), na.rm = TRUE)
  })
  
  qoi <- data.frame(group = grplev,   # combine probabilities/counts into data frame
                    outcome = glm.prob)
  
  # generate bootstrapped credibility intervals 
  B <- coef(mod) # extract coefficients from model
  V <- vcov(mod) # extract covariance matrix from model
  set.seed(1017) # for reproducibility; you can change this
  sim.coefs <- mvrnorm(1000, mu = B, Sigma = V)  # simulate 1000 coefficients (mvrnorm from MASS)
                                                 # (drawn from ~ N(beta, var(beta)))
  
  # use simulated coefficients to generate a distribution of predicted probabilities/counts
  glm.sim <- mod
  sim.qi <- apply(sim.coefs, 1, FUN=function(x){
    glm.sim$coefficients <- x
    glm.prob <- sapply(grplev, FUN=function(y){
      mean(predict(glm.sim, type = "response", 
                   newdata = mutate(df, !! grp := !! y)), na.rm=TRUE)
    })
  })
  sim.qi <- t(sim.qi) # transpose the resulting matrix of predicted probabilites/counts

  se <- apply(sim.qi, 2, sd) # estimate the standard error via the std. dev. of the simulated probabilities
  lower <- apply(sim.qi, 2, FUN=function(x){ # calculate the lower bound of each interval
    quantile(x, .05) # the values with 5% of observations below (for 90% interval)
  })
  upper <- apply(sim.qi, 2, FUN=function(x){
    quantile(x, .95) # the values with 95% of observations above (for 90% interval)
  })
  
  qoi <- mutate(qoi, # add the estimated s.e, lower and upper bound for each predicted probability/count
                se = se,
                lower = lower,
                upper = upper)
}

