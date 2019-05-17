# Generate quantities of interest (qoi) for ordered logit/proibt models
# Adpated from work shared by Charlotte McClintock and Jan Kropko
# May 2019

# input group variable (grp), data frame (df), model object (mod): gen_qoi_ol(grp, df, mod)
# output qoi (named whatever you want): 
#   boostrapped standard errors and credibililty interval bounds.
#   note: average predicted probabilites of model outcomes for each grp currently calculated prior to using this function. 

gen_qoi_ol <- function(df, grp, mod){
  grplev <- levels(factor(df[[grp]])) # vector of levels for group var
  grp <- enquo(grp)
  
  # # predicted probability/count for glm model
  # glm.prob <- sapply(grplev, FUN=function(x){ # get mean predicted probabilityfor each level
  #   colMeans(predict(mod, type="probs",
  #                newdata = mutate(df, !! grp := !! x)), na.rm = TRUE)
  # })
  # 
  # 
  # qoi <- data.frame(t(glm.prob))   # combine probabilities/counts into data frame
  # 
  # # add group categories to dataframe
  # qoi <- mutate(qi.df, !! grp := rownames(qoi))
  # qoi <- dplyr::select(qoi, !! grp, everything()) # move group to the left
  # qoi <- gather(qoi, -!! grp, key = "QI", value = "point.estimate")  # reshape the data into a long format
  # 

  # generate bootstrapped credibility intervals
  B <- c(coef(mod), mod$zeta) # extract coefficients from model
  V <- vcov(mod) # extract covariance matrix from model
  set.seed(1017) # for reproducibility; you can change this
  sim.coefs <- mvrnorm(1000, mu = B, Sigma = V)  # simulate 1000 coefficients (mvrnorm from MASS)
  # (drawn from ~ N(beta, var(beta)))

  # use simulated coefficients to generate a distribution of predicted probabilities
  glm.sim <- mod
  numcoef <- length(coef(find3))
  numparam <- dim(V)[1]
  sim.qi <- apply(sim.coefs, 1, FUN=function(s){
    newcoef <- s[1:numcoef]
    newcutpoints <- s[(numcoef+1):numparam]
    glm.sim$coefficients <- newcoef
    glm.sim$zeta <- newcutpoints
    glm.prob <- sapply(grplev, FUN=function(x){
      colMeans(predict(glm.sim, type = "probs",
                   newdata = mutate(df, !! grp := !! x)), na.rm=TRUE)
    })
    sim.qi.se <- data.frame(t(glm.prob))
    sim.qi.se <- mutate(sim.qi.se, !! grp := rownames(sim.qi.se))
    sim.qi.se <- dplyr::select(sim.qi.se, !! grp, everything())
    sim.qi.se <- gather(sim.qi.se, -!! grp, key="QI", value="point.estimate")
    return(sim.qi.se)
  })

  # append simulated QI data
  sim.qi <- plyr::rbind.fill(sim.qi)
}
#   # calculate the standard deviation, 5th, and 95th percentiles
#   sim.qi <- sim.qi %>% 
#     group_by(!! grp, !! QI) %>% 
#     summarize(se = sd(point.estimate),
#               lower = quantile(point.estimate, .05),
#               upper = quantile(point.estimate, .95))
# }
#   # add bounds and standard errors into the qi.df data frame
#   qoi <- full_join(qoi, sim.qi)
# 
# }

