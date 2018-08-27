print(ch9.cityyear.assoc, digits=4)

set.seed(1337)

beta <- fixef(ch9.cityyear.assoc)
betas <- beta[,1]

covvar <- vcov(ch9.cityyear.assoc)
coefs.sim <- mvrnorm(10000, betas, covvar)

# Setting values for Internet penetration

summary(log(cityyear.data$numactivepc_l+1))
numactivepc <- 0
numactivepc_mod <- 0.38696

# Setting values for peace duration

summary(cityyear.data$v2x_frassoc_thick_l)
v2x_frassoc_thick_l <- c(0.02804,0.44023)

#bind them together in a matrix   

effects0 <- cbind(1,
                  numactivepc,
                  v2x_frassoc_thick_l,
                  0,
                  0,
                  0,
                  0,
                  1,0,
                  0,
                  numactivepc*v2x_frassoc_thick_l)

effects1 <- cbind(1,
                  numactivepc_mod,
                  v2x_frassoc_thick_l,
                  0,
                  0,
                  0,
                  0,
                  1,0,
                  0,
                  numactivepc_mod*v2x_frassoc_thick_l)

#multiply the effects matrix with the matrix of simulated coefficients to obtain marginal effects
results0 <- coefs.sim %*% t(effects0)
results1 <- coefs.sim %*% t(effects1)

#evaluate each marginal effect using inverse logit, to obtain predicted probabilities
results0 <- apply(results0,1:2, function(x) invlogit(x))
results1 <- apply(results1,1:2, function(x) invlogit(x))

# subtract the estimated probabilities from each other
results <- results1 - results0

means <- apply(results,2,mean)
sdUpper <- apply(results,2,function(x) quantile(x, .975))
sdLower <- apply(results,2,function(x) quantile(x, .025))

means
sdUpper
sdLower

results.secdiff <- as.data.frame(results[,2]-results[,1])

means <- apply(results.secdiff,2,mean)
sdUpper <- apply(results.secdiff,2,function(x) quantile(x, .975))
sdLower <- apply(results.secdiff,2,function(x) quantile(x, .025))

means
sdUpper
sdLower

rm(list=setdiff(ls(), c("cityyear.data", 
                        "cityweek.data.persistence", 
                        "cityweek.data.diffusion", 
                        "ch9.cityyear.assoc", 
                        "ch9.cityweek.persistence.assoc", 
                        "ch9.cityweek.diffusion.assoc",
                        "ch9.cityyear.country")))



















print(ch9.cityyear.assoc, digits=4)

set.seed(1337)

beta <- fixef(ch9.cityyear.assoc)
betas <- beta[,1]

covvar <- vcov(ch9.cityyear.assoc)
coefs.sim <- mvrnorm(10000, betas, covvar)

# Setting values for Internet penetration

summary(log(cityyear.data$numactivepc_l+1))
numactivepc <- c(0, 0.38696)

# Setting values for peace duration

summary(cityyear.data$v2x_frassoc_thick_l)
v2x_frassoc_thick_l <- 0.10054
v2x_frassoc_thick_l_mod <- 0.44023

#bind them together in a matrix   

effects0 <- cbind(1,
                  numactivepc,
                  v2x_frassoc_thick_l,
                  0,
                  0,
                  0,
                  0,
                  1,0,
                  0,
                  numactivepc*v2x_frassoc_thick_l)

effects1 <- cbind(1,
                  numactivepc,
                  v2x_frassoc_thick_l_mod,
                  0,
                  0,
                  0,
                  0,
                  1,0,
                  0,
                  numactivepc*v2x_frassoc_thick_l_mod)

#multiply the effects matrix with the matrix of simulated coefficients to obtain marginal effects
results0 <- coefs.sim %*% t(effects0)
results1 <- coefs.sim %*% t(effects1)

#evaluate each marginal effect using inverse logit, to obtain predicted probabilities
results0 <- apply(results0,1:2, function(x) invlogit(x))
results1 <- apply(results1,1:2, function(x) invlogit(x))

# subtract the estimated probabilities from each other
results <- results1 - results0

means <- apply(results,2,mean)
sdUpper <- apply(results,2,function(x) quantile(x, .975))
sdLower <- apply(results,2,function(x) quantile(x, .025))

means
sdUpper
sdLower

results.secdiff <- as.data.frame(results[,2]-results[,1])

means <- apply(results.secdiff,2,mean)
sdUpper <- apply(results.secdiff,2,function(x) quantile(x, .975))
sdLower <- apply(results.secdiff,2,function(x) quantile(x, .025))

means
sdUpper
sdLower

rm(list=setdiff(ls(), c("cityyear.data", 
                        "cityweek.data.persistence", 
                        "cityweek.data.diffusion", 
                        "ch9.cityyear.assoc", 
                        "ch9.cityweek.persistence.assoc", 
                        "ch9.cityweek.diffusion.assoc",
                        "ch9.cityyear.country")))





















print(ch9.cityweek.diffusion.assoc, digits=4)

set.seed(1337)

beta <- fixef(ch9.cityweek.diffusion.assoc)
betas <- beta[,1]

covvar <- vcov(ch9.cityweek.diffusion.assoc)
coefs.sim <- mvrnorm(10000, betas, covvar)

# Setting values for receiving Internet penetration

summary(log(cityweek.data.diffusion$numactivepc_l+1))
receiving_internet <- 0.41085

# Setting values for sendign Internet penetration

summary(cityweek.data.diffusion$lastprotestelsewhere_lnumactivepc_max)
sending_internet <- c(0,1.4733)

# Setting values for association

summary(cityweek.data.diffusion$v2x_frassoc_thick_l)
v2x_frassoc_thick_l <- 0.101
v2x_frassoc_thick_l_mod <- 0.440

#bind them together in a matrix   

effects0 <- cbind(1,
                  receiving_internet,
                  sending_internet,
                  v2x_frassoc_thick_l,
                  log(2),
                  log(12),
                  0,
                  0,
                  1,0,
                  0,
                  sending_internet*v2x_frassoc_thick_l)

effects1 <- cbind(1,
                  receiving_internet,
                  sending_internet,
                  v2x_frassoc_thick_l_mod,
                  log(2),
                  log(12),
                  0,
                  0,
                  1,0,
                  0,
                  sending_internet*v2x_frassoc_thick_l_mod)


#multiply the effects matrix with the matrix of simulated coefficients to obtain marginal effects
results0 <- coefs.sim %*% t(effects0)
results1 <- coefs.sim %*% t(effects1)

#evaluate each marginal effect using inverse logit, to obtain predicted probabilities
results0 <- apply(results0,1:2, function(x) invlogit(x))
results1 <- apply(results1,1:2, function(x) invlogit(x))

# subtract the estimated probabilities from each other
results <- results1 - results0

# means and cis

means <- apply(results,2,mean)
sdUpper <- apply(results,2,function(x) quantile(x, .975))
sdLower <- apply(results,2,function(x) quantile(x, .025))

means
sdUpper
sdLower

results.secdiff <- as.data.frame(results[,2]-results[,1])

means <- apply(results.secdiff,2,mean)
sdUpper <- apply(results.secdiff,2,function(x) quantile(x, .975))
sdLower <- apply(results.secdiff,2,function(x) quantile(x, .025))

means
sdUpper
sdLower

rm(list=setdiff(ls(), c("cityyear.data", 
                        "cityweek.data.persistence", 
                        "cityweek.data.diffusion", 
                        "ch9.cityyear.assoc", 
                        "ch9.cityweek.persistence.assoc", 
                        "ch9.cityweek.diffusion.assoc",
                        "ch9.cityyear.country")))

