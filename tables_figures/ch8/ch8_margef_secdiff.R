# MARGINAL VALUES

print(ch8.cityweek.twoway, digits=4)

set.seed(1337)

beta <- fixef(ch8.cityweek.twoway)
betas <- beta[,1]

covvar <- vcov(ch8.cityweek.twoway)
coefs.sim <- mvrnorm(10000, betas, covvar)

# Setting values for Internet penetration

summary(log(cityweek.data.diffusion$numactivepc_l+1))
receiving_internet <- 0
receiving_internet_mod <- 0.41059

# Setting values for peace duration

#summary(log(cityweek.data.full$t_s_protests_slag+1))
#timesinceprotest_slag <- sort(unique(log(cityweek.data.full$t_s_protests_slag+1)))

#summary(log(cityweek.data.full$t_s_protests_tlag1+1))

#log(12)

# Setting values for sending Internet penetration

summary(cityweek.data.diffusion$lastprotestelsewhere_lnumactivepc_max)
sending_internet <- c(0, 2.6696)

#bind them together in a matrix   

effects0 <- cbind(1,
                  receiving_internet,
                  sending_internet,
                  log(2),
                  log(12),
                  0,
                  0,
                  1,0,
                  0,
                  receiving_internet*sending_internet)

effects1 <- cbind(1,
                  receiving_internet_mod,
                  sending_internet,
                  log(2),
                  log(12),
                  0,
                  0,
                  1,0,
                  0,
                  receiving_internet_mod*sending_internet)

#multiply the effects matrix with the matrix of simulated coefficients to obtain marginal effects
results0 <- coefs.sim %*% t(effects0)
results1 <- coefs.sim %*% t(effects1)

#evaluate each marginal effect using inverse logit, to obtain predicted probabilities
results0 <- apply(results0,1:2, function(x) invlogit(x))
results1 <- apply(results1,1:2, function(x) invlogit(x))

# subtract the estimated probabilities from each other
results <- results1 - results0

results

results.seconddiff <- as.data.frame(results[,2]-results[,1])

# means and cis

means <- apply(results,2,mean)
sdUpper <- apply(results,2,function(x) quantile(x, .975))
sdLower <- apply(results,2,function(x) quantile(x, .025))

means
sdUpper
sdLower

means <- apply(results.seconddiff,2,mean)
sdUpper <- apply(results.seconddiff,2,function(x) quantile(x, .975))
sdLower <- apply(results.seconddiff,2,function(x) quantile(x, .025))

means
sdUpper
sdLower

rm(list=setdiff(ls(), c("cityweek.data.diffusion", 
                        "ch8.cityweek.baseline", 
                        "ch8.cityweek.bare", 
                        "ch8.cityweek.bare.yearslope",
                        "ch8.cityweek.distance",
                        "ch8.cityweek.threeway", 
                        "ch8.cityweek.threeway.repr", 
                        "ch8.cityweek.twoway")))





















# MARGINAL VALUES

print(ch8.cityweek.twoway, digits=4)

set.seed(1337)

beta <- fixef(ch8.cityweek.twoway)
betas <- beta[,1]

covvar <- vcov(ch8.cityweek.twoway)
coefs.sim <- mvrnorm(10000, betas, covvar)

# Setting values for Internet penetration

summary(log(cityweek.data.diffusion$numactivepc_l+1))
quantile(log(cityweek.data.diffusion$numactivepc_l+1), .82)
receiving_internet <- c(0.03289,2)

# Setting values for peace duration

#summary(log(cityweek.data.full$t_s_protests_slag+1))
#timesinceprotest_slag <- sort(unique(log(cityweek.data.full$t_s_protests_slag+1)))

#summary(log(cityweek.data.full$t_s_protests_tlag1+1))

#log(12)

# Setting values for sending Internet penetration

summary(cityweek.data.diffusion$lastprotestelsewhere_lnumactivepc_max)
sending_internet <- 0
sending_internet_mod <- 1.5375

#bind them together in a matrix   

effects0 <- cbind(1,
                  receiving_internet,
                  sending_internet,
                  log(2),
                  log(12),
                  0,
                  0,
                  1,0,
                  0,
                  receiving_internet*sending_internet)

effects1 <- cbind(1,
                  receiving_internet,
                  sending_internet_mod,
                  log(2),
                  log(12),
                  0,
                  0,
                  1,0,
                  0,
                  receiving_internet*sending_internet_mod)

#multiply the effects matrix with the matrix of simulated coefficients to obtain marginal effects
results0 <- coefs.sim %*% t(effects0)
results1 <- coefs.sim %*% t(effects1)

#evaluate each marginal effect using inverse logit, to obtain predicted probabilities
results0 <- apply(results0,1:2, function(x) invlogit(x))
results1 <- apply(results1,1:2, function(x) invlogit(x))

# subtract the estimated probabilities from each other
results <- results1 - results0

results

results.seconddiff <- as.data.frame(results[,2]-results[,1])

# means and cis

means <- apply(results,2,mean)
sdUpper <- apply(results,2,function(x) quantile(x, .975))
sdLower <- apply(results,2,function(x) quantile(x, .025))

means
sdUpper
sdLower


means <- apply(results.seconddiff,2,mean)
sdUpper <- apply(results.seconddiff,2,function(x) quantile(x, .975))
sdLower <- apply(results.seconddiff,2,function(x) quantile(x, .025))

means
sdUpper
sdLower

rm(list=setdiff(ls(), c("cityweek.data.diffusion", 
                        "ch8.cityweek.baseline", 
                        "ch8.cityweek.bare", 
                        "ch8.cityweek.bare.yearslope",
                        "ch8.cityweek.distance",
                        "ch8.cityweek.threeway", 
                        "ch8.cityweek.threeway.repr", 
                        "ch8.cityweek.twoway")))
