# Simulating the effect of Internet penetration on protest
# MARGINAL VALUES

print(ch7.cityweek, digits=4)

set.seed(1337)

beta <- fixef(ch7.cityweek)
betas <- beta[,1]

covvar <- vcov(ch7.cityweek)
coefs.sim <- mvrnorm(10000, betas, covvar)

# Setting values for Internet penetration

summary(log(cityweek.data.persistence$numactivepc_l+1))
numactivepc <- 0.10781
numactivepc_mod <- 0.52957

# Setting values for peace duration

summary(log(cityweek.data.persistence$timesinceprotest))
timesinceprotest <- c(log(1),log(52))

#bind them together in a matrix   

effects0 <- cbind(1,
                  numactivepc,
                  timesinceprotest,
                  log(12),
                  0,
                  0,
                  1,0,
                  0,
                  numactivepc*timesinceprotest)

effects1 <- cbind(1,
                  numactivepc_mod,
                  timesinceprotest,
                  log(12),
                  0,
                  0,
                  1,0,
                  0,
                  numactivepc_mod*timesinceprotest)

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

