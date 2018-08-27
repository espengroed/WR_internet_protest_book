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

# Setting values for sending Internet penetration

summary(cityweek.data.diffusion$lastprotestelsewhere_lnumactivepc_max)
sending_internet <- sort(unique(cityweek.data.diffusion$lastprotestelsewhere_lnumactivepc_max))
sending_internet <- sort(sample(sending_internet,200))
sending_internet

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

# means and cis

x <- sending_internet

means <- apply(results,2,mean)
sdUpper <- apply(results,2,function(x) quantile(x, .975))
sdLower <- apply(results,2,function(x) quantile(x, .025))

x
means
sdUpper
sdLower

x[1]
means[1]
sdUpper[1]
sdLower[1]

x[187]
means[187]
sdUpper[187]
sdLower[187]

#graph the results

dfplot <- data.frame(x, means, sdLower, sdUpper)
p <- ggplot(dfplot, aes(x=x, y=means, ymin=sdLower, ymax=sdUpper)) + 
  geom_line() +
  geom_ribbon(aes(ymin=sdLower,ymax=sdUpper),alpha=0.3) +
  theme_bw() + geom_hline(yintercept = 0, lty=2) + xlab('Internet penetration, sending (ln)') + ylab('Change in probability of protest diffusion as \n Internet penetration (receiving city) increases') +
  geom_rug(sides="b") +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=15,face="bold"))

p

pdf("ch8/cityweek_diffusion_bayes_twoway_receivingeffect.pdf", width=8, height=5)
par(mar=c(4.5,4.5,1,1))
p
dev.off()

rm(list=setdiff(ls(), c("cityweek.data.diffusion", 
                        "ch8.cityweek.bare", 
                        "ch8.cityweek.bare.yearslope",
                        "ch8.cityweek.distance",
                        "ch8.cityweek.threeway", 
                        "ch8.cityweek.threeway.repr", 
                        "ch8.cityweek.twoway")))





















# Simulating the effect of Internet penetration on protest
# MARGINAL VALUES

print(ch8.cityweek.twoway, digits=4)

set.seed(1337)

beta <- fixef(ch8.cityweek.twoway)
betas <- beta[,1]

covvar <- vcov(ch8.cityweek.twoway)
coefs.sim <- mvrnorm(10000, betas, covvar)

# Setting values for receiving Internet penetration

summary(log(cityweek.data.diffusion$numactivepc_l+1))
receiving_internet <- sort(unique(log(cityweek.data.diffusion$numactivepc_l+1)))
receiving_internet <- sort(sample(receiving_internet,200))
receiving_internet

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

# means and cis

x <- receiving_internet

means <- apply(results,2,mean)
sdUpper <- apply(results,2,function(x) quantile(x, .975))
sdLower <- apply(results,2,function(x) quantile(x, .025))

x[12]
means[12]
sdUpper[12]
sdLower[12]

x[172]
means[172]
sdUpper[172]
sdLower[172]

#graph the results

dfplot <- data.frame(x, means, sdLower, sdUpper)
p <- ggplot(dfplot, aes(x=x, y=means, ymin=sdLower, ymax=sdUpper)) + 
  geom_line() +
  geom_ribbon(aes(ymin=sdLower,ymax=sdUpper),alpha=0.3) +
  theme_bw() + geom_hline(yintercept = 0, lty=2) + xlab('Internet penetration, receiving (ln)') + ylab('Change in probability of protest diffusion as \n Internet penetration (sending city) increases') +
  geom_rug(sides="b") +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=15,face="bold"))

p

pdf("ch8/cityweek_diffusion_bayes_twoway_sendingeffect.pdf", width=8, height=5)
par(mar=c(4.5,4.5,1,1))
p
dev.off()

rm(list=setdiff(ls(), c("cityweek.data.diffusion", 
                        "ch8.cityweek.bare", 
                        "ch8.cityweek.bare.yearslope",
                        "ch8.cityweek.distance",
                        "ch8.cityweek.threeway", 
                        "ch8.cityweek.threeway.repr", 
                        "ch8.cityweek.twoway")))