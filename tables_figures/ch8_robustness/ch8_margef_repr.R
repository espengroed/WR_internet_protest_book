# Simulating the effect of Internet penetration on protest
# MARGINAL VALUES

print(ch8.cityweek.threeway.repr, digits=4)

set.seed(1337)

beta <- fixef(ch8.cityweek.threeway.repr)
betas <- beta[,1]

covvar <- vcov(ch8.cityweek.threeway.repr)
coefs.sim <- mvrnorm(10000, betas, covvar)

# Setting values for Internet penetration

summary(log(cityweek.data.diffusion$numactivepc_l+1))
receiving_internet <- median(log(cityweek.data.diffusion$numactivepc_l+1))

# Setting values for peace duration

summary(log(cityweek.data.diffusion$timesinceprotestelsewhere))
timesinceprotest_slag <- sort(unique(log(cityweek.data.diffusion$timesinceprotestelsewhere)))
timesinceprotest_slag <- timesinceprotest_slag[1:52]

# Setting values for sending Internet penetration

summary(cityweek.data.diffusion$lastprotestelsewhere_lnumactivepc_max)
sending_internet <- 0
sending_internet_mod <- 1.5375

# Setting values for repression

repr <- 0

#bind them together in a matrix   

effects0 <- cbind(1,
                  receiving_internet,
                  sending_internet,
                  timesinceprotest_slag,
                  repr,
                  log(12),
                  0,
                  0,
                  1,0,
                  0,
                  1,
                  sending_internet*timesinceprotest_slag,
                  sending_internet*repr,
                  timesinceprotest_slag*repr,
                  sending_internet*timesinceprotest_slag*repr)

effects1 <- cbind(1,
                  receiving_internet,
                  sending_internet_mod,
                  timesinceprotest_slag,
                  repr,
                  log(12),
                  0,
                  0,
                  1,0,
                  0,
                  1,
                  sending_internet_mod*timesinceprotest_slag,
                  sending_internet_mod*repr,
                  timesinceprotest_slag*repr,
                  sending_internet_mod*timesinceprotest_slag*repr)

#multiply the effects matrix with the matrix of simulated coefficients to obtain marginal effects
results0 <- coefs.sim %*% t(effects0)
results1 <- coefs.sim %*% t(effects1)

#evaluate each marginal effect using inverse logit, to obtain predicted probabilities
results0 <- apply(results0,1:2, function(x) invlogit(x))
results1 <- apply(results1,1:2, function(x) invlogit(x))

# subtract the estimated probabilities from each other
results <- results1 - results0

# means and cis

x <- exp(timesinceprotest_slag)

means <- apply(results,2,mean)
sdUpper <- apply(results,2,function(x) quantile(x, .975))
sdLower <- apply(results,2,function(x) quantile(x, .025))

#graph the results

dfplot <- data.frame(x, means, sdLower, sdUpper)
p <- ggplot(dfplot, aes(x=x, y=means, ymin=sdLower, ymax=sdUpper)) + 
  geom_line() +
  geom_ribbon(aes(ymin=sdLower,ymax=sdUpper),alpha=0.3) +
  theme_bw() + geom_hline(yintercept = 0, lty=2) + xlab('Weeks since protest in different city') + ylab('Change in probability of protest diffusion as \n Internet penetration (sending city) increases') +
  geom_rug(sides="b") + coord_cartesian(ylim = c(-0.005,.007)) +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=15,face="bold"))

p

pdf("ch8_robustness/cityweek_diffusion_bayes_sendingeffect_time_repr0_robustness.pdf")
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

print(ch8.cityweek.threeway.repr, digits=4)

set.seed(1337)

beta <- fixef(ch8.cityweek.threeway.repr)
betas <- beta[,1]

covvar <- vcov(ch8.cityweek.threeway.repr)
coefs.sim <- mvrnorm(10000, betas, covvar)

# Setting values for Internet penetration

summary(log(cityweek.data.diffusion$numactivepc_l+1))
receiving_internet <- median(log(cityweek.data.diffusion$numactivepc_l+1))

# Setting values for peace duration

summary(log(cityweek.data.diffusion$timesinceprotestelsewhere))
timesinceprotest_slag <- sort(unique(log(cityweek.data.diffusion$timesinceprotestelsewhere)))
timesinceprotest_slag <- timesinceprotest_slag[1:52]

# Setting values for sending Internet penetration

summary(cityweek.data.diffusion$lastprotestelsewhere_lnumactivepc_max)
sending_internet <- 0
sending_internet_mod <- 1.5375

# Setting values for repression

repr <- 1

#bind them together in a matrix   

effects0 <- cbind(1,
                  receiving_internet,
                  sending_internet,
                  timesinceprotest_slag,
                  repr,
                  log(12),
                  0,
                  0,
                  1,0,
                  0,
                  1,
                  sending_internet*timesinceprotest_slag,
                  sending_internet*repr,
                  timesinceprotest_slag*repr,
                  sending_internet*timesinceprotest_slag*repr)

effects1 <- cbind(1,
                  receiving_internet,
                  sending_internet_mod,
                  timesinceprotest_slag,
                  repr,
                  log(12),
                  0,
                  0,
                  1,0,
                  0,
                  1,
                  sending_internet_mod*timesinceprotest_slag,
                  sending_internet_mod*repr,
                  timesinceprotest_slag*repr,
                  sending_internet_mod*timesinceprotest_slag*repr)

#multiply the effects matrix with the matrix of simulated coefficients to obtain marginal effects
results0 <- coefs.sim %*% t(effects0)
results1 <- coefs.sim %*% t(effects1)

#evaluate each marginal effect using inverse logit, to obtain predicted probabilities
results0 <- apply(results0,1:2, function(x) invlogit(x))
results1 <- apply(results1,1:2, function(x) invlogit(x))

# subtract the estimated probabilities from each other
results <- results1 - results0

# means and cis

x <- exp(timesinceprotest_slag)

means <- apply(results,2,mean)
sdUpper <- apply(results,2,function(x) quantile(x, .975))
sdLower <- apply(results,2,function(x) quantile(x, .025))

#graph the results

dfplot <- data.frame(x, means, sdLower, sdUpper)
p <- ggplot(dfplot, aes(x=x, y=means, ymin=sdLower, ymax=sdUpper)) + 
  geom_line() +
  geom_ribbon(aes(ymin=sdLower,ymax=sdUpper),alpha=0.3) +
  theme_bw() + geom_hline(yintercept = 0, lty=2) + xlab('Weeks since protest in different city') + ylab('Change in probability of protest diffusion as \n Internet penetration (sending city) increases') +
  geom_rug(sides="b") + coord_cartesian(ylim = c(-0.005,.007)) +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=15,face="bold"))


p

pdf("ch8_robustness/cityweek_diffusion_bayes_sendingeffect_time_repr1_robustness.pdf")
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
