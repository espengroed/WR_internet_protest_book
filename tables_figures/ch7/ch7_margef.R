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
timesinceprotest <- sort(unique(log(cityweek.data.persistence$timesinceprotest)))

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

# means and cis

x <- exp(timesinceprotest)

means <- apply(results,2,mean)
sdUpper <- apply(results,2,function(x) quantile(x, .975))
sdLower <- apply(results,2,function(x) quantile(x, .025))

means[1]
sdUpper[1]
sdLower[1]

means[52]
sdUpper[52]
sdLower[52]

#graph the results

dfplot <- data.frame(x, means, sdLower, sdUpper)
p <- ggplot(dfplot, aes(x=x, y=means, ymin=sdLower, ymax=sdUpper)) + 
  geom_line() +
  geom_ribbon(aes(ymin=sdLower,ymax=sdUpper),alpha=0.3) +
  theme_bw() + geom_hline(yintercept = 0, lty=2) + xlab('Weeks since protest in same city') + ylab('Change in probability of protest persistence \n as Internet penetration increases') +
  geom_rug(sides="b") +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=15,face="bold"))

p

x <- x[1:52]
means <- means[1:52]
sdLower <- sdLower[1:52]
sdUpper <- sdUpper[1:52]

dfplot <- data.frame(x, means, sdLower, sdUpper)
p <- ggplot(dfplot, aes(x=x, y=means, ymin=sdLower, ymax=sdUpper)) + 
  geom_line() +
  geom_ribbon(aes(ymin=sdLower,ymax=sdUpper),alpha=0.3) +
  theme_bw() + geom_hline(yintercept = 0, lty=2) + xlab('Weeks since protest in same city') + ylab('Change in probability of protest persistence \n as Internet penetration increases') +
  geom_rug(sides="b") +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=15,face="bold"))

p

pdf("ch7/cityweek_persistence_first52_bayes.pdf", width=8, height=5)
par(mar=c(4.5,4.5,1,1))
p
dev.off()

rm(list=setdiff(ls(), c("cityweek.data.persistence", 
                        "ch7.cityweek",
                        "ch7.cityweek.bare", 
                        "ch7.cityweek.bare.yearslope", 
                        "ch7.cityweek.bare.report", 
                        "ch7.cityweek.report", 
                        "ch7.cityweek.repr", 
                        "ch7.cityweek.repr.report")))








































# NO REPRESSION PLOT

print(ch7.cityweek.repr, digits=4)

set.seed(1337)

beta <- fixef(ch7.cityweek.repr)
betas <- beta[,1]

covvar <- vcov(ch7.cityweek.repr)
coefs.sim <- mvrnorm(10000, betas, covvar)

# Setting values for Internet penetration

summary(log(cityweek.data.persistence$numactivepc_l+1))
numactivepc <- 0.10781
numactivepc_mod <- 0.52957

# Setting values for peace duration

summary(log(cityweek.data.persistence$timesinceprotest))
timesinceprotest <- sort(unique(log(cityweek.data.persistence$timesinceprotest)))

# Setting repression

repr <- 0

#bind them together in a matrix   

effects0 <- cbind(1,
                  numactivepc,
                  timesinceprotest,
                  repr,
                  log(12),
                  0,
                  0,
                  1,0,
                  0,
                  numactivepc*timesinceprotest,
                  numactivepc*repr,
                  timesinceprotest*repr,
                  numactivepc*timesinceprotest*repr)

effects1 <- cbind(1,
                  numactivepc_mod,
                  timesinceprotest,
                  repr,
                  log(12),
                  0,
                  0,
                  1,0,
                  0,
                  numactivepc_mod*timesinceprotest,
                  numactivepc_mod*repr,
                  timesinceprotest*repr,
                  numactivepc_mod*timesinceprotest*repr)

#multiply the effects matrix with the matrix of simulated coefficients to obtain marginal effects
results0 <- coefs.sim %*% t(effects0)
results1 <- coefs.sim %*% t(effects1)

#evaluate each marginal effect using inverse logit, to obtain predicted probabilities
results0 <- apply(results0,1:2, function(x) invlogit(x))
results1 <- apply(results1,1:2, function(x) invlogit(x))

# subtract the estimated probabilities from each other
results <- results1 - results0

# means and cis

x <- exp(timesinceprotest)

means <- apply(results,2,mean)
sdUpper <- apply(results,2,function(x) quantile(x, .975))
sdLower <- apply(results,2,function(x) quantile(x, .025))

#graph the results

dfplot <- data.frame(x, means, sdLower, sdUpper)
p <- ggplot(dfplot, aes(x=x, y=means, ymin=sdLower, ymax=sdUpper)) + 
  geom_line() +
  geom_ribbon(aes(ymin=sdLower,ymax=sdUpper),alpha=0.3) +
  theme_bw() + geom_hline(yintercept = 0, lty=2) + xlab('Weeks since protest in same city') + ylab('Change in probability of protest persistence \n as Internet penetration increases') +
  geom_rug(sides="b") + coord_cartesian(ylim = c(-0.001,.005)) +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=15,face="bold"))

p

#pdf("ch7/cityweek_persistence_NOrepr_bayes.pdf")
#par(mar=c(4.5,4.5,1,1))
#p
#dev.off()

x <- x[1:52]
means <- means[1:52]
sdLower <- sdLower[1:52]
sdUpper <- sdUpper[1:52]

dfplot <- data.frame(x, means, sdLower, sdUpper)
p <- ggplot(dfplot, aes(x=x, y=means, ymin=sdLower, ymax=sdUpper)) + 
  geom_line() +
  geom_ribbon(aes(ymin=sdLower,ymax=sdUpper),alpha=0.3) +
  theme_bw() + geom_hline(yintercept = 0, lty=2) + xlab('Weeks since protest in same city') + ylab('Change in probability of protest persistence \n as Internet penetration increases') +
  geom_rug(sides="b") + coord_cartesian(ylim = c(-0.001,.005)) +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=15,face="bold"))

p

pdf("ch7/cityweek_persistence_NOrepr_first52_bayes.pdf")
par(mar=c(4.5,4.5,1,1))
p
dev.off()

rm(list=setdiff(ls(), c("cityweek.data.persistence", 
                        "ch7.cityweek",
                        "ch7.cityweek.bare", 
                        "ch7.cityweek.bare.yearslope", 
                        "ch7.cityweek.bare.report", 
                        "ch7.cityweek.report", 
                        "ch7.cityweek.repr", 
                        "ch7.cityweek.repr.report")))


























# REPRESSION PLOT

print(ch7.cityweek.repr, digits=4)

set.seed(1337)

beta <- fixef(ch7.cityweek.repr)
betas <- beta[,1]

covvar <- vcov(ch7.cityweek.repr)
coefs.sim <- mvrnorm(10000, betas, covvar)

# Setting values for Internet penetration

summary(log(cityweek.data.persistence$numactivepc_l+1))
numactivepc <- 0.10781
numactivepc_mod <- 0.52957

# Setting values for peace duration

summary(log(cityweek.data.persistence$timesinceprotest))
timesinceprotest <- sort(unique(log(cityweek.data.persistence$timesinceprotest)))

# Setting repression

repr <- 1

#bind them together in a matrix   

effects0 <- cbind(1,
                  numactivepc,
                  timesinceprotest,
                  repr,
                  log(12),
                  0,
                  0,
                  1,0,
                  0,
                  numactivepc*timesinceprotest,
                  numactivepc*repr,
                  timesinceprotest*repr,
                  numactivepc*timesinceprotest*repr)

effects1 <- cbind(1,
                  numactivepc_mod,
                  timesinceprotest,
                  repr,
                  log(12),
                  0,
                  0,
                  1,0,
                  0,
                  numactivepc_mod*timesinceprotest,
                  numactivepc_mod*repr,
                  timesinceprotest*repr,
                  numactivepc_mod*timesinceprotest*repr)

#multiply the effects matrix with the matrix of simulated coefficients to obtain marginal effects
results0 <- coefs.sim %*% t(effects0)
results1 <- coefs.sim %*% t(effects1)

#evaluate each marginal effect using inverse logit, to obtain predicted probabilities
results0 <- apply(results0,1:2, function(x) invlogit(x))
results1 <- apply(results1,1:2, function(x) invlogit(x))

# subtract the estimated probabilities from each other
results <- results1 - results0

# means and cis

x <- exp(timesinceprotest)

means <- apply(results,2,mean)
sdUpper <- apply(results,2,function(x) quantile(x, .975))
sdLower <- apply(results,2,function(x) quantile(x, .025))

#graph the results

dfplot <- data.frame(x, means, sdLower, sdUpper)
p <- ggplot(dfplot, aes(x=x, y=means, ymin=sdLower, ymax=sdUpper)) + 
  geom_line() +
  geom_ribbon(aes(ymin=sdLower,ymax=sdUpper),alpha=0.3) +
  theme_bw() + geom_hline(yintercept = 0, lty=2) + xlab('Weeks since protest in same city') + ylab('Change in probability of protest persistence \n as Internet penetration increases') +
  geom_rug(sides="b")  + coord_cartesian(ylim = c(-0.001,.005)) +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=15,face="bold"))

p

#pdf("ch7/cityweek_persistence_YESrepr_bayes.pdf")
#par(mar=c(4.5,4.5,1,1))
#p
#dev.off()

x <- x[1:52]
means <- means[1:52]
sdLower <- sdLower[1:52]
sdUpper <- sdUpper[1:52]

dfplot <- data.frame(x, means, sdLower, sdUpper)
p <- ggplot(dfplot, aes(x=x, y=means, ymin=sdLower, ymax=sdUpper)) + 
  geom_line() +
  geom_ribbon(aes(ymin=sdLower,ymax=sdUpper),alpha=0.3) +
  theme_bw() + geom_hline(yintercept = 0, lty=2) + xlab('Weeks since protest in same city') + ylab('Change in probability of protest persistence \n as Internet penetration increases') +
  geom_rug(sides="b") + coord_cartesian(ylim = c(-0.001,.005)) +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=15,face="bold"))

p

pdf("ch7/cityweek_persistence_YESrepr_first52_bayes.pdf")
par(mar=c(4.5,4.5,1,1))
p
dev.off()

rm(list=setdiff(ls(), c("cityweek.data.persistence", 
                        "ch7.cityweek",
                        "ch7.cityweek.bare", 
                        "ch7.cityweek.bare.yearslope", 
                        "ch7.cityweek.bare.report", 
                        "ch7.cityweek.report", 
                        "ch7.cityweek.repr", 
                        "ch7.cityweek.repr.report")))

























# Simulating the effect of Internet penetration on protest
# MARGINAL VALUES

print(ch7.cityweek.report, digits=4)

set.seed(1337)

beta <- fixef(ch7.cityweek.report)
betas <- beta[,1]

covvar <- vcov(ch7.cityweek.report)
coefs.sim <- mvrnorm(10000, betas, covvar)

# Setting values for Internet penetration

summary(log(cityweek.data.persistence$numactivepc_l+1))
numactivepc <- 0.10781
numactivepc_mod <- 0.52957

# Setting values for peace duration

summary(log(cityweek.data.persistence$timesinceprotest))
timesinceprotest <- sort(unique(log(cityweek.data.persistence$timesinceprotest)))

#bind them together in a matrix   

effects0 <- cbind(1,
                  numactivepc,
                  timesinceprotest,
                  1,
                  log(12),
                  0,
                  0,
                  1,0,
                  0,
                  numactivepc*timesinceprotest)

effects1 <- cbind(1,
                  numactivepc_mod,
                  timesinceprotest,
                  1,
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

# means and cis

x <- exp(timesinceprotest)

means <- apply(results,2,mean)
sdUpper <- apply(results,2,function(x) quantile(x, .975))
sdLower <- apply(results,2,function(x) quantile(x, .025))

means[1]
sdUpper[1]
sdLower[1]

means[52]
sdUpper[52]
sdLower[52]

#graph the results

x <- x[1:52]
means <- means[1:52]
sdLower <- sdLower[1:52]
sdUpper <- sdUpper[1:52]

dfplot <- data.frame(x, means, sdLower, sdUpper)
p <- ggplot(dfplot, aes(x=x, y=means, ymin=sdLower, ymax=sdUpper)) + 
  geom_line() +
  geom_ribbon(aes(ymin=sdLower,ymax=sdUpper),alpha=0.3) +
  theme_bw() + geom_hline(yintercept = 0, lty=2) + xlab('Weeks since protest in same city') + ylab('Change in probability of protest persistence \n as Internet penetration increases') +
  geom_rug(sides="b") +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=15,face="bold"))

p

pdf("ch7/cityweek_persistence_first52_bayes_report.pdf", width=8, height=5)
par(mar=c(4.5,4.5,1,1))
p
dev.off()

rm(list=setdiff(ls(), c("cityweek.data.persistence", 
                        "ch7.cityweek",
                        "ch7.cityweek.bare", 
                        "ch7.cityweek.bare.yearslope", 
                        "ch7.cityweek.bare.report", 
                        "ch7.cityweek.report", 
                        "ch7.cityweek.repr", 
                        "ch7.cityweek.repr.report")))


























# NO REPRESSION PLOT

print(ch7.cityweek.repr.report, digits=4)

set.seed(1337)

beta <- fixef(ch7.cityweek.repr.report)
betas <- beta[,1]

covvar <- vcov(ch7.cityweek.repr.report)
coefs.sim <- mvrnorm(10000, betas, covvar)

# Setting values for Internet penetration

summary(log(cityweek.data.persistence$numactivepc_l+1))
numactivepc <- 0.10781
numactivepc_mod <- 0.52957

# Setting values for peace duration

summary(log(cityweek.data.persistence$timesinceprotest))
timesinceprotest <- sort(unique(log(cityweek.data.persistence$timesinceprotest)))

# Setting repression

repr <- 0

#bind them together in a matrix   

effects0 <- cbind(1,
                  numactivepc,
                  timesinceprotest,
                  repr,
                  1,
                  log(12),
                  0,
                  0,
                  1,0,
                  0,
                  numactivepc*timesinceprotest,
                  numactivepc*repr,
                  timesinceprotest*repr,
                  numactivepc*timesinceprotest*repr)

effects1 <- cbind(1,
                  numactivepc_mod,
                  timesinceprotest,
                  repr,
                  1,
                  log(12),
                  0,
                  0,
                  1,0,
                  0,
                  numactivepc_mod*timesinceprotest,
                  numactivepc_mod*repr,
                  timesinceprotest*repr,
                  numactivepc_mod*timesinceprotest*repr)

#multiply the effects matrix with the matrix of simulated coefficients to obtain marginal effects
results0 <- coefs.sim %*% t(effects0)
results1 <- coefs.sim %*% t(effects1)

#evaluate each marginal effect using inverse logit, to obtain predicted probabilities
results0 <- apply(results0,1:2, function(x) invlogit(x))
results1 <- apply(results1,1:2, function(x) invlogit(x))

# subtract the estimated probabilities from each other
results <- results1 - results0

# means and cis

x <- exp(timesinceprotest)

means <- apply(results,2,mean)
sdUpper <- apply(results,2,function(x) quantile(x, .975))
sdLower <- apply(results,2,function(x) quantile(x, .025))

#graph the results

x <- x[1:52]
means <- means[1:52]
sdLower <- sdLower[1:52]
sdUpper <- sdUpper[1:52]

dfplot <- data.frame(x, means, sdLower, sdUpper)
p <- ggplot(dfplot, aes(x=x, y=means, ymin=sdLower, ymax=sdUpper)) + 
  geom_line() +
  geom_ribbon(aes(ymin=sdLower,ymax=sdUpper),alpha=0.3) +
  theme_bw() + geom_hline(yintercept = 0, lty=2) + xlab('Weeks since protest in same city') + ylab('Change in probability of protest persistence \n as Internet penetration increases') +
  geom_rug(sides="b") + coord_cartesian(ylim = c(-0.002,.007)) +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=15,face="bold"))

p

pdf("ch7/cityweek_persistence_NOrepr_first52_bayes_report.pdf")
par(mar=c(4.5,4.5,1,1))
p
dev.off()

rm(list=setdiff(ls(), c("cityweek.data.persistence", 
                        "ch7.cityweek",
                        "ch7.cityweek.bare", 
                        "ch7.cityweek.bare.yearslope", 
                        "ch7.cityweek.bare.report", 
                        "ch7.cityweek.report", 
                        "ch7.cityweek.repr", 
                        "ch7.cityweek.repr.report")))


























# REPRESSION PLOT

print(ch7.cityweek.repr.report, digits=4)

set.seed(1337)

beta <- fixef(ch7.cityweek.repr.report)
betas <- beta[,1]

covvar <- vcov(ch7.cityweek.repr.report)
coefs.sim <- mvrnorm(10000, betas, covvar)

# Setting values for Internet penetration

summary(log(cityweek.data.persistence$numactivepc_l+1))
numactivepc <- 0.10781
numactivepc_mod <- 0.52957

# Setting values for peace duration

summary(log(cityweek.data.persistence$timesinceprotest))
timesinceprotest <- sort(unique(log(cityweek.data.persistence$timesinceprotest)))

# Setting repression

repr <- 1

#bind them together in a matrix   

effects0 <- cbind(1,
                  numactivepc,
                  timesinceprotest,
                  repr,
                  1,
                  log(12),
                  0,
                  0,
                  1,0,
                  0,
                  numactivepc*timesinceprotest,
                  numactivepc*repr,
                  timesinceprotest*repr,
                  numactivepc*timesinceprotest*repr)

effects1 <- cbind(1,
                  numactivepc_mod,
                  timesinceprotest,
                  repr,
                  1,
                  log(12),
                  0,
                  0,
                  1,0,
                  0,
                  numactivepc_mod*timesinceprotest,
                  numactivepc_mod*repr,
                  timesinceprotest*repr,
                  numactivepc_mod*timesinceprotest*repr)

#multiply the effects matrix with the matrix of simulated coefficients to obtain marginal effects
results0 <- coefs.sim %*% t(effects0)
results1 <- coefs.sim %*% t(effects1)

#evaluate each marginal effect using inverse logit, to obtain predicted probabilities
results0 <- apply(results0,1:2, function(x) invlogit(x))
results1 <- apply(results1,1:2, function(x) invlogit(x))

# subtract the estimated probabilities from each other
results <- results1 - results0

# means and cis

x <- exp(timesinceprotest)

means <- apply(results,2,mean)
sdUpper <- apply(results,2,function(x) quantile(x, .975))
sdLower <- apply(results,2,function(x) quantile(x, .025))

#graph the results

x <- x[1:52]
means <- means[1:52]
sdLower <- sdLower[1:52]
sdUpper <- sdUpper[1:52]

dfplot <- data.frame(x, means, sdLower, sdUpper)
p <- ggplot(dfplot, aes(x=x, y=means, ymin=sdLower, ymax=sdUpper)) + 
  geom_line() +
  geom_ribbon(aes(ymin=sdLower,ymax=sdUpper),alpha=0.3) +
  theme_bw() + geom_hline(yintercept = 0, lty=2) + xlab('Weeks since protest in same city') + ylab('Change in probability of protest persistence \n as Internet penetration increases') +
  geom_rug(sides="b") + coord_cartesian(ylim = c(-0.002,.007)) +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=15,face="bold"))

p

pdf("ch7/cityweek_persistence_YESrepr_first52_bayes_report.pdf")
par(mar=c(4.5,4.5,1,1))
p
dev.off()

rm(list=setdiff(ls(), c("cityweek.data.persistence", 
                        "ch7.cityweek",
                        "ch7.cityweek.bare", 
                        "ch7.cityweek.bare.yearslope", 
                        "ch7.cityweek.bare.report", 
                        "ch7.cityweek.report", 
                        "ch7.cityweek.repr", 
                        "ch7.cityweek.repr.report")))