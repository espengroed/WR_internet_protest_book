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
v2x_frassoc_thick_l <- sort(unique(cityyear.data$v2x_frassoc_thick_l))

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

# means and cis

x <- v2x_frassoc_thick_l
means <- apply(results,2,mean)
sdUpper <- apply(results,2,function(x) quantile(x, .975))
sdLower <- apply(results,2,function(x) quantile(x, .025))

summary(cityyear.data$v2x_frassoc_thick_l)

x[1]
means[1]

x[150]
means[150]

#graph the results

dfplot <- data.frame(x, means, sdLower, sdUpper)
p <- ggplot(dfplot, aes(x=x, y=means, ymin=sdLower, ymax=sdUpper)) + 
  geom_line() +
  geom_ribbon(aes(ymin=sdLower,ymax=sdUpper),alpha=0.3) +
  theme_bw() + geom_hline(yintercept = 0, lty=2) + xlab('Freedom of association') + ylab('Change in probability of protest occurrence \n as Internet penetration increases') +
  geom_rug(sides="b") + 
  theme(axis.text=element_text(size=14), axis.title=element_text(size=15,face="bold"))

p

pdf("ch9/cityyear_incidence_bayes_assoc.pdf", width=8, height=5)
par(mar=c(4.5,4.5,1,1))
p
dev.off()

rm(list=setdiff(ls(), c("cityyear.data", 
                        "cityweek.data.persistence", 
                        "cityweek.data.diffusion", 
                        "ch9.cityyear.assoc", 
                        "ch9.cityweek.persistence.assoc", 
                        "ch9.cityweek.diffusion.assoc")))


























print(ch9.cityyear.assoc, digits=4)

set.seed(1337)

beta <- fixef(ch9.cityyear.assoc)
betas <- beta[,1]

covvar <- vcov(ch9.cityyear.assoc)
coefs.sim <- mvrnorm(10000, betas, covvar)

# Setting values for Internet penetration

summary(log(cityyear.data$numactivepc_l+1))
numactivepc <- sort(unique(log(cityyear.data$numactivepc_l+1)))
numactivepc <- sort(sample(numactivepc, 200))

# Setting values for peace duration

summary(cityyear.data$v2x_frassoc_thick_l)
v2x_frassoc_thick_l <- 0.08454
v2x_frassoc_thick_l_mod <- 0.40272

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

# means and cis

x <- sort(numactivepc)
means <- apply(results,2,mean)
sdUpper <- apply(results,2,function(x) quantile(x, .975))
sdLower <- apply(results,2,function(x) quantile(x, .025))

summary(log(cityyear.data$numactivepc_l+1))

x[17]
means[17]

x[150]
means[150]

#graph the results

dfplot <- data.frame(x, means, sdLower, sdUpper)
p <- ggplot(dfplot, aes(x=x, y=means, ymin=sdLower, ymax=sdUpper)) + 
  geom_line() +
  geom_ribbon(aes(ymin=sdLower,ymax=sdUpper),alpha=0.3) +
  theme_bw() + geom_hline(yintercept = 0, lty=2) + xlab('Internet penetration') + ylab('Change in probability of protest occurrence \n as freedom of association increases') +
  geom_rug(sides="b") +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=15,face="bold"))

p

pdf("ch9/cityyear_incidence_bayes_assoc_symm.pdf", width=8, height=5)
par(mar=c(4.5,4.5,1,1))
p
dev.off()

rm(list=setdiff(ls(), c("cityyear.data", 
                        "cityweek.data.persistence", 
                        "cityweek.data.diffusion", 
                        "ch9.cityyear.assoc", 
                        "ch9.cityweek.persistence.assoc", 
                        "ch9.cityweek.diffusion.assoc")))























print(ch9.cityweek.persistence.assoc, digits=4)

set.seed(1337)

beta <- fixef(ch9.cityweek.persistence.assoc)
betas <- beta[,1]

covvar <- vcov(ch9.cityweek.persistence.assoc)
coefs.sim <- mvrnorm(10000, betas, covvar)

# Setting values for Internet penetration

summary(log(cityweek.data.persistence$numactivepc_l+1))
numactivepc <- 0
numactivepc_mod <- 0.10781

# Setting values for association

summary(cityweek.data.persistence$v2x_frassoc_thick_l)
v2x_frassoc_thick_l <- sort(unique(cityweek.data.persistence$v2x_frassoc_thick_l))

#bind them together in a matrix   

effects0 <- cbind(1,
                  numactivepc,
                  v2x_frassoc_thick_l,
                  log(12),
                  log(12),
                  0,
                  0,
                  1,0,
                  0,
                  numactivepc*v2x_frassoc_thick_l)

effects1 <- cbind(1,
                  numactivepc_mod,
                  v2x_frassoc_thick_l,
                  log(12),
                  log(12),
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

# means and cis

x <- v2x_frassoc_thick_l
means <- apply(results,2,mean)
sdUpper <- apply(results,2,function(x) quantile(x, .975))
sdLower <- apply(results,2,function(x) quantile(x, .025))

#graph the results

dfplot <- data.frame(x, means, sdLower, sdUpper)
p <- ggplot(dfplot, aes(x=x, y=means, ymin=sdLower, ymax=sdUpper)) + 
  geom_line() +
  geom_ribbon(aes(ymin=sdLower,ymax=sdUpper),alpha=0.3) +
  theme_bw() + geom_hline(yintercept = 0, lty=2) + xlab('Freedom of association') + ylab('Change in probability of protest persistence \n as Internet penetration increases') +
  geom_rug(sides="b") +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=15,face="bold"))

p

pdf("ch9/cityweek_persistence_bayes_assoc.pdf", width=8, height=5)
par(mar=c(4.5,4.5,1,1))
p
dev.off()

rm(list=setdiff(ls(), c("cityyear.data", 
                        "cityweek.data.persistence", 
                        "cityweek.data.diffusion", 
                        "ch9.cityyear.assoc", 
                        "ch9.cityweek.persistence.assoc", 
                        "ch9.cityweek.diffusion.assoc")))



















print(ch9.cityweek.persistence.assoc, digits=4)

set.seed(1337)

beta <- fixef(ch9.cityweek.persistence.assoc)
betas <- beta[,1]

covvar <- vcov(ch9.cityweek.persistence.assoc)
coefs.sim <- mvrnorm(10000, betas, covvar)

# Setting values for Internet penetration

summary(log(cityweek.data.persistence$numactivepc_l+1))
numactivepc <- sort(unique(log(cityweek.data.persistence$numactivepc_l+1)))
numactivepc <- sample(numactivepc, 200)

# Setting values for peace duration

summary(cityweek.data.persistence$v2x_frassoc_thick_l)
v2x_frassoc_thick_l <- 0.0881
v2x_frassoc_thick_l_mod <- 0.5571

#bind them together in a matrix   

effects0 <- cbind(1,
                  numactivepc,
                  v2x_frassoc_thick_l,
                  log(12),
                  log(12),
                  0,
                  0,
                  1,0,
                  0,
                  numactivepc*v2x_frassoc_thick_l)

effects1 <- cbind(1,
                  numactivepc,
                  v2x_frassoc_thick_l_mod,
                  log(12),
                  log(12),
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

# means and cis

x <- numactivepc
means <- apply(results,2,mean)
sdUpper <- apply(results,2,function(x) quantile(x, .975))
sdLower <- apply(results,2,function(x) quantile(x, .025))

#graph the results

dfplot <- data.frame(x, means, sdLower, sdUpper)
p <- ggplot(dfplot, aes(x=x, y=means, ymin=sdLower, ymax=sdUpper)) + 
  geom_line() +
  geom_ribbon(aes(ymin=sdLower,ymax=sdUpper),alpha=0.3) +
  theme_bw() + geom_hline(yintercept = 0, lty=2) + xlab('Internet penetration') + ylab('Change in probability of protest persistence \n as freedom of association increases') +
  geom_rug(sides="b") +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=15,face="bold"))

p

pdf("ch9/cityweek_persistence_bayes_assoc_symm.pdf", width=8, height=5)
par(mar=c(4.5,4.5,1,1))
p
dev.off()

rm(list=setdiff(ls(), c("cityyear.data", 
                        "cityweek.data.persistence", 
                        "cityweek.data.diffusion", 
                        "ch9.cityyear.assoc", 
                        "ch9.cityweek.persistence.assoc", 
                        "ch9.cityweek.diffusion.assoc")))
























print(ch9.cityweek.diffusion.assoc, digits=4)

set.seed(1337)

beta <- fixef(ch9.cityweek.diffusion.assoc)
betas <- beta[,1]

covvar <- vcov(ch9.cityweek.diffusion.assoc)
coefs.sim <- mvrnorm(10000, betas, covvar)

# Setting values for receiving Internet penetration

summary(log(cityweek.data.diffusion$numactivepc_l+1))
receiving_internet <- 0.41059

# Setting values for sendign Internet penetration

summary(cityweek.data.diffusion$lastprotestelsewhere_lnumactivepc_max)
sending_internet <- 0
sending_internet_mod <- 1.5375

# Setting values for association

summary(cityweek.data.diffusion$v2x_frassoc_thick_l)
v2x_frassoc_thick_l <- sort(unique(cityweek.data.diffusion$v2x_frassoc_thick_l))

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
                  sending_internet_mod,
                  v2x_frassoc_thick_l,
                  log(2),
                  log(12),
                  0,
                  0,
                  1,0,
                  0,
                  sending_internet_mod*v2x_frassoc_thick_l)


#multiply the effects matrix with the matrix of simulated coefficients to obtain marginal effects
results0 <- coefs.sim %*% t(effects0)
results1 <- coefs.sim %*% t(effects1)

#evaluate each marginal effect using inverse logit, to obtain predicted probabilities
results0 <- apply(results0,1:2, function(x) invlogit(x))
results1 <- apply(results1,1:2, function(x) invlogit(x))

# subtract the estimated probabilities from each other
results <- results1 - results0

# means and cis

x <- v2x_frassoc_thick_l
means <- apply(results,2,mean)
sdUpper <- apply(results,2,function(x) quantile(x, .975))
sdLower <- apply(results,2,function(x) quantile(x, .025))

#graph the results

dfplot <- data.frame(x, means, sdLower, sdUpper)
p <- ggplot(dfplot, aes(x=x, y=means, ymin=sdLower, ymax=sdUpper)) + 
  geom_line() +
  geom_ribbon(aes(ymin=sdLower,ymax=sdUpper),alpha=0.3) +
  theme_bw() + geom_hline(yintercept = 0, lty=2) + xlab('Freedom of association') + ylab('Change in probability of protest as \n Internet penetration (sending city) increases') +
  geom_rug(sides="b") +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=15,face="bold"))

p

pdf("ch9/cityweek_diffusion_bayes_assoc.pdf", width=8, height=5)
par(mar=c(4.5,4.5,1,1))
p
dev.off()

rm(list=setdiff(ls(), c("cityyear.data", 
                        "cityweek.data.persistence", 
                        "cityweek.data.diffusion", 
                        "ch9.cityyear.assoc", 
                        "ch9.cityweek.persistence.assoc", 
                        "ch9.cityweek.diffusion.assoc")))



print(ch9.cityweek.diffusion.assoc, digits=4)

set.seed(1337)

beta <- fixef(ch9.cityweek.diffusion.assoc)
betas <- beta[,1]

covvar <- vcov(ch9.cityweek.diffusion.assoc)
coefs.sim <- mvrnorm(10000, betas, covvar)

# Setting values for receiving Internet penetration

summary(log(cityweek.data.diffusion$numactivepc_l+1))
receiving_internet <- 0.41059

# Setting values for sendign Internet penetration

summary(cityweek.data.diffusion$lastprotestelsewhere_lnumactivepc_max)
sending_internet <- sort(unique(cityweek.data.diffusion$lastprotestelsewhere_lnumactivepc_max))
sending_internet <- sort(sample(sending_internet,200))

# Setting values for association

summary(cityweek.data.diffusion$v2x_frassoc_thick_l)
v2x_frassoc_thick_l <- 0.0845
v2x_frassoc_thick_l_mod <- 0.4027

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

x <- sending_internet
means <- apply(results,2,mean)
sdUpper <- apply(results,2,function(x) quantile(x, .975))
sdLower <- apply(results,2,function(x) quantile(x, .025))

#graph the results

dfplot <- data.frame(x, means, sdLower, sdUpper)
p <- ggplot(dfplot, aes(x=x, y=means, ymin=sdLower, ymax=sdUpper)) + 
  geom_line() +
  geom_ribbon(aes(ymin=sdLower,ymax=sdUpper),alpha=0.3) +
  theme_bw() + geom_hline(yintercept = 0, lty=2) + xlab('Internet penetration, sending') + ylab('Change in probability of protest \n as Freedom of association increases') +
  geom_rug(sides="b") +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=15,face="bold"))

p

pdf("ch9/cityweek_diffusion_bayes_assoc_symm.pdf", width=8, height=5)
par(mar=c(4.5,4.5,1,1))
p
dev.off()

rm(list=setdiff(ls(), c("cityyear.data", 
                        "cityweek.data.persistence", 
                        "cityweek.data.diffusion", 
                        "ch9.cityyear.assoc", 
                        "ch9.cityweek.persistence.assoc", 
                        "ch9.cityweek.diffusion.assoc")))

