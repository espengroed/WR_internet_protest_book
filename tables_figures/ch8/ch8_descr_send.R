# 1) Show that there is a diffusion effect
# 2) Show that internet penetration has something to do it

summary(cityweek.data.diffusion$timesinceprotestelsewhere)
summary(cityweek.data.diffusion$timesinceprotest)

cityweek.data.diffusion$protestelsewhere_dich <- NA
cityweek.data.diffusion$protestelsewhere_dich[cityweek.data.diffusion$timesinceprotestelsewhere>1 & cityweek.data.diffusion$timesinceprotest>12] <- 0
cityweek.data.diffusion$protestelsewhere_dich[cityweek.data.diffusion$timesinceprotestelsewhere==1 & cityweek.data.diffusion$timesinceprotest>12] <- 1

table(cityweek.data.diffusion$protestelsewhere_dich)

summary(cityweek.data.diffusion$lastprotestelsewhere_lnumactivepc_max)

cityweek.data.diffusion$protestelsewhere_dich2 <- NA
cityweek.data.diffusion$protestelsewhere_dich2[cityweek.data.diffusion$timesinceprotestelsewhere==1 & cityweek.data.diffusion$timesinceprotest>12 & cityweek.data.diffusion$lastprotestelsewhere_lnumactivepc_max<=1.5375] <- 0
cityweek.data.diffusion$protestelsewhere_dich2[cityweek.data.diffusion$timesinceprotestelsewhere==1 & cityweek.data.diffusion$timesinceprotest>12 & cityweek.data.diffusion$lastprotestelsewhere_lnumactivepc_max>1.5375] <- 1
table(cityweek.data.diffusion$protestelsewhere_dich2)

table(cityweek.data.diffusion$protestelsewhere_dich2)

# ALL COUNTRIES

pooled.diffusion <- glm(protests ~ protestelsewhere_dich,
                        data = cityweek.data.diffusion, family = "binomial"(link = "logit"))

summary(pooled.diffusion)

set.seed(1337)

beta <- coef(pooled.diffusion)
covvar <- vcov(pooled.diffusion)
coefs.sim <- mvrnorm(10000, beta, covvar)

timesinceprotestelsewhere <- c(0,1)
#bind them together in a matrix   

effects0 <- cbind(1,
                  timesinceprotestelsewhere)

#multiply the effects matrix with the matrix of simulated coefficients to obtain marginal effects
results0 <- coefs.sim %*% t(effects0)

#evaluate each marginal effect using inverse logit, to obtain predicted probabilities
results0 <- apply(results0,1:2, function(x) invlogit(x))

# means and cis

x <- c("No recent \n protest elsewhere", "Recent protest \n elsewhere")
#x <- c(0,1)
means <- apply(results0,2,mean)
sdUpper <- apply(results0,2,function(x) quantile(x, .975))
sdLower <- apply(results0,2,function(x) quantile(x, .025))

means
sdUpper
sdLower

pooled.diffusion <- glm(protests ~ protestelsewhere_dich2,
                        data = cityweek.data.diffusion, family = "binomial"(link = "logit"))

summary(pooled.diffusion)

set.seed(1337)

beta <- coef(pooled.diffusion)
covvar <- vcov(pooled.diffusion)
coefs.sim <- mvrnorm(10000, beta, covvar)

timesinceprotestelsewhere <- c(0,1)
#bind them together in a matrix   

effects0 <- cbind(1,
                  timesinceprotestelsewhere)

#multiply the effects matrix with the matrix of simulated coefficients to obtain marginal effects
results0 <- coefs.sim %*% t(effects0)

#evaluate each marginal effect using inverse logit, to obtain predicted probabilities
results0 <- apply(results0,1:2, function(x) invlogit(x))

# means and cis

x2 <- c("Recent protest \n elsewhere, \n low Internet", "Recent protest \n elsewhere, \n high Internet")
#x <- c(0,1)
means2 <- apply(results0,2,mean)
sdUpper2 <- apply(results0,2,function(x) quantile(x, .975))
sdLower2 <- apply(results0,2,function(x) quantile(x, .025))

means2
sdUpper2
sdLower2

dfplot <- data.frame(means, sdLower, sdUpper, x)
dfplot2 <- data.frame(means2, sdLower2, sdUpper2, x2)

p <- ggplot(dfplot, aes(x=x, y=means, ymin=sdLower, ymax=sdUpper)) + 
  geom_point() +
  geom_pointrange(aes(ymin=sdLower,ymax=sdUpper),alpha=0.3) +
  coord_cartesian(ylim = c(0,.016)) +
  theme_bw() + geom_hline(yintercept = 0, lty=2) + xlab('') + ylab('Probability of protest') +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=15,face="bold"))

p

p2 <- ggplot(dfplot2, aes(x=x2, y=means2, ymin=sdLower2, ymax=sdUpper2)) + 
  geom_point() +
  geom_pointrange(aes(ymin=sdLower2,ymax=sdUpper2),alpha=0.3) +
  coord_cartesian(ylim = c(0,.016)) +
  theme_bw() + geom_hline(yintercept = 0, lty=2) + xlab('') + ylab('Probability of protest') +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=15,face="bold"))

p2

grid.arrange(p,p2, nrow=1)

pdf("ch8/cityweek_diffusion_sending1.pdf", height=4, width=4)
par(mar=c(4.5,4.5,1,1))
p
dev.off()

pdf("ch8/cityweek_diffusion_sending2.pdf", height=4, width=4)
par(mar=c(4.5,4.5,1,1))
p2
dev.off()

rm(list=setdiff(ls(), c("cityweek.data.diffusion", 
                        "ch8.cityweek.bare", 
                        "ch8.cityweek.bare.yearslope",
                        "ch8.cityweek.distance",
                        "ch8.cityweek.threeway", 
                        "ch8.cityweek.threeway.repr", 
                        "ch8.cityweek.twoway")))



















# RUSSIA

summary(cityweek.data.diffusion$lastprotestelsewhere_lnumactivepc_max[cityweek.data.diffusion$cowcode==365])

cityweek.data.diffusion$protestelsewhere_dich2 <- NA
cityweek.data.diffusion$protestelsewhere_dich2[cityweek.data.diffusion$timesinceprotestelsewhere==1 & cityweek.data.diffusion$timesinceprotest>12 & cityweek.data.diffusion$lastprotestelsewhere_lnumactivepc_max<=2.635] <- 0
cityweek.data.diffusion$protestelsewhere_dich2[cityweek.data.diffusion$timesinceprotestelsewhere==1 & cityweek.data.diffusion$timesinceprotest>12 & cityweek.data.diffusion$lastprotestelsewhere_lnumactivepc_max>2.635] <- 1
table(cityweek.data.diffusion$protestelsewhere_dich2)

table(cityweek.data.diffusion$protestelsewhere_dich2)

pooled.diffusion <- glm(protests ~ protestelsewhere_dich,
                        data = cityweek.data.diffusion[cityweek.data.diffusion$cowcode==365, ], family = "binomial"(link = "logit"))

summary(pooled.diffusion)

set.seed(1337)

beta <- coef(pooled.diffusion)
covvar <- vcov(pooled.diffusion)
coefs.sim <- mvrnorm(10000, beta, covvar)

timesinceprotestelsewhere <- c(0,1)
#bind them together in a matrix   

effects0 <- cbind(1,
                  timesinceprotestelsewhere)

#multiply the effects matrix with the matrix of simulated coefficients to obtain marginal effects
results0 <- coefs.sim %*% t(effects0)

#evaluate each marginal effect using inverse logit, to obtain predicted probabilities
results0 <- apply(results0,1:2, function(x) invlogit(x))

# means and cis

x <- c("No recent \n protest elsewhere", "Recent protest \n elsewhere")
#x <- c(0,1)
means <- apply(results0,2,mean)
sdUpper <- apply(results0,2,function(x) quantile(x, .975))
sdLower <- apply(results0,2,function(x) quantile(x, .025))

means
sdUpper
sdLower

pooled.diffusion <- glm(protests ~ protestelsewhere_dich2,
                        data = cityweek.data.diffusion[cityweek.data.diffusion$cowcode==365, ], family = "binomial"(link = "logit"))

summary(pooled.diffusion)

set.seed(1337)

beta <- coef(pooled.diffusion)
covvar <- vcov(pooled.diffusion)
coefs.sim <- mvrnorm(10000, beta, covvar)

timesinceprotestelsewhere <- c(0,1)
#bind them together in a matrix   

effects0 <- cbind(1,
                  timesinceprotestelsewhere)

#multiply the effects matrix with the matrix of simulated coefficients to obtain marginal effects
results0 <- coefs.sim %*% t(effects0)

#evaluate each marginal effect using inverse logit, to obtain predicted probabilities
results0 <- apply(results0,1:2, function(x) invlogit(x))

# means and cis

x2 <- c("Recent protest \n elsewhere, \n low Internet", "Recent protest \n elsewhere, \n high Internet")
#x <- c(0,1)
means2 <- apply(results0,2,mean)
sdUpper2 <- apply(results0,2,function(x) quantile(x, .975))
sdLower2 <- apply(results0,2,function(x) quantile(x, .025))

dfplot <- data.frame(means, sdLower, sdUpper, x)
dfplot2 <- data.frame(means2, sdLower2, sdUpper2, x2)

p <- ggplot(dfplot, aes(x=x, y=means, ymin=sdLower, ymax=sdUpper)) + 
  geom_point() +
  geom_pointrange(aes(ymin=sdLower,ymax=sdUpper),alpha=0.3) +
  coord_cartesian(ylim = c(0,.025)) +
  theme_bw() + geom_hline(yintercept = 0, lty=2) + xlab('') + ylab('Probability of protest') +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=15,face="bold"))

p

p2 <- ggplot(dfplot2, aes(x=x2, y=means2, ymin=sdLower2, ymax=sdUpper2)) + 
  geom_point() +
  geom_pointrange(aes(ymin=sdLower2,ymax=sdUpper2),alpha=0.3) +
  coord_cartesian(ylim = c(0,.025)) +
  theme_bw() + geom_hline(yintercept = 0, lty=2) + xlab('') + ylab('Probability of protest') +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=15,face="bold"))

p2

grid.arrange(p,p2)

pdf("ch8/cityweek_diffusion_sending_russia1.pdf", height=4, width=4)
par(mar=c(4.5,4.5,1,1))
p
dev.off()

pdf("ch8/cityweek_diffusion_sending_russia2.pdf", height=4, width=4)
par(mar=c(4.5,4.5,1,1))
p2
dev.off()

rm(list=setdiff(ls(), c("cityweek.data.diffusion", 
                        "ch8.cityweek.bare", 
                        "ch8.cityweek.bare.yearslope",
                        "ch8.cityweek.distance",
                        "ch8.cityweek.threeway", 
                        "ch8.cityweek.threeway.repr", 
                        "ch8.cityweek.twoway")))

















# Iran

summary(cityweek.data.diffusion$lastprotestelsewhere_lnumactivepc_max[cityweek.data.diffusion$cowcode==630])

cityweek.data.diffusion$protestelsewhere_dich2 <- NA
cityweek.data.diffusion$protestelsewhere_dich2[cityweek.data.diffusion$timesinceprotestelsewhere==1 & cityweek.data.diffusion$timesinceprotest>12 & cityweek.data.diffusion$lastprotestelsewhere_lnumactivepc_max<=0.15931] <- 0
cityweek.data.diffusion$protestelsewhere_dich2[cityweek.data.diffusion$timesinceprotestelsewhere==1 & cityweek.data.diffusion$timesinceprotest>12 & cityweek.data.diffusion$lastprotestelsewhere_lnumactivepc_max>0.15931] <- 1
table(cityweek.data.diffusion$protestelsewhere_dich2)

pooled.diffusion <- glm(protests ~ protestelsewhere_dich,
                        data = cityweek.data.diffusion[cityweek.data.diffusion$cowcode==630, ], family = "binomial"(link = "logit"))

summary(pooled.diffusion)

set.seed(1337)

beta <- coef(pooled.diffusion)
covvar <- vcov(pooled.diffusion)
coefs.sim <- mvrnorm(10000, beta, covvar)

timesinceprotestelsewhere <- c(0,1)
#bind them together in a matrix   

effects0 <- cbind(1,
                  timesinceprotestelsewhere)

#multiply the effects matrix with the matrix of simulated coefficients to obtain marginal effects
results0 <- coefs.sim %*% t(effects0)

#evaluate each marginal effect using inverse logit, to obtain predicted probabilities
results0 <- apply(results0,1:2, function(x) invlogit(x))

# means and cis

x <- c("No recent \n protest elsewhere", "Recent protest \n elsewhere")
#x <- c(0,1)
means <- apply(results0,2,mean)
sdUpper <- apply(results0,2,function(x) quantile(x, .975))
sdLower <- apply(results0,2,function(x) quantile(x, .025))

means
sdUpper
sdLower

pooled.diffusion <- glm(protests ~ protestelsewhere_dich2,
                        data = cityweek.data.diffusion[cityweek.data.diffusion$cowcode==630, ], family = "binomial"(link = "logit"))

summary(pooled.diffusion)

set.seed(1337)

beta <- coef(pooled.diffusion)
covvar <- vcov(pooled.diffusion)
coefs.sim <- mvrnorm(10000, beta, covvar)

timesinceprotestelsewhere <- c(0,1)
#bind them together in a matrix   

effects0 <- cbind(1,
                  timesinceprotestelsewhere)

#multiply the effects matrix with the matrix of simulated coefficients to obtain marginal effects
results0 <- coefs.sim %*% t(effects0)

#evaluate each marginal effect using inverse logit, to obtain predicted probabilities
results0 <- apply(results0,1:2, function(x) invlogit(x))

# means and cis

x2 <- c("Recent protest \n elsewhere, \n low Internet", "Recent protest \n elsewhere, \n high Internet")
#x <- c(0,1)
means2 <- apply(results0,2,mean)
sdUpper2 <- apply(results0,2,function(x) quantile(x, .975))
sdLower2 <- apply(results0,2,function(x) quantile(x, .025))

dfplot <- data.frame(means, sdLower, sdUpper, x)
dfplot2 <- data.frame(means2, sdLower2, sdUpper2, x2)

p <- ggplot(dfplot, aes(x=x, y=means, ymin=sdLower, ymax=sdUpper)) + 
  geom_point() +
  geom_pointrange(aes(ymin=sdLower,ymax=sdUpper),alpha=0.3) +
  coord_cartesian(ylim = c(0,.025)) +
  theme_bw() + geom_hline(yintercept = 0, lty=2) + xlab('') + ylab('Probability of protest') +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=15,face="bold"))

p

p2 <- ggplot(dfplot2, aes(x=x2, y=means2, ymin=sdLower2, ymax=sdUpper2)) + 
  geom_point() +
  geom_pointrange(aes(ymin=sdLower2,ymax=sdUpper2),alpha=0.3) +
  coord_cartesian(ylim = c(0,.025)) +
  theme_bw() + geom_hline(yintercept = 0, lty=2) + xlab('') + ylab('Probability of protest') +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=15,face="bold"))

p2

grid.arrange(p,p2)

pdf("ch8/cityweek_diffusion_sending_iran1.pdf", height=4, width=4)
par(mar=c(4.5,4.5,1,1))
p
dev.off()

pdf("ch8/cityweek_diffusion_sending_iran2.pdf", height=4, width=4)
par(mar=c(4.5,4.5,1,1))
p2
dev.off()

rm(list=setdiff(ls(), c("cityweek.data.diffusion", 
                        "ch8.cityweek.bare", 
                        "ch8.cityweek.bare.yearslope",
                        "ch8.cityweek.distance",
                        "ch8.cityweek.threeway", 
                        "ch8.cityweek.threeway.repr", 
                        "ch8.cityweek.twoway")))

















# EGYPT

summary(cityweek.data.diffusion$lastprotestelsewhere_lnumactivepc_max[cityweek.data.diffusion$cowcode==651])

cityweek.data.diffusion$protestelsewhere_dich2 <- NA
cityweek.data.diffusion$protestelsewhere_dich2[cityweek.data.diffusion$timesinceprotestelsewhere==1 & cityweek.data.diffusion$timesinceprotest>12 & cityweek.data.diffusion$lastprotestelsewhere_lnumactivepc_max<=0.3873] <- 0
cityweek.data.diffusion$protestelsewhere_dich2[cityweek.data.diffusion$timesinceprotestelsewhere==1 & cityweek.data.diffusion$timesinceprotest>12 & cityweek.data.diffusion$lastprotestelsewhere_lnumactivepc_max>0.3873] <- 1
table(cityweek.data.diffusion$protestelsewhere_dich2)

pooled.diffusion <- glm(protests ~ protestelsewhere_dich,
                        data = cityweek.data.diffusion[cityweek.data.diffusion$cowcode==651, ], family = "binomial"(link = "logit"))

summary(pooled.diffusion)

set.seed(1337)

beta <- coef(pooled.diffusion)
covvar <- vcov(pooled.diffusion)
coefs.sim <- mvrnorm(10000, beta, covvar)

timesinceprotestelsewhere <- c(0,1)
#bind them together in a matrix   

effects0 <- cbind(1,
                  timesinceprotestelsewhere)

#multiply the effects matrix with the matrix of simulated coefficients to obtain marginal effects
results0 <- coefs.sim %*% t(effects0)

#evaluate each marginal effect using inverse logit, to obtain predicted probabilities
results0 <- apply(results0,1:2, function(x) invlogit(x))

# means and cis

x <- c("No recent \n protest elsewhere", "Recent protest \n elsewhere")
#x <- c(0,1)
means <- apply(results0,2,mean)
sdUpper <- apply(results0,2,function(x) quantile(x, .975))
sdLower <- apply(results0,2,function(x) quantile(x, .025))

means
sdUpper
sdLower

pooled.diffusion <- glm(protests ~ protestelsewhere_dich2,
                        data = cityweek.data.diffusion[cityweek.data.diffusion$cowcode==651, ], family = "binomial"(link = "logit"))

summary(pooled.diffusion)

set.seed(1337)

beta <- coef(pooled.diffusion)
covvar <- vcov(pooled.diffusion)
coefs.sim <- mvrnorm(10000, beta, covvar)

timesinceprotestelsewhere <- c(0,1)
#bind them together in a matrix   

effects0 <- cbind(1,
                  timesinceprotestelsewhere)

#multiply the effects matrix with the matrix of simulated coefficients to obtain marginal effects
results0 <- coefs.sim %*% t(effects0)

#evaluate each marginal effect using inverse logit, to obtain predicted probabilities
results0 <- apply(results0,1:2, function(x) invlogit(x))

# means and cis

x2 <- c("Recent protest \n elsewhere, \n low Internet", "Recent protest \n elsewhere, \n high Internet")
#x <- c(0,1)
means2 <- apply(results0,2,mean)
sdUpper2 <- apply(results0,2,function(x) quantile(x, .975))
sdLower2 <- apply(results0,2,function(x) quantile(x, .025))

dfplot <- data.frame(means, sdLower, sdUpper, x)
dfplot2 <- data.frame(means2, sdLower2, sdUpper2, x2)

p <- ggplot(dfplot, aes(x=x, y=means, ymin=sdLower, ymax=sdUpper)) + 
  geom_point() +
  geom_pointrange(aes(ymin=sdLower,ymax=sdUpper),alpha=0.3)  +
  coord_cartesian(ylim = c(0,.025)) +
  theme_bw() + geom_hline(yintercept = 0, lty=2) + xlab('') + ylab('Probability of protest') +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=15,face="bold"))

p

p2 <- ggplot(dfplot2, aes(x=x2, y=means2, ymin=sdLower2, ymax=sdUpper2)) + 
  geom_point() +
  geom_pointrange(aes(ymin=sdLower2,ymax=sdUpper2),alpha=0.3)  +
  coord_cartesian(ylim = c(0,.025)) +
  theme_bw() + geom_hline(yintercept = 0, lty=2) + xlab('') + ylab('Probability of protest') +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=15,face="bold"))

p2

grid.arrange(p,p2)

pdf("ch8/cityweek_diffusion_sending_egypt1.pdf", height=4, width=4)
par(mar=c(4.5,4.5,1,1))
p
dev.off()

pdf("ch8/cityweek_diffusion_sending_egypt2.pdf", height=4, width=4)
par(mar=c(4.5,4.5,1,1))
p2
dev.off()

rm(list=setdiff(ls(), c("cityweek.data.diffusion", 
                        "ch8.cityweek.bare", 
                        "ch8.cityweek.bare.yearslope",
                        "ch8.cityweek.distance",
                        "ch8.cityweek.threeway", 
                        "ch8.cityweek.threeway.repr", 
                        "ch8.cityweek.twoway")))
