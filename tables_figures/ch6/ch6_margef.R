# Simulating the effect of Internet penetration on protest
# ABSOLUTE VALUES

print(ch6.cityyear.logit, digits=4)

# Simulate coefficients based on the model

set.seed(1337)

beta <- fixef(ch6.cityyear.logit)
betas <- beta[,1]

covvar <- vcov(ch6.cityyear.logit)
coefs.sim <- mvrnorm(10000, betas, covvar)

# Internet penetration

summary(log(cityyear.data$numactivepc_l+1))
#numactivepc <- sort(sample(log(cityyear.data$numactivepc_l+1), 300))
numactivepc <- sort(unique(log(cityyear.data$numactivepc_l+1)))
numactivepc <- sort(sample(numactivepc, 1000))
#bind them together in a matrix   

effects0 <- cbind(1,
                  numactivepc,
                  0,
                  0,
                  0,
                  0,
                  1,0,
                  0)

#multiply the effects matrix with the matrix of simulated coefficients to obtain marginal effects
results0 <- coefs.sim %*% t(effects0)

#evaluate each marginal effect using inverse logit, to obtain predicted probabilities
results0 <- apply(results0,1:2, function(x) invlogit(x))

# means and cis

x <- numactivepc
means <- apply(results0,2,mean)
sdUpper <- apply(results0,2,function(x) quantile(x, .975))
sdLower <- apply(results0,2,function(x) quantile(x, .025))

summary(log(cityyear.data$numactivepc_l+1))
summary(numactivepc)

x[1]
means[1]

x[380]
means[380]

x[686]
means[686]

means[686]/means[1]

par(mfrow=c(1,1))

dfplot <- data.frame(x, means, sdLower, sdUpper)
p <- ggplot(dfplot, aes(x=x, y=means, ymin=sdLower, ymax=sdUpper)) + 
  geom_line() +
  geom_ribbon(aes(ymin=sdLower,ymax=sdUpper),alpha=0.3) +
  theme_bw() + geom_hline(yintercept = 0, lty=2) + xlab('Internet penetration') + ylab('Probability of protest occurrence') +
  geom_rug(sides="b") +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=15,face="bold"))

p

pdf("ch6/cityyear_incidence_absolute_bayes.pdf", width=8, height=5)
par(mar=c(4.5,4.5,1,1))
p
dev.off()

rm(list=setdiff(ls(), c("cityyear.data", 
                        "ch6.cityweek.fixed",
                        "ch6.cityweek.fixed2",
                        "ch6.cityyear.logit", 
                        "ch6.cityyear.nb", 
                        "ch6.cityyear.ols", 
                        "ch6.cityyear.rob1", 
                        "ch6.cityyear.rob2", 
                        "ch6.cityyear.rob3",
                        "ch6.onset1",
                        "ch6.onset2")))
