# MARGINAL VALUES

print(ch8.cityweek.distance, digits=4)

set.seed(1337)

beta <- fixef(ch8.cityweek.distance)
betas <- beta[,1]

covvar <- vcov(ch8.cityweek.distance)
coefs.sim <- mvrnorm(10000, betas, covvar)

# Setting values for Internet penetration

summary(log(cityweek.data.diffusion$numactivepc_l+1))
receiving_internet <- 0.41059

# Setting values for sending Internet penetration

summary(cityweek.data.diffusion$lastprotestelsewhere_lnumactivepc_max)
sending_internet <- 0
sending_internet_mod <- 1.5375


# Setting values for sending Internet penetration

summary(log(cityweek.data.diffusion$lastprotestelsewhere_maxlnumactivepc_distance))
distance <- sort(unique(log(cityweek.data.diffusion$lastprotestelsewhere_maxlnumactivepc_distance)))
distance <- sort(sample(distance,1000))
distance

#bind them together in a matrix   

effects0 <- cbind(1,
                  receiving_internet,
                  sending_internet,
                  distance,
                  log(2),
                  log(12),
                  0,
                  0,
                  1,0,
                  0,
                  sending_internet*distance)

effects1 <- cbind(1,
                  receiving_internet,
                  sending_internet,
                  distance,
                  log(2),
                  log(12),
                  0,
                  0,
                  1,0,
                  0,
                  sending_internet_mod*distance)

#multiply the effects matrix with the matrix of simulated coefficients to obtain marginal effects
results0 <- coefs.sim %*% t(effects0)
results1 <- coefs.sim %*% t(effects1)

#evaluate each marginal effect using inverse logit, to obtain predicted probabilities
results0 <- apply(results0,1:2, function(x) invlogit(x))
results1 <- apply(results1,1:2, function(x) invlogit(x))

# subtract the estimated probabilities from each other
results <- results1 - results0

# means and cis

x <- distance

means <- apply(results,2,mean)
sdUpper <- apply(results,2,function(x) quantile(x, .975))
sdLower <- apply(results,2,function(x) quantile(x, .025))

x
means
sdUpper
sdLower

summary(cityweek.data.diffusion$lastprotestelsewhere_maxlnumactivepc_distance)
hist(cityweek.data.diffusion$lastprotestelsewhere_maxlnumactivepc_distance)
table(cityweek.data.diffusion$cowcode[cityweek.data.diffusion$lastprotestelsewhere_maxlnumactivepc_distance>mean(cityweek.data.diffusion$lastprotestelsewhere_maxlnumactivepc_distance)])
table(cityweek.data.diffusion$cowcode[cityweek.data.diffusion$lastprotestelsewhere_maxlnumactivepc_distance>quantile(cityweek.data.diffusion$lastprotestelsewhere_maxlnumactivepc_distance, .90)])

log(50)
x[7]
means[7]
sdUpper[7]
sdLower[7]

mean(cityweek.data.diffusion$lastprotestelsewhere_maxlnumactivepc_distance)
log(mean(cityweek.data.diffusion$lastprotestelsewhere_maxlnumactivepc_distance))
x[87]
means[87]
sdUpper[87]
sdLower[87]

log(quantile(cityweek.data.diffusion$lastprotestelsewhere_maxlnumactivepc_distance, .90))
x[170]
means[170]
sdUpper[170]
sdLower[170]

#graph the results

dfplot <- data.frame(x, means, sdLower, sdUpper)
p <- ggplot(dfplot, aes(x=x, y=means, ymin=sdLower, ymax=sdUpper)) + 
  geom_line() +
  geom_ribbon(aes(ymin=sdLower,ymax=sdUpper),alpha=0.3) +
  theme_bw() + geom_hline(yintercept = 0, lty=2) + xlab('Distance, sending and receiving (ln)') + ylab('Change in probability of protest diffusion as \n Internet penetration (sending city) increases') +
  geom_rug(sides="b") +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=15,face="bold"))

p

pdf("ch8/cityweek_diffusion_bayes_sendingeffect_distance.pdf", width=8, height=5)
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