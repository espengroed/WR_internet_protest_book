# Slopes in different institutional settings

summary(cityyear.data$v2x_frassoc_thick_l)
summary(log(cityyear.data$numactivepc_l+1))

cityyear.data$v2x_frassoc_thick_l_dich <- ifelse(cityyear.data$v2x_frassoc_thick_l<0.22905,0,NA)
cityyear.data$v2x_frassoc_thick_l_dich <- ifelse(cityyear.data$v2x_frassoc_thick_l>=0.22905,1,cityyear.data$v2x_frassoc_thick_l_dich)
summary(cityyear.data$v2x_frassoc_thick_l_dich)

label <- c(`0` = "Low freedom of association",
               `1` = "High freedom of association")

gg1 <- ggplot(cityyear.data[!is.na(cityyear.data$v2x_frassoc_thick_l), ], aes(x = log(numactivepc_l+1), y = protests, group=v2x_frassoc_thick_l_dich)) +
  geom_smooth(method = "glm", se = TRUE, formula = "y ~ log(x+1)", method.args = list(family = "binomial"(link = "logit")), color = "black") +
  facet_wrap(~v2x_frassoc_thick_l_dich, labeller = as_labeller(label), ncol=2) +
  #  geom_point(alpha = .1, size = 1, color = "darkgrey") +
  xlab("Internet penetration") + ylab("Probability of protest occurrence") +
  geom_hline(yintercept = 0, lty=2) +
  theme_bw() +
  geom_rug(sides="b") + coord_cartesian(ylim = c(0,.30)) +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=15,face="bold"), aspect.ratio=1)

print(gg1)

pdf("ch9/cityyear_incidence_slope_assoc.pdf", width=8, height=5)
par(mar=c(1,1,1,1))
gg1
dev.off()

cityyear.data0 <- subset(cityyear.data, v2x_frassoc_thick_l_dich==0)
x <- glm(protests ~ log(numactivepc_l+1), data=cityyear.data0, family = "binomial"(link = "logit"))
summary(x)
cityyear.data0$pred0 <- predict(x, type = "response")

median(log(cityyear.data0$numactivepc_l+1))
median(cityyear.data0$pred0[log(cityyear.data0$numactivepc_l+1)==0])
median(cityyear.data0$pred0[log(cityyear.data0$numactivepc_l+1)>0.4800331])


cityyear.data1 <- subset(cityyear.data, v2x_frassoc_thick_l_dich==1)
x <- glm(protests ~ log(numactivepc_l+1), data=cityyear.data1, family = "binomial"(link = "logit"))
summary(x)
cityyear.data1$pred0 <- predict(x, type = "response")

median(log(cityyear.data1$numactivepc_l+1))
median(cityyear.data1$pred0[log(cityyear.data1$numactivepc_l+1)==0])
median(cityyear.data1$pred0[log(cityyear.data1$numactivepc_l+1)>0.1961373])

rm(list=setdiff(ls(), c("cityyear.data", 
                        "cityweek.data.persistence", 
                        "cityweek.data.diffusion", 
                        "ch9.cityyear.assoc", 
                        "ch9.cityweek.persistence.assoc", 
                        "ch9.cityweek.diffusion.assoc")))




















# Slopes in different institutional settings

summary(cityweek.data.persistence$v2x_frassoc_thick_l)
summary(log(cityweek.data.persistence$numactivepc_l+1))

cityweek.data.persistence$v2x_frassoc_thick_l_dich <- ifelse(cityweek.data.persistence$v2x_frassoc_thick_l<0.3476,0,NA)
cityweek.data.persistence$v2x_frassoc_thick_l_dich <- ifelse(cityweek.data.persistence$v2x_frassoc_thick_l>=0.3476,1,cityweek.data.persistence$v2x_frassoc_thick_l_dich)
summary(cityweek.data.persistence$v2x_frassoc_thick_l_dich)


label <- c(`0` = "Low freedom of association",
           `1` = "High freedom of association")

gg1 <- ggplot(cityweek.data.persistence[!is.na(cityweek.data.persistence$v2x_frassoc_thick_l), ], aes(x = log(numactivepc_l+1), y = protests, group=v2x_frassoc_thick_l_dich)) +
  geom_smooth(method = "glm", se = TRUE, formula = "y ~ log(x+1)", method.args = list(family = "binomial"(link = "logit")), color = "black") +
  facet_wrap(~v2x_frassoc_thick_l_dich, labeller = as_labeller(label), ncol=2) +
  #  geom_point(alpha = .1, size = 1, color = "darkgrey") +
  xlab("Internet penetration") + ylab("Probability of protest persistence") +
  geom_hline(yintercept = 0, lty=2) +
  theme_bw() +
  geom_rug(sides="b") + coord_cartesian(ylim = c(0,.25)) +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=15,face="bold"), aspect.ratio=1)

print(gg1)

pdf("ch9/cityweek_persistence_slope_assoc.pdf", width=8, height=5)
gg1
dev.off()

cityweek.data.persistence0 <- subset(cityweek.data.persistence, v2x_frassoc_thick_l_dich==0)
x <- glm(protests ~ log(numactivepc_l+1), data=cityweek.data.persistence0, family = "binomial"(link = "logit"))
summary(x)
cityweek.data.persistence0$pred0 <- predict(x, type = "response")

median(log(cityweek.data.persistence0$numactivepc_l+1))
median(cityweek.data.persistence0$pred0[log(cityweek.data.persistence0$numactivepc_l+1)==0])
median(cityweek.data.persistence0$pred0[log(cityweek.data.persistence0$numactivepc_l+1)>0.1446385])


cityweek.data.persistence1 <- subset(cityweek.data.persistence, v2x_frassoc_thick_l_dich==1)
x <- glm(protests ~ log(numactivepc_l+1), data=cityweek.data.persistence1, family = "binomial"(link = "logit"))
summary(x)
cityweek.data.persistence1$pred0 <- predict(x, type = "response")

median(log(cityweek.data.persistence1$numactivepc_l+1))
median(cityweek.data.persistence1$pred0[log(cityweek.data.persistence1$numactivepc_l+1)==0])
median(cityweek.data.persistence1$pred0[log(cityweek.data.persistence1$numactivepc_l+1)>0.08367498])


rm(list=setdiff(ls(), c("cityyear.data", 
                        "cityweek.data.persistence", 
                        "cityweek.data.diffusion", 
                        "ch9.cityyear.assoc", 
                        "ch9.cityweek.persistence.assoc", 
                        "ch9.cityweek.diffusion.assoc")))
















# Slopes in different institutional settings

summary(cityweek.data.diffusion$timesinceprotestelsewhere)
summary(cityweek.data.diffusion$timesinceprotest)

summary(cityweek.data.diffusion$lastprotestelsewhere_lnumactivepc_max)
summary(cityweek.data.diffusion$v2x_frassoc_thick_l)

cityweek.data.diffusion$protestelsewhere_dich2 <- NA
cityweek.data.diffusion$protestelsewhere_dich2[cityweek.data.diffusion$timesinceprotestelsewhere==1 & cityweek.data.diffusion$timesinceprotest>12 & cityweek.data.diffusion$lastprotestelsewhere_lnumactivepc_max<=1.5375 & cityweek.data.diffusion$v2x_frassoc_thick_l<0.2250] <- 0
cityweek.data.diffusion$protestelsewhere_dich2[cityweek.data.diffusion$timesinceprotestelsewhere==1 & cityweek.data.diffusion$timesinceprotest>12 & cityweek.data.diffusion$lastprotestelsewhere_lnumactivepc_max>1.5375 & cityweek.data.diffusion$v2x_frassoc_thick_l<0.2250] <- 1
cityweek.data.diffusion$protestelsewhere_dich2[cityweek.data.diffusion$timesinceprotestelsewhere==1 & cityweek.data.diffusion$timesinceprotest>12 & cityweek.data.diffusion$lastprotestelsewhere_lnumactivepc_max<=1.5375 & cityweek.data.diffusion$v2x_frassoc_thick_l>=0.2250] <- 2
cityweek.data.diffusion$protestelsewhere_dich2[cityweek.data.diffusion$timesinceprotestelsewhere==1 & cityweek.data.diffusion$timesinceprotest>12 & cityweek.data.diffusion$lastprotestelsewhere_lnumactivepc_max>1.5375 & cityweek.data.diffusion$v2x_frassoc_thick_l>=0.2250] <- 3

table(cityweek.data.diffusion$protestelsewhere_dich2)

# ALL COUNTRIES

pooled.diffusion <- glm(protests ~ factor(protestelsewhere_dich2),
                        data = cityweek.data.diffusion, family = "binomial"(link = "logit"))

summary(pooled.diffusion)

set.seed(1337)

beta <- coef(pooled.diffusion)
covvar <- vcov(pooled.diffusion)
coefs.sim <- mvrnorm(10000, beta, covvar)

#bind them together in a matrix   

effects0 <- cbind(1,
                  0,0,0)
results0 <- coefs.sim %*% t(effects0)
results0 <- apply(results0,1:2, function(x) invlogit(x))

effects1 <- cbind(1,
                  1,0,0)
results1 <- coefs.sim %*% t(effects1)
results1 <- apply(results1,1:2, function(x) invlogit(x))

effects2 <- cbind(1,
                  0,1,0)
results2 <- coefs.sim %*% t(effects2)
results2 <- apply(results2,1:2, function(x) invlogit(x))

effects3 <- cbind(1,
                  0,0,1)
results3 <- coefs.sim %*% t(effects3)
results3 <- apply(results3,1:2, function(x) invlogit(x))

# means and cis

means0 <- apply(results0,2,mean)
sdUpper0 <- apply(results0,2,function(x) quantile(x, .975))
sdLower0 <- apply(results0,2,function(x) quantile(x, .025))

means1 <- apply(results1,2,mean)
sdUpper1 <- apply(results1,2,function(x) quantile(x, .975))
sdLower1 <- apply(results1,2,function(x) quantile(x, .025))

means2 <- apply(results2,2,mean)
sdUpper2 <- apply(results2,2,function(x) quantile(x, .975))
sdLower2 <- apply(results2,2,function(x) quantile(x, .025))

means3 <- apply(results3,2,mean)
sdUpper3 <- apply(results3,2,function(x) quantile(x, .975))
sdLower3 <- apply(results3,2,function(x) quantile(x, .025))

m <- rbind(means0[1], means1[1], means2[1], means3[1])
hi <- rbind(sdUpper0[1], sdUpper1[1], sdUpper2[1], sdUpper3[1])
lo <- rbind(sdLower0[1], sdLower1[1], sdLower2[1], sdLower3[1])
l <- c("Low Internet", "High Internet","Low Internet", "High Internet")
protestelsewhere_dich2 <- c(1,1,2,2)

label <- c(`1` = "Low freedom of association",
           `2` = "High freedom of association")

dfplot <- data.frame(protestelsewhere_dich2, l, m, lo, hi)
p <- ggplot(dfplot, aes(x=l, y=m, ymin=lo, ymax=hi)) + 
  geom_point() +
  geom_pointrange(aes(ymin=lo,ymax=hi), fill = "grey70") +
#  geom_crossbar(aes(ymin=lo,ymax=hi),alpha=0.3) +
  facet_wrap(~protestelsewhere_dich2, labeller = as_labeller(label), ncol=2) +
  theme_bw() + geom_hline(yintercept = 0, lty=2) + xlab('') + ylab('Probability of protest') +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=15,face="bold"), aspect.ratio=1)

p

pdf("ch9/cityweek_diffusion_sending_assoc.pdf", width=8, height=5)
par(mar=c(4.5,4.5,1,1))
p
dev.off()

rm(list=setdiff(ls(), c("cityyear.data", 
                        "cityweek.data.persistence", 
                        "cityweek.data.diffusion", 
                        "ch9.cityyear.assoc", 
                        "ch9.cityweek.persistence.assoc", 
                        "ch9.cityweek.diffusion.assoc")))
