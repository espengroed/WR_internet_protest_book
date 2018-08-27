# Coefficient plots

# Main model

coefs <- fixef(ch6.cityyear.logit)
coefs <- coefs[2:9, 1]
ses <- fixef(ch6.cityyear.logit)
ses <- ses[2:9, 2]

ylo <- coefs - 1.96*ses
yhi <- coefs + 1.96*ses
names <- c("Internet penetration (ln)","Protest in same city (t-1)","Protest in different city (t-1)","Night lights pc (ln)","Population (ln)","Regional capital","National capital","Civil war")
names <- factor(names, levels = rev(c("Internet penetration (ln)","Protest in same city (t-1)","Protest in different city (t-1)","Night lights pc (ln)","Population (ln)","Regional capital","National capital","Civil war")))

dfplot <- data.frame(names, coefs, ses, ylo, yhi)

p <- ggplot(dfplot, aes(x=names, y=coefs, ymin=ylo, ymax=yhi)) + 
  geom_pointrange(colour=ifelse(ylo < 0 & yhi > 0, "grey", "black")) + 
  theme_bw() + 
  geom_hline(yintercept = 0, lty=2) + 
  coord_flip() + xlab('') + ylab('')  +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=15,face="bold"))
p

pdf("ch6/coefplot_incidence_bayes_ml.pdf", height=4, width=6)
par(mar=c(3.5,1,1,1))
p
dev.off()

rm(coefs, ses, names, ylo, yhi, p, dfplot)
















# Robustness tests

print(ch6.cityyear.ols, digits=4)
coefs1 <- fixef(ch6.cityyear.ols)
ses1 <- fixef(ch6.cityyear.ols)
coefs1 <- coefs1[2,1]
ses1 <- ses1[2,2]

print(ch6.cityyear.nb, digits=4)
coefs2 <- fixef(ch6.cityyear.nb)
ses2 <- fixef(ch6.cityyear.nb)
coefs2 <- coefs2[2,1]
ses2 <- ses2[2,2]

print(ch6.cityyear.rob2, digits=4)
coefs4 <- fixef(ch6.cityyear.rob2)
ses4 <- fixef(ch6.cityyear.rob2)
coefs4 <- coefs4[2,1]
ses4 <- ses4[2,2]

print(ch6.cityyear.rob3, digits=4)
coefs5 <- fixef(ch6.cityyear.rob3)
ses5 <- fixef(ch6.cityyear.rob3)
coefs5 <- coefs5[3,1]
ses5 <- ses5[3,2]

print(ch6.cityyear.rob3, digits=4)
coefs6 <- fixef(ch6.cityyear.rob3)
ses6 <- fixef(ch6.cityyear.rob3)
coefs6 <- coefs6[12,1]
ses6 <- ses6[12,2]

print(ch6.onset1, digits=4)
coefs7 <- fixef(ch6.onset1)
ses7 <- fixef(ch6.onset1)
coefs7 <- coefs7[2,1]
ses7 <- ses7[2,2]

print(ch6.onset2, digits=4)
coefs8 <- fixef(ch6.onset2)
ses8 <- fixef(ch6.onset2)
coefs8 <- coefs8[2,1]
ses8 <- ses8[2,2]

# LPM models

summary(ch6.cityweek.fixed)
coefs9 <- -0.0153
ses9 <- 0.0064
summary(ch6.cityweek.fixed2)
coefs10 <- -0.2817
ses10 <- 0.0844

coefs <- rev(cbind(coefs7, coefs8, coefs1,coefs2,coefs4,coefs9,coefs10,coefs5,coefs6))
ses <- rev(cbind(ses7, ses8, ses1,ses2,ses4,ses9,ses10,ses5,ses6))

ylo <- coefs - 1.96*ses
yhi <- coefs + 1.96*ses

names <- rev(c("Onset 1","Onset 2","Linear reg.","Negative binomial reg.","Logistic reg., dichotomous Internet penetration","Linear prob. model with fixed effects","Log-linear OLS model with fixed effects","Multinomial logistic reg. (local)","Multinomial logistic reg. (national)"))
names <- factor(names, levels = rev(c("Onset 1","Onset 2","Linear reg.","Negative binomial reg.","Logistic reg., dichotomous Internet penetration","Linear prob. model with fixed effects","Log-linear OLS model with fixed effects","Multinomial logistic reg. (local)","Multinomial logistic reg. (national)")))

dfplot <- data.frame(names, coefs, ses, ylo, yhi)

p <- ggplot(dfplot, aes(x=names, y=coefs, ymin=ylo, ymax=yhi)) + 
  geom_pointrange(colour=ifelse(ylo < 0 & yhi > 0, "grey", "black")) + 
  theme_bw() + geom_hline(yintercept = 0, lty=2) + coord_flip() + xlab('') + ylab('') +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=15,face="bold"))
  

p

pdf("ch6/coefplot_incidence_bayes_ml_robustness.pdf", height=4, width=6)
par(mar=c(3.5,1,1,1))
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
