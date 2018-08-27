# Overall slopes

x <- glm(protests ~ log(numactivepc_l+1), data=cityyear.data, family = "binomial"(link = "logit"))
summary(x)
cityyear.data$prob <- predict(x, type="response")
summary(cityyear.data$prob)
plot(log(cityyear.data$numactivepc_l+1), cityyear.data$prob)

gg1 <- ggplot(cityyear.data, aes(x = log(numactivepc_l+1), y = protests)) +
  geom_smooth(method = "glm", se = TRUE, formula = "y ~ log(x+1)", method.args = list(family = "binomial"(link = "logit")), color = "black") +
#  geom_point(alpha = .1, size = 1, color = "darkgrey") +
  xlab("Internet penetration (ln)") + ylab("Probability of protest occurrence") +
  geom_hline(yintercept = 0, lty=2) + #geom_rug(sides="b") + expand_limits(y=c(0,.25)) +
  theme_bw() +
  geom_rug(sides="b") + coord_cartesian(ylim = c(0,.16)) +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=15,face="bold"))

print(gg1)

gg2 <- ggplot(cityyear.data, aes(x = log(numactivepc_l+1), y = protests)) +
  geom_smooth(method = "glm", se = TRUE, formula = "y ~ poly(log(x+1),3,raw=T)", method.args = list(family = "binomial"(link = "logit")), color = "black") +
#  geom_point(alpha = .1, size = 1, color = "darkgrey") +
  geom_hline(yintercept = 0, lty=2) +
  xlab("Internet penetration (ln)") + ylab("Probability of protest occurrence") +
  theme_bw() +
  geom_rug(sides="b") + coord_cartesian(ylim = c(0,.16)) +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=15,face="bold"))

print(gg2)

grid.arrange(gg1,gg2, ncol=1)

pdf("ch6/cityyear_incidence_slope.pdf")
gg1
dev.off()

pdf("ch6/cityyear_incidence_slope_poly.pdf")
gg2
dev.off()

rm(gg1,gg2)























### Country examples, Russia, Iran, Egypt ###

cnt.examples <- subset(cityyear.data, cowcode==365 | cowcode==630 | cowcode==651)

cnt.names <- c(`365` = "Russia",
  `630` = "Iran",
  `651` = "Egypt")

table(cnt.examples$protests,cnt.examples$cowcode)

russia.iran.egypt <- ggplot(cnt.examples, aes(x = log(numactivepc_l+1), y = protests, group = cowcode)) +
    geom_smooth(method = "glm", se = TRUE, formula = "y ~ log(x+1)", method.args = list(family = "binomial"(link = "logit")), color = "black") +
#  geom_point(alpha = 0.1, size = 1, color = "darkgrey") +
  facet_wrap(~cowcode, labeller = as_labeller(cnt.names), ncol=1) + 
  xlab("Internet penetration (ln)") + ylab("Probability of protest occurrence") +
  geom_hline(yintercept = 0, lty=2) +
  theme_bw() +
  geom_rug(sides="b") + coord_cartesian(ylim = c(0,.65)) +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=15,face="bold"))

russia.iran.egypt

pdf("ch6/cityyear_incidence_slope_russia_iran_egypt.pdf")
russia.iran.egypt
dev.off()




















# Betas for Internet penetration

samples <- posterior_samples(ch6.cityyear.logit, pars = "b_lognumactivepc_lP1")
samples$chain <- NA
samples[1:250, 2] <- 1
samples[251:500, 2] <- 2
samples[501:750, 2] <- 3
table(samples$chain)

samples$iter <- ave(samples$b_lognumactivepc_lP1, samples$chain,  FUN = seq_along)

samples$chain <- as.factor(samples$chain)

traceplot <- ggplot(samples, aes(y = b_lognumactivepc_lP1, x = iter, linetype = chain)) +
  geom_line() + 
  xlab("Iterations") + ylab("Betas") +
  theme_bw() +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=15,face="bold"))

traceplot

densityplot <- ggplot(samples, aes(x=b_lognumactivepc_lP1, linetype = chain)) +
  geom_density() + 
  xlab("Betas") + ylab("Density") +
  theme_bw() +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=15,face="bold"))

densityplot

pdf("ch6/cityyear_betas_incidence_internet_bayes_ml.pdf", onefile=FALSE, width=8, height=8)
grid.arrange(traceplot, densityplot, ncol=1)
dev.off()

rm(samples, densityplot, traceplot)


