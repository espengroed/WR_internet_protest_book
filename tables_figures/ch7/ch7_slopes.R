# Overall slopes

gg1 <- ggplot(cityweek.data.persistence[cityweek.data.persistence$timesinceprotest<53, ], aes(x = log(numactivepc_l+1), y = protests)) +
  geom_smooth(method = "glm", se = TRUE, formula = "y ~ log(x+1)", method.args = list(family = "binomial"(link = "logit")), color = "black") +
#  geom_point(alpha = .1, size = 1, color = "darkgrey") +
  xlab("Internet penetration") + ylab("Probability of protest persistence") +
  geom_hline(yintercept = 0, lty=2) +
  theme_bw() +
  geom_rug(sides="b") + coord_cartesian(ylim = c(0,.25)) +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=15,face="bold"))

print(gg1)

gg2 <- ggplot(cityweek.data.persistence[cityweek.data.persistence$timesinceprotest<53, ], aes(x = log(numactivepc_l+1), y = protests)) +
  geom_smooth(method = "glm", se = TRUE, formula = "y ~ poly(log(x+1),3,raw=T)", method.args = list(family = "binomial"(link = "logit")), color = "black") +
#  geom_point(alpha = .1, size = 1, color = "darkgrey") +
  xlab("Internet penetration") + ylab("Probability of protest persistence") +
  geom_hline(yintercept = 0, lty=2) +
  theme_bw() +
  geom_rug(sides="b") + coord_cartesian(ylim = c(0,.25)) +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=15,face="bold"))

print(gg2)

grid.arrange(gg1,gg2, ncol=1)

pdf("ch7/cityweek_persistence_slope.pdf")
gg1
dev.off()

pdf("ch7/cityweek_persistence_slope_poly.pdf")
gg2
dev.off()

rm(gg1,gg2)























### Country examples, Russia, Iran, Egypt ###

cnt.examples <- subset(cityweek.data.persistence, cowcode==365 | cowcode==630 | cowcode==651)

cnt.names <- c(`365` = "Russia",
               `630` = "Iran",
               `651` = "Egypt")

russia.iran.egypt <- ggplot(cnt.examples[cnt.examples$timesinceprotest<53, ], aes(x = log(numactivepc_l+1), y = protests, group = cowcode)) +
  geom_smooth(method = "glm", se = TRUE, formula = "y ~ log(x+1)", method.args = list(family = "binomial"(link = "logit")), color = "black") +
  #  geom_point(alpha = 0.1, size = 1, color = "darkgrey") +
  facet_wrap(~cowcode, labeller = as_labeller(cnt.names), ncol=1) + 
  xlab("Internet penetration") + ylab("Probability of protest persistence") +
  geom_hline(yintercept = 0, lty=2) +
  theme_bw() +
  geom_rug(sides="b") + coord_cartesian(ylim = c(0,.65)) +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=15,face="bold"))

russia.iran.egypt

pdf("ch7/cityweek_persistence_slope_russia_iran_egypt.pdf")
russia.iran.egypt
dev.off()
