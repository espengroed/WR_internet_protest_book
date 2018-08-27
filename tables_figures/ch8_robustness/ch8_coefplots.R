# Coefficient plots

coefs <- fixef(ch8.cityweek.bare)
coefs <- coefs[2:11, 1]
ses <- fixef(ch8.cityweek.bare)
ses <- ses[2:11, 2]
ylo <- coefs - 1.96*ses
yhi <- coefs + 1.96*ses
names <- c("Internet penetration, receiving (ln)","Internet penetration, sending (ln)","Weeks since protest, sending (ln)","Weeks since protest, receiving (ln)","Night lights pc, receiving (ln)",
           "Population, receiving (ln)","Regional capital, receiving","National capital, receiving","Civil war", "Report ratio, receiving (ln)")
names <- factor(names, levels = rev(c("Internet penetration, receiving (ln)","Internet penetration, sending (ln)","Weeks since protest, sending (ln)","Weeks since protest, receiving (ln)","Night lights pc, receiving (ln)",
                                      "Population, receiving (ln)","Regional capital, receiving","National capital, receiving","Civil war", "Report ratio, receiving (ln)")))


dfplot <- data.frame(names, coefs, ses, ylo, yhi)

p <- ggplot(dfplot, aes(x=names, y=coefs, ymin=ylo, ymax=yhi)) + 
  geom_pointrange(colour=ifelse(ylo < 0 & yhi > 0, "grey", "black")) + 
  theme_bw() + geom_hline(yintercept = 0, lty=2) + coord_flip() + xlab('') + ylab('') +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=15,face="bold"))

p

pdf("ch8_robustness/coefplot_diffusion_bayes_ml_bare_robustness.pdf", height=4, width=6)
par(mar=c(3.5,1,1,1))
p
dev.off()

rm(coefs, ses, names, ylo, yhi, p, dfplot)
