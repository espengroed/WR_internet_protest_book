# Coefficient plots

coefs <- fixef(ch7.cityweek.bare)
coefs <- coefs[2:9, 1]
ses <- fixef(ch7.cityweek.bare)
ses <- ses[2:9, 2]

ylo <- coefs - 1.96*ses
yhi <- coefs + 1.96*ses
names <- c("Internet penetration","Weeks since protest in same city (ln)","Weeks since protest in different city (ln)","Nightlights pc (ln)","Population (ln)","Regional capital","National capital","Civil war")
names <- factor(names, levels = rev(c("Internet penetration","Weeks since protest in same city (ln)","Weeks since protest in different city (ln)","Nightlights pc (ln)","Population (ln)","Regional capital","National capital","Civil war")))

dfplot <- data.frame(names, coefs, ses, ylo, yhi)

p <- ggplot(dfplot, aes(x=names, y=coefs, ymin=ylo, ymax=yhi)) + 
  geom_pointrange(colour=ifelse(ylo < 0 & yhi > 0, "grey", "black")) + theme_bw() + 
  geom_hline(yintercept = 0, lty=2) + coord_flip() + xlab('') + ylab('') +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=15,face="bold"))

p

pdf("ch7/coefplot_persistence_bayes_ml_bare.pdf", height=4, width=6)
par(mar=c(3.5,1,1,1))
p
dev.off()

rm(coefs, ses, names, ylo, yhi, p, dfplot)
























# Coefficient plots

print(ch7.cityweek.bare.report, digits=4)

coefs <- fixef(ch7.cityweek.bare.report)
coefs <- coefs[2:10, 1]
ses <- fixef(ch7.cityweek.bare.report)
ses <- ses[2:10, 2]

ylo <- coefs - 1.96*ses
yhi <- coefs + 1.96*ses
names <- c("Internet penetration","Weeks since protest in same city (ln)","Report ratio (ln)","Weeks since protest in different city (ln)","Night lights pc (ln)","Population (ln)","Regional capital","National capital","Civil war")
names <- factor(names, levels = rev(c("Internet penetration","Weeks since protest in same city (ln)","Report ratio (ln)","Weeks since protest in different city (ln)","Night lights pc (ln)","Population (ln)","Regional capital","National capital","Civil war")))

dfplot <- data.frame(names, coefs, ses, ylo, yhi)

p <- ggplot(dfplot, aes(x=names, y=coefs, ymin=ylo, ymax=yhi)) + 
  geom_pointrange(colour=ifelse(ylo < 0 & yhi > 0, "grey", "black")) + 
  theme_bw() + geom_hline(yintercept = 0, lty=2) + coord_flip() + xlab('') + ylab('') +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=15,face="bold"))

p

pdf("ch7/coefplot_persistence_bayes_ml_bare_report.pdf", height=4, width=6)
par(mar=c(3.5,1,1,1))
p
dev.off()

rm(coefs, ses, names, ylo, yhi, p, dfplot)
