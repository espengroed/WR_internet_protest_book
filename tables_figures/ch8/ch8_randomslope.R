print(ch8.cityweek.bare.yearslope, digits=4)

coefs <- fixef(ch8.cityweek.bare.yearslope)
coefs <- coefs[2:10, 1]

beta <- coef(ch8.cityweek.bare.yearslope)
ranef <- as.data.frame(beta$year)

df <- data.frame(matrix(vector(), nrow=0, ncol=5))
colnames(df) <- c("slope", "intercept", "year","x","y")

rowcounter <- 1

for (i in c(2005:2012)){
  
  #i <- 2005  
  
  print(paste("The year is", i))
  x <- unique(cityweek.data.diffusion$lastprotestelsewhere_lnumactivepc_max[cityweek.data.diffusion$year==i])
  x <- sort(sample(x))
  print(max(x))
  y <- rep(c(0),length(x))
  slope <- rep(ranef$Estimate.lastprotestelsewhere_lnumactivepc_max[rowcounter],length(x))
  intercept <- rep(ranef$Estimate.Intercept[rowcounter],length(x))
  year <- rep(c(i),length(x))
  
  df.temp <- data.frame(cbind(slope,intercept,year,x,y))
  df <- rbind(df, df.temp)
  
  rowcounter <- rowcounter + 1 
}

df$value <- df$intercept + (df$slope * df$x)
df$prob <-  exp(df$value)/(1 + exp(df$value))

varyingslope.prob <- ggplot(df, aes(x = x, y = prob, group = year)) +
  geom_line() +
  #  geom_point(alpha = 0.1, size = 1, color = "darkgrey") +
  facet_wrap(~year) + 
  xlab("Internet penetration, sending") + ylab("Probability of protest") +
  #  scale_y_continuous(limits=c(-0.1,.4))+
  theme_bw() +
  geom_rug(sides="b") +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=15,face="bold"))

varyingslope.prob

varyingslope.beta <- ggplot(df, aes(x = x, y = value, group = year)) +
  geom_line() +
  #  geom_point(alpha = 0.1, size = 1, color = "darkgrey") +
  facet_wrap(~year) + 
  xlab("Internet penetration, sending") + ylab("Probability of protest") +
  geom_hline(yintercept = 0, lty=2) +
  theme_bw() +
  geom_rug(sides="b") +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=15,face="bold"))

varyingslope.beta

pdf("ch8/cityweek_diffusion_varyingslope_prob.pdf")
varyingslope.prob
dev.off()

rm(list=setdiff(ls(), c("cityweek.data.diffusion", 
                        "ch8.cityweek.bare", 
                        "ch8.cityweek.bare.yearslope",
                        "ch8.cityweek.distance",
                        "ch8.cityweek.threeway", 
                        "ch8.cityweek.threeway.repr", 
                        "ch8.cityweek.twoway")))