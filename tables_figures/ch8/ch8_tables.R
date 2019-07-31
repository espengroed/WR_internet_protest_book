
####################################################
# Cityweek diffusion
####################################################

print(ch8.cityweek.bare, digits=4)

samples <- posterior_samples(ch8.cityweek.bare)
names(samples)

samples <- samples[,c("b_Intercept",
                      "b_lognumactivepc_lP1",
                      "b_lastprotestelsewhere_lnumactivepc_max",
                      "b_logtimesinceprotestelsewhere",
                      "b_logtimesinceprotest_diffusion",
                      "b_nightlights_5k_pc_l_centered",
                      "b_lspop2008_5k_centered",
                      "b_factorfeaturecode_modPPLA",
                      "b_factorfeaturecode_modPPLC",
                      "b_cw",
                      "sd_cowcode__Intercept",
                      "sd_geonameid__Intercept",
                      "sd_year__Intercept")]

meanBeta <- apply(samples,2,mean)
hHPD <- apply(samples,2,function(x) quantile(x, .975))
lHPD <- apply(samples,2,function(x) quantile(x, .025))

results <-t(rbind(meanBeta,lHPD,hHPD))
row.names(results) <- c("Intercept",
                        "Internet penetration, receiving",
                        "Internet penetration, sending",
                        "Weeks since protest, sending (ln)",
                        "Weeks since protest, receiving (ln)",
                        "Night lights pc, receiving (ln)",
                        "Population, receiving (ln)",
                        "Regional capital, receiving",
                        "National capital, receiving",
                        "Civil war, receiving",
                        "SD country (cowcode)",
                        "SD city (geonameid)",
                        "SD year")

colnames(results) <- c("Mean Beta", "2.5%", "97.5%")

print(xtable(results, digits=4, caption="The effect of sending Internet penetration on anti-regime protest diffusion (city-week resolution, logistic regression).", label="ch8:logitbare"), 
      hline.after = c(-1,0,nrow(results)-3,nrow(results)),
      file="ch8/diffusion_cityweek_bayes_ml_bare.tex")

####################################################
# Cityweek diffusion
####################################################

print(ch8.cityweek.bare.yearslope, digits=4)

ps <- posterior_samples(ch8.cityweek.bare.yearslope, pars=c("b_lastprotestelsewhere_lnumactivepc_max"))
ps.above <- ps[ps$b_lastprotestelsewhere_lnumactivepc_max>=0, ]
ps.below <- ps[ps$b_lastprotestelsewhere_lnumactivepc_max<0, ]

13/750

rm(ps, ps.below, ps.above)

samples <- posterior_samples(ch8.cityweek.bare.yearslope)
names(samples)

samples <- samples[,c("b_Intercept",
                      "b_lognumactivepc_lP1",
                      "b_lastprotestelsewhere_lnumactivepc_max",
                      "b_logtimesinceprotestelsewhere",
                      "b_logtimesinceprotest_diffusion",
                      "b_nightlights_5k_pc_l_centered",
                      "b_lspop2008_5k_centered",
                      "b_factorfeaturecode_modPPLA",
                      "b_factorfeaturecode_modPPLC",
                      "b_cw",
                      "sd_cowcode__Intercept",
                      "sd_geonameid__Intercept",
                      "sd_year__Intercept",
                      "sd_year__lastprotestelsewhere_lnumactivepc_max")]

meanBeta <- apply(samples,2,mean)
hHPD <- apply(samples,2,function(x) quantile(x, .975))
lHPD <- apply(samples,2,function(x) quantile(x, .025))

results <-t(rbind(meanBeta,lHPD,hHPD))
row.names(results) <- c("Intercept",
                        "Internet penetration, receiving",
                        "Internet penetration, sending",
                        "Weeks since protest, sending (ln)",
                        "Weeks since protest, receiving (ln)",
                        "Night lights pc, receiving (ln)",
                        "Population, receiving (ln)",
                        "Regional capital, receiving",
                        "National capital, receiving",
                        "Civil war, receiving",
                        "SD country (cowcode)",
                        "SD city (geonameid)",
                        "SD year",
                        "SD Internet penetration, sending")

colnames(results) <- c("Mean Beta", "2.5%", "97.5%")

print(xtable(results, digits=4, caption="The effect of sending Internet penetration on anti-regime protest diffusion, varying slope by year (city-week resolution, logistic regression).", label="ch8:logitbare_varslope"), 
      hline.after = c(-1,0,nrow(results)-4,nrow(results)),
      file="ch8/diffusion_cityweek_bayes_ml_bare_yearslope.tex")


####################################################
# Cityweek diffusion
####################################################

print(ch8.cityweek.twoway, digits=4)

samples <- posterior_samples(ch8.cityweek.twoway)
names(samples)

samples <- samples[,c("b_Intercept",
                      "b_lognumactivepc_lP1",
                      "b_lastprotestelsewhere_lnumactivepc_max",
                      "b_logtimesinceprotestelsewhere",
                      "b_logtimesinceprotest_diffusion",
                      "b_nightlights_5k_pc_l_centered",
                      "b_lspop2008_5k_centered",
                      "b_factorfeaturecode_modPPLA",
                      "b_factorfeaturecode_modPPLC",
                      "b_cw",
                      "b_lognumactivepc_lP1:lastprotestelsewhere_lnumactivepc_max",
                      "sd_cowcode__Intercept",
                      "sd_geonameid__Intercept",
                      "sd_year__Intercept")]

meanBeta <- apply(samples,2,mean)
hHPD <- apply(samples,2,function(x) quantile(x, .975))
lHPD <- apply(samples,2,function(x) quantile(x, .025))

results <-t(rbind(meanBeta,lHPD,hHPD))
row.names(results) <- c("Intercept",
                        "Internet penetration, receiving",
                        "Internet penetration, sending",
                        "Weeks since protest, sending (ln)",
                        "Weeks since protest, receiving (ln)",
                        "Night lights pc, receiving (ln)",
                        "Population, receiving (ln)",
                        "Regional capital, receiving",
                        "National capital, receiving",
                        "Civil war, receiving",
                        "Internet, receiving * Internet, sending",
                        "SD country (cowcode)",
                        "SD city (geonameid)",
                        "SD year")

colnames(results) <- c("Mean Beta", "2.5%", "97.5%")

print(xtable(results, digits=4, caption="The effect of sending Internet penetration on anti-regime protest diffusion, interaction with receiving Internet penetration (city-week resolution, logistic regression).", label="ch8:logittwoway"), 
      hline.after = c(-1,0,nrow(results)-3,nrow(results)),
      file="ch8/diffusion_cityweek_bayes_ml_twoway.tex")


####################################################
# Cityweek diffusion
####################################################

print(ch8.cityweek.threeway, digits=4)

samples <- posterior_samples(ch8.cityweek.threeway)
names(samples)

samples <- samples[,c("b_Intercept",
                      "b_lognumactivepc_lP1",
                      "b_lastprotestelsewhere_lnumactivepc_max",
                      "b_logtimesinceprotestelsewhere",
                      "b_logtimesinceprotest_diffusion",
                      "b_nightlights_5k_pc_l_centered",
                      "b_lspop2008_5k_centered",
                      "b_factorfeaturecode_modPPLA",
                      "b_factorfeaturecode_modPPLC",
                      "b_cw",
                      "b_lastprotestelsewhere_lnumactivepc_max:logtimesinceprotestelsewhere",
                      "sd_cowcode__Intercept",
                      "sd_geonameid__Intercept",
                      "sd_year__Intercept")]

meanBeta <- apply(samples,2,mean)
hHPD <- apply(samples,2,function(x) quantile(x, .975))
lHPD <- apply(samples,2,function(x) quantile(x, .025))

results <-t(rbind(meanBeta,lHPD,hHPD))
row.names(results) <- c("Intercept",
                        "Internet penetration, receiving",
                        "Internet penetration, sending",
                        "Weeks since protest, sending (ln)",
                        "Weeks since protest, receiving (ln)",
                        "Night lights pc, receiving (ln)",
                        "Population, receiving (ln)",
                        "Regional capital, receiving",
                        "National capital, receiving",
                        "Civil war, receiving",
                        "Internet, send. * W. s. protest, send.",
                        "SD country (cowcode)",
                        "SD city (geonameid)",
                        "SD year")

colnames(results) <- c("Mean Beta", "2.5%", "97.5%")

print(xtable(results, digits=4, caption="The effect of sending Internet penetration on anti-regime protest diffusion, interaction with logged weeks since protest in sending city (city-week resolution, logistic regression).", label="ch8:logitthreeway"), 
      hline.after = c(-1,0,nrow(results)-3,nrow(results)),
      file="ch8/diffusion_cityweek_bayes_ml_threeway.tex")


####################################################
# Cityweek diffusion
####################################################

print(ch8.cityweek.threeway.repr, digits=4)

samples <- posterior_samples(ch8.cityweek.threeway.repr)
names(samples)

samples <- samples[,c("b_Intercept",
                      "b_lognumactivepc_lP1",
                      "b_lastprotestelsewhere_lnumactivepc_max",
                      "b_logtimesinceprotestelsewhere",
                      "b_lastprotestelsewhere_maxlnumactivepc_repressed",
                      "b_logtimesinceprotest_diffusion",
                      "b_nightlights_5k_pc_l_centered",
                      "b_lspop2008_5k_centered",
                      "b_factorfeaturecode_modPPLA",
                      "b_factorfeaturecode_modPPLC",
                      "b_cw",
                      "b_lastprotestelsewhere_lnumactivepc_max:logtimesinceprotestelsewhere",
                      "b_lastprotestelsewhere_lnumactivepc_max:lastprotestelsewhere_maxlnumactivepc_repressed",
                      "b_logtimesinceprotestelsewhere:lastprotestelsewhere_maxlnumactivepc_repressed",
                      "b_lastprotestelsewhere_lnumactivepc_max:logtimesinceprotestelsewhere:lastprotestelsewhere_maxlnumactivepc_repressed",
                      "sd_cowcode__Intercept",
                      "sd_geonameid__Intercept",
                      "sd_year__Intercept")]

meanBeta <- apply(samples,2,mean)
hHPD <- apply(samples,2,function(x) quantile(x, .975))
lHPD <- apply(samples,2,function(x) quantile(x, .025))

results <-t(rbind(meanBeta,lHPD,hHPD))
row.names(results) <- c("Intercept",
                        "Internet penetration, receiving",
                        "Internet penetration, sending",
                        "Weeks since protest, sending (ln)",
                        "Last protest repressed, sending",
                        "Weeks since protest, receiving (ln)",
                        "Night lights pc, receiving (ln)",
                        "Population, receiving (ln)",
                        "Regional capital, receiving",
                        "National capital, receiving",
                        "Civil war, receiving",
                        "Internet, send. * W. s. protest, send.",
                        "Internet, send. * Repr., send.",
                        "W. s. protest, send. * Repr., send.",
                        "Internet, send. * W. s. protest, send. * Repr., send.",
                        "SD country (cowcode)",
                        "SD city (geonameid)",
                        "SD year")

colnames(results) <- c("Mean Beta", "2.5%", "97.5%")

print(xtable(results, digits=4, caption="The effect of sending Internet penetration on anti-regime protest diffusion, interaction with logged weeks since protest and repression in sending city (city-week resolution, logistic regression).", label="ch8:logitthreeway_repr"), 
      hline.after = c(-1,0,nrow(results)-3,nrow(results)),
      file="ch8/diffusion_cityweek_bayes_ml_threeway_repr.tex")



####################################################
# Cityweek diffusion - distance
####################################################

print(ch8.cityweek.distance, digits=4)

samples <- posterior_samples(ch8.cityweek.distance)
names(samples)

samples <- samples[,c("b_Intercept",
                      "b_lognumactivepc_lP1",
                      "b_lastprotestelsewhere_lnumactivepc_max",
                      "b_loglastprotestelsewhere_maxlnumactivepc_distance",
                      "b_logtimesinceprotestelsewhere",
                      "b_logtimesinceprotest_diffusion",
                      "b_nightlights_5k_pc_l_centered",
                      "b_lspop2008_5k_centered",
                      "b_factorfeaturecode_modPPLA",
                      "b_factorfeaturecode_modPPLC",
                      "b_cw",
                      "b_lastprotestelsewhere_lnumactivepc_max:loglastprotestelsewhere_maxlnumactivepc_distance",
                      "sd_cowcode__Intercept",
                      "sd_geonameid__Intercept",
                      "sd_year__Intercept")]

meanBeta <- apply(samples,2,mean)
hHPD <- apply(samples,2,function(x) quantile(x, .975))
lHPD <- apply(samples,2,function(x) quantile(x, .025))

results <-t(rbind(meanBeta,lHPD,hHPD))
row.names(results) <- c("Intercept",
                        "Internet penetration, receiving",
                        "Internet penetration, sending",
                        "Distance, sending and receiving (ln)",
                        "Weeks since protest, sending (ln)",
                        "Weeks since protest, receiving (ln)",
                        "Night lights pc, receiving (ln)",
                        "Population, receiving (ln)",
                        "Regional capital, receiving",
                        "National capital, receiving",
                        "Civil war, receiving",
                        "Internet, send. * Distance",
                        "SD country (cowcode)",
                        "SD city (geonameid)",
                        "SD year")

colnames(results) <- c("Mean Beta", "2.5%", "97.5%")

print(xtable(results, digits=4, caption="The effect of sending Internet penetration on anti-regime protest diffusion, interaction with logged distance between sending and receiving cities (city-week resolution, logistic regression).", label="ch8:logitdistance"), 
      hline.after = c(-1,0,nrow(results)-3,nrow(results)),
      file="ch8/diffusion_cityweek_bayes_ml_distance.tex")
