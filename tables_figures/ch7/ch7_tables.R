####################################################
# Cityweek persistence
####################################################

samples <- posterior_samples(ch7.cityweek.bare)
names(samples)
samples <- samples[,c("b_Intercept",
                      "b_lognumactivepc_lP1",
                      "b_logtimesinceprotest",
                      "b_logtimesinceprotestelsewhere_persistence",
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
                        "Internet penetration",
                        "Weeks since protest in same city (ln)",
                        "Weeks since protest in different city (ln)",
                        "Night lights pc (ln)",
                        "Population (ln)",
                        "Regional capital",
                        "National capital",
                        "Civil war",
                        "SD country (cowcode)",
                        "SD city (geonameid)",
                        "SD year")

colnames(results) <- c("Mean Beta", "2.5%", "97.5%")

print(xtable(results, digits=4, caption="The effect of Internet penetration on anti-regime protest persistence (city-week resolution, logistic regression).", label="ch6:barelogit"), 
      hline.after = c(-1,0,nrow(results)-3,nrow(results)),
      file="ch7/persistence_bayes_ml_bare.tex")

#Betas for Internet penetration

#pdf("/Users/roed/Dropbox/Apps/ShareLaTeX/MMAD_Book/06_dynamics/betas_persistence_internet_bayes_ml.pdf", onefile=FALSE, width=8, height=4)
#plot(ch7.cityweek, pars = c("b_lognumactivepc_lP1"), parameters = NA)
#dev.off()

rm(results, lHPD, hHPD, meanBeta, samples)


























####################################################
# Cityweek persistence
####################################################

samples <- posterior_samples(ch7.cityweek.bare.yearslope)
names(samples)
samples <- samples[,c("b_Intercept",
                      "b_lognumactivepc_lP1",
                      "b_logtimesinceprotest",
                      "b_logtimesinceprotestelsewhere_persistence",
                      "b_nightlights_5k_pc_l_centered",
                      "b_lspop2008_5k_centered",
                      "b_factorfeaturecode_modPPLA",
                      "b_factorfeaturecode_modPPLC",
                      "b_cw",
                      "sd_cowcode__Intercept",
                      "sd_geonameid__Intercept",
                      "sd_year__Intercept",
                      "sd_year__lognumactivepc_lP1")]

meanBeta <- apply(samples,2,mean)
hHPD <- apply(samples,2,function(x) quantile(x, .975))
lHPD <- apply(samples,2,function(x) quantile(x, .025))

results <-t(rbind(meanBeta,lHPD,hHPD))
row.names(results) <- c("Intercept",
                        "Internet penetration",
                        "Weeks since protest in same city (ln)",
                        "Weeks since protest in different city (ln)",
                        "Night lights pc (ln)",
                        "Population (ln)",
                        "Regional capital",
                        "National capital",
                        "Civil war",
                        "SD country (cowcode)",
                        "SD city (geonameid)",
                        "SD year",
                        "SD Internet penetration:year")

colnames(results) <- c("Mean Beta", "2.5%", "97.5%")

print(xtable(results, digits=4, caption="The effect of Internet penetration on anti-regime protest persistence, varying slopes by year (city-week resolution, logistic regression).", label="ch6:barelogityearslope"), 
      hline.after = c(-1,0,nrow(results)-4,nrow(results)),
      file="ch7/persistence_bayes_ml_bare_yearslope.tex")

#Betas for Internet penetration

#pdf("/Users/roed/Dropbox/Apps/ShareLaTeX/MMAD_Book/06_dynamics/betas_persistence_internet_bayes_ml.pdf", onefile=FALSE, width=8, height=4)
#plot(ch7.cityweek, pars = c("b_lognumactivepc_lP1"), parameters = NA)
#dev.off()

rm(results, lHPD, hHPD, meanBeta, samples)



























####################################################
# Cityweek persistence
####################################################

samples <- posterior_samples(ch7.cityweek)
names(samples)
samples <- samples[,c("b_Intercept",
                      "b_lognumactivepc_lP1",
                      "b_logtimesinceprotest",
                      "b_logtimesinceprotestelsewhere_persistence",
                      "b_nightlights_5k_pc_l_centered",
                      "b_lspop2008_5k_centered",
                      "b_factorfeaturecode_modPPLA",
                      "b_factorfeaturecode_modPPLC",
                      "b_cw",               
                      "b_lognumactivepc_lP1:logtimesinceprotest",
                      "sd_cowcode__Intercept",
                      "sd_geonameid__Intercept",
                      "sd_year__Intercept")]

meanBeta <- apply(samples,2,mean)
hHPD <- apply(samples,2,function(x) quantile(x, .975))
lHPD <- apply(samples,2,function(x) quantile(x, .025))

results <-t(rbind(meanBeta,lHPD,hHPD))
row.names(results) <- c("Intercept",
                        "Internet penetration",
                        "Weeks since protest in same city (ln)",
                        "Weeks since protest in different city (ln)",
                        "Night lights pc (ln)",
                        "Population (ln)",
                        "Regional capital",
                        "National capital",
                        "Civil war",
                        "Internet * W. s. protest same city (ln)",
                        "SD country (cowcode)",
                        "SD city (geonameid)",
                        "SD year")

colnames(results) <- c("Mean Beta", "2.5%", "97.5%")

print(xtable(results, digits=4, caption="The effect of Internet penetration on anti-regime protest persistence, interaction with logged weeks since protest (city-week resolution, logistic regression).", label="ch6:logittimesincelogged"), 
      hline.after = c(-1,0,nrow(results)-3,nrow(results)),
      file="ch7/persistence_bayes_ml.tex")

#Betas for Internet penetration

#pdf("/Users/roed/Dropbox/Apps/ShareLaTeX/MMAD_Book/06_dynamics/betas_persistence_internet_bayes_ml.pdf", onefile=FALSE, width=8, height=4)
#plot(ch7.cityweek, pars = c("b_lognumactivepc_lP1"), parameters = NA)
#dev.off()

rm(results, lHPD, hHPD, meanBeta, samples)



















































####################################################
# Cityweek persistence and repression
####################################################

samples <- posterior_samples(ch7.cityweek.repr)
names(samples)
samples <- samples[,c("b_Intercept",
                      "b_lognumactivepc_lP1",
                      "b_logtimesinceprotest",
                      "b_lastprotestrepressed",
                      "b_logtimesinceprotestelsewhere_persistence",
                      "b_nightlights_5k_pc_l_centered",
                      "b_lspop2008_5k_centered",
                      "b_factorfeaturecode_modPPLA",
                      "b_factorfeaturecode_modPPLC",
                      "b_cw",
                      "b_lognumactivepc_lP1:logtimesinceprotest",
                      "b_lognumactivepc_lP1:lastprotestrepressed",
                      "b_logtimesinceprotest:lastprotestrepressed",
                      "b_lognumactivepc_lP1:logtimesinceprotest:lastprotestrepressed",
                      "sd_cowcode__Intercept",
                      "sd_geonameid__Intercept",
                      "sd_year__Intercept")]

meanBeta <- apply(samples,2,mean)
hHPD <- apply(samples,2,function(x) quantile(x, .975))
lHPD <- apply(samples,2,function(x) quantile(x, .025))

results <-t(rbind(meanBeta,lHPD,hHPD))
row.names(results) <- c("Intercept",
                        "Internet penetration",
                        "Weeks since protest in same city (ln)",
                        "Last protest repressed",
                        "Weeks since protest in different city (ln)",
                        "Night lights pc (ln)",
                        "Population (ln)",
                        "Regional capital",
                        "National capital",
                        "Civil war",
                        "Internet * W. s. protest same city",
                        "Internet * Repressed",
                        "W. s. protest same city * Repressed",
                        "Internet * W. s. protest same city * Repressed",
                        "SD country (cowcode)",
                        "SD city (geonameid)",
                        "SD year")

colnames(results) <- c("Mean Beta", "2.5%", "97.5%")

print(xtable(results, digits=4, caption="The effect of Internet penetration on anti-regime protest persistence, interaction with logged weeks since protest and repression (city-week resolution, logistic regression).", label="ch6:logitrepression"), 
      hline.after = c(-1,0,nrow(results)-3,nrow(results)),
      file="ch7/persistence_repression_bayes_ml.tex")

#Betas for Internet penetration

#pdf("/Users/roed/Dropbox/Apps/ShareLaTeX/MMAD_Book/06_dynamics/betas_persistence_internet_bayes_ml.pdf", onefile=FALSE, width=8, height=4)
#plot(ch7.cityweek, pars = c("b_lognumactivepc_lP1"), parameters = NA)
#dev.off()

rm(results, lHPD, hHPD, meanBeta, samples)



























## REPORT RATIO MODELS ##

####################################################
# Cityweek persistence bare (reporting)
####################################################

samples <- posterior_samples(ch7.cityweek.bare.report)
names(samples)
samples <- samples[,c("b_Intercept",
                      "b_lognumactivepc_lP1",
                      "b_logtimesinceprotest",
                      "b_report_ratio_ag_l_centered",
                      "b_logtimesinceprotestelsewhere_persistence",
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
                        "Internet penetration",
                        "Weeks since protest in same city (ln)",
                        "Report ratio (ln)",
                        "Weeks since protest in different city (ln)",
                        "Night lights pc (ln)",
                        "Population (ln)",
                        "Regional capital",
                        "National capital",
                        "Civil war",
                        "SD country (cowcode)",
                        "SD city (geonameid)",
                        "SD year")

colnames(results) <- c("Mean Beta", "2.5%", "97.5%")

print(xtable(results, digits=4, caption="The effect of Internet penetration on anti-regime protest persistence, controlling for reporting (city-week resolution, logistic regression).", label="ch6:barelogitreporting"), 
      hline.after = c(-1,0,nrow(results)-3,nrow(results)),
      file="ch7/persistence_bayes_ml_report_bare.tex")

#Betas for Internet penetration

#pdf("/Users/roed/Dropbox/Apps/ShareLaTeX/MMAD_Book/06_dynamics/betas_persistence_internet_bayes_ml_bare.pdf", onefile=FALSE, width=8, height=4)
#plot(ch7.cityweek, pars = c("b_lognumactivepc_lP1"), parameters = NA)
#dev.off()

rm(results, lHPD, hHPD, meanBeta, samples)




























####################################################
# Cityweek persistence (reporting)
####################################################

samples <- posterior_samples(ch7.cityweek.report)
names(samples)
samples <- samples[,c("b_Intercept",
                      "b_lognumactivepc_lP1",
                      "b_logtimesinceprotest",
                      "b_report_ratio_ag_l_centered",
                      "b_logtimesinceprotestelsewhere_persistence",
                      "b_nightlights_5k_pc_l_centered",
                      "b_lspop2008_5k_centered",
                      "b_factorfeaturecode_modPPLA",
                      "b_factorfeaturecode_modPPLC",
                      "b_cw",               
                      "b_lognumactivepc_lP1:logtimesinceprotest",
                      "sd_cowcode__Intercept",
                      "sd_geonameid__Intercept",
                      "sd_year__Intercept")]

meanBeta <- apply(samples,2,mean)
hHPD <- apply(samples,2,function(x) quantile(x, .975))
lHPD <- apply(samples,2,function(x) quantile(x, .025))

results <-t(rbind(meanBeta,lHPD,hHPD))
row.names(results) <- c("Intercept",
                        "Internet penetration",
                        "Weeks since protest in same city (ln)",
                        "Report ratio (ln)",
                        "Weeks since protest in different city (ln)",
                        "Night lights pc (ln)",
                        "Population (ln)",
                        "Regional capital",
                        "National capital",
                        "Civil war",
                        "Internet * W. s. protest same city",
                        "SD country (cowcode)",
                        "SD city (geonameid)",
                        "SD year")

colnames(results) <- c("Mean Beta", "2.5%", "97.5%")

print(xtable(results, digits=4, caption="The effect of Internet penetration on anti-regime protest persistence, interaction with logged weeks since protest and controlling for reporting (city-week resolution, logistic regression).", label="ch6:logittimesinceloggedreporting"), 
      hline.after = c(-1,0,nrow(results)-3,nrow(results)),
      file="ch7/persistence_bayes_ml_report.tex")

#Betas for Internet penetration

#pdf("/Users/roed/Dropbox/Apps/ShareLaTeX/MMAD_Book/06_dynamics/betas_persistence_internet_bayes_ml.pdf", onefile=FALSE, width=8, height=4)
#plot(ch7.cityweek, pars = c("b_lognumactivepc_lP1"), parameters = NA)
#dev.off()

rm(results, lHPD, hHPD, meanBeta, samples)




























####################################################
# Cityweek persistence and repression (reporting)
####################################################

samples <- posterior_samples(ch7.cityweek.repr.report)
names(samples)
samples <- samples[,c("b_Intercept",
                      "b_lognumactivepc_lP1",
                      "b_logtimesinceprotest",
                      "b_lastprotestrepressed",
                      "b_report_ratio_ag_l_centered",
                      "b_logtimesinceprotestelsewhere_persistence",
                      "b_nightlights_5k_pc_l_centered",
                      "b_lspop2008_5k_centered",
                      "b_factorfeaturecode_modPPLA",
                      "b_factorfeaturecode_modPPLC",
                      "b_cw",               
                      "b_lognumactivepc_lP1:logtimesinceprotest",
                      "b_lognumactivepc_lP1:lastprotestrepressed",
                      "b_logtimesinceprotest:lastprotestrepressed",
                      "b_lognumactivepc_lP1:logtimesinceprotest:lastprotestrepressed",
                      "sd_cowcode__Intercept",
                      "sd_geonameid__Intercept",
                      "sd_year__Intercept")]

meanBeta <- apply(samples,2,mean)
hHPD <- apply(samples,2,function(x) quantile(x, .975))
lHPD <- apply(samples,2,function(x) quantile(x, .025))

results <-t(rbind(meanBeta,lHPD,hHPD))
row.names(results) <- c("Intercept",
                        "Internet penetration",
                        "Weeks since protest in same city (ln)",
                        "Last protest repressed",
                        "Report ratio (ln)",
                        "Weeks since protest in different city (ln)",
                        "Night lights pc (ln)",
                        "Population (ln)",
                        "Regional capital",
                        "National capital",
                        "Civil war",
                        "Internet * W. s. protest same city",
                        "Internet * Repressed",
                        "W. s. protest same city * Repressed",
                        "Internet * W. s. protest same city * Repressed",
                        "SD country (cowcode)",
                        "SD city (geonameid)",
                        "SD year")

colnames(results) <- c("Mean Beta", "2.5%", "97.5%")

print(xtable(results, digits=4, caption="The effect of Internet penetration on anti-regime protest persistence, interaction with logged weeks since protest and repression , controlling for reporting (city-week resolution, logistic regression).", label="ch6:logitrepressionreporting"), 
      hline.after = c(-1,0,nrow(results)-3,nrow(results)),
      file="ch7/persistence_repression_bayes_ml_report.tex")

#Betas for Internet penetration

#pdf("/Users/roed/Dropbox/Apps/ShareLaTeX/MMAD_Book/06_dynamics/betas_persistence_internet_bayes_ml.pdf", onefile=FALSE, width=8, height=4)
#plot(ch7.cityweek, pars = c("b_lognumactivepc_lP1"), parameters = NA)
#dev.off()

rm(results, lHPD, hHPD, meanBeta, samples)

