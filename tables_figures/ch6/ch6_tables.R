####################################################
# Cityyear occurrence, logit
####################################################

# Creating regression table

print(ch6.cityyear.logit, digits=4)

samples <- posterior_samples(ch6.cityyear.logit)
names(samples)
samples <- samples[,c("b_Intercept",
                      "b_lognumactivepc_lP1",
                      "b_protests_l",
                      "b_protests_slag_l",
                      "b_nightlights_5k_pc_l_centered",
                      "b_lspop2008_5k_centered",
                      "b_factorfeaturecode_mod21",
                      "b_factorfeaturecode_mod22",
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
                        "Protest in same city (t-1)",
                        "Protest in different city (t-1)",
                        "Night lights pc (ln)",
                        "Population (ln)",
                        "Regional capital",
                        "National capital",
                        "Civil war",
                        "SD country (cowcode)",
                        "SD city (geonameid)",
                        "SD year")

colnames(results) <- c("Mean Beta", "2.5%", "97.5%")

print(xtable(results, digits=4, caption="The effect of local Internet penetration on anti-regime protest occurrence (city-year resolution, logistic regression).", label="ch5:logit"),
      hline.after = c(-1,0,nrow(results)-3,nrow(results)),
      file="ch6/cityyear_incidence_logit_bayes_ml.tex")

rm(samples, results, lHPD, hHPD, meanBeta)
























####################################################
# Cityyear number of protests, ols
####################################################

# Creating regression table

print(ch6.cityyear.ols, digits=4)

samples <- posterior_samples(ch6.cityyear.ols)
names(samples)
samples <- samples[,c("b_Intercept",
                      "b_lognumactivepc_lP1",
                      "b_lognrprotests_lP1",
                      "b_protests_slag_l",
                      "b_nightlights_5k_pc_l_centered",
                      "b_lspop2008_5k_centered",
                      "b_factorfeaturecode_mod21",
                      "b_factorfeaturecode_mod22",
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
                        "Number of protests t-1",
                        "Protest in different city (t-1)",
                        "Night lights pc (ln)",
                        "Population (ln)",
                        "Regional capital",
                        "National capital",
                        "Civil war",
                        "SD country (cowcode)",
                        "SD city (geonameid)",
                        "SD year")

colnames(results) <- c("Mean Beta", "2.5%", "97.5%")

print(xtable(results, digits=4, caption="The effect of local Internet penetration on number of anti-regime protests (city-year resolution, OLS).", label="ch5:robols"),
      hline.after = c(-1,0,nrow(results)-3,nrow(results)), 
      file="ch6/cityyear_nrprotests_ols_bayes_ml.tex")

rm(samples, results, lHPD, hHPD, meanBeta)



















####################################################
# Cityyear number of protests, NBreg
####################################################

# Creating regression table

print(ch6.cityyear.nb, digits=4)

samples <- posterior_samples(ch6.cityyear.nb)
names(samples)
samples <- samples[,c("b_Intercept",
                      "b_lognumactivepc_lP1",
                      "b_protests_l",
                      "b_protests_slag_l",
                      "b_nightlights_5k_pc_l_centered",
                      "b_lspop2008_5k_centered",
                      "b_factorfeaturecode_mod21",
                      "b_factorfeaturecode_mod22",
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
                        "Protest in same city (t-1)",
                        "Protest in different city (t-1)",
                        "Night lights pc (ln)",
                        "Population (ln)",
                        "Regional capital",
                        "National capital",
                        "Civil war",
                        "SD country (cowcode)",
                        "SD city (geonameid)",
                        "SD year")

colnames(results) <- c("Mean Beta", "2.5%", "97.5%")


print(xtable(results, digits=4, caption="The effect of local Internet penetration on number of anti-regime protests (city-year resolution, negative binomial regression).", label="ch5:robnb"),
      hline.after = c(-1,0,nrow(results)-3,nrow(results)), 
      file="ch6/cityyear_nrprotests_nbreg_bayes_ml.tex")

rm(samples, results, lHPD, hHPD, meanBeta)




















####################################################
# Cityyear protest occurrence, varying slope by year
####################################################

# Creating regression table

print(ch6.cityyear.rob1, digits=4)

samples <- posterior_samples(ch6.cityyear.rob1)
names(samples)
samples <- samples[,c("b_Intercept",
                      "b_lognumactivepc_lP1",
                      "b_protests_l",
                      "b_protests_slag_l",
                      "b_nightlights_5k_pc_l_centered",
                      "b_lspop2008_5k_centered",
                      "b_factorfeaturecode_mod21",
                      "b_factorfeaturecode_mod22",
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
                        "Protest in same city (t-1)",
                        "Protest in different city (t-1)",
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


print(xtable(results, digits=4, caption="The effect of local Internet penetration on number of anti-regime protests (city-year resolution, logistic regression, local Internet penetration slope varies by year).", label="ch5:robyearslope"),
      hline.after = c(-1,0,nrow(results)-4,nrow(results)),
      file="ch6/cityyear_incidence_logit_bayes_ml_yearslope.tex")

rm(samples, results, lHPD, hHPD, meanBeta)

















####################################################
# Cityyear protest occurrence, dichotomous internet penetration
####################################################

# Creating regression table

print(ch6.cityyear.rob2, digits=4)

samples <- posterior_samples(ch6.cityyear.rob2)
names(samples)
samples <- samples[,c("b_Intercept",
                      "b_numactive_dich_l",
                      "b_protests_l",
                      "b_protests_slag_l",
                      "b_nightlights_5k_pc_l_centered",
                      "b_lspop2008_5k_centered",
                      "b_factorfeaturecode_mod21",
                      "b_factorfeaturecode_mod22",
                      "b_cw",
                      "sd_cowcode__Intercept",
                      "sd_geonameid__Intercept",
                      "sd_year__Intercept")]

meanBeta <- apply(samples,2,mean)
hHPD <- apply(samples,2,function(x) quantile(x, .975))
lHPD <- apply(samples,2,function(x) quantile(x, .025))

results <-t(rbind(meanBeta,lHPD,hHPD))
row.names(results) <- c("Intercept",
                        "Internet dummy",
                        "Protest in same city (t-1)",
                        "Protest in different city (t-1)",
                        "Night lights pc (ln)",
                        "Population (ln)",
                        "Regional capital",
                        "National capital",
                        "Civil war",
                        "SD country (cowcode)",
                        "SD city (geonameid)",
                        "SD year")

colnames(results) <- c("Mean Beta", "2.5%", "97.5%")


print(xtable(results, digits=4, caption="The effect of local Internet penetration on number of anti-regime protests  (city-year resolution, logistic regression, Internet penetration dummy).", label="ch5:robintdum"),
      hline.after = c(-1,0,nrow(results)-3,nrow(results)),
      file="ch6/cityyear_incidence_logit_bayes_ml_intdum.tex")

rm(samples, results, lHPD, hHPD, meanBeta)


####################################################
# Cityyear protest occurrence, multinomial logit
####################################################

# Creating regression table

print(ch6.cityyear.rob3, digits=4)

samples <- posterior_samples(ch6.cityyear.rob3)
names(samples)
samples <- samples[,c("b_mu1_Intercept",
                      "b_mu1_lognumactivepc_lP1",
                      "b_mu1_factormaxscope_l1",
                      "b_mu1_factormaxscope_l2",
                      "b_mu1_protests_slag_l",
                      "b_mu1_nightlights_5k_pc_l_centered",
                      "b_mu1_lspop2008_5k_centered",
                      "b_mu1_factorfeaturecode_mod21",
                      "b_mu1_factorfeaturecode_mod22",
                      "b_mu1_cw",
                      "sd_cowcode__mu1_Intercept",
                      "sd_geonameid__mu1_Intercept",
                      "sd_year__mu1_Intercept",
                      "b_mu2_Intercept",
                      "b_mu2_lognumactivepc_lP1",
                      "b_mu2_factormaxscope_l1",
                      "b_mu2_factormaxscope_l2",                         
                      "b_mu2_protests_slag_l",
                      "b_mu2_nightlights_5k_pc_l_centered",
                      "b_mu2_lspop2008_5k_centered",
                      "b_mu2_factorfeaturecode_mod21",
                      "b_mu2_factorfeaturecode_mod22",
                      "b_mu2_cw",
                      "sd_cowcode__mu2_Intercept",
                      "sd_geonameid__mu2_Intercept",
                      "sd_year__mu2_Intercept")]

meanBeta <- apply(samples,2,mean)
hHPD <- apply(samples,2,function(x) quantile(x, .975))
lHPD <- apply(samples,2,function(x) quantile(x, .025))

results <-t(rbind(meanBeta,lHPD,hHPD))
row.names(results) <- c("1 Intercept",
                        "1 Internet penetration",
                        "1 Local Protest in same city (t-1)",
                        "1 National Protest in same city (t-1)",
                        "1 Protest in different city (t-1)",
                        "1 Night lights pc (ln)",
                        "1 Population (ln)",
                        "1 Regional capital",
                        "1 National capital",
                        "1 Civil war",
                        "1 SD country (cowcode)",
                        "1 SD city (geonameid)",
                        "1 SD year",
                        "2 Intercept",
                        "2 Internet penetration",
                        "2 Local Protest in same city (t-1)",
                        "2 National Protest in same city (t-1)",
                        "2 Protest in different city (t-1)",
                        "2 Night lights pc (ln)",
                        "2 Population (ln)",
                        "2 Regional capital",
                        "2 National capital",
                        "2 Civil war",
                        "2 SD country (cowcode)",
                        "2 SD city (geonameid)",
                        "2 SD year")

colnames(results) <- c("Mean Beta", "2.5%", "97.5%")


print(xtable(results, digits=4, caption="The effect of local Internet penetration on local and national anti-regime protests (city-year resolution, multinomial logistic regression).", label="ch5:roblocalnational"), 
      file="ch6/cityyear_incidence_mlogit_bayes_ml_localnational.tex")

rm(samples, results, lHPD, hHPD, meanBeta)



#LPM

require(stargazer)

stargazer(ch6.cityweek.fixed, ch6.cityweek.fixed2, title="The effect of local Internet penetration on anti-regime protest
(city-year resolution, linear probability models with fixed effects by city and year).", align=TRUE, dep.var.labels=c("Protest occurrence", "Number of protests"), omit=c("geonameid","year"),
          covariate.labels=c("Internet penetration", 
                             "Protest in same city (t-1)", "Protest in different city (t-1)", 
                             "Night lights pc (ln)", "Population (ln)", "Regional capital", "National capital", 
                             "Civil war"), star.cutoffs=c(0.1,0.05,0.01), keep.stat = c("n"), label = "lpm",
          out="ch6/cityyear_lpm.tex")
















#### ONSET 1

print(ch6.onset1, digits=4)

samples <- posterior_samples(ch6.onset1)
names(samples)
samples <- samples[,c("b_Intercept",
                      "b_lognumactivepc_lP1",
                      "b_protests_slag_l",
                      "b_nightlights_5k_pc_l_centered",
                      "b_lspop2008_5k_centered",
                      "b_factorfeaturecode_mod21",
                      "b_factorfeaturecode_mod22",
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
                        "Protest in different city (t-1)",
                        "Night lights pc (ln)",
                        "Population (ln)",
                        "Regional capital",
                        "National capital",
                        "Civil war",
                        "SD country (cowcode)",
                        "SD city (geonameid)",
                        "SD year")

colnames(results) <- c("Mean Beta", "2.5%", "97.5%")

print(xtable(results, digits=4, caption="The effect of local Internet penetration on anti-regime protest onset (city-year resolution, logistic regression).", label="ch5:logit_onset1"),
      hline.after = c(-1,0,nrow(results)-3,nrow(results)),
      file="ch6/cityyear_onset1_logit_bayes_ml.tex")

rm(samples, results, lHPD, hHPD, meanBeta)




#### ONSET 2

print(ch6.onset2, digits=4)

samples <- posterior_samples(ch6.onset2)
names(samples)
samples <- samples[,c("b_Intercept",
                      "b_lognumactivepc_lP1",
                      "b_protests_slag_l",
                      "b_nightlights_5k_pc_l_centered",
                      "b_lspop2008_5k_centered",
                      "b_factorfeaturecode_mod21",
                      "b_factorfeaturecode_mod22",
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
                        "Protest in different city (t-1)",
                        "Night lights pc (ln)",
                        "Population (ln)",
                        "Regional capital",
                        "National capital",
                        "Civil war",
                        "SD country (cowcode)",
                        "SD city (geonameid)",
                        "SD year")

colnames(results) <- c("Mean Beta", "2.5%", "97.5%")


print(xtable(results, digits=4, caption="The effect of local Internet penetration on anti-regime protest onset (city-year resolution, logistic regression).", label="ch5:logit_onset2"),
      hline.after = c(-1,0,nrow(results)-3,nrow(results)),
      file="ch6/cityyear_onset2_logit_bayes_ml.tex")

rm(samples, results, lHPD, hHPD, meanBeta)

