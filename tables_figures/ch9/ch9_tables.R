
print(ch9.cityyear.assoc, digits=4)

samples <- posterior_samples(ch9.cityyear.assoc)
names(samples)
samples <- samples[,c("b_Intercept",
                      "b_lognumactivepc_lP1",
                      "b_v2x_frassoc_thick_l",
                      "b_protests_l",
                      "b_protests_slag_l",
                      "b_nightlights_5k_pc_l_centered",
                      "b_lspop2008_5k_centered",
                      "b_factorfeaturecode_mod21",
                      "b_factorfeaturecode_mod22",
                      "b_cw",
                      "b_lognumactivepc_lP1:v2x_frassoc_thick_l",
                      "sd_cowcode__Intercept",
                      "sd_geonameid__Intercept",
                      "sd_year__Intercept")]

meanBeta <- apply(samples,2,mean)
hHPD <- apply(samples,2,function(x) quantile(x, .975))
lHPD <- apply(samples,2,function(x) quantile(x, .025))

results <-t(rbind(meanBeta,lHPD,hHPD))
row.names(results) <- c("Intercept",
                        "Internet penetration (ln)",
                        "Freedom of association",
                        "Protest in same city (t-1)",
                        "Protest in different city (t-1)",
                        "Night lights pc (ln)",
                        "Population (ln)",
                        "Regional capital",
                        "National capital",
                        "Civil war",
                        "Internet penetration (ln) * Freedom of association",
                        "SD country (cowcode)",
                        "SD city (geonameid)",
                        "SD year")

colnames(results) <- c("Mean Beta", "2.5%", "97.5%")

print(xtable(results, digits=4, caption="The conditional effect of Internet penetration and freedom of association on anti-regime protest occurrence (city-year resolution, logistic regression).", label="ch8:cityyear_incidence_bayes_assoc"), 
      hline.after = c(-1,0,nrow(results)-3,nrow(results)), 
      file="ch9/cityyear_incidence_bayes_assoc.tex")

rm(samples, results, lHPD, hHPD, meanBeta)
























print(ch9.cityweek.persistence.assoc, digits=4)

samples <- posterior_samples(ch9.cityweek.persistence.assoc)
names(samples)
samples <- samples[,c("b_Intercept",
                      "b_lognumactivepc_lP1",
                      "b_v2x_frassoc_thick_l",
                      "b_logtimesinceprotest",
                      "b_logtimesinceprotestelsewhere_persistence",
                      "b_nightlights_5k_pc_l_centered",
                      "b_lspop2008_5k_centered",
                      "b_factorfeaturecode_modPPLA",
                      "b_factorfeaturecode_modPPLC",
                      "b_cw",
                      "b_lognumactivepc_lP1:v2x_frassoc_thick_l",
                      "sd_cowcode__Intercept",
                      "sd_geonameid__Intercept",
                      "sd_year__Intercept")]


meanBeta <- apply(samples,2,mean)
hHPD <- apply(samples,2,function(x) quantile(x, .975))
lHPD <- apply(samples,2,function(x) quantile(x, .025))

results <-t(rbind(meanBeta,lHPD,hHPD))
row.names(results) <- c("Intercept",  
                        "Internet penetration (ln)",
                        "Freedom of association",
                        "Weeks since protest in same city (ln)",
                        "Weeks since protest in different city (ln)",
                        "Night lights pc (ln)",
                        "Population (ln)",
                        "Regional capital",
                        "National capital",
                        "Civil war",
                        "Internet penetration (ln) * Freedom of association",
                        "SD country (cowcode)",
                        "SD city (geonameid)",
                        "SD year")

colnames(results) <- c("Mean Beta", "2.5%", "97.5%")

print(xtable(results, digits=4, caption="The conditional effect of local Internet penetration and freedom of association on anti-regime protest persistence (city-week resolution, logistic regression).", label="ch8:cityweek_persistence_bayes_assoc"), 
      hline.after = c(-1,0,nrow(results)-3,nrow(results)), 
      file="ch9/cityweek_persistence_bayes_assoc.tex")


rm(samples, results, lHPD, hHPD, meanBeta)
























print(ch9.cityweek.diffusion.assoc, digits=4)

samples <- posterior_samples(ch9.cityweek.diffusion.assoc)
names(samples)
samples <- samples[,c("b_Intercept",
                      "b_lognumactivepc_lP1",
                      "b_lastprotestelsewhere_lnumactivepc_max",
                      "b_v2x_frassoc_thick_l",
                      "b_logtimesinceprotestelsewhere",
                      "b_logtimesinceprotest_diffusion",
                      "b_nightlights_5k_pc_l_centered",
                      "b_lspop2008_5k_centered",
                      "b_factorfeaturecode_modPPLA",
                      "b_factorfeaturecode_modPPLC",
                      "b_cw",
                      "b_lastprotestelsewhere_lnumactivepc_max:v2x_frassoc_thick_l",
                      "sd_cowcode__Intercept",
                      "sd_geonameid__Intercept",
                      "sd_year__Intercept")]


meanBeta <- apply(samples,2,mean)
hHPD <- apply(samples,2,function(x) quantile(x, .975))
lHPD <- apply(samples,2,function(x) quantile(x, .025))

results <-t(rbind(meanBeta,lHPD,hHPD))
row.names(results) <- c("Intercept",
                        "Internet penetration, receiving (ln)",
                        "Internet penetration, sending (ln)",
                        "Freedom of association",
                        "Weeks since protest, sending (ln)",
                        "Weeks since protest, receiving (ln)",
                        "Night lights pc, receiving (ln)",
                        "Population, receiving (ln)",
                        "Regional capital, receiving",
                        "National capital, receiving",
                        "Civil war, receiving",
                        "Internet, sending, * Freedom of association",
                        "SD country (cowcode)",
                        "SD city (geonameid)",
                        "SD year")

colnames(results) <- c("Mean Beta", "2.5%", "97.5%")

print(xtable(results, digits=4, caption="The conditional effect of local Internet penetration and freedom of association on anti-regime protest diffusion (city-week resolution, logistic regression).", label="ch8:cityweek_diffusion_bayes_assoc"), 
      hline.after = c(-1,0,nrow(results)-3,nrow(results)), 
      file="ch9/cityweek_diffusion_bayes_assoc.tex")

rm(samples, results, lHPD, hHPD, meanBeta)
