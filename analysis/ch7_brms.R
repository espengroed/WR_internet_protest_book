print("##################")
print("Chapter 7 models")
print("##################")

Sys.time()
ptm <- proc.time() # Start the clock

ch7.cityweek.bare <- brm(protests ~ log(numactivepc_l+1) + log(timesinceprotest) + 
                      log(timesinceprotestelsewhere_persistence) + nightlights_5k_pc_l_centered + lspop2008_5k_centered + factor(featurecode_mod) + cw +
                      (1 | cowcode) + (1 | geonameid) + (1 | year), 
                    data = cityweek.data.persistence,
                    family = bernoulli(link = "logit"),
                    thin=n.thin, 
                    chains=n.chains,
                    iter = n.iter, 
                    warmup = n.burnin,
                    control = list(adapt_delta = 0.95),
                    cores = n.cores,
                    save_all_pars = TRUE)

print(ch7.cityweek.bare, digits=4)

save.image(file = "chapter7_results.RData")

ptm2 <- proc.time()
print((ptm2[3] - ptm[3])/60) # Stop the clock


Sys.time()
ptm <- proc.time() # Start the clock

ch7.cityweek.bare.yearslope <- brm(protests ~ log(numactivepc_l+1) + log(timesinceprotest) + 
                           log(timesinceprotestelsewhere_persistence) + nightlights_5k_pc_l_centered + lspop2008_5k_centered + factor(featurecode_mod) + cw +
                           (1 | cowcode) + (1 | geonameid) + (1 + log(numactivepc_l+1) | year), 
                         data = cityweek.data.persistence,
                         family = bernoulli(link = "logit"),
                         thin=n.thin, 
                         chains=n.chains,
                         iter = n.iter, 
                         warmup = n.burnin,
                         control = list(adapt_delta = 0.95),
                         cores = n.cores,
                         save_all_pars = TRUE)

print(ch7.cityweek.bare.yearslope, digits=4)

save.image(file = "chapter7_results.RData")

ptm2 <- proc.time()
print((ptm2[3] - ptm[3])/60) # Stop the clock



Sys.time()
ptm <- proc.time() # Start the clock

ch7.cityweek.bare.report <- brm(protests ~ log(numactivepc_l+1) + log(timesinceprotest) + report_ratio_ag_l_centered +
                                  log(timesinceprotestelsewhere_persistence) + nightlights_5k_pc_l_centered + lspop2008_5k_centered + factor(featurecode_mod) + cw +
                                  (1 | cowcode) + (1 | geonameid) + (1 | year), 
                                data = cityweek.data.persistence,
                                family = bernoulli(link = "logit"),
                                thin=n.thin, 
                                chains=n.chains,
                                iter = n.iter, 
                                warmup = n.burnin,
                                control = list(adapt_delta = 0.95),
                                cores = n.cores,
                                save_all_pars = TRUE)

print(ch7.cityweek.bare.report, digits=4)

save.image(file = "chapter7_results.RData")

ptm2 <- proc.time()
print((ptm2[3] - ptm[3])/60) # Stop the clock


Sys.time()
ptm <- proc.time() # Start the clock

ch7.cityweek <- brm(protests ~ log(numactivepc_l+1)*log(timesinceprotest) + 
                      log(timesinceprotestelsewhere_persistence) + nightlights_5k_pc_l_centered + lspop2008_5k_centered + factor(featurecode_mod) + cw +
                         (1 | cowcode) + (1 | geonameid) + (1 | year), 
                       data = cityweek.data.persistence,
                       family = bernoulli(link = "logit"),
                       thin=n.thin, 
                       chains=n.chains,
                       iter = n.iter, 
                       warmup = n.burnin,
                       control = list(adapt_delta = 0.95),
                       cores = n.cores,
                    save_all_pars = TRUE)

print(ch7.cityweek, digits=4)

save.image(file = "chapter7_results.RData")

ptm2 <- proc.time()
print((ptm2[3] - ptm[3])/60) # Stop the clock


  
Sys.time()
ptm <- proc.time() # Start the clock

ch7.cityweek.report <- brm(protests ~ log(numactivepc_l+1)*log(timesinceprotest) + report_ratio_ag_l_centered +
                      log(timesinceprotestelsewhere_persistence) + nightlights_5k_pc_l_centered + lspop2008_5k_centered + factor(featurecode_mod) + cw +
                      (1 | cowcode) + (1 | geonameid) + (1 | year), 
                    data = cityweek.data.persistence,
                    family = bernoulli(link = "logit"),
                    thin=n.thin, 
                    chains=n.chains,
                    iter = n.iter, 
                    warmup = n.burnin,
                    control = list(adapt_delta = 0.95),
                    cores = n.cores,
                    save_all_pars = TRUE)

print(ch7.cityweek.report, digits=4)

save.image(file = "chapter7_results.RData")

ptm2 <- proc.time()
print((ptm2[3] - ptm[3])/60) # Stop the clock

Sys.time()
ptm <- proc.time() # Start the clock

ch7.cityweek.repr <- brm(protests ~ log(numactivepc_l+1)*log(timesinceprotest)*lastprotestrepressed + 
                           log(timesinceprotestelsewhere_persistence) + nightlights_5k_pc_l_centered + lspop2008_5k_centered + factor(featurecode_mod) + cw +
                      (1 | cowcode) + (1 | geonameid) + (1 | year), 
                    data = cityweek.data.persistence,
                    family = bernoulli(link = "logit"),
                    thin=n.thin, 
                    chains=n.chains,
                    iter = n.iter, 
                    warmup = n.burnin,
                    control = list(adapt_delta = 0.95),
                    cores = n.cores,
                    save_all_pars = TRUE)

print(ch7.cityweek.repr, digits=4)

save.image(file = "chapter7_results.RData")

ptm2 <- proc.time()
print((ptm2[3] - ptm[3])/60) # Stop the clock

Sys.time()
ptm <- proc.time() # Start the clock

ch7.cityweek.repr.report <- brm(protests ~ log(numactivepc_l+1)*log(timesinceprotest)*lastprotestrepressed + report_ratio_ag_l_centered +
                           log(timesinceprotestelsewhere_persistence) + nightlights_5k_pc_l_centered + lspop2008_5k_centered + factor(featurecode_mod) + cw +
                           (1 | cowcode) + (1 | geonameid) + (1 | year), 
                         data = cityweek.data.persistence,
                         family = bernoulli(link = "logit"),
                         thin=n.thin, 
                         chains=n.chains,
                         iter = n.iter, 
                         warmup = n.burnin,
                         control = list(adapt_delta = 0.95),
                         cores = n.cores,
                         save_all_pars = TRUE)

print(ch7.cityweek.repr.report, digits=4)

save.image(file = "chapter7_results.RData")

ptm2 <- proc.time()
print((ptm2[3] - ptm[3])/60) # Stop the clock


rm(list=setdiff(ls(), c("cityyear.data", "cityweek.data.persistence", "cityweek.data.diffusion","n.thin","n.chains","n.iter","n.burnin","n.cores")))

