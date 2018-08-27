print("##################")
print("Chapter 9 models")
print("##################")

Sys.time()
ptm <- proc.time() # Start the clock

ch9.cityyear.assoc <- brm(protests ~ log(numactivepc_l+1)*v2x_frassoc_thick_l +
                          protests_l + protests_slag_l + nightlights_5k_pc_l_centered + lspop2008_5k_centered + factor(featurecode_mod2) + cw +
                          (1 | cowcode) + (1 | geonameid) + (1 | year),
                       data = cityyear.data, 
                       family = bernoulli(link = "logit"),
                       thin=n.thin, 
                       chains=n.chains,
                       iter = n.iter, 
                       warmup = n.burnin,
                       control = list(adapt_delta = 0.95),
                       cores = n.cores,
                       save_all_pars = TRUE)

print(ch9.cityyear.assoc, digits=4)

save.image(file = "chapter9_results.RData")

ptm2 <- proc.time()
print((ptm2[3] - ptm[3])/60) # Stop the clock



Sys.time()
ptm <- proc.time() # Start the clock

ch9.cityweek.persistence.assoc <- brm(protests ~ log(numactivepc_l+1)*v2x_frassoc_thick_l +
                            log(timesinceprotest) + log(timesinceprotestelsewhere_persistence) + nightlights_5k_pc_l_centered + lspop2008_5k_centered + factor(featurecode_mod) + cw +
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

print(ch9.cityweek.persistence.assoc, digits=4)

save.image(file = "chapter9_results.RData")

ptm2 <- proc.time()
print((ptm2[3] - ptm[3])/60) # Stop the clock


Sys.time()
ptm <- proc.time() # Start the clock

ch9.cityweek.diffusion.assoc <- brm(protests ~ log(numactivepc_l+1) + lastprotestelsewhere_lnumactivepc_max*v2x_frassoc_thick_l + log(timesinceprotestelsewhere) + 
                                      log(timesinceprotest_diffusion) + nightlights_5k_pc_l_centered + lspop2008_5k_centered + factor(featurecode_mod) + cw +
                                      (1 | cowcode) + (1 | geonameid) + (1 | year), 
                                    data = cityweek.data.diffusion,
                                    family = bernoulli(link = "logit"),
                                    thin=n.thin, 
                                    chains=n.chains,
                                    iter = n.iter, 
                                    warmup = n.burnin,
                                    control = list(adapt_delta = 0.95),
                                    cores = n.cores,
                                    save_all_pars = TRUE)

print(ch9.cityweek.diffusion.assoc, digits=4)

save.image(file = "chapter9_results.RData")

ptm2 <- proc.time()
print((ptm2[3] - ptm[3])/60) # Stop the clock


rm(list=setdiff(ls(), c("cityyear.data", "cityweek.data.persistence", "cityweek.data.diffusion","n.thin","n.chains","n.iter","n.burnin","n.cores")))