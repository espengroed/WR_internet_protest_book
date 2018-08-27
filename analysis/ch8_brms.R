print("##################")
print("Chapter 8 models")
print("##################")



Sys.time()
ptm <- proc.time() # Start the clock
  
ch8.cityweek.bare <- brm(protests ~ log(numactivepc_l+1) + lastprotestelsewhere_lnumactivepc_max + log(timesinceprotestelsewhere) + 
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
  
print(ch8.cityweek.bare, digits=4)
  
save.image(file = "chapter8_results.RData")
  
ptm2 <- proc.time()
print((ptm2[3] - ptm[3])/60) # Stop the clock
  

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
Sys.time()
ptm <- proc.time() # Start the clock
  
ch8.cityweek.bare.yearslope <- brm(protests ~ log(numactivepc_l+1) + lastprotestelsewhere_lnumactivepc_max + log(timesinceprotestelsewhere) + 
                                       log(timesinceprotest_diffusion) + nightlights_5k_pc_l_centered + lspop2008_5k_centered + factor(featurecode_mod) + cw +   
                                       (1 | cowcode) + (1 | geonameid) + (1 + lastprotestelsewhere_lnumactivepc_max | year), 
                                     data = cityweek.data.diffusion,
                                     family = bernoulli(link = "logit"),
                                     thin=n.thin, 
                                     chains=n.chains,
                                     iter = n.iter, 
                                     warmup = n.burnin,
                                     control = list(adapt_delta = 0.95),
                                     cores = n.cores,
                                   save_all_pars = TRUE)
  
print(ch8.cityweek.bare.yearslope, digits=4)
  
save.image(file = "chapter8_results.RData")
  
ptm2 <- proc.time()
print((ptm2[3] - ptm[3])/60) # Stop the clock
  
  
  
  
  
  
  
  
  
  

  
  
  
  
  
  
  
  
  
  
  
  

  


  
Sys.time()
ptm <- proc.time() # Start the clock
  
ch8.cityweek.threeway <- brm(protests ~ log(numactivepc_l+1) + lastprotestelsewhere_lnumactivepc_max*log(timesinceprotestelsewhere) + 
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
  
print(ch8.cityweek.threeway, digits=4)
  
save.image(file = "chapter8_results.RData")
  
ptm2 <- proc.time()
print((ptm2[3] - ptm[3])/60) # Stop the clock












Sys.time()
ptm <- proc.time() # Start the clock

ch8.cityweek.threeway.repr <- brm(protests ~ log(numactivepc_l+1) + lastprotestelsewhere_lnumactivepc_max*log(timesinceprotestelsewhere)*lastprotestelsewhere_maxlnumactivepc_repressed + 
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

print(ch8.cityweek.threeway.repr, digits=4)

save.image(file = "chapter8_results.RData")

ptm2 <- proc.time()
print((ptm2[3] - ptm[3])/60) # Stop the clock






















  
Sys.time()
ptm <- proc.time() # Start the clock
  
ch8.cityweek.distance <- brm(protests ~ log(numactivepc_l+1) + lastprotestelsewhere_lnumactivepc_max*log(lastprotestelsewhere_maxlnumactivepc_distance) + log(timesinceprotestelsewhere) + 
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
  
print(ch8.cityweek.distance, digits=4)
  
save.image(file = "chapter8_results.RData")

ptm2 <- proc.time()
print((ptm2[3] - ptm[3])/60) # Stop the clock
  
  
  
Sys.time()
ptm <- proc.time() # Start the clock
  
ch8.cityweek.twoway <- brm(protests ~ log(numactivepc_l+1)*lastprotestelsewhere_lnumactivepc_max + log(timesinceprotestelsewhere) + 
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
  
print(ch8.cityweek.twoway, digits=4)
  
save.image(file = "chapter8_results.RData")
  
ptm2 <- proc.time()
print((ptm2[3] - ptm[3])/60) # Stop the clock

rm(list=setdiff(ls(), c("cityyear.data", "cityweek.data.persistence", "cityweek.data.diffusion","n.thin","n.chains","n.iter","n.burnin","n.cores")))