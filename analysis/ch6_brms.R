print("##################")
print("Chapter 6 models")
print("##################")


## LOGIT

Sys.time()
ptm <- proc.time() # Start the clock

ch6.cityyear.logit <- brm(protests ~ log(numactivepc_l+1) +
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

print(ch6.cityyear.logit, digits=4)

save.image(file = "chapter6_results.RData")

ptm2 <- proc.time()
print((ptm2[3] - ptm[3])/60) # Stop the clock

## OLS

Sys.time()
ptm <- proc.time() # Start the clock

ch6.cityyear.ols <- brm(log(nrprotests+1) ~ log(numactivepc_l+1) +
                               log(nrprotests_l+1) + protests_slag_l + nightlights_5k_pc_l_centered + lspop2008_5k_centered + factor(featurecode_mod2) + cw +
                               (1 | cowcode) + (1 | geonameid) + (1 | year), 
                             data = cityyear.data, 
                             thin=n.thin, 
                             chains=n.chains,
                             iter = n.iter, 
                             warmup = n.burnin,
                             control = list(adapt_delta = 0.95),
                             cores = n.cores,
                        save_all_pars = TRUE)
print(ch6.cityyear.ols, digits=4)

save.image(file = "chapter6_results.RData")

ptm2 <- proc.time()
print((ptm2[3] - ptm[3])/60) # Stop the clock

## Negative binomial

Sys.time()
ptm <- proc.time() # Start the clock

ch6.cityyear.nb <- brm(nrprotests ~ log(numactivepc_l+1) + 
                              protests_l + protests_slag_l + nightlights_5k_pc_l_centered + lspop2008_5k_centered + factor(featurecode_mod2) + cw +
                              (1 | cowcode) + (1 | geonameid) + (1 | year), 
                            data = cityyear.data,
                            family = negbinomial(link = "log"),
                            thin=n.thin, 
                            chains=n.chains,
                            iter = n.iter, 
                            warmup = n.burnin,
                            control = list(adapt_delta = 0.95),
                            cores = n.cores,
                       save_all_pars = TRUE)
print(ch6.cityyear.nb, digits=4)

save.image(file = "chapter6_results.RData")

ptm2 <- proc.time()
print((ptm2[3] - ptm[3])/60) # Stop the clock


#### Robustness checks ####

### 1 - year varying slope

Sys.time()
ptm <- proc.time() # Start the clock

ch6.cityyear.rob1 <- brm(protests ~ log(numactivepc_l+1) +
                           protests_l + protests_slag_l + nightlights_5k_pc_l_centered + lspop2008_5k_centered + factor(featurecode_mod2) + cw +
                     (1 | cowcode) + (1 | geonameid) + (1 + log(numactivepc_l+1) | year), 
                   data = cityyear.data,
                   family = bernoulli(link = "logit"),
                   thin=n.thin, 
                   chains=n.chains,
                   iter = n.iter, 
                   warmup = n.burnin,
                   control = list(adapt_delta = 0.95),
                   cores = n.cores,
                   save_all_pars = TRUE)

print(ch6.cityyear.rob1, digits=4)

save.image(file = "chapter6_results.RData")

ptm2 <- proc.time()
print((ptm2[3] - ptm[3])/60) # Stop the clock

### 2 - include dummy of Internet penetration

Sys.time()
ptm <- proc.time() # Start the clock

ch6.cityyear.rob2 <- brm(protests ~ numactive_dich_l +
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

print(ch6.cityyear.rob2, digits=4)

save.image(file = "chapter6_results.RData")

ptm2 <- proc.time()
print((ptm2[3] - ptm[3])/60) # Stop the clock

### 4 - scope

table(cityyear.data$maxscope)
table(cityyear.data$maxscope_l)

Sys.time()
ptm <- proc.time() # Start the clock

ch6.cityyear.rob3 <- brm(maxscope ~ log(numactivepc_l+1) +
                      factor(maxscope_l) + protests_slag_l + nightlights_5k_pc_l_centered + lspop2008_5k_centered + factor(featurecode_mod2) + cw +
                      (1 | cowcode) + (1 | geonameid) + (1 | year), 
                    data = cityyear.data,
                    family = categorical(link = "logit"),
                    thin=n.thin, 
                    chains=n.chains,
                    iter = n.iter, 
                    warmup = n.burnin,
                    control = list(adapt_delta = 0.95),
                    cores = n.cores,
                    save_all_pars = TRUE)

print(ch6.cityyear.rob3, digits=4)

ptm2 <- proc.time()
print((ptm2[3] - ptm[3])/60) # Stop the clock

save.image(file = "chapter6_results.RData")

####################################################
# LPMs
####################################################

ch6.cityweek.fixed <- lm(protests ~ log(numactivepc_l+1) +
                            protests_l + protests_slag_l + nightlights_5k_pc_l_centered + lspop2008_5k_centered + factor(featurecode_mod2) + cw +
                            as.factor(geonameid) + as.factor(year), 
                          data = cityyear.data)
summary(ch6.cityweek.fixed)

save.image(file = "chapter6_results.RData")

ch6.cityweek.fixed2 <- lm(nrprotests ~ log(numactivepc_l+1) +
                            protests_l + protests_slag_l + nightlights_5k_pc_l_centered + lspop2008_5k_centered + factor(featurecode_mod2) + cw +
                            as.factor(geonameid) + as.factor(year), 
                          data = cityyear.data)
summary(ch6.cityweek.fixed2)

save.image(file = "chapter6_results.RData")


# Onset DV

cities <- unique(cityyear.data$geonameid)
df.names <- c('geonameid', 'onsetyear')

onsetdata <- data.frame(matrix(vector(), length(cities), length(df.names),
                               dimnames=list(c(), df.names)),
                        stringsAsFactors=F)

i <- 1

for (c in cities) {
  
  onsetdata[i,1] <- c
  onsetdata[i,2] <- min(cityyear.data[cityyear.data$geonameid==c & cityyear.data$protests==1, ]$year)
  
  i <- i + 1
}

cityyear.data <- merge(cityyear.data,onsetdata,by.x=c("geonameid"),by.y=c("geonameid"), all.x=T)

x <- cityyear.data$year[cityyear.data$year>cityyear.data$onsetyear]

# Estimating models

Sys.time()
ptm <- proc.time() # Start the clock

ch6.onset1 <- brm(protests ~ log(numactivepc_l+1) +
                    protests_slag_l + nightlights_5k_pc_l_centered + lspop2008_5k_centered + factor(featurecode_mod2) + cw +
                    (1 | cowcode) + (1 | geonameid) + (1 | year), 
                  data = cityyear.data[cityyear.data$protests_l!=1, ],
                  family = bernoulli(link = "logit"),
                  thin=n.thin, 
                  chains=n.chains,
                  iter = n.iter, 
                  warmup = n.burnin,
                  control = list(adapt_delta = 0.95),
                  cores = n.cores,
                  save_all_pars = TRUE)

print(ch6.onset1, digits=4)

save.image(file = "chapter6_results.RData")

ptm2 <- proc.time()
print((ptm2[3] - ptm[3])/60) # Stop the clock



Sys.time()
ptm <- proc.time() # Start the clock

ch6.onset2 <- brm(protests ~ log(numactivepc_l+1) +
                    protests_slag_l + nightlights_5k_pc_l_centered + lspop2008_5k_centered + factor(featurecode_mod2) + cw +
                    (1 | cowcode) + (1 | geonameid) + (1 | year), 
                  data = cityyear.data[cityyear.data$year<=cityyear.data$onsetyear | cityyear.data$onsetyear==Inf, ],
                  family = bernoulli(link = "logit"),
                  thin=n.thin, 
                  chains=n.chains,
                  iter = n.iter, 
                  warmup = n.burnin,
                  control = list(adapt_delta = 0.95),
                  cores = n.cores,
                  save_all_pars = TRUE)

print(ch6.onset2, digits=4)

save.image(file = "chapter6_results.RData")

ptm2 <- proc.time()
print((ptm2[3] - ptm[3])/60) # Stop the clock



rm(list=setdiff(ls(), c("cityyear.data", "cityweek.data.persistence", "cityweek.data.diffusion","n.thin","n.chains","n.iter","n.burnin","n.cores")))
