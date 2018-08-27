# August 6, 2018
# Replication files for analysis in Chapters 6-9 in "The Internet and Political Protest in Autocracies"

rm(list = ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

require(brms)

# Reading in data

file.exists("replication_data.RData")
load("replication_data.RData")

####################################################
# Macro parameters = 750 effective samples
####################################################

n.thin <- 2
n.chains <- 3
n.iter <- 1000
n.burnin <- 500
n.cores <- 3 # Use only this setting if you can spare three cores
#n.cores <- 1 # Local

(500/2) * 3

print(n.thin)
print(n.chains)
print(n.iter)
print(n.burnin)
print(n.cores)

####################################################
# Macro parameters for test run
####################################################

#n.thin <- 1
#n.chains <- 1
#n.iter <- 300
#n.burnin <- 150
#n.cores <- 1

####################################################
# Sampling 1000 obs for test run
####################################################

#cityyear.data <- cityyear.data[sample(nrow(cityyear.data), 1000), ]
#cityweek.data.persistence <- cityweek.data.persistence[sample(nrow(cityweek.data.persistence), 1000), ]
#cityweek.data.diffusion <- cityweek.data.diffusion[sample(nrow(cityweek.data.diffusion), 1000), ]

####################################################
# CHAPTER 6
####################################################

#Run file ch6_brms.R for Chapter 6 analysis

####################################################
# CHAPTER 7
####################################################

#Run file ch7_brms.R for Chapter 7 analysis

####################################################
# CHAPTER 8
####################################################

#Run file ch8_brms.R for Chapter 8 analysis

#Run file ch8_brms_robustness.R for Chapter 8 robustness analysis

####################################################
# CHAPTER 9
####################################################

#Run file ch9_brms.R for Chapter 9 analysis

