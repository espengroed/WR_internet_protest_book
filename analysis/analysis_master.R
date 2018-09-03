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
n.cores <- 3 # Use only this setting if you can spare multiple cores
#n.cores <- 1 # Local

print(n.thin)
print(n.chains)
print(n.iter)
print(n.burnin)
print(n.cores)


####################################################
# CHAPTER 6
####################################################

#Run file ch6_brms.R for Chapter 6 analysis

source("ch6_brms.R")

####################################################
# CHAPTER 7
####################################################

#Run file ch7_brms.R for Chapter 7 analysis
source("ch7_brms.R")

####################################################
# CHAPTER 8
####################################################

#Run file ch8_brms.R for Chapter 8 analysis
source("ch8_brms.R")

#Run file ch8_brms_robustness.R for Chapter 8 robustness analysis
source("ch8_brms_robustness.R")

####################################################
# CHAPTER 9
####################################################

#Run file ch9_brms.R for Chapter 9 analysis
source("ch9_brms.R")
