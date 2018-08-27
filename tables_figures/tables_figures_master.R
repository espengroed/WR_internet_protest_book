# August 6, 2018
# Replication files for generating tables and figures in Chapters 6-9 in "The Internet and Political Protest in Autocracies"

rm(list = ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

if (!require("pacman")) install.packages("pacman")
pacman::p_load(brms, ggplot2, gridExtra, xtable, memisc, MASS, pscl, stargazer, foreign, lme4, data.table, reshape2, survival, plyr, rms, arm)

### NOTE 1: Because an incompatibility in ggplot and R, the PDFs do not properly generate when sourcing syntax files in a master syntax. 
### Running the individual syntax files will solve the problem

### NOTE 2: For some of the varying slope models, brms gives an error message saying that the models have not converged. 
### This occurs because some estimates have zero variance, which leads to NA values. These can be ignored. 
### For more on this, see http://discourse.mc-stan.org/t/convergence-failure-maybe-in-brms/4148/8

### Chapter 6

load(file = "ch6/chapter6_results.RData")

# Tables

source("ch6/ch6_tables.R")

# Figures

source("ch6/ch6_slopes.R")
source("ch6/ch6_coefplots.R")
source("ch6/ch6_margef.R")
source("ch6/ch6_randomslope.R")

rm(list = ls())

# Chapter 7

load(file = "ch7/chapter7_results.RData")

# Tables

source("ch7/ch7_tables.R")

# Figures

source("ch7/ch7_slopes.R")
source("ch7/ch7_coefplots.R")
source("ch7/ch7_margef.R")
source("ch7/ch7_randomslope.R")

rm(list = ls())

# Chapter 8
load(file = "ch8/chapter8_results.RData")

# Tables

source("ch8/ch8_tables.R")

# Figures

source("ch8/ch8_descr_send.R")
source("ch8/ch8_coefplots.R")
source("ch8/ch8_margef.R")
source("ch8/ch8_margef_time.R")
source("ch8/ch8_margef_repr.R")
source("ch8/ch8_margef_distance.R")
source("ch8/ch8_randomslope.R")

rm(list = ls())

# Chapter 8 robustness
load(file = "ch8_robustness/chapter8_results_robustness.RData")

# Tables

source("ch8_robustness/ch8_tables.R")

# Figures

source("ch8_robustness/ch8_descr_send.R")
source("ch8_robustness/ch8_coefplots.R")
source("ch8_robustness/ch8_margef.R")
source("ch8_robustness/ch8_margef_time.R")
source("ch8_robustness/ch8_margef_repr.R")
source("ch8_robustness/ch8_margef_distance.R")

rm(list = ls())

# Chapter 9

load(file = "ch9/chapter9_results.RData")

# Tables

source("ch9/ch9_tables.R")

# Figures

source("ch9/ch9_slopes.R")
source("ch9/ch9_margef.R")

rm(list = ls())