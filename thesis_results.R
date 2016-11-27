# Code to obtain results for thesis
# Jessica Kim
# Fall 2016

setwd("/Users/jsk474/Google Drive/QMSS/QMSS-Fall2016/Thesis/data_chs")
load("felonies_chs_sales.RData")

############# IDENTIFY GENTRIFIABLE NEIGHBORHOODS ############# 

Criteria are:
  \indent Those baseline conditions I use to identify 'gentrifiable' neighborhoods are median household income and median property sales values in the lowest quantile of both distributions across neighborhoods. 

############# IDENTIFY GENTRIFIED NEIGHBORHOODS ############# 

Gentrified neighborhoods are ones that experience an increase in average income and education level that is greater than the median of increases across gentrifiable neighborhoods.\\




############# FIT BAYESIAN MODEL ############# 
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# Define MCMC parameters 
niter <- 1E4   # definitely overkill, but good for comparison
nchains <- 4