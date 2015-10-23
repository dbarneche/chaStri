library(R2jags)
library(plyr)
source('paths.R')
rm(list=ls())
source('R/functions-analyses.R')

#############################
# BAYESIAN POISSON GLM FOR 
# BOOTSTRAPPED ABUNDANCE DATA
#############################
abun            <-  readFile('data/density_raw.csv')
densityMCMC     <-  bootstrapDensity(abun)
meanParameters  <-  densityMCMC$meanParameters
MCMCParameters  <-  densityMCMC$MCMCParameters

rmFcts('R/functions-analyses.R')
save.image('output/RDatafiles/densityAnalysis.RData')
