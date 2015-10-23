library(R2jags)
library(plyr)
source('paths.R')
rm(list=ls())
source('R/functions-analyses.R')

##############
# RNA/DNA DATA
##############
rna                <-  readFile('data/rna_raw.csv')
rna$site           <-  tolower(gsub('[[:digit:].]', '', rna$sample))
rna$sample         <-  as.numeric(gsub('[[:alpha:].]', '', rna$sample))
rna$ln_r_d_ratio   <-  log(rna$rna_dna_ratio)

#####################
# RNA/DNA RATIO ANOVA
#####################
rnaAnova  <-  aov(ln_r_d_ratio ~ site, data=rna)
rnaTest   <-  TukeyHSD(rnaAnova)$site

rmFcts('R/functions-analyses.R')
save.image('output/RDatafiles/rnaDnaAnalysis.RData')
