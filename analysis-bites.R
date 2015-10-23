library(MASS)
library(plyr)
source('paths.R')
rm(list=ls())
source('R/functions-analyses.R')

###################
# LOAD CLEANED DATA
###################
load('output/RDatafiles/bitesData.RData')
summaryBites  <-  ddply(bites, .(site, id), function(x){
	x    <-  x[, -c(1:8), drop=FALSE]
	data.frame(bites=sum(x, na.rm=TRUE), stringsAsFactors=FALSE)
})

#################
# MODEL SELECTION
#################
bitesNegBin    <-  glm.nb(bites ~ site - 1, data=summaryBites)
bitesPoisson   <-  glm(bites ~ site - 1, data=summaryBites, family=poisson)

#######################
# NEGATIVE BINOMIAL GLM
# FOR BITE RATE DATA
# (BETTER FIT)
#######################
bitesGLM  <-  signifGLM(sites = unique(bites$site), data=summaryBites, y = 'bites', mod = 'glm.nb(data[[y]] ~ data$site)')

#########
# TABLE 2
#########
table2Effects  <-  drawTable2(summaryBites, 'Estimate')
table2pvals    <-  drawTable2(summaryBites, 'pVal')
write.csv(table2Effects, 'output/data/table2Effects.csv')
write.csv(table2pvals, 'output/data/table2pvals.csv')

rm(list=ls()[!(ls() %in% c('summaryBites', 'bitesNegBin', 'bitesPoisson', 'bitesGLM'))])
save.image('output/RDatafiles/bitesAnalysis.RData')
