library(survival)
library(car)
source('paths.R')
rm(list=ls())
source('R/functions-analyses.R')

#################
# LOAD BITES DATA 
#################
load('output/RDatafiles/bitesData.RData')

######################################
# RUNNING MODEL; CLUSTER ARGUMENT 
# TO ESTIMATE ROBUST STANDARD ERRORS
# AND THEN, CONTROLLING FOR 
# PSEUDO-REPLICATION WITHIN INDIVIDUAL
######################################
modelSelectivity  <-  coxph(Surv(step, bite) ~ food * site, data=bitesPerItem)
modelAnova        <-  Anova(modelSelectivity, type=3) # all terms are significant
modelSelectivity  <-  coxph(Surv(step, bite) ~ food * site + cluster(id), data=bitesPerItem)

# predicted selection estimates by food/site 
sites     <-  levels(as.factor(bitesPerItem$site))
food      <-  levels(as.factor(bitesPerItem$food))
results   <-  expand.grid(food=food, site=sites, stringsAsFactors=FALSE)
predSmp   <-  predict(modelSelectivity, results, se.fit=TRUE, type='risk', reference='sample')
results   <-  data.frame(results, fit=predSmp[[1]], se=predSmp[[2]], stringsAsFactors=FALSE)

write.csv(results, 'output/data/selectivityAnalysis.csv')
rm(list=ls()[!(ls() %in% c('modelSelectivity', 'results', 'modelAnova'))])
save.image('output/RDatafiles/selectivityAnalysis.RData')
