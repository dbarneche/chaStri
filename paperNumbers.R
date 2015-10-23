# individuals per site for bites
rm(list=ls())
load('output/RDatafiles/bitesData.RData')
indsPerSite  <-  tapply(bites$id, bites$site, function(x)length(unique(x)))
sum(indsPerSite)

# relationship between bites and temperature
rm(list=ls())
source('table1.R')
load('output/RDatafiles/bitesAnalysis.RData')
source('R/functions-figures.R')
meanBites  <-  tapply(summaryBites$bites, summaryBites$site, mean)
meanSst    <-  sst$meanSst[-8] # exclude trindade
summary(lm(meanBites~meanSst))
responseFigure  <-  function() {
	par(omi=rep(1, 4), cex=1)
	plot(meanBites~meanSst, xlab='Sea Surface Temperature (Celsius)', ylab='Foraging rates / 3 min', las=1)
}
to.dev(responseFigure(), png, fig.path('responseFigure.png'), width=7, height=7, units='in', res=300)

# individuals per site for cover
rm(list=ls())
load('output/RDatafiles/coverData.RData')
indsPerSite  <-  tapply(newCover$fishID, newCover$site, function(x)length(unique(x)))
sum(indsPerSite)

# individuals per site for diet
rm(list=ls())
load('output/RDatafiles/dietData.RData')
indsPerSite  <-  tapply(diet$individual, diet$local, function(x)length(unique(x)))
sum(indsPerSite)

# individuals per site for rna/dna
rm(list=ls())
load('output/RDatafiles/rnaDnaAnalysis.RData')
indsPerSite  <-  tapply(rna$sample, rna$site, function(x)length(unique(x)))
sum(indsPerSite)
