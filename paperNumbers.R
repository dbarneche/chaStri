# individuals per site for bites
rm(list=ls())
load('output/RDatafiles/bitesData.RData')
indsPerSite  <-  tapply(bites$id, bites$site, function(x)length(unique(x)))
sum(indsPerSite)
tapply(bites$prof_m, bites$site, range, na.rm=TRUE)

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

# number of photographs taken
rm(list=ls())
source('R/functions-analyses.R')
cover         <-  readFile('data/benthic_cover_raw.csv')
cover$photos  <-  apply(cover[3:ncol(cover)], 1, sum)/20
range(tapply(cover$photos, cover$site, sum))

# individuals per site for diet
rm(list=ls())
load('output/RDatafiles/dietData.RData')
indsPerSite   <-  tapply(diet$individual, diet$local, function(x)length(unique(x)))
itemsPerSite  <-  tapply(summaryDiet$table3Category, summaryDiet$local, function(x)length(unique(x)))
sum(indsPerSite)
range(itemsPerSite)
length(unique(summaryDiet$table3Category))
rawItems  <-  unique(diet[,c('local', 'phylum', 'lowerClassItem', 'mediumClassItem', 'highClassItem', 'feedItem')])
rawItems  <-  rawItems[with(rawItems, order(phylum, lowerClassItem, mediumClassItem, highClassItem, feedItem)), ]
range(table(rawItems$local))
nrow(unique(rawItems[,c('phylum', 'lowerClassItem', 'mediumClassItem', 'highClassItem', 'feedItem')]))

# individuals per site for rna/dna
rm(list=ls())
load('output/RDatafiles/rnaDnaAnalysis.RData')
indsPerSite  <-  tapply(rna$sample, rna$site, function(x)length(unique(x)))
sum(indsPerSite)

# density numbers
tapply(abun$abun, abun$site, sum) #number of individuals per site
tapply(abun$transect, abun$site, function(x)length(unique(x))) #number of transects per site

# gut length numbers
gcs  <-  readFile('data/gut_length_c_striatus_raw.csv')
range(gcs[,2])
range(gcs[,2]/gcs[,1])
mean(gcs[,2]/gcs[,1])
sd(gcs[,2]/gcs[,1])/sqrt(nrow(gcs))

