library(plyr)
source('paths.R')
rm(list=ls())
source('R/functions-analyses.R')

diet         <-  readFile('data/diet_raw.csv')
summaryDiet  <-  ddply(diet, .(local), function(x) {
	nInds   <-  length(unique(x$individual))
	sumVol  <-  sum(x$volume_mm3)
	part1   <-  ddply(x, .(table3Category), function(y, sumVol, nInds) {
		freqOcc  <-  length(unique(y$individual))/nInds*100
		volPercent  <-  sum(y$volume_mm3)/sumVol*100
		data.frame(FO=freqOcc, VO=volPercent, stringsAsFactors=FALSE)
		}, sumVol, nInds)
	part1$IAi  <-  (part1$VO*part1$FO)/sum(part1$VO*part1$FO)*100
	part1
})

###########################
# CREATE AND EXPORT TABLE 3
###########################
table3  <-  summaryDiet
for(j in 3:5) {
	table3[,j]  <-  ifelse(table3[,j] < 0.1, '< 0.1', round(table3[,j], 1))
}
write.csv(table3, 'output/data/table3.csv')

rm(list=ls()[!(ls() %in% c('diet', 'summaryDiet'))])
save.image('output/RDatafiles/dietData.RData')
