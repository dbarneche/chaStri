library(plyr)
source('paths.R')
rm(list=ls())
source('R/functions-analyses.R')

#########################
# LOAD AND CLEAN DATA
# EXPRESSED AS COUNTS
# FIRST TRANSFORM TO % 
# REMOVE UNWANTED COLUMNS
#########################
cover     <-  readFile('data/benthic_cover_raw.csv')
cover     <-  cbind(cover[,1:2], t(apply(cover[,3:ncol(cover)], 1, function(x)x/sum(x)*100)))
fewItems  <-  c('SHA','ECHI','HYD_MIL','HYD','MA','CRU','ASC','BRY','NI')
cover     <-  cover[, setdiff(names(cover), fewItems)]
rm(fewItems)

# fix names
names(cover)[names(cover) == 'TURF']  <-  'EAM'
names(cover)[names(cover) == 'MCRO']  <-  'MCRU'
names(cover)[names(cover) == 'ANT_GOR']  <-  'ANT_OCT'

#join columns that are synonyms
cover$ANT_ZOA  <-  cover$ANT_ZOA + cover$ANT
cover$ANT_OCT  <-  cover$ANT_OCT + cover$ANT_PLE
cover$MFOL     <-  cover$MFOL + cover$MFIL
cover          <-  cover[, setdiff(names(cover), c('ANT', 'ANT_PLE', 'MFIL'))]

################
# SUMMARISE DATA
################
newCover  <-  data.frame(stringsAsFactors=FALSE)
for(j in 3:ncol(cover)) {
	newCover  <-  rbind(newCover, data.frame(site=cover$site, fishID=cover$fishID, item=names(cover)[j], percent=cover[,j], stringsAsFactors=FALSE))
}
newCover$fishID  <-  as.numeric(as.factor(paste0(newCover$site, newCover$fishID)))

summaryNewCover  <-  ddply(newCover, .(site), function(data) {
	dat  <-  tapply(data$percent, list(data$fishID, data$item), sum)
	data.frame(item=colnames(dat), mean=colMeans(dat), sd=apply(dat, 2, sd), se=apply(dat, 2, sd)/sqrt(nrow(dat)), stringsAsFactors=FALSE)
})

rmFcts('R/functions-analyses.R')
save.image('output/RDatafiles/coverData.RData')
