library(plyr)
source('paths.R')
rm(list=ls())
source('R/functions-analyses.R')

#######################
# LOAD AND CLEAN DATA
# REMOVE UNWANTED ITEMS
#######################
bites     <-  readFile('data/bites_raw.csv', na.strings=c(NA,''))
bites     <-  bites[bites$life_stage == 'adult', setdiff(names(bites), c('EGGS_ABUSAX','SEAURCHIN','PLANKTON'))]

######################################
# PREPARE BITES FOR SELECTIVITY MATRIX
######################################
selec  <-  ddply(bites, .(site, id), function(x) {
	x    <-  x[,-c(1:8)]
	x    <-  apply(x, 2, sum, na.rm=TRUE)
	dat  <-  data.frame()
	for(i in seq_along(x)) {
		dat  <-  rbind(dat, data.frame(bite=rep(1, x[i]), food=rep(names(x)[i], x[i]), step=rep(1, x[i]), stringsAsFactors=FALSE))
	}
	dat
})

###############################
# CREATE FOR SELECTIVITY MATRIX
# FIRST, EITHER RUN THE BENTHIC 
# COVER FILE OR LOAD THE 
# CORRESPONDING OUTPUT RDATA 
###############################
load('output/RDatafiles/coverData.RData')
set.seed(1)
bitesPerItem  <-  ddply(selec, .(site, id), function(x, cover) {
	cover  <-  cover[cover$site == unique(x$site), ]
	food   <-  sample(cover$item, 1e3, prob=cover$mean/1e2, replace=TRUE)
	rbind(x, data.frame(site=rep(x$site[1], length(food)), id=rep(x$id[1], length(food)), bite=rep(0, length(food)), food=food, step=rep(1, length(food)), stringsAsFactors=FALSE))
}, cover=summaryNewCover)

########################
# AVAILABLE/USED 
# BY SITE AND SUBSTRATUM
########################
proportions  <-  ddply(bitesPerItem, .(site), function(x) {
	tab  <-  t(table(x$bite,x$food)/as.vector(table(x$bite)))
	colnames(tab)  <-  c('available', 'bite')
	data.frame(food=rownames(tab), cbind(tab, selection=tab[,2]/tab[,1]), stringsAsFactors=FALSE)
})

rm(list=ls()[!(ls() %in% c('bites', 'selec', 'bitesPerItem', 'proportions'))])
save.image('output/RDatafiles/bitesData.RData')
