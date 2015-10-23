rm(list=ls())
source('R/functions-analyses.R')

##############################################
# LOAD SST DATA AND 
# RESPECTIVE COORDINATES
# NOTICE THAT NAMES ARE STRUCTURE AS:
# 1. XYEARMONTHDAY_SST - EXAMPLE X20091231_SST
# - 31ST OF DECEMBER 2009; 2. NOTICE THAT THE
# DATABASE SPANS WEEKLY AVERAGED SST DATA 
# (EVERY 7 DAYS - CHECK COLUMN NAMES) FROM 
# 1982 TO 2009
##############################################
sst    <-  readFile('data/sst_raw.csv', row.names=1)
sites  <-  readFile('data/sst_coords.csv')

#################################
# CALCULATE TOTAL AVERAGED VALUES 
# CHOSEN TIME WINDOW: 1999-2009
#################################
year   <-  as.numeric(sapply(names(sst), function(x){substr(x, 2, 5)}))
sst    <-  sst[, which(year > 1998 & year < 2010)]
names(sst) #starts on 02/01/1999 and ends on 31/12/2009

############################
# CALCULATE MINIMUM AVERAGED
# VALUES AMONG YEARS
############################
filtYr   <-  year[year > 1998 & year < 2010]
unYr     <-  sort(unique(filtYr))
minDat   <-  matrix(0, nrow(sst), length(unYr))
maxDat   <-  matrix(0, nrow(sst), length(unYr))
for(i in seq_along(unYr)) {
	yearfilt    <-  sst[, filtYr %in% unYr[i]]
	minDat[,i]  <-  apply(yearfilt, 1, min)
	maxDat[,i]  <-  apply(yearfilt, 1, max)
}
colnames(minDat)  <-  unYr
colnames(maxDat)  <-  colnames(minDat)
rownames(minDat)  <-  paste0(sites$distance_m, '_', sites$site)
rownames(maxDat)  <-  rownames(minDat)

######################
# CREATE FINAL TABLE
# EVALUATE CORRELATION 
# BETWEEN AVERAGES
######################
sst       <-  data.frame(site=rownames(minDat), meanSst=rowMeans(sst), minMeanSst=rowMeans(minDat), maxMeanSst=rowMeans(maxDat), stringsAsFactors=FALSE)
sst       <-  sst[grep('50_', sst$site, fixed=TRUE), ]
sst$site  <-  sub('50_', '', sst$site, fixed=TRUE)
sst[,2:4] <-  round(sst[,2:4], 1)
sst       <-  sst[order(sst$site), ]
