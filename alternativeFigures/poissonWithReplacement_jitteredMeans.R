library(plyr)
library(extrafont)
library(fontcm)
loadfonts()

rm(list=ls())
source('paths.R')
source('R/functions-analyses.R')
source('R/functions-figures.R')
abun  <-  readFile('data/density_raw.csv')

transectInfo  <-  function(data, area=200) {
	list(data               =  data,
		availableTransects  =  unique(data$transect),
		neededTransects     =  area/unique(data$area))
}

standardisedTransects  <-  function(dataList, nOfSamples=15) {
	data  <-  dataList$data
	out   <-  data.frame(abun=matrix(0, nOfSamples, 1), stringsAsFactors=FALSE)
	for(i in seq_len(nOfSamples)) {
		transectCluster  <-  sample(dataList$availableTransects, dataList$neededTransects, replace=TRUE)
		out[i,]  <-  sum(data$abun[data$transect %in% transectCluster])
	}
	out
}

extractFixedEffects  <-  function(modelSummary) {
	estimates  <-  coef(modelSummary)[,'Estimate']
	data.frame(site=gsub('site','',names(estimates)), estimates=estimates, stringsAsFactors=FALSE, row.names=NULL)
}

drawJitterPoints  <-  function(data, response, responseNums) {
    points(jitter(data[[responseNums]], amount=0.15), data[[response]], pch=16, cex=1.2, col=make.transparent('grey50',0.1))    
    qts  <-  quantile(data[[response]], probs=c(0.025, 0.975), type=2)
    avs  <-  data.frame(mean=mean(data[[response]]), ci2.5=qts[1], ci97.5=qts[2])
    lines(rep(unique(data[[responseNums]]), 2), c(avs$ci2.5, avs$ci97.5), lty=2, lwd=2, col='darkred')
    lines(unique(data[[responseNums]]) - c(0.1,-0.1), rep(avs$ci2.5, 2), lty=1, lwd=2, col='darkred')
    lines(unique(data[[responseNums]]) - c(0.1,-0.1), rep(avs$ci97.5, 2), lty=1, lwd=2, col='darkred')
    points(unique(data[[responseNums]]), avs$mean, pch=21, cex=1.3, col='darkred', bg='tomato')
}

iter                <-  1000
listOfTransectInfo  <-  dlply(abun, .(site), transectInfo)
parameters          <-  data.frame(stringsAsFactors=FALSE)
responses           <-  vector(mode='list', length=iter)
set.seed(1)
for(j in seq_len(iter)) {
	standardisedData    <-  ldply(listOfTransectInfo, standardisedTransects)
	modelSummary        <-  summary(glm(abun ~ site - 1, data=standardisedData, family=poisson))
	responses[[j]]      <-  standardisedData
	parameters          <-  rbind(parameters, extractFixedEffects(modelSummary))
}

iterationHistograms  <-  function() {
	par(mfcol=c(9,5), oma=c(5,4,2,2), cex=1)
	nRowslPlots  <-  par('mfcol')[1]
	nColslPlots  <-  par('mfcol')[2]
	maxNOfPlots  <-  nRowslPlots*nColslPlots
	pos          <-  seq(nRowslPlots, maxNOfPlots, nRowslPlots)
	n            <-  0
	for(i in seq_along(responses)) {
		n  <-  n + 1
		if(n %in% 1:9){
			par(mar=c(0.1,1.1,0.1,0.1)); yaxt='s'
		} else {
			par(mar=c(0.1,1.1,0.1,0.1)); yaxt='n'
		}
		y  <-  responses[[i]]$abun
		y  <-  y[y < 20]
		hist(y, breaks=0:20, xlab='', ylab='', main='', las=1, xlim=c(0,20), ylim=c(0, 30), xaxt='n', yaxt=yaxt, cex.axis=0.6)
		if(n %in% pos) {
			axis(1, cex.axis=0.6, mgp=c(3, 0.5, 0))
		}
		box()
		if(n == maxNOfPlots | i == length(responses)) {
	    mtext(expression(paste('Average density (inds. / 200 m'^2, ')')), side=1, line=2.5, outer=TRUE, cex=1.2)
	 	mtext('Frequency', side=2, line=1.7, outer=TRUE, cex=1.2)
				n  <-  0
		}
		label(0.05, 0.93, paste0('Iteration #', i), adj=c(0, 0.5), font=3, cex=0.6)
	}
}

iterationResiduals  <-  function() {
	par(mfcol=c(9,5), oma=c(5,4,2,2), cex=1)
	nRowslPlots  <-  par('mfcol')[1]
	nColslPlots  <-  par('mfcol')[2]
	maxNOfPlots  <-  nRowslPlots*nColslPlots
	pos          <-  seq(nRowslPlots, maxNOfPlots, nRowslPlots)
	n            <-  0
	for(i in seq_along(responses)) {
		n  <-  n + 1
		if(n %in% 1:9){
			par(mar=c(0.1,1.1,0.1,0.1)); yaxt='s'
		} else {
			par(mar=c(0.1,1.1,0.1,0.1)); yaxt='n'
		}
		model  <-  glm(abun ~ site - 1, data=responses[[i]], family=poisson)
		plot(jitter(predict(model),amount=0.01), sqrt(abs(resid(model))), xlim=c(0.8, 1.8), ylim=c(0, 2), xlab='', ylab='', las=1, xaxt='n', yaxt=yaxt, cex.axis=0.6, pch=16, col=make.transparent('grey50',0.6))
		if(n %in% pos) {
			axis(1, cex.axis=0.6, mgp=c(3, 0.5, 0))
		}
		if(n == maxNOfPlots | i == length(responses)) {
	    mtext('Predicted', side=1, line=2.5, outer=TRUE, cex=1.2)
	 	mtext(expression(paste(sqrt('|Standardised deviance residuals|'))), side=2, line=1.7, outer=TRUE, cex=1.2)
				n  <-  0
		}
		label(0.05, 0.93, paste0('Iteration #', i), adj=c(0, 0.5), font=3, cex=0.6)
	}
}

poissonJitter  <-  function() {
	siteNames    <-  c('puerto_rico', 'tamandare', 'salvador', 'abrolhos', 'guarapari', 'arraial', 'florianopolis')
	siteLabels   <-  c('Puerto Rico', 'Tamandaré', 'Salvador', 'Abrolhos', 'Guarapari', 'A. do Cabo', 'Florianópolis')
	siteNumbers  <-  1:7
	parameters$siteLabels   <-  siteLabels[match(parameters$site, siteNames)]
	parameters$siteNums     <-  siteNumbers[match(parameters$site, siteNames)]
	par(omi=c(0.2, 0.1, 0, 0.1), mai=c(1.42, 0.82, 0.42, 0.42), cex=1)
	plot(NA, ylim=c(0.5, 2.5), xlim=c(0.8,7.2), ylab=expression(paste('Average density (inds. / 200 m'^2, ')')), xlab='', xpd=NA, cex.lab=1.3, las=1, xaxt='n')
	d_ply(parameters, .(siteNums), drawJitterPoints, response='estimates', responseNums='siteNums')
	axis(1, at=1:7, rep('', 7))
	text(1:7, rep(0.34, 7), siteLabels, srt=45, xpd=NA, adj=c(1,0.5))
	label(0.5, -0.3, 'Sites', xpd=NA, cex=1.3, adj=c(0.5,0.5))
}

to.pdf(poissonJitter(), fig.path(name='poissonWithReplacement_Jittered.pdf'), width=6, height=6)
embed_fonts(fig.path(name='poissonWithReplacement_Jittered.pdf'))

to.pdf(iterationHistograms(), fig.path(name='poissonWithReplacement_iterationHistograms.pdf'), width=8, height=10)
embed_fonts(fig.path(name='poissonWithReplacement_iterationHistograms.pdf'))

to.pdf(iterationResiduals(), fig.path(name='poissonWithReplacement_iterationResiduals.pdf'), width=8, height=10)
embed_fonts(fig.path(name='poissonWithReplacement_iterationResiduals.pdf'))
