library(plyr)
library(hdrcde)
library(RColorBrewer)
library(extrafont)
library(fontcm)
loadfonts()

rm(list=ls())
source('paths.R')
source('R/functions-analyses.R')
source('R/functions-figures.R')
load('output/RDatafiles/densityAnalysis.RData')

interpolateZeros  <-  function(x) {
	sapply(x, function(y)c(0,y))
}

iterationHistograms  <-  function() {
	responses  <-  densityMCMC$data
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

drawJitterPoints  <-  function(data, response, responseNums) {
    points(jitter(data[[responseNums]], amount=0.15), data[[response]], pch=16, cex=1.2, col=make.transparent('grey50',0.01))    
    qts  <-  quantile(data[[response]], probs=c(0.025, 0.975), type=2)
    avs  <-  data.frame(mean=mean(data[[response]]), ci2.5=qts[1], ci97.5=qts[2])
    lines(rep(unique(data[[responseNums]]), 2), c(avs$ci2.5, avs$ci97.5), lty=2, lwd=2, col='darkred')
    lines(unique(data[[responseNums]]) - c(0.1,-0.1), rep(avs$ci2.5, 2), lty=1, lwd=2, col='darkred')
    lines(unique(data[[responseNums]]) - c(0.1,-0.1), rep(avs$ci97.5, 2), lty=1, lwd=2, col='darkred')
    points(unique(data[[responseNums]]), avs$mean, pch=21, cex=1.3, col='darkred', bg='tomato')
}

Jitter  <-  function(data=meanParameters) {
	siteNames    <-  c('puerto_rico', 'tamandare', 'salvador', 'abrolhos', 'guarapari', 'arraial', 'florianopolis')
	siteLabels   <-  c('Puerto Rico', 'Tamandaré', 'Salvador', 'Abrolhos', 'Guarapari', 'A. do Cabo', 'Florianópolis')
	siteNumbers  <-  1:7
	data$siteLabels   <-  siteLabels[match(data$site, siteNames)]
	data$siteNums     <-  siteNumbers[match(data$site, siteNames)]
	par(omi=c(0.2, 0.1, 0, 0.1), mai=c(1.42, 0.82, 0.42, 0.42), cex=1)
	plot(NA, ylim=c(0.5, 2.5), xlim=c(0.8,7.2), ylab=expression(paste('Average density (inds. / 200 m'^2, ')')), xlab='', xpd=NA, cex.lab=1.3, las=1, xaxt='n')
	d_ply(data, .(siteNums), drawJitterPoints, response='estimates', responseNums='siteNums')
	axis(1, at=1:7, rep('', 7))
	text(1:7, rep(0.34, 7), siteLabels, srt=45, xpd=NA, adj=c(1,0.5))
	label(0.5, -0.3, 'Sites', xpd=NA, cex=1.3, adj=c(0.5,0.5))
}

drawBayesDens  <-  function(data, response, responseNums, responseLabels) {
    xDens    <-  density(data[[response]])
    scaledX  <-  linearRescale(xDens$y, c(0, 0.7)) + unique(data[[responseNums]])
    xLwHalf  <-  scaledX[1:which.max(scaledX)]
    xUpHalf  <-  scaledX[(which.max(scaledX)+1):length(scaledX)]
    yLwHalf  <-  xDens$x[1:which.max(scaledX)]
	yUpHalf  <-  xDens$x[(which.max(scaledX)+1):length(scaledX)]
    cols     <-  colorRampPalette(brewer.pal(9, 'YlOrRd')[3:9])(10*length(xLwHalf))
    linearX  <-  seq(min(xLwHalf), max(xLwHalf), length.out=10*length(xLwHalf))
    for(j in seq_along(xLwHalf)) {
	   	y1   <-  yLwHalf[which.min(abs(xLwHalf - xLwHalf[j]))]
	   	y2   <-  yUpHalf[which.min(abs(xUpHalf - xLwHalf[j]))]
	   	lines(rep(xLwHalf[j], 2), c(y1, y2), col=cols[which.min(abs(linearX - xLwHalf[j]))])
    }
	polygon(c(scaledX, min(scaledX)), c(xDens$x, min(xDens$x)), border='black', col=NA)

    # overlay extremes outside 95% HDR
    xDensHDR  <-  hdr(data[[response]])$hdr['95%',]
    trim1     <-  xDens$x < xDensHDR[1]
    trim2     <-  xDens$x > xDensHDR[2]
    polygon(c(scaledX[trim1], rep(min(scaledX), 2), min(scaledX[trim1])), c(xDens$x[trim1], max(xDens$x[trim1]), rep(min(xDens$x[trim1]), 2)), border='black', col=NA, lwd=1.5)
    polygon(c(scaledX[trim2], rep(min(scaledX), 2), min(scaledX[trim2])), c(xDens$x[trim2], max(xDens$x[trim2]), rep(min(xDens$x[trim2]), 2)), border='black', col=NA, lwd=1.5)

    usr  <-  par('usr')
    xpo  <-  (unique(data[[responseNums]])-usr[1])/(usr[2]-usr[1])
    label(xpo, 0.93, unique(data[[responseLabels]]), cex=0.9, font=3, adj=c(0, 0.5))
    max(xDens$y)
}

Dens  <-  function(data=MCMCParameters) {
	siteNames    <-  c('puerto_rico', 'tamandare', 'salvador', 'abrolhos', 'guarapari', 'arraial', 'florianopolis')
	siteLabels   <-  c('Puerto Rico', 'Tamandaré', 'Salvador', 'Abrolhos', 'Guarapari', 'A. do Cabo', 'Florianópolis')
	siteNumbers  <-  1:7
	data$siteLabels   <-  siteLabels[match(data$site, siteNames)]
	data$siteNums     <-  siteNumbers[match(data$site, siteNames)]
	par(omi=c(0,0.4,0,0), mai=c(1.02, 0.82, 0.4, 0.42), cex=1)
	plot(NA, ylim=c(0.2, 2.8), xlim=c(1,7.8), ylab=expression(paste('Average density (inds. / 200 m'^2, ')')), xlab='Posterior density', xpd=NA, cex.lab=1.3, las=1, xaxt='n')
	xticks  <-  daply(data, .(siteNums), drawBayesDens, response='estimates', responseNums='siteNums', responseLabels='siteLabels')
	axis(1, at=sort(c(1:7, 1:7+0.7)), interpolateZeros(round(xticks, 1)), mgp=c(3, 0.5, 0))
}

to.pdf(Jitter(), fig.path(name='poissonJAGS_jittered.pdf'), width=6, height=6)
embed_fonts(fig.path(name='poissonJAGS_jittered.pdf'))

to.pdf(Dens(), fig.path(name='poissonJAGS_posteriorDensity.pdf'), width=9, height=4)
embed_fonts(fig.path(name='poissonJAGS_posteriorDensity.pdf'))

to.pdf(iterationHistograms(), fig.path(name='poissonJAGS_iterationHistograms.pdf'), width=8, height=10)
embed_fonts(fig.path(name='poissonJAGS_iterationHistograms.pdf'))
