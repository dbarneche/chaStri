library(plyr)
library(extrafont)
library(fontcm)
loadfonts()

rm(list=ls())
source('paths.R')
source('R/functions-analyses.R')
source('R/functions-figures.R')
load('output/RDatafiles/bitesAnalysis.RData')

abun                         <-  readFile('data/density_raw.csv')
abun$abun[abun$area == 100]  <-  round(abun$abun[abun$area == 100]*0.4)
abun$area                    <-  40

insertZeroes  <-  function(x) {
  x  <-  round(x, 2)
  as.numeric(sapply(x, function(x)c(0,x)))
}

drawInnerHistograms  <-  function(data, response, responseNums, responseLabels, breaks, yli) {
    quant  <-  quantile(data[[response]], probs=c(0.025, 0.975), type=2)
    data   <-  data[data[[response]] >= quant[1] & data[[response]] <= quant[2], ]
    xhist  <-  hist(data[[response]], plot=FALSE, breaks=breaks[1]:breaks[2])

    maxDensity       <-  max(xhist$density)
    rescaledDensity  <-  linearRescale(xhist$density, c(0, 0.7))
    barplot(rescaledDensity, axes=FALSE, space=0, horiz=TRUE, add=TRUE, ylim=yli, offset=unique(data[[responseNums]]))
    usr  <-  par('usr')
    xpo  <-  (unique(data[[responseNums]])-usr[1])/(usr[2]-usr[1])
    label(xpo, 0.93, unique(data[[responseLabels]]), cex=0.9, font=3, adj=c(0, 0.5))
    
    avs  <-  draw95Ci(data, response=response)
    lines(rep(unique(data[[responseNums]])+0.3, 2), c(avs$ci2.5, avs$ci97.5), lty=1, lwd=1, col='tomato')
    points(unique(data[[responseNums]])+0.3, avs$mean, pch=21, cex=0.7, col='darkred', bg='tomato')
    maxDensity
}

bitesAndAbunHists  <-  function() {
  siteNames    <-  c('puerto_rico', 'tamandare', 'salvador', 'abrolhos', 'guarapari', 'arraial', 'florianopolis')
  siteLabels   <-  c('Puerto Rico', 'Tamandaré', 'Salvador', 'Abrolhos', 'Guarapari', 'A. do Cabo', 'Florianópolis')
  siteNumbers  <-  1:7
  
  summaryBites$siteLabels  <-  siteLabels[match(summaryBites$site, siteNames)]
  summaryBites$siteNums    <-  siteNumbers[match(summaryBites$site, siteNames)]
  abun$siteLabels   <-  siteLabels[match(abun$site, siteNames)]
  abun$siteNums     <-  siteNumbers[match(abun$site, siteNames)]

  par(mfrow=c(2, 1), omi=c(0,0,0,0), mai=c(0.5, 0.92, 0.82, 0.32), cex=1)
  plot(NA, ylim=c(0, 35), xlim=c(1,7.8), ylab='Bites / 3 min', xlab='', xpd=NA, cex.lab=1.1, las=1, xaxt='n', yaxt='n')
  plt  <-  ddply(summaryBites, .(siteNums), drawInnerHistograms, response='bites', responseNums='siteNums', responseLabels='siteLabels', breaks=c(0,31), yli=c(0,1))
  axis(1, at=sort(c(1:7, 1:7+0.7)), insertZeroes(plt$V1), mgp=c(3, 0.5, 0), cex.axis=0.8)
  axis(2, at=seq(0, 30, 5), las=1, cex.axis=0.8)
  lines(par('usr')[1:2], rep(mean(summaryBites$bites), 2), lty=2, lwd=2)

  par(mai=c(1.32, 0.92, 0, 0.32))
  plot(NA, ylim=c(0, 12), xlim=c(1,7.8), ylab=expression('Abundance / 40 m'^2), xlab='Proportion', xpd=NA, cex.lab=1.1, las=1, xaxt='n', yaxt='n')
  plt  <-  ddply(abun, .(siteNums), drawInnerHistograms, response='abun', responseNums='siteNums', responseLabels='siteLabels', breaks=c(0,10), yli=c(0,1))
  axis(1, at=sort(c(1:7, 1:7+0.7)), insertZeroes(plt$V1), mgp=c(3, 0.5, 0), cex.axis=0.8)
  axis(2, at=seq(0, 12, length.out=5), las=1, cex.axis=0.8)
  lines(par('usr')[1:2], rep(mean(abun$abun), 2), lty=2, lwd=2)
}

to.pdf(bitesAndAbunHists(), fig.path(name='bitesAndAbunHists.pdf'), width=9, height=6)
embed_fonts(fig.path(name='bitesAndAbunHists.pdf'))
