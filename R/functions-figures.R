######################
# AUXILLIARY FUNCTIONS
######################
make.transparent <- function(col, opacity=0.5) {
  if (length(opacity) > 1 && any(is.na(opacity))) {
    n <- max(length(col), length(opacity))
    opacity <- rep(opacity, length.out=n)
    col <- rep(col, length.out=n)
    ok <- !is.na(opacity)
    ret <- rep(NA, length(col))
    ret[ok] <- Recall(col[ok], opacity[ok])
    ret
  } else {
    tmp <- col2rgb(col)/255
    rgb(tmp[1,], tmp[2,], tmp[3,], alpha=opacity)
  }
}

label <- function(px, py, lab, adj=c(0, 1), text=TRUE, log=FALSE, ...) {
  usr  <-  par('usr')
  x.p  <-  usr[1] + px*(usr[2] - usr[1])
  y.p  <-  usr[3] + py*(usr[4] - usr[3])
  if(log=='x'){x.p<-10^(x.p)}
  if(log=='y'){y.p<-10^(y.p)}
  if(log=='xy'){x.p<-10^(x.p);y.p<-10^(y.p)}
  if(text){
    text(x.p, y.p, lab, adj=adj, ...)
  } else {
    points(x.p, y.p, ...)
  }
}

to.pdf <- function(expr, filename, ...) {
  to.dev(expr, pdf, filename, ...)
}

fig.path  <-  function(name) {
  file.path('output/figures', name)
}

to.dev <- function(expr, dev, filename, ..., verbose=TRUE) {
  if ( verbose )
    cat(sprintf('Creating %s\n', filename))
  dev(filename, family='CM Roman', ...)
  on.exit(dev.off())
  eval.parent(substitute(expr))
}

to.pdf <- function(expr, filename, ...) {
  to.dev(expr, pdf, filename, ...)
}

linearRescale <- function(x, r.out) {
  p <- (x - min(x)) / (max(x) - min(x))
  r.out[[1]] + p * (r.out[[2]] - r.out[[1]])
}

rounded  <-  function(value, precision=1, change=FALSE) {
  if(change) {
    value  <-  value * -1
  }
  sprintf(paste0('%.', precision, 'f'), round(value, precision))
}

drawBayesDensOverlap  <-  function(data, response, responseNums, responseLabels) {
    set.seed(1)
    theCol   <-  brewer.pal(7, 'RdYlBu')
    theCol[5]<-  '#B2DFEE'
    theCol   <-  theCol[unique(data[[responseNums]])]
    yLabPos  <-  seq(0.85, 0.45, length.out=7)[unique(data[[responseNums]])]
    xDens    <-  density(data[[response]])
    polygon(c(xDens$x, min(xDens$x)), c(xDens$y, min(xDens$y)), border=theCol, col=make.transparent(theCol, 0.3))
	label(c(0.02, 0.07), rep(yLabPos, 2), text=FALSE, type='l', lwd=2, col=theCol)
    label(0.08, yLabPos, unique(data[[responseLabels]]), adj=c(0, 0.5), cex=0.8)
}

###############
# PAPER FIGURES
###############
fig1  <-  function() {
	desired   <-  c('04.Hexac. Actiniaria',
					'05.Hexac. Zoantharia',
					'06.Hexac. Corallim./ Scle.',
					'07.Hexac. Other',
					'08.Sabellidae',
					'10.Cirratulidae',
					'14.POLYCHAETA Other',
					'19.EGGS Mollusca')
	data      <-  summaryDiet[summaryDiet$table3Category %in% desired, ]
	data      <-  as.data.frame(tapply(data$IAi, list(data$table3Category, data$local), sum))
	data[is.na(data)]  <-  0
	data      <-  data[c('04.Hexac. Actiniaria', '06.Hexac. Corallim./ Scle.', '05.Hexac. Zoantharia', '07.Hexac. Other', '10.Cirratulidae', '08.Sabellidae', '14.POLYCHAETA Other', '19.EGGS Mollusca'), c('puerto_rico', 'tamandare', 'salvador', 'abrolhos', 'trindade', 'guarapari', 'arraial', 'florianopolis')]

	coords    <-  readFile('data/sites_coords.csv')
	mp        <-  read.table('data/map.dat', header=TRUE)
	names(mp) <-  c('lon','lat')
	mp        <-  mp[mp$lon > -98 & mp$lon < -28, ]
	mp        <-  mp[mp$lat > -32 & mp$lat < 34.5, ]
	siteCols  <-  c(paste0('darkolivegreen', c(2:4,'')), paste0('dodgerblue', 2:4), 'tomato')

	layout(cbind(matrix(1, 4, 2), matrix(2:9, 4, 2, byrow=TRUE)))
	
	#map
	par(mai=c(0.596, 0.264, 0.264, 0.9))
	plot(0, 0, type='n', axes=FALSE, xlab='', ylab='', xlim=c(-100, -30), ylim=c(-40, 35))
	lines(mp[,1], mp[,2], type="l", lwd=0.5, xpd=NA)
	points(coords$lon, coords$lat, pch=coords$shape, col='black', bg=coords$color, cex=1.5)
	lines(c(-98, -24, -24, -98, -98), c(-32, -32, 34.5, 34.5, -32), lty=2, col='grey40', xpd=NA)
	lines(c(-100, -98), c(-30, -30)); text(-99, -30, -30, pos=2, cex=1.1, xpd=NA)
	lines(c(-100, -98), c(-15, -15)); text(-99, -15, -15, pos=2, cex=1.1, xpd=NA)
	lines(c(-100, -98), c(0, 0)); text(-99, 0, 0, pos=2, cex=1.1, xpd=NA)
	lines(c(-100, -98), c(15, 15)); text(-99, 15, 15, pos=2, cex=1.1, xpd=NA)
	lines(c(-100, -98), c(30, 30)); text(-99, 30, 30, pos=2, cex=1.1, xpd=NA)
	polygon(c(-90, -89, -88, -90), c(-30, -24, -30, -30), col='black')
	text(-89, -22, 'N', adj=c(0.5, 0.5), cex=1.1)
	lines(c(-96, -96), c(34.5, 36.5)); text(-96, 39, -96, adj=c(0.7, 0.5), cex=1.1, xpd=NA)
	lines(c(-62.5, -62.5), c(34.5, 36.5)); text(-62.5, 39, -62.5, adj=c(0.6, 0.5), cex=1.1, xpd=NA)
	lines(c(-29, -29), c(34.5, 36.5)); text(-29, 39, -29, adj=c(0.7, 0.5), cex=1.1, xpd=NA)

	#barplots
	par(mai=rep(0.2, 4), xpd=NA)
	sites  <-  c('Puerto Rico', 'Tamandaré', 'Salvador', 'Abrolhos', 'Trindade', 'Guarapari', 'Arraial do Cabo', 'Florianópolis')
	for(k in 1:ncol(data)) {
		#if(k==5){par(mai=c(1.1732, 0.55, 0.0412, 0.1684))}
		barplot(data[,k], ylim=c(0, 80), col=siteCols, names.arg=FALSE, las=1)
		label(0.05, 0.99, text=FALSE, pch=coords$shape[coords$site==names(data)[k]], bg=coords$color[coords$site==names(data)[k]], cex=1.5, xpd=NA)
		label(0.09, 1.03, sites[k], cex=1.2, xpd=NA)
	}
	mtext('Feeding index (%)', side=2, at=0.51, line=-32.5, outer=TRUE, cex=1.3)

	#legend
	items  <-  c('Actiniaria', 'Corallimorpharia / Scleractinia', 'Zoantharia', 'Hexacorallia (other)', 'Cirratulidae', 'Sabellidae', 'Polychaeta (other)', 'Eggs')
	xpos  <-  rep(c(-3.5, -2.2), each=4)
	ypos  <-  rep(c(seq(0.9, -0.05, length.out=4)), 4)

	for(k in seq_along(items)) {
		label(xpos[k], ypos[k], text=FALSE, pch=22, bg=siteCols[k], cex=1.8, xpd=NA)
		label(xpos[k]+0.05, ypos[k]+0.05, items[k], cex=1.4, xpd=NA)
	}
}

fig2  <-  function() {
	siteNames    <-  c('puerto_rico', 'tamandare', 'salvador', 'abrolhos', 'guarapari', 'arraial', 'florianopolis')
	siteLabels   <-  c('Puerto Rico', 'Tamandaré', 'Salvador', 'Abrolhos', 'Guarapari', 'A. do Cabo', 'Florianópolis')
	siteNumbers  <-  1:7
	
	# abundance
	MCMCParameters$siteLabels   <-  siteLabels[match(MCMCParameters$site, siteNames)]
	MCMCParameters$siteNums     <-  siteNumbers[match(MCMCParameters$site, siteNames)]
	par(mfrow=c(1, 2), mai=c(1.02,1,0.42,0.42), cex=1)
	plot(NA, xlim=c(0, 2.6), ylim=c(0,3), xlab=expression(paste('Average density (inds. / 200 m'^2, ')')), ylab='Posterior density', axes=FALSE)
	usr  <-  par('usr')
	rect(usr[1], usr[3], usr[2], usr[4], col='grey90', border=NA)
	box()
	axis(1, cex.axis=0.9, mgp=c(3, 0.6, 0))
	axis(2, las=1, cex.axis=0.9)
	d_ply(MCMCParameters, .(siteNums), drawBayesDensOverlap, response='estimates', responseNums='siteNums', responseLabels='siteLabels')
	label(.05, .95, 'A', font=2, adj=c(0.5,0.5))

	# bites
	par(mai=c(1.02,0.7,0.42,0.72))
	avBites            <-  data.frame(coef(summary(bitesNegBin)))
	avBites$ci2.5      <-  avBites$Estimate - 1.96*avBites$Std..Error
	avBites$ci97.5     <-  avBites$Estimate + 1.96*avBites$Std..Error
	rownames(avBites)  <-  gsub('site', '', rownames(avBites))

	plot(NA, xlim=c(1, length(siteNames)), ylim=c(0 , ceiling(max(summaryBites$bites))), xlab='', ylab='Bites / 3 min', xpd=NA, axes=FALSE)
	usr  <-  par('usr')
	rect(usr[1], usr[3], usr[2], usr[4], col='grey90', border=NA)
	box()
	axis(1, labels=NA, cex.axis=0.9)
	axis(2, las=1, cex.axis=0.9)
	for(k in seq_along(siteNames)) {
		y     <-  summaryBites$bites[summaryBites$site == siteNames[k]]
		points(jitter(rep(k, length(y)), amount=.05), y, pch=21, col='white', bg=make.transparent('white',0.5), lwd=0.5)
		lines(rep(k, 2), c(avBites$ci2.5[rownames(avBites) == siteNames[k]], avBites$ci97.5[rownames(avBites) == siteNames[k]]), lty=1, col='dodgerblue2', lwd=1)
		lines(c(k-.05, k+.05), rep(avBites$ci2.5[rownames(avBites) == siteNames[k]], 2), col='dodgerblue2', lwd=1.5)
		lines(c(k-.05, k+.05), rep(avBites$ci97.5[rownames(avBites) == siteNames[k]], 2), col='dodgerblue2', lwd=1.5)
		points(k, avBites$Estimate[rownames(avBites) == siteNames[k]], cex=0.9, pch=21, col='dodgerblue4', bg='dodgerblue2')
	}
	text(1:length(siteNames), -3.5, labels=siteLabels, srt=45, xpd=NA, adj=c(1, 0.5), cex=0.9)
	label(.05, .95, 'B', font=2, adj=c(0.5,0.5))
}

fig3  <-  function() {
	coords    <-  readFile('data/sites_coords.csv')
	###############################
	# LABELING SITES AND DIET ITEMS
	###############################
	siteOrder  <-  c('puerto_rico', 'tamandare', 'salvador', 'abrolhos', 'guarapari', 'arraial', 'florianopolis')
	siteLabel  <-  c('Puerto Rico', 'Tamandaré', 'Salvador', 'Abrolhos', 'Guarapari', 'A. do Cabo', 'Florianópolis')
	foodOrder  <-  c('EAM','MCRU','MFOL','MCOR','MCT','MCA','ANT_OCT','ANT_ZOA','ANT_SCL','POR','SAND')

	#######
	# GRAPH
	#######
	par(mfrow=c(8, 1), omi=c(rep(0.5, 3), 0.8), mai=c(0, 0.53, 0, 0.1), cex=1, cex.axis=0.8, cex.lab=0.8)

	for(i in seq_along(siteOrder)) {
		results1  <-  results[results$site == siteOrder[i], ]
		results1  <-  results1[match(foodOrder, results1$food), ]
		prop1     <-  proportions[proportions$site == siteOrder[i], ]
		prop1     <-  prop1[match(foodOrder, prop1$food), ]
		results1$fit[prop1$selection == 'NaN']  <-  -10 #won't show up on the plot
		results1$se[prop1$selection == 'NaN']   <-  -10 #won't show up on the plot
		results1$fit[prop1$selection == 'Inf']  <-   62 #arbitrary value, high selectivity
	
		plotCI(seq_along(foodOrder)[results1$fit <= 8], results1$fit[results1$fit <= 8], uiw=1.96*results1$se[results1$fit <= 8], xaxt='n', yaxt='n', xlab='Food item', ylab='', pch=coords$shape[coords$site == siteOrder[i]], col='black', pt.bg=coords$color[coords$site == siteOrder[i]], ylim=c(-0.5,10), las=1, cex=0.8)
		axis(2, at=seq(0, 8, 2), las=1)
		outLierX  <-  seq_along(foodOrder)[results1$fit > 8]
		outLierY  <-  results1$fit[results1$fit > 8]
		if(length(outLierX) > 0) {
			plotCI(outLierX, rep(8, length(outLierX)), uiw=rep(1.5, length(outLierX)), add=TRUE, axes=FALSE, xlab='', ylab='', pch=coords$shape[coords$site == siteOrder[i]], col='black', pt.bg=coords$color[coords$site == siteOrder[i]], cex=0.8)
			text(outLierX-0.25, rep(6.5, length(outLierX)), round(results1$fit[results1$fit > 8] - 1.96*results1$se[results1$fit > 8]), adj=c(1, 0.5), cex=0.5)
			text(outLierX-0.25, rep(9.5, length(outLierX)), round(results1$fit[results1$fit > 8] + 1.96*results1$se[results1$fit > 8]), adj=c(1, 0.5), cex=0.5)
			text(outLierX+0.25, rep(8, length(outLierX)), round(outLierY), adj=c(0, 0.5), cex=0.5)
		}
		label(0.04, 0.85, siteLabel[i], adj=c(0, 0.5), cex=0.85)
		abline(h=1, lty=3)
	}
	axis(1, seq_along(foodOrder), rep('', length(foodOrder)), las=2)
	text(seq_along(foodOrder), rep(-3, length(foodOrder)), gsub('_', ' ', foodOrder), xpd=NA, cex=0.8, srt=45, adj=c(1, 1))
	mtext('Strength selection', outer=TRUE, side=2, cex=1.5)
	mtext('Food item', outer=TRUE, side=1, cex=1.5, line=-0.5)
}

fig4  <-  function() {
	gut     <-  readFile('data/gut_length_all.csv')
	gcs     <-  readFile('data/gut_length_c_striatus_raw.csv')
	gcs$qi  <-  gcs$gut_length_cm / gcs$size_cm
	par(omi=c(0.5, 0.5, 0.3, 0.2), cex=1, cex.lab=1.2, cex.axis=0.9, mai=c(0.5, 0.62, 0.72, 0))

	plot(0, 0, type='n', xlim=c(0.5, nrow(gut)+1), ylim=c(0.7, 9), xlab ='', ylab='Relative gut length (mm / mm)', yaxt='n', xaxt='n', yaxs='i', xpd=NA, axes=FALSE)
	usr  <-  par('usr')
	rect(usr[1], usr[3], usr[2], usr[4], col='grey90', border=NA)
	box()
	axis(2, at=seq(1,9,2), las=1)

	labs  <-  sub('\\n', '\n', gut$type, fixed=TRUE)
	text(1:nrow(gut), 0.3, labs, srt=45, xpd=NA, adj=c(1, 0.5), cex=0.8, font=c(1,1,1,3,1,1))
	for(i in 1:nrow(gut)) {
		
		if(i == 4) {
			avGcs  <-  draw95Ci(gcs, 'qi')
			points(jitter(rep(i, nrow(gcs)), amount=.05), gcs$qi, pch=21, col='white', bg=make.transparent('white', 0.7), cex=1.5, lwd=0.5)
			lines(rep(i, 2), avGcs[2:3], lty=2, col='dodgerblue4', lwd=1.5)
			lines(c(i-.05, i+.05), rep(avGcs[2], 2), col='dodgerblue2', lwd=1.5)
			lines(c(i-.05, i+.05), rep(avGcs[3], 2), col='dodgerblue2', lwd=1.5)
			points(i, avGcs[1], cex=1.5, pch=21, col='dodgerblue4', bg='dodgerblue2', lwd=0.5)
		} else {
			polygon(c(i-0.4,i-0.4,i+0.4,i+0.4), c(-1, gut$average[i], gut$average[i], -1), col=make.transparent('white', 0.8), border='black')	
			lines(rep(i,2), c(gut$average[i] - + gut$se[i]*1.96, gut$average[i] + gut$se[i]*1.96))
			lines(c(i-0.05, i+0.05), rep(gut$average[i]+gut$se[i]*1.96, 2))
			lines(c(i-0.05, i+0.05), rep(gut$average[i]-gut$se[i]*1.96, 2))
		}

	}

}

fig5  <-  function() {
	avRnaPersite  <-  ddply(rna, .(site), draw95Ci, response='ln_r_d_ratio')[c(2,3,1,4), ]
	labs          <-  c('Puerto Rico', 'Salvador', 'Guarapari', 'Florianópolis')
	par(omi=c(0.5, 0.5, 0.3, 0.2), cex=1, cex.lab=1.2, cex.axis=0.9, mai=c(0.5, 0.62, 0.72, 0))
	plot(NA, xlim=c(0.7, 4.3), ylim=c(0, 2), xlab='', ylab='RNA / DNA ratio', xpd=NA, axes=FALSE)
	usr  <-  par('usr')
	rect(usr[1], usr[3], usr[2], usr[4], col='grey90', border=NA)
	box()
	axis(1, at=1:4, labels=NA)
	axis(2, at=seq(0,2,0.5), labels=rounded(exp(seq(0,2,0.5))), las=1)

	for(k in seq_along(avRnaPersite)) {
		y  <-  rna$ln_r_d_ratio[rna$site == avRnaPersite$site[k]]
		points(jitter(rep(k, length(y)), amount=.05), y, pch=21, col='white', bg=make.transparent('white', 0.8), cex=1.4, lwd=0.5)
		lines(rep(k, 2), c(avRnaPersite$ci2.5[k], avRnaPersite$ci97.5[k]), lty=2, col='dodgerblue4', lwd=1.5)
		lines(c(k-.05, k+.05), rep(avRnaPersite$ci2.5[k], 2), col='dodgerblue2', lwd=1.5)
		lines(c(k-.05, k+.05), rep(avRnaPersite$ci97.5[k], 2), col='dodgerblue2', lwd=1.5)
		points(k, avRnaPersite$mean[k], cex=1.5, pch=21, col='dodgerblue4', bg='dodgerblue2')
		if(avRnaPersite$site[k] == 'sal')
			text(k, (avRnaPersite$ci97.5[k] + 0.15*avRnaPersite$ci97.5[k]), '*', cex=1.5, col='dodgerblue2')
	}
	text(1:4, rep(-0.2, 4), labs, srt=45, xpd=NA, adj=c(1,1))
}

figS1Plots  <- 	function(data, foodOrder, siteLabel, siteOrder) {
	data  <-  data[match(foodOrder, data$item), ]
	bp    <-  barplot(data$mean, ylim=c(0, 60), las=1, xlab='', ylab='', xaxt='n', yaxt='n')
	box()
	for(i in 1:nrow(data)) {
		lines(rep(bp[i], 2), c(data$mean[i] - 1.96*data$se[i], data$mean[i] + 1.96*data$se[i]))
		lines(c(bp[i]-0.1, bp[i]+0.1), rep(data$mean[i] - 1.96*data$se[i], 2))
		lines(c(bp[i]-0.1, bp[i]+0.1), rep(data$mean[i] + 1.96*data$se[i], 2))
	}
	site  <-  siteLabel[siteOrder == unique(data$site)]
	label(0.05, 0.9, site, adj=c(0, 0.5), cex=0.7)
	axis(2, at=seq(10,50,20), las=1)
}

figS1  <-  function() {
	###############################
	# LABELING SITES AND DIET ITEMS
	###############################
	siteOrder  <-  c('puerto_rico', 'tamandare', 'salvador', 'abrolhos', 'guarapari', 'arraial', 'florianopolis')
	siteLabel  <-  c('Puerto Rico', 'Tamandaré', 'Salvador', 'Abrolhos', 'Guarapari', 'A. do Cabo', 'Florianópolis')
	foodOrder  <-  c('EAM','MCRU','MFOL','MCOR','MCT','MCA','ANT_OCT','ANT_ZOA','ANT_SCL','POR','SAND')
	foodLabs   <-  c('EAM','MCRU','MFOL','MCOR','MCT','MCA','ANT OCT','ANT ZOA','ANT SCL','POR','SAND')

	#######
	# GRAPH
	#######
	nums                     <-  seq_along(siteOrder)
	summaryNewCover$siteNum  <-  nums[match(summaryNewCover$site, siteOrder)]
	par(mfrow=c(8, 1), omi=c(rep(0.5, 3), 0.8), mai=c(0, 0.53, 0, 0.1), cex=1, cex.axis=0.8, cex.lab=0.8)
	ddply(summaryNewCover, .(siteNum), figS1Plots, foodOrder=foodOrder, siteLabel=siteLabel, siteOrder=siteOrder)	

	xpos=c(0.7,1.9,3.1,4.3,5.5,6.7,7.9,9.1,10.3,11.5,12.7)
	axis(1, at=xpos, labels=NA)
	text(xpos, rep(-8, length(foodLabs)), foodLabs, xpd=NA, cex=0.8, srt=45, adj=c(1, 1))
	mtext(expression(paste('Cover (%, mean '%+-%' 95% CI)')), outer=TRUE, side=2, cex=1.2)
	mtext('Food item', outer=TRUE, side=1, cex=1.2, line=-1.5)
}
