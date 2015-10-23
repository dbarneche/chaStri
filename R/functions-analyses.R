#########
# GENERAL
#########
readFile  <-  function(path, ...) {
	read.csv(path, header=TRUE, stringsAsFactors=FALSE, ...)
}

draw95Ci  <-  function(x, response) {
	av      <-  mean(x[[response]])
	se      <-  sd(x[[response]])/sqrt(length(x[[response]]))
	ci2_5   <-  av - 1.96*se
	ci97_5  <-  av + 1.96*se
	data.frame(mean=av, ci2.5=ci2_5, ci97.5=ci97_5)
}

rmFcts  <-  function(filename) {
  source(filename, local=TRUE)
  fct_files  <-  ls()
  rm(list=ls(.GlobalEnv)[ls(.GlobalEnv) %in% fct_files], envir=.GlobalEnv)
}

##########
# ANALYSIS
##########
getCoefTab  <-  function(newLevel, data, y, model) {
	data$site  <-  as.factor(data$site)
	data$site  <-  relevel(data$site, newLevel)
	mod        <-  eval(parse(text = model))
	coefs      <-  coef(summary(mod))
	colnames(coefs)  <-  c('Estimate', 'StEr', 'tVal', 'pVal')
	cbind(data.frame(reference=newLevel, sites=levels(data$site), coefs), stringsAsFactors=FALSE)
}

fixOrder  <-  function(x) {
	x  <-  sort(strsplit(x, ' - ')[[1]])
	paste(x[1], '-', x[2])
}

significant  <-  function(tab) {
	z  <-  tab$pVal < 0.05
	z[1]     <-  TRUE
	qual     <-  which(z)
	signifs  <-  paste(tab$sites[1], '-', tab$sites[qual])
	signifs  <-  sapply(signifs, fixOrder)
	pTab     <-  tab[qual, c('Estimate', 'StEr', 'tVal', 'pVal')]
	data.frame(sites=signifs, pTab, stringsAsFactors=FALSE)
}

cleanEquals  <-  function(data, column) {
	equals  <-  sapply(strsplit(data[[column]], ' - '), function(x)x[1]==x[2])
	data    <-  data[!equals, ]
	data    <-  data[!duplicated(data[[column]]), ]
}

signifGLM  <-  function(sites, data, y, mod) {
	coefTab  <-  ldply(sites, getCoefTab, data=data, y=y, mod=mod)
	results  <-  ddply(coefTab, .(reference), significant)
	results  <-  cleanEquals(results, 'sites')
	results[order(results$sites), ]
}

################
# BOOTSTRAP CODE
################
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

bootstrapDensity  <-  function(data=abun, iter=1000) {
	set.seed(1)
	listOfTransectInfo  <-  dlply(data, .(site), transectInfo)
	responses           <-  vector(mode='list', length=iter)
	meanParameters      <-  data.frame(stringsAsFactors=FALSE)
	MCMCParameters      <-  data.frame(stringsAsFactors=FALSE)

	for(j in seq_len(iter)) {
		standardisedData          <-  ldply(listOfTransectInfo, standardisedTransects)
		standardisedData$siteNum  <-  as.numeric(as.factor(standardisedData$site))
		modelPoissonJAGS          <-  runPoissonJAGS(standardisedData)
		responses[[j]]            <-  standardisedData
		meanParameters            <-  rbind(meanParameters, extractMeanEstimates(modelPoissonJAGS, j))
		MCMCParameters            <-  rbind(MCMCParameters, extractMCMCEstimates(modelPoissonJAGS, j))
	}
	list(data            =  responses,
		 meanParameters  =  prettyOutputs(meanParameters, standardisedData),
		 MCMCParameters  =  prettyOutputs(MCMCParameters, standardisedData))
}

###################
# BAYESIAN ANALYSIS
###################
poissonModel  <-  function() {
	for(i in 1:length(abun)) {
		abun[i]     ~   dpois(mu[i])
		log(mu[i])  <-  b[siteNum[i]] #log link
	}
	for(j in 1:max(siteNum)) {
		b[j] ~ dnorm(0, 0.00001) #prior
	}
}

runPoissonJAGS  <-  function(standardisedData) {
	dJags       <-  list('abun'=standardisedData$abun,
						 'siteNum'=standardisedData$siteNum)
	dfit        <-  jags(data=dJags, parameters.to.save=paste0('b[', seq_len(max(standardisedData$siteNum)), ']'), model.file=poissonModel, n.chains=3, n.iter=1e4, DIC=TRUE, n.thin=30)
	autojags(dfit, n.iter=1e4, n.thin=30, n.update=100)
}

extractMeanEstimates  <-  function(modelPoissonJAGS, iterNumber) {
	data.frame(t(data.frame(c(iter=iterNumber, modelPoissonJAGS$BUGSoutput$summary[-8,'mean']))), row.names=NULL)
}

extractMCMCEstimates <-  function(modelPoissonJAGS, iterNumber) {
	cbind(iter=iterNumber, data.frame(modelPoissonJAGS$BUGSoutput$sims.matrix)[,-8])
}

prettyOutputs  <-  function(output, standardisedData) {
	prettyOutput   <-  data.frame(stringsAsFactors=FALSE)
	names(output)  <-  c('iter', sort(unique(standardisedData$site)))
	for(j in 2:ncol(output)) {
		prettyOutput  <-  rbind(prettyOutput, data.frame(site=names(output)[j], iter=output[,1], estimates=output[,j], stringsAsFactors=FALSE, row.names=NULL))
	}
	prettyOutput
}

#########
# TABLE 2
#########
drawTable2  <-  function(data, response) {

	coefTab           <-  ldply(unique(data$site), getCoefTab, data=data, y='bites', mod='glm.nb(data[[y]] ~ data$site)')
	table2            <-  data.frame(matrix(0, 7, 7))
	rownames(table2)  <-  sort(unique(data$site))
	colnames(table2)  <-  rownames(table2)

	for(i in 1:nrow(table2)) {
		reference  <-  rownames(table2)[i]
		places     <-  which(!(colnames(table2) %in% reference))
		sites      <-  colnames(table2)[!(colnames(table2) %in% reference)]
		results    <-  vector('numeric', length=length(sites))
		for(j in seq_along(results)) {
			results[j]  <-  coefTab[coefTab$reference == reference & coefTab$sites == sites[j], response]
		}
		table2[i, places]  <-  results
	}
	table2
}

