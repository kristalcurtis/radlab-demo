visualizeLatencyQuantilesVsCardinality = function(queryData, queryType) {
	cardinalityValues = getCardinalityValues(queryData)
	numCardinalityValues = length(cardinalityValues)

	quantiles = matrix(nrow=3, ncol=numCardinalityValues)
	
	quantiles[1,] = getLatencyQuantileForEachCardinalityValue(queryData, 0.5)
	quantiles[2,] = getLatencyQuantileForEachCardinalityValue(queryData, 0.9)
	quantiles[3,] = getLatencyQuantileForEachCardinalityValue(queryData, 0.99)
	
	#barplot(quantiles, beside=TRUE, names.arg=cardinalityValues, col=c("red", "blue", "green"), xlab="Cardinality", ylab="Latency (ms)", main="Latency vs. Cardinality")
	#legend("topright", legend=c("median", "90th", "99th"), col=c("red", "blue", "green"), lwd=2)
	
	par(mar=c(5,5,4,2)+0.1)
	plot(cardinalityValues, quantiles[1,], ylim=c(0,30), col=0, xlab="Cardinality", ylab="Latency (ms)", main=paste(queryType, ": Latency vs. Cardinality", sep=""))
	lines(cardinalityValues, quantiles[1,], type="o", col="red", lwd=2)
	lines(cardinalityValues, quantiles[2,], type="o", col="blue", lwd=2)
	lines(cardinalityValues, quantiles[3,], type="o", col="green", lwd=2)
	
	legend("topright", legend=c("median", "90th", "99th"), col=c("red", "blue", "green"), lwd=2, pch="o")
}

getCardinalityValues = function(queryData) {
	return(unique(queryData$cardinality))	
}

getLatencyQuantileForEachCardinalityValue = function(queryData, quantile) {
	cardinalityValues = getCardinalityValues(queryData)
	numCardinalityValues = length(cardinalityValues)
	
	quantiles = vector(length=numCardinalityValues)
	
	for (i in 1:numCardinalityValues) {
		quantiles[i] = quantile(getDataForGivenCardinalityValue(queryData, cardinalityValues[i]), quantile)
	}
	
	return(quantiles)
}

getDataForGivenCardinalityValue = function(queryData, cardinalityValue) {
	return(queryData$elapsedTime[which(queryData$cardinality == cardinalityValue)])
}

# for plotting thoughtstream (2D input space)

plotThoughtstreamSensitivityAnalysis = function(queryData, quantile, queryType) {
	sensitivityAnalysisMatrix = getLatencySensitivityAnalysisMatrix(queryData, quantile)
	persp(unique(queryData$numSubscriptions), unique(queryData$numPerPage), sensitivityAnalysisMatrix, theta=30, phi=30, expand=0.5, col="lightblue", ticktype="detailed", zlim=c(0,1.2*max(sensitivityAnalysisMatrix)), xlab="# Subscriptions Per User", ylab="# Thoughts Per Page", zlab="Latency (ms)")
	mtext(paste(queryType, " latency, q=", quantile, sep=""), side=3)
}

getQueryLatencyForGivenCardinalityTuple = function(queryData, numSubscriptions, numPerPage) {
	queryDataFixedNumSubs = queryData[queryData$numSubscriptions == numSubscriptions,]
	queryDataFixedNumSubsAndNumPerPage = queryDataFixedNumSubs[queryDataFixedNumSubs$numPerPage == numPerPage, "elapsedTime"]
	return(queryDataFixedNumSubsAndNumPerPage)
}

getLatencySensitivityAnalysisMatrix = function(queryData, quantile) {
	numSubscriptions = unique(queryData$numSubscriptions)
	numPerPage = unique(queryData$numPerPage)
	
	latencyMatrix = matrix(nrow=length(numSubscriptions), ncol=length(numPerPage))
	
	for (s in 1:length(numSubscriptions)) {
		for (p in 1:length(numPerPage)) {
			latencyMatrix[s,p] = quantile(getQueryLatencyForGivenCardinalityTuple(queryData, numSubscriptions[s], numPerPage[p]), quantile)
		}
	}
	
	return(latencyMatrix)
}


# for merging results from several trace collectors into a single matrix
getSingleDataset = function(basePath) {
	setwd(basePath)
	files = list.files(basePath)
	
	print(1)
	data = as.data.frame(read.csv(files[1]))
	
	for (i in 2:length(files)) {
		print(i)
		newdata = as.data.frame(read.csv(file=files[i]))
		data = rbind(data, newdata)
	}
	
	return(data)
}