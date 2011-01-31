visualizeLatencyQuantilesVsCardinality = function(queryData) {
	cardinalityValues = getCardinalityValues(queryData)
	numCardinalityValues = length(cardinalityValues)

	quantiles = matrix(nrow=3, ncol=numCardinalityValues)
	
	quantiles[1,] = getLatencyQuantileForEachCardinalityValue(queryData, 0.5)
	quantiles[2,] = getLatencyQuantileForEachCardinalityValue(queryData, 0.9)
	quantiles[3,] = getLatencyQuantileForEachCardinalityValue(queryData, 0.99)
	
	barplot(quantiles, beside=TRUE, names.arg=cardinalityValues, col=c("red", "blue", "green"), xlab="Cardinality", ylab="Latency (ms)", main="Latency vs. Cardinality")
	legend("topright", legend=c("median", "90th", "99th"), col=c("red", "blue", "green"), lwd=2)
}

getCardinalityValues = function(queryData) {
	return(unique(queryData$rangeLength))	
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
	return(queryData$elapsedTime[which(queryData$rangeLength == cardinalityValue)])
}