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
	plot(cardinalityValues, quantiles[1,], xlim=c(0,100), ylim=c(0,30), col=0, xlab="Cardinality", ylab="Latency (ms)", main=paste(queryType, ": Latency vs. Cardinality", sep=""))
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