visualizeLatencyQuantilesVsCardinality = function(queryData) {
	median = getLatencyQuantileForEachCardinalityValue(queryData, 0.5)
	q90 = getLatencyQuantileForEachCardinalityValue(queryData, 0.9)
	q99 = getLatencyQuantileForEachCardinalityValue(queryData, 0.99)
	
	cardinalityValues = getCardinalityValues(queryData)
	
	xlim=c(0,1.1*max(cardinalityValues))
	ylim=c(0,1.1*max(median, q90, q99))
	
	par(mar=c(5,5,4,2)+0.1)
	plot(cardinalityValues, median, col=0, xlim=xlim, ylim=ylim, xlab="Cardinality", ylab="Latency (ms)", main="Latency vs. Cardinality")
	lines(cardinalityValues, median, lwd=2, col="red", pch=19)
	lines(cardinalityValues, q90, lwd=2, col="blue", pch=19)
	lines(cardinalityValues, q99, lwd=2, col="green", pch=19)
	
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
		quantiles[i] = quantile(queryData$elapsedTime[which(queryDataInMs$rangeLength == cardinalityValues[i])], quantile)
	}
	
	return(quantiles)
}
