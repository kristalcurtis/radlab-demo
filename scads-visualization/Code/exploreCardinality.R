queryFilename = "/Users/ksauer/Desktop/piqltrace.avro-4094216517284-parsed.csv"

queryData = as.data.frame(read.csv(queryFilename))

dim(queryData)
colnames(queryData)

for (i in unique(queryData$rangeLength)) {
	print(length(which(queryData$rangeLength == i)))
}

setwd("/Users/ksauer/Desktop/radlab-demo/scads-visualization/Code")
source("visualization-functions.R")

queryDataInMs = convertTimesFromNanosecondsToMilliseconds(queryData)

dim(queryDataInMs)

for (i in unique(queryData$rangeLength)) {
	print(i)
	print(paste("median=", median(queryDataInMs$elapsedTime[which(queryDataInMs$rangeLength == i)])))
	print(paste("90th=", quantile(queryDataInMs$elapsedTime[which(queryDataInMs$rangeLength == i)], 0.9)))
	print(paste("99th=", quantile(queryDataInMs$elapsedTime[which(queryDataInMs$rangeLength == i)], 0.99)))
}

visualizeLatencyQuantilesVsCardinality = function(queryData) {
	median = getLatencyQuantileForEachCardinalityValue(queryData, 0.5)
	q90 = getLatencyQuantileForEachCardinalityValue(queryData, 0.9)
	q99 = getLatencyQuantileForEachCardinalityValue(queryData, 0.99)
	
	plot(getCardinalityValues(queryData), q99)
}

getCardinalityValues = function(queryData) {
	return(unique(queryData$rangeLength))	
}

getLatencyQuantileForEachCardinalityValue = function(queryData, quantile) {
	cardinalityValues = getCardinalityValues(queryData)
	numCardinalityValues = length(cardinalityValues)
	
	quantiles = vector(length=numCardinalityValues)
	
	for (i in cardinalityValues) {
		quantiles[i] = quantile(queryData$elapsedTime[which(queryDataInMs$rangeLength == i)], quantile)
	}
	
	return(quantiles)
}

visualizeLatencyQuantilesVsCardinality(queryDataInMs)