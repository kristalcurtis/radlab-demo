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


source("exploreCardinalityFunctions.R")
visualizeLatencyQuantilesVsCardinality(queryDataInMs)

getLatencyQuantileForEachCardinalityValue(queryDataInMs, 0.5)

getCardinalityValues(queryDataInMs)


# try again with a different run
queryFilename = "/Users/ksauer/Desktop/piqltrace.avro-1296073176519.csv"
queryData = as.data.frame(read.csv(queryFilename))
queryDataInMs = convertTimesFromNanosecondsToMilliseconds(queryData)
source("exploreCardinalityFunctions.R")
visualizeLatencyQuantilesVsCardinality(queryDataInMs)


cardinalityValues = getCardinalityValues(queryDataInMs)

for (i in 1:length(cardinalityValues)) {
	print(cardinalityValues[i])
	print(summary(getDataForGivenCardinalityValue(queryDataInMs, cardinalityValues[i])))
}


# try again with different run -- this time, we have a larger cardinality at the end
queryFilename = "/Users/ksauer/Desktop/piqltrace.avro-1296165307356.csv"
queryData = as.data.frame(read.csv(queryFilename))
queryDataInMs = convertTimesFromNanosecondsToMilliseconds(queryData)

source("exploreCardinalityFunctions.R")

pdf("~/Desktop/piqltrace.avro-1296165307356.pdf")
visualizeLatencyQuantilesVsCardinality(queryDataInMs)
dev.off()

summary(getDataForGivenCardinalityValue(queryDataInMs, 10))



# making plot for join query
setwd("/Users/ksauer/Desktop/radlab-demo/scads-visualization/Code")
source("visualization-functions.R")
source("exploreCardinalityFunctions.R")

queryFilename = "/Users/ksauer/Desktop/piqltrace.avro-1296262054148.csv"

queryData = as.data.frame(read.csv(queryFilename))
queryDataInMs = convertTimesFromNanosecondsToMilliseconds(queryData)

pdf(paste(queryFilename, ".pdf", sep=""))
visualizeLatencyQuantilesVsCardinality(queryDataInMs)
dev.off()

# try again with reversed cardinality list
queryFilename="/Users/ksauer/Desktop/piqltrace.avro-1296264746793.csv"

queryData = as.data.frame(read.csv(queryFilename))
queryDataInMs = convertTimesFromNanosecondsToMilliseconds(queryData)

pdf(paste(queryFilename, ".pdf", sep=""))
visualizeLatencyQuantilesVsCardinality(queryDataInMs)
dev.off()
