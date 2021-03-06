#miniTraceDataFilename = "~/Desktop/radlab-demo/scads-visualization/Data/Processed/miniTraceData.csv"
miniTraceDataFilename = "~/Desktop/radlab-demo/scads-visualization/Data/Processed/miniTraceDataWithElapsedTimes.csv"
miniTraceData = as.data.frame(read.csv(miniTraceDataFilename))

colnames(miniTraceData)

# first: make timestamps relative to first timestamp

minTimestamp = min(miniTraceData$timestamp)

miniTraceData$timestamp = miniTraceData$timestamp - minTimestamp

miniTraceData[1:14,]

# next: figure out how to correlate beginning & end of event
unique(miniTraceData$operationType[1:14])

traceData = miniTraceData
operationType = "getQuery100000"
getElapsedTime = function(traceData, operationType) {
	times = traceData$timestamp[which(traceData$operationType == operationType)]
	
}
# UPDATE:  no longer necessary -- did this in Scala

# next: visualize

query1Data = miniTraceData[1:7,]
max(query1Data$timestamp + query1Data$elapsedTime)

plot(c(0,750000), c(0,100), col=0, xlab="Time (ns)", ylab="", main="Visualization of GET query")
lines(c(query1Data$timestamp[query1Data$type == "query"], query1Data$elapsedTime[query1Data$type == "query"]), c(75, 75), lw=2, col="red")

for (i in 1:6) {
	if (query1Data$type[i] == "iterator") {
		lines(c(query1Data$timestamp[i], query1Data$timestamp[i] + query1Data$elapsedTime[i]), c(50, 50), lw=2, col="blue")
	} else if (query1Data$type[i] == "message") {
		lines(c(query1Data$timestamp[i], query1Data$timestamp[i] + query1Data$elapsedTime[i]), c(25, 25), lw=2, col="purple")
	}
}

legend("topright", legend=c("query", "iterator", "message"), col=c("red", "blue", "purple"), lw=2)

# testing functions in visualization-functions.R
setwd("~/Desktop/radlab-demo/scads-visualization/Code")
source("visualization-functions.R")

getQueryIndices(miniTraceData)

getIndexOfGivenQuery(miniTraceData, "getQuery100000")
getIndexOfGivenQuery(miniTraceData, "getRangeQuery100000")

getIndexOfPreviousQuery(miniTraceData, "getQuery100000")
getIndexOfPreviousQuery(miniTraceData, "getRangeQuery100000")

getQueryData = normalizeTimestampsToMin(getAllEventsForGivenQuery(miniTraceData, "getQuery100000"))
getAllEventsForGivenQuery(miniTraceData, "getRangeQuery100000")


#getQueryData = getAllEventsForGivenQuery(miniTraceData, "getQuery100000")
getQueryData = normalizeTimestampsToMin(getAllEventsForGivenQuery(miniTraceData, "getQuery100000"))
pdf("~/Desktop/getQueryVisualization.pdf")
plotAllEventsForGivenQuery(getQueryData)
dev.off()

getRangeQueryData = normalizeTimestampsToMin(getAllEventsForGivenQuery(miniTraceData, "getRangeQuery100000"))
pdf("~/Desktop/getRangeQueryVisualization.pdf")
plotAllEventsForGivenQuery(getRangeQueryData)
dev.off()



# look at plots for a few different queries of each type
smallTraceDataFilename = "~/Desktop/radlab-demo/scads-visualization/Data/Processed/smallTraceDataWithElapsedTimes.csv"
smallTraceData = as.data.frame(read.csv(smallTraceDataFilename))

queryIds = smallTraceData$id[which(smallTraceData$type == "query")]
getQueries = grep("getQuery", smallTraceData$id[which(smallTraceData$type == "query")])
getRangeQueries = grep("getRangeQuery", smallTraceData$id[which(smallTraceData$type == "query")])

for (i in 1:length(queryIds)) {
	pdf(paste("~/Desktop/", queryIds[i], ".pdf", sep=""))
	plotAllEventsForGivenQuery(normalizeTimestampsToMin(getAllEventsForGivenQuery(smallTraceData, queryIds[i])))
	dev.off()
}


# checking out the join query
# requires using a separate line for each iterator

smallTraceDataFilename = "/Users/ksauer/Desktop/radlab-demo/scads-visualization/Data/Processed/smallTraceDataWithElapsedTimesFromHugeTrace.csv"
smallTraceData = as.data.frame(read.csv(smallTraceDataFilename))

dim(smallTraceData)
smallTraceData[1:10,]

# figure out how many distinct iterators there are
library(stringr)
?str_split
functionNameElements = str_split(smallTraceData[1,4], ":")
functionNameElements[[1]][1]

paste(functionNameElements[[1]][1], functionNameElements[[1]][2], sep=":")

setwd("~/Desktop/radlab-demo/scads-visualization/Code")
source("visualization-functions.R")

getUniqueIterators(smallTraceData[which(smallTraceData[,"type"] == "iterator"),"id"])

joinQueryData = normalizeTimestampsToMin(convertTimesFromNanosecondsToMilliseconds(getAllEventsForGivenQuery(smallTraceData, "joinQuery269399")))
joinQueryData = getAllNormalizedEventsForGivenQuery(smallTraceData, "joinQuery269399")
getUniqueIterators(joinQueryData[which(joinQueryData[,"type"] == "iterator"),"id"])
getMapFromPlotElementToYCoord(joinQueryData)

plotAllEventsForGivenQuery(joinQueryData)
getMapFromPlotElementToYCoord(joinQueryData)

queryData = joinQueryData


numberOfMessagesInQuery(joinQueryData)
getMapFromPlotElementToYCoord(joinQueryData)

# figure out how to label iterator lines
uniqueIterators = getUniqueIterators(getIteratorEventIdsFromQueryData(joinQueryData))
map = getMapFromPlotElementToYCoord(joinQueryData)
mtext(uniqueIterators, side=2, at=c(86, 111), line=1, las=1)

pdf("~/Desktop/joinQuery.pdf", width=20, height=10)
plotAllEventsForGivenQuery(joinQueryData)
dev.off()
labelIteratorsOnPlotWithAllQueryEvents(joinQueryData)

# make sure it still works for getQuery, getRangeQuery
getQueryData = getAllNormalizedEventsForGivenQuery(smallTraceData, "getQuery269400")
getRangeQueryData = getAllNormalizedEventsForGivenQuery(smallTraceData, "getRangeQuery269400")


pdf("~/Desktop/getQuery.pdf", width=20, height=10)
plotAllEventsForGivenQuery(getQueryData)
dev.off()

pdf("~/Desktop/getRangeQuery.pdf", width=20, height=10)
plotAllEventsForGivenQuery(getRangeQueryData)
dev.off()

## seeing if 2nd get range is gone
tracePath = "/Users/ksauer/Desktop/piqltraceWithElapsedTimes.csv"
newTraceData = as.data.frame(read.csv(tracePath))

setwd("~/Desktop/radlab-demo/scads-visualization/Code")
source("visualization-functions.R")

getRangeQueryData = getAllNormalizedEventsForGivenQuery(newTraceData, "getRangeQuery8")

pdf("~/Desktop/getRangeQuery2.pdf", width=20, height=10)
plotAllEventsForGivenQuery(getRangeQueryData)
dev.off()

# checking out merge sort join query
tracePath = "/Users/ksauer/Desktop/piqltraceWithElapsedTimes2.csv"
newTraceData = as.data.frame(read.csv(tracePath))

mergeSortJoinQueryData = getAllNormalizedEventsForGivenQuery(newTraceData, "mergeSortJoinQuery4")

pdf("~/Desktop/mergeSortJoinQuery2.pdf", width=20, height=10)
plotAllEventsForGivenQuery(mergeSortJoinQueryData)
dev.off()


# look at thoughtstream query
setwd("~/workspace/radlab-demo/scads-visualization/Code")
source("visualization-functions.R")

tracePath = "/Users/kcurtis/Desktop/thoughtstreamQueryDataTrace1.csv"
traceData = getAllNormalizedEventsForGivenQuery(as.data.frame(read.csv(tracePath)), "thoughtstream1-100-10")
dim(traceData)

plotAllEventsForGivenQuery(traceData)

numberOfMessagesInQuery(traceData)


# look at debugged thoughtstream query
setwd("~/workspace/radlab-demo/scads-visualization/Code")
source("visualization-functions.R")

tracePath = "/Users/kcurtis/Desktop/thoughtstreamQueryDataTrace.csv"
traceData = getAllNormalizedEventsForGivenQuery(as.data.frame(read.csv(tracePath)), "thoughtstream1-100-10")
dim(traceData)

plotAllEventsForGivenQuery(traceData)

numberOfMessagesInQuery(traceData)
