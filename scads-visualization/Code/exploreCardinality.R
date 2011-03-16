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


<<<<<<< Updated upstream
# 2.11.10
# look at myThoughts query data

queryFilename="/Users/ksauer/Desktop/myThoughtsData.csv"

setwd("/Users/ksauer/Desktop/radlab-demo/scads-visualization/Code")
source("visualization-functions.R")
source("exploreCardinalityFunctions.R")

queryData = as.data.frame(read.csv(queryFilename))
queryDataInMs = convertTimesFromNanosecondsToMilliseconds(queryData)

visualizeLatencyQuantilesVsCardinality(queryDataInMs, "myThoughts")

dim(queryData)
colnames(queryData)
queryData[1:10,]


# look at myThoughts message data (trace)
traceFilename="/Users/ksauer/Desktop/myThoughtsDataWithAllMessages.csv"

setwd("/Users/ksauer/Desktop/radlab-demo/scads-visualization/Code")
source("visualization-functions.R")
source("exploreCardinalityFunctions.R")

traceData = as.data.frame(read.csv(traceFilename))
dim(traceData)

queryData = getAllNormalizedEventsForGivenQuery(traceData, "myThoughts1")

plotAllEventsForGivenQuery(queryData)

=======
# look at thoughtstream query
setwd("/Users/kcurtis/workspace/radlab-demo/scads-visualization/Code")
source("visualization-functions.R")

thoughtstreamData = convertTimesFromNanosecondsToMilliseconds(as.data.frame(read.csv("~/Desktop/thoughtstreamQueryData.csv")))


# figuring out how to use persp for 3D plot

?persp

numPerPage = c(10,20)
numSubs = c(100,200)

thoughtstreamData100Subs = thoughtstreamData[thoughtstreamData$numSubscriptions == 100,]
thoughtstreamData100Subs10PerPage = thoughtstreamData100Subs[thoughtstreamData$numPerPage == 10,"elapsedTime"]
thoughtstreamData100Subs20PerPage = thoughtstreamData100Subs[thoughtstreamData$numPerPage == 20,"elapsedTime"]

thoughtstreamData200Subs = thoughtstreamData[thoughtstreamData$numSubscriptions == 200,]
thoughtstreamData200Subs10PerPage = thoughtstreamData200Subs[thoughtstreamData200Subs$numPerPage == 10,"elapsedTime"]
thoughtstreamData200Subs20PerPage = thoughtstreamData200Subs[thoughtstreamData200Subs$numPerPage == 20,"elapsedTime"]

medianLatency = matrix(nrow=2, ncol=2)
medianLatency[1,1] = median(thoughtstreamData100Subs10PerPage, na.rm=TRUE)
medianLatency[1,2] = median(thoughtstreamData100Subs20PerPage, na.rm=TRUE)
medianLatency[2,1] = median(thoughtstreamData200Subs10PerPage, na.rm=TRUE)
medianLatency[2,2] = median(thoughtstreamData200Subs20PerPage, na.rm=TRUE)

latency90th = matrix(nrow=2, ncol=2)
latency90th[1,1] = quantile(thoughtstreamData100Subs10PerPage, 0.9, na.rm=TRUE)
latency90th[1,2] = quantile(thoughtstreamData100Subs20PerPage, 0.9, na.rm=TRUE)
latency90th[2,1] = quantile(thoughtstreamData200Subs10PerPage, 0.9, na.rm=TRUE)
latency90th[2,2] = quantile(thoughtstreamData200Subs20PerPage, 0.9, na.rm=TRUE)

latency99th = matrix(nrow=2, ncol=2)
latency99th[1,1] = quantile(thoughtstreamData100Subs10PerPage, 0.99, na.rm=TRUE)
latency99th[1,2] = quantile(thoughtstreamData100Subs20PerPage, 0.99, na.rm=TRUE)
latency99th[2,1] = quantile(thoughtstreamData200Subs10PerPage, 0.99, na.rm=TRUE)
latency99th[2,2] = quantile(thoughtstreamData200Subs20PerPage, 0.99, na.rm=TRUE)

latency=latency99th
persp(numPerPage, numSubs, latency, theta=30, phi=30, expand=0.5, col="lightblue", ticktype="detailed", zlim=c(0,1.2*max(latency)))

# a couple of fake points, just to get a better sense for what it would look like
numPerPage = c(10,13,16,20)
numSubs = c(100, 133, 166, 200)

latency = matrix(nrow=4,ncol=4)
latency[1,] = c(115, 120, 123, 127)
latency[2,] = c(150, 180, 185, 190)
latency[3,] = c(180, 200, 205, 210)
latency[4,] = c(211, 220, 227, 233)

# TODO:  make this more automatic

# test out function to get data for given (numSubs, numPerPage) tuple
testLatency = matrix(nrow=2, ncol=2)
for (s in 1:2) {
	for (p in 1:2) {
		testLatency[s,p] = median(getQueryLatencyForGivenCardinalityTuple(thoughtstreamData, numSubs[s], numPerPage[p]))
	}
}

# test out function to get latency sensitivity analysis matrix

getLatencySensitivityAnalysisMatrix(thoughtstreamData, 0.5)
getLatencySensitivityAnalysisMatrix(thoughtstreamData, 0.9)
getLatencySensitivityAnalysisMatrix(thoughtstreamData, 0.99)

plotThoughtstreamSensitivityAnalysis(thoughtstreamData, 0.5)

# try plotting data for new thoughtstream run
setwd("/Users/kcurtis/workspace/radlab-demo/scads-visualization/Code")
source("visualization-functions.R")
source("exploreCardinalityFunctions.R")

filename = "~/Desktop/thoughtstream-modeling/thoughtstreamQueryDataMore.csv"
queryData = convertTimesFromNanosecondsToMilliseconds(as.data.frame(read.csv(filename)))

dim(queryData)

queryData[1:10,]

plotThoughtstreamSensitivityAnalysis(queryData, 0.5)

# check out mySubs data
setwd("/Users/kcurtis/workspace/radlab-demo/scads-visualization/Code")
source("visualization-functions.R")
source("exploreCardinalityFunctions.R")

filename = "~/Desktop/mySubsQueryData.csv"
queryData = convertTimesFromNanosecondsToMilliseconds(as.data.frame(read.csv(filename)))

pdf("~/Desktop/mySubs.pdf")
visualizeLatencyQuantilesVsCardinality(queryData, "mySubscriptions")
dev.off()

# check out thoughtstream data

setwd("/Users/kcurtis/workspace/radlab-demo/scads-visualization/Code")
source("visualization-functions.R")
source("exploreCardinalityFunctions.R")

filename = "~/Desktop/thoughtstream-modeling/thoughtstreamQueryData.csv"
thoughtstreamQueryData = convertTimesFromNanosecondsToMilliseconds(as.data.frame(read.csv(filename)))

pdf("~/Desktop/thoughtstream.pdf")
par(mfrow=c(1,2))
plotThoughtstreamSensitivityAnalysis(thoughtstreamQueryData, 0.5, "thoughtstream")
dev.off()


filename = "~/Desktop/thoughtstream-modeling/localUserThoughtstreamQueryData.csv"
localUserThoughtstreamQueryData = convertTimesFromNanosecondsToMilliseconds(as.data.frame(read.csv(filename)))

pdf("~/Desktop/localUserThoughtstream.pdf")
plotThoughtstreamSensitivityAnalysis(localUserThoughtstreamQueryData, 0.5, "localUserThoughtstream")
dev.off()

getLatencySensitivityAnalysisMatrix(thoughtstreamQueryData, 0.99)
getLatencySensitivityAnalysisMatrix(localUserThoughtstreamQueryData, 0.99)

getLatencySensitivityAnalysisMatrix(thoughtstreamQueryData, 0.9)
getLatencySensitivityAnalysisMatrix(localUserThoughtstreamQueryData, 0.9)

getLatencySensitivityAnalysisMatrix(thoughtstreamQueryData, 0.5)
getLatencySensitivityAnalysisMatrix(localUserThoughtstreamQueryData, 0.5)

# check out timelines
setwd("/Users/kcurtis/workspace/radlab-demo/scads-visualization/Code")
source("visualization-functions.R")
source("exploreCardinalityFunctions.R")

filename = "~/Desktop/thoughtstream-modeling/thoughtstreamTimeline.csv"
rawThoughtstreamTimeline = as.data.frame(read.csv(filename))
thoughtstreamTimeline = convertTimesFromNanosecondsToMilliseconds(normalizeTimestampsToMin(as.data.frame(read.csv(filename))))

pdf("~/Desktop/thoughtstreamTimeline.pdf", height=10, width=20)
plotAllEventsForGivenQuery(thoughtstreamTimeline)
dev.off()

numberOfMessagesInQuery(thoughtstreamTimeline)

filename = "~/Desktop/thoughtstream-modeling/localUserThoughtstreamTimeline.csv"
localUserThoughtstreamTimeline = convertTimesFromNanosecondsToMilliseconds(normalizeTimestampsToMin(as.data.frame(read.csv(filename))))

pdf("~/Desktop/localUserThoughtstreamTimeline.pdf", height=10, width=20)
plotAllEventsForGivenQuery(localUserThoughtstreamTimeline)
dev.off()

numberOfMessagesInQuery(localUserThoughtstreamTimeline)

localUserThoughtstreamQueryData[1:100,"elapsedTime"]

# look at localUserThoughtstreamTimeline for multiple queries; want to see a really long one

filename = "~/Desktop/thoughtstream-modeling/localUserThoughtstreamMultipleTimelines.csv"
localUserQueryData = convertTimesFromNanosecondsToMilliseconds(normalizeTimestampsToMin(as.data.frame(read.csv(filename))))

getQueryIdFromQueryData(localUserQueryData)

pdf("~/Desktop/localUserThoughtstream14-100-10.pdf", height=10, width=20)
plotAllEventsForGivenQuery(normalizeTimestampsToMin(getAllEventsForGivenQuery(localUserQueryData, "localUserThoughtstream14-100-10")))
dev.off()

# do the same for real thoughtstream

filename = "~/Desktop/thoughtstream-modeling/thoughtstreamTrace.csv"
thoughtstreamQueryData = convertTimesFromNanosecondsToMilliseconds(normalizeTimestampsToMin(as.data.frame(read.csv(filename))))

getQueryIdFromQueryData(thoughtstreamQueryData)

pdf("~/Desktop/thoughtstream7-100-10.pdf", height=10, width=20)
plotAllEventsForGivenQuery(normalizeTimestampsToMin(getAllEventsForGivenQuery(thoughtstreamQueryData, "thoughtstream7-100-10")))
dev.off()

# look at thoughtstream timelines for a higher cardinality


filename = "~/Desktop/thoughtstreamTraceFile.csv"
thoughtstreamQueryData = convertTimesFromNanosecondsToMilliseconds(normalizeTimestampsToMin(as.data.frame(read.csv(filename))))

getQueryIdFromQueryData(thoughtstreamQueryData)
thoughtstreamQueryData$elapsedTime[thoughtstreamQueryData$type == "query"]

pdf("~/Desktop/thoughtstream10-500-30.pdf", height=10, width=20)
plotAllEventsForGivenQuery(normalizeTimestampsToMin(getAllEventsForGivenQuery(thoughtstreamQueryData, "thoughtstream10-500-30")))
dev.off()


filename = "~/Desktop/localUserThoughtstreamTraceFile.csv"
localUserThoughtstreamQueryData = convertTimesFromNanosecondsToMilliseconds(normalizeTimestampsToMin(as.data.frame(read.csv(filename))))

getQueryIdFromQueryData(localUserThoughtstreamQueryData)
localUserThoughtstreamQueryData$elapsedTime[localUserThoughtstreamQueryData$type == "query"]

pdf("~/Desktop/localUserThoughtstream4-500-30.pdf", height=10, width=20)
plotAllEventsForGivenQuery(normalizeTimestampsToMin(getAllEventsForGivenQuery(localUserThoughtstreamQueryData, "localUserThoughtstream4-500-30")))
dev.off()

# 2.22.11
# looking at thoughtstream, localUserThoughtstream

thoughtstreamFilename = "/Users/kcurtis/Desktop/thoughtstreamQueryDataOverNio.csv"
localUserThoughtstreamFilename = "/Users/kcurtis/Desktop/localUserThoughtstreamQueryDataOverNio.csv"

thoughtstreamQueryDataNio = convertTimesFromNanosecondsToMilliseconds(as.data.frame(read.csv(thoughtstreamFilename)))
localUserThoughtstreamQueryDataNio = convertTimesFromNanosecondsToMilliseconds(as.data.frame(read.csv(localUserThoughtstreamFilename)))

dim(thoughtstreamQueryDataNio)
dim(localUserThoughtstreamQueryDataNio)

par(mfrow=c(1,2))
plotThoughtstreamSensitivityAnalysis(thoughtstreamQueryDataNio, 0.5, "thoughtstreamNio")
plotThoughtstreamSensitivityAnalysis(localUserThoughtstreamQueryDataNio, 0.5, "localUserThoughtstreamNio")

par(mfrow=c(1,2))
plotThoughtstreamSensitivityAnalysis(thoughtstreamQueryDataNio, 0.9, "thoughtstreamNio")
plotThoughtstreamSensitivityAnalysis(localUserThoughtstreamQueryDataNio, 0.9, "localUserThoughtstreamNio")

par(mfrow=c(1,2))
plotThoughtstreamSensitivityAnalysis(thoughtstreamQueryDataNio, 0.99, "thoughtstreamNio")
plotThoughtstreamSensitivityAnalysis(localUserThoughtstreamQueryDataNio, 0.99, "localUserThoughtstreamNio")

getLatencySensitivityAnalysisMatrix(thoughtstreamQueryDataNio, 0.5)
getLatencySensitivityAnalysisMatrix(localUserThoughtstreamQueryDataNio, 0.5)

getLatencySensitivityAnalysisMatrix(thoughtstreamQueryDataNio, 0.9)
getLatencySensitivityAnalysisMatrix(localUserThoughtstreamQueryDataNio, 0.9)

getLatencySensitivityAnalysisMatrix(thoughtstreamQueryDataNio, 0.99)
getLatencySensitivityAnalysisMatrix(localUserThoughtstreamQueryDataNio, 0.99)


# 3.14.11
# looking at thoughtstream, localUserThoughtstream results from distributed setting

setwd("/Users/kcurtis/workspace/radlab-demo/scads-visualization/Code")
source("visualization-functions.R")
source("exploreCardinalityFunctions.R")

thoughtstreamFilename = "/Users/kcurtis/Desktop/thoughtstream1.csv"
localUserThoughtstreamFilename = "/Users/kcurtis/Desktop/localUserThoughtstream.csv"

thoughtstreamQueryData = convertTimesFromNanosecondsToMilliseconds(as.data.frame(read.csv(thoughtstreamFilename)))
localUserThoughtstreamQueryData = convertTimesFromNanosecondsToMilliseconds(as.data.frame(read.csv(localUserThoughtstreamFilename)))

dim(thoughtstreamQueryData)
dim(localUserThoughtstreamQueryData)

par(mfrow=c(1,2))
plotThoughtstreamSensitivityAnalysis(thoughtstreamQueryData, 0.5, "thoughtstream")
plotThoughtstreamSensitivityAnalysis(localUserThoughtstreamQueryData, 0.5, "localUserThoughtstream")

plotThoughtstreamSensitivityAnalysis(thoughtstreamQueryData, 0.9, "thoughtstream")
plotThoughtstreamSensitivityAnalysis(localUserThoughtstreamQueryData, 0.9, "localUserThoughtstream")

plotThoughtstreamSensitivityAnalysis(thoughtstreamQueryData, 0.99, "thoughtstream")
plotThoughtstreamSensitivityAnalysis(localUserThoughtstreamQueryData, 0.99, "localUserThoughtstream")


# 3.16.11
# looking at thoughtstream, localUserThoughtstream merged results from distributed setting
setwd("/Users/kcurtis/workspace/radlab-demo/scads-visualization/Code")
source("visualization-functions.R")
source("exploreCardinalityFunctions.R")

thoughtstreamData = convertTimesFromNanosecondsToMilliseconds(getSingleDataset("~/Desktop/thoughtstream"))
localUserThoughtstreamData = convertTimesFromNanosecondsToMilliseconds(getSingleDataset("~/Desktop/localUserThoughtstream"))

par(mfrow=c(1,2))
plotThoughtstreamSensitivityAnalysis(thoughtstreamData, 0.5, "thoughtstream")
plotThoughtstreamSensitivityAnalysis(localUserThoughtstreamData, 0.5, "localUserThoughtstream")

plotThoughtstreamSensitivityAnalysis(thoughtstreamData, 0.9, "thoughtstream")
plotThoughtstreamSensitivityAnalysis(localUserThoughtstreamData, 0.9, "localUserThoughtstream")

plotThoughtstreamSensitivityAnalysis(thoughtstreamData, 0.99, "thoughtstream")
plotThoughtstreamSensitivityAnalysis(localUserThoughtstreamData, 0.99, "localUserThoughtstream")

# figure out how to plot the cdf of the latency
colnames(thoughtstreamData)
cdf = ecdf(thoughtstreamData$elapsedTime)
cdfSeq = seq(min(thoughtstreamData$elapsedTime), quantile(thoughtstreamData$elapsedTime, 0.999))
plot(cdfSeq, cdf(cdfSeq), col=0, xlab="Latency (ms)", ylab="Quantile", main="thoughtstream")
lines(cdfSeq, cdf(cdfSeq), col="black", lw=2)

basePath = "~/Desktop/thoughtstream"
setwd(basePath)
files = list.files(basePath)
numFiles = length(files)
colors = rainbow(numFiles)

cdf = ecdf(thoughtstreamData$elapsedTime)
cdfSeq = seq(min(thoughtstreamData$elapsedTime), quantile(thoughtstreamData$elapsedTime, 0.999))
plot(cdfSeq, cdf(cdfSeq), col=0, xlab="Latency (ms)", ylab="Quantile", main="thoughtstream")

for (i in 1:numFiles) {
	print(i)
	miniThoughtstreamData = convertTimesFromNanosecondsToMilliseconds(as.data.frame(read.csv(files[i])))
	cdf = ecdf(miniThoughtstreamData$elapsedTime)
	lines(cdfSeq, cdf(cdfSeq), col=colors[i], lw=2)
}