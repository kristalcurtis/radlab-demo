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

plot(c(0,750000), c(0,100), col=0, xlab="Time (ns)", ylab="")
lines(c(query1Data$timestamp[query1Data$type == "query"], query1Data$elapsedTime[query1Data$type == "query"]), c(75, 75), lw=2, col="red")

for (i in 1:6) {
	if (query1Data$type[i] == "iterator") {
		lines(c(query1Data$timestamp[i], query1Data$timestamp[i] + query1Data$elapsedTime[i]), c(50, 50), lw=2, col="blue")
	} else if (query1Data$type[i] == "message") {
		lines(c(query1Data$timestamp[i], query1Data$timestamp[i] + query1Data$elapsedTime[i]), c(25, 25), lw=2, col="purple")
	}
}