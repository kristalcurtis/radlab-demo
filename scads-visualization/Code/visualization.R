miniTraceData = as.data.frame(read.csv("~/Desktop/miniTraceData.csv"))

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