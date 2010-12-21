normalizeTimestampsToMin = function(traceData) {
	minTimestamp = min(traceData$timestamp)
	traceData$timestamp = traceData$timestamp - minTimestamp
	return(traceData)
}

# Assumption:  queries' events are not mingled -- ie, all of query1's events occur before any of query2's events begin
getAllEventsForGivenQuery = function(traceData, queryId) {
	indexOfPreviousQuery = getIndexOfPreviousQuery(traceData, queryId)
	rangeOfEventsForGivenQuery = seq(from=indexOfPreviousQuery+1, to=getIndexOfGivenQuery(traceData, queryId), by=1)
	return(traceData[rangeOfEventsForGivenQuery,])
}

getQueryIndices = function(traceData) {
	return(which(traceData$type == "query"))
}

getIndexOfPreviousQuery = function(traceData, queryId) {
	queryIndices = getQueryIndices(traceData)
	indexOfGivenQuery = getIndexOfGivenQuery(traceData, queryId)
	
	positionInQueryIndicesOfGivenQueryIndex = which(queryIndices == indexOfGivenQuery)
	
	if (positionInQueryIndicesOfGivenQueryIndex > 1)
		return(queryIndices[positionInQueryIndicesOfGivenQueryIndex-1])
	else
		return(0)
}

getIndexOfGivenQuery = function(traceData, queryId) {
	return(which(traceData$id == queryId))
}

plotAllEventsForGivenQuery = function(queryData) {
	queryId = getQueryIdFromQueryData(queryData)

	xlimMin = min(queryData$timestamp)
	xlimMax = max(queryData$timestamp + queryData$elapsedTime)

	par(yaxt="n", mar=c(5,2,4,2))
	plot(c(xlimMin, xlimMax), c(0,100), col=0, xlab="Time (ns)", ylab="", main=paste("Visualization of", queryId, "query"))

	for (i in 1:nrow(queryData)) {
		if (queryData$type[i] == "query") {
			lines(getEventStartAndEndTimes(queryData[i,]), c(75, 75), lw=2, col="red")
		} else if (queryData$type[i] == "iterator") {
			lines(getEventStartAndEndTimes(queryData[i,]), c(50, 50), lw=2, col="blue")
		} else if (queryData$type[i] == "message") {
			lines(getEventStartAndEndTimes(queryData[i,]), c(25, 25), lw=2, col="purple")
		}
	}

	legend("topright", legend=c("query", "iterator", "message"), col=c("red", "blue", "purple"), lw=2)
}

getQueryIdFromQueryData = function(queryData) {
	return(queryData$id[queryData$type == "query"])
}

getEventStartAndEndTimes = function(event) {
	return(c(event$timestamp, event$timestamp + event$elapsedTime))
}