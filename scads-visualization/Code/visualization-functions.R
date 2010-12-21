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
	plot(c(xlimMin, xlimMax), c(0,120), col=0, xlab="Time (ns)", ylab="", main=paste("Visualization of", queryId, "query"))

	colorVectorForIteratorEvents = getColorVectorForIteratorEvents(queryData)

	for (i in 1:nrow(queryData)) {
		if (queryData$type[i] == "query") {
			lines(getEventStartAndEndTimes(queryData[i,]), c(75, 75), lw=2, col="red")
		} else if (queryData$type[i] == "iterator") {
			lines(getEventStartAndEndTimes(queryData[i,]), c(50, 50), lw=2, col=colorVectorForIteratorEvents[i])
		} else if (queryData$type[i] == "message") {
			lines(getEventStartAndEndTimes(queryData[i,]), c(25, 25), lw=2, col="purple")
		}
	}

	#legend("topright", legend=c("query", "iterator", "message"), col=c("red", "blue", "purple"), lw=2)
	createLegendForPlotWithAllQueryEvents()
}

getQueryIdFromQueryData = function(queryData) {
	return(queryData$id[queryData$type == "query"])
}

getEventStartAndEndTimes = function(event) {
	return(c(event$timestamp, event$timestamp + event$elapsedTime))
}

getColorVectorForIteratorEvents = function(queryData) {
	colorVectorForIteratorEvents = vector(length=nrow(queryData))
	colorMap = getMapFromIteratorFunctionsToColors()
	
	for (i in 1:nrow(queryData)) {
		if (queryData$type[i] == "iterator") {
			if (length(grep("open", queryData$id[i])) == 1) {
				colorVectorForIteratorEvents[i] = colorMap[1,2]
			} else if (length(grep("hasNext", queryData$id[i])) == 1) {
				colorVectorForIteratorEvents[i] = colorMap[2,2]
			} else if (length(grep("next", queryData$id[i])) == 1) {
				colorVectorForIteratorEvents[i] = colorMap[3,2]
			} else if (length(grep("close", queryData$id[i])) == 1) {
				colorVectorForIteratorEvents[i] = colorMap[4,2]
			}
		}
	}
	
	return(colorVectorForIteratorEvents)
}

getMapFromIteratorFunctionsToColors = function() {
	map = matrix(nrow=4, ncol=2)
	colnames(map) = c("function", "color")
	
	map[1,] = c("iterator:open", "blue")
	map[2,] = c("iterator:hasNext", "cyan")
	map[3,] = c("iterator:next", "olivedrab")
	map[4,] = c("iterator:close", "magenta")
	
	return(map)
}

createLegendForPlotWithAllQueryEvents = function() {
	mapFromIteratorFunctionsToColors = getMapFromIteratorFunctionsToColors()
	
	legend("topright", legend=c("query", mapFromIteratorFunctionsToColors[,"function"], "message"), col=c("red", mapFromIteratorFunctionsToColors[,"color"], "purple"), lw=2)
	
}
