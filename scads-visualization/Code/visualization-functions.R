normalizeTimestampsToMin = function(traceData) {
	minTimestamp = min(traceData$timestamp)
	traceData$timestamp = traceData$timestamp - minTimestamp
	return(traceData)
}

convertTimesFromNanosecondsToMilliseconds = function(traceData) {
	traceData$timestamp = convertNanosecondsToMilliseconds(traceData$timestamp)
	traceData$elapsedTime = convertNanosecondsToMilliseconds(traceData$elapsedTime)
	return(traceData)
}

convertNanosecondsToMilliseconds = function(nanoseconds) {
	return(nanoseconds/1000000)
}

# Assumption:  queries' events are not mingled -- ie, all of query1's events occur before any of query2's events begin
getAllEventsForGivenQuery = function(traceData, queryId) {
	indexOfPreviousQuery = getIndexOfPreviousQuery(traceData, queryId)
	rangeOfEventsForGivenQuery = seq(from=indexOfPreviousQuery+1, to=getIndexOfGivenQuery(traceData, queryId), by=1)
	return(traceData[rangeOfEventsForGivenQuery,])
}

getAllNormalizedEventsForGivenQuery = function(traceData, queryId) {
	return(normalizeTimestampsToMin(convertTimesFromNanosecondsToMilliseconds(getAllEventsForGivenQuery(traceData, queryId))))
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

	# determine how many iterators there are => y positioning of query, messages, each iterator, legend
	mapFromPlotElementToYCoord = getMapFromPlotElementToYCoord(queryData)

	par(yaxt="n", mar=c(5,17,4,2))
	plot(c(xlimMin, xlimMax), c(0, mapFromPlotElementToYCoord[mapFromPlotElementToYCoord[,"plotElement"] == "top", "yCoord"]), col=0, xlab="Time (ms)", ylab="", main=paste("Visualization of", queryId, "query"))

	messageCount = 0

	for (i in 1:nrow(queryData)) {
		if (queryData$type[i] == "query") {
			yCoord = mapFromPlotElementToYCoord[mapFromPlotElementToYCoord[,"plotElement"] == "query", "yCoord"]
			lines(getEventStartAndEndTimes(queryData[i,]), c(yCoord, yCoord), lw=2, col="red")
		} else if (queryData$type[i] == "iterator") {
			color = getColorForIteratorEventType(queryData$id[i])
			iteratorNameAndPosition = getIteratorNameAndPosition(queryData[i,"id"])
			yCoord = mapFromPlotElementToYCoord[mapFromPlotElementToYCoord[,"plotElement"] == iteratorNameAndPosition, "yCoord"]
			lines(getEventStartAndEndTimes(queryData[i,]), c(yCoord, yCoord), lw=2, col=color)
		} else if (queryData$type[i] == "message") {
			messageCount = messageCount + 1
			color = getColorForMessageType(queryData$id[i])
			yCoord = as.numeric(mapFromPlotElementToYCoord[mapFromPlotElementToYCoord[,"plotElement"] == "message", "yCoord"]) + messageCount * getMessageOffset()
			lines(getEventStartAndEndTimes(queryData[i,]), c(yCoord, yCoord), lw=2, col=color)
		}
	}

	createLegendForPlotWithAllQueryEvents() 
	labelIteratorsOnPlotWithAllQueryEvents(queryData)
}

getQueryIdFromQueryData = function(queryData) {
	return(queryData$id[queryData$type == "query"])
}

getEventStartAndEndTimes = function(event) {
	return(c(event$timestamp, event$timestamp + event$elapsedTime))
}

getColorForIteratorEventType = function(iteratorId) {
	colorMap = getMapFromIteratorFunctionsToColors()
	
	if (length(grep("open", iteratorId)) == 1) {
		color = colorMap[1,2]
	} else if (length(grep("hasNext", iteratorId)) == 1) {
		color = colorMap[2,2]
	} else if (length(grep("next", iteratorId)) == 1) {
		color = colorMap[3,2]
	} else if (length(grep("close", iteratorId)) == 1) {
		color = colorMap[4,2]
	} else {
		color = "black"
	}
	
	return(color)
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
	mapFromMessagesToColors = getMapFromMessagesToColors()
	
	legend("topright", legend=c("query", mapFromIteratorFunctionsToColors[,"function"], mapFromMessagesToColors[,"messageType"]), col=c("red", mapFromIteratorFunctionsToColors[,"color"], mapFromMessagesToColors[,"color"]), lw=2)
	
}

labelIteratorsOnPlotWithAllQueryEvents = function(queryData) {
	uniqueIterators = getUniqueIterators(getIteratorEventIdsFromQueryData(queryData))
	map = getMapFromPlotElementToYCoord(queryData)

	mtext(uniqueIterators, side=2, at=map[which(map[,"plotElement"] %in% uniqueIterators),"yCoord"], line=1, las=1)
}

getIteratorNameAndPosition = function(iteratorId) {
	library(stringr)
	iteratorIdElements = str_split(iteratorId, ":")
	return(paste(iteratorIdElements[[1]][1], iteratorIdElements[[1]][2], sep=":"))
}

getUniqueIterators = function(iterators) {
	iteratorNameAndPosition = vector(length=length(iterators))
	
	for (i in 1:length(iterators)) {
		iteratorNameAndPosition[i] = getIteratorNameAndPosition(iterators[i])
	}
	
	return(unique(iteratorNameAndPosition))
}

getIteratorEventIdsFromQueryData = function(queryData) {
	return(queryData[which(queryData[,"type"] == "iterator"),"id"])
}

getMapFromPlotElementToYCoord = function(queryData) {
	uniqueIterators = getUniqueIterators(getIteratorEventIdsFromQueryData(queryData))
	numUniqueIterators = length(uniqueIterators)
	
	map = matrix(nrow=(3 + numUniqueIterators), ncol=2)
	colnames(map) = c("plotElement", "yCoord")
	
	increment = 25
	map[1,] = c("message", increment)
	yCoordForLastMessage = increment + getMessageOffset() * numberOfMessagesInQuery(queryData)

	for (i in 1:numUniqueIterators) {
		map[i+1,"plotElement"] = uniqueIterators[i]
		map[i+1,"yCoord"] = yCoordForLastMessage + i*increment
	}
	yCoordForLastIterator = yCoordForLastMessage + numUniqueIterators*increment
	
	queryIndex = numUniqueIterators + 2
	yCoordForQuery = yCoordForLastIterator + increment
	map[queryIndex,"plotElement"] = "query"
	map[queryIndex,"yCoord"] = yCoordForQuery
	
	topIndex = queryIndex + 1
	map[topIndex,"plotElement"] = "top"
	map[topIndex,"yCoord"] = yCoordForQuery + 6*increment
	
	return(map)
}

# number of pixels in graph by which messages should be offset
getMessageOffset = function() {
	return(3)
}

getMapFromMessagesToColors = function() {
	map = matrix(nrow=2, ncol=2)
	colnames(map) = c("messageType", "color")
	
	map[1,] = c("Get", "purple")
	map[2,] = c("GetRange", "orange")
	
	return(map)
}

getColorForMessageType = function(messageId) {
	map = getMapFromMessagesToColors()
	
	if (length(grep("Get:", messageId)) == 1) {
		color = map[1,"color"]
	} else if (length(grep("GetRange", messageId)) == 1) {
		color = map[2,"color"]
	} else {
		color = "yellow"
	}
	
	return(color)
}

numberOfMessagesInQuery = function(queryData) {
	return(length(which(queryData$type == "message")))
}