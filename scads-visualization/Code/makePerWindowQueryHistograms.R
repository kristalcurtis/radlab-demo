perWindowQueryHistogramData = as.data.frame(read.csv("/Users/ksauer/Desktop/radlab-demo/scads-visualization/Data/Processed/queryHistogramsForEachWindow.txt", header=FALSE, row.names=1))

dim(perWindowQueryHistogramData)

plot(1:1000, perWindowQueryHistogramData[1,], type="S", xlim=c(0,10))

# make a plot for each window
# a window's plot should have a plot for each query 

for (i in unique(perWindowQueryHistogramData[,1])) {
	pdf(paste("~/Desktop/window", i, ".pdf", sep=""))
	par(mar=c(5,5,4,2)+0.1, mfrow=c(3,1))
	
for (j in which(perWindowQueryHistogramData[,1] == i)) {
		plot(1:1000, perWindowQueryHistogramData[j,2:1001], type="S", xlim=c(0,10), xlab="Latency (ms)", ylab="Count", main=rownames(perWindowQueryHistogramData[j,]))	
	}
	
	dev.off()
}


# now with huge trace
perWindowQueryHistogramData = as.data.frame(read.csv("/Users/ksauer/Desktop/radlab-demo/scads-visualization/Data/Processed/queryHistogramsForEachWindowFromHugeTrace.csv", header=FALSE, row.names=1))
