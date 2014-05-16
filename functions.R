RollingMedians <- function (x, window, ind) {
# Returns a vector of rolling medians for specified elements of x. 
# Variable window specifies over how many elements the median is computed.
# Negative window looks back; positive window looks forward.
  
	roll.med <- rep(0, length(ind)) # pre-allocate for faster computation
	n <- length(x)
	win.abs <- abs(window)
	x.expanded <- c(x[2:win.abs], x, x[(n - win.abs + 1):(n-1)])

	for (i in 1:length(ind)) {
		roll.med[i] <- median(x.expanded[((ind[i] + win.abs - 1) + min(0, window + 1)):
			                            ((ind[i] + win.abs - 1) + max(0, window - 1))])
	}	
  
  return(roll.med)
}
	

GetIndexesOfOutliers <- function (prices) {
	n <- length(prices[,1]) # number of observations
	window.size <- 20 # size (in input) of window used to compute lookahead and lookback median
	
	diffs <- c(0, abs(diff(prices[,2])))
	# Outliers will be checked only beyond cutoff set to 99th percentile (saves computation time)
	cutoff <- quantile(diffs, 0.999, names=FALSE)

	suspect.indexes <- which(diffs > cutoff)

	# compute lookback medians
	lookback.medians <- RollingMedians(prices, -1 * window.size, suspect.indexes)

	# compute lookahead medians
	lookahead.medians <- RollingMedians(prices, window.size, suspect.indexes) 
	
	diff.lookback.median <- abs(prices[suspect.indexes,2] - lookback.medians) 
	diff.lookahead.median <- abs(prices[suspect.indexes,2] - lookahead.medians) 

	# compute max price change beyond which prices are market as outliers
	max.diff <-  mean(bid.diff) * 30
	print("max diff in pips:")
	print(max.diff*10000)
	# take 40 bid.diffs, drop 10 highest and calc sd of 30 
	
	indexes.outliers <- which((diff.lookback.median > max.diff) & (diff.lookahead.median > max.diff))
	indexes.to.delete <- suspect.indexes[indexes.outliers]
	diff.lookback.median <- diff.lookback.median[indexes.outliers] # store only medians of ticks to be deleted
	diff.lookahead.median <- diff.lookahead.median[indexes.outliers] # store only medians of ticks to be deleted
	cat("Going to delete ", length(indexes.to.delete), " ticks.\n", sep="")
	cat("The following ticks will be deleted (indexes): \n")
	print(indexes.to.delete)
	for (i in 1:length(indexes.to.delete)) {
		cat("Outlier # ", i, ":\n", sep="")
		temp <- cbind(
			input[(indexes.to.delete[i] - 1):(indexes.to.delete[i] + 1),], 
			bid.diff[(indexes.to.delete[i] - 1):(indexes.to.delete[i] + 1)],
			diff.lookback.median[i], # medians of ticks not be deleted were dropped earlier
			diff.lookahead.median[i]
		)
		names(temp) <- c("timestamp", "bid", "ask", "bid.diff", "diff.lb.med", "diff.la.med")
		print(temp)
		cat("--------------------------------------------------\n")
	}

	if (length(indexes.to.delete) > 0) {
		return(input[-indexes.to.delete,])
	} else {
		return(input)
	}
}

