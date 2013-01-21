
indexes.about <- function(quotes, center=as.integer(nrow(quotes)/2), band.in.secs=60*5) {
	# Returns indexes centered around a specified quote. Returned are only 
	# indexes within a specified band (in seconds) from the timestamp of 
	# specified quote.

	start <- as.POSIXct(quotes[center, 1]) - band.in.secs
	end <- as.POSIXct(quotes[center, 1]) + band.in.secs
	indexes <- which(quotes[, 1] > start & quotes[, 1] < end)
	return(indexes)	
}
