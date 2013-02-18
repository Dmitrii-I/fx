align_ticks <- function(ticks_list) {
    # Returns a single data frame with aligned bids and asks of multiple
	# instruments. The columns of this data frame are: timestamp, bid1, ask1, 
	# bid2, ask2, bid3, ask3, ...
	#
	# The input is a list of data frames of ticks, where each data frame
	# contains bid/ask prices of a single instrument.
	# 
    # The merging into one data frame is done according to this rule: 
	# if an instrument has a bid/ask price at a particular time while the
	# others do not, the last observed bid/ask of these will be carried
	# forward to fill the gap.  

    require(zoo, quietly = TRUE) # to load the na.locf function 
    
    headers <- "timestamp"
    
	timestamps <- NULL
    for (ticks in ticks_list) { 
        headers <- c(headers, names(ticks[,2:3]))
        timestamps <- append(timestamps, ticks[,1])
    } 
	# some timestamps will have duplicates as several instruments may have
	# ticked at the same time. Remove these duplicates.
    timestamps <- unique(timestamps)
    
	ticks.combined <- data.frame(timestamps)
    NAs <- data.frame(timestamps, NA, NA)

    for (ticks in ticks_list) {
        names(NAs) <- names(ticks) # so that we can use rbind()
        
		# pad with missing timestamps and NAs
        ticks <- rbind(ticks, NAs[!is.element(NAs[,1], ticks[,1]),]) 
        
		# sort by timestamp, increasing
        ticks <- ticks[order(ticks[,1]),]

        # Substitute all NAs except leading ones (na.rm = TRUE does not remove entire row, like we need)
        ticks[, 2:3] <- na.locf(ticks[, 2:3], na.rm = FALSE)  
        ticks.combined <- merge(ticks.combined, ticks, by = 1)
    }
    
    names(ticks.combined) <- headers # set the headers
    
	# Remove rows that have at least one NA. Some NAs are still here because
	# there was no last observation that na.locf could use to substitute them.
    while (sum(is.na(ticks.combined[1,])) > 0) 
		ticks.combined <- ticks.combined[-1,] 

    return(ticks.combined)
}
