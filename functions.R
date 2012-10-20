RollingMedians <- function (x, window.size, x.indexes) {
    # Returns a vector of rolling lookback or lookahead medians
    # Arguments:
    #   x:              A numeric vector.
    #   window.size:    Number of values used to compute rolling median. 
    #                   Negative window looks back from the pivot point; 
    #                   positive window looks forward from the pivot point.
    #   x.indexes:      Specifies which elements of x are used as pivot points.   

    rolling.medians <- rep(0, length(x.indexes)) # pre-allocate for faster computation
    n <- length(x)
    window.size.abs <- abs(window.size)
    # Expand x so that lookback and lookahead window do not go beyond boundaries of x.
    # E.g. if pivot is the 17th element of x, we will be able to compute rolling median
    # looking 20 elements back (windows.size = -20).
    x.expanded <- c(x[2:window.size.abs], x, x[(n - window.size.abs + 1):(n-1)])

    # Compute the rolling medians
    for (i in 1:length(x.indexes)) {
        start <- x.indexes[i] + window.size.abs - 1 + min(0, window.size + 1)
        end <-  x.indexes[i] + window.size.abs - 1 + max(0, window.size - 1)
        rolling.medians[i] <- median(x.expanded[start:end])
    }   

    return(rolling.medians)  
}
    

GetIndexesOfOutliers <- function (x) {
    # Returns a vector of indexes of outliers in numeric vector x.
    # The algorithm to detect outliers is backward and forward looking and is based on medians.

    n <- length(x) # number of observations
    
    # Set as low as possible, such that probability of the median, of any subset of consecutive 
    # values of x having this length, being an outlier is zero.
    x.subset.length <- 10 
    
    # To save computation time, only elements of x with lag 1 differences beyond 99.9% 
    # percentile will be checked for outliers.
    x.diffs <- c(0, abs(diff(x))) # compute lag 1 differences
    cutoff <- quantile(x.diffs, 0.999, names=FALSE)

    suspected.x <- which(x.diffs > cutoff)

    # compute lookback and lookahead medians
    lookback.medians <- RollingMedians(x, -1 * x.subset.length, suspected.x)
    lookahead.medians <- RollingMedians(x, x.subset.length, suspected.x) 
   
    # Compute the difference of suspected x and the medians 
    diffs.lookback.median <- abs(x[suspected.x] - lookback.medians) 
    diffs.lookahead.median <- abs(x[suspected.x] - lookahead.medians) 

    # Max difference between suspected x and the rolling median beyond which susepcted x 
    # is marked as outlier.
    max.diff <-  0.0040 # Equal to 40 pips. For JPY currencies, adjust appropriately
    # Better way to compute max.diff: take 40 previous bid.diffs, drop 10 highest and
    # get the sd of 30 elements. Use this to compute max.diff.
   
    # Identify outliers and store their indexes 
    indexes.outliers <- suspected.x[(diffs.lookback.median > max.diff) & (diffs.lookahead.median > max.diff)]

    return(indexes.outliers)
}


RemoveOutliers <- function(ticks.list) {
    # Wrapper function for GetIndexesOfOutliers(), Returns list of tick 
    # data frames without outliers.

    # Loop through tick files and remove outliers
    for (i in 1:length(ticks.list)) {
        indexes.outliers <- c(GetIndexesOfOutliers(ticks.list[[i]][,2]), # bid quotes
                              GetIndexesOfOutliers(ticks.list[[i]][,3])) # ask quotes
        if (length(indexes.outliers > 0)) {
            ticks.list[[i]] <- ticks.list[[i]][-indexes.outliers,]
        }
    }

    return(ticks.list)
}


PrintInfoOutliers <- function(x, x.indexes.outliers) {
    # Takes in a data frame (3 columns: timestamp, bid, ask), indexes of outliers, and prints info
    
    cat("Found ", length(x.indexes.outliers), " outliers.\n", sep="")
    cat("The indexes of outliers are:\n")
    print(x.indexes.outliers)

    x.abs.diffs <-  cbind(c(0, abs(diff(x[,2]))), c(0, abs(diff(x[,3])))) 

    # Loop through outliers and print info for each
    for (i in 1:length(x.indexes.outliers)) {
        cat("Outlier # ", i, ":\n", sep="")
        start <- x.indexes.outliers[i] - 1
        end <- x.indexes.outliers[i] + 1
        data <- cbind(
            x[start:end,], 
            x.abs.diffs[start:end,],
            abs((x[start:end,2] - RollingMedians(x[,2], -20, start:end))), 
            abs(x[start:end,2] - RollingMedians(x[,2], 20, start:end)) 
        )
        names(data) <- c("timestamp", "bid", "ask", "bid.diff", "ask.diff", "diff.lb.med", "diff.la.med")
        print(data)
        cat("--------------------------------------------------\n")
    }
}

AlignAndFill <- function(ticks.list) {
    # Combines a list of ticks of currencies one data frame with columns: 
    # timestamp, bid, ask, bid, ask, bid, ask, etc.
    # For currencies that have no ticks at certain timestamps, NA is filled in,
    # which is subsequently substituted with NAs by last observation.

    require(zoo, quietly = TRUE)
    
    ticks.combined.headers <- "timestamp"
    unique.timestamps <- NULL
    for (ticks in ticks.list) { 
        ticks.combined.headers <- c(ticks.combined.headers, names(ticks[,2:3]))
        unique.timestamps <- append(unique.timestamps, ticks[,1])
    } 
    unique.timestamps <- unique(unique.timestamps)
    ticks.combined <- data.frame(unique.timestamps)
    NAs <- data.frame(unique.timestamps, NA, NA)

    for (ticks in ticks.list) {
        names(NAs) <- names(ticks)
        # remove rows duplicate in the timestamp
        ticks <- ticks[!duplicated(ticks[,1], fromLast = TRUE),]
        # pad with missing timestamps and NAs
        ticks <- rbind(ticks, NAs[!is.element(NAs[,1], ticks[,1]),]) 
        # sort by timestamp increasing
        ticks <- ticks[order(ticks[,1]),]
        # Substitute all NAs except leading ones (na.rm = TRUE does not remove entire row, like we need)
        ticks[, 2:3] <- na.locf(ticks[, 2:3], na.rm = FALSE)  
        ticks.combined <- merge(ticks.combined, ticks, by = 1)
    }
    
    names(ticks.combined) <- ticks.combined.headers # set the headers
    # Remove entire row that has at least one leading NA
    while (sum(is.na(ticks.combined[1,])) > 0) ticks.combined <- ticks.combined[-1,] # remove leading NAs

    return(ticks.combined)
}
