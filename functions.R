RunningLookBackMedians <- function (x, window) {
    # window should be odd (runmed() requires that), if not, make it odd
    if ((window %% 2) == 0) window <- window + 1
    
    # compute offset used to shift the vector of medians from runmed() (in stats package)
    offset <- (window - 1) / 2 + 1

    running.medians <- runmed(x, window)
    running.look.back.medians <- 
        c(rep(running.medians[offset], window - 1), 
        running.medians[offset : (length(running.medians) - offset + 1)])

    return(running.look.back.medians)
}

RunningLookAheadMedians <- function (x, window) {
    # window should be odd (runmed() requires that), if not, make it odd
    if (window %% 2 == 0) window <- window + 1 

    # compute offset
    offset <- (window - 1) / 2 + 1

    running.medians <- runmed(x, window)

    running.look.ahead.medians <- 
        c(running.medians[offset : (length(running.medians) - offset + 1)],
        rep(running.medians[length(running.medians) - offset + 1], window -1))

    return(running.look.ahead.medians)
}


GetIndexesOfOutliers <- function (x) {
    # Returns a vector of indexes of outliers in numeric vector x.
    # The algorithm to detect outliers is backward and forward looking and is based on medians.

    n <- length(x) 
    
    # Should be odd. Set as low as possible, such that probability of the median, 
    # of any subset of consecutive values of x having this length, being an outlier is zero.
    window <- 11 
    
    # compute lookback and lookahead medians
    lookback.medians <- RunningLookBackMedians(x, window)
    lookahead.medians <- RunningLookAheadMedians(x, window) 
   
    # Compute the difference of suspected x and the medians 
    diffs.lookback.median <- abs(x - lookback.medians) 
    diffs.lookahead.median <- abs(x - lookahead.medians) 

    # Max difference between suspected x and the rolling median beyond which susepcted x 
    # is marked as outlier.
    max.diff <-  0.0040 # Equal to 40 pips. For JPY currencies, adjust appropriately
    if (x[1] > 10) max.diff <- 0.4 # if x is quoted in JPY, adjust max.diff
    # Better way to compute max.diff: take 40 previous bid.diffs, drop 10 highest and
    # get the sd of 30 elements. Use this to compute max.diff.
   
    # Identify outliers and store their indexes 
    indexes.outliers <- which((diffs.lookback.median > max.diff) & (diffs.lookahead.median > max.diff))

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


PrintInfoOutliers <- function(x) {
    # Takes in a data frame (3 columns: timestamp, bid, ask), indexes of outliers, and prints info
    x.indexes.outliers <- unique(c(GetIndexesOfOutliers(x[,2]), GetIndexesOfOutliers(x[,3])))
    if (length(x.indexes.outliers) < 1) return(cat("No outliers detected.\n")) 
    
    cat("Found ", length(x.indexes.outliers), " outliers.\n", sep="")

    x.abs.diffs <-  cbind(c(0, abs(diff(x[,2]))), c(0, abs(diff(x[,3])))) 
    diff.lb.med.bid <- RunningLookBackMedians(x[, 2], 11)
    diff.lb.med.ask <- RunningLookBackMedians(x[, 3], 11)
    diff.la.med.bid <- RunningLookAheadMedians(x[, 2], 11)
    diff.la.med.ask <- RunningLookAheadMedians(x[, 3], 11)
    # Loop through outliers and print info for each
    for (i in 1:length(x.indexes.outliers)) {
        cat("Outlier # ", i, ":\n", sep="")
        start <- x.indexes.outliers[i] - 1
        end <- x.indexes.outliers[i] + 1
        data <- cbind(
            x[start:end, ], 
            x.abs.diffs[start:end, ],
            abs(x[start:end, 2] - diff.lb.med.bid[start:end]), 
            abs(x[start:end, 2] - diff.lb.med.ask[start:end]), 
            abs(x[start:end, 3] - diff.la.med.bid[start:end]), 
            abs(x[start:end, 3] - diff.la.med.ask[start:end])
        )
        names(data) <- c("timestamp", "bid", "ask", "bid.diff", "ask.diff", 
                         "diff.lb.med.bid", "diff.la.med.bid", "diff.lb.med.ask",
                         "diff.la.med.ask")
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
