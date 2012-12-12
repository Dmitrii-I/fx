find_missing_ticks <- function(ticksList, verbose = FALSE) 
{
    # This function compares tick files (where each file contains ticks on
    # same symbol) obtained by simulataneosly mining same data feed. The tick
    # files may come from different servers, data mining programs, scripts, 
    # etc. The aim is to compare data recording performance of these
    # servers or programs.
    #
    # The function returns a data frame containing colums: timestamp1, 
    # bid1, ask1, timestamp2, bid2, ask2, etc. If a tick file misses a tick, 
    # NA is filled in. Missing ticks are found by comparing all tick files, 
    # one tick at a time. The tick files do not have to start ticking at the 
    # same time: unnecessary leading ticks are purged. Timestamps do not have
    # to be in sync as it may be hard to synchronize clocks across servers.
    
    # First, purge unneccessary leading ticks
    
    startingTimestamps <- vector()
    for (i in 1:length(ticksList)) 
    {
        initialTimestamps[i] <- ticksList[[i]][1,1]
    }

    commonStartingTimestamp <- max(startingTimestamps) - 10

    # Remove unnecessary leading ticks
    for (i in 1:length(ticksList))
    {
        ticksList[[i]] <- 
            ticksList[[i]][ticksList[[i]][,1] > commonStartingTimestamp, ]
    }
   
    browser()
     
    tickFilesInDataFrame <- data.frame()
    
    return(tickFilesInDataFrame)
    
  
} 


load_ticks_dir <- function(csvFileDir, verbose = FALSE, bHeader = TRUE) {
    # Loads csv files into a list containing data frames of ticks. 
    # Each csv file should be for one symbol only and will produce one 
    # data frame. The first three columns of a row in csv file should be 
    # timestamp, bid, and ask. E.g. 2012-12-31 15:04:06.543424 1.54542 1.54555

    csvFiles <- list.files(path = csvFileDir,  pattern = "csv")
    
    if (verbose) cat("Found ", length(csvFiles), 
                 " csv files in ", csvFileDir, ". \n", sep="")

    ## Load csv files into list
    ticks <- list()
    startTime <- as.numeric(Sys.time()) # measure execution time of for loop

    # Load each csv file into a data frame and put it into list
    for (i in 1:length(csvFiles)) {
        ticks[[i]] <- 
            read.csv(paste(csvFileDir, csvFiles[i], sep=""), header=bHeader, sep=",")
        ticks[[i]][,1] <- as.POSIXct(ticks[[i]][,1])
        if (verbose) cat("Loaded ", csvFiles[i], "\n", sep = "")
    }
    endTime <- as.numeric(Sys.time())
    totalTicksAllFiles <- sum(sapply(ticks, nrow))
    
    if (verbose) {
        cat("Loaded ", length(csvFiles), " csv file(s) and ", 
            totalTicksAllFiles, " ticks.\n", sep="")
        cat("Loaded in ", endTime - startTime, " seconds.\n", sep="")
    }
    
    return(ticks)
}


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

ticksListToDataFrame <- function(ticks.list) {
    # Returns a data frame containing ticks of several currencies.
    # The columns are: timestamp, bid1, ask1, bid2, ask2, bid3, ask3, etc.
    # For currencies that have no ticks at certain timestamps, last
    # observation is carried forward. This is needed because not every
    # currency has same number of ticks and because timestamps are not
    # aligned (i.e. EURUSD may tick at 12:33:44.543 while EURCHF ticks later).  

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
