# Copyright: Dmitrii Izgurskii

build.LCS <- function(B, x, i, j) {
	if (i == 0 | j == 0) {
		 return(x[0, ])
	}
	if (B[i, j] == "upleft") {
		rbind(build.LCS(B, x, i-1, j-1), x[i, ])
	} else if (B[i, j] == "up")	{
		build.LCS(B, x, i-1, j)
	} else {
		build.LCS(B, x, i, j-1)
	}
}

LCS <- function(quotes) {
	LCS <- quotes[[1]][0, ]
	x.prefix <- quotes[[1]][0, ]
	y.prefix <- quotes[[2]][0, ]
	while(nrow(quotes[[1]]) != 0 || nrow(quotes[[2]]) != 0) {
		x.end <- min(10, nrow(quotes[[1]]))
		x <- rbind(x.prefix, quotes[[1]][1:x.end, ])
		y.end <- min(10, nrow(quotes[[2]]))
		y <- rbind(y.prefix, quotes[[2]][1:y.end, ])
		B <- matrix(0, x.end, y.end)
		C <- matrix(0, x.end + 1, y.end + 1)
		
		for (i in 1:x.end) {
			for (j in 1:y.end) {
				if (x[i, 2] == y[j, 2] & x[i, 3] == y[j, 3]) {
					C[i+1, j+1] <- C[i, j] + 1
					B[i, j] <- "upleft"
				} else if (C[i, j+1] >= C[i+1, j]) {
					C[i+1, j+1] <- C[i+1, j]
					B[i, j] <- "up"
				} else {
					C[i+1, j+1] <- C[i+1, j]
					B[i, j] <- "left"
				}
			}
		} 
		
	
		LCS <- rbind(LCS, build.LCS(B, x, x.end, y.end))
		
		build.prefixes <- function(B, x, y, i, j) {
			x.prefix <- x[0, ]
			y.prefix <- y[0, ]
			if (i == 0 | j == 0 | B[i, j] == "upleft") return(list(x.prefix, y.prefix))
			if (B[i,j] == "up") {
				build.prefixes(B, x, y, i-1, j)
				x.prefix <- rbind(x.prefix, x[i, ])
			} else {
				build.prefixes(B, x, y, i, j-1)
				y.prefix <- rbind(y.prefix, y[j, ])	
			}
		}
		
		quotes[[1]] <- quotes[[1]][-(1:x.end), ]
		quotes[[2]] <- quotes[[2]][-(1:y.end), ]

		x.prefix <- build.prefixes(B, x, y, i, j)[[1]]
		y.prefix <- build.prefixes(B, x, y, i, j)[[2]]

	}
	return(LCS)
}



indexes.about <- function(quotes, center=as.integer(nrow(quotes)/2), band.in.secs=60*5)
{
	# Returns indexes centered around a specified quote. Returned are only 
	# indexes within a specified band (in seconds) from the timestamp of 
	# specified quote.

	start <- as.POSIXct(quotes[center, 1]) - band.in.secs
	end <- as.POSIXct(quotes[center, 1]) + band.in.secs
	indexes <- which(quotes[, 1] > start & quotes[, 1] < end)
	return(indexes)	
}


sync.fcq <- function(quotes, max.diff.ms = 200) 
{
	# Returns a list of quote files with synchronized first quote.
	# Unnecessary leading quotes are purged.

	start <- max(do.call(c, lapply(quotes, "[[", 1, 1)))
	quotes[[1]] <-quotes[[1]][quotes[[1]][, 1] >= start, ]
	quotes[[2]] <-quotes[[2]][quotes[[2]][, 1] >= start, ]
	
	if (quotes[[1]][1, 2] == quotes[[2]][1, 2] && quotes[[1]][1, 3] == quotes[[2]][1, 3]) {
		equal.quotes <- TRUE
	} else {
		equal.quotes <- FALSE
	}

	while (!equal.quotes) {
		last.item <- max(which(quotes[[2]][, 1] < (quotes[[1]][1, 1] + max.diff.ms)))
		for (i in 1:last.item) {
			if (quotes[[1]][1, 2] == quotes[[2]][i, 2] && quotes[[1]][1, 3] == quotes[[2]][i, 3]) {
				quotes[[2]] <- quotes[[2]][i:nrow(quotes[[2]]), ]
				equal.quotes = TRUE
			}
		}
		if (!equal.quotes) quotes[[1]] <- quotes[[1]][-1, ]	
	}	
	return(quotes)
}


show.missed.quotes <- function(quotes) 
{
    # Compares tick files (where each file contains quotes on
    # same symbol) obtained by simulataneosly mining same source. The tick
    # files come from different data minining instances which may run on 
	# different computers. The aim is to compare data recording performance of these
    # servers or programs.
    #
    # The function returns a data frame with colums: timestamp1, 
    # bid1, ask1, timestamp2, bid2, ask2, etc. If a tick file misses a tick, 
    # na is filled in. Missing quotes are found by comparing all tick files, 
    # one tick at a time. The tick files do not have to start ticking at the 
    # same time: unnecessary leading quotes are purged. Timestamps do not have
    # to be in sync as it may be hard to synchronize clocks across servers.
    
    # Since symbols may start ticking at different times, we need to find 
    # common starting point and purge unneccessary leading quotes
   
	x <- sync.fcq(quotes)[[1]] # x hold the first quotes data frame
	y <- sync.fcq(quotes)[[2]] # y hold the second quotes data frame

		
	na.x.ind <- c()
	na.y.ind <- c() 

	i <- 2
	while (i <= nrow(x)) 
	{
		if (x[i, 2] != y[i, 2] || x[i, 3] != y[i, 3])
		{
			print(i)
			if (x[i, 2] != y[i+1, 2] || x[i, 3] != y[i+1, 3])  
			{
				if  (x[i, 2] != y[i+2, 2] || x[i, 3] != y[i+2, 3]) 
				{
					y <- rbind(y[1:(i-1), ], NA, y[i:nrow(y), ])
				} 
				else 
				{
					x <- rbind(x[1:(i-1), ], NA, x[i:nrow(x), ])
					x <- rbind(x[1:(i-1), ], NA, x[i:nrow(x), ])
					i <- i + 1
				}
			} 
			else 
			{
				x <- rbind(x[1:(i-1), ], NA, x[i:nrow(x), ])
			}
		}
	
		i <- i + 1
	}

	max.rows <- min(nrow(x), nrow(y))	
	return(list(x[1:max.rows, ], y[1:max.rows, ]))
} 


files.in.dir <- function(dir)
{
    # A wrapper function for list.files
    files <- list.files(path = dir, pattern = "csv", full.names = TRUE,
                        recursive = TRUE)
    return(files)
}

load.quotes <- function(files, header = TRUE) 
{
	# Loads csv files into a list containing data frames of ticks. 
	# Each csv file should be for one symbol only and will produce one 
	# data frame. The first three columns of a row in csv file should be 
	# timestamp, bid, and ask. E.g. 2012-12-31 15:04:06.543424 1.54542 1.54555

	quotes <- list()
    
	# Load each csv file into a data frame and put it into list
	for (i in 1:length(files)) 
	{
		quotes[[i]] <- read.csv(files[i], header = header)
		quotes[[i]][,1] <- as.POSIXct(quotes[[i]][,1])
	}
    
	return(quotes)
}


run.lb.meds <- function (x, window) {
    # window should be odd (runmed() requires that), if not, make it odd
    if ((window %% 2) == 0) window <- window + 1
    # compute offset used to shift the vector of medians from runmed() (in stats package)
    offset <- (window - 1) / 2 + 1

    running.medians <- runmed(x, window)
    running.look.back.medians <- c(rep(running.medians[offset], window - 1), 
		   running.medians[offset : (length(running.medians) - offset + 1)])
    return(running.look.back.medians)
}

run.la.meds <- function (x, window) {
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


ind.outliers <- function (x) {
    # Returns a vector of indexes of outliers in numeric vector x.
    # The algorithm to detect outliers is backward and forward looking and is based on medians.

    n <- length(x) 
    
    # Should be odd. Set as low as possible, such that probability of the median, 
    # of any subset of consecutive values of x having this length, being an outlier is zero.
    window <- 11 
    
    # compute lookback and lookahead medians
    lookback.medians <- run.lb.meds(x, window)
    lookahead.medians <- run.la.meds(x, window) 
   
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


del.outliers <- function(ticks.list) {
    # Wrapper function for ind.outliers. Returns list of tick data frames without outliers.

    # Loop through tick files and remove outliers
    for (i in 1:length(ticks.list)) {
        indexes.outliers <- c(ind.outliers(ticks.list[[i]][,2]), # bid quotes
                              ind.outliers(ticks.list[[i]][,3])) # ask quotes
        if (length(indexes.outliers > 0)) {
            ticks.list[[i]] <- ticks.list[[i]][-indexes.outliers,]
        }
    }

    return(ticks.list)
}


info.outliers <- function(x) {
    # Takes in a data frame of quotes (3 columns: timestamp, bid, ask), 
	# indexes of outliers, and prints info for each outlier
    x.indexes.outliers <- unique(c(ind.outliers(x[,2]), ind.outliers(x[,3])))
    if (length(x.indexes.outliers) < 1) return(cat("No outliers detected.\n")) 
    
    cat("Found ", length(x.indexes.outliers), " outliers.\n", sep="")

    x.abs.diffs <-  cbind(c(0, abs(diff(x[,2]))), c(0, abs(diff(x[,3])))) 
    diff.lb.med.bid <- run.lb.meds(x[, 2], 11)
    diff.lb.med.ask <- run.lb.meds(x[, 3], 11)
    diff.la.med.bid <- run.la.meds(x[, 2], 11)
    diff.la.med.ask <- run.la.meds(x[, 3], 11)
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

align.quotes <- function(quotes.list) {
    # Returns a single data frame built from the list of quote data frames.
    # The columns are: timestamp, bid1, ask1, bid2, ask2, bid3, ask3, etc.
    # For contracts that have no quotes at certain timestamps, last
    # observation is carried forward. This is needed because not every
    # currency has same number of quotes and because timestamps are not
    # aligned (i.e. EURUSD may tick at 12:33:44.543 while EURCHF quotes later).  

    require(zoo, quietly = TRUE)
    
    quotes.combined.headers <- "timestamp"
    unique.timestamps <- NULL
    for (quotes in quotes.list) { 
        quotes.combined.headers <- c(quotes.combined.headers, names(quotes[,2:3]))
        unique.timestamps <- append(unique.timestamps, quotes[,1])
    } 
    unique.timestamps <- unique(unique.timestamps)
    quotes.combined <- data.frame(unique.timestamps)
    NAs <- data.frame(unique.timestamps, NA, NA)

    for (quotes in quotes.list) {
        names(NAs) <- names(quotes)
        # remove rows duplicate in the timestamp
        quotes <- quotes[!duplicated(quotes[,1], fromLast = TRUE),]
        # pad with missing timestamps and NAs
        quotes <- rbind(quotes, NAs[!is.element(NAs[,1], quotes[,1]),]) 
        # sort by timestamp increasing
        quotes <- quotes[order(quotes[,1]),]
        # Substitute all NAs except leading ones (na.rm = TRUE does not remove entire row, like we need)
        quotes[, 2:3] <- na.locf(quotes[, 2:3], na.rm = FALSE)  
        quotes.combined <- merge(quotes.combined, quotes, by = 1)
    }
    
    names(quotes.combined) <- quotes.combined.headers # set the headers
    # Remove entire row that has at least one leading NA
    while (sum(is.na(quotes.combined[1,])) > 0) quotes.combined <- quotes.combined[-1,] # remove leading NAs

    return(quotes.combined)
}
