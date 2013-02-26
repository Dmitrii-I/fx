info_outliers <- function(ticks) {
    # Prints info on each outlier in ticks, detected with outliers()
	# 
	# ticks: a data frame of 3 columns: timestamp, bid, ask

	bid_outliers <- outliers(ticks[, 2])
	ask_outliers <- outliers(ticks[, 3])
	outliers <- unique(c(bid_outliers, ask_outliers)) 

    if (length(outliers) < 1) return(cat("No outliers detected.\n")) 
    
    cat("Found ", length(outliers), " outliers.\n", sep="")

    abs_diffs_bid <- c(0, abs(diff(ticks[, 2])))
	abs_diffs_ask <- c(0, abs(diff(ticks[, 3])))
	abs_diffs <- cbind(abs_diffs_bid, abs_diffs_ask)

	# Delta is defined as the absolute value of a price (bid or ask)
	# and the median (lookback or lookahead). They are used in outliers() 
	# function to determine outliers. It is useful to eticksamine them
    delta_1 <- abs(ticks[, 2] - lookback_medians(ticks[, 2], 11))
    delta_2 <- abs(ticks[, 3] - lookback_medians(ticks[, 3], 11))
    delta_3 <- abs(ticks[, 2] - lookahead_medians(ticks[, 2], 11))
    delta_4 <- abs(ticks[, 3] - lookahead_medians(ticks[, 3], 11))

    # Loop through outliers and print info for each
    for (i in 1:length(outliers)) {
        cat("Outlier # ", i, ":\n", sep="")
        start <- outliers[i] - 1
        end <- outliers[i] + 1
        data <- cbind( ticks[start:end, ], abs_diffs[start:end, ], delta_1, 
			delta_2, delta_3, delta_4)
        names(data) <- c("timestamp", "bid", "ask", "bid.diff", "ask.diff", 
                         "delta 1", "delta 2", "delta 3", "delta 4")
        print(data)
        cat("--------------------------------------------------\n")
    }

}

