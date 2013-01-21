

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

