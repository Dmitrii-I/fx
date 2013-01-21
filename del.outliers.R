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
