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
