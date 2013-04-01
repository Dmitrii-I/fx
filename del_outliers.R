del_outliers <- function(ticks_list) {
    # Wrapper function for outliers() 
	# Returns list of tick data frames without outliers.

    # Loop through tick files and remove outliers
    for (i in 1:length(ticks_list)) {
        indexes_outliers <- c(outliers(ticks_list[[i]][,2]), # bid quotes
                              outliers(ticks_list[[i]][,3])) # ask quotes
        if (length(indexes_outliers > 0)) {
            ticks_list[[i]] <- ticks_list[[i]][-indexes_outliers,]
        }
    }

    return(ticks_list)
}
