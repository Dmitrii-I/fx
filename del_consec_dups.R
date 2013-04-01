del_consec_dups <- function(ticks_list) {
	# A wrapper function for consec_dups()
	# Removes consecutive duplicated rows in a data frame
	#
	# ticks_list: list of data frames

	for (i in 1:length(ticks_list)) {
		dups <- consec_dups(ticks_list[[i]])
		if (length(dups) != 0)	{ 
			ticks_list[[i]] <- ticks_list[[i]][-dups, ]
		}
	}
	
	return(ticks_list)
}




