del_consec_dups <- function(ticks_list) {
	# A wrapper function for consec_dups()
	# Removes consecutive duplicated rows in a data frame
	#
	# ticks_list: list of data frames


	for (ticks in ticks_list) {
		ticks <- ticks[-consec_dups(ticks), ]
	}
	
	return(ticks_list)
}




