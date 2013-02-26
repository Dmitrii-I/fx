load_ticks <- function(files, header = TRUE) 
{
	# Returns a list of ticks files.
	#
	# Arguments:
	# files: a vector of full paths. Each path is to a single csv file.
	#  
	# Each csv file, containing data for one instrument only, will
	# produce one data frame. 
	#
	# The columns in a csv file should be: timestamp, bid, and ask. 
	# E.g. 2012-12-31 15:04:06.543424 1.54542 1.54555

	ticks_list <- list()
    
	# Load each csv file into a data frame and put it into list
	for (i in 1:length(files)) 
	{
		ticks_list[[i]] <- read.csv(files[i], header = header)
		ticks_list[[i]][,1] <- as.POSIXct(ticks_list[[i]][,1])
	}
    
	return(ticks_list)
}

