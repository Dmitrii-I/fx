
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

