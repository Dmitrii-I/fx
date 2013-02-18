consec_dups <- function(x) {
	# Returns indexes of two or more consecutive duplicate rows.
	# x is a numeric data frame

	x <- data.matrix(x) # coerce to numeric because apply() requires numeric
						# (when it coerces itself it produces character)
	
	diffs <- apply(x, 2, diff) # to each column of x, apply diff() 
	diffs <- abs(diffs) 
	diffs <- apply(diffs, 1, sum)

	indexes <- which(diffs == 0)
	return(indexes)
}
