consec.dups <- function(x) {
	# Returns indexes of consecutive duplicate rows.
	# x is a numeric data frame

	indexes <- which(apply(abs(apply(data.matrix(x), 2, diff)), 1, sum) == 0) + 1
	return(indexes)
}
