lookback_medians <- function (x, window) {
	# Returns a vector of lookback medians. the length of this vector
	# equals the length of x. 
	# 
	# arguments
	# x: a numerical vector
	# window: how many observations to look back; should be odd

    # window should be odd (runmed() requires that), if not, make it odd
    if (window %% 2 == 0) window <- window + 1 

    # runmed() centers the window around current observation. 
	# since we are computing look back median from current observation,
	# we need to offset the center. compute this offset:
    offset <- (window - 1) / 2 + 1

    medians <- runmed(x, window)

	# rep() is used to copy into first (window - 1) lookback medians
	# the first properly computed lookbacl median
    lookback_medians <- c(rep(medians[offset], window - 1), 
		  medians[offset : (length(medians) - offset + 1)])

    return(lookback_medians)
}
