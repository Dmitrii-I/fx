
run.lb.meds <- function (x, window) {
    # window should be odd (runmed() requires that), if not, make it odd
    if ((window %% 2) == 0) window <- window + 1
    # compute offset used to shift the vector of medians from runmed() (in stats package)
    offset <- (window - 1) / 2 + 1

    running.medians <- runmed(x, window)
    running.look.back.medians <- c(rep(running.medians[offset], window - 1), 
		   running.medians[offset : (length(running.medians) - offset + 1)])
    return(running.look.back.medians)
}


