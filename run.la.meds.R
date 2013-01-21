
run.la.meds <- function (x, window) {
    # window should be odd (runmed() requires that), if not, make it odd
    if (window %% 2 == 0) window <- window + 1 

    # compute offset
    offset <- (window - 1) / 2 + 1

    running.medians <- runmed(x, window)

    running.look.ahead.medians <- 
        c(running.medians[offset : (length(running.medians) - offset + 1)],
        rep(running.medians[length(running.medians) - offset + 1], window -1))

    return(running.look.ahead.medians)
}
