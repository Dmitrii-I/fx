ind.outliers <- function (x) {
    # Returns a vector of indexes of outliers in numeric vector x.
    # The algorithm to detect outliers is backward and forward looking and is based on medians.

    n <- length(x) 
    
    # Should be odd. Set as low as possible, such that probability of the median, 
    # of any subset of consecutive values of x having this length, being an outlier is zero.
    window <- 11 
    
    # compute lookback and lookahead medians
    lookback.medians <- run.lb.meds(x, window)
    lookahead.medians <- run.la.meds(x, window) 
   
    # Compute the difference of suspected x and the medians 
    diffs.lookback.median <- abs(x - lookback.medians) 
    diffs.lookahead.median <- abs(x - lookahead.medians) 

    # Max difference between suspected x and the rolling median beyond which susepcted x 
    # is marked as outlier.
    max.diff <-  0.0040 # Equal to 40 pips. For JPY currencies, adjust appropriately
    if (x[1] > 10) max.diff <- 0.4 # if x is quoted in JPY, adjust max.diff
    # Better way to compute max.diff: take 40 previous bid.diffs, drop 10 highest and
    # get the sd of 30 elements. Use this to compute max.diff.
   
    # Identify outliers and store their indexes 
    indexes.outliers <- which((diffs.lookback.median > max.diff) & (diffs.lookahead.median > max.diff))

    return(indexes.outliers)
}
