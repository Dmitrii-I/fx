has.header <- function(files) {
    # Returns TRUE only if all elements in first row of a text/csv
    # file are non-numeric.

    # Retrieve elements in first line
    results <- vector(length = length(files), mode = "logical")

    for (i in 1:length(files)) {
        elements <- read.csv(files[i], header = FALSE, sep = ",", nrows = 1)
        if (sum(sapply(elements[1, ], is.numeric)) > 0) results[i] <- FALSE
        else results[i] <- TRUE
    }

    return(results)
}
