has_header <- function(files) 
{
    # Returns TRUE only if all elements in first row of a text/csv
    # file are non-numeric.

    # Retrieve elements in first line
    results <- vector(length = length(files), mode = "logical")

    for (i in 1:length(files)) 
    {
        elements <- read.csv(files[i], header = FALSE, sep = ",", nrows = 1)
    
        if (sum(sapply(elements[1, ], is.numeric)) > 0) results[i] <- FALSE
        else results[i] <- TRUE
    }

    return(results)
}

add_header <- function(files, pattern = "csv") 
{
    # Add header to a text/csv file. Header text is based on the filename.
    # Returns TRUE indicating that a header was added, FALSE otherwise.
    
    results <- vector(length = length(files), mode = "logical")

    for (i in 1:length(files)) 
    {
        if (!has_header(files[i]))
        {
            # Retrieve the filename without direcories and extension
            # E.g. myfile will be retrieved from ~/mydir/myfile.csv 
            common_part <- rev(unlist(strsplit(unlist(
                               strsplit(files[i], "\\."))[1], "/")))[1]
            header <- paste(common_part, "_timestamp,", common_part, "_bid,",
                            common_part, "_ask", sep = "")

            ticks <- readLines(files[i])
            writeLines(c(header, ticks), files[i], sep = "\n")
            results[i] <- TRUE
        }
        else results[i] <- FALSE
    }
    return(results)
}

