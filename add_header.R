has_header <- function(csv_file) 
{
    # Returns TRUE only if all elements in first row are non-numeric.

    # Retrieve elements in first line
    elements <- read.csv(csv_file, header = FALSE, sep = ",", nrows = 1)
    
    if (sum(sapply(elements[1, ], is.numeric)) > 0) return(FALSE)
    else return(TRUE)
}

add_header <- function(file, pattern = "csv") 
{
    # Add header to a text/csv file. Header text is based on the filename.
    # Returns TRUE indicating that a header was added, FALSE otherwise.

    if (!has_header(file)) 
    {
        # Retrieve the filename without direcories and extension
        # E.g. myfile will be retrieved from ~/mydir/myfile.csv 
        common_part <- rev(unlist(strsplit(unlist(
                            strsplit(file, "\\."))[1], "/")))[1]

        header <- paste(common_part, "_timestamp,", common_part, "_bid,",
        common_part, "_ask", sep = "")

        ticks <- readLines(file)
        writeLines(c(header, ticks), file, sep = "\n")
        return(TRUE)
    }
    else
    {
        return(FALSE)
    }
}

