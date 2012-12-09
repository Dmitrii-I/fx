has_header <- function(csv_file) 
{
    # Returns TRUE if at least one of the elements in first row is numeric.

    # Retrieve elements in first line
    elements <- read.csv(csv_file, header = FALSE, sep = ",", nrows = 1)
    
    if (sum(sapply(elements[1, ], is.numeric)) > 0) return(FALSE)
    else return(TRUE)
}

add_header <- function(csv_file) 
{
    # Add header to a csv file. Header name is based on the file name.
    # Retruns TRUE is header was added.
    if (!has_header(csv_file)) 
    {
        # Retrieve the filename without direcories and extension
        # E.g. myfile will be retrieved from ~/mydir/myfile.csv 
        common_part <- rev(unlist(strsplit(unlist(
            strsplit(csv_file, "\\."))[1], "/")))[1]

        header <- paste(common_part, "_timestamp,", common_part, "_bid,",
            common_part, "_ask", sep = "")

        ticks <- readLines(csv_file)
        writeLines(c(header, ticks), csv_file, sep = "\n")
        return(TRUE) 
        }
        
    else
    {
        return(FALSE)
    }
}



