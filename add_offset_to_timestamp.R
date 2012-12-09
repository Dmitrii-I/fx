add_offset_to_timestamp <- function(dir, offset_seconds = 0, bool_header = TRUE, verbose = FALSE)
{
    csv_files <- list.files(path = dir,  pattern = "csv")

    if (verbose) 
    {
        cat("Found ", length(csv_files), " csv files in ", dir, ". \n", sep="")
    }

    start_time <- as.numeric(Sys.time()) # measure execution time of for loop

    # Load each csv file into a data frame and put it into list
    for (i in 1:length(csv_files)) 
    {
        file_name <- paste(dir, csv_files[i], sep="")
        ticks <- read.csv(file_name, header = bool_header, sep = ",")
        ticks[, 1] <- as.POSIXct(ticks[, 1])
        ticks[, 1] <- ticks[, 1] + offset_seconds

        # avoid inaccurate milliseconds due to POSIX to character conversion.
        # we will round the milliseconds ourselves.
        date_time <- format(ticks[, 1], '%Y-%m-%d %H:%M:%S')
        milliseconds <- round(as.numeric(format(ticks[, 1], '%OS6')) %% 1, 3)
        # convert milliseconds to character and format properly
        milliseconds <- substring(format(milliseconds, nsmall = 3), 2)
        # put the timestamp converted to chars back to ticks 
        ticks[, 1] <- paste(date_time, milliseconds, "Z", sep = "")

        write.table(ticks, file = file_name, quote = FALSE, eol = "\n", 
            row.names = FALSE, col.names = bool_header, sep = ",", dec = ".")
        if (verbose) 
        { 
            cat("Done adjusting timestamps in ", csv_files[i], "\n", sep = "")
            user_answer <- readline("Press 'y' to continue, 'q' to quit: ")
        }
        
        if (verbose && user_answer != "y") return ("Finished as you instructed")
    }
    
    processing_time <- start_time - as.numeric(Sys.time())
    if (verbose) cat("Finished in  ", processing_time, " seconds.\n", sep="")
    
    return("Done adjusting timestamps in all specified files.")
}
