add_offset_to_timestamp <- function(file, offset_in_sec = 0, header = TRUE)
{
    ticks <- read.csv(file, header = header)
    ticks[, 1] <- as.POSIXct(ticks[, 1])
    ticks[, 1] <- ticks[, 1] + offset_in_sec

    # avoid inaccurate milliseconds due to POSIX to character conversion.
    # we will round the milliseconds ourselves.
    date_time <- format(ticks[, 1], '%Y-%m-%d %H:%M:%S')
    milliseconds <- round(as.numeric(format(ticks[, 1], '%OS6')) %% 1, 3)
    
    # convert milliseconds to character and format properly
    milliseconds <- substring(format(milliseconds, nsmall = 3), 2)
    
    # put the timestamp converted to chars back to ticks 
    ticks[, 1] <- paste(date_time, milliseconds, "Z", sep = "")

    write.table(ticks, file = file, quote = FALSE, eol = "\n", 
                row.names = FALSE, col.names = header, sep = ",", dec = ".")
    
    return(TRUE)    
}
