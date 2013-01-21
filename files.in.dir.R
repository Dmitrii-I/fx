
files.in.dir <- function(dir) {
    # A wrapper function for list.files. Returns a vector of all csv files in dir
    files <- list.files(path = dir, pattern = "csv", full.names = TRUE,
                        recursive = TRUE)
    return(files)
}
