## Load csv files
path <- "~/data/oanda/1/test/";
csv.files <- list.files(path=path, pattern="csv");
cat("Found ", length(csv.files), " csv files in ", path, ". \n", sep="");

## Load csv files into list
ticks <- list();
start.time <- as.numeric(Sys.time()); # used to measure execution time of the for loop
for (i in 1:length(csv.files)) {
    ticks[[i]] <- read.csv(paste(path, csv.files[i], sep=""), header=TRUE, sep=",");
    ticks[[i]][,1] <- as.POSIXct(ticks[[i]][,1]);
}
end.time <- as.numeric(Sys.time());
total.raw.ticks <- sum(sapply(ticks, nrow))
cat("Loaded ", length(csv.files), " csv file(s) and ", total.raw.ticks, " ticks.\n", sep="")
cat("Loaded in ", end.time - start.time, " seconds.\n", sep="");

# Remove outliers
ticks.clean <- RemoveOutliers(ticks)
num.outliers.removed <- total.raw.ticks - sum(sapply(ticks.clean, nrow))
cat("Removed ", num.outliers.removed, " outliers.\n", sep="")

# Combine and fill
ticks.combined <- AlignAndFill(ticks)   
