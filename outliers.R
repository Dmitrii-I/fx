# Read in ticks, and convert times to POSIXct for easer datetime manupulation
path.to.files <- "~/data/oanda/1/test/";
csv.files <- list.files(path=path.to.files, pattern="csv");

cat("Found ", length(csv.files), " csv files in ", path.to.files, ". \n", sep="");

if (exists("ticks") == FALSE) { # only load csv file into ticks object if it does not exist yet
	ticks <- list();
	start.time <- as.numeric(Sys.time()); # used to measure execution time of hthe for loop
	total.ticks <- 0;
	for (i in 1:length(csv.files)) {
		ticks[[i]] <- read.csv(paste(path.to.files, csv.files[i], sep=""), header=TRUE, sep=",");
		ticks[[i]][,1] <- as.POSIXct(ticks[[i]][,1]);
		total.ticks <- total.ticks + length(ticks[[i]][,1]);
	}
	end.time <- as.numeric(Sys.time());
	cat("Loaded ", length(csv.files), " csv files and ", total.ticks, " ticks in ", end.time - start.time, " seconds.\n", sep="");
} else { 
	cat("Object with name ticks exists. Will skip loading it. Make sure it contains the data you want to edit. \n");
}

for (i in 1:length(ticks)) {
	num.ticks.with.outliers <- length(ticks[[i]][,1]);
	ticks[[i]] <- DeleteOutliers(ticks[[i]]); # remove the outliers
	num.outliers <- num.ticks.with.outliers - length(ticks[[i]][,1]);
	cat("Removed ", num.outliers, " outliers from ", names(ticks[[i]])[2], "\n", sep="");
}
	
