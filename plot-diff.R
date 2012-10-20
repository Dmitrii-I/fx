# Script reads csv file and 


ticks <- read.csv(csv.file.name, header=FALSE, sep=",");
cat("Number of ticks loaded:", length(ticks[,1]), "\n");

# convert to POSIXct for easier datetime manipulation
ticks[,1] <- as.POSIXct(ticks[,1]);

cat("Datetime is now", class(ticks[,1]), "class.\n");

# print a sample of ticks data.frame
cat("\nThe first 5 rows of ticks data frame:\n");
print(ticks[1:5,]);
cat("\n");

# print a summary
time.diff <- as.numeric(diff(ticks[,1]));
print(summary(time.diff, digits=5));

#plot histogram, ingoring outliers by defining xlim in hist
hist(time.diff[time.diff<1], col="blue");

# save histogram to png
#filename <- paste("~/Pictures/R/", as.integer(Sys.time()),".png", sep="");
#png(filename);
#hist(time.diff[time.diff<1], col="blue");
#dev.off();

