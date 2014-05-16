# A list to save work typing
file.name <- as.character(list());
file.name[1] <- "AUDCAD-OANDA.csv";
file.name[2] <- "AUDCHF-OANDA.csv"; 
file.name[3] <- "AUDJPY-OANDA.csv";
file.name[4] <- "AUDNZD-OANDA.csv";
file.name[5] <- "AUDUSD-OANDA.csv";
file.name[6] <- "CADCHF-OANDA.csv";
file.name[7] <- "CADJPY-OANDA.csv";
file.name[8] <- "CHFJPY-OANDA.csv";
file.name[9] <- "EURAUD-OANDA.csv";
file.name[10] <- "EURCAD-OANDA.csv";
file.name[11] <- "EURCHF-OANDA.csv";
file.name[12] <- "EURGBP-OANDA.csv";
file.name[13] <- "EURJPY-OANDA.csv";
file.name[14] <- "EURNZD-OANDA.csv";
file.name[15] <- "EURUSD-OANDA.csv";
file.name[16] <- "GBPCHF-OANDA.csv";
file.name[17] <- "GBPJPY-OANDA.csv";
file.name[18] <- "GBPUSD-OANDA.csv";
file.name[19] <- "NZDJPY-OANDA.csv";
file.name[20] <- "NZDUSD-OANDA.csv";
file.name[21] <- "USDCAD-OANDA.csv";
file.name[22] <- "USDCHF-OANDA.csv";
file.name[23] <- "USDJPY-OANDA.csv";
file.dir <- "~/data/oanda/1/";

# load functions to process outliers
source("~/fx/functions.R");

# Read in ticks, and convert times to POSIXct for easer datetime manupulation
csv.file <- paste(file.dir, file.name[1], sep="");
cat("Loading file: ", csv.file, "\n", sep="");
ticks <- read.csv(csv.file, header=FALSE, sep=",");
ticks[,1] <- as.POSIXct(ticks[,1]);

ticks.clean <- delete.outliers(ticks);


plot(ticks[,1], ticks[,2], type="l", col="blue");
lines(ticks.clean[,1], ticks.clean[,2], type="l", col="red");

