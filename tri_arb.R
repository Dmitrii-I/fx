## Basic script to explore triangular arbitrage opportunities
#
#source("load_ticks.R")
#source("outliers.R")
#source("info_outliers.R")
#source("lookback_medians.R")
#source("lookahead_medians.R")
#source("del_consec_dups.R")
#source("del_outliers.R")
#source("align_ticks.R")
#source("consec_dups.R")
#
#
#files <- c("~/data/oanda/hosteurope/1/EURUSD_OANDA.csv",
#		   "~/data/oanda/hosteurope/1/GBPUSD_OANDA.csv",
#		   "~/data/oanda/hosteurope/1/EURGBP_OANDA.csv")
#
#ticks_list <- load_ticks(files)
#ticks_list <- del_outliers(ticks_list)
#ticks_list <- del_consec_dups(ticks_list)
#ticks <-  align_ticks(ticks_list)

load("~/triarb/data/forex_quotes.rda")

rate_product_1 <- ticks$EURUSD.OANDA_bid /
 				  ticks$GBPUSD.OANDA_ask /
 				  ticks$EURGBP.OANDA_ask
 
rate_product_2 <- ticks$EURGBP.OANDA_bid *
 				  ticks$GBPUSD.OANDA_bid /
 				  ticks$EURUSD.OANDA_ask
 
ticks <- cbind(ticks, rate_product_1, rate_product_2)
 
par(mfrow=c(2,1))
 
opportunities_1 <- ticks$rate_product_1[ticks$rate_product_1>1] * 10000 - 10000
barplot(sort(opportunities_1, decreasing = TRUE))
title(main = paste("Number of opportunities: ", length(opportunities_1)),
	ylab="Profit in pips") 
 
opportunities_2 <- ticks$rate_product_2[ticks$rate_product_2>1] * 10000 - 10000
barplot(sort(opportunities_2, decreasing = TRUE))
title(main = paste("Number of opportunities: ", length(opportunities_2)), 
	ylab = "Profit in pips") 
