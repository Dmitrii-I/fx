# Basic script to explore triangular arbitrage opportunities

source("load_ticks.R")
source("outliers.R")
source("info_outliers.R")
source("lookback_medians.R")
source("lookahead_medians.R")
source("del_consec_dups.R")
source("del_outliers.R")
source("align_ticks.R")
source("consec_dups.R")


files <- c("~/data/oanda/hosteurope/1/AUDCAD_OANDA.csv",
		   "~/data/oanda/hosteurope/1/AUDCHF_OANDA.csv",
		   "~/data/oanda/hosteurope/1/CADCHF_OANDA.csv")

ticks_list <- load_ticks(files)
ticks_list <- del_outliers(ticks_list)
ticks_list <- del_consec_dups(ticks_list)
combined_ticks <-  align_ticks(ticks_list)

rate_product_1 <- combined_ticks$AUDCAD.OANDA_bid *
				  combined_ticks$CADCHF.OANDA_bid /
				  combined_ticks$AUDCHF.OANDA_ask

rate_product_2 <- combined_ticks$AUDCHF.OANDA_bid /
				  combined_ticks$CADCHF.OANDA_ask /
				  combined_ticks$AUDCAD.OANDA_ask

combined_ticks <- cbind(combined_ticks, rate_product_1, rate_product_2)

par(mfrow=c(2,1))

opportunities_1 <- combined_ticks$rate_product_1[combined_ticks$rate_product_1>1] * 10000 - 10000
barplot(sort(opportunities_1, decreasing = TRUE))
title(main = paste("Number of opportunities: ", length(opportunities_1)),
	ylab="Profit in pips") 

opportunities_2 <- combined_ticks$rate_product_2[combined_ticks$rate_product_2>1] * 10000 - 10000
barplot(sort(opportunities_2, decreasing = TRUE))
title(main = paste("Number of opportunities: ", length(opportunities_2)), 
	ylab = "Profit in pips") 
