# Basic script to explore triangular arbitrage opportunities

source("load_ticks.R")
source("outliers.R")
source("info_outliers.R")
source("lookback_medians.R")
source("lookahead_medians.R")

files <- c("~/data/oanda/hosteurope/1/AUDCAD_OANDA.csv",
		   "~/data/oanda/hosteurope/1/AUDCHF_OANDA.csv",
		   "~/data/oanda/hosteurope/1/CADCHF_OANDA.csv")

ticks_list <- load_ticks(files)
ticks_list <- del_outliers(ticks_list)
ticks_list <- del_consec_dups(ticks_list)


