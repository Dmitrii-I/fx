Functions to collect and analyze FOREX tick data

A sample workflow could be:
1. Load raw tick data from csv files. Usually you'd want to load 3 tick files
to analyze triangular arbitrage
2. Delete outliers
3. Delete duplicated ticks  
4. Combine the ticks into one data frame
5. Compute the two rate products
