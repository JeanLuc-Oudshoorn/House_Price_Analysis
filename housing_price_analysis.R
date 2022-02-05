library(BEST)
library(quantmod)
library(xts)
library(csodata)
library(readr)
library(data.table)


# 1.) Importing index data from Yahoo Finance
nasdaq <- getSymbols(Symbols = "^IXIC", src = "yahoo", auto.assign = FALSE)
sp <- getSymbols(Symbols = "^GSPC", src = "yahoo", auto.assign = FALSE)
vanguard_us <- getSymbols(Symbols = "VNQ", src = "yahoo", auto.assign = FALSE)
ishares_europe <- getSymbols(Symbols = "IPRP.AS", src = "yahoo", auto.assign = FALSE)
dow_globe <- getSymbols(Symbols = "RWO", src = "yahoo", auto.assign = FALSE)


# 2.) Converting the data to monthly interval 
nasdaq <- to.period(nasdaq, period = "months", k = 1, OHLC = FALSE)
sp <- to.period(sp, period = "months", k = 1, OHLC = FALSE)
vanguard_us <- to.period(vanguard_us, period = "months", k = 1, OHLC = FALSE)
ishares_europe <- to.period(ishares_europe, period = "months", k = 1, OHLC = FALSE)
dow_globe <- to.period(dow_globe, period = "months", k = 1, OHLC = FALSE)


# 3.) Subsetting for a period where each series has data (period should be 2008-06/2021-11)
nasdaq <- nasdaq["2008-06/2021-11"]
sp <- sp["2008-06/2021-11"]
vanguard_us <- vanguard_us["2008-06/2021-11"]
ishares_europe <- ishares_europe["2008-06/2021-11"]
dow_globe <- dow_globe["2008-06/2021-11"]


# 4.) Fortifying the dataset with Irish and Dutch property price indices
irish_housing <- cso_get_data("HPM09")
irish_index_full <- as.numeric(unlist(transpose(irish_housing)))[-1:-2]
irish_index <- irish_index_full[42:203]
dublin_index <- irish_index_full[657:818]

dutch_housing <- read_csv2("83906NED_UntypedDataSet_05022022_160508.csv")
dutch_index <- dutch_housing[,3]
dutch_index <- unlist(dutch_index)
dutch_index <- as.numeric(dutch_index[162:length(dutch_index[-1])])


# 5.) Assigning the appropriate time index to Irish and Dutch property price indices
idx <- index(nasdaq)
dutch_index <- xts(dutch_index, order.by = idx)
irish_index <- xts(irish_index, order.by = idx)
dublin_index <- xts(dublin_index, order.by = idx)


# 6.) Combining everything in one dataframe and log differencing the values
dfd <- cbind(nasdaq[,4], sp[,4], vanguard_us[,4], ishares_europe[,4], 
             dow_globe[,4], irish_index, dublin_index, dutch_index)

dfd <- diff(log(dfd))
dfd <- na.omit(dfd)


# 7.) Creating a matrix of S&P500 returns subtracted by respective property index returns
sp_diff <- matrix(nrow = length(dfd[,2]), ncol = 6)

sp_diff[,1] <- dfd[,2] - dfd[,3]
sp_diff[,2] <- dfd[,2] - dfd[,4]
sp_diff[,3] <- dfd[,2] - dfd[,5]
sp_diff[,4] <- dfd[,2] - dfd[,6]
sp_diff[,5] <- dfd[,2] - dfd[,7]
sp_diff[,6] <- dfd[,2] - dfd[,8]


c_names <- c("US", "Europe", "Globe", "Ireland", "Dublin", "Netherlands")
colnames(sp_diff) <- c_names


# 8.) Calculate relative performance using Bayesian Parameter Estimation
par(mfrow = c(2, 3))


for (i in 1:6) {
  BESTout <- BESTmcmc(coredata(sp_diff[,i]))
  print(BESTout)
  plot(BESTout, credMass = 0.99, main = paste("S&P500 vs.", c_names[i]))
}


# 9.) Performing T-tests to sanity check results

for (i in 3:8) {
  out <- t.test(coredata(dfd[,2]), coredata(dfd[,i]), paired = TRUE)
  print(paste("Chance that S&P500 underperforms", colnames(dfd[,i]),":", round(out$p.value*100,2),"%"))
  print(paste("Mean monthly outperformance of S&P500", round(out$estimate*100,2), "%"))
}

# 10.) Testing a shorter time difference

dfd2016 <- dfd['2016/']

short_diff <- matrix(nrow = length(dfd2016[,2]), ncol = 6)

short_diff[,1] <- dfd2016[,2] - dfd2016[,3]
short_diff[,2] <- dfd2016[,2] - dfd2016[,4]
short_diff[,3] <- dfd2016[,2] - dfd2016[,5]
short_diff[,4] <- dfd2016[,2] - dfd2016[,6]
short_diff[,5] <- dfd2016[,2] - dfd2016[,7]
short_diff[,6] <- dfd2016[,2] - dfd2016[,8]


for (i in 1:6) {
  BESTout <- BESTmcmc(coredata(short_diff[,i]))
  print(BESTout)
  plot(BESTout, credMass = 0.99, main = paste("S&P500 vs.", c_names[i]))
}


