## This is a function which implements www.volatilitymadesimple.com's
## 'Don’t Fear the Bear’s (DFTB) Spread Model'
##
## This function is designed assuming that it will be executed by the
## ./src/backtest_strategy.R script. This script backtests the
## strategy (function) on the main data set which is described below.
##
## The input to this function is the dataset which is specified
## below. The path to the data file is ../data/main_data.R.
##
## This function returns a dataframe which tells us the buy dates,
## sell dates indicated by the strategy over the period of back test
## and the returns generated during each investment period.
##
## Rules of the strategy:
## Go long VXX (XIV) at the close when the 20-day average of the VIX
## futures front month premium is greater than 5% (less than -5%),
## otherwise to cash.
##
## For strategy details refer to:
## http://volatilitymadesimple.com/backtest-dont-fear-the-bears-spread-model/
##  
## > load('../data/main_data.RData')
## >
## > head(data)
##     UX1   UX2  UX_CM1M   vix     snp    xiv    vxx   vxv      ziv   vxz       date
## 1 23.20 25.80 23.89333 23.54 1180.55  9.557 788.64 25.19 12.29875 74.56 2010-11-30
## 2 21.90 24.75 22.75500 21.36 1206.07  9.787 751.20 23.99 12.45500 72.57 2010-12-01
## 3 19.85 23.00 20.90000 19.39 1221.53 10.429 692.96 21.99 12.45500 70.13 2010-12-02
## 4 19.05 22.15 20.18667 18.01 1224.71 11.041 660.80 21.00 13.09500 68.63 2010-12-03
## 5 18.60 21.60 20.00000 18.02 1223.12 11.252 648.80 21.04 13.09500 68.02 2010-12-06 
## 6 18.60 21.45 20.02500 17.99 1223.75 11.358 642.40 20.97 13.31875 67.50 2010-12-07
##
## > str(data)
## 'data.frame':	1162 obs. of  11 variables:
##  $ UX1    : num  23.2 21.9 19.9 19.1 18.6 ...
##  $ UX2    : num  25.8 24.8 23 22.1 21.6 ...
##  $ UX_CM1M: num  23.9 22.8 20.9 20.2 20 ...
##  $ vix    : num  23.5 21.4 19.4 18 18 ...
##  $ snp    : num  1181 1206 1222 1225 1223 ...
##  $ xiv    : num  9.56 9.79 10.43 11.04 11.25 ...
##  $ vxx    : num  789 751 693 661 649 ...
##  $ vxv    : num  25.2 24 22 21 21 ...
##  $ ziv    : num  12.3 12.5 12.5 13.1 13.1 ...
##  $ vxz    : num  74.6 72.6 70.1 68.6 68 ...
##  $ date   : POSIXct, format: "2010-11-30" "2010-12-01" ...
## >
library(zoo)

runConversionFunctions <- list(
  "FALSE"=function (ofLength) {
    temp <- rep_len(0, ofLength)
    temp[c(1, ofLength)] <- c(1, -1)
    return(temp)
  },
  "TRUE"=function (ofLength) {
    return(rep_len(NA, ofLength))
  }
  )

convertRun <- function (ofType, ofLength,
                        convertFuncs=runConversionFunctions) {
  return(convertFuncs[[as.character(ofType)]](ofLength))
}

strategy <- function(data)
  {
                                        # converting data to a zoo object
    data_tmp <- zoo(x = data[c("UX1", "UX2", "UX_CM1M", "vix", "snp", "xiv", "vxx", "vxv")],
                    order.by = data$date)
    
                                        # time series of vix front month contract premium wrt
                                        # back month
    fm_prem <- ((data_tmp$UX1 - data$UX2)/data$UX1)
    fm_prem <- rollapply(data = fm_prem, 20, mean)
    fm_prem <- c(rep(0, nrow(data) - length(fm_prem)), fm_prem)

                                        # vxx buy sell points
    vxx_pos <- fm_prem; vxx_pos[which(as.numeric(fm_prem) < 0.05)] <- NA
    vxx_pos[which(!is.na(vxx_pos))] <- 1
    runs <- rle(is.na(vxx_pos))
    vxx_buy_sell <- unlist(mapply(FUN = convertRun, runs[['values']], runs[['lengths']]))

                                        # xiv buy sell points
    xiv_pos <- fm_prem; xiv_pos[which(as.numeric(fm_prem) >= -0.05)] <- NA
    xiv_pos[which(!is.na(xiv_pos))] <- 1
    runs <- rle(is.na(xiv_pos))
    xiv_buy_sell <- unlist(mapply(FUN = convertRun, runs[['values']], runs[['lengths']]))

                                        # dates when we want to buy and sell xiv
    buy_xiv_dates <- index(data_tmp)[which(xiv_buy_sell == 1)]
    sell_xiv_dates <- index(data_tmp)[which(xiv_buy_sell == -1)]
                                        # dates when we want to buy and sell vxx
    buy_vxx_dates <- index(data_tmp)[which(vxx_buy_sell == 1)]
    sell_vxx_dates <- index(data_tmp)[which(vxx_buy_sell == -1)]

                                        # buy and sell prices for xiv
    buy_price_xiv <- data$xiv[which(data$date %in% buy_xiv_dates)]
    sell_price_xiv <- data$xiv[which(data$date %in% sell_xiv_dates)]
                                        # buy and sell prices for vxx
    buy_price_vxx <- data$vxx[which(data$date %in% buy_vxx_dates)]
    sell_price_vxx <- data$vxx[which(data$date %in% sell_vxx_dates)]

                                        # returns
    returns_xiv <- ((sell_price_xiv - buy_price_xiv)/ buy_price_xiv)
    returns_xiv <- data.frame(returns = returns_xiv,
                              buy_date = as.POSIXct(buy_xiv_dates),
                              sell_date = as.POSIXct(sell_xiv_dates),
                              stringsAsFactors = FALSE)    

    returns_vxx <- ((sell_price_vxx - buy_price_vxx)/ buy_price_vxx)
    returns_vxx <- data.frame(returns = returns_vxx,
                              buy_date = as.POSIXct(buy_vxx_dates),
                              sell_date = as.POSIXct(sell_vxx_dates),
                              stringsAsFactors = FALSE)    
    
    ret_val <- rbind(returns_xiv, returns_vxx)
    ret_val <- ret_val[order(ret_val$buy_date),]

    return(ret_val)
  }
