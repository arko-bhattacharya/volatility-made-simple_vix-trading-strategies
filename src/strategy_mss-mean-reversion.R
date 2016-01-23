## This is a function which implements www.volatilitymadesimple.com's
## 'MarketSci’s Mean-Reversion VIX Trading Strategy'.
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
## - Go long VXX/XIV at the close when the 10-day EMA of the VIX closed
## under/over the 10-day SMA, and the average daily impact of the
## term-structure was greater/less than zero, otherwise move to cash.
## - 'Term-structure' loosely means roll yield, time decay, and all
## other factors not directly related to immediate changes in the VIX
## itself.
##
## For strategy details refer to:
## http://volatilitymadesimple.com/marketscis-mean-reversion-vix-trading-strategy/
## https://marketsci.wordpress.com/2012/04/12/strategy-2-for-trading-volatility-etps-
## timing-the-vix/
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
library(zoo); library(TTR)

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
                                        # EMA, SMA of vix
    sma <- SMA(x = data$vix, n = 10); ema <- EMA(x = data$vix, n = 10)
    ma_bool <- (ema > sma)
                                        # average daily impact of term structure
    ts <- (data$UX2 - data$UX1)/data$UX1
    ts_bool <- (ts < 0)

                                        # xiv position
    tmp <- rep(NA, length(ma_bool))
    tmp[which(ma_bool & ts_bool)] <- 1
    xiv_pos <- tmp
    for(i in 2:length(tmp)) if(is.na(tmp[i]) && (tmp[i - 1] %in% 1)) xiv_pos[i] <- 1
    if(is.na(xiv_pos[length(xiv_pos) - 1]) & (xiv_pos[length(xiv_pos)] %in% 1)) {
      xiv_pos[length(xiv_pos)] <- NA
    }
    runs <- rle(is.na(xiv_pos))
    xiv_pos <- unlist(mapply(FUN = convertRun, runs[['values']], runs[['lengths']]))

                                        # vxx position
    tmp <- rep(NA, length(ma_bool))
    tmp[which((!ma_bool) & (!ts_bool))] <- 1
    vxx_pos <- tmp
    for(i in 2:length(tmp)) if(is.na(tmp[i]) && (tmp[i - 1] %in% 1)) vxx_pos[i] <- 1
    if(is.na(vxx_pos[length(vxx_pos) - 1]) & (vxx_pos[length(vxx_pos)] %in% 1))
      {
        vxx_pos[length(vxx_pos)] <- NA
      }
    runs <- rle(is.na(vxx_pos))
    vxx_pos <- unlist(mapply(FUN = convertRun, runs[['values']], runs[['lengths']]))

                                        # dates when we want to buy and sell xiv
    buy_xiv_dates <- data$date[which(xiv_pos == 1)]
    sell_xiv_dates <- data$date[which(xiv_pos == -1)]
                                        # dates when we want to buy and sell vxx
    buy_vxx_dates <- data$date[which(vxx_pos == 1)]
    sell_vxx_dates <- data$date[which(vxx_pos == -1)]

                                        # buy and sell prices for xiv
    buy_price_xiv <- data$xiv[which(data$date %in% buy_xiv_dates)]
    sell_price_xiv <- data$xiv[which(data$date %in% sell_xiv_dates)]
                                        # buy and sell prices for vxx
    buy_price_vxx <- data$vxx[which(data$date %in% buy_vxx_dates)]
    sell_price_vxx <- data$vxx[which(data$date %in% sell_vxx_dates)]

                                        # returns xiv
    returns_xiv <- ((sell_price_xiv - buy_price_xiv)/ buy_price_xiv)
    returns_xiv <- data.frame(returns = returns_xiv,
                              buy_date = as.POSIXct(buy_xiv_dates),
                              sell_date = as.POSIXct(sell_xiv_dates),
                              stringsAsFactors = FALSE)    
                                        # returns vxx
    returns_vxx <- ((sell_price_vxx - buy_price_vxx)/ buy_price_vxx)
    returns_vxx <- data.frame(returns = returns_vxx,
                              buy_date = as.POSIXct(buy_vxx_dates),
                              sell_date = as.POSIXct(sell_vxx_dates),
                              stringsAsFactors = FALSE)    

    ret_val <- rbind(returns_xiv, returns_vxx)
    ret_val <- ret_val[order(ret_val$buy_date),]
    return(ret_val)
  }
