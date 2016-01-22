## For strategy details refer to
## http://volatilitymadesimple.com/vix-trading-strategies-in-june/
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
                                        # determining the days when vix > ux1 > ux2
    tmp <- rep(NA, nrow(data))
    tmp[which((data$vix > data$UX1) & (data$UX1 > data$UX2))] <- 1
    vxx_pos <- tmp
    for(i in 2:length(tmp)) if(is.na(tmp[i]) && (tmp[i - 1] %in% 1)) vxx_pos[i] <- 1
    if(is.na(vxx_pos[length(vxx_pos) - 1]) & (vxx_pos[length(vxx_pos)] %in% 1))
      {
        vxx_pos[length(vxx_pos)] <- NA
      }
    runs <- rle(is.na(vxx_pos))
    vxx_pos <- unlist(mapply(FUN = convertRun, runs[['values']], runs[['lengths']]))

                                        # determining days when vix < ux1 < ux2 and
                                        # RY > average(20 day RY )
    ry <- zoo(x = (data$UX2 - data$UX1)/data$UX1, order.by = data$date)
    ry_mean_20 <- rollapply(data = ry, 20, mean)
    ry <- as.numeric(ry)
    ry_mean_20 <- c(rep(NA, length(ry) - length(ry_mean_20)), as.numeric(ry_mean_20))
    tmp <- rep(NA, length(ry))
    tmp[which(((data$vix < data$UX1) & (data$UX1 < data$UX2)) & (ry > ry_mean_20))] <- 1
    xiv_pos <- tmp
    for(i in 2:length(tmp)) if(is.na(tmp[i]) && (tmp[i - 1] %in% 1)) xiv_pos[i] <- 1
        if(is.na(xiv_pos[length(xiv_pos) - 1]) & (xiv_pos[length(xiv_pos)] %in% 1))
      {
        xiv_pos[length(xiv_pos)] <- NA
      }
    runs <- rle(is.na(xiv_pos))
    xiv_pos <- unlist(mapply(FUN = convertRun, runs[['values']], runs[['lengths']]))

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
