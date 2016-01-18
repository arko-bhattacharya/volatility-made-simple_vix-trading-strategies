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
  '1' = function (ofLength) {
    temp <- rep_len(0, ofLength); temp[c(1, ofLength)] <- c(1, -1); return(temp)
  },
  '2' = function (ofLength) {
    temp <- rep_len(0, ofLength); temp[c(1, ofLength)] <- c(2, -2); return(temp)
  },
  '3' = function (ofLength) {
    temp <- rep_len(0, ofLength); temp[c(1, ofLength)] <- c(3, -3); return(temp)
  },
  '4' = function (ofLength) {
    temp <- rep_len(0, ofLength); temp[c(1, ofLength)] <- c(4, -4); return(temp)
  })

convertRun <- function (ofType, ofLength,
                        convertFuncs=runConversionFunctions) {
  return(convertFuncs[[as.character(ofType)]](ofLength))
}

strategy <- function(data)
  {
                                        # calculating the returns of the required ETNs
    data_ret <- do.call(cbind,
                        lapply(X = data[c("xiv", "vxx", "ziv", "vxz")],
                               FUN = function(k) {
                                 return(
                                   (k[2:length(k)] - k[1:(length(k) - 1)])/
                                   k[1:(length(k) - 1)])
                               }))
                                        # calculating the 83 day window rolling mean
    data_ret_tmp <- zoo(x = data_ret, order.by = data$date[2:nrow(data)])
    data_ret_mean <- rollapply(data = data_ret_tmp, 83, mean)
    data_ret_mean <- as.data.frame(data_ret_mean, stringsAsFactors = FALSE)
    rownames(data_ret_mean) <- NULL
    rownames(data_ret_mean) <- as.character(data$date[84:nrow(data)])

                                        # determining the ETN with max mean 83 day look back
                                        # mean return
    prod <- apply(data_ret_mean, MARGIN = 1,
                  FUN = function(k) {
                    if(all(as.numeric(k) < 0)) return(NA) else return(which(k %in% max(k)))
                  })
                                        # determing the ETN positions (buy and sell date for
                                        # each ETN)
    prod_pos <- prod; just_changes <- 0
    for(i in 2:length(prod_pos))
      {
        if((prod_pos[i] != prod[i-1]) && (just_changed == 0)) {
          prod_pos[i] <- prod[i-1]; just_changed <- 1
        } else just_changed <- 0
      }
    runs <- rle(as.numeric(prod_pos))
    prod_pos <- unlist(mapply(FUN = convertRun, runs[['values']], runs[['lengths']]))
    names(prod_pos) <- names(prod)

                                        # dates when we want to buy and sell xiv
    buy_xiv_dates <- names(prod_pos)[which(prod_pos == 1)]
    sell_xiv_dates <- names(prod_pos)[which(prod_pos == -1)]
                                        # dates when we want to buy and sell vxx
    buy_vxx_dates <- names(prod_pos)[which(prod_pos == 2)]
    sell_vxx_dates <- names(prod_pos)[which(prod_pos == -2)]
                                        # dates when we want to buy and sell ziv
    buy_ziv_dates <- names(prod_pos)[which(prod_pos == 3)]
    sell_ziv_dates <- names(prod_pos)[which(prod_pos == -3)]
                                        # dates when we want to buy and sell vxz
    buy_vxz_dates <- names(prod_pos)[which(prod_pos == 4)]
    sell_vxz_dates <- names(prod_pos)[which(prod_pos == -4)]
    
                                        # buy and sell prices for xiv
    buy_price_xiv <- data$xiv[which(data$date %in% as.POSIXct(buy_xiv_dates))]
    sell_price_xiv <- data$xiv[which(data$date %in% as.POSIXct(sell_xiv_dates))]
                                        # buy and sell prices for vxx
    buy_price_vxx <- data$vxx[which(data$date %in% as.POSIXct(buy_vxx_dates))]
    sell_price_vxx <- data$vxx[which(data$date %in% as.POSIXct(sell_vxx_dates))]
                                        # buy and sell prices for ziv
    buy_price_ziv <- data$ziv[which(data$date %in% as.POSIXct(buy_ziv_dates))]
    sell_price_ziv <- data$ziv[which(data$date %in% as.POSIXct(sell_ziv_dates))]
                                        # buy and sell prices for vxz
    buy_price_vxz <- data$vxz[which(data$date %in% as.POSIXct(buy_vxz_dates))]
    sell_price_vxz <- data$vxz[which(data$date %in% as.POSIXct(sell_vxz_dates))]
    
                                        # returns xiv
    returns_xiv <- ((sell_price_xiv - buy_price_xiv)/ buy_price_xiv)
    names(returns_xiv) <- buy_xiv_dates
                                        # returns vxx
    returns_vxx <- ((sell_price_vxx - buy_price_vxx)/ buy_price_vxx)
    names(returns_vxx) <- buy_vxx_dates
                                        # returns ziv
    returns_ziv <- ((sell_price_ziv - buy_price_ziv)/ buy_price_ziv)
    names(returns_ziv) <- buy_ziv_dates
                                        # returns vxz
    returns_vxz <- ((sell_price_vxz - buy_price_vxz)/ buy_price_vxz)
    names(returns_vxz) <- buy_vxz_dates
    
    ret_val <- c(returns_xiv, returns_vxx, returns_ziv, returns_vxz)
    ret_val <- ret_val[order(as.POSIXct(names(ret_val)))]

    return(ret_val)
  }
