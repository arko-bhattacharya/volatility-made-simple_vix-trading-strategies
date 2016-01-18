## > load('../data/main_data.RData')
## >
## > head(data)
##     UX1   UX2  UX_CM1M   vix     snp    xiv    vxx   vxv       date
## 1 23.20 25.80 23.89333 23.54 1180.55  9.557 788.64 25.19 2010-11-30
## 2 21.90 24.75 22.75500 21.36 1206.07  9.787 751.20 23.99 2010-12-01
## 3 19.85 23.00 20.90000 19.39 1221.53 10.429 692.96 21.99 2010-12-02
## 4 19.05 22.15 20.18667 18.01 1224.71 11.041 660.80 21.00 2010-12-03
## 5 18.60 21.60 20.00000 18.02 1223.12 11.252 648.80 21.04 2010-12-06
## 6 18.60 21.45 20.02500 17.99 1223.75 11.358 642.40 20.97 2010-12-07
## > 
## > str(data)
## 'data.frame':	1162 obs. of  9 variables:
##  $ UX1    : num  23.2 21.9 19.9 19.1 18.6 ...
##  $ UX2    : num  25.8 24.8 23 22.1 21.6 ...
##  $ UX_CM1M: num  23.9 22.8 20.9 20.2 20 ...
##  $ vix    : num  23.5 21.4 19.4 18 18 ...
##  $ snp    : num  1181 1206 1222 1225 1223 ...
##  $ xiv    : num  9.56 9.79 10.43 11.04 11.25 ...
##  $ vxx    : num  789 751 693 661 649 ...
##  $ vxv    : num  25.2 24 22 21 21 ...
##  $ date   : POSIXct, format: "2010-11-30" "2010-12-01" ...
## >
library(zoo)
strategy <- function(data)
  {
    data <- zoo(x = data[c("UX1", "UX2", "UX_CM1M", "vix", "snp", "xiv", "vxx", "vxv")],
                order.by = data$date)

                                        # sd(vix returns)
    vix_returns <- ((as.numeric(data$vix[2:length(data$vix)]) -
                    as.numeric(data$vix[1:(length(data$vix) - 1)]))/
                    as.numeric(data$vix[1:(length(data$vix) - 1)]))
    names(vix_returns) <- index(data)[2:nrow(data)]
    sd_vix <- rollapply(data = vix_returns, 10, sd)
    names(sd_vix) <- as.character(index(data))[(nrow(data) - length(sd_vix) + 1):nrow(data)]
    
                                        # determining buy and sell points in vxx
    vxx_pos <- rep(NA, length(sd_vix)); names(vxx_pos) <- names(sd_vix)
    is_long <- 0
    for(i in 1:length(vxx_pos))
      {
        if((is_long == 1) && (as.numeric(sd_vix[i]) < 0.10)) {vxx_pos[i] <- -1; is_long <- 0}
        if((is_long == 1) && (as.numeric(sd_vix[i]) >= 0.10)) {vxx_pos[i] <- 0}       
        if((is_long == 0) && (as.numeric(sd_vix[i]) > 0.11)) {vxx_pos[i] <- 1; is_long <- 1}
      }
    if(vxx_pos[length(vxx_pos)] == 0) vxx_pos[length(vxx_pos)] <- -1
    vxx_pos <- c(rep(NA, nrow(data) - length(vxx_pos)), vxx_pos)
    names(vxx_pos) <- as.character(index(data))
                                        # determining buy and sell points in xiv
    xiv_pos <- vxx_pos; xiv_pos[which(is.na(xiv_pos))] <- 10; xiv_pos <- as.numeric(xiv_pos)
    df_xiv_pos <- c(xiv_pos[1], xiv_pos[2:length(xiv_pos)] - xiv_pos[1:(length(xiv_pos) - 1)])
    db_xiv_pos <- c(xiv_pos[1:(length(xiv_pos) - 1)] - xiv_pos[2:length(xiv_pos)],
                    xiv_pos[length(xiv_pos)])
    xiv_buy <- df_xiv_pos; xiv_buy[which(xiv_buy < 9)] <- NA
    xiv_buy[which(!is.na(xiv_buy))] <- 1; names(xiv_buy) <- names(vxx_pos)
    xiv_sell <- db_xiv_pos; xiv_sell[which(xiv_sell != 9)] <- NA
    xiv_sell[which(!is.na(xiv_sell))] <- -1; names(xiv_sell) <- names(vxx_pos)

                                        # dates when we want to buy and sell xiv
    buy_xiv_dates <- names(xiv_buy)[which(!is.na(xiv_buy))]
    sell_xiv_dates <- names(xiv_sell)[which(!is.na(xiv_sell))]
                                        # dates when we want to buy and sell vxx
    buy_vxx_dates <- names(vxx_pos)[which(vxx_pos == 1)]
    sell_vxx_dates <- names(vxx_pos)[which(vxx_pos == -1)]

                                        # buy and sell prices for xiv
    data <- as.data.frame(data); data$date <- rownames(data); rownames(data) <- NULL
    buy_price_xiv <- data$xiv[which(data$date %in% buy_xiv_dates)]
    sell_price_xiv <- data$xiv[which(data$date %in% sell_xiv_dates)]
                                        # buy and sell prices for vxx
    buy_price_vxx <- data$vxx[which(data$date %in% buy_vxx_dates)]
    sell_price_vxx <- data$vxx[which(data$date %in% sell_vxx_dates)]

                                        # returns
    returns_xiv <- ((sell_price_xiv - buy_price_xiv)/ buy_price_xiv)
    names(returns_xiv) <- buy_xiv_dates
    
    returns_vxx <- ((sell_price_vxx - buy_price_vxx)/ buy_price_vxx)
    names(returns_vxx) <- buy_vxx_dates
    
    ret_val <- c(returns_xiv, returns_vxx)
    ret_val <- ret_val[order(as.POSIXct(names(ret_val)))]

    return(ret_val)
  }
