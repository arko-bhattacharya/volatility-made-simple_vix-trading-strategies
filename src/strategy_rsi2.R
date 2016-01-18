## > load('../data/main_data.RData')
## >
## > head(data)
##     UX1   UX2  UX_CM1M   vix     snp    xiv    vxx       date
## 1 23.20 25.80 23.89333 23.54 1180.55  9.557 788.64 2010-11-30
## 2 21.90 24.75 22.75500 21.36 1206.07  9.787 751.20 2010-12-01
## 3 19.85 23.00 20.90000 19.39 1221.53 10.429 692.96 2010-12-02
## 4 19.05 22.15 20.18667 18.01 1224.71 11.041 660.80 2010-12-03
## 5 18.60 21.60 20.00000 18.02 1223.12 11.252 648.80 2010-12-06
## 6 18.60 21.45 20.02500 17.99 1223.75 11.358 642.40 2010-12-07
## >
## > str(data)
## 'data.frame':	1162 obs. of  8 variables:
##  $ UX1    : num  23.2 21.9 19.9 19.1 18.6 ...
##  $ UX2    : num  25.8 24.8 23 22.1 21.6 ...
##  $ UX_CM1M: num  23.9 22.8 20.9 20.2 20 ...
##  $ vix    : num  23.5 21.4 19.4 18 18 ...
##  $ snp    : num  1181 1206 1222 1225 1223 ...
##  $ xiv    : num  9.56 9.79 10.43 11.04 11.25 ...
##  $ vxx    : num  789 751 693 661 649 ...
##  $ date   : POSIXct, format: "2010-11-30" "2010-12-01" ...
## >
library(TTR)
strategy <- function(data)
  {
                                        # rsi vector
    rsi <- RSI(price = data$xiv, n = 2)
    
                                        # understanding the state of RSI (entry and exit
                                        # points)
    rsi_state <- rep(NA, length(rsi))
    is_bought <- 0
    for(i in 1:length(rsi))
      {
        if(!is.na(rsi[i]))
          {
            if(is_bought == 1)
              {
                if(rsi[i] <= 50) rsi_state[i] <- 0 
                if(rsi[i] > 50) { rsi_state[i] <- -1; is_bought <- 0 }
              }
            if((rsi[i] < 10) && (is_bought == 0)) { rsi_state[i] <- 1; is_bought <- 1 }
          }
      }
                                        # understanding the state of RSI (increasing position
                                        # size points)
    long_on <- 0
    total_position <- 0
    for(i in 1:length(rsi_state))
      {
        if(!is.na(rsi_state[i]))
          {
            if(rsi_state[i] == 1)
              {
                long_on <- 1
                entry_price <- data$xiv[i]
                total_position <- 1
              }
            if((rsi_state[i] == 0) && (long_on == 1) && (data$xiv[i] < entry_price))
              {
                rsi_state[i] <- 2
                long_on <- 2
                entry_price <- data$xiv[i]
                total_position <- 3
              }
            if((rsi_state[i] == 0) && (long_on == 2) && (data$xiv[i] < entry_price))
              {
                rsi_state[i] <- 3
                long_on <- 3
                entry_price <- data$xiv[i]
                total_position <- 6
              }
            if(rsi_state[i] == -1) {rsi_state[i] <- -total_position; long_on <- 0}
          }
      }

                                        # calculating the returns from these positions
    buy_price <- 0; sell_price <- 0
    ret_val <- rep(NA, length(which(rsi_state < 0)))
    for(i in 1:length(rsi_state))
      {
        if(!is.na(rsi_state[i]))
          {
            if(rsi_state[i] == 1) buy_price <- data$xiv[i]
            if(rsi_state[i] == 2) buy_price <- (buy_price + (2*data$xiv[i]))
            if(rsi_state[i] == 3) buy_price <- (buy_price + (3*data$xiv[i]))
            if(rsi_state[i] < 0) sell_price <- (-1*rsi_state[i]*data$xiv[i])
            if((buy_price != 0) && (sell_price != 0))
              {
                ret_val[i] <- ((sell_price - buy_price)/buy_price)
                buy_price <- 0; sell_price <- 0
              }
          }
      }
    ret_val <- ret_val[which(!is.na(ret_val))]
    names(ret_val) <- data$date[which(rsi_state %in% 1)]

    return(ret_val)
  }
