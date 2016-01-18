## >
## > head(data)
##       UX_CM1M    UX1    UX2       date
## 2231 15.21333 14.150 15.600 2015-07-14
## 2230 15.24500 14.475 15.575 2015-07-13
## 2229 17.01500 16.775 17.175 2015-07-10
## 2228 18.74000 18.825 18.675 2015-07-09
## 2227 18.29167 18.425 18.175 2015-07-08
## 2226 16.50000 16.325 16.675 2015-07-07
## >
## > str(data)
## 'data.frame':	2231 obs. of  4 variables:
##  $ UX_CM1M: num  15.2 15.2 17 18.7 18.3 ...
##  $ UX1    : num  14.2 14.5 16.8 18.8 18.4 ...
##  $ UX2    : num  15.6 15.6 17.2 18.7 18.2 ...
##  $ date   : POSIXct, format: "2015-07-14" "2015-07-13" ...
## >
rm(list = ls())
strategy <- function(data)
  {
    buy_date <- data$date[1:(norw(data) - 1)]
    sell_date <- data$date[2:norw(data)]

    buy_price <- data$UX_CM1M[which(data$date %in% buy_date)]
    sell_price <- data$UX_CM1M[which(data$date %in% sell_date)]

    buy_quantity <- rep(1, length(buy_date))
    sell_quantity <- rep(1, length(sell_date))
    
    ret_val <- (((sell_price*sell_quantity) - (buy_price*buy_quantity))/
                (buy_price*buy_quantity))
    names(ret_val) <- sell_date
    return(ret_val)
  }
