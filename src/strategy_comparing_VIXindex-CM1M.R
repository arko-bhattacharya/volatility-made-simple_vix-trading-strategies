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
strategy <- function(data)
  {
                                        # generating a binary vector that tells us if vix
                                        # index or CM1M is greater
    cm1m_ux1 <- mapply(FUN = function(vix_index, cm1m)
                      {
                        if(vix_index <= cm1m) ret_val =  1
                        if(vix_index > cm1m) ret_val = 2
                        return(ret_val)
                      }, vix_index = data$vix, cm1m = data$UX_CM1M, SIMPLIFY = TRUE)
    
                                        # taking the froward and backward difference of the
                                        # above vector
    fdif_cm1m_ux1 <- cm1m_ux1[2:length(cm1m_ux1)] - cm1m_ux1[1:(length(cm1m_ux1) - 1)]
    bdif_cm1m_ux1 <- cm1m_ux1[1:(length(cm1m_ux1) - 1)] - cm1m_ux1[2:length(cm1m_ux1)]
    
                                        # determining the index positions when we want to buy
                                        # and sell xiv (this does not tell us the action to
                                        # be taken at the terminal points of the data-set)
    buy_xiv <- which(fdif_cm1m_ux1 %in% -1); buy_xiv <- buy_xiv + 1
    sell_xiv <- which(bdif_cm1m_ux1 %in% -1); sell_xiv <- sell_xiv + 1
    
                                        # determining the index positions when we want to buy
                                        # and sell vxx (this does not tell us the action to
                                        # be taken at the terminal points of the data-set)
    buy_vxx <- which(fdif_cm1m_ux1 %in% 1); buy_vxx <- buy_vxx + 1
    sell_vxx <- which(bdif_cm1m_ux1 %in% 1); sell_vxx <- sell_vxx + 1
    
                                        # determining the action to be taken at the terminal
                                        # points of the data set
    if(cm1m_ux1[[1]] == 1) buy_xiv <- c(1, buy_xiv)
    if(cm1m_ux1[[1]] == 2) buy_vxx <- c(1, buy_vxx)

    if(cm1m_ux1[[length(cm1m_ux1)]] == 1) sell_xiv <- c(sell_xiv, length(cm1m_ux1))
    if(cm1m_ux1[[length(cm1m_ux1)]] == 2) sell_vxx <- c(sell_vxx, length(cm1m_ux1))

                                        # dates when we want to buy and sell xiv
    buy_xiv_dates <- data$date[buy_xiv]
    sell_xiv_dates <- data$date[sell_xiv]
                                        #  dates when we want to buy and sell vxx
    buy_vxx_dates <- data$date[buy_vxx]
    sell_vxx_dates <- data$date[sell_vxx]

                                        # buy and sell prices for xiv
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
