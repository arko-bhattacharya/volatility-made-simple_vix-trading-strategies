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
    data_tmp <- zoo(x = data[c('vix', 'vxv')], order.by = data$date)
    data_tmp$vix_2 <- (data_tmp$vix*data_tmp$vix)

                                        # converting the above zoo obejct into a row
                                        # wise list
    x_bar <- vector(mode = 'list', length = (nrow(data_tmp) - 149))
    for(i in 150:nrow(data_tmp)) {
      x_bar[[i - 149]] <- c(1, as.numeric(data_tmp[,c('vix', 'vix_2')][i,]))
    }
                                        # caculating the rolling quad regression estimate
    model <- vector(mode = 'list', length = (nrow(data_tmp) - 149))
    for(i in 150:nrow(data_tmp))
      {
        model[[i - 149]] <- as.numeric(coef(
          lm(data_tmp$vxv[(i - 149):i] ~ (data_tmp$vix[(i - 149):i] +
                                          data_tmp$vix_2[(i - 149):i]))))
      }
    vxv_cap <- unlist(mapply(FUN = function(formula, X) return(sum(formula * X)),
                            formula = model, X = x_bar))

                                        # delta
    delta <- data$vxv[150:nrow(data)] - vxv_cap
    delta[which(delta >= 0)] <- TRUE; delta[which(delta < 0)] <- FALSE
                                        # vix vxv comparison
    vix_vxv_comp <- apply(X = as.matrix(data_tmp[150:nrow(data),][,c('vix', 'vxv')],
                            stringsAsFactors = FALSE), MARGIN = 1,
                          FUN = function(k) {
                            if(k[2] > k[1]) return(1) else return(0)})

                                        # buy and sell points
    pos <- delta + vix_vxv_comp
    pos[which(pos != 2)] <- NA
    runs <- rle(is.na(as.numeric(pos)))
    for(i in 1:length(runs$lengths))
      {
        if((runs$lengths[i] == 1) && (runs$values == TRUE)) pos[sum(runs$lengths[1:i])] <- 2
      }
    runs <- rle(is.na(as.numeric(pos)))
    bs_pos <- pos
    for(i in 2:length(pos))
      {
        if(is.na(pos[i]) && (pos[i-1] %in% 2)) bs_pos[i] <- 2
      }
    runs <- rle(is.na(as.numeric(bs_pos)))
    bs_pos <- unlist(mapply(FUN = convertRun, runs[['values']], runs[['lengths']]))
    names(bs_pos) <- names(pos)

                                        # buy and sell dates
    buy_date <- names(bs_pos)[which(bs_pos %in% 1)]
    sell_date <- names(bs_pos)[which(bs_pos %in% -1)] 

                                        # buy and sell prices
    buy_price <- data$xiv[which(data$date %in% as.POSIXct(buy_date))]
    sell_price <- data$xiv[which(data$date %in% as.POSIXct(sell_date))] 

                                        # retrns
    ret_val <- (((sell_price) - (buy_price))/(buy_price))
    names(ret_val) <- buy_date
    return(ret_val)
  }
