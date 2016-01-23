rm(list = ls()); library(zoo)
## This script is used to assimilate various time series data.  Some
## of these time series have been downloaded from yahoo finance while
## a few have been provided by aquifer.  The raw data sets are read
## from the '../data' folder and the final assimilated data set is
## also stored in '../data' folder.

                                        # read main data
main_data <- read.csv(file = '../data/VIXdata_20150714.csv', header = TRUE, sep = ' ',
                      stringsAsFactors = FALSE)
main_data$date <- as.POSIXct(main_data$Index); main_data$Index <- NULL
main_data <- main_data[order(main_data$date, decreasing = TRUE),]
main_data <- zoo(x = main_data[c('UX1', 'UX2', 'UX_CM1M')], order.by = main_data$date)

                                        # read VIX index
vix <- read.csv(file = '../data/y_VIX.csv', stringsAsFactors = FALSE)
vix$Date <- as.POSIXct(vix$Date)
vix <- zoo(x = vix$Adj.Close, order.by = vix$Date)

                                        # read S&P 500 index
snp <- read.csv(file = '../data/y_SnP.csv', stringsAsFactors = FALSE)
snp$Date <- as.POSIXct(snp$Date)
snp <- zoo(x = snp$Adj.Close, order.by = snp$Date)

                                        # read SPY
spy <- read.csv(file = '../data/y_SPY.csv', stringsAsFactors = FALSE)
spy$Date <- as.POSIXct(spy$Date)
spy <- zoo(x = spy$Adj.Close, order.by = spy$Date)

                                        # read XIV
xiv <- read.csv(file = '../data/y_XIV.csv', stringsAsFactors = FALSE)
xiv$Date <- as.POSIXct(xiv$Date)
xiv <- zoo(x = xiv$Adj.Close, order.by = xiv$Date)

                                        # read VXX
vxx <- read.csv(file = '../data/y_VXX.csv', stringsAsFactors = FALSE)
vxx$Date <- as.POSIXct(vxx$Date)
vxx <- zoo(x = vxx$Adj.Close, order.by = vxx$Date)

                                        # read VXV
vxv <- read.csv(file = '../data/y_VXV.csv', stringsAsFactors = FALSE)
vxv$Date <- as.POSIXct(vxv$Date)
vxv <- zoo(x = vxv$Adj.Close, order.by = vxv$Date)

                                        # read ZIV
ziv <- read.csv(file = '../data/y_ZIV.csv', stringsAsFactors = FALSE)
ziv$Date <- as.POSIXct(ziv$Date)
ziv <- zoo(x = ziv$Adj.Close, order.by = ziv$Date)

                                        # read VXZ
vxz <- read.csv(file = '../data/y_VXZ.csv', stringsAsFactors = FALSE)
vxz$Date <- as.POSIXct(vxz$Date)
vxz <- zoo(x = vxz$Adj.Close, order.by = vxz$Date)

                                        # merge data
data <- merge.zoo(main_data, vix, snp, xiv, vxx, vxv, ziv, vxz, spy, all = FALSE)
data <- as.data.frame(data, stringsAsFactors = FALSE)
data$date <- rownames(data); rownames(data) <- NULL
data$date <- as.POSIXct(data$date)
save(data, file = '../data/main_data.RData')
