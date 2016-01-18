## This is the script that runs the strategies.
## Every strategy is a function which takes in data and gives us an
## object which shows us the returns generated using that strategy.
#!/usr/bin/env Rscript
rm(list = ls())

usage = paste('Usage: ./backtest_strategy.R', 'INPUT: /strategy_file_path',
  'OUTPUT: /strategy_backtest_result', sep = ' ')

## read inputs
cargs <- commandArgs(trailingOnly = TRUE)
if (length(cargs) != 2) {
  print(cat(usage))
} else {
  file_path <- cargs[[1]]
}

## read a strategy
source(file_path)

## record strategy name
strategy_name <- unlist(lapply(X = strsplit(x = file_path, split = '/'),
                               FUN = function(k) {
                                 ret_val <- lapply(X = strsplit(x = k[2], split = '_'),
                                                   FUN = function(m) return(
                                                     substr(x = m[2], start = 1,
                                                            stop = nchar(m[2]) - 4)))
                                 
                               }))

## load data
load('../data/main_data.RData')

## run strategy
result <- strategy(data = data)

## store result
save(result, file = paste('../strategy_results/', strategy_name, '.RData', sep = ''))
print('Back test completed')
