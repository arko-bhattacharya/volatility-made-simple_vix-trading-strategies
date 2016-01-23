## This is the script that runs the strategies.
## Every strategy is a function which takes in data and gives us an
## object which shows us the returns generated using that strategy.
##
## This script is designed for execution from command line.
## The inputs to this script are the strategy script with its path and
## the output file name with its desired storage location.
## Example: If one wants to run ./src/strategy_rsi2.R, then one should
## set the working directory to ./src and then type the following
## command -
## Rscript backtest_strategy.R ./strategy_rsi2.R ../strategy_results/rsi2.RData
## i.e
## Rscript [file_path]/backtest_strategy.R [file_path]/[strategy name] [file path]/[desired
## name for storing strategy result]

#!/usr/bin/env Rscript
rm(list = ls())

usage = paste('Usage: ./backtest_strategy.R', 'INPUT: /strategy_file_path',
  'OUTPUT: /strategy_backtest_result', sep = ' ')

## read inputs
cargs <- commandArgs(trailingOnly = TRUE)
if (length(cargs) != 2) {
  print(cat(usage))
} else {
  ip_file_path <- cargs[[1]]
  op_file_path <- cargs[[2]]
}

## read a strategy
source(ip_file_path)

## load data
load('../data/main_data.RData')

## run strategy
result <- strategy(data = data)

## store result
op_file_path <- gsub(pattern = 'NULL', replacement = '', x = op_file_path)
save(result, file = op_file_path)
print('Back test completed')
