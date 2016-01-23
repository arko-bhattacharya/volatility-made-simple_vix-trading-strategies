rm(list = ls())
## This script can be used if one wants to backtest all the strategies
## in the ./src folder
##
## This script runs from command line.
##
## To run this script set the current directory in the command line to
## ./src i.e. where all the strategies exist, then execute the
## following command - Rscript ./backtest_all.R

strat_scripts <- list.files(path = '.', pattern = 'strategy_', full.names = TRUE)
strat_scripts <- strat_scripts[(grep(pattern = 'R~', x = strat_scripts) - 1)]
strat_scripts <- strat_scripts[which(!(strat_scripts %in% "./strategy_template.R"))]

op_path <- gsub(pattern = './strategy_', replacement = '../strategy_results/',
                x = strat_scripts)
op_path <- gsub(pattern = '.R', replacement = '.RData', x = op_path)

for(i in 1:length(strat_scripts))
  {
    print(cat(i))
    print(cat(strat_scripts[i]))
    system(paste('Rscript ./backtest_strategy.R', strat_scripts[i], op_path[i], sep = ' '))
  }
