rm(list = ls())

##-----------------------------------------------------------------------------------------##
                                        # read srategy backtest results
files <- list.files(path = '../strategy_results/', full.names = TRUE)
strategy_names <- list.files(path = '../strategy_results/', full.names = FALSE)
strategy_names <- gsub(pattern = '.RData', replacement = '', x = strategy_names)
strategy_names <- gsub(pattern = '-', replacement = ' ', x = strategy_names)
strategy_names <- gsub(pattern = '_', replacement = ' ', x = strategy_names)

##-----------------------------------------------------------------------------------------##
## FUNCTIONS

##-----------------------------------------------------------------------------------------##
## MAIN
load(files[12])
strat_results <- lapply(X = files,
                        FUN = function(k)
                        {
                          load(k); ip <- result
                          ip$buy_date <- as.POSIXct(ip$buy_date)
                          ip$sell_date <- as.POSIXct(ip$sell_date)
                          op <- c(get_annual_return(ip), get_avg_ret_perannum(ip),
                                  get_sharpe_ratio(ip),
                                  get_max_drawdown(ip), get_ulcer_performance_index(ip),
                                  get_cor_with_snp(ip),
                                  get_best_month_ret(ip), get_worst_month_ret(ip),
                                  get_best_return(ip), get_worst_return(i),p
                                  get_hit_rate(ip), get_trades_per_year(ip))
                          names(op) <- c('Annual return', 'Average per annum return',
                                         'Sharpe ratio', 'Max drawdown',
                                         'Ulcer performance index', 'Corr with SnP 500',
                                         'Best month', 'Worst month',
                                         'Best return (p.a.)', 'Worst return (p.a.)',
                                         'Hit rate', 'Trades per year')
                          return(op)
                        })
names(strat_results) <- strategy_names
strat_results <- do.call(rbind, strat_results)
