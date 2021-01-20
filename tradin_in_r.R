?PerformanceAnalytics
?quantmod
library (quantmod)
library (PerformanceAnalytics)



date <- "2020-1-1"
BCClose <- getSymbols.yahoo("BTC-USD", from = date, auto.assign = F)[,6]
BCClose

BCRets <- dailyReturn(BCClose, type = "log")

chartSeries(BCRets)
