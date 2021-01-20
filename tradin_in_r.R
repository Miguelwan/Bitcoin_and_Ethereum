?PerformanceAnalytics
?quantmod
library(quantmod)
library(PerformanceAnalytics)
library(dplyr)
library(caret)
library(tidyverse)
library(ggplot2)
library(tibble)

#choose the date to start collecting the data
date <- "2020-1-1"

#collect the data from yahoo
LTCUSD <- getSymbols.yahoo("LTC-USD", from = date, auto.assign = F)[,] #Litecoin
ETHUSD <- getSymbols.yahoo("ETH-USD", from = date, auto.assign = F)[,] #Ethereum
XRPUSD <- getSymbols.yahoo("XRP-USD", from = date, auto.assign = F)[,] #Ripple
BTCUSD <- getSymbols.yahoo("BTC-USD", from = date, auto.assign = F)[,] #Bitcoin

#extract highest value of the day
LTCHigh <- LTCUSD[,2]
ETHHigh <- ETHUSD[,2]
XRPHigh <- XRPUSD[,2]
BTCHigh <- BTCUSD[,2]

#graphic the data
chartSeries(LTCHigh)
chartSeries(ETHHigh)
chartSeries(XRPHigh)
chartSeries(BTCHigh)

#extract closed value of the day
LTCClose <- LTCUSD[,4]
ETHClose <- ETHUSD[,4]
XRPClose <- XRPUSD[,4]
BTCClose <- BTCUSD[,4]

#graphic the data
chartSeries(LTCClose)
chartSeries(ETHClose)
chartSeries(XRPClose)
chartSeries(BTCClose)

#extract the adjusted value of the day
LTCAdjust <- LTCUSD[,6]
ETHAdjust <- ETHUSD[,6]
XRPAdjust <- XRPUSD[,6]
BTCAdjust <- BTCUSD[,6]

#extract the returns
LTCRets <- na.omit(dailyReturn(LTCClose, type = "log"))
ETHRets <- na.omit(dailyReturn(ETHClose, type = "log"))
XRPRets <- na.omit(dailyReturn(XRPClose, type = "log"))
BTCRets <- na.omit(dailyReturn(BTCClose, type = "log"))

#graph the returns
chartSeries(LTCRets)
chartSeries(ETHRets)
chartSeries(XRPRets)
chartSeries(BTCRets)

#Prepare the data frames
dfLTC <- data.frame(LTCUSD)
LTCwith_ticket <- mutate(dfLTC, Company_Ticket = "LTC-USD", Date = row.names(dfLTC))
row.names(LTCwith_ticket) <- 1:nrow(LTCUSD)
colnames(LTCwith_ticket) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted", "Ticket", "Date")

dfETH <- data.frame(ETHUSD)
ETHwith_ticket <- mutate(dfETH, Company_Ticket="ETH-USD", Date = row.names(dfETH))
row.names(ETHwith_ticket) <- 1:nrow(ETHUSD)
colnames(ETHwith_ticket) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted", "Ticket", "Date")

dfXRP <- data.frame(XRPUSD)
XRPwith_ticket <- mutate(dfXRP, Company_Ticket="XRP-USD", Date = row.names(dfXRP))
row.names(XRPwith_ticket) <- 1:nrow(XRPUSD)
colnames(XRPwith_ticket) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted", "Ticket", "Date")

dfBTC <- data.frame(BTCUSD)
BTCwith_ticket <- mutate(dfBTC, Company_Ticket="BTC-USD", Date = row.names(dfBTC))
row.names(BTCwith_ticket) <- 1:nrow(BTCUSD)
colnames(BTCwith_ticket) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted", "Ticket", "Date")

Cripto_index <- rbind(LTCwith_ticket, ETHwith_ticket, XRPwith_ticket, BTCwith_ticket)
Cripto_index_ordered <- Cripto_index[, c(7, 8, 1, 2, 3, 4, 5, 6)]

#overview
str(Cripto_index_ordered)
summary(Cripto_index_ordered)
sd(Cripto_index_ordered$Adjusted)

meanLTC <- mean(LTCwith_ticket$Adjusted)
meanETH <- mean(ETHwith_ticket$Adjusted)
meanXRP <- mean(XRPwith_ticket$Adjusted)
meanBTC <- mean(BTCwith_ticket$Adjusted)

sdLTC <- sd(LTCwith_ticket$Adjusted)
sdETH <- sd(ETHwith_ticket$Adjusted)
sdXRP <- sd(XRPwith_ticket$Adjusted)
sdBTC <- sd(BTCwith_ticket$Adjusted)

CoefLTC <- (sdLTC/meanLTC)*100
CoefETH <- (sdETH/meanLTC)*100
CoefXRP <- (sdXRP/meanLTC)*100
CoefBTC <- (sdBTC/meanLTC)*100

#Get the recent prices
date2 <- "2020-12-1"
tickers <- c("LTC-USD", "ETH-USD", "XRP-USD", "BTC-USD")

portfolioPrices <- NULL
for (ticker in tickers){
  portfolioPrices <- cbind(portfolioPrices, 
                           LTCUSD <- getSymbols.yahoo(ticker,
                                                      from = date2,
                                                      periodicity = "daily",
                                                      auto.assign = F)[,6])
  
}

#prepare the dataframe to plot
portfolioPrices <- as.data.frame(portfolioPrices)
portfolioPrices <- rownames_to_column(portfolioPrices, var = "Date")
df_for_plot <- portfolioPrices %>%
  gather(key = "variable", value = "value", -Date)

