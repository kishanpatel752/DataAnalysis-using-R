#################################################
##                 PROG8430                    ##
##               ASSIGNMENT 02                 ## 
#################################################
#################################################
##          Written by Kishan Patel            ## 
##               ID: 8781642                   ##
#################################################
##           Time Series Analysis              ##
#################################################

#For clearing existing plots
if(!is.null(dev.list())) dev.off()

#For clearing the console
cat("\014")

#For clearing workspace
rm(list=ls())

setwd("C:/Users/Admin1/Documents/R")

options(scipen = 9)

#############################################################
###                                                       ###
###        SECTION : 1 (Woodstock Temperature)            ###
###                                                       ###
#############################################################

load("Woodstock_21F.Rdata")

str(Woodstock_21F)

head(Woodstock_21F)

#Answer 1.1
TempTS_KP<-ts(Woodstock_21F,frequency = 4,start = c(1988,1), end = c(2017,4))
TempTS_KP

#Answer 2.1
summary(TempTS_KP)

#Answer 2.2
plot.ts(TempTS_KP, main = "Graph of Woodstock Temperature Data", ylim=c(-20,20), xlab = "Year", ylab="Temperature")

#Answer 2.3
decompTS_KP <- decompose(TempTS_KP, type = "additive")
decompTS_KP
plot(decompTS_KP)

#Answer 2.4
library(tseries)
adf.test(TempTS_KP) #This time series is stationary as p-value is under 0.05

#Answer 2.5
TempDeseas_KP <- TempTS_KP - decompTS_KP$seasonal
plot.ts(TempDeseas_KP, main = "Deseasonalized Woodstock data", ylim = c(-15,25), xlab = "Year", ylab = "Temperature")

#Answer 2.6
#Trend over the years does not change at all. It almost follows the same pattern over the span of few years so it is likely to predict the data for upcoming years easily. If we look at the 'random' graph, it fluctuates a lot. The temperature goes higher and then it goes lower after reaching the peak point. 



#############################################################
#############################################################
###                                                       ###
###           SECTION : 2 (AYR Temperature)               ###
###                                                       ###
#############################################################
#############################################################

load("Ayr_21F.Rdata")

str(Ayr_21F)

head(Ayr_21F)

#Answer 1.1
TempAyr_KP<-ts(Ayr_21F,frequency = 1,start = c(1968), end = c(2003))
TempAyr_KP

#Answer 2.1
summary(TempAyr_KP)

#Answer 2.2
plot.ts(TempAyr_KP, main = "Graph of AYR Temperature Data", ylim=c(10.5,14), xlab = "Year", ylab="Temperature")

#Answer 2.3
library(TTR)
TempSMA5_KP <- SMA(TempAyr_KP, n=5)
plot.ts(TempSMA5_KP)

TempSMA10_KP <- SMA(TempAyr_KP, n=10)
plot.ts(TempSMA10_KP)
  
TempSMA15_KP <- SMA(TempAyr_KP, n=15)
plot.ts(TempSMA15_KP)
#Observation : There are three graphs shown above for different values (n) to smoothen the temperature chart using Simple Moving Average (SMA). And we can clearly notice that the graph with the value of 15 (n=15) smoothens the temperature chart more than n=5 and n=10 do.

#Answer 2.4
library(tseries)
adf.test(TempAyr_KP) #This time series is not stationary as p-value is above 0.05

#Answer 2.5
acf(TempAyr_KP)
#Observation : The Lags which cross the blue line are generally considered as the significant. So, Lag 0, Lag 3, Lag 6, Lag 9 are considered as significant lags. The previous values seem to influence current values up to some extent as the graph is gradually showing downward trend with uneven heights in between.

#Answer 2.6
#Trend over the years does not change at all. It almost follows the same pattern over the span of few years so it is likely to predict the data for upcoming years easily. If we look at the 'random' graph, it fluctuates a lot.The temperature goes higher and then it goes lower after reaching the peak point. 

#Answer 3.1
library(smooth)
moveavg_KP <- sma(TempAyr_KP)
moveavg_KP
moveavg_KP <- forecast(moveavg_KP, h=5, level=0.75)
moveavg_KP
plot(moveavg_KP)

#Answer 3.2
Esavg_KP <- es(TempAyr_KP)
Esavg_KP
Esavg_KP <- forecast(Esavg_KP, h=5, level=0.75)
Esavg_KP
plot(Esavg_KP)

#Answer 3.3
#Forecasts created in answer 3.1 and answer 3.2 are somewhat similar but it would be slightly difficult to understand the overall trend and forecast from the graph generated from answer 3.1 as the overall trend follows the gradual upward path with uneven results whereas it would be easier to understand the trend from exponential graph as it is linear and helps to predict the forecast easily. The forecast cannot always be accurate as we assumed/predicted, so exponential graph can get you better understanding on the variations of forecast.  