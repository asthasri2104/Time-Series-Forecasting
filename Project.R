library(forecast)
library(ggplot2)
library(zoo)
load <- read.csv('final_data.csv')
firstHour <- as.Date("2014-01-01 00:00:00")
load.ts <- ts(load$Load, frequency = 24*365.25, start=c(2014,1,1))

load.w <- window(load.ts, start=c(2018,1),end=c(2018,24*365.25))

time(load.ts)
autoplot(load.ts)
autoplot(decompose(load.ts))
hist(load.ts)
thisyear = window(load.ts, start=c(2020,1))
autoplot(thisyear)+
  scale_x_continuous(breaks=seq(from = 2020, to = 2021, length.out = 12))

boxplot(load.ts ~ cycle(load.ts), ylab="", xlab = "")
seasonplot(load.ts, col=rainbow(24), year.labels=TRUE)

data.agg <- aggregate(load.ts, FUN = median)
plot(data.agg)


# Temperature

temp <- ts(load$mean_temp, frequency = 24*365.25, start = c(2014,1))
temp.w <- window(temp, start=c(2018,1),end=c(2018,24*365.25))

temp.agg <- rollmean(temp, align="right", k=24*30)

seasonplot(temp.agg, col=rainbow(5), year.labels=TRUE, type="l", lwd=2)

autoplot(temp, main="Hourly Mean Temperature")

autoplot(temp.w)

# moving average
ma_day <- rollmean(load.ts, k=24, align="right")
plot(ma_day)

ma_month <- rollmean(load.ts, k=24*365.25/12, align="right")
plot(ma_month)

ma_week <- rollmean(load.ts, k=24*7, align="right")
plot(ma_week)

autoplot(load.ts)+autolayer(ma_day)+autolayer(ma_week)+autolayer(ma_month)

## External variables
data <- read.csv("final_data.csv")

data$Date = ymd_hms(data$Date)
new_load = data[,c('Load','Date')]
new_temp = data[,c('mean_temp','Date')]
load.ts = xts(new_load$Load, order.by = new$Date)
temp.ts = xts(new_temp$mean_temp, order.by = new$Date)

load.ts <- ts(new_load$Load)

autoplot(load.ts) + labs(titles = "Hourly Electricity Demand", x = "year")
autoplot(temp.ts) + labs(titles = "Hourly Mean Temperature", x = "year")

library(tseries)
#?adf.test

adf.test(load.ts, alternative = "stationary", k=0)
adf.test(temp.ts, alternative = "stationary", k=0)

pp.test(load.ts, alternative = "stationary")
pp.test(temp.ts, alternative = "stationary")

pairs(cbind(Load=load.ts, Temperature=temp.ts))

#Training
#load.tr <- window(load.ts, start=c(2018,8), end=c(2019,8))
#temp.tr <- window(temp.ts, start=c(2018,8), end=c(2019,8))


trend = time(load.ts)

library(forecast)
m1 = tslm(load.ts ~ trend)
m2 = tslm(load.ts ~ trend + temp.ts)
m3 = tslm(load.ts ~ trend + temp.ts + I(temp.ts^2))