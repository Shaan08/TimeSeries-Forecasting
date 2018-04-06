
mydata <- read.csv("./PK_data/data.csv", sep = ',')


library(dplyr)
mydata$time <-as.POSIXct(mydata[,1], format="%Y-%m-%d %H:%M:%S")
mydata$idle <- as.numeric(mydata[,2])
myvar <- c("time","idle")
myvar1 <- c("time","idle","memused")

newdata <-mydata[myvar]
newdata1 <- mydata[myvar1]
prophet_df <- mydata[myvar]
#data55 <- (newdata %>% filter(vm == "vmy2955"))
#data54 <- (newdata %>% filter(vm == "vmy2954"))


require(zoo)
newdata.zoo <- with(newdata, zoo(idle, order.by = time))
#data55.zoo <- with(data55, zoo(V3, order.by = time))
#data54.zoo <- with(data54, zoo(V3, order.by = time))

require(xts)
tsdata <- as.xts(newdata.zoo)
#tsdata <- as.xts(data54.zoo)


#changing data to ts else the arima models wont work

df_ts <- as.ts(x = newdata[, -1], order.by = newdata$time)


library(tseries)

cpu <-tsdata

#cpudiff <- diff(cpu, differences=1)
#cpudiff <- na.omit(cpudiff)
#cpu54 <- means54.rounded
#adf = adf.test(na.omit(cpudiff))
#adf
newcpu <- df_ts


library(TSstudio)
ts_plot(cpu,slider = TRUE)
#ts_plot(fcst1,slider = TRUE)

#ndiffs(cpu, alpha = 0.05, test = c("kpss", "adf", "pp"), max.d = 2)

library(forecast)
#vmy 2955#
fit_arima <- auto.arima(newcpu,stepwise = FALSE, approximation = FALSE,trace = FALSE)
fit_nn <-nnetar(newcpu, size = 10,repeats = 10)
#fit3 <- arfima(cpu,drange = c(0, 0.8),estim = c("mle"))

fcst_nn <- forecast(fit_nn,PI = TRUE,h = 10)
fcst_arima <- forecast(fit_arima,PI = TRUE,h = 10)
fcstdf <- as.data.frame(forecast(fit_nn,PI = TRUE))












#vmy2954
#fit_arima54 <- auto.arima(cpu54,stepwise = FALSE, approximation = FALSE,trace = FALSE)
#fit_nn54 <-nnetar(cpu54, decay = 0.005,repeats = 20)
#fit3 <- arfima(cpu,drange = c(0, 0.8),estim = c("mle"))
#fcst54 <- forecast(fit_nn54,PI = FALSE,h = 60)
#fcstdf54 <- as.data.frame(forecast(fit_nn54,PI = TRUE))

library(ggplot2)
#autoplot(fcst54)
autoplot(fcst_arima,PI=TRUE)
autoplot(fcst_nn,PI=TRUE)


library('forecastHybrid')
modhybrid <-hybridModel(newcpu, models = "aen", lambda = NULL, a.args = NULL,
e.args = NULL, n.args = NULL, s.args = NULL, t.args = NULL,
weights = "equal",
errorMethod = c("RMSE"),
parallel = FALSE, verbose = TRUE)

#to check the components of the model like nnetar,auto.arima


fc <- forecast(modhybrid,h =50, level = c(80, 95))
#plot(newcpu, type = , ggplot = TRUE)
plot(modhybrid, type = "fit", ggplot = TRUE)
#summary(modhybrid)

library('prophet')
colnames(prophet_df) <- c("ds", "y")

m <- prophet(prophet_df,yearly.seasonality = "auto",
weekly.seasonality = "auto", daily.seasonality = "auto",)
future <- make_future_dataframe(m,periods = 24,freq = 3600)


forecast <- predict(m, future)

plot(m, forecast)


prophet_plot_components(m, forecast)



library(forecastxgb)
model <- xgbar(newcpu,seas_method = "fourier", trend_method = "none",lambda = 1)
fc_xgb <- forecast(model, h = 50)
accuracy(fc_xgb)

plot(fc_xgb)


modhybdf <- as.data.frame(forecast(modhybrid,h =10, PI = TRUE,fan = TRUE))
modhybdf

ggtsdisplay(newcpu, plot.type="histogram",points = TRUE,smooth = TRUE)

accuracy(fit_nn)

accuracy(fit_arima)

fcstdf



require(zoo)
multiidle <- newdata1[,2]
multimem <- newdata1[,3]
multi <- cbind(multiidle,multimem)
newdata1.zoo <- with(newdata1, zoo(multi, order.by = newdata1$time))
require(xts)
multitsdata <- as.xts(newdata1.zoo)
#tsdata <- as.xts(data54.zoo)


multidf_ts <- as.ts(x = newdata1[, -1], order.by = newdata1$time)
autoplot(multidf_ts,shape = 10,facets = FALSE,series = TRUE,ylab = "Idle & Memory")

idly <-multidf_ts[,1]
memu <- multidf_ts[,2]
multi_model <- xgbar(y = idly, xreg = memu)


cpu_future <- matrix(forecast(xgbar(multidf_ts[,2]), h = 10)$mean, 
                        dimnames = list(NULL, "memused"))
plot(forecast(multi_model, xreg = cpu_future))

#library('MAPA')
#mapafit.x <- mapaest(newcpu,display = 1,xreg = )
#frc <- mapafor(newcpu,mapafit,ifh=6,fh=6,conf.lvl=c(0.8,0.9,0.95),comb="w.mean")
#frc$infor


#plot(1:505,newcpu, type="l",xlim=c(1,520),xaxs="i",main="MAPA",xlab="TIME",ylab="CPU")
#for (i in 1:505){
#  lines(i:(506+i),exp(frc$infor[,i]),col="red")
#}

