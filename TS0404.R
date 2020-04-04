rm(list=ls())
install.packages('forecast') #for forecasting, includes arima fitting function
install.packages('quantmod')  #call financial time series

library('forecast')
#arima.sim-> sampling from arima
#arima- arima fitting
library('quantmod')
#data sample  sampling 200 samples with ARMA(2,1)
set.seed(1)
dat<-arima.sim(model=list(ar=c(0.6,0.3),ma=c(0.2)),n=200)
plot(dat)

#fitting AR(1)
fit_ar<-arima(dat, order=c(1,0,0), include.mean=F) #default is TRUE
fit_ar
names(fit_ar)
fit_ar$coef
fit_ar$residuals
points(fitted(fit_ar),col=2, type='l')#point in original plot

#fitting MA(1) model
fit_ma<-arima(dat,order=c(0,0,1),include.mean = F)
fit_ma
points(fitted(fit_ma),col=3, type='l')

#fitting ARMA(2,1) model
fit_arma<-arima(dat, order=c(2,0,1),include.mean = F)
fit_arma
points(fitted(fit_arma), col=4, type='l')

#check 
sum(fit_ar$residuals^2)
sum(fit_ma$residuals^2)
sum(fit_arma$residuals^2)

#prediction 
#h : number of periods for forecasting
pred_arma<-forecast(fit_arma,h = 20)
pred_arma # expected values and confidence interval
plot(pred_arma)
?forecast
pred_arma2<-forecast(fit_arma, find.frequency=T)
#find appropriate frequency -> if there is seasonality, it will provide value for a season
pred_arma2
plot(pred_arma2)

?arima
?acf

#acf and pacf
set.seed(1)
dat_ar<-arima.sim(model=list(ar=c(-0.5)),n=500)
dat_ma<-arima.sim(model=list(ma=c(-0.5)),n=500)
dat_arma<-arima.sim(model=list(ar=c(0.5),ma=c(0.5)),n=500)

plot(dat_ar)
plot(dat_ma)
plot(dat_arma)

par(mfrow=c(1,2))

acf(dat_ar)
pacf(dat_ar)


acf(dat_ma)
pacf(dat_ma)


acf(dat_arma)
pacf(dat_arma)

ar_acf=acf(dat_ar)
ar_acf # note that lag 0 -> 1 

# auto arima--> automatically gives order to data 
#just for check, model should be selected by me
set.seed(1)
# default is include mean false
dat_arma<-arima.sim(model=list(ar=c(0.5),ma=c(0.5)),n=2000)
plot(dat_arma)
fit_auto<-auto.arima(dat_arma)
fit_auto
par(mfrow=c(1,1))
plot(forecast(fit_auto,h=20),xlim=c(1800,2020))

#Lag a time series
#push backward and fill with NA
testdata<-dat_ar[1:10]; testdata
?Lag
Lag(testdata,3)

##non-invertible MA(1)

set.seed(1)
invma<-arima.sim(model=list(ma=c(0.5)),n=200,n.start=NA)

set.seed(1)
non_invma<-arima.sim(model=list(ma=c(2)),n=200, n.start=NA, sd=0.5)

par(mfrow=c(1,2))
plot(invma, type='l')
plot(non_invma,type='l')
par(mfrow=c(1,1))
#preference : invertible because parameter space(?) is (0,1)
#whereas that for non invertible one is (1,infinity)
# easy when doing inference, symmetric
#stationary-> solution not in unit disk (단위원)
#expressible in AR(infinity)
#problem of non invertibility is that it gives more weight to
#more distant past


#fitting MA(1)
fit_ma<-arima(non_invma,order=c(0,0,1), include.mean = F)
fit_ma$coef

#quantmod
#get data from webpages

usd_krw<-getSymbols(Symbols = 'USD/KRW', src='oanda',from=Sys.Date()-30,to=Sys.Date(),auto.assign = F)

plot(usd_krw)

kospi<-getSymbols(Symbols='^KS11',
                  src='yahoo',
                  from='2017-01-01',
                  to='2017-05-01',auto.assign = F)

plot(kospi)
kospi
apple<-getSymbols(Symbols='AAPL',
                  src='yahoo',
                  from=Sys.Date()-100,
                  to=Sys.Date(),auto.assign = F)
plot(apple)

samsung<-getSymbols('005930.KS',
                    src='yahoo',
                    from=Sys.Date()-60,
                    to=Sys.Date(),auto.assign = F)
plot(samsung)
