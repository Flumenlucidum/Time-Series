library(forecast)
#ARIMA simulation

##data generating ARIMA(1,1,1)
set.seed(1)
arma_sample<-arima.sim(model=list(ar=c(0.2),ma=c(0.3)),n=200)
plot(arma_sample)
#cumulative sum of arma sample
arima_sample<-cumsum(arma_sample)

plot(arima_sample, type='l')

set.seed(1)
arima_sample2<-arima.sim(model=list(order=c(1,1,1),ar=c(0.2),ma=c(0.3)),n=200)
points(arima_sample2,col=3, type='l') # almost same 

#data fitting
set.seed(1)
data<-arima.sim(model=list(order=c(1,1,1),ar=c(0.5),ma=c(0.3)),n=220)
traindata<-data[1:200]
arima.fit<-arima(traindata, order=c(1,1,1))
arima.forecast<-forecast(arima.fit, h=20)
arima.fit

plot(arima.forecast, col=3)
points(data,type='l')

#one step ahead forecast using new observations without reestimation
arima.forecast2<-Arima(data[201:220],model=arima.fit)
arima.forecast2
arima.forecast2$fitted
accuracy(arima.forecast2)
# big Arima is wrapper function for arima 
# adding more functions than the previous arima function 
#brings the coefficient from arima fit 

arima_RMSE<-sqrt(mean(arima.forecast2$residuals^2))
arima_RMSE

#one step forecasting using the new observation 
prediction=c()
for (i in 1:20){
  data2<-data[1:c(199+i)]
  arima.fitting<-arima(data2,order=c(1,1,1))
  phi<-as.numeric(arima.fitting$coef[1]); theta<-as.numeric(arima.fitting$coef[2])
  prediction[i]<-data[199+i]+phi*(data[199+i]-data[198+i])+theta*arima.fitting
}