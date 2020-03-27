rm(list=ls())
setwd('C:\\Users\\main\\Desktop\\Dataanalysis\\수학 및 계량\\R\\TS')

?rnorm
?cumsum #cumulative sum
?arima.sim # sample from arima model
?abline  

## random walk

epsilon<-rnorm(n=1000, mean=0, sd=1)
rand.walk<-cumsum(epsilon) # rv that sums up normal dist
plot(rand.walk, type='l', 
     xlab='Time', ylab='X', main='TS plot of random walk')

plot(cumsum(rnorm(100)),type='l',xlab='time', ylab='X', main='TS plot of random walk')

## AR model 
## AR(2) :X_{t}=0.5*X_{t-1}+0.3*X_{t-2}+e_{t}
?set.seed  # generating random number we should make initial value
#and this is a function that fix it so that we can generate same random number
set.seed(1)
ar_ex<-arima.sim(model=list(ar=c(0.5,0.3)),n=100, mean=10, sd=2)
# sd of whitenoise or innovation only changes scale not the shape itself
plot(ar_ex)
?abline
abline(h=0,col=grey(0.5),lty=2)

# burn in  # process that makes random number do not rely on initial value 
set.seed(1)
ar_ex_burnin<-arima.sim(model=list(ar=c(0.5,0.3)),n=100, n.start = 40)
# makes 140 random numbers and takes away first 40 numbers (40 is default)
plot(ar_ex_burnin)
abline(h=0, col=grey(0.5),lty=5)

#innovation option
ar_ex<-arima.sim(model=list(ar=c(0.5,0.3)), n=100 , rand.gen=function(n, ...) rt(n, df=5))
#picking 100 e_{t} from t distribution of df 5
plot(ar_ex)
abline(h=0,col=grey(0.5),lty=2)


#ar_specified
ar_spe<-arima.sim(model=list(order=c(2,0,0), ar=c(0.5,0.3)),n=100)
plot(ar_spe)                   
abline(h=0, col=grey(0.5), lty=2)
 # at the order c(p,d,q) order of AR, integrated term, order of MA

##MA model : MA(1): X_{t}=e_{t}+0.5*e_{t-1}
ma_ex<-arima.sim(model=list(ma=c(0.5)),100)
plot(ma_ex)
abline(h=0,col=grey(0.5),lty=2)

##ARMA Model: ARMA(2,1) X_{t}-0.5*X_{t-1}-0.3*X_{t-2}=e_{t}+0.5*e_{t-1}

arma_ex<-arima.sim(model=list(ar=c(0.5,0.3),ma=0.5),100)
plot(arma_ex)
abline(h=0,col=grey(0.5), lty=2)
?abline


#csv file read
library(lubridate)
dir<-choose.dir()
setwd(dir)
data<-read.csv('ex_ch1_8.csv')
fix(data)

kospi<-data[,1:2]
head(kospi)
kospi[,1]<-ymd(kospi[,1])
head(kospi)
?gsub
kospi[,2]<-gsub(",","",kospi[,2])
kospi
head(kospi)
plot(kospi, type='l', xlab='Time', ylab='kospi', main='TS plot with kospi')


## smoothing
kospi[1,2]+kospi[2,2]
typeof(kospi[,2])
kospi[,2]<-as.numeric(kospi[,2])
dim(kospi)
kospi_smooth<-c()
m=50
for (i in (m+1):length(kospi[,2])){
  kospi_smooth[i]<-mean(kospi[(i-m):i,2])
}
lines(kospi[,1],kospi_smooth,col=2)

m=3
for (i in (m+1):length(kospi[,2])){
  kospi_smooth[i]<-mean(kospi[(i-m):i,2])
}
lines(kospi[,1],kospi_smooth,col=3)
