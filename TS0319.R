print('hello world')

1+1

a<-2; b=3
c<-a+b; c; print(c)

#list objects
ls()

#remove objects
rm(list=ls())

#help
?rm
help(rm)

### data types ###

## vector ##
a<-c(1,2,5,6,-2.4) #numeric vector
b<-c('two','three','four')#character vector
c<-c(TRUE,TRUE,TRUE,FALSE,TRUE)#logical vector
a[1]
b[2];c[3]
a[-2]
a[c(1,3)]

## data frame
d<-1:4
e<-c('red','white','red',NA)
f<-c(TRUE,TRUE,TRUE,FALSE)
testdata<-data.frame(d,e,f)
testdata

names(testdata)<-c('ID','Color','passed')#variable names
testdata
testdata[1,]
testdata[,1]
testdata[,c(1,3)]
testdata[,c(1:3)]

##matrix

g<-matrix(c(0:3),2,2);g
g1<-matrix(c(0:5),2,3);g1
g2<-matrix(c(2,3,4,5,0,1),3,2);g2
m1<-matrix(c(2,3,4,5,0,1),3,2,byrow=T);m1
# default is column vector 
dim(g1) # row *column

g1%*%g2
g2%*%g1
t(g1)*g2 #componentwise multiplication
#t-> transpose

##list variable
h<-list(a,e,g);h
h[[1]]
h[[2]][1]
h[[3]][1,2]

####function####

a 
mean(a);median(a)
var(a);sd(a)
summary(a)
boxplot(a)

which.max(c(11:20)) #location of maximum value
which(c(11:20)!=20)  #location which is not 20

# rnorm :sample from normal distribution
i<-rnorm(1000,0,1)
j<-1.5*i+rnorm(1000,0,1)
mean(i)
var(i)

#dnorm; pdf value of normaldist at ()
dnorm(0)

#pnorm: cdf value of normal dist at ()
pnorm(1.96)

#qnorm: quantile value of normal dist at()
qnorm(0.5)
qnorm(0.975)

##user defined function

temp_function<-function(a,b){
  temp1<-a-b
  return(temp1)
}
temp_function2<-function(a,b){
  temp2_1=a+b
  temp2_2=a-b
  temp2<-list(temp2_1,temp2_2)
  return(temp2)
}

test2<-temp_function2(1,2)
test2

##loop
test3<-0; test3
test4<-c(); test4 #null
for (i in 1:10){
  test3<-test3+i
  test4[i+1]<-i
}
test3
test4


##plots
stem(i)
par(mfrow=c(1,2))
hist(i)
plot(i)
par(mfrow=c(1,1))
plot(i,type='l') # connected graph 

plot(1:20,1:20, pch=1:20) # plotting with symbols
?legend
legend('topleft',legend=c('A','B'),col=c('red','blue'),lty=1,lwd=2,cex=.8)
dev.off()

regfit<-lm(j ~ i)#linear regression
regfit
plot(i,j)
abline(regfit,col=2)

#ggplot package should be used 

## data analysis 
install.packages('lubridate')
getwd() #ctrl alt r  커서의 수늘ㄹ
setwd('C:\\Users\\main\\Desktop\\Dataanalysis\\수학 및 계량\\R\\TS')
library(lubridate)
#read a csv file 
kospi<-read.csv('kospi.csv')
head(kospi)
fix(kospi)
tail(kospi,10)
dim(kospi)
#mdy 
kospi[,1]<-mdy(kospi[,1]);head(kospi);tail(kospi)
?mdy
head(kospi)
fix(kospi)
kospi2<-kospi[,c(1,2)]; head(kospi2)
?which
a<-which(diff(as.numeric(substr(kospi2[,1],6,7)))!=0)
head(a)

kospi_mean<-c()
for (i in 1:length(a)){
  if (i==1){
    kospi_mean[i]<-mean(kospi[1:a[i],2])
  }
if(i!=1){
  kospi_mean[i]<-mean(kospi[(a[i-1]+1):a[i],2])
}
}
kospi3 <- data.frame(as.Date(kospi2[a+1,1], origin="1970-01-01"), kospi_mean)

colnames(kospi3) <- c("Time", "Closing_Price")
head(kospi3)
tail(kospi3)

plot(kospi3, main= "KOSPI data", type='o')


