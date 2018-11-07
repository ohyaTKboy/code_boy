options(stringsAsFactors = F,fileEncoding="utf8")
install.packages("readxl")
install.packages("ggplot2")
install.packages("dplyr")
library(readxl)
library(ggplot2)
library(dplyr)

##_______________________________read exchange rate and turn to panel form_______________________

us_ex<-read.csv("D:/exchange_rate/ExchangeRate_US_20180106.csv",header=T,sep=",")
us_ex_buy<-subset(us_ex[,c(1:12)])
us_ex_sell<-subset(us_ex[,c(1:2,13:ncol(us_ex))])
colnames(us_ex_sell)<-c("date","currency","exchange_type","Cash","intime","far_10days","far_20days",  "far_30days",
               "far_90days","far_120days","far_150days","far_180days")


str(us_ex_buy)
str(us_ex_sell)

us_ex_buy$exchange_type<-gsub("本行買入","buy",us_ex_buy$exchange_type)
us_ex_sell$exchange_type<-gsub("本行賣出","sell",us_ex_sell$exchange_type)


View(us_ex_buy)
View(us_ex_sell)

##___________________plot_________________________________


buy_p<-ggplot(us_ex_buy,aes(x=date))+geom_line(aes(y=Cash,color="Cash"))+geom_line(aes(y=intime,color="intime"))+
  geom_line(aes(y=far_10days,color="far_10days"))+geom_line(aes(y=far_20days,color="far_20days"))+geom_line(aes(y=far_30days,color="far_30days"))+
  geom_line(aes(y=far_90days,color="far_90days"))+geom_line(aes(y=far_120days,color="far_120days"))+geom_line(aes(y=far_150days,color="far_150days"))+
  geom_line(aes(y=far_180days,color="far_180days"))+scale_color_discrete(name="exchange type")+labs(title="USD exchange price(sell)",y="price")+
  theme_bw()
              
  
buy_p

##從圖中可發現，本行買入的匯率在月初變動較大，月中後較為穩定


sell_p<-ggplot(us_ex_sell,aes(x=date))+geom_line(aes(y=Cash,color="Cash"))+geom_line(aes(y=intime,color="intime"))+
  geom_line(aes(y=far_10days,color="far_10days"))+geom_line(aes(y=far_20days,color="far_20days"))+geom_line(aes(y=far_30days,color="far_30days"))+
  geom_line(aes(y=far_90days,color="far_90days"))+geom_line(aes(y=far_120days,color="far_120days"))+geom_line(aes(y=far_150days,color="far_150days"))+
  geom_line(aes(y=far_180days,color="far_180days"))+scale_color_discrete(name="exchange type")+labs(title="USD exchange price(sell)",y="price")+
  theme_bw()


sell_p



##______________________________stationarity check______________________________


var(us_ex_buy$Cash)# check its variance


cash.diff<-diff(us_ex_buy$Cash)
cash.diff<-as.data.frame(cash.diff)
cash.diff<-cbind(cash.diff,us_ex_buy$date[-120])
colnames(cash.diff)<-c("cash.diff","date")
cash.diff_p<-ggplot(cash.diff,aes(date,cash.diff))+geom_line(size=0.7)+theme_bw()
cash.diff_p


acf(cash.diff$cash.diff,lag.max = 10)


## or use tseries package

library(tseries)

adf.test(us_ex_buy$Cash) #we conclude the data is stationary. 


## _____________________________original data autocorrelation_____________________


lagged<-data_frame(lag(us_ex_buy$Cash,1))

for(i in 2:10){
  lagged.temp<-lag(us_ex_buy$Cash,i)
  lagged<-cbind(lagged,lagged.temp)
}

colnames(lagged)<-c("cash.lag1","cash.lag2","cash.lag3","cash.lag4","cash.lag5","cash.lag6",
                    "cash.lag7","cash.lag8","cash.lag9","cash.lag10")


cor(us_ex_buy$Cash,lagged$cash.lag10,use="na.or.complete")


## or use acf function

acf(cash_buy.d,lag.max = 10) # the price of buying is highly autocorrelated. 


##_________________________autoregression____________________________

cash.arma<-arma(us_ex_buy$Cash,c(0,1))
summary(cash.arma)
