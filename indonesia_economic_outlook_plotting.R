
##indonesia economic outlook plotting


options(stringsAsFactors = T,fileEncoding="utf8")

library(readxl)
library(ggplot2)

current<-read_excel("D:/研究所/台經院/印尼盾重貶/current_indonesia.xlsx",sheet=1)
View(current)
export<-read_excel("D:/研究所/台經院/印尼盾重貶/export_indonesia.xlsx",sheet=2)
View(export)
import_r<-read_excel("D:/研究所/台經院/印尼盾重貶/import_GDP.xlsx",sheet=1)
View(import_r)
exchange_rate<-read_excel("D:/研究所/台經院/印尼盾重貶/exchange_rate.xlsx",sheet=1)
View(exchange_rate)

export_month<-read_excel("D:/研究所/台經院/印尼盾重貶/export_indonesia.xlsx",sheet=1)

View(export_month)
export_month$indonesia<-as.numeric(export_month$indonesia)
export_month$time<-as.numeric(export_month$time)
class(export$Indonesia)
class(export_month$time)

ggplot(current,aes(x=year))+
  geom_line(aes(y=current$Indonesia,color="CAB"),size=1)+
  geom_line(aes(y=exchange_rate$Indonesia*,color="exchange rate"),size=1)+
  scale_y_continuous(sec.axis = sec_axis(~.,name = "ratio %"))+
  theme_bw()+
  labs(title="CAB of Indonesia and Import from Taiwan")

ggplot(export_month,aes(x=time,y=indonesia))+geom_line(color="#0089A7",size=0.5)+
  geom_smooth(color="#E16B8C")+
  scale_x_discrete(limits=c(200101,200501,201001,201501))+
  labs(y="USD $",
       x="month",
       title="Taiwan export to Indonesia")+
  theme_bw()


ggplot(import_r,aes(x=Year,y=Indonesia))+geom_line(color="#0089A7",size=1)+
  labs(y="GDP %",
       title="Imports of goods and services (% of GDP)")+
  theme_bw()
  


acf(export_month$indonesia,lag.max = 12)


cor(current$Indonesia,export$Indonesia)
