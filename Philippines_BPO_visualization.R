### Philippines BPO visualization


options(stringAsfactor=F,fileEncoding="utf8")
library(readxl)
library(ggplot2)

FDI<-read_excel("D:/研究所/台經院/菲律賓BPO/FDIinflow2017.xlsx",sheet=1)
FDI2000<-FDI[30:nrow(FDI),]

FDI_p<-ggplot(FDI2000,aes(x=Year))+geom_line(aes(y=ICT_export,color="ICT exports"),size=1)+
  geom_line(aes(y=FDI_inflow,color="FDI inflows"),size=1)+labs(title="ICT service exports and FDI inflows of the Philippines",
                                                              y="USD",
                                                              color="")+
  theme_bw()
FDI_p

ICT_value<-ggplot(FDI2000,aes(x=Year))+geom_line(aes(y=service_value_added,color="service industry value added"),size=1)+
  geom_line(aes(y=ratio_ICTinService*2000000000,color="ICT ratio in service industry %"),size=1)+
  scale_y_continuous(sec.axis = sec_axis(~./2000000000,name = "ratio %"))+
  labs(title="The status of ICT service industry in the Philippines",y="USD",color="")+
  theme_bw()
ICT_value

ICT_unem<-ggplot(FDI2000,aes(x=Year))+
  geom_line(aes(y=ratio_ICTinService,color="ICT ratio in service %"),size=1)+
  geom_line(aes(y=,color="unemployment rate %"),size=1)+
  labs(title="ICT service & unemployment in Philippines",y="ratio",color="")+
  theme_bw()
ICT_unem

regu<-ggplot(FDI2000,aes(x=Year))+geom_line(aes(y=regulatory_quality,color="regulatory quality"),size=1)+
  labs(title="Regulatory quality of Philippines",y="estimate",color="")+theme_bw()
regu

remi<-ggplot(FDI2000,aes(x=Year))+geom_line(aes(y=regulatory_quality,color="regulatory quality"),size=1)+
  labs(title="Regulatory quality of Philippines",y="estimate",color="")+theme_bw()
