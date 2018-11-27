options(stringsAsFactors = F,fileEncoding='utf8')

#library the required
library(httr)
library(readr)
library(readxl)
library(plyr)
library(dplyr)
library(ggplot2)
library(scales)
library(ggmap)
library(shiny)


#get csv file from opendata: example

# Specify an url to get
url <- 'http://opendata.epa.gov.tw/ws/Data/ATM00605/?%24skip=0&%24top=1000&format=csv'
# Get url and wirte to disk
GET(url, write_disk("D:/R1052/air/0607pingtung.csv", overwrite=TRUE))


#load files

Nanzi<-read.csv('D:/R1052/air/0607nanzi.csv')
Daliao<-read.csv('D:/R1052/air/0607Daliao.csv')
Fengshan<-read.csv('D:/R1052/air/0607fengshan.csv')
chaochou<-read.csv('D:/R1052/air/0607chaochou.csv')
frontgold<-read.csv('D:/R1052/air/0607frontgold.csv')
fronttown<-read.csv('D:/R1052/air/0607fronttown.csv')
fuxing<-read.csv('D:/R1052/air/0607fuxing.csv')
linyuan<-read.csv('D:/R1052/air/0607linyuan.csv')
meinon<-read.csv('D:/R1052/air/0607meinon.csv')
qiaoto<-read.csv('D:/R1052/air/0607qiauto.csv')
renwu<-read.csv('D:/R1052/air/0607renwu.csv')
smallport<-read.csv('D:/R1052/air/0607smallport.csv')
tsoing<-read.csv('D:/R1052/air/0607tsoing.csv')
pingtung<-read.csv('D:/R1052/air/0607pingtung.csv')

df<-rbind(Nanzi,Daliao,Fengshan,chaochou,frontgold,fuxing,linyuan,meinon,qiaoto,renwu,smallport,tsoing,pingtung,fronttown)

#transform time
df$MonitorDate<-gsub('上午','AM',df$MonitorDate)
df$MonitorDate<-gsub('下午','PM',df$MonitorDate)
Sys.setlocale("LC_TIME", "English")
Sys.setlocale("LC_TIME", "C")
df$MonitorDate <- strptime(df$MonitorDate, "%Y/%m/%d %p %I:%M:%S")
df$MonitorDate<-as.POSIXct(df$MonitorDate)

#NAs
df$Concentration<-gsub('x',NA,df$Concentration)
df$Concentration<-as.numeric(df$Concentration)



df<-filter(df,!is.na(df$Concentration))

#separate latitude and longitude to specify location
list<-strsplit(df$location,',')
location<-ldply(list)
colnames(location)<-c('latitude','longitude')
df<-data.frame(df,location)
df<- df[-10]



#grouping by pollution type
dfSO2<-filter(df,df$ItemId==1)
dfCO<-filter(df,df$ItemId==2)
dfO3<-filter(df,df$ItemId==3)
dfPM10<-filter(df,df$ItemId==4)
dfNO2<-filter(df,df$ItemId==7)
dfPM2.5<-filter(df,df$ItemId==33)


View(dfSO2)

#各觀測站汙染物平均觀測值

so2a<-tapply(dfSO2$Concentration,dfSO2$SiteName,mean,na.rm=T)%>%
  scale() %>%
  as.data.frame()
colnames(so2a)<-c('so2_ave')

COa<-tapply(dfCO$Concentration,dfCO$SiteName,mean,na.rm=T)%>%
  scale() %>%
  as.data.frame()
colnames(COa)<-c('CO_ave')

O3a<-tapply(dfO3$Concentration,dfO3$SiteName,mean,na.rm=T)%>%
  scale() %>%
  as.data.frame()
colnames(O3a)<-c('O3_ave')

PM10a<-tapply(dfPM10$Concentration,dfPM10$SiteName,mean, na.rm=T)%>%
  scale() %>%
  as.data.frame()
colnames(PM10a)<-c('PM10_ave')

NO2a<-tapply(dfNO2$Concentration,dfNO2$SiteName,mean,na.rm=T)%>%
  scale() %>%
  as.data.frame()
colnames(NO2a)<-c('NO2_ave')

PM2.5a<-tapply(dfPM2.5$Concentration,dfPM2.5$SiteName,mean,na.rm=T)%>%
  scale() %>%
  as.data.frame()
colnames(PM2.5a)<-c('PM2.5_ave')

padf<-data.frame(so2a,COa,O3a,PM10a,NO2a,PM2.5a)
View(padf)

lat<-c(22.5658827,22.5662551,22.6884507,22.6741872,22.4773354,22.6320382,22.6051828,22.6730818,22.883998,22.608456,22.7334902,22.6276584,22.5228948,22.7576591)
lon<-c(120.4214513,120.3362196,120.3319856,120.2906849,120.4084894,120.2849198,120.306135,120.4861309,120.5281481,120.3109237,120.3256939,120.3552544,120.5580587,120.3035613)

padf$lat<-cbind(lat)
padf$lon<-cbind(lon)



#分別觀察各項污染源在該地區的影響大小
indloc<-read_excel('D:/R1052/air/kindustrylocation.xlsx',sheet = 1)

#畫圖

#SO2
ggmap(get_googlemap(center=c(120.3226946,22.6791855),zoom=10,maptype='roadmap')) +
  geom_point(data=padf, aes(x=lon, y=lat,color=padf$so2_ave), size= padf$so2_ave*10,alpha=0.4)+
  geom_point(data=indloc,aes(x=Long,y=Lat),size=2,color="navy")+
  ggtitle('高雄市 SO2 汙染狀況')


#CO
ggmap(get_googlemap(center=c(120.3226946,22.6791855),zoom=10,maptype='roadmap')) +
  geom_point(data=padf, aes(x=lon, y=lat,color=padf$CO_ave),size=padf$CO_ave*10,alpha=0.4)+
  geom_point(data=indloc,aes(x=Long,y=Lat),size=2,color="navy")+
  ggtitle('高雄市 CO 汙染狀況')

#O3
ggmap(get_googlemap(center=c(120.3226946,22.6791855),zoom=10,maptype='roadmap')) +
  geom_point(data=padf, aes(x=lon, y=lat,color=padf$O3_ave),size=padf$O3_ave*10,alpha=0.4)+
  geom_point(data=indloc,aes(x=Long,y=Lat),size=2,color="navy")+
  ggtitle('高雄市 O3 汙染狀況')


#PM10
ggmap(get_googlemap(center=c(120.3226946,22.6791855),zoom=10,maptype='roadmap')) +
  geom_point(data=padf, aes(x=lon, y=lat,color= padf$PM10_ave),size=padf$PM10_ave*10,alpha=0.4)+
  geom_point(data=indloc,aes(x=Long,y=Lat),size=2,color="navy")+
  ggtitle('高雄市 PM10 汙染狀況')


#NO2
ggmap(get_googlemap(center=c(120.3226946,22.6791855),zoom=10,maptype='roadmap')) +
  geom_point(data=padf, aes(x=lon, y=lat,color= padf$NO2_ave),size=padf$NO2_ave*10,alpha=0.4)+
  geom_point(data=indloc,aes(x=Long,y=Lat),size=2,color="navy")+
  ggtitle('高雄市 NO2 汙染狀況')


##PM2.5
ggmap(get_googlemap(center=c(120.3226946,22.6791855),zoom=10,maptype='roadmap')) +
  geom_point(data=padf, aes(x=lon, y=lat, color=padf$PM2.5_ave),size=padf$PM2.5_ave*10,alpha=0.4)+
  geom_point(data=indloc,aes(x=Long,y=Lat),size=2,color="navy")+
  ggtitle('高雄市 PM2.5 汙染狀況')





#轉換時間

#繪圖檢視一週內各時段各測站對於各種汙染物的觀測


ggplot(dfSO2,aes(MonitorDate,Concentration,color=SiteName))+geom_jitter(size= 2,alpha=0.5)+scale_x_datetime(breaks = date_breaks('12 hour'),labels = date_format('%d-%H'))
ggplot(dfCO,aes(MonitorDate,Concentration,color=SiteName))+geom_jitter(size=2,alpha=0.5)+scale_x_datetime(breaks = date_breaks('12 hour'),labels = date_format('%d-%H'))
ggplot(dfO3,aes(MonitorDate,Concentration,color=SiteName))+geom_jitter(size=2,alpha=0.5)+scale_x_datetime(breaks = date_breaks('12 hour'),labels = date_format('%d-%H'))
ggplot(dfPM10,aes(MonitorDate,Concentration,color=SiteName))+geom_jitter(size=2,alpha=0.5)+scale_x_datetime(breaks = date_breaks('12 hour'),labels = date_format('%d-%H'))
ggplot(dfNO2,aes(MonitorDate,Concentration,color=SiteName))+geom_jitter(size=2,alpha=0.5)+scale_x_datetime(breaks = date_breaks('12 hour'),labels = date_format('%d-%H'))
ggplot(dfPM2.5,aes(MonitorDate,Concentration,color=SiteName))+geom_jitter(size=2,alpha=0.5)+scale_x_datetime(breaks = date_breaks('12 hour'),labels = date_format('%d-%H'))


#各測站每小時資料比較



dfSO2d<-tapply(dfSO2$Concentration,list(dfSO2$SiteName,format(dfSO2$MonitorDate,"%d")),mean,na.rm=T) %>%
  scale()%>%
  as.data.frame()%>%
  cbind(lat)%>%
  cbind(lon)

dfCOd<-tapply(dfCO$Concentration,list(dfCO$SiteName,format(dfCO$MonitorDate,"%d")),mean,na.rm=T) %>%
  scale()%>%
  as.data.frame()%>%
  cbind(lat)%>%
  cbind(lon)


dfO3d<-tapply(dfO3$Concentration,list(dfO3$SiteName,format(dfO3$MonitorDate,"%d")),mean,na.rm=T) %>%
  scale()%>%
  as.data.frame()%>%
  cbind(lat)%>%
  cbind(lon)

dfPM10d<-tapply(dfPM10$Concentration,list(dfPM10$SiteName,format(dfPM10$MonitorDate,"%d")),mean,na.rm=T) %>%
  scale()%>%
  as.data.frame()%>%
  cbind(lat)%>%
  cbind(lon)

dfNO2d<-tapply(dfNO2$Concentration,list(dfNO2$SiteName,format(dfNO2$MonitorDate,"%d")),mean,na.rm=T) %>%
  scale()%>%
  as.data.frame()%>%
  cbind(lat)%>%
  cbind(lon)

dfPM2.5d<-tapply(dfPM2.5$Concentration,list(dfPM2.5$SiteName,format(dfPM2.5$MonitorDate,"%d")),mean,na.rm=T) %>%
  scale()%>%
  as.data.frame()%>%
  cbind(lat)%>%
  cbind(lon)

View(dfPM2.5d)

#mapping

#汙染密度
ggmap(get_googlemap(center=c(120.3226946,22.6791855),zoom=11,maptype='roadmap')) +
  stat_density2d(data=padf, aes(x=lon, y=lat),size=2,alpha=0.2, geom = 'polygon')+
  ggtitle('空氣汙染密度')


#6/1~6/2 SO2
for(i in 1:7){
print(ggmap(get_googlemap(center=c(120.3226946,22.6791855),zoom=10,maptype='roadmap')) +
  geom_point(data=dfSO2d, aes(x=lon, y=lat,color=dfSO2d[,i]), size= dfSO2d[,i]*10,alpha=0.4)+
  scale_color_gradient(name= 'SO2 Concentration')+
  geom_point(data=indloc,aes(x=Long,y=Lat),size=2,color="navy")+
  ggtitle('高雄市 SO2 汙染分布',paste0('6/',i)))
}

#6/1~6/2 CO

for(i in 1:7){
  print(ggmap(get_googlemap(center=c(120.3226946,22.6791855),zoom=10,maptype='roadmap')) +
          geom_point(data=dfCOd, aes(x=lon, y=lat,color=dfCOd[,i]), size= dfCOd[,i]*10,alpha=0.4)+
          scale_color_gradient(name= 'CO Concentration')+
          geom_point(data=indloc,aes(x=Long,y=Lat),size=2,color="navy")+
          ggtitle('高雄市 CO 汙染分布',paste0('6/',i)))
}

#6/1~6/2 O3

for(i in 1:7){
  print(ggmap(get_googlemap(center=c(120.3226946,22.6791855),zoom=10,maptype='roadmap')) +
          geom_point(data=dfO3d, aes(x=lon, y=lat,color=dfO3d[,i]), size= dfO3d[,i]*10,alpha=0.4)+
          scale_color_gradient(name= 'O3 Concentration')+
          geom_point(data=indloc,aes(x=Long,y=Lat),size=2,color="navy")+
          ggtitle('高雄市 O3 汙染分布',paste0('6/',i)))
}


#6/1~6/7 PM10

for(i in 1:7){
  print(ggmap(get_googlemap(center=c(120.3226946,22.6791855),zoom=10,maptype='roadmap')) +
          geom_point(data=dfPM10d, aes(x=lon, y=lat,color=dfPM10d[,i]), size= dfPM10d[,i]*10,alpha=0.4)+
          scale_color_gradient(name= 'O3 Concentration')+
          geom_point(data=indloc,aes(x=Long,y=Lat),size=2,color="navy")+
          ggtitle('高雄市 PM10 汙染分布',paste0('6/',i)))
}

#6/1~6/7 NO2

for(i in 1:7){
  print(ggmap(get_googlemap(center=c(120.3226946,22.6791855),zoom=10,maptype='roadmap')) +
          geom_point(data=dfNO2d, aes(x=lon, y=lat,color=dfO3d[,i]), size= dfNO2d[,i]*10,alpha=0.4)+
          scale_color_gradient(name= 'NO2 Concentration')+
          geom_point(data=indloc,aes(x=Long,y=Lat),size=2,color="navy")+
          ggtitle('高雄市 NO2 汙染分布',paste0('6/',i)))
}

#6/1~6/7 PM2.5

for(i in 1:7){
  print(ggmap(get_googlemap(center=c(120.3226946,22.6791855),zoom=10,maptype='roadmap')) +
          geom_point(data=dfPM2.5d, aes(x=lon, y=lat,color=dfPM2.5d[,i]), size= dfPM2.5d[,i]*10,alpha=0.4)+
          scale_color_gradient(name= 'PM2.5 Concentration')+
          geom_point(data=indloc,aes(x=Long,y=Lat),size=2,color="navy")+
          ggtitle('高雄市 PM2.5 汙染分布',paste0('6/',i)))
}
