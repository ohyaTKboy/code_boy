##Taipei metro 1/8/2016 10:00-11:00 exit person-time

library(httr)
library(readr)
library(dplyr)
library(ggplot2)
library(ggmap)



options(stringsAsFactors = F,fileEncoding="utf8")

###____________crawl the data________________________ 
url<-"http://data.taipei/opendata/datalist/datasetMeta/download;jsessionid=75E834F6BDB5A753CEF201D5E70F42BA?id=1d71c478-205f-42c5-8386-35f86d74fdd1&rid=95fc5ee6-c56c-47eb-8e08-b209c9ff39e5"
GET(url,write_disk("mrtexit.csv",overwrite = T))
mrt<-read.csv("mrtexit.csv")
View(mrt)

##________data transforming+__________________________
mrtt<-t(mrt)
mrtt<-as.data.frame(mrtt)
class(mrtt)
colnames(mrtt)<-mrtt[1:2,]
mrtt<-mrtt[-c(1:2),]
mrtt<-cbind(station = rownames(mrtt),mrtt)
row.names(mrtt)<-NULL
View(mrtt)

##________________create the data I need______________________
mrt080110<-data.frame(mrtt$station,mrtt[,7])
colnames(mrt080110)<-c("station","exit080110")
mrt080110$exit080110<-gsub(",","",mrt080110$exit080110)
View(mrt080110)


##______________extract the data of position of each MRT station___________
mrtp<-read.csv("mrtposition.csv")
colnames(mrtp)<-c("id","station","lat","lon")
View(mrtp)
mrtt.df<-inner_join(mrt080110,mrtp,by=c("station"))
mrtt.df$exit080110<-as.numeric(mrtt.df$exit080110)
mrtt.df$lat<-as.numeric(mrtt.df$lat)
mrtt.df$lon<-as.numeric(mrtt.df$lon)



##___________create the function to assign colors__________________
assignColor <- function(index){
  if(index <= 500){return("#86A697")}
  else if(index >= 501 && index <= 1000){return("#096148")}
  else if(index >= 1001 && index <= 1500){return("#6E552F")}
  else if(index >= 1501 && index <= 2000){return("#7D532C")}
  else if(index >= 2001 && index <= 2500){return("#CA7853")}
  else {return("#64363C")}
}

mrtt.df$color<-sapply(mrtt.df$exit080110,assignColor)


##_____________plot_____________________________________

ggmap(get_googlemap(center=c(121.5482788,25.0620062),zoom=11,maptype='roadmap')) +
  geom_point(data=mrtt.df, aes(x=lon, y=lat), colour=mrtt.df$color, size=2, alpha=0.8)


