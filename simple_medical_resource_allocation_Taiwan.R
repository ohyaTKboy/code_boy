options(stringsAsFactors = F, fileEcoding="utf8")
library(ggplot2)
library(XML)

url<-"http://data.gov.tw/iisi/logaccess/60295?dataUrl=http://data.nhi.gov.tw/Datasets/DatasetResource.ashx?rId=A21030000I-E30008-003&ndctype=XML&ndcnid=18585"
x <- xmlParse(url)
xmlfiles <- xmlRoot(x) 
y <- xmlToDataFrame(xmlfiles) # 轉換成 dataframe


colnames(y) <- c("指標名稱","年度季別","分區業務組","縣市別","醫事機構代碼","醫事機構名稱","院所指標值","所屬分區值","全國指標值")


#找出各區回報醫事機構數量
yres<-tapply(y$醫事機構代碼,y$分區業務組,length)
yres<-as.data.frame(yres)
yres$names<-rownames(yres)
rownames(yres)<-NULL
colnames(yres)<-c("醫事機構數量","各區別")
View(yres)

#plot
pyres<-ggplot(yres,aes(yres$各區別,yres$醫事機構數量))+geom_bar(stat = "identity",fill="#336774",alpha=0.5)
pyres
#可發現東區醫事機構明顯少於各區。
