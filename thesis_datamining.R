##___Master's thesis data mining code save ________________
##__Unorganized, do not copy without permission____________


CO2e<-read.csv("D:/data_thesis/thesis/CO2e.csv",header = T)
nitroe<-read.csv("D:/data_thesis/thesis/nitro_e.csv",header=T)
methane_e<-read.csv("D:/data_thesis/thesis/methane_e.csv",header = T)
totalge<-read.csv("D:/data_thesis/thesis/totalg_e.csv",header=T)
BIT<-read_excel("D:/data_thesis/thesis/country_m_BIT.xlsx")
BITm<-merge(BIT,CO2e,by=c("year","Country.Code"),all=T)
View(BITm)
BITm<-merge(BITm,totalge,by=c("year","Country.Code"),all=T)

qdata$GNI_pc_sqt<-qdata$GNI_pc_t^2

write.csv(qdata,"D:/data_thesis/thesis/qdata.csv")




FDI<-read_excel("D:/data_thesis/thesis/FDI_in.xlsx")
library(reshape2)

FDI<-melt(FDI,id.vars = c("Country.Code"))
colnames(FDI)<-c("Country.Code","year","FDI_inflows")
data<-merge(data,FDI,by=c("Country.Code","year"))


BITm<-BITm[order(BITm$Country.Code),]

write.csv(data,"D:/data_thesis/thesis/BIT_data_all.csv")


###_____________________merging controls_____________________________
options(stringsAsFactors = F,fileEncoding="utf8")

library(readxl)

BITm<-read.csv("D:/data_thesis/thesis/BIT_m.csv",header = T)
WDI_control<-read_excel("D:/data_thesis/thesis/controls/controls.xlsx",sheet=1)
class(BITm)
WDI_control<-as.data.frame(WDI_control)

BITm<-merge(BITm,WDI_control,by=c("year","Country.Code"),all=T)

polityIV<-read_excel("D:/data_thesis/thesis/controls/p4v2016.xlsx",sheet = 2)
class(polityIV)
polityIV<-as.data.frame(polityIV)

BITm<-merge(BITm,polityIV,by=c("year","Country.Code"),all=T)

BITm<-BITm[order(BITm$Country.Code),]

write.csv(BITm,"D:/data_thesis/thesis/BIT_all.csv")


###_________dsc___________
BIT<-read_excel("D:/data_thesis/thesis/BIT_1979_all.xlsx")
class(BIT)
BIT<-as.data.frame(BIT)
dsc<-read_excel("D:/data_thesis/thesis/DSC.xlsx",sheet = 2)
BIT<-merge(BIT,dsc,by=c("year","Country.Num"),all=T)
BIT<-BIT[order(BIT$Country.Code),]
write.csv(BIT,"D:/data_thesis/thesis/BIT_dsc_all.csv")


###_________plm___________
options(stringsAsFactors = F)

install.packages("stargazer")
library(stargazer)

library(plm)
library(readxl)


data$B_scope<-replace(data$B_scope,data$B_scope==0,NA)
data$B_props<-replace(data$B_props,data$B_props==0,NA)
data$B_nondis<-replace(data$B_nondis,data$B_nondis==0, NA)
data$B_access<-replace(data$B_access,data$B_access==0,NA)
data$B_extra<-replace(data$B_extra,data$B_extra==0,NA)
data$B_dsc<-replace(data$B_dsc,data$B_dsc==-9,NA)
write.csv(data,"D:/data_thesis/thesis/BIT_dsc_all.csv")

data<-read_excel("D:/data_thesis/thesis/data_all.xlsx")


pdata<-plm.data(data,c("Country.Num","year"))

pdata$B_scope<-replace(pdata$B_dsc,pdata$B_dsc==0,NA)

###_____narrative_____
options(stringsAsFactors = F,fileEncoding="utf8")

library(readxl)

data<-read_excel("D:/data_thesis/thesis/data_all.xlsx")

data$GNI_pc_t<-data$GNI_pc/1000
data$GNI_pc_sqt<-data$GNI_pc^2/1000
data$industry_va<-data$industry_va/1000

data$FDI_inflows<-data$FDI_inflows/1000000


data<-replace(data,data==-9,NA)



mean(data$FDI_inflows,na.rm = T)
sd(data$FDI_inflows,na.rm = T)
min(data$FDI_inflows,na.rm=T)
max(data$industry_va,na.rm = T)
median(data$FDI_inflows,na.rm = T)
length(na.omit(data$FDI_inflows))

write.csv(data,"D:/data_thesis/thesis/data_all.csv")

###______________lag variable___________________

data<-read_excel("D:/data_thesis/thesis/data_all.xlsx")
data<-replace(data,data==-9,NA)


###_______________lag variable__________________

library(dplyr)

data<-
  data%>%
  group_by(Country.Num)%>%
  mutate(FDI_inflows_1=lag(FDI_inflows,1))%>%
  mutate(FDI_inflows_3=lag(FDI_inflows,3))

write.csv(data,"D:/data_thesis/thesis/data_all_lag.csv")

###___________pearson corelation_______________________
options(stringsAsFactors = F,fileEncoding="utf8")
library(readxl)
library(stargazer)

data<-read_excel("D:/data_thesis/thesis/data_all_lag.xlsx")

data<-as.data.frame(data)


for(i in c(2:3,5:ncol(data))){
  data[,i]<-as.numeric(data[,i])
}

library(psych)
library(Hmisc)

?cor
?cor.test
a1<-cor(pdata[,c(50,12,11,18:24,48)],use="complete.obs",method = c("pearson"))
a<-pdata[,c(50,12,11,18:24,48)]
View(a)
a2<-corr.test(a)
View(a2$r)
View(a2$p)

###_________plm H1.1 co2 and examine____________________

library(plm)
library(lmtest)
library(stargazer)

?stargazer

data<-read_excel("D:/data_thesis/thesis/data_all_lag.xlsx",sheet = 1)


data<-as.data.frame(data)


for(i in c(2:3,5:ncol(data))){
  data[,i]<-as.numeric(data[,i])
}


for(i in c(5:10,12,25:38)){
  data[,i]<-scale(data[,i])
}

for(i in c(11,13:16,18,39:40)){
  data[,i]<-log(data[,i])
}





with(pdata, levels(Country.Num)[tapply(year, Country.Num,
                                 function(x) any(table(x) > 1))])

finiteElements <- which(is.finite(data$other_gas))
data1 <- data[finiteElements,]

qdata<-plm.data(data1,c("Country.Num","year"))
write.csv(qdata,"D:/data_thesis/thesis/data_no_ifinite.csv")


CO2p<-plm(CO2e~B_scope+B_nondis+B_props+
            B_access+B_extra+B_dsc+industry_va+pop_pq_m+trade_GDP+urbanization+
            polity+GNI_pc_t+GNI_pc_sqt,data=qdata,model="pool",na.action = na.exclude)
summary(CO2p)
m<-coeftest(CO2p, vcovHC)



CO2f<-plm(CO2e~B_scope+B_nondis+B_props+
               B_access+B_extra+B_dsc,data=qdata,model="within",na.action = na.exclude)
summary(CO2f)
coeftest(CO2f, vcovHC)

CO2f_1<-plm(CO2e~B_scope_1+B_nondis_1+B_props_1+
            B_access_1+B_extra_1+B_dsc_1,data=qdata,model="within",na.action = na.exclude)
summary(CO2f_1)
coeftest(CO2f_1, vcovHC)

CO2f_3<-plm(CO2e~B_scope_3+B_nondis_3+B_props_3+
              B_access_3+B_extra_3+B_dsc_3,data=qdata,model="within",na.action = na.exclude)
summary(CO2f_3)
coeftest(CO2f_3, vcovHC)



CO2f_c<-plm(CO2e~B_scope+B_nondis+B_props+
            B_access+B_extra+B_dsc+industry_va+pop_pq_m+trade_GDP+urbanization+
            polity+GNI_pc_t+GNI_pc_sqt,data=qdata,model="within",na.action = na.exclude)
summary(CO2f_c)
coeftest(CO2f_c, vcovHC)


CO2f_c_1<-plm(CO2e~B_scope_1+B_nondis_1+B_props_1+
              B_access_1+B_extra_1+B_dsc_1+industry_va+pop_pq_m+trade_GDP+urbanization+
              polity+GNI_pc_t+GNI_pc_sqt,data=qdata,model="within",na.action = na.exclude)
summary(CO2f_c_1)
coeftest(CO2f_c_1, vcovHC)


CO2f_c_3<-plm(CO2e~B_scope_3+B_nondis_3+B_props_3+
                B_access_3+B_extra_3+B_dsc_3+industry_va+pop_pq_m+trade_GDP+urbanization+
                polity+GNI_pc_t+GNI_pc_sqt,data=qdata,model="within",na.action = na.exclude)
summary(CO2f_c_3)
coeftest(CO2f_c_3, vcovHC)

stargazer(m,type = "html",dep.var.labels = c("CO2 emission"),
          covariate.labels = c("BIT Scope","BIT Nondis",
                               "BIT Props","BIT Access",
                               "BIT extra","BIT dsc"),out = "test.html")





###__________CH4 H1.1__________________________

CH4p<-plm(methane_e~B_scope+B_nondis+B_props+
            B_access+B_extra+B_dsc+industry_va+pop_pq_m+trade_GDP+urbanization+
            polity+GNI_pc_t+GNI_pc_sqt,data=qdata,model="pool",na.action = na.exclude)
summary(CO2p)
coeftest(CO2p, vcovHC)


CH4f<-plm(methane_e~B_scope+B_nondis+B_props+
            B_access+B_extra+B_dsc,data=qdata,model="within",na.action = na.exclude)
summary(CH4f)
coeftest(CH4f, vcovHC)


CH4f_1<-plm(methane_e~B_scope_1+B_nondis_1+B_props_1+
            B_access_1+B_extra_1+B_dsc_1,data=qdata,model="within",na.action = na.exclude)
summary(CH4f_1)
coeftest(CH4f_1, vcovHC)

CH4f_3<-plm(methane_e~B_scope_3+B_nondis_3+B_props_3+
              B_access_3+B_extra_3+B_dsc_3,data=qdata,model="within",na.action = na.exclude)
summary(CH4f_1)
coeftest(CH4f_1, vcovHC)




CH4f_c<-plm(methane_e~B_scope+B_nondis+B_props+
              B_access+B_extra+B_dsc+industry_va+pop_pq_m+trade_GDP+urbanization+
              polity+GNI_pc_t+GNI_pc_sqt,data=qdata,model="within",na.action = na.exclude)
summary(CH4f_c)
coeftest(CH4f_c, vcovHC)


CH4f_c_1<-plm(methane_e~B_scope_1+B_nondis_1+B_props_1+
              B_access_1+B_extra_1+B_dsc_1+industry_va+pop_pq_m+trade_GDP+urbanization+
              polity+GNI_pc_t+GNI_pc_sqt,data=qdata,model="within",na.action = na.exclude)
summary(CH4f_c_1)
coeftest(CH4f_c_1, vcovHC)


CH4f_c_3<-plm(methane_e~B_scope_3+B_nondis_3+B_props_3+
                B_access_3+B_extra_3+B_dsc_3+industry_va+pop_pq_m+trade_GDP+urbanization+
                polity+GNI_pc_t+GNI_pc_sqt,data=qdata,model="within",na.action = na.exclude)
summary(CH4f_c_3)
coeftest(CH4f_c_3, vcovHC)



###____________NO H1______________________

NOp<-plm(nitro_e~B_scope+B_nondis+B_props+
            B_access+B_extra+B_dsc+industry_va+pop_pq_m+trade_GDP+urbanization+
           polity+GNI_pc_t+GNI_pc_sqt,data=qdata,model="pool",na.action = na.exclude)
summary(NOp)
coeftest(NOp, vcovHC)


NOf<-plm(nitro_e~B_scope+B_nondis+B_props+
            B_access+B_extra+B_dsc,data=qdata,model="within",na.action = na.exclude)
summary(NOf)
coeftest(NOf, vcovHC)

NOf_1<-plm(nitro_e~B_scope_1+B_nondis_1+B_props_1+
           B_access_1+B_extra_1+B_dsc_1,data=qdata,model="within",na.action = na.exclude)
summary(NOf_1)
coeftest(NOf_1, vcovHC)

NOf_3<-plm(nitro_e~B_scope_3+B_nondis_3+B_props_3+
             B_access_3+B_extra_3+B_dsc_3,data=qdata,model="within",na.action = na.exclude)
summary(NOf_3)
coeftest(NOf_3, vcovHC)



NOf_c<-plm(nitro_e~B_scope+B_nondis+B_props+
              B_access+B_extra+B_dsc+industry_va+pop_pq_m+trade_GDP+urbanization+
              polity+GNI_pc_t+GNI_pc_sqt,data=qdata,model="within",na.action = na.exclude)
summary(NOf_c)
coeftest(NOf_c, vcovHC)

NOf_c_1<-plm(nitro_e~B_scope_1+B_nondis_1+B_props_1+
             B_access_1+B_extra_1+B_dsc_1+industry_va+pop_pq_m+trade_GDP+urbanization+
             polity+GNI_pc_t+GNI_pc_sqt,data=qdata,model="within",na.action = na.exclude)
summary(NOf_c)
coeftest(NOf_c, vcovHC)

NOf_c_3<-plm(nitro_e~B_scope_1+B_nondis_1+B_props_1+
               B_access_1+B_extra_1+B_dsc_1+industry_va+pop_pq_m+trade_GDP+urbanization+
               polity+GNI_pc_t+GNI_pc_sqt,data=qdata,model="within",na.action = na.exclude)
summary(NOf_c)
coeftest(NOf_c, vcovHC)




###_____________other H1_______________________

otherp<-plm(other_gas~B_scope+B_nondis+B_props+
            B_access+B_extra+B_dsc+industry_va+pop_pq_m+trade_GDP+urbanization+
              polity+GNI_pc_t+GNI_pc_sqt,data=qdata,model="pool",na.action = na.exclude)
summary(otherp)
coeftest(otherp, vcovHC)


otherf<-plm(other_gas~B_scope+B_nondis+B_props+
            B_access+B_extra+B_dsc,data=qdata,model="within",na.action = na.exclude)
summary(otherf)
coeftest(otherf, vcovHC)


otherf_1<-plm(other_gas~B_scope_1+B_nondis_1+B_props_1+
              B_access_1+B_extra_1+B_dsc_1,data=qdata,model="within",na.action = na.exclude)
summary(otherf_1)
coeftest(otherf_1, vcovHC)





otherf_c<-plm(other_gas~B_scope+B_nondis+B_props+
              B_access+B_extra+B_dsc+industry_va+pop_pq_m+trade_GDP+urbanization+
              polity+GNI_pc_t+GNI_pc_sqt,data=qdata,model="within",na.action = na.exclude)
summary(otherf_c)
coeftest(otherf_c, vcovHC)


otherf_c_1<-plm(other_gas~B_scope_1+B_nondis_1+B_props_1+
                B_access_1+B_extra_1+B_dsc_1+industry_va+pop_pq_m+trade_GDP+urbanization+
                polity+GNI_pc_t+GNI_pc_sqt,data=qdata,model="within",na.action = na.exclude)
summary(otherf_c_1)
coeftest(otherf_c_1, vcovHC)


otherf_c_3<-plm(other_gas~B_scope_3+B_nondis_3+B_props_3+
                  B_access_3+B_extra_3+B_dsc_3+industry_va+pop_pq_m+trade_GDP+urbanization+
                  polity+GNI_pc_t+GNI_pc_sqt,data=qdata,model="within",na.action = na.exclude)
summary(otherf_c_3)
coeftest(otherf_c_3, vcovHC)


####___________h1 MODERATION_______________________________


CO2mp<-plm(CO2e~B_scope+B_nondis+B_props+B_access+B_extra+
            B_dsc+env_in+B_scope*env_in+B_nondis*env_in+B_props*env_in+
            B_access*env_in+B_extra*env_in+B_dsc*env_in+industry_va+pop_pq_m+trade_GDP+urbanization+
            polity+GNI_pc_t+GNI_pc_sqt,data=qdata,model="pool",na.action = na.exclude)
summary(CO2p)
coeftest(CO2p, vcovHC)



CO2mf<-plm(CO2e~B_scope+B_nondis+B_props+
            B_access+B_extra+B_dsc+env_in+B_scope*env_in+B_nondis*env_in+B_props*env_in+
            B_access*env_in+B_extra*env_in+B_dsc*env_in,data=qdata,model="within",na.action = na.exclude)
summary(CO2mf)
coeftest(CO2mf, vcovHC)


CO2mf_c<-plm(CO2e~B_scope+B_nondis+B_props+
              B_access+B_extra+B_dsc+B_scope*env_in+B_nondis*env_in+B_props*env_in+
               B_access*env_in+B_extra*env_in+B_dsc*env_in+industry_va+pop_pq_m+trade_GDP+urbanization+
              polity+GNI_pc_t+GNI_pc_sqt,data=qdata,model="within",na.action = na.exclude)
summary(CO2mf_c)
coeftest(CO2mf_c, vcovHC)


CO2mf_c_3<-plm(CO2e~B_scope_3+B_nondis_3+B_props_3+
                B_access_3+B_extra_3+B_dsc_3+B_scope_3*env_in+B_nondis_3*env_in+B_props_3*env_in+
                 B_access_3*env_in+B_extra_3*env_in+B_dsc_3*env_in+industry_va+pop_pq_m+trade_GDP+urbanization+
                polity+GNI_pc_t+GNI_pc_sqt,data=qdata,model="within",na.action = na.exclude)
summary(CO2mf_c_3)
coeftest(CO2mf_c_3, vcovHC)


###____________CH4 moderation H1____________________

CH4mp<-plm(methane_e~B_scope+B_nondis+B_props+
            B_access+B_extra+B_dsc+B_scope*env_in+B_nondis*env_in+B_props*env_in+
            B_access*env_in+B_extra*env_in+B_dsc*env_in+industry_va+pop_pq_m+trade_GDP+urbanization+
            polity+GNI_pc_t+GNI_pc_sqt,data=qdata,model="pool",na.action = na.exclude)
summary(CH4mp)
coeftest(CH4mp, vcovHC)


CH4mf<-plm(methane_e~B_scope*env_in+B_nondis*env_in+B_props*env_in+
            B_access*env_in+B_extra*env_in+B_dsc*env_in,data=qdata,model="within",na.action = na.exclude)
summary(CH4mf)
coeftest(CH4mf, vcovHC)


CH4f_1<-plm(methane_e~B_scope_1+B_nondis_1+B_props_1+
              B_access_1+B_extra_1+B_dsc_1,data=qdata,model="within",na.action = na.exclude)
summary(CH4f_1)
coeftest(CH4f_1, vcovHC)

CH4f_3<-plm(methane_e~B_scope_3+B_nondis_3+B_props_3+
              B_access_3+B_extra_3+B_dsc_3,data=qdata,model="within",na.action = na.exclude)
summary(CH4f_1)
coeftest(CH4f_1, vcovHC)




CH4mf_c<-plm(methane_e~B_scope+B_nondis+B_props+
              B_access+B_extra+B_dsc+B_scope*env_in+B_nondis*env_in+B_props*env_in+
               B_access*env_in+B_extra*env_in+B_dsc*env_in+industry_va+pop_pq_m+trade_GDP+urbanization+
              polity+GNI_pc_t+GNI_pc_sqt,data=qdata,model="within",na.action = na.exclude)
summary(CH4mf_c)
coeftest(CH4mf_c, vcovHC)


CH4f_c_1<-plm(methane_e~B_scope_1+B_nondis_1+B_props_1+
                B_access_1+B_extra_1+B_dsc_1+industry_va+pop_pq_m+trade_GDP+urbanization+
                polity+GNI_pc_t+GNI_pc_sqt,data=qdata,model="within",na.action = na.exclude)
summary(CH4f_c_1)
coeftest(CH4f_c_1, vcovHC)


CH4f_c_3<-plm(methane_e~B_scope_3+B_nondis_3+B_props_3+
                B_access_3+B_extra_3+B_dsc_3+industry_va+pop_pq_m+trade_GDP+urbanization+
                polity+GNI_pc_t+GNI_pc_sqt,data=qdata,model="within",na.action = na.exclude)
summary(CH4f_c_3)
coeftest(CH4f_c_3, vcovHC)


##____________________melt____________________

B_dsc_all<-read_excel("D:/data_thesis/thesis/BIT_dsc_all.xlsx",sheet=3)
yes<-merge(qdata,B_dsc_all,by=c("Country.Num","year"),all=T)
green<-read_excel("D:/data_thesis/thesis/totalgreenhouse.xlsx")
library(reshape2)
green<-melt(green,id.vars=("Country Code"))
colnames(green)<-c("Country.Code","year","total")
yes1<-merge(yes,green,by=c("Country.Code","year"),all=T)
write.csv(yes1,"D:/data_thesis/thesis/qdata1.csv")
