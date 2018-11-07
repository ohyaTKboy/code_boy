##________Master's thesis plm testing code______
##_________Unorganized, do not copy without permission______



####________________CO2 h1.2_____________________
library(plm)
library(stargazer)
library(lmtest)


CO2mp<-plm(CO2e~B_scope+B_nondis+B_props+
             B_access+B_extra+B_dsc+env_in+B_scope*env_in+B_nondis*env_in+B_props*env_in+
             B_access*env_in+B_extra*env_in+B_dsc*env_in+industry_va+pop_pq_m+trade_GDP+urbanization+
             polity+GNI_pc_t+GNI_pc_sqt+GNI_pc_trt,data=qdata,model="pool",na.action = na.exclude)
summary(CO2mp)
coeftest(CO2mp, vcovHC)



CO2mf<-plm(CO2e~B_scope+B_nondis+B_props+
             B_access+B_extra+B_dsc+env_in+B_scope*env_in+B_nondis*env_in+B_props*env_in+
             B_access*env_in+B_extra*env_in+B_dsc*env_in,data=qdata,model="within",na.action = na.exclude)
summary(CO2mf)
coeftest(CO2mf, vcovHC)




CO2mf_c<-plm(CO2e~B_scope+B_nondis+B_props+
               B_access+B_extra+B_dsc+B_scope*env_in+B_nondis*env_in+B_props*env_in+
               B_access*env_in+B_extra*env_in+B_dsc*env_in+industry_va+pop_pq_m+trade_GDP+urbanization+
               polity+GNI_pc_t+GNI_pc_sqt+GNI_pc_trt,data=qdata,model="within",na.action = na.exclude)
summary(CO2mf_c)
coeftest(CO2mf_c, vcovHC)


CO2mf_c_3<-plm(CO2e~B_scope_3+B_nondis_3+B_props_3+
                 B_access_3+B_extra_3+B_dsc_3+env_in_3+B_scope_3*env_in_3+B_nondis_3*env_in_3+B_props_3*env_in_3+
                 B_access_3*env_in_3+B_extra_3*env_in_3+B_dsc_3*env_in_3+industry_va+pop_pq_m+trade_GDP+urbanization+
                 polity+GNI_pc_t+GNI_pc_sqt+GNI_pc_trt,data=qdata,model="within",na.action = na.exclude)
summary(CO2mf_c_3)
coeftest(CO2mf_c_3, vcovHC)


rob.CO2mp<- coeftest(CO2mp, function(x) vcovHC(x, type="HC0"))
rob.CO2mf<- coeftest(CO2mf, function(x) vcovHC(x, type="HC0"))
rob.CO2mf_c<-coeftest(CO2mf_c,function(x) vcovHC(x,type="HC0"))
rob.CO2mf_c3<-coeftest(CO2mf_c_3,function(x) vcovHC(x,type="HC0"))
summ.CO2mp <- summary(CO2mp, vcov. = function(x) vcovHC(x, type="HC0"), diagnostics=T)
summ.CO2mf <- summary(CO2mf, vcov. = function(x) vcovHC(x, type="HC0"), diagnostics=T)
summ.CO2mf_c <- summary(CO2mf_c, vcov. = function(x) vcovHC(x, type="HC0"), diagnostics=T)
summ.CO2mf_c3 <- summary(CO2mf_c_3, vcov. = function(x) vcovHC(x, type="HC0"), diagnostics=T)



stargazer(CO2mf,CO2mf_c,CO2mp,CO2mf_c_3, type = "html", 
          se = list(rob.CO2mf[,"Std. Error"], rob.CO2mp[,"Std. Error"],
                    rob.CO2mf_c[,"Std. Error"],rob.CO2mf_c3[,"Std. Error"]),
          dep.var.labels = c("二氧化碳排放量"),
          column.labels = c("FE","FE","OLS","FE"),
          covariate.labels= c("BIT 適用範圍","BIT 非歧視原則","BIT 財產安全",
                              "BIT 市場准入","BIT 額外條款","BIT 爭端解決","環境公約拘束",
                              "BIT 適用範圍lag3","BIT 非歧視原則lag3","BIT 財產安全lag3",
                              "BIT 市場准入lag3","BIT 額外條款lag3","BIT 爭端解決lag3","環境公約拘束lag3",                              
                              "工業附加價值","人口密度","貿易佔GDP比重","都市化程度","政治發展(polity IV)",
                              "人均所得","人均所得平方","人均所得立方","BIT 適用範圍*環境公約拘束",
                              "BIT非歧視原則*環境公約拘束","BIT 財產安全*環境公約拘束",
                              "BIT 市場准入*環境公約拘束","BIT 額外條款*環境公約拘束","BIT 爭端解決*環境公約拘束",
                              "BIT 適用範圍lag3*環境公約拘束lag3","BIT 非歧視原則lag3*環境公約拘束lag3","BIT 財產安全lag3*環境公約拘束lag3",
                              "BIT 市場准入lag3*環境公約拘束lag3","BIT 額外條款lag3*環境公約拘束lag3","BIT 爭端解決lag3*環境公約拘束lag3","常數"),
          single.row = T,
          out="thesis_frame/CO2h1.2.html")


###_____________________CH4 H1.2_________________________________________

CH4mp<-plm(methane_e~B_scope+B_nondis+B_props+
             B_access+B_extra+B_dsc+env_in+B_scope*env_in+B_nondis*env_in+B_props*env_in+
             B_access*env_in+B_extra*env_in+B_dsc*env_in+industry_va+pop_pq_m+trade_GDP+urbanization+
             polity+GNI_pc_t+GNI_pc_sqt+GNI_pc_trt,data=qdata,model="pool",na.action = na.exclude)
summary(CH4mp)
coeftest(CH4mp, vcovHC)



CH4mf<-plm(methane_e~B_scope+B_nondis+B_props+
             B_access+B_extra+B_dsc+env_in+B_scope*env_in+B_nondis*env_in+B_props*env_in+
             B_access*env_in+B_extra*env_in+B_dsc*env_in,data=qdata,model="within",na.action = na.exclude)
summary(CH4mf)
coeftest(CH4mf, vcovHC)


CH4mf_c<-plm(methane_e~B_scope+B_nondis+B_props+
               B_access+B_extra+B_dsc+B_scope*env_in+B_nondis*env_in+B_props*env_in+
               B_access*env_in+B_extra*env_in+B_dsc*env_in+industry_va+pop_pq_m+trade_GDP+urbanization+
               polity+GNI_pc_t+GNI_pc_sqt+GNI_pc_trt,data=qdata,model="within",na.action = na.exclude)
summary(CO2mf_c)
coeftest(CO2mf_c, vcovHC)


CH4mf_c_3<-plm(methane_e~B_scope_3+B_nondis_3+B_props_3+
                 B_access_3+B_extra_3+B_dsc_3+env_in_3+B_scope_3*env_in_3+B_nondis_3*env_in_3+B_props_3*env_in_3+
                 B_access_3*env_in_3+B_extra_3*env_in_3+B_dsc_3*env_in_3+industry_va+pop_pq_m+trade_GDP+urbanization+
                 polity+GNI_pc_t+GNI_pc_sqt+GNI_pc_trt,data=qdata,model="within",na.action = na.exclude)
summary(CH4mf_c_3)
coeftest(CH4mf_c_3, vcovHC)



rob.CH4mp<- coeftest(CH4mp, function(x) vcovHC(x, type="HC0"))
rob.CH4mf<- coeftest(CH4mf, function(x) vcovHC(x, type="HC0"))
rob.CH4mf_c<-coeftest(CH4mf_c,function(x) vcovHC(x,type="HC0"))
rob.CH4mf_c3<-coeftest(CH4mf_c_3,function(x) vcovHC(x,type="HC0"))
summ.CH4mp <- summary(CH4mp, vcov. = function(x) vcovHC(x, type="HC0"), diagnostics=T)
summ.CH4mf <- summary(CH4mf, vcov. = function(x) vcovHC(x, type="HC0"), diagnostics=T)
summ.CH4mf_c <- summary(CH4mf_c, vcov. = function(x) vcovHC(x, type="HC0"), diagnostics=T)
summ.CH4mf_c3 <- summary(CH4mf_c_3, vcov. = function(x) vcovHC(x, type="HC0"), diagnostics=T)


stargazer(CH4mf,CH4mf_c,CH4mp,CH4mf_c_3, type = "html", 
          se = list(rob.CH4mf[,"Std. Error"], rob.CH4mp[,"Std. Error"],
                    rob.CH4mf_c[,"Std. Error"],rob.CH4mf_c3[,"Std. Error"]),
          dep.var.labels = c("甲烷排放量"),
          column.labels = c("FE","FE","OLS","FE"),
          covariate.labels= c("BIT 適用範圍","BIT 非歧視原則","BIT 財產安全",
                              "BIT 市場准入","BIT 額外條款","BIT 爭端解決","環境公約拘束",
                              "BIT 適用範圍lag3","BIT 非歧視原則lag3","BIT 財產安全lag3",
                              "BIT 市場准入lag3","BIT 額外條款lag3","BIT 爭端解決lag3","環境公約拘束lag3",                              
                              "工業附加價值","人口密度","貿易佔GDP比重","都市化程度","政治發展(polity IV)",
                              "人均所得","人均所得平方","人均所得立方","BIT 適用範圍*環境公約拘束",
                              "BIT非歧視原則*環境公約拘束","BIT 財產安全*環境公約拘束",
                              "BIT 市場准入*環境公約拘束","BIT 額外條款*環境公約拘束","BIT 爭端解決*環境公約拘束",
                              "BIT 適用範圍lag3*環境公約拘束lag3","BIT 非歧視原則lag3*環境公約拘束lag3","BIT 財產安全lag3*環境公約拘束lag3",
                              "BIT 市場准入lag3*環境公約拘束lag3","BIT 額外條款lag3*環境公約拘束lag3","BIT 爭端解決lag3*環境公約拘束lag3","常數"),
          single.row = T,
          out="thesis_frame/CH4h1.2.html")



###_______________________NO H1.2______________________________


NOmp<-plm(nitro_e~B_scope+B_nondis+B_props+
             B_access+B_extra+B_dsc+env_in+B_scope*env_in+B_nondis*env_in+B_props*env_in+
             B_access*env_in+B_extra*env_in+B_dsc*env_in+industry_va+pop_pq_m+trade_GDP+urbanization+
             polity+GNI_pc_t+GNI_pc_sqt+GNI_pc_trt,data=qdata,model="pool",na.action = na.exclude)
summary(NOmp)
coeftest(NOmp, vcovHC)



NOmf<-plm(nitro_e~B_scope+B_nondis+B_props+
             B_access+B_extra+B_dsc+env_in+B_scope*env_in+B_nondis*env_in+B_props*env_in+
             B_access*env_in+B_extra*env_in+B_dsc*env_in,data=qdata,model="within",na.action = na.exclude)
summary(NOmf)
coeftest(NOmf, vcovHC)


NOmf_c<-plm(nitro_e~B_scope+B_nondis+B_props+
               B_access+B_extra+B_dsc+B_scope*env_in+B_nondis*env_in+B_props*env_in+
               B_access*env_in+B_extra*env_in+B_dsc*env_in+industry_va+pop_pq_m+trade_GDP+urbanization+
               polity+GNI_pc_t+GNI_pc_sqt+GNI_pc_trt,data=qdata,model="within",na.action = na.exclude)
summary(NOmf_c)
coeftest(NOmf_c, vcovHC)


NOmf_c_3<-plm(nitro_e~B_scope_3+B_nondis_3+B_props_3+
                 B_access_3+B_extra_3+B_dsc_3+env_in_3+B_scope_3*env_in_3+B_nondis_3*env_in_3+B_props_3*env_in_3+
                 B_access_3*env_in_3+B_extra_3*env_in_3+B_dsc_3*env_in_3+industry_va+pop_pq_m+trade_GDP+urbanization+
                 polity+GNI_pc_t+GNI_pc_sqt+GNI_pc_trt,data=qdata,model="within",na.action = na.exclude)
summary(NOmf_c_3)
coeftest(NOmf_c_3, vcovHC)



rob.NOmp<- coeftest(NOmp, function(x) vcovHC(x, type="HC0"))
rob.NOmf<- coeftest(NOmf, function(x) vcovHC(x, type="HC0"))
rob.NOmf_c<-coeftest(NOmf_c,function(x) vcovHC(x,type="HC0"))
rob.NOmf_c3<-coeftest(NOmf_c_3,function(x) vcovHC(x,type="HC0"))
summ.NOmp <- summary(NOmp, vcov. = function(x) vcovHC(x, type="HC0"), diagnostics=T)
summ.NOmf <- summary(NOmf, vcov. = function(x) vcovHC(x, type="HC0"), diagnostics=T)
summ.NOmf_c <- summary(NOmf_c, vcov. = function(x) vcovHC(x, type="HC0"), diagnostics=T)
summ.NOmf_c3 <- summary(NOmf_c_3, vcov. = function(x) vcovHC(x, type="HC0"), diagnostics=T)


stargazer(NOmf,NOmf_c,NOmp,NOmf_c_3, type = "html", 
          se = list(rob.NOmf[,"Std. Error"], rob.NOmp[,"Std. Error"],
                    rob.NOmf_c[,"Std. Error"],rob.NOmf_c3[,"Std. Error"]),
          dep.var.labels = c("氮氧化物排放量"),
          column.labels = c("FE","FE","OLS","FE"),
          covariate.labels= c("BIT 適用範圍","BIT 非歧視原則","BIT 財產安全",
                              "BIT 市場准入","BIT 額外條款","BIT 爭端解決","環境公約拘束",
                              "BIT 適用範圍lag3","BIT 非歧視原則lag3","BIT 財產安全lag3",
                              "BIT 市場准入lag3","BIT 額外條款lag3","BIT 爭端解決lag3","環境公約拘束lag3",                              
                              "工業附加價值","人口密度","貿易佔GDP比重","都市化程度","政治發展(polity IV)",
                              "人均所得","人均所得平方","人均所得立方","BIT 適用範圍*環境公約拘束",
                              "BIT非歧視原則*環境公約拘束","BIT 財產安全*環境公約拘束",
                              "BIT 市場准入*環境公約拘束","BIT 額外條款*環境公約拘束","BIT 爭端解決*環境公約拘束",
                              "BIT 適用範圍lag3*環境公約拘束lag3","BIT 非歧視原則lag3*環境公約拘束lag3","BIT 財產安全lag3*環境公約拘束lag3",
                              "BIT 市場准入lag3*環境公約拘束lag3","BIT 額外條款lag3*環境公約拘束lag3","BIT 爭端解決lag3*環境公約拘束lag3","常數"),
          single.row = T,
          out="thesis_frame/NOh1.2.html")


###___________________other h1.2______________________


othermp<-plm(other_gas.x~B_scope+B_nondis+B_props+
            B_access+B_extra+B_dsc+env_in+B_scope*env_in+B_nondis*env_in+B_props*env_in+
            B_access*env_in+B_extra*env_in+B_dsc*env_in+industry_va+pop_pq_m+trade_GDP+urbanization+
            polity+GNI_pc_t+GNI_pc_sqt+GNI_pc_trt,data=qdata,model="pool",na.action = na.exclude)
summary(othermp)
coeftest(othermp, vcovHC)



othermf<-plm(other_gas.x~B_scope+B_nondis+B_props+
            B_access+B_extra+B_dsc+env_in+B_scope*env_in+B_nondis*env_in+B_props*env_in+
            B_access*env_in+B_extra*env_in+B_dsc*env_in,data=qdata,model="within",na.action = na.exclude)
summary(othermf)
coeftest(othermf, vcovHC)


othermf_c<-plm(other_gas.x~B_scope+B_nondis+B_props+
              B_access+B_extra+B_dsc+B_scope*env_in+B_nondis*env_in+B_props*env_in+
              B_access*env_in+B_extra*env_in+B_dsc*env_in+industry_va+pop_pq_m+trade_GDP+urbanization+
              polity+GNI_pc_t+GNI_pc_sqt+GNI_pc_trt,data=qdata,model="within",na.action = na.exclude)
summary(othermf_c)
coeftest(othermf_c, vcovHC)


othermf_c_3<-plm(other_gas.x~B_scope_3+B_nondis_3+B_props_3+
                B_access_3+B_extra_3+B_dsc_3+env_in_3+B_scope_3*env_in_3+B_nondis_3*env_in_3+B_props_3*env_in_3+
                B_access_3*env_in_3+B_extra_3*env_in_3+B_dsc_3*env_in_3+industry_va+pop_pq_m+trade_GDP+urbanization+
                polity+GNI_pc_t+GNI_pc_sqt+GNI_pc_trt,data=qdata,model="within",na.action = na.exclude)
summary(othermf_c_3)
coeftest(othermf_c_3, vcovHC)



rob.othermp<- coeftest(othermp, function(x) vcovHC(x, type="HC0"))
rob.othermf<- coeftest(othermf, function(x) vcovHC(x, type="HC0"))
rob.othermf_c<-coeftest(othermf_c,function(x) vcovHC(x,type="HC0"))
rob.othermf_c3<-coeftest(othermf_c_3,function(x) vcovHC(x,type="HC0"))
summ.othermp <- summary(othermp, vcov. = function(x) vcovHC(x, type="HC0"), diagnostics=T)
summ.othermf <- summary(othermf, vcov. = function(x) vcovHC(x, type="HC0"), diagnostics=T)
summ.othermf_c <- summary(othermf_c, vcov. = function(x) vcovHC(x, type="HC0"), diagnostics=T)
summ.othermf_c3 <- summary(othermf_c_3, vcov. = function(x) vcovHC(x, type="HC0"), diagnostics=T)


stargazer(othermf,othermf_c,othermp,othermf_c_3, type = "html", 
          se = list(rob.othermf[,"Std. Error"], rob.othermp[,"Std. Error"],
                    rob.othermf_c[,"Std. Error"],rob.othermf_c3[,"Std. Error"]),
          dep.var.labels = c("其他汙染物排放量"),
          column.labels = c("FE","FE","OLS","FE"),
          covariate.labels= c("BIT 適用範圍","BIT 非歧視原則","BIT 財產安全",
                              "BIT 市場准入","BIT 額外條款","BIT 爭端解決","環境公約拘束",
                              "BIT 適用範圍lag3","BIT 非歧視原則lag3","BIT 財產安全lag3",
                              "BIT 市場准入lag3","BIT 額外條款lag3","BIT 爭端解決lag3","環境公約拘束lag3",                              
                              "工業附加價值","人口密度","貿易佔GDP比重","都市化程度","政治發展(polity IV)",
                              "人均所得","人均所得平方","人均所得立方","BIT 適用範圍*環境公約拘束",
                              "BIT非歧視原則*環境公約拘束","BIT 財產安全*環境公約拘束",
                              "BIT 市場准入*環境公約拘束","BIT 額外條款*環境公約拘束","BIT 爭端解決*環境公約拘束",
                              "BIT 適用範圍lag3*環境公約拘束lag3","BIT 非歧視原則lag3*環境公約拘束lag3","BIT 財產安全lag3*環境公約拘束lag3",
                              "BIT 市場准入lag3*環境公約拘束lag3","BIT 額外條款lag3*環境公約拘束lag3","BIT 爭端解決lag3*環境公約拘束lag3","常數"),
          single.row = T,
          out="thesis_frame/otherh1.2.html")
