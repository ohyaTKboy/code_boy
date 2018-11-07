library(readxl)

BIT_ENs<-read_excel("D:/研究所/BIT_coding/BIT/BIT_all_substantive.xlsm",sheet=1)
BIT_ENs<-as.data.frame(BIT_ENs)

BIT_ESs<-read_excel("D:/研究所/BIT_coding/BIT/esBIT-substantive.xlsx",sheet=1)
BIT_ESs<-as.data.frame(BIT_ESs)
View(BIT_ESs)

BIT_FRs<-read_excel("D:/研究所/BIT_coding/BIT/BIT_FRs_final.xlsx",sheet=2)
BIT_FRs<-as.data.frame(BIT_FRs)



##___________partial replacement based on the other dataframe________________________________
replacement <- BIT_ESs$marketlib[match(BIT_ENs$bit_ids, BIT_ESs$bit_ids)]
BIT_ENs$marketlib <- ifelse(is.na(replacement), BIT_ENs$marketlib, replacement)

n<-c(11:ncol(BIT_ENs))

for(i in n){
  replacement<-BIT_ESs[,i-5][match(BIT_ENs$bit_ids, BIT_ESs$bit_ids)]
  BIT_ENs[,i] <- ifelse(is.na(replacement), BIT_ENs[,i], replacement)
}

for(i in n){
  replacement<-BIT_FRs[,i][match(BIT_ENs$bit_ids, BIT_FRs$bit_ids)]
  BIT_ENs[,i] <- ifelse(is.na(replacement), BIT_ENs[,i], replacement)
}

View(BIT_ENs)

write.csv(BIT_ENs,"D:/研究所/BIT_coding/BIT/BIT_sub_all.csv")


###_______________________split column______________________

library(stringr)

BIT_suball<-read_excel("D:/研究所/BIT_coding/BIT/BIT_sub_all.xlsx")
BIT_suball<-as.data.frame(BIT_suball)
View(BIT_suball)
parties<-str_split_fixed(BIT_suball$Parties,",",2)
parties<-as.data.frame(parties)
colnames(parties)<-c("Parties1","Parties2")
BIT_suball<-cbind(BIT_suball,parties)
BIT_suball<-BIT_suball[,-7]
write.csv(BIT_suball,"D:/研究所/BIT_coding/BIT/BIT_sub_all.csv")

###______________country level______________________________

duplicated(BIT_suball$`Short title`)

BIT_suball<-BIT_suball[!duplicated(BIT_suball$`Short title`),]
write.csv(BIT_suball,"D:/研究所/BIT_coding/BIT/BIT_sub_countrylevel.csv")


###______________procedure law______________________
library(readxl)
library(stringr)


BIT_proEN<-read_excel("D:/研究所/BIT_coding/BIT/BIT_procedure.xlsx",sheet=1)
BIT_proEN<-as.data.frame(BIT_proEN)
BIT_proFR<-read_excel("D:/研究所/BIT_coding/BIT/Procedure_FR_final.xlsx",sheet=2)
BIT_proFR<-as.data.frame(BIT_proFR)
BIT_proES<-read_excel("D:/研究所/BIT_coding/BIT/BIT-ES-Procedure.xlsx",sheet=1)
BIT_proES<-as.data.frame(BIT_proES)
View(BIT_proES)

n<-c(11:ncol(BIT_proEN))

for(i in n){
  replacement<-BIT_proFR[,i-8][match(BIT_proEN$bit_ids, BIT_proFR$bit_ids)]
  BIT_proEN[,i] <- ifelse(is.na(replacement), BIT_proEN[,i], replacement)
}

for(i in n){
  replacement<-BIT_proES[,i][match(BIT_proEN$bit_ids, BIT_proES$bit_ids)]
  BIT_proEN[,i] <- ifelse(is.na(replacement), BIT_proEN[,i], replacement)
}

write.csv(BIT_proEN,"D:/研究所/BIT_coding/BIT/BIT_pro_all.csv")

BIT_proEN_c<-BIT_proEN[!duplicated(BIT_proEN$bit_ids),]
View(BIT_proEN_c)

BIT_suball<-read_excel("D:/研究所/BIT_coding/BIT/BIT_sub_all.xlsx",sheet=1)
View(BIT_suball)

n<-c(31:ncol(BIT_suball))
n

for(i in n){
  BIT_suball[,i]<-BIT_proEN_c[,i-20][match(BIT_suball$bit_ids, BIT_proEN_c$bit_ids)]
  print(i)
}


parties<-str_split_fixed(BIT_suball$Parties,",",2)
parties<-as.data.frame(parties)
colnames(parties)<-c("Party1","Party2")
BIT_suball<-cbind(BIT_suball,parties)
BIT_suball<-BIT_suball[,-7]


write.csv(BIT_suball,"D:/研究所/BIT_coding/BIT/BIT_all.csv")


###______________BIT all country level_______________________

BIT_suball_c<-BIT_suball[!duplicated(BIT_suball$bit_ids),]


###______________dyads_____________________________________

BIT_case<-read_excel("D:/研究所/BIT_coding/BIT/BIT_all.xlsx",sheet=2)
Party<-data.frame(BIT_case$Party2,BIT_case$Party1)
colnames(Party)<-c("Party1","Party2")
BIT_case<-BIT_case[,-c(39:40)]
BIT_case<-cbind(BIT_case,Party)

write.csv(BIT_case,"D:/研究所/BIT_coding/BIT/BIT_countryd2.csv")

BIT_1<-BIT_case[order(BIT_case$Party1),]

