rm(list=ls())
setwd("E:/projet")
library(data.table)
library(FactoMineR)

capi_ca <- fread("INCA/Table_capi_ca.csv")
carnet_ca_1 <- fread("INCA/Table_carnet_ca_1.csv")
conso <- fread("INCA/Table_conso.csv")
indiv <- fread("INCA/Table_indiv.csv")
indiv_ca <- fread("INCA/Table_indiv_ca.csv")
indnut <- fread("INCA/Table_indnut.csv")
menage_1 <- fread("INCA/Table_menage_1.csv")
repas <- fread("INCA/Table_repas.csv")

summary(capi_ca)
summary(carnet_ca_1)
summary(conso)
summary(indiv)
summary(indiv_ca)
summary(indnut)
summary(menage_1)
summary(repas)

capi_ca2 <- capi_ca[-which(is.na(capi_ca)),]
carnet_ca_1_2 <- carnet_ca_1[-which(is.na(carnet_ca_1)),]
conso2 <- conso[-which(is.na(conso)),]

indiv2 <-  indiv[-which(is.na(indiv)),]

indiv_ca2 <- indiv_ca[-which(is.na(indiv_ca)),]
indnut2 <- indnut[-which(is.na(indnut),arr.ind=T)[,1],]
menage_1_2 <- menage_1[-which(is.na(menage_1)),]
repas2 <- repas[-which(is.na(repas)),]

nrow(capi_ca2)/nrow(capi_ca)
nrow(carnet_ca_1_2)/nrow(carnet_ca_1)
nrow(conso2)/nrow(conso)
nrow(indiv2)/nrow(indiv)
nrow(indiv_ca2)/nrow(indiv_ca)
nrow(indnut2)/nrow(indnut)
nrow(menage_1_2)/nrow(menage_1)
nrow(repas2)/nrow(repas)

indiv2 <- apply(indiv2,MARGIN=2,FUN = as.factor)
class(indiv2)
indiv2 <- as.data.frame(indiv2)


for (i in 1:ncol(indiv2)){
  plot(indiv2[,i],type="h")
}

res.indiv=MCA(indiv2)

barplot(res.indiv$eig[1:20,2],names=paste("Dim",1:nrow(res.indiv$eig[1:20,])))
round(res.indiv$eig[1:5,],2)

plot(res.indiv, invisible=c("var"), habillage="sexeps")
plot(res.indiv, invisible=c("ind"),habillage="sexeps")
plot(res.indiv, choix="var")

dimdesc(res.indiv)
summary(indiv2)

indiv3 <- apply(indiv2,MARGIN=2, FUN= function(x) if(nlevels(x)==0) {x<-1} )
class(indiv3)
class(indiv2)
summary(indiv3)
