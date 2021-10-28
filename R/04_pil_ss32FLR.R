################################################################################
#                      IBPIS 2021 - ss3 output to FLStock                      # 
#------------------------------------------------------------------------------#
#                                                                              #
#   Laura Wise (IPMA)                                                          #
#   created:  19/10/2021                                                       #
#                                                                              #
################################################################################

# Copyright: IPMA, 2021
# Author: Laura Wise (IPMA) (<lwise@ipma.pt>)
#
# Distributed under the terms of the GNU GPLv3


#==============================================================================
#                                                                          ----
#==============================================================================

rm(list=ls())

#==============================================================================
# WORKING DIRECTORY                                                        ----
#==============================================================================

wd <- "D:/ICES/IBPIS2021"
setwd(wd)

# directory with SS runs
res.ss <- file.path("./SS_runs")

#==============================================================================
# LOAD LIBRARIES                                                           ----
#==============================================================================

library(FLCore)

#==============================================================================
#  Convert SS outputs to a FLStock object                                  ----
#==============================================================================

ss3flr <- function(file.path,desc){

dados <- r4ss::SS_readdat(paste0(file.path,"/data.ss_new"), verbose = TRUE, echoall = FALSE, section = NULL,version = "3.30")
assess <- r4ss::SS_output(file.path)
maxyear <- assess$endyr

########################## EXTRACT ASSESSMENT INPUTS ###################################

### WEIGHTS-AT-AGE AND MATURITY

west<-assess$wtatage[assess$wtatage$Fleet==0 ,-c(1:6)]

maturity<-assess$wtatage[assess$wtatage$Fleet==-2 ,-c(1:6)]

maturity<-maturity/west
maturity[,1]<-0
maturity[nrow(maturity),] <- colMeans(maturity[(nrow(maturity)-6):(nrow(maturity)-1),])

#west <- west[-nrow(west),]

stock.wt=FLQuant(unname(as.matrix(t(west))),quant="age",units="kg")
dimnames(stock.wt)[[1]]=as.character(min(assess$agebins):max(assess$agebins))
dimnames(stock.wt)[[2]]=as.character(assess$startyr:(assess$endyr))


weca<-assess$wtatage[assess$wtatage$Fleet==1 & assess$wtatage$Yr %in% c(assess$startyr:(assess$endyr)),-c(1:6)]

landings.wt=FLQuant(unname(as.matrix(t(weca))),quant="age",units="kg")
dimnames(landings.wt)[[1]]=as.character(min(assess$agebins):max(assess$agebins))
dimnames(landings.wt)[[2]]=as.character(assess$startyr:(assess$endyr))



mat=FLQuant(unname(as.matrix(t(maturity))),quant="age")
dimnames(mat)[[1]]=as.character(min(assess$agebins):max(assess$agebins))
dimnames(mat)[[2]]=as.character(assess$startyr:(assess$endyr))



### LANDINGS, CATCH-AT-AGE,  M-AT-AGE

landings<-assess$catch$Exp[assess$catch$Yr %in% c(assess$startyr:(assess$endyr))]
#landings<-dados$catch[dados$catch$year %in% c(assess$startyr:(assess$endyr)),"catch"]

landings=FLQuant(unname(as.matrix(t(landings))),units="t",quant="age")
dimnames(landings)[[2]]=as.character(assess$startyr:(assess$endyr))


#landings.n<-subset(dados$agecomp,FltSvy==1 ,c("a0","a1","a2","a3","a4","a5","a6"))
landings.n<-subset(assess$catage,Fleet==1 & Yr %in% c(assess$startyr:(assess$endyr)),c("0","1","2","3","4","5","6"))

#landings.n[nrow(landings.n)+1,]<-rep(0,ncol(landings.n))

landings.n=FLQuant(unname(t(landings.n)),quant="age",units="10^3")
dimnames(landings.n)[[2]]=as.character(assess$startyr:(assess$endyr))
dimnames(landings.n)[[1]]=as.character(min(assess$agebins):max(assess$agebins))


m<-matrix(rep(assess$endgrowth$M,length(assess$startyr:(assess$endyr))),byrow=F,nrow=7)

m=FLQuant(unname(as.matrix(m)),quant="age")
dimnames(m)[[2]]=as.character(assess$startyr:(assess$endyr))
dimnames(m)[[1]]=as.character(min(assess$agebins):max(assess$agebins))



########################## EXTRACT ASSESSMENT INPUTS ###################################

## SSB, REC, SELECTIVITY, F-AT-AGE, REFERENCE F, N-AT-AGE


ssb<-assess$timeseries[assess$timeseries$Era=="TIME" & assess$timeseries$Yr<=assess$endyr,c("Yr","Bio_smry")]

ssb=FLQuant(unname(t(as.matrix(ssb$Bio_smry))),units="t",quant="age")
dimnames(ssb)[[2]]=c(as.character(assess$startyr:(assess$endyr)))


rec<-assess$timeseries[assess$timeseries$Era=="TIME" & assess$timeseries$Yr<=assess$endyr,c("Yr","Recruit_0")]

rec=FLQuant(unname(t(as.matrix(rec$Recruit_0))),units="10^3",quant="age")
dimnames(rec)[[2]]=c(as.character(assess$startyr:(assess$endyr)))



#### CALCULATE F-AT-AGE FROM APICAL F AND SELECTIVITY-AT-AGE

selectivity<-subset(assess$ageselex[assess$ageselex$Fleet==1 & assess$ageselex$Factor=="Asel2" & assess$ageselex$Yr %in% c(assess$startyr:(assess$endyr)),c("Yr","0","1","2","3","4","5","6") ])


linhas<-grep("F_\\d",assess$derived_quants$Label)
f.apical<-data.frame(f=assess$derived_quants[linhas,"Value"],Yr=assess$startyr:assess$endyr)


f.apsel<-merge(f.apical,selectivity,all.x = T,by="Yr")


f<-f.apsel$f*f.apsel[,c("0","1","2","3","4","5","6")]
f$year<-f.apsel$Yr

harvest=FLQuant(unname(as.matrix(t(f[,-8]))),quant="age",units="f")
dimnames(harvest)[[2]]=as.character(assess$startyr:(assess$endyr))
dimnames(harvest)[[1]]=as.character(min(assess$agebins):max(assess$agebins))

refF<-apply(f[,c("2","3","4","5")],1,mean)

##########

natage=assess$natage[assess$natage$Era=="TIME" & assess$natage$`Beg/Mid`=="B" & assess$natage$Yr<=assess$endyr,c("0","1","2","3","4","5","6"),]

stock.n=FLQuant(unname(as.matrix(t(natage))), quant='age', units='10^3')
dimnames(stock.n)[[2]]=as.character(assess$startyr:(assess$endyr))
dimnames(stock.n)[[1]]=as.character(min(assess$agebins):max(assess$agebins))


########################## OTHER FLQUANT OBJECTS NEEDED ################################  
harvest.spwn=FLQuant(matrix(0,ncol=dim(harvest)[2],nrow=dim(harvest)[1]),quant="age",units="f")
dimnames(harvest.spwn)[[1]]=as.character(min(assess$agebins):max(assess$agebins))
dimnames(harvest.spwn)[[2]]=as.character(assess$startyr:(assess$endyr))

m.spwn=harvest.spwn

stock=quantSums(stock.wt*stock.n)

catch.n=landings.n
units(catch.n)<-"10^3"
catch.wt=landings.wt
units(catch.wt)<-"kg"
catch=landings
units(catch)<-"t"
# 
discards.n<-matrix(rep(rep(0,7),length(assess$startyr:(assess$endyr))),byrow=F,nrow=7)
discards.n=FLQuant(unname(as.matrix(discards.n)),quant="age")
dimnames(discards.n)[[2]]=as.character(assess$startyr:(assess$endyr))
dimnames(discards.n)[[1]]=as.character(min(assess$agebins):max(assess$agebins))
# 
discards.wt<-discards.n
# 
discards=FLQuant(unname(rep(0,length(assess$startyr:(assess$endyr)))),units="t",quant="age")
dimnames(discards)[[2]]=as.character(assess$startyr:(assess$endyr))


pil.stock=FLStock(landings=landings,landings.n=landings.n,landings.wt=landings.wt,
                  discards=discards,discards.n=discards.n,discards.wt=discards.wt,
                  catch=catch,catch.n=catch.n,catch.wt=catch.wt,stock=stock,stock.n=stock.n,
                  stock.wt=stock.wt,mat=mat,m=m,harvest=harvest,harvest.spwn=harvest.spwn,
                  m.spwn=m.spwn,name="pil",desc=desc)
range(pil.stock,"minfbar")<-2
range(pil.stock,"maxfbar")<-5


validObject(pil.stock)
summary(pil.stock)
save(pil.stock, file=paste0(file.path,"/pil.stock.RData"))

rm(list=ls())

}


ss3flr(file.path = paste0(res.ss,"/Setupa"), desc = "Setupa")
ss3flr(file.path = paste0(res.ss,"/Setupb"), desc = "Setupb")
ss3flr(file.path = paste0(res.ss,"/Setupc"), desc = "Setupc")
ss3flr(file.path = paste0(res.ss,"/SetupaSDTune2"), desc = "SetupaSDTune2")
ss3flr(file.path = paste0(res.ss,"/SetupaSDQTune2"), desc = "SetupaSDQTune2")
