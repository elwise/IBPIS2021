################################################################################
#                      IBPIS 2021 - Short term forecast                        # 
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

# directory to save plots 
res.plots <- file.path("./plots")

# directory with SS runs
res.ss <- file.path("./SS_runs")

# directory with data
res.dir  <- file.path("./data")

#==============================================================================
# LOAD LIBRARIES                                                           ----
#==============================================================================

library(FLCore)
library(FLAssess)
library(FLash)

#==============================================================================
#  Convert SS outputs to a FLStock object                                  ----
#==============================================================================

file.path <- paste0(res.ss,"/Setupa")
dados <- r4ss::SS_readdat(paste0(file.path,"/data.ss_new"), verbose = TRUE, echoall = FALSE, section = NULL,version = "3.30")
assess <- r4ss::SS_output(file.path)
maxyear<-assess$endyr

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
                  m.spwn=m.spwn,name="pil",desc="2019")
range(pil.stock,"minfbar")<-2
range(pil.stock,"maxfbar")<-5


validObject(pil.stock)
summary(pil.stock)

pil <- pil.stock


#############################  CLEAN AND SAVE WORKSPACE #############################################

rm(list=ls(pat="har"))
rm(list=ls(pat="f"))
rm(list=ls(pat="cat"))
rm(list=ls(pat="lan"))
rm(list=ls(pat="we"))
rm(list=ls(pat="m"))
rm(list=ls(pat="li"))
rm(list=ls(pat="ass"))
rm(list=ls(pat="da"))
rm(list=ls(pat="disc"))
rm("rec","ssb","stock.n","stock","maxyear","stock.wt","selectivity","natage")
rm(pil.stock)

save(pil, file="D:/IPMA/SARDINE/Interbenchmark2021/SS_runs/Final_Setupa/pil.stock.RData")

###STF
#---------------------------------------------------------------
# Steps necessary for the short term forecast
#---------------------------------------------------------------
#Define the assessment/interim year
ass.yr <- endyr <- 2020 

#set working directory
stf.dir <- 'D:IPMA/SARDINE/Interbenchmark2021/SS_runs/Final_Setupa'
setwd(stf.dir)

# Load pil.27.8c9a stock FLR object
load("pil.stock.RData")
pil.stock <- pil

# Do a 2 year forecast
pil.stock_stf <- stf(pil.stock, nyears = 2, wts.nyears=1)
# Now the stock goes up to endyr+1
summary(pil.stock_stf)


harvest(pil.stock_stf)[,ac(2021:2022)] <- harvest(pil.stock_stf)[,ac(2019)] 
landings(pil.stock_stf)[,ac(2021:2022)] <- NA
landings.n(pil.stock_stf)[,ac(2020:2022)] <- 1.0
catch(pil.stock_stf)[,ac(2021:2022)] <- NA
#---------------------------------------------------------------
# The stock-recruitment relationship (SRR)
#---------------------------------------------------------------

# A STF does not use a SRR 
# Instead it assumes that recruitment in the future is the mean of the last X years
# However, we still need to pass the projection a SRR that contains the mean value
# For the pil278c9a stock we use the mean of the last 5 years
# In this case we will use the interim year estimate also, while before we didn't
mean_rec <- exp(mean(log(rec(pil.stock)[,ac((endyr-4):(endyr))])))
# set up an FLSR object with a geometric mean model
pil.stock_sr <- as.FLSR(pil.stock, model="geomean")
params(pil.stock_sr)['a',] <- mean_rec

# what if we used the estimate from the last year only?
recEndyr <- as.numeric(rec(pil.stock_stf[,ac(2020)]))
pil.stock_sr2 <- as.FLSR(pil.stock, model="geomean")
params(pil.stock_sr2)['a',] <- recEndyr

# or the last 10 years?
rec10 <- exp(mean(log(rec(pil.stock)[,ac((endyr-9):(endyr))])))
# set up an FLSR object with a geometric mean model
pil.stock_sr3 <- as.FLSR(pil.stock, model="geomean")
params(pil.stock_sr3)['a',] <- rec10

#---------------------------------------------------------------
# compute the interim year with the assumed catch
#---------------------------------------------------------------

# Set Fsqtatus quo equal to the mean of the last 3 years in the assessment

fbar_status_quo<-mean(fbar(pil.stock[,ac((endyr-3):(endyr-1))]))

#---------------------------------------------------------------
# Short term forecast with many F scenarios
#---------------------------------------------------------------

# Typically when running STF you explore several different future F scenarios

# We are going to run several F scenarios for the STF
# The scenarios are based on 'F status quo', which we calculated above as the mean F of the last 3 years
# For a 2 year STF the F pattern is:
# year 1: fbar_status_quo* fbar_multiplier
# year 2: fbar_status_quo * fbar_multiplier
# The fbar_multiplier is the same for years 1 and 2

# We are going to run several STFs with different values for the fbar_multiplier
# The fbar_multiplier ranges from 0.1 to 2 by 0.1
fbar_multiplier <- seq(from = 0, to = 2, by = 0.1)

#or just select the Fbar scenarios you want

# As we already have the projection for the interim year, we use the multiplier already in the first projection year 
fbar_scenarios <- cbind(c(0,0.032,0.064,0.118,0.156,as.numeric(fbar(pil.stock_stf)[,ac(endyr)]),
                          fbar_multiplier*fbar_status_quo),c(0,0.032,0.064,0.118,0.156,
                                                             as.numeric(fbar(pil.stock_stf)[,ac(endyr)]),fbar_multiplier*fbar_status_quo))
colnames(fbar_scenarios) <- c("2021","2022")


fbar_scenarios

# There are various results we want to extract from the STF
# Like predicted Catch, SSB and the relative change in these
# The following is what we calculate in the STECF Med. WG
# Make an empty matrix in which to store the results
stf_results <- matrix(NA,nrow = nrow(fbar_scenarios),ncol = 10)
# Set some column names
final_year <- endyr
colnames(stf_results) <- c(paste('B1+',final_year,sep="_"),
                           paste0('F',endyr),
                           paste('Catch',final_year,sep="_"),
                           paste('B1+',final_year+1,sep="_"),
                           #'Fsq','Fmult',
                           'F',
                           paste('Catch',final_year+1,sep="_"), 
                           paste('B1+',final_year+2,sep="_"),
                           paste('Catch',final_year+2,sep="_"),
                           paste('Change_B1+_',final_year+1,'-',final_year+2,'(%)',sep=""),
                           paste('Change_Catch_',final_year-1,'-',final_year+1,'(%)',sep=""))
head(stf_results)

# Store the resulting FLStock each time
stk_stf <- FLStocks()
# Loop over the scenarios (each row in the fbar_scenarios table)
for (scenario in 1:nrow(fbar_scenarios)) {
  cat("Scenario: ", scenario, "\n")
  # Make a target object withe F values for that scenario
  ctrl_target <- data.frame(year = (endyr+1):(endyr+2),
                            quantity = "f",
                            val = fbar_scenarios[scenario,])
  # Set the control object - year, quantity and value for the moment
  ctrl_f <- fwdControl(ctrl_target)
  # ctrl_target
  # Run the forward projection. We include an additional argument, maxF.
  # By default the value of maxF is 2.0
  # Here we increase it to 10.0 so that F is not limited
  pil.stock_fwd <- fwd(pil.stock_stf, ctrl = ctrl_f, sr = pil.stock_sr3)#, maxF = 10.0)
  ## Check it has worked - uncomment out to check scenario by scenario
  # plot(pil.stock_fwd[,ac(2001:2018)])
  # Store the result - if you want to, comment out if unnecessary
  #stk_stf[[as.character(scenario)]] <- pil.stock_fwd
  
  # Fill results table
  stf_results[scenario,1] <- stock(pil.stock_fwd)[,ac(final_year)]   # Interim year B1+
  stf_results[scenario,2] <- mean(harvest(pil.stock_fwd)[ac(2:5),ac(final_year)])
  stf_results[scenario,3] <- catch(pil.stock_fwd)[,ac(final_year)]   # Interim year catch stf year
  stf_results[scenario,4] <- stock(pil.stock_fwd)[,ac(final_year+1)] # 1st stf year B1+
  #stf_results[scenario,5] <- fbar_status_quo # f status quo
  #stf_results[scenario,6] <- fbar_multiplier[scenario] # F_multiplier
  stf_results[scenario,5] <- fbar_scenarios[scenario,1]
  stf_results[scenario,6] <- catch(pil.stock_fwd)[,ac(final_year+1)] # 1st stf year catch
  stf_results[scenario,7] <- stock(pil.stock_fwd)[,ac(final_year+2)] # 2nd stf year B1+
  stf_results[scenario,8] <- catch(pil.stock_fwd)[,ac(final_year+2)] # 2nd stf year Catch
  
  # Change in SSB and Catch
  stf_results[scenario,9] <- (stock(pil.stock_fwd)[,ac(final_year+2)]-stock(pil.stock_fwd)[,ac(final_year+1)])/stock(pil.stock_fwd)[,ac(final_year+1)]*100 # change in B1+ in last two stf years
  stf_results[scenario,10] <- (catch(pil.stock_fwd)[,ac(final_year+1)]-catch(pil.stock_fwd)[,ac(final_year-1)])/catch(pil.stock_fwd)[,ac(final_year-1)]*100 # change in catch from true year, to 2nd to last stf year
}
# Give the FLStocks object some names
#names(stk_stf) <- rownames(fbar_scenarios)

# Plotting
# plot(window(stk_stf, start=2010, end=final_year+3))

# Look at the table of results
head(stf_results)
stf_results


# export this if necessary
write.csv(stf_results,file="scenarios10years.csv",row.names=F)

#request for F that B1+(2022)==Blim==196334
ctrl_target <- data.frame(year = (endyr):(endyr+1),quantity = c("catch","ssb"),val = c(9670,195740))
ctrl_f <- fwdControl(ctrl_target)
pil.stock_Blim <- fwd(pil.stock_stf, ctrl = ctrl_f, sr = pil.stock_sr)

#request for F that B1+(2022)==Bpa==252523
ctrl_target <- data.frame(year = (endyr):(endyr+1),quantity = c("catch","ssb"),val = c(9670,251775))
ctrl_f <- fwdControl(ctrl_target)
pil.stock_Bpa <- fwd(pil.stock_stf, ctrl = ctrl_f, sr = pil.stock_sr)

rm(list=ls())

stfGeoMean <- read.csv(file="scenariosGeoMean.csv")
stfGeoMean$STF <- "Geomean"
stfGeoMean$rec <- mean_rec
stfLastYr <- read.csv(file="scenariosLastYear.csv")
stfLastYr$STF <- "LastYr"
stfLastYr$rec <- recEndyr
stf10Yr <- read.csv(file="scenarios10years.csv")
stf10Yr$STF <- "tenYrs"
stf10Yr$rec <- rec10
stf2020 <- read.csv("D:/IPMA/SARDINE/WGHANSA2020/STF/pil.27.8c9a_stf_scenarios2020_OptionB_HCR12.csv")
stf2020$STF <- "A2020"
stf2020$rec <- 7584483

stfs <- bind_rows(stfGeoMean,stfLastYr,stf10Yr,stf2020)
stfs %>%
  filter(F < 0.18) %>%
  group_by(STF) %>%
  mutate(lbl = case_when(F == max(F) ~ round(rec,0), TRUE ~ NA_real_)) %>%
  ggplot(aes(x=F,y=Catch_2021,colour=STF))+
  geom_line(size=1)+
  geom_text(aes(label = lbl, vjust = -(STF == "Geomean")), hjust = -0.2, show.legend = F) +
  expand_limits(x= 0.2) +
  coord_cartesian(clip = 'off') 