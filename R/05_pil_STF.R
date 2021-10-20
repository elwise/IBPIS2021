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

# directory with SS runs
res.ss <- file.path("./SS_runs")

#==============================================================================
# LOAD LIBRARIES                                                           ----
#==============================================================================

library(FLCore)
library(FLAssess)
library(FLash)

library(dplyr)
library(ggplot2)

#==============================================================================
#  Run short term forecast                                                 ----
#==============================================================================

#Define the assessment/interim year
ass.yr <- endyr <- 2020 

#set working directory
stf.dir <- paste0(res.ss,"/Setupa")
setwd(stf.dir)

# Load stock FLR object
load("pil.stock.RData")

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

# Do a 2 year forecast
pil.stock_stf <- stf(pil.stock, nyears = 2, wts.nyears=1)
# Now the stock goes up to endyr+1
summary(pil.stock_stf)


harvest(pil.stock_stf)[,ac(2021:2022)] <- harvest(pil.stock_stf)[,ac(2019)] 
landings(pil.stock_stf)[,ac(2021:2022)] <- NA
landings.n(pil.stock_stf)[,ac(2020:2022)] <- 1.0
catch(pil.stock_stf)[,ac(2021:2022)] <- NA


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


## change SRR accordingly!!


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

# export this if necessary
write.csv(stf_results,file="scenariosGeoMean.csv",row.names=F)
write.csv(stf_results,file="scenariosLastYear.csv",row.names=F)
write.csv(stf_results,file="scenariosGeoMean.csv",row.names=F)


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
