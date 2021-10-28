################################################################################
#                      IBPIS 2021 - ss3diags                                   # 
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
# LOAD LIBRARIES                                                           ----
#==============================================================================

#library(tidyverse)
library(ss3diags)


#==============================================================================
#  Retrospectives and ss3 diag for SETUP A                                ----
#==============================================================================

diagSS <- function(file.path,run){

  
  setwd(file.path)
 
  dir.create("Plotdiags",showWarnings = F)
  
  # Normally load model output
  # Reference run
  ss3rep = r4ss::SS_output(dir =file.path,printstats = T, covar=T,forecast = F,printhighcor=70,printlowcor=20,ncols=2000,covarfile = "covar.sso")
  
  ## Normally we would run the r4ss function to do the retrospective analysis
  ## However this does not work for our case, as far as I know, since we don't have cath at age in the final year and have to change the recdev year
  ## Instead we have to do the retrospective 'manually'. The outputs are in folder retrosectives within each SS run
  
  # run retrospective analysis (select number of years)
  #r4ss::SS_doRetro(masterdir=file.path, oldsubdir="", newsubdir="retrospectives", years=0:-5)
  
  # read in outputs
  retroModels <- r4ss::SSgetoutput(dirvec=file.path(file.path,"retrospectives",paste("retro",0:-5,sep="")))
  
  # summarize output
  retroSummary <- r4ss::SSsummarize(retroModels)
  r4ss::SSplotComparisons(summaryoutput=retroSummary,subplots = c(2,8,10,12),xlim=c(1978,2020),legendlabels = c(0:-5),
                    print = TRUE,plotdir = file.path(file.path,"retrospectives"))
  
  # make Squid Plot of recdev retrospectives 
  png(paste0('Plotdiags/retrospective_dev_plots_',run,'.png'),width = 680, height = 480)
  par(mfrow=c(2,1))
  # first scaled relative to most recent estimate
  r4ss::SSplotRetroRecruits(retroSummary, endyrvec=c(2020:2015), cohorts=2020:2013,
                      relative=TRUE, legend=FALSE)
  # second without scaling
  r4ss::SSplotRetroRecruits(retroSummary, endyrvec=c(2020:2015), cohorts=2020:2013,
                      relative=FALSE, legend=FALSE)
  dev.off()

  
  # Check Data
  sspar()
  r4ss::SSplotData(ss3rep,subplot = 2)
  dev.print(jpeg,paste0("Plotdiags/Data_",run,".jpg"), width = 8, height = 6, res = 300, units = "in")
  
  
  #****************************************
  # Basic Residual Diags
  #****************************************
  
  # Check Runs Test and Joint residuals option for mean composition data
  sspar(mfrow=c(2,3),plot.cex = 0.8)
  # For cpue
  SSplotRunstest(ss3rep,subplots="cpue",add=T)
  # For length
  SSplotRunstest(ss3rep,subplots="age",add=T)
  dev.print(jpeg,paste0("Plotdiags/RunsTestResiduals_",run,".jpg"), width = 8, height = 7, res = 300, units = "in")
  
  # Check conflict between mean lengths
  sspar(mfrow=c(1,2),plot.cex = 0.8)
  SSplotJABBAres(ss3rep,subplots="cpue",add=T)
  SSplotJABBAres(ss3rep,subplots="age",add=T)
  dev.print(jpeg,paste0("Plotdiags/JointResiduals_",run,".jpg"), width = 8, height = 3.5, res = 300, units = "in")
  
  
  #*******************************************************************
  #  Retrospective Analysis with Hindcasting (DOES NOT WORK WITH OUR RETRO ONLY AUTOMATIC RUNS OF r4ss)
  #*******************************************************************

  # Now Check Retrospective Analysis with one-step ahead Forecasts
  sspar(mfrow=c(2,1),plot.cex = 0.9)
  SSplotRetro(retroSummary,forecast = T,add=T,legend=F)
  SSplotRetro(retroSummary,xmin=2005,forcastrho = T,add=T,legend = F)
  dev.print(jpeg,paste0("Plotdiags/RetroForecast_",run,".jpg"), width = 8, height = 9, res = 300, units = "in")
  
  # Do Hindcast with Cross-Validation of CPUE observations
  sspar(mfrow=c(1,1),plot.cex = 0.9)
  SSplotHCxval(retroSummary,xmin=2005,add=T)
  dev.print(jpeg,paste0("Plotdiags/HCxvalIndex_",run,".jpg"), width = 8, height = 5, res = 300, units = "in")
  # Also test new feature of Hindcast with Cross-Validation for mean length
  
  # Use new converter fuction SSretroComps()
  hccomps = SSretroComps(retroModels)
  sspar(mfrow=c(1,2),plot.cex = 0.7)
  SSplotHCxval(hccomps,add=T,subplots = "age",legendloc="topleft")
  dev.print(jpeg,paste0("Plotdiags/HCxvalLen_",run,".jpg"), width = 8, height = 4, res = 300, units = "in")
  
  
  #****************************************************************
  # Approximate uncertainty with MVLN (hessian)
  
  # Check starter file
  starter = SSsettingsBratioF(ss3rep)
  # Get uncertainty from MVLN for F/F_Btrg with original F setting F_abs
  
  # >>>>>>> Problem here (new version?)
  sspar(mfrow=c(1,1),plot.cex = 0.9)
  mvn = SSdeltaMVLN(ss3rep,plot = T,years=1978:2020)
  mvn$labels # the out put is SSB/SSBtrg
  dev.print(jpeg,paste0("Plotdiags/Kobe_",run,".jpg"), width = 6.5, height = 6.5, res = 300, units = "in")
  
  sspar(mfrow=c(2,2),plot.cex = 0.9)
  SSplotEnsemble(mvn$kb,ylabs = mvn$labels,add=T)
  dev.print(jpeg,paste0("Plotdiags/MLVN_trj_",run,".jpg"), width = 8, height = 6.5, res = 300, units = "in")
  
  
}

diagSS(file.path='D:/ICES/IBPIS2021/SS_runs/Setupa',run="Setupa")

diagSS(file.path='D:/ICES/IBPIS2021/SS_runs/Setupb',run="Setupb")

diagSS(file.path='D:/ICES/IBPIS2021/SS_runs/Setupc',run="Setupc")

diagSS(file.path='D:/ICES/IBPIS2021/SS_runs/SetupaSDTune2',run="SetupaSDTune2")

diagSS(file.path='D:/ICES/IBPIS2021/SS_runs/SetupaSDQTune2',run="SetupaSDQTune2")


#Mohn's rho
retroModels <- r4ss::SSgetoutput(dirvec=file.path('D:/ICES/IBPIS2021/SS_runs/SetupaSDTune2',"retrospectives",paste("retro",0:-5,sep="")))
retroSummary <- SSsummarize(retroModels)
endyrvec <- retroSummary[["endyrs"]]


x <- round(icesAdvice::mohn(retroSummary$recruits,details = T)$rho,3)
y <- round(icesAdvice::mohn(retroSummary$Fvalue,details = T)$rho,3)
z <- round(icesAdvice::mohn(retroSummary$SpawnBio,details = T)$rho,3)
Label <- c("SSB","Recruits","Fvalue")

dd <- data.frame(Label=Label,Rho=c(x,y,z))

dd%>%
  gt::gt()%>%
  gt::gtsave("MohnRhoSDTune2.png")

#Mohn's rho
retroModels <- r4ss::SSgetoutput(dirvec=file.path('D:/ICES/IBPIS2021/SS_runs/SetupaSDQTune2',"retrospectives",paste("retro",0:-5,sep="")))
retroSummary <- SSsummarize(retroModels)
endyrvec <- retroSummary[["endyrs"]]


x <- round(icesAdvice::mohn(retroSummary$recruits,details = T)$rho,3)
y <- round(icesAdvice::mohn(retroSummary$Fvalue,details = T)$rho,3)
z <- round(icesAdvice::mohn(retroSummary$SpawnBio,details = T)$rho,3)
Label <- c("SSB","Recruits","Fvalue")

dd <- data.frame(Label=Label,Rho=c(x,y,z))

dd%>%
  gt::gt()%>%
  gt::gtsave("MohnRhoSDQTune2.png")

