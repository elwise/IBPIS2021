################################################################################
#                      IBPIS 2021 - SS runs                                    # 
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


#==============================================================================
# LOAD LIBRARIES                                                           ----
#==============================================================================

library(r4ss)
library(tidyverse)

load("D:/ICES/IBPIS2021/SS_runs/SetupaSDQTune2/retrospectives/retro0/retro-0.RData")
load("D:/ICES/IBPIS2021/SS_runs/SetupaSDQTune2/retrospectives/retro-1/retro-1.RData")
load("D:/ICES/IBPIS2021/SS_runs/SetupaSDQTune2/retrospectives/retro-2/retro-2.RData")
load("D:/ICES/IBPIS2021/SS_runs/SetupaSDQTune2/retrospectives/retro-3/retro-3.RData")
load("D:/ICES/IBPIS2021/SS_runs/SetupaSDQTune2/retrospectives/retro-4/retro-4.RData")
load("D:/ICES/IBPIS2021/SS_runs/SetupaSDQTune2/retrospectives/retro-5/retro-5.RData")

xx0 <- xx0[,c(1,4,11:13)]
names(xx0) <- c("B1y","B1y+1","endyr_rec","georec","retro")
xx1 <- xx1[,c(1,4,11:13)]
names(xx1) <- c("B1y","B1y+1","endyr_rec","georec","retro")
xx2 <- xx2[,c(1,4,11:13)]
names(xx2) <- c("B1y","B1y+1","endyr_rec","georec","retro")
xx3 <- xx3[,c(1,4,11:13)]
names(xx3) <- c("B1y","B1y+1","endyr_rec","georec","retro")
xx4 <- xx4[,c(1,4,11:13)]
names(xx4) <- c("B1y","B1y+1","endyr_rec","georec","retro")
xx5 <- xx5[,c(1,4,11:13)]
names(xx5) <- c("B1y","B1y+1","endyr_rec","georec","retro")

new <- bind_rows(xx0,xx1,xx2,xx3,xx4,xx5)
new$year <- c(2020:2015)
new$stf <- "new"

rm(xx0,xx1,xx2,xx3,xx4,xx5)


load("D:/ICES/IBPIS2021/SS_runs/SetupaSDQTune2/retrospectives/retro0/Take2_retro-0.RData")
load("D:/ICES/IBPIS2021/SS_runs/SetupaSDQTune2/retrospectives/retro-1/Take2_retro-1.RData")
load("D:/ICES/IBPIS2021/SS_runs/SetupaSDQTune2/retrospectives/retro-2/Take2_retro-2.RData")
load("D:/ICES/IBPIS2021/SS_runs/SetupaSDQTune2/retrospectives/retro-3/Take2_retro-3.RData")
load("D:/ICES/IBPIS2021/SS_runs/SetupaSDQTune2/retrospectives/retro-4/Take2_retro-4.RData")
load("D:/ICES/IBPIS2021/SS_runs/SetupaSDQTune2/retrospectives/retro-5/Take2_retro-5.RData")

xx0 <- xx0[,c(1,4,11:13)]
names(xx0) <- c("B1y","B1y+1","endyr_rec","georec","retro")
xx1 <- xx1[,c(1,4,11:13)]
names(xx1) <- c("B1y","B1y+1","endyr_rec","georec","retro")
xx2 <- xx2[,c(1,4,11:13)]
names(xx2) <- c("B1y","B1y+1","endyr_rec","georec","retro")
xx3 <- xx3[,c(1,4,11:13)]
names(xx3) <- c("B1y","B1y+1","endyr_rec","georec","retro")
xx4 <- xx4[,c(1,4,11:13)]
names(xx4) <- c("B1y","B1y+1","endyr_rec","georec","retro")
xx5 <- xx5[,c(1,4,11:13)]
names(xx5) <- c("B1y","B1y+1","endyr_rec","georec","retro")

old <- bind_rows(xx0,xx1,xx2,xx3,xx4,xx5)
old$year <- c(2020:2015)
old$stf <- "old"

rm(xx0,xx1,xx2,xx3,xx4,xx5)

stf<-bind_rows(old,new)

rm(old,new)

dd <- stf%>%
  group_by(stf)%>%
  summarise(
    rmse_B1=sqrt(mean((lag(B1y)-`B1y+1`)^2,na.rm=T)),
    rmse_Rec=sqrt(mean((lag(endyr_rec)-georec)^2,na.rm=T)))


