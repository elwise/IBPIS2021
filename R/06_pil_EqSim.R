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

setwd('D:/ICES/IBPIS2021/SS_runs/SetupaSDQTune2')
load("pil.stock.RData")

#==============================================================================
# LOAD LIBRARIES                                                           ----
#==============================================================================

library(FLCore)
library(msy)

#==============================================================================
# Estimate Reference points in EqSim (with the 2020 assessment model)      ----
#==============================================================================

setwd("D:/IPMA/SARDINE/CAS/2020/regimeProductividade")
load("pil.stock.RData")
mat(pil) <- c(0,1,1,1,1,1,1)

#Calculate for the Short Series with fixed Blim
### Get Blim and Bloss
stk <- window(pil, start=2006, end=2019)
# set age0 stock.wt close to 0 needed to get things running
stock.wt(stk)[1] <- rep (0.00000001, ncol(stock.wt(stk)))
Blim <- 196334
Bpa <- 252523
sigmaB <- 0.153

### Define new stock recruit relationships
## determine segreg model with Blim breakpoint and (roughly) geomean rec above this
SegregBlim  <- function(ab, ssb) log(ifelse(ssb >= Blim, ab$a * Blim, ab$a * ssb))


noSims <- 1000

### Create EQSIM SRR fits
FIT_segregBlim <- eqsr_fit(stk,nsamp=noSims, models = "SegregBlim")

### Run EQSIM with the FIT_segregBlim

set.seed(1919)
sim_06_19_fixedBlim <- eqsim_run (FIT_segregBlim, # choose SR-fit
                                  bio.years = c(2014, 2019), # years to generate noise in mat,M and wi
                                  bio.const = FALSE,  # average maturity used (TRUE)
                                  sel.years = c(2014, 2019), # noise in selection at age
                                  sel.const = FALSE, # average selection used (TRUE)
                                  Fscan = seq(0, 2, by = 0.01), # range of F values tested
                                  Blim = Blim , # set BRP?s
                                  Bpa = Bpa, # set BRP?s
                                  Nrun = 200)

set.seed(1919)
sim_06_19_cv_fixedBlim <- eqsim_run (FIT_segregBlim, # choose SR-fit
                                     bio.years = c(2014, 2019), # years to generate noise in mat,M and wi
                                     bio.const = FALSE,  # average maturity used (TRUE)
                                     sel.years = c(2014, 2019), # noise in selection at age
                                     sel.const = FALSE, # average selection used (TRUE)
                                     Fscan = seq(0, 2, by = 0.01), # range of F values tested
                                     Blim = Blim , # set BRP?s
                                     Bpa = Bpa, # set BRP?s
                                     Nrun = 200,
                                     Fcv = 0.058 , # F CV assessment error last year
                                     Fphi = 0.423 , # autocorrelation in F assessment error; default value
                                     SSBcv = sigmaB)

set.seed(1919)
sim_06_19_trigger_fixedBlim <- eqsim_run (FIT_segregBlim, # choose SR-fit
                                          bio.years = c(2014, 2019), # years to generate noise in mat,M and wi
                                          bio.const = FALSE,  # average maturity used (TRUE)
                                          sel.years = c(2014, 2019), # noise in selection at age
                                          sel.const = FALSE, # average selection used (TRUE)
                                          Fscan = seq(0, 2, by = 0.01), # range of F values tested
                                          Blim = Blim , # set BRP?s
                                          Bpa = Bpa, # set BRP?s
                                          Nrun = 200,
                                          Fcv = 0.058 , # F CV assessment error last year
                                          Fphi = 0.423 , # autocorrelation in F assessment error; default value
                                          SSBcv = sigmaB,
                                          Btrigger = Bpa # apply HCR (to check precautionarity)
)

t(sim_06_19_trigger_fixedBlim$Refs)

rm(pil)

#==============================================================================
# Estimate Reference points in EqSim (with Setup ASDQTune2)                ----
#==============================================================================

setwd('D:/ICES/IBPIS2021/SS_runs/SetupaSDQTune2')
load("pil.stock.RData")
pil <- pil.stock
mat(pil) <- c(0,1,1,1,1,1,1)

Blim <- 196334
Bpa <- 252523
#sigmaB <- 0.163 #with data up until 2020
sigmaB <- 0.169 #with data up until 2019

noSims <- 1000

#Calculate for the Short Series with fixed Blim
### Get Blim and Bloss
#stk <- window(pil, start=2006, end=2020)
stk <- window(pil, start=2006, end=2019)
# set age0 stock.wt close to 0 needed to get things running
stock.wt(stk)[1] <- rep (0.00000001, ncol(stock.wt(stk)))

### Define new stock recruit relationships
## determine segreg model with Blim breakpoint and (roughly) geomean rec above this
SegregBlim  <- function(ab, ssb) log(ifelse(ssb >= Blim, ab$a * Blim, ab$a * ssb))


### Create EQSIM SRR fits
FIT_segreg2020 <- eqsr_fit(stk,nsamp=noSims, models = "SegregBlim") #with Blim fixed
FIT_segreg <- eqsr_fit(stk,nsamp=noSims,models = "Segreg")
#Blim if not fixed:
FIT_segreg$sr.det$b
#CI of Blim if not fixed
paste0(round(quantile(FIT_segreg$sr.sto$b,0.025),0),' - ',round(quantile(FIT_segreg$sr.sto$b,0.975),0))

#Plots
eqsr_plot(FIT_segreg)
eqsr_plot(FIT_segreg2020)

### Run EQSIM with the FIT_segregBlim

set.seed(1919)
sim_06_20_fixedBlim <- eqsim_run (FIT_segreg2020, # choose SR-fit
                                  bio.years = c(2014, 2019), # years to generate noise in mat,M and wi
                                  bio.const = FALSE,  # average maturity used (TRUE)
                                  sel.years = c(2014, 2019), # noise in selection at age
                                  sel.const = FALSE, # average selection used (TRUE)
                                  Fscan = seq(0, 2, by = 0.01), # range of F values tested
                                  Blim = Blim , # set BRP?s
                                  Bpa = Bpa, # set BRP?s
                                  Nrun = 200)

set.seed(1919)
sim_06_20_cv_fixedBlim <- eqsim_run (FIT_segreg2020, # choose SR-fit
                                     bio.years = c(2014, 2019), # years to generate noise in mat,M and wi
                                     bio.const = FALSE,  # average maturity used (TRUE)
                                     sel.years = c(2014, 2019), # noise in selection at age
                                     sel.const = FALSE, # average selection used (TRUE)
                                     Fscan = seq(0, 2, by = 0.01), # range of F values tested
                                     Blim = Blim , # set BRP?s
                                     Bpa = Bpa, # set BRP?s
                                     Nrun = 200,
                                     Fcv = 0.058 , # F CV assessment error last year
                                     Fphi = 0.423 , # autocorrelation in F assessment error; default value
                                     SSBcv = sigmaB)

set.seed(1919)
sim_06_20_trigger_fixedBlim <- eqsim_run (FIT_segreg2020, # choose SR-fit
                                          bio.years = c(2014, 2019), # years to generate noise in mat,M and wi
                                          bio.const = FALSE,  # average maturity used (TRUE)
                                          sel.years = c(2014, 2019), # noise in selection at age
                                          sel.const = FALSE, # average selection used (TRUE)
                                          Fscan = seq(0, 2, by = 0.01), # range of F values tested
                                          Blim = Blim , # set BRP?s
                                          Bpa = Bpa, # set BRP?s
                                          Nrun = 200,
                                          Fcv = 0.058 , # F CV assessment error last year
                                          Fphi = 0.423 , # autocorrelation in F assessment error; default value
                                          SSBcv = sigmaB,
                                          Btrigger = Bpa # apply HCR (to check precautionarity)
)

t(sim_06_20_fixedBlim$Refs)
t(sim_06_20_cv_fixedBlim$Refs)
t(sim_06_20_trigger_fixedBlim$Refs)

### Run EQSIM with the FIT_segregBlim

set.seed(1919)
sim_06_21 <- eqsim_run (FIT_segreg2020, # choose SR-fit
                                  bio.years = c(2014, 2019), # years to generate noise in mat,M and wi
                                  bio.const = FALSE,  # average maturity used (TRUE)
                                  sel.years = c(2014, 2019), # noise in selection at age
                                  sel.const = FALSE, # average selection used (TRUE)
                                  Fscan = seq(0, 2, by = 0.01), # range of F values tested
                                  Blim = Blim , # set BRP?s
                                  Bpa = Bpa, # set BRP?s
                                  Nrun = 200)

set.seed(1919)
sim_06_20_cv_fixedBlim <- eqsim_run (FIT_segreg2020, # choose SR-fit
                                     bio.years = c(2014, 2019), # years to generate noise in mat,M and wi
                                     bio.const = FALSE,  # average maturity used (TRUE)
                                     sel.years = c(2014, 2019), # noise in selection at age
                                     sel.const = FALSE, # average selection used (TRUE)
                                     Fscan = seq(0, 2, by = 0.01), # range of F values tested
                                     Blim = Blim , # set BRP?s
                                     Bpa = Bpa, # set BRP?s
                                     Nrun = 200,
                                     Fcv = 0.058 , # F CV assessment error last year
                                     Fphi = 0.423 , # autocorrelation in F assessment error; default value
                                     SSBcv = sigmaB)

set.seed(1919)
sim_06_20_trigger_fixedBlim <- eqsim_run (FIT_segreg2020, # choose SR-fit
                                          bio.years = c(2014, 2019), # years to generate noise in mat,M and wi
                                          bio.const = FALSE,  # average maturity used (TRUE)
                                          sel.years = c(2014, 2019), # noise in selection at age
                                          sel.const = FALSE, # average selection used (TRUE)
                                          Fscan = seq(0, 2, by = 0.01), # range of F values tested
                                          Blim = Blim , # set BRP?s
                                          Bpa = Bpa, # set BRP?s
                                          Nrun = 200,
                                          Fcv = 0.058 , # F CV assessment error last year
                                          Fphi = 0.423 , # autocorrelation in F assessment error; default value
                                          SSBcv = sigmaB,
                                          Btrigger = Bpa # apply HCR (to check precautionarity)
)

t(sim_06_20_fixedBlim$Refs)
t(sim_06_20_cv_fixedBlim$Refs)
t(sim_06_20_trigger_fixedBlim$Refs)
