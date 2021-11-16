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


########################################
##HS plot with both functions in the same plot
##########################################

# Segmented regression in WKSARMPHCR (inflexion point at Blim from WKSARMP 2019)
setwd("D:/IPMA/SARDINE/CAS/2020/regimeProductividade")
load("pil.stock.RData")
mat(pil) <- c(0,1,1,1,1,1,1)

max.ssb <- max(max(ssb(pil)),max(ssb(pil.stock)))

#Calculate for the Short Series with fixed Blim
### Get Blim and Bloss
stk <- window(pil, start=2006, end=2019)

SR <- as.FLSR(stk)
newssb <- FLQuant(seq(0, max.ssb, length.out=20000))

model(SR) <- segreg()
#params for sr low regime with 2019 data point
yrs.srlow <- 2006:2019
params_low <- fmle(window(SR, start=2006, end=2019), fixed=list(b=196334))@params

pilsr <- fmle(SR, fixed=list(a=c(params_low)[1],b=196334))
segrec <- predict(pilsr, ssb = newssb)
residsd_low <- sqrt(var(log(exp(residuals(pilsr)[,ac(yrs.srlow)])),na.rm=T))
res <- exp(rnorm(20000, 0, residsd_low))

#Calculate for the Short Series with fixed Blim
### Get Blim and Bloss
stk4 <- window(pil, start=2006, end=2019)

SR4 <- as.FLSR(stk4)
newssb4 <- FLQuant(seq(0, max(pil@stock), length.out=20000))

model(SR4) <- segreg()
#params for sr low regime with 2019 data point

params_low4 <- fmle(window(SR4, start=2006, end=2019))@params

pilsr4 <- fmle(SR4)
segrec4 <- predict(pilsr4, ssb = newssb4)
residsd_low4 <- sqrt(var(log(exp(residuals(pilsr4)[,ac(yrs.srlow)])),na.rm=T))
res4 <- exp(rnorm(20000, 0, residsd_low4))

# Segmented regression in IBPIS (inflexion point at Blim from WKSARMP 2019)
setwd('D:/ICES/IBPIS2021/SS_runs/SetupaSDQTune2')
load("pil.stock.RData")
mat(pil.stock) <- c(0,1,1,1,1,1,1)

#Calculate for the Short Series with fixed Blim
### Get Blim and Bloss
stk2 <- window(pil.stock, start=2006, end=2020)

SR2 <- as.FLSR(stk2)
newssb2 <- FLQuant(seq(0, max(pil@stock), length.out=20000))

model(SR2) <- segreg()
#params for sr low regime with 2019 data point
params_low2 <- fmle(window(SR2, start=2006, end=2020), fixed=list(b=196334))@params

pilsr2 <- fmle(SR2, fixed=list(a=c(params_low2)[1],b=196334))
segrec2 <- predict(pilsr2, ssb = newssb2)
residsd_low2 <- sqrt(var(log(exp(residuals(pilsr2)[,ac(yrs.srlow)])),na.rm=T))
res2 <- exp(rnorm(20000, 0, residsd_low2))

# Segmented regression in IBPIS (without inflexion point at Blim from WKSARMP 2019)

#Calculate for the Short Series with fixed Blim
### Get Blim and Bloss
stk3 <- window(pil.stock, start=2006, end=2020)

SR3 <- as.FLSR(stk3)
newssb3 <- FLQuant(seq(0, max(pil@stock), length.out=20000))

model(SR3) <- segreg()
#params for sr low regime with 2019 data point
params_low3 <- fmle(window(SR3, start=2006, end=2020))@params

pilsr3 <- fmle(SR3)
segrec3 <- predict(pilsr3, ssb = newssb3)
residsd_low3 <- sqrt(var(log(exp(residuals(pilsr3)[,ac(yrs.srlow)])),na.rm=T))
res3 <- exp(rnorm(20000, 0, residsd_low3))

# comparison of two recruitment scenarios:

dat1 <- data.frame(
  ssb=seq(0, max(pil@stock), length.out=20000), 
  rec=c(segrec),
  low=c(segrec)*qlnorm(0.025, 0, residsd_low),
  up=c(segrec)*qlnorm(0.975, 0, residsd_low), 
  wk="WKSARHCR",
  Blim = "fixed")

dat2 <- data.frame(
  ssb=seq(0, max(pil@stock), length.out=20000), 
  rec=c(segrec2),
  low=c(segrec2)*qlnorm(0.025, 0, residsd_low2),
  up=c(segrec2)*qlnorm(0.975, 0, residsd_low2), 
  wk="IBPIS",
  Blim = "fixed")

dat3 <- data.frame(
  ssb=seq(0, max(pil@stock), length.out=20000), 
  rec=c(segrec3),
  low=c(segrec3)*qlnorm(0.025, 0, residsd_low3),
  up=c(segrec3)*qlnorm(0.975, 0, residsd_low3), 
  wk="IBPIS",
  Blim = " not fixed")

dat4 <- data.frame(
  ssb=seq(0, max(pil@stock), length.out=20000), 
  rec=c(segrec4),
  low=c(segrec4)*qlnorm(0.025, 0, residsd_low4),
  up=c(segrec4)*qlnorm(0.975, 0, residsd_low4), 
  wk="WKSARHCR",
  Blim = " not fixed")

dat <- rbind(dat1, dat2,dat3, dat4)
dat[,2:4] <- dat[,2:4]/1000000


ggplot(dat, aes(ssb, rec, fill=wk, col=wk))+
  geom_line(lwd=1)+
  geom_ribbon(aes(ssb, ymin=low, ymax=up), alpha=0.3, linetype="dashed")+
  xlab("Spawning stock biomass")+
  ylab("Recruits (billion)")+ facet_grid(Blim~.,scales="free_y")+
  theme(legend.position = "bottom")+
  geom_point(data=data.frame(ssb=c(ssb(pil.stock)), rec=c(rec(pil.stock)/1000000), year=dimnames(pil.stock)$year), aes(ssb, rec), inherit.aes = F)+
  geom_text(data=data.frame(ssb=c(ssb(pil.stock)), rec=c(rec(pil.stock)/1000000), year=dimnames(pil.stock)$year), aes(ssb, rec,label=year), inherit.aes = F,vjust=-0.3,size=2)

ggplot(subset(dat,Blim == "fixed"), aes(ssb, rec, fill=wk, col=wk))+
  geom_line(lwd=1)+
  geom_ribbon(aes(ssb, ymin=low, ymax=up), alpha=0.3, linetype="dashed")+
  xlab("Spawning stock biomass")+
  ylab("Recruits (billion)")+
  theme(legend.position = "bottom") + ggtitle("Blim is fixed at 196 334 tonnes") +
  geom_point(data=data.frame(ssb=c(ssb(pil.stock)), rec=c(rec(pil.stock)/1000000), year=dimnames(pil.stock)$year), aes(ssb, rec), inherit.aes = F)+
  geom_text(data=data.frame(ssb=c(ssb(pil.stock)), rec=c(rec(pil.stock)/1000000), year=dimnames(pil.stock)$year), aes(ssb, rec,label=year), inherit.aes = F,vjust=-0.3,size=2)

ggplot(subset(dat,Blim == "fixed"), aes(ssb, rec, fill=wk, col=wk))+
  geom_line(lwd=1)+
  geom_ribbon(aes(ssb, ymin=low, ymax=up), alpha=0.3, linetype="dashed")+
  xlab("Spawning stock biomass")+
  ylab("Recruits (billion)")+
  theme(legend.position = "bottom")+
  xlim(c(0,300000))+ylim(c(0,20))+ggtitle("Blim is fixed at 196 334 tonnes")+
  geom_point(data=data.frame(ssb=c(ssb(pil.stock)), rec=c(rec(pil.stock)/1000000), year=dimnames(pil.stock)$year), aes(ssb, rec), inherit.aes = F)+
  geom_text(data=data.frame(ssb=c(ssb(pil.stock)), rec=c(rec(pil.stock)/1000000), year=dimnames(pil.stock)$year), aes(ssb, rec,label=year), inherit.aes = F,vjust=-0.3,size=2)

ggplot(subset(dat,Blim == " not fixed"), aes(ssb, rec, fill=wk, col=wk))+
  geom_line(lwd=1)+
  geom_ribbon(aes(ssb, ymin=low, ymax=up), alpha=0.3, linetype="dashed")+
  xlab("Spawning stock biomass")+
  ylab("Recruits (billion)")+
  theme(legend.position = "bottom")+ ggtitle("Blim is not fixed")+
  geom_point(data=data.frame(ssb=c(ssb(pil.stock)), rec=c(rec(pil.stock)/1000000), year=dimnames(pil.stock)$year), aes(ssb, rec), inherit.aes = F)+
  geom_text(data=data.frame(ssb=c(ssb(pil.stock)), rec=c(rec(pil.stock)/1000000), year=dimnames(pil.stock)$year), aes(ssb, rec,label=year), inherit.aes = F,vjust=-0.3,size=2)

ggplot(subset(dat,Blim == " not fixed"), aes(ssb, rec, fill=wk, col=wk))+
  geom_line(lwd=1)+
  geom_ribbon(aes(ssb, ymin=low, ymax=up), alpha=0.3, linetype="dashed")+
  xlab("Spawning stock biomass")+
  ylab("Recruits (billion)")+
  theme(legend.position = "bottom")+
  xlim(c(0,300000))+ylim(c(0,20))+ ggtitle("Blim is not fixed")+
  geom_point(data=data.frame(ssb=c(ssb(pil.stock)), rec=c(rec(pil.stock)/1000000), year=dimnames(pil.stock)$year), aes(ssb, rec), inherit.aes = F)+
  geom_text(data=data.frame(ssb=c(ssb(pil.stock)), rec=c(rec(pil.stock)/1000000), year=dimnames(pil.stock)$year), aes(ssb, rec,label=year), inherit.aes = F,vjust=-0.3,size=2)

