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

# directory with data
res.dir  <- file.path("./data")

#==============================================================================
# LOAD LIBRARIES                                                           ----
#==============================================================================

library(r4ss)
library(tidyverse)

#==============================================================================
#  Read SS runs for the different settings                                 ----
#==============================================================================

# The original 2020 assessment model
run2020 <- SS_output(dir = paste0(res.ss,"/2020_Update"),forecast=FALSE,ncols=62,verbose = TRUE, printstats = TRUE)

# The 2020 assessment model with the 2020 DEPM point estimate (was not available at the time)
run2020.DEPM <- SS_output(dir = paste0(res.ss,"/2020_DEPM"),forecast=FALSE,ncols=62,verbose = TRUE, printstats = TRUE)

# Setup a - Same settings as the current model but with new recruitment index
runa <- SS_output(dir = paste0(res.ss,'/Setupa'),forecast=FALSE,ncols=62,verbose = TRUE, printstats = TRUE)

# Setup a - Same settings as the current model but with different recruitment index estimates (years and areas)
run1 <-  SS_output(dir = paste0(res.ss,'/run1'),forecast=FALSE,ncols=62,verbose = TRUE, printstats = TRUE) # w/o ECO-REC all years
run2 <-  SS_output(dir = paste0(res.ss,'/run2'),forecast=FALSE,ncols=62,verbose = TRUE, printstats = TRUE) # 9aCN and 9aCS all years
run3 <-  SS_output(dir = paste0(res.ss,'/run3'),forecast=FALSE,ncols=62,verbose = TRUE, printstats = TRUE) # 9aCN all years
run4 <-  SS_output(dir = paste0(res.ss,'/run4'),forecast=FALSE,ncols=62,verbose = TRUE, printstats = TRUE) # w/o ECO-REC from 1997
run5 <-  SS_output(dir = paste0(res.ss,'/run5'),forecast=FALSE,ncols=62,verbose = TRUE, printstats = TRUE) # 9aCN and 9aCS from 1997
run6 <-  SS_output(dir = paste0(res.ss,'/run6'),forecast=FALSE,ncols=62,verbose = TRUE, printstats = TRUE) # 9aCN all years
run7 <-  SS_output(dir = paste0(res.ss,'/run7'),forecast=FALSE,ncols=62,verbose = TRUE, printstats = TRUE) # w/o ECO-REC from 2013
run8 <-  SS_output(dir = paste0(res.ss,'/run8'),forecast=FALSE,ncols=62,verbose = TRUE, printstats = TRUE) # 9aCN and 9aCS from 1997
run9 <-  SS_output(dir = paste0(res.ss,'/run9'),forecast=FALSE,ncols=62,verbose = TRUE, printstats = TRUE) # 9aCN all years
run10 <- SS_output(dir = paste0(res.ss,'/run10'),forecast=FALSE,ncols=62,verbose = TRUE, printstats = TRUE) # all areas and years
run11 <- SS_output(dir = paste0(res.ss,'/run11'),forecast=FALSE,ncols=62,verbose = TRUE, printstats = TRUE) # all areas from 1997
run12 <- SS_output(dir = paste0(res.ss,'/run12'),forecast=FALSE,ncols=62,verbose = TRUE, printstats = TRUE) # all areas from 2013

# Setup b - Different selectivity, no time blocks, S-at-age is not dome shaped
runb <- SS_output(dir = paste0(res.ss,'/Setupb'),forecast=FALSE,ncols=62,verbose = TRUE, printstats = TRUE)

# Setup c - Different selectivity, no time blocks, S-at-age is dome shaped
runc <- SS_output(dir = paste0(res.ss,'/Setupc'),forecast=FALSE,ncols=62,verbose = TRUE, printstats = TRUE)

# Make SS plots for each run
#SS_plots(runa)
#SS_plots(runb)
#SS_plots(runc)
#SS_plots(run2020.DEPM)


#==============================================================================
#  Compare models with same settings but different recruitment input data  ----
#==============================================================================
cpl <- list(run10,run1,run2,run3,run11,run4,run5,run6,run12,run7,run8,run9)
cpl.sum <- SSsummarize(biglist=cpl)
SSplotComparisons(summaryoutput=cpl.sum,subplots = c(2,8,10,12),xlim=c(1978,2021),print = TRUE,plotdir = res.dir)
SSplotComparisons(summaryoutput=cpl.sum,subplots = c(13),indexfleets = c(2),xlim=c(1978,2021),print = TRUE,plotdir = res.dir)
SSplotComparisons(summaryoutput=cpl.sum,subplots = c(13),indexfleets = c(3),xlim=c(1978,2021),print = TRUE,plotdir = res.dir)
SSplotComparisons(summaryoutput=cpl.sum,subplots = c(13),indexfleets = c(4),xlim=c(1978,2021),print = TRUE,plotdir = res.dir)

xx <- SStableComparisons(cpl.sum,models = "all",likenames = c("TOTAL","Survey", "Age_comp"),
                         names = c("SSB_2020","Recr_2020","F_2019"), digits = rep(2,16),
                         verbose = TRUE,mcmc = FALSE)
xx <- rbind(xx,c("Maximum Gradient",cpl.sum$maxgrad))
xx <- rbind(xx,cpl.sum$likelihoods[c(2,3,6,9,10),c(ncol(cpl.sum$likelihoods),1:(ncol(cpl.sum$likelihoods)-1))])
xx <- rbind(xx,c("Number parameters",cpl.sum$npars))
AIC <- c(Label="AIC", cpl.sum$npars*2+(2*cpl.sum$likelihoods[1,-ncol(cpl.sum$likelihoods)]))
xx <- rbind(xx,AIC)
xx <- xx[c(1,8,9,2,3,10:13,4:6,14,7),]
xx$Label <- c("Total","Catch","Equil_catch","Survey","Age_comp","Recruitment","Parm_softbounds","Parm_devs","N parm","SSB_2020",
              "Recr_2020","F_2019","AIC","Max Grad")

xx%>%
  gt::gt()%>%
  gt::gtsave("tblCompModelAreas.tex")

#pearson residuals plots
run2020$agedbase$Model <- rep("2020 assm",dim(run2020$agedbase)[1])
run2020.DEPM$agedbase$Model <- rep("2020 DEPM", dim(run2020.DEPM$agedbase)[1])
runa$agedbase$Model <- rep("Setup A",dim(runa$agedbase)[1])
runb$agedbase$Model <- rep("Setup B",dim(runb$agedbase)[1])
runc$agedbase$Model <- rep("Setup C",dim(runc$agedbase)[1])
PearsonRes <- bind_rows(run2020$agedbase,run2020.DEPM$agedbase,runa$agedbase,runb$agedbase,runc$agedbase)

ggplot(subset(PearsonRes,!(Fleet==2 & Bin==0)), aes(Pearson,colour=Model))+
  geom_density()+
  facet_grid(Fleet~Bin, labeller=label_both)+
  ylab("Density")+
  xlab("Pearson residuals for age composition")+
  theme(legend.position = "bottom")
ggsave(paste0(res.plots,'/',"PearsonDensity.png"))

#==============================================================================
#  Compare models with same input data but different selectivity settings  ----
#==============================================================================

cpl <- list(run2020,run2020.DEPM,runa,runb,runc)
cpl.sum <- SSsummarize(biglist=cpl)
SSplotComparisons(summaryoutput=cpl.sum,subplots = c(2,8,10,12),xlim=c(1978,2021),print = TRUE,plotdir = res.plots)
SSplotComparisons(summaryoutput=cpl.sum,subplots = c(13),indexfleets = c(2),xlim=c(1978,2021),print = TRUE,plotdir = res.plots)
SSplotComparisons(summaryoutput=cpl.sum,subplots = c(13),indexfleets = c(3),xlim=c(1978,2021),print = TRUE,plotdir = res.plots)
SSplotComparisons(summaryoutput=cpl.sum,subplots = c(13),indexfleets = c(4),xlim=c(1978,2021),print = TRUE,plotdir = res.plots)

xx <- SStableComparisons(cpl.sum,models = "all",likenames = c("TOTAL","Survey", "Age_comp"),
                         names = c("SSB_2020","Recr_2020","F_2019"), digits = rep(2,16),
                         verbose = TRUE,mcmc = FALSE)
xx <- rbind(xx,c("Maximum Gradient",cpl.sum$maxgrad))
xx <- rbind(xx,cpl.sum$likelihoods[c(2,3,6,9,10),c(ncol(cpl.sum$likelihoods),1:(ncol(cpl.sum$likelihoods)-1))])
xx <- rbind(xx,c("Number parameters",cpl.sum$npars))
AIC <- c(Label="AIC", cpl.sum$npars*2+(2*cpl.sum$likelihoods[1,-ncol(cpl.sum$likelihoods)]))
xx <- rbind(xx,AIC)
xx <- xx[c(1,8,9,2,3,10:13,4:6,14,7),]
xx$Label <- c("Total","Catch","Equil_catch","Survey","Age_comp","Recruitment","Parm_softbounds","Parm_devs","N parm","SSB_2020",
               "Recr_2020","F_2019","AIC","Max Grad")
colnames(xx) <- c("Label", "Assm2020", "AssmDEPM","SetupA","SetupB","SetupC")

xx%>%
  gt::gt()%>%
  gt::gtsave("tblCompModel2020.tex")


sso.runa <- SS_read_summary(file = "D:/ICES/IBPIS2021/SS_runs/Setupa/ss_summary.sso",verbose = TRUE)
sso.dqa <- sso.runa$derived_quants[1:219,] %>% rownames_to_column("rn") %>% separate(rn, into = c("Variable", "Yr"))
sso.dqa$Run <- "Setupa"
sso.runb <- SS_read_summary(file = "D:/ICES/IBPIS2021/SS_runs/Setupb/ss_summary.sso",verbose = TRUE)
sso.dqb <- sso.runb$derived_quants[1:219,] %>% rownames_to_column("rn") %>% separate(rn, into = c("Variable", "Yr"))
sso.dqb$Run <- "Setupb"
sso.runc <- SS_read_summary(file = "D:/ICES/IBPIS2021/SS_runs/Setupc/ss_summary.sso",verbose = TRUE)
sso.dqc <- sso.runc$derived_quants[1:219,] %>% rownames_to_column("rn") %>% separate(rn, into = c("Variable", "Yr"))
sso.dqc$Run <- "Setupc"
sso.2020 <- SS_read_summary(file = "D:/ICES/IBPIS2021/SS_runs/2020_Update/ss_summary.sso",verbose = TRUE)
sso.dq2020 <- sso.2020$derived_quants[1:219,] %>% rownames_to_column("rn") %>% separate(rn, into = c("Variable", "Yr"))
sso.dq2020$Run <- "Assm2020"
sso.DEPM <- SS_read_summary(file = "D:/ICES/IBPIS2021/SS_runs/2020_DEPM/ss_summary.sso",verbose = TRUE)
sso.dqDEPM <- sso.DEPM$derived_quants[1:219,] %>% rownames_to_column("rn") %>% separate(rn, into = c("Variable", "Yr"))
sso.dqDEPM$Run <- "DEPM2020"

  
ssos <- 

full_join(
  sso.dq2020 %>%
  pivot_longer(cols = c(Value, SE), names_to = "Type") %>%
  select(Variable, Yr, Type, Ass2020 = "value"),
  bind_rows(sso.dq2020,sso.dqDEPM,sso.dqa,sso.dqb,sso.dqc) %>%
  pivot_longer(cols = c(Value, SE), names_to = "Type"),
  by = c("Variable", "Yr", "Type")
) %>%
  mutate(delta = Ass2020 - value)

rm(list = ls(pattern = "^sso\\."))

ssos %>%
  filter(Yr %in% 1978:2020,Type == "Value", Variable %in% c('SSB','Recr','F')) %>%
  mutate(Yr = as.numeric(as.character(Yr))) %>%
  ggplot(aes(x=Yr,y=delta,color=Run))+
  geom_line()+
  facet_wrap(Variable ~ .,scale="free_y",nrow=3)
ggsave(paste0(res.plots,'/',"DiffBetweenSetups.png"))

ssos %>%
  filter(Yr %in% 1978:2020,Type == "SE", Variable %in% c('SSB','Recr','F')) %>%
  mutate(Yr = as.numeric(as.character(Yr))) %>%
  ggplot(aes(x=Yr,y=delta,color=Run))+
  geom_line()+
  facet_wrap(Variable ~ .,scale="free_y",nrow=3)

ssos %>% 
  group_by(Variable, Type, Run)%>% 
  summarise (mean= mean(value), sd=sd(value))%>%
  filter(Variable %in% c("SSB","Recr","F"))


#==============================================================================
#  Check percentage of catch that comes from recruitment areas             ----
#==============================================================================

ctArea <- read.csv(file = paste0(res.dir,"/CathByArea.csv"))
colnames(ctArea) <- c('Year','8c','9aN','9aCN','9aCS','9aS-Alg','9aS-Cad')
Recruits <- run2020$timeseries$Recruit_0[3:length(run2020$timeseries$Recruit_0)]

ctArea%>%pivot_longer(cols=-Year, names_to = 'Area',values_to = 'Numbers',)%>%
mutate(RecArea = ifelse(Area %in% c('9aN','9aCN','9aS-Cad'),TRUE,FALSE))%>%
group_by(Year,RecArea)%>%
summarise(n=sum(Numbers))%>%
group_by(Year)%>%
mutate(rel=n/sum(n))%>%
ggplot(aes(fill=RecArea, y=rel, x=Year)) +
geom_bar(position="fill", stat="identity")+
geom_vline(xintercept = c(1987.5,2006.5),linetype="dashed") +
geom_hline(yintercept = 0.5,linetype="dashed")
ggsave(paste0(res.plots,'/',"CatchByArea.png"))

ctArea%>%pivot_longer(cols=-Year, names_to = 'Area',values_to = 'Numbers',)%>%
  mutate(RecArea = ifelse(Area %in% c('9aN','9aCN','9aS-Cad'),TRUE,FALSE))%>%
  group_by(Year,RecArea)%>%
  summarise(n=sum(Numbers))%>%
  group_by(Year)%>%
  mutate(rel=n/sum(n))%>%
  filter(RecArea=='TRUE')%>%
  add_column(Recruits)%>%
  pivot_longer(cols=c(rel,Recruits),names_to='Variable',values_to='Value')%>%
  ggplot(aes( y=Value, x=Year)) + 
  geom_line()+
  facet_grid(Variable~. ,scales = 'free_y')+
  geom_vline(xintercept = c(1987.5,2006.5))
ggsave(paste0(res.plots,'/',"RecruitsVsCatchByArea.png"))


#======================================================================================
#  Compare recruitment estimates from stock assessment model with survey estimates ----
#======================================================================================


f.cor <- function(run,run.base){

rec <- run$recruit[,c(1,6)]
rec <- rec%>%
  mutate(geoMean=exp(zoo::rollmean(log(pred_recr),k = 5, align = "right", fill = NA)))
surv <- run$cpue[run$cpue$Fleet==4,c(4,10)]
rec <- merge(rec,surv,by="Yr")

base <- run.base$recruit[run.base$recruit$Yr %in% rec$Yr,c(1,6)]

# ggpubr::ggscatter(data=rec,x = "Obs", y = "pred_recr",
#           add = "reg.line",  # Add regressin line
#           add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
#           conf.int = TRUE, # Add confidence interval
#           fullrange = TRUE, # Extending the regression line
#           rug = TRUE   # Add marginal rug
# )+DEGreport::geom_cor(method = "pearson")+
#   geom_text(aes(label=Yr),hjust=0, vjust=0,size=3) +
#   #theme_ipsum() +
#   ylab(latex2exp::TeX('$Number_{a=0,y,assess}$')) +
#   xlab(latex2exp::TeX('$Number_{a=0,y,survey}$'))+
#   theme(legend.position = "none") 
#   theme(plot.title = element_text(size = 12, face = "bold"))
# ggsave(paste0(res.plots,'/',"Survey-base recruits total vs Assessment recruits estimate.png"))



r1 <- cor.test(log(rec$Obs),log(rec$pred_recr))# comparar estimativa cruzeiro com estimativa modelo
r2 <- cor.test(log(rec$Obs),log(base$pred_recr))# comparar estimativa cruzeiro com estimativa do modelo sem survey
r3 <- cor.test(log(rec$Obs),log(rec$geoMean))# comparar estimativa cruzeiro com média geométrica

tbl.cor <- rbind(r1,r2,r3)

return(tbl.cor)

}

cor.Setupa <- f.cor(runa,run2020)
cor.Setupb <- f.cor(runb,run2020)
cor.Setupc <- f.cor(runc,run2020)

