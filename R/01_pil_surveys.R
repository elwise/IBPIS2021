################################################################################
#                      IBPIS 2021 Survey data analysis                         # 
#------------------------------------------------------------------------------#
#                                                                              #
#   Laura Wise (IPMA)                                                          #
#   created:  18/10/2021                                                       #
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
res.plots<-"D:/ICES/IBPIS2021/plots"

# directory with data
res.dir  <- file.path("./data")

#==============================================================================
# LOAD LIBRARIES                                                           ----
#==============================================================================

library(tidyverse)
library(ggpubr) # for the ggscatter function
library(DEGreport) # for the geom_cor function
library(latex2exp) # for the function TeX

#==============================================================================
#  Read file with time series of survey data                               ----
#==============================================================================

srv <- read.table(paste0(res.dir,"/surveysData.csv"), header = TRUE,sep=",")


# Filter data set
srv <- srv %>%
  filter(.,!survey2 %in% c("PELGAS", "ECOCADIZ","SAR-PT-SUM"))%>%
  mutate(type = case_when(survey2 %in% c("SAR-PT-AUT","JUVESAR","ECOCADIZ-REC","IBERAS") ~ "REC",
                          TRUE~"B1plus"))%>%
  group_by(type,survey2,year,cohort,age,area)%>%
  summarise(number= sum(n,na.rm=T),
            biomass= sum(b,na.rm=T))%>%
  mutate(logN=log(number))


#==============================================================================
#  Basic plots to describe surveys                                         ----
#==============================================================================

## Point and line plots of total biomass and number per year

# - For all ages
srv %>%
  group_by(type,survey2,year)%>%
  summarise(number=sum(number,na.rm=T),
            biomass=sum(biomass,na.rm=T))%>%
  pivot_longer(.,cols=c(4,5),names_to = "estimate")%>%
  ggplot(aes(y=value,x=year,colour=survey2))+
  geom_point(alpha=0.4) + geom_line()+
  scale_size( range=c(.1,10),name="")+
  facet_grid(estimate ~ type, scales = 'free') +
  viridis::scale_fill_viridis(discrete=TRUE, guide=FALSE, option="A") +
  hrbrthemes::theme_ipsum() +
  theme(legend.position="bottom") +
  ylab("Total") +
  xlab("Year") 

# - For age zero
srv %>%
  filter(age==0 & type =="REC")%>%
  group_by(survey2,year)%>%
  summarise(number=sum(number,na.rm=T),
            biomass=sum(biomass,na.rm=T))%>%
  pivot_longer(.,cols=c(3:4),names_to = "estimate")%>%
  ggplot(aes(y=value,x=year,colour=survey2))+
  geom_point(alpha=0.4) + geom_line()+
  scale_size( range=c(.1,10),name="")+
  facet_grid(estimate ~ ., scales = 'free') +
  viridis::scale_fill_viridis(discrete=TRUE, guide=FALSE, option="A") +
  hrbrthemes::theme_ipsum() +
  theme(legend.position="bottom") +
  ylab("Age Zero") +
  xlab("Year") 
ggsave(paste0(res.plots,'/',"EstimatesAgeZeroRecruitmentsurveys.png"))

# - For age zero in npor (9aCN)
srv %>%
  filter(type== "REC" & age==0 & area=='npor')%>%
  group_by(survey2,year)%>%
  summarise(number=sum(number,na.rm=T),
            biomass=sum(biomass,na.rm=T))%>%
  pivot_longer(.,cols=c(3:4),names_to = "estimate")%>%
  ggplot(aes(y=value,x=year,colour=survey2))+
  geom_point(alpha=0.4) + geom_line()+
  scale_size( range=c(.1,10),name="")+
  facet_grid(estimate ~ ., scales = 'free') +
  viridis::scale_fill_viridis(discrete=TRUE, guide=FALSE, option="A") +
  hrbrthemes::theme_ipsum() +
  theme(legend.position="bottom") +
  ylab("Age Zero (9aCN)") +
  xlab("Year") 
#ggsave(paste0(res.plots,'/',"EstimatesAgeZero9aCN.png"))

## Proportion of each recruitment survey to the total estimate of recruitment
srv%>%
  filter(age==0 , survey2 %in% c("SAR-PT-AUT","ECOCADIZ-REC","JUVESAR","IBERAS"), area!='ngal')%>%
  group_by(year)%>%
  mutate(total= sum(number,na.rm=T))%>%
  group_by(year,area)%>%
  summarise(prop=number/total)%>%
  droplevels()%>%
  ggplot(aes(x=year,y=prop,fill=area))+
  geom_bar(stat='identity')+
  scale_fill_discrete(name="Area",
                      breaks=c("sgal","npor","swpor","spor","cad"),
                      labels=c("9aN","9aCN","9aCS","9aS-Algarve","9aS-Cadiz"))+
  xlab('Year')+ylab('Proportion (%)')

#==============================================================================
# Check correlation between autumn survey-based recruitment estimate (Age zero) 
#   with spring survey-based estimate of Age 1                              ---
#==============================================================================

# Create data.frame with age zero in rec surveys and age one in spring surveys
nZeroTot <- srv%>%
  filter(.,age==0 & type=="REC")%>%
  group_by(cohort)%>%
  arrange(.,year)%>%
  summarise(N=sum(number,na.rm=T),logN = log(N))

nZeroTotCadiz <- srv%>%
  filter(.,age==0 & type=="REC" & survey2!="ECOCADIZ-REC")%>%
  group_by(cohort)%>%
  arrange(.,year)%>%
  summarise(NER=sum(number,na.rm=T),logNER = log(NER))

nZeroTotCommon <- srv%>%
  filter(.,age==0 & type=="REC" & area %in% c("npor","swpor"))%>%
  group_by(cohort)%>%
  arrange(.,year)%>%
  summarise(NC=sum(number,na.rm=T),logNC = log(NC))

nZeroTotNpor <- srv%>%
  filter(.,age==0 & type=="REC" & area == "npor")%>%
  group_by(cohort)%>%
  arrange(.,year)%>%
  summarise(NCN=sum(number,na.rm=T),logNCN = log(NCN))

nZeroECOCADIZ <- srv%>%
  filter(.,age==0 & survey2=="ECOCADIZ-REC")%>%
  group_by(cohort)%>%
  arrange(.,year)%>%
  summarise(NEco=sum(number,na.rm=T),logNEco = log(NEco))

nOneTot <- srv%>%
  filter(.,type=="B1plus" & age==1)%>%
  filter(.,!year %in% c(2004,2012))%>% #remove years where PELAGO was not carried out
  group_by(cohort)%>%
  arrange(.,year)%>%
  summarise(N1=sum(number,na.rm=T),logN1 = log(N1))

ff <- as.data.frame(full_join(nZeroTot,nZeroTotCadiz,by=c("cohort"))%>%
                      full_join(.,nZeroTotCommon,by='cohort')%>%
                      full_join(.,nZeroTotNpor,by='cohort')%>%
                      full_join(.,nZeroECOCADIZ,by='cohort')%>%
                      full_join(.,nOneTot, by='cohort'))

remove(nZeroTot,nZeroTotCadiz,nZeroTotCommon,nZeroTotNpor,nOneTot)

### Pearson correlation plots

## - for all surveys

  # - N

ggscatter(data=ff,x = "N", y ="N1",
          add = "reg.line",  # Add regressin line
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE, # Add confidence interval
          fullrange = TRUE, # Extending the regression line
          rug = TRUE   # Add marginal rug
)+geom_cor(method = "pearson")+
  geom_text(aes(label=cohort),hjust=0, vjust=0,size=3) +
  hrbrthemes::theme_ipsum() +
  ylab(TeX('$Number_{a=1,y+1}$')) +
  xlab(TeX('$Number_{a=0,y}$'))+
  theme(legend.position = "none") +
  ggtitle("Age0 autumn estimate vs Age1 spring estimate")+
  theme(plot.title = element_text(size = 12, face = "bold"))

ggscatter(data=subset(ff,!cohort %in% c(2000)),x = "N", y ="N1", #remove the 2000 point to check if correlation still holds
          add = "reg.line",  # Add regressin line
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE, # Add confidence interval
          fullrange = TRUE, # Extending the regression line
          rug = TRUE   # Add marginal rug
)+geom_cor(method = "pearson")+
  geom_text(aes(label=cohort),hjust=0, vjust=0,size=3) +
  hrbrthemes::theme_ipsum() +
  ylab(TeX('$Number_{a=1,y+1}$')) +
  xlab(TeX('$Number_{a=0,y}$'))+
  theme(legend.position = "none") +
  ggtitle("Age0 autumn estimate vs Age1 spring estimate")+
  theme(plot.title = element_text(size = 12, face = "bold"))

  # - log(N)

ggscatter(data=ff,x = "logN", y ="logN1",
          add = "reg.line",  # Add regressin line
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE, # Add confidence interval
          fullrange = TRUE, # Extending the regression line
          rug = TRUE   # Add marginal rug
)+geom_cor(method = "pearson")+
  geom_text(aes(label=cohort),hjust=0, vjust=0,size=3) +
  hrbrthemes::theme_ipsum() +
  ylab(TeX('$log(Number_{a=1,y+1})$')) +
  xlab(TeX('$log(Number_{a=0,y})$'))+
  theme(legend.position = "none") +
  ggtitle("Age0 autumn estimate vs Age1 spring estimate")+
  theme(plot.title = element_text(size = 12, face = "bold"))
#ggsave(paste0(res.plots,'/',"Recruits[All] vs PELAGO+PELACUS.png"))

ggscatter(data=subset(ff,!cohort %in% c(2014)),x = "logN", y ="logN1", #remove the 2014 point to check if correlation still holds
          add = "reg.line",  # Add regressin line
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE, # Add confidence interval
          fullrange = TRUE, # Extending the regression line
          rug = TRUE   # Add marginal rug
)+geom_cor(method = "pearson")+
  geom_text(aes(label=cohort),hjust=0, vjust=0,size=3) +
  hrbrthemes::theme_ipsum() +
  ylab(TeX('$log(Number_{a=1,y+1})$')) +
  xlab(TeX('$log(Number_{a=0,y})$'))+
  theme(legend.position = "none") +
  ggtitle("Age0 autumn estimate vs Age1 spring estimate")+
  theme(plot.title = element_text(size = 12, face = "bold"))

  # subset for years > 1996

ggscatter(data=subset(ff,cohort>1996),x = "logN", y ="logN1",
          add = "reg.line",  # Add regressin line
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE, # Add confidence interval
          fullrange = TRUE, # Extending the regression line
          rug = TRUE   # Add marginal rug
)+geom_cor(method = "pearson")+
  geom_text(aes(label=cohort),hjust=0, vjust=0,size=3) +
  hrbrthemes::theme_ipsum() +
  ylab(TeX('$log(Number_{a=1,y+1})$')) +
  xlab(TeX('$log(Number_{a=0,y})$'))+
  theme(legend.position = "none") +
  ggtitle("After 1996, Age0 autumn estimate vs Age1 spring estimate")+
  theme(plot.title = element_text(size = 12, face = "bold"))
#ggsave(paste0(res.plots,'/',"Recruits[1996] vs PELAGO+PELACUS.png"))

# subset for years > 2012

ggscatter(data=subset(ff,cohort>2012),x = "logN", y ="logN1",
          add = "reg.line",  # Add regressin line
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE, # Add confidence interval
          fullrange = TRUE, # Extending the regression line
          rug = TRUE   # Add marginal rug
)+geom_cor(method = "pearson")+
  geom_text(aes(label=cohort),hjust=0, vjust=0,size=3) +
  hrbrthemes::theme_ipsum() +
  ylab(TeX('$log(Number_{a=1,y+1})$')) +
  xlab(TeX('$log(Number_{a=0,y})$'))+
  theme(legend.position = "none") +
  ggtitle("After 2012, Age0 autumn estimate vs Age1 spring estimate")+
  theme(plot.title = element_text(size = 12, face = "bold"))
#ggsave(paste0(res.plots,'/',"Recruits[2012] vs PELAGO+PELACUS.png"))


## - for all surveys except ECOCADIZ-RECLUTAS

 # - Log(N)

ggscatter(data=ff,x = "logNER", y ="logN1",
          add = "reg.line",  # Add regressin line
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE, # Add confidence interval
          fullrange = TRUE, # Extending the regression line
          rug = TRUE   # Add marginal rug
)+geom_cor(method = "pearson")+
  geom_text(aes(label=cohort),hjust=0, vjust=0,size=3) +
  hrbrthemes::theme_ipsum() +
  ylab(TeX('$log(Number_{a=1,y+1})$')) +
  xlab(TeX('$log(Number_{a=0,y})$'))+
  theme(legend.position = "none") +
  ggtitle("Without ECOCADIZ-RECLUTAS, Age0 autumn estimate vs Age1 spring estimate")+
  theme(plot.title = element_text(size = 12, face = "bold"))
#ggsave(paste0(res.plots,'/',"Recruits[Without Cadiz] vs PELAGO+PELACUS.png"))

ggscatter(data=subset(ff,cohort>1996),x = "logNER", y ="logN1",
          add = "reg.line",  # Add regressin line
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE, # Add confidence interval
          fullrange = TRUE, # Extending the regression line
          rug = TRUE   # Add marginal rug
)+geom_cor(method = "pearson")+
  geom_text(aes(label=cohort),hjust=0, vjust=0,size=3) +
  hrbrthemes::theme_ipsum() +
  ylab(TeX('$log(Number_{a=1,y+1})$')) +
  xlab(TeX('$log(Number_{a=0,y})$'))+
  theme(legend.position = "none") +
  ggtitle("Without ECOCADIZ-RECLUTAS, After 1996, Age0 autumn estimate vs Age1 spring estimate")+
  theme(plot.title = element_text(size = 12, face = "bold"))
#ggsave(paste0(res.plots,'/',"Recruits[Without Cadiz 1996] vs PELAGO+PELACUS.png"))

ggscatter(data=subset(ff,cohort>2012),x = "logNER", y ="logN1",
          add = "reg.line",  # Add regressin line
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE, # Add confidence interval
          fullrange = TRUE, # Extending the regression line
          rug = TRUE   # Add marginal rug
)+geom_cor(method = "pearson")+
  geom_text(aes(label=cohort),hjust=0, vjust=0,size=3) +
  hrbrthemes::theme_ipsum() +
  ylab(TeX('$log(Number_{a=1,y+1})$')) +
  xlab(TeX('$log(Number_{a=0,y})$'))+
  theme(legend.position = "none") +
  ggtitle("Without ECOCADIZ-RECLUTAS, After 2012, Age0 autumn estimate vs Age1 spring estimate")+
  theme(plot.title = element_text(size = 12, face = "bold"))
#ggsave(paste0(res.plots,'/',"Recruits[Without Cadiz 2012] vs PELAGO+PELACUS.png"))

## - for common areas (9aCN and 9aCS)

# - Log(N)

ggscatter(data=ff,x = "logNC", y ="logN1",
          add = "reg.line",  # Add regressin line
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE, # Add confidence interval
          fullrange = TRUE, # Extending the regression line
          rug = TRUE   # Add marginal rug
)+geom_cor(method = "pearson")+
  geom_text(aes(label=cohort),hjust=0, vjust=0,size=3) +
  hrbrthemes::theme_ipsum() +
  ylab(TeX('$log(Number_{a=1,y+1})$')) +
  xlab(TeX('$log(Number_{a=0,y})$'))+
  theme(legend.position = "none") +
  ggtitle("Common, Age0 autumn estimate vs Age1 spring estimate")+
  theme(plot.title = element_text(size = 12, face = "bold"))
#ggsave(paste0(res.plots,'/',"Recruits[Common] vs PELAGO+PELACUS.png"))

ggscatter(data=subset(ff,cohort>1996),x = "logNC", y ="logN1",
          add = "reg.line",  # Add regressin line
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE, # Add confidence interval
          fullrange = TRUE, # Extending the regression line
          rug = TRUE   # Add marginal rug
)+geom_cor(method = "pearson")+
  geom_text(aes(label=cohort),hjust=0, vjust=0,size=3) +
  hrbrthemes::theme_ipsum() +
  ylab(TeX('$log(Number_{a=1,y+1})$')) +
  xlab(TeX('$log(Number_{a=0,y})$'))+
  theme(legend.position = "none") +
  ggtitle("Common, After 1996, Age0 autumn estimate vs Age1 spring estimate")+
  theme(plot.title = element_text(size = 12, face = "bold"))
#ggsave(paste0(res.plots,'/',"Recruits[Common, 1996] vs PELAGO+PELACUS.png"))

ggscatter(data=subset(ff,cohort>2012),x = "logNC", y ="logN.y",
          add = "reg.line",  # Add regressin line
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE, # Add confidence interval
          fullrange = TRUE, # Extending the regression line
          rug = TRUE   # Add marginal rug
)+geom_cor(method = "pearson")+
  geom_text(aes(label=cohort),hjust=0, vjust=0,size=3) +
  hrbrthemes::theme_ipsum() +
  ylab(TeX('$log(Number_{a=1,y+1})$')) +
  xlab(TeX('$log(Number_{a=0,y})$'))+
  theme(legend.position = "none") +
  ggtitle("Common, After 2012, Age0 autumn estimate vs Age1 spring estimate")+
  theme(plot.title = element_text(size = 12, face = "bold"))
#ggsave(paste0(res.plots,'/',"Recruits[Common, 2012] vs PELAGO+PELACUS.png"))


## - for only 9aCN

# - Log(N)

ggscatter(data=ff,x = "logNCN", y ="logN1",
          add = "reg.line",  # Add regressin line
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE, # Add confidence interval
          fullrange = TRUE, # Extending the regression line
          rug = TRUE   # Add marginal rug
)+geom_cor(method = "pearson")+
  geom_text(aes(label=cohort),hjust=0, vjust=0,size=3) +
  hrbrthemes::theme_ipsum() +
  ylab(TeX('$log(Number_{a=1,y+1})$')) +
  xlab(TeX('$log(Number_{a=0,y})$'))+
  theme(legend.position = "none") +
  ggtitle("NPOR, Age0 autumn estimate vs Age1 spring estimate")+
  theme(plot.title = element_text(size = 12, face = "bold"))
#ggsave(paste0(res.plots,'/',"Recruits[NPOR] vs PELAGO+PELACUS.png"))

ggscatter(data=subset(ff,cohort>1996),x = "logNCN", y ="logN1",
          add = "reg.line",  # Add regressin line
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE, # Add confidence interval
          fullrange = TRUE, # Extending the regression line
          rug = TRUE   # Add marginal rug
)+geom_cor(method = "pearson")+
  geom_text(aes(label=cohort),hjust=0, vjust=0,size=3) +
  hrbrthemes::theme_ipsum() +
  ylab(TeX('$log(Number_{a=1,y+1})$')) +
  xlab(TeX('$log(Number_{a=0,y})$'))+
  theme(legend.position = "none") +
  ggtitle("9aCN, After 1996, Age0 autumn estimate vs Age1 spring estimate")+
  theme(plot.title = element_text(size = 12, face = "bold"))
#ggsave(paste0(res.plots,'/',"Recruits[NPOR,1996] vs PELAGO+PELACUS.png"))

ggscatter(data=subset(ff,cohort>2012),x = "logNCN", y ="logN1",
          add = "reg.line",  # Add regressin line
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE, # Add confidence interval
          fullrange = TRUE, # Extending the regression line
          rug = TRUE   # Add marginal rug
)+geom_cor(method = "pearson")+
  geom_text(aes(label=cohort),hjust=0, vjust=0,size=3) +
  hrbrthemes::theme_ipsum() +
  ylab(TeX('$log(Number_{a=1,y+1})$')) +
  xlab(TeX('$log(Number_{a=0,y})$'))+
  theme(legend.position = "none") +
  ggtitle("9aCN, After 2012, Age0 autumn estimate vs Age1 spring estimate")+
  theme(plot.title = element_text(size = 12, face = "bold"))
#ggsave(paste0(res.plots,'/',"Recruits[NPOR,2012] vs PELAGO+PELACUS.png"))

