# Script info -------------------------------------------------------------

# extend runs SetupaSDQTune2 and SetupaSDTune2 to include estimation of steepness
# IBPIS2021, 28/10/2021

# Set directories ---------------------------------------------------------

# wd <- "D:/ICES/IBPIS2021"
wd <- "C:/Use/GitHub/IBPIS2021"
setwd(wd)

# directory to save plots 
res.plots <- file.path("./plots")

# directory with SS runs
res.ss <- file.path("./SS_runs")

# directory with data
res.dir  <- file.path("./data")

# Load libraries ----------------------------------------------------------

library(r4ss)
library(tidyverse)

theme_set(theme_bw(base_size = 16))

# Read run's outputs ------------------------------------------------------

#Setup a
Setupa <- SS_output(dir = paste0(res.ss,'/Setupa'),forecast=FALSE,ncols=62,verbose = TRUE, printstats = TRUE)

#Setup a power for recruitment catchability and steepness fixed
SetupaQ <- SS_output(dir = paste0(res.ss,'/SetupaQ'),forecast=FALSE,ncols=62,verbose = TRUE, printstats = TRUE)

#Setup a with sd extra parameter for q_All, fine tuning and steepness fixed
SetupaSDTune2 <- SS_output(dir = paste0(res.ss,'/SetupaSDTune2'),forecast=FALSE,ncols=62,verbose = TRUE, printstats = TRUE)

#Setup a with sd extra parameter for q_All, fine tuning and steepness of SR model estimated
SetupaSDTune2H <- SS_output(dir = paste0(res.ss,'/SetupaSDTune2H'),forecast=FALSE,ncols=62,verbose = TRUE, printstats = TRUE)

#Setup a with sd extra parameter for q_All, power for recruitment catchability, fine tuning and steepness fixed
SetupaSDQTune2 <- SS_output(dir = paste0(res.ss,'/SetupaSDQTune2'),forecast=FALSE,ncols=62,verbose = TRUE, printstats = TRUE)

#Setup a with sd extra parameter for q_All, power for recruitment catchability, fine tuning and steepness of SR model estimated
SetupaSDQTune2H <- SS_output(dir = paste0(res.ss,'/SetupaSDQTune2H'),forecast=FALSE,ncols=62,verbose = TRUE, printstats = TRUE)


# Plots for new runs ------------------------------------------------------

SS_plots(SetupaQ)
SS_plots(SetupaSDTune2H)
SS_plots(SetupaSDQTune2H)


# Comparison --------------------------------------------------------------


cpl <- list(SetupaSDTune2,SetupaSDTune2H,SetupaSDQTune2,SetupaSDQTune2H)
cpl.sum <- SSsummarize(biglist=cpl)
SSplotComparisons(summaryoutput=cpl.sum,subplots = c(2,8,10,12,13,14),xlim=c(1978,2021),print = TRUE,plotdir = res.dir,
                  legendlabels = c("SD","SDH","SDQ","SDQH"),legendloc = "topright",filenameprefix = "Steepness")


cpl <- list(Setupa, SetupaQ, SetupaSDTune2, SetupaSDQTune2)
cpl.sum <- SSsummarize(biglist=cpl)
SSplotComparisons(summaryoutput=cpl.sum,subplots = c(2,8,10,12,13,14),xlim=c(1978,2021),print = TRUE,plotdir = res.dir,
                  legendlabels = c("base", "Q","SD","SDQ"),legendloc = "topright",filenameprefix = "Q_SD_")
