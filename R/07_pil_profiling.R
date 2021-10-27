## likelihood profile analysis of steepness

#for model SetupaSD
dirSD=paste0(res.ss,'/SetupaSD')

starter <- SS_readstarter(file.path(dirSD, 'starter.ss'))
starter$ctlfile <- "control_modified.ss"
starter$prior_like <- 1
SS_writestarter(starter, dir=dirSD, overwrite=TRUE)

h.vec <- seq(0.1,0.9,.1)
Nprofile <- length(h.vec)

profileSD <- SS_profile(dir=dirSD, # directory
                      masterctlfile="control.ss_new",
                      newctlfile="control_modified.ss",
                      string="steep",
                      profilevec=h.vec,model='ss_3.30.16.02')

#Warning messages:
#1: In shell(cmd = command) :
#  'ss_3.30.16.02 -nox' execution failed with error code 1
#2: In shell(cmd = command) :
#  'ss_3.30.16.02 -nox' execution failed with error code 1

proModelsSD <- SSgetoutput(dirvec=dirSD, keyvec=1:Nprofile,getcovar = FALSE)

profSumSD <- SSsummarize(proModelsSD)

SSplotProfile(profSumSD,           # summary object
              profile.string = "steep", # substring of profile parameter
              profile.label="Stock-recruit steepness (h)",xlim = c(0.3,0.9)) # axis label

PinerPlot(profSumSD,component="Surv_like",main = "Surv_like",
            profile.string = "steep",
          profile.label = "Stock-recruit steepness (h)",
          xlim=c(0.3,0.9))

SSplotComparisons(profSumSD,legendlabels=paste("h =",h.vec),subplots = c(2,8,10,12,13),xlim=c(1978,2020),print = TRUE,plotdir = res.plots,
                  legendloc = "topright",filenameprefix = "ProfileSD")


#for model SetupaSDQ
dirSDQ=paste0(res.ss,'/SetupaSDQ')

starter <- SS_readstarter(file.path(dirSDQ, 'starter.ss'))
starter$ctlfile <- "control_modified.ss"
starter$prior_like <- 1
SS_writestarter(starter, dir=dirSDQ, overwrite=TRUE)

h.vec <- seq(0.1,0.9,.1)
Nprofile <- length(h.vec)

profileSDQ <- SS_profile(dir=dirSDQ, # directory
                        masterctlfile="control.ss_new",
                        newctlfile="control_modified.ss",
                        string="steep",
                        profilevec=h.vec,model='ss_3.30.16.02')

#Warning messages:
#1: In shell(cmd = command) :
#  'ss_3.30.16.02 -nox' execution failed with error code 1
#2: In shell(cmd = command) :
#  'ss_3.30.16.02 -nox' execution failed with error code 1

proModelsSDQ <- SSgetoutput(dirvec=dirSDQ, keyvec=1:Nprofile,getcovar = FALSE)

profSumSDQ <- SSsummarize(proModelsSDQ)

SSplotProfile(profSumSDQ,           # summary object
              profile.string = "steep", # substring of profile parameter
              profile.label="Stock-recruit steepness (h)",xlim = c(0.3,0.9)) # axis label

PinerPlot(profSumSDQ,component="Surv_like",main = "Surv_like",
          profile.string = "steep",
          profile.label = "Stock-recruit steepness (h)",
          xlim=c(0.3,0.9)
          )

SSplotComparisons(profSumSDQ,legendlabels=paste("h =",h.vec),subplots = c(2,8,10,12,13),xlim=c(1978,2020),print = TRUE,plotdir = res.plots,
                  legendloc = "topright",filenameprefix = "ProfileSDQ")

