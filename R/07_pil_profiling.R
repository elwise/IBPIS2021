## likelihood profile analysis of steepness

#for model without the recruitment index
dirDEPM=paste0(res.ss,'/2020_DEPM')

starter <- SS_readstarter(file.path(dirDEPM, 'starter.ss'))
starter$ctlfile <- "control_modified.ss"
starter$prior_like <- 1
SS_writestarter(starter, dir=dirDEPM, overwrite=TRUE)

h.vec <- seq(0.3,0.9,.05)
Nprofile <- length(h.vec)

profileDEPM <- SS_profile(dir=dirDEPM, # directory
                      masterctlfile="control.ss_new",
                      newctlfile="control_modified.ss",
                      string="steep",
                      profilevec=h.vec,model='ss_3.30.16.02')

proModelsDEPM <- SSgetoutput(dirvec=dirDEPM, keyvec=1:Nprofile,getcovar = FALSE)

profSumDEPM <- SSsummarize(proModelsDEPM)

SSplotProfile(profSumDEPM,           # summary object
              profile.string = "steep", # substring of profile parameter
              profile.label="Stock-recruit steepness (h)") # axis label

PinerPlot(profSumDEPM,profile.string = "steep",profile.label = "Stock-recruit steepness (h)",component="Surv_like")

profSumDEPM$likelihoods_by_fleet%>%
  pivot_longer(,cols=3:6,names_to="Fleet",values_to="value")%>%
  subset(., Label %in% c("Age_like","Catch_like","Surv_like","Init_equ_like"))%>%
  ggplot(aes(x=model,y=value,colour=Fleet))+
  geom_point()+
  geom_line()+
  facet_wrap(~Label,scales = 'free_y')

SSplotComparisons(profSumDEPM,legendlabels=paste("h =",h.vec))

#likehood without profiling

profSum <- SSsummarize(list(run2020,run2020.DEPM,runa))


profSum$likelihoods_by_fleet%>%
  pivot_longer(,cols=3:7,names_to="Fleet",values_to="value")%>%
  subset(., Label %in% c("Age_like","Catch_like","Surv_like","Init_equ_like"))%>%
  mutate_at("model", factor, levels = 1:3, labels = c("Assm2020","AssmDEPM","Setup a"))%>%
  ggplot(aes(fill=model,y=value,x=Fleet))+
  geom_col(position = "dodge") +
  facet_wrap(~Label,scales = 'free_y')+
  theme(axis.text=element_text(size=8),axis.text.x = element_text(angle = 90))



##############################################################
mydir=paste0(res.ss,'/SetupaSD')

starter <- SS_readstarter(file.path(mydir, 'starter.ss'))
starter$ctlfile <- "control_modified.ss"
starter$prior_like <- 1
SS_writestarter(starter, dir=mydir, overwrite=TRUE)


profile <- SS_profile(dir=mydir, # directory
# "NatM" is a subset of one of the
# parameter labels in control.ss_new
masterctlfile="control.ss_new",
newctlfile="control_modified.ss",
string="steep",
profilevec=h.vec,model='ss_3.30.16.02')

profilemodels <- SSgetoutput(dirvec=mydir, keyvec=1:Nprofile)

profilesummary <- SSsummarize(profilemodels)

SSplotProfile(profilesummary,           # summary object
profile.string = "steep", # substring of profile parameter
profile.label="Stock-recruit steepness (h)") # axis label

profilesummary$likelihoods_by_fleet%>%
  pivot_longer(,cols=3:7,names_to="Fleet",values_to="value")%>%
  subset(., Label %in% c("Age_like","Catch_like","Surv_like","Init_equ_like"))%>%
  ggplot(aes(x=model,y=value,colour=Fleet))+
  geom_point()+
  geom_line()+
  facet_wrap(~Label,scales = 'free_y')

SSplotComparisons(profilesummary,legendlabels=paste("h =",h.vec))



profSum <- SSsummarize(list(run2020,run2020.DEPM,runa))

profSum$likelihoods%>%pivot_longer(.,cols=1:3,names_to="Model",values_to="Likehood")%>%
  + ggplot(aes(x=Model,y=Likehood)) + geom_point()+geom_line()+facet_wrap(.~Label,scales="free_y")

profSum$likelihoods_by_fleet%>%
  pivot_longer(,cols=3:7,names_to="Fleet",values_to="value")%>%
  subset(., Label %in% c("Age_like","Catch_like","Surv_like","Init_equ_like"))%>%
  mutate_at("model", factor, levels = 1:3, labels = c("Assm2020","AssmDEPM","Setup a"))%>%
              ggplot(aes(fill=model,y=value,x=Fleet))+
              geom_col(position = "dodge") +
              facet_wrap(~Label,scales = 'free_y')
            
profSum$likelihoods_by_fleet%>%
    pivot_longer(,cols=3:7,names_to="Fleet",values_to="value")%>%
  subset(., Label %in% c("Age_like","Catch_like","Surv_like","Init_equ_like"))%>%
  mutate_at("model", factor, levels = 1:3, labels = c("Assm2020","AssmDEPM","Setup a"))%>%
  ggplot(aes(x=as.factor(model),y=value,colour=Fleet))+
  geom_point()+
  geom_line(aes(group = Fleet))+
  facet_wrap(~Label,scales = 'free_y')
