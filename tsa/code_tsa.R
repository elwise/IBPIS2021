# Sript info --------------------------------------------------------------

# code to study the cross-correlation between age0 from autumn acoustic surveys and 
# age 1 from the spring surveys by accounting for the auto-correlation of the time-series
# suggestion by Kelli Johnson during IBPIS2021

# leire ibaibarriaga, 29/10/2021


# load libraries ----------------------------------------------------------

library(TSA)
library(ggplot2)
library(ggrepel)
library(tidyverse)

theme_set(theme_bw(base_size = 16))

# working directories -----------------------------------------------------

getwd()

# load data ---------------------------------------------------------------

load("Age0vsAge1.RData")

summary(dd)
head(dd)

# check that the function scale standardises to mean 0 and var 1 
data.frame(S1=scale(dd$NCN), S2=(dd$NCN-mean(dd$NCN, na.rm=T))/sqrt(var(dd$NCN, na.rm=T)))

# compute standardised time-series
dd$NCNscaled <- scale(dd$NCN)
dd$N1scaled <- scale(dd$N1)

# Exploratory plots -------------------------------------------------------

# individual time-series
ggplot(dd, aes(x=cohort, y=NCN))+
  geom_point(pch=19, size=2)+
  geom_line()+
  geom_vline(xintercept=1996.5, lty=2)

ggplot(dd, aes(x=cohort, y=logNCN))+
  geom_point(pch=19, size=2)+
  geom_line()+
  geom_vline(xintercept=1996.5, lty=2)

ggplot(dd, aes(x=cohort, y=N1))+
  geom_point(pch=19, size=2)+
  geom_line()+
  geom_vline(xintercept=1996.5, lty=2)

ggplot(dd, aes(x=cohort, y=logN1))+
  geom_point(pch=19, size=2)+
  geom_line()+
  geom_vline(xintercept=1996.5, lty=2)

# comparison of the scaled time-series

dd2 <- pivot_longer(dd[, c("cohort", "NCNscaled","N1scaled")], cols=2:3, names_to = "name", values_to = "value") %>% 
  as.data.frame()

ggplot(dd2, aes(x=cohort, y=value, group=name, col=name))+
  geom_point(pch=19, size=2)+
  geom_line(lwd=1)+
  geom_hline(yintercept=0, lty=2)+
  ylab("Standardised series")+
  geom_vline(xintercept=1996.5, lty=2)

# cross-correlations

ggplot(dd, aes(NCN, N1, label=cohort))+
  geom_point(pch=19, size=2)+
  geom_text_repel()

ggplot(dd, aes(logNCN, logN1, label=cohort))+
  geom_point(pch=19, size=2)+
  geom_text_repel()


ggplot(dd, aes(NCN, N1, label=cohort))+
  geom_point(pch=19, size=2)+
  geom_text_repel()+
  geom_smooth(method="lm", formula=y~x, col=2, se=F)+
  geom_smooth(method="lm", formula=y~x-1, col=3, se=F)

# cross-correlation analyses (same as in the presentation)

cor(dd$logNCN, dd$logN1, use="complete.obs", method="pearson")
cor.test(dd$logNCN, dd$logN1, use="complete.obs", method="pearson")

cor(dd$logNCN[dd$cohort>1996], dd$logN1[dd$cohort>1996], use="complete.obs", method="pearson")
cor.test(dd$logNCN[dd$cohort>1996], dd$logN1[dd$cohort>1996], use="complete.obs", method="pearson")

# alternative NOT USED: spearman
# cor(dd$logNCN[dd$cohort>1996], dd$logN1[dd$cohort>1996], use="complete.obs", method="spearman")
# cor.test(dd$logNCN[dd$cohort>1996], dd$logN1[dd$cohort>1996], use="complete.obs", method="spearman")

# alternative NOT USED:not-logged, pearson and spearman
# cor(dd$NCN[dd$cohort>1996], dd$N1[dd$cohort>1996], use="complete.obs", method="pearson")
# cor(dd$NCN[dd$cohort>1996], dd$N1[dd$cohort>1996], use="complete.obs", method="spearman")
# cor.test(dd$NCN[dd$cohort>1996], dd$N1[dd$cohort>1996], use="complete.obs", method="pearson")
# cor.test(dd$NCN[dd$cohort>1996], dd$N1[dd$cohort>1996], use="complete.obs", method="spearman")


# Check auto-correlation --------------------------------------------------

par(mfrow=c(1,2))
acf(dd$logNCN, na.action=na.pass)
pacf(dd$logNCN, na.action=na.pass)

par(mfrow=c(1,2))
acf(dd$logNCN[dd$cohort>1996], na.action=na.pass)
pacf(dd$logNCN[dd$cohort>1996], na.action=na.pass)

par(mfrow=c(1,2))
acf(dd$logN1, na.action=na.pass)
pacf(dd$logN1, na.action=na.pass)

par(mfrow=c(1,2))
acf(dd$logN1[dd$cohort>1996], na.action=na.pass)
pacf(dd$logN1[dd$cohort>1996], na.action=na.pass)

# cross-correlation. See also https://online.stat.psu.edu/stat510/lesson/8/8.2
ccf(dd$logNCN, dd$logN1, na.action=na.pass)
ccf(dd$logNCN[dd$cohort>1996], dd$logN1[dd$cohort>1996], na.action=na.pass) # most dominant cross-correlation for h=0, as expected!

# 

TSA::prewhiten(dd$logNCN[dd$cohort>1996], dd$logN1[dd$cohort>1996], na.action=na.omit)


