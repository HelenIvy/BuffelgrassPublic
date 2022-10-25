
#read in data

library(readr)
IndividualsBG2 <- read_csv("Data/IndividualsBG2.csv")
View(IndividualsBG2)



DATAglmm <- IndividualsBG2


DATAglmm <- BG_Individuals

#Set categorical factors for main effects:
DATAglmm <- within(DATAglmm, {
  BLOCK<-as.factor(Block)
  PLOT<-as.factor(Plot)
  BLOCKPLOT<-factor(BlockPlot)
  TREATMENT<-factor(Treatment)
  YEAR<-factor(year)
  SEASONYEAR<-factor(seasonyear)
  
})
#If use YEAR is categorical, if use Year is continous
summary(DATAglmm)


#repeated measures(1\SUBJECT) for the random subject effect 

#try the glmmTMB, https://cran.r-project.org/web/packages/glmmTMB/vignettes/glmmTMB.pdf load packages

install.packages("glmmTMB")
install.packages("car")
install.packages("MASS")
install.packages("bbmle")
install.packages("emmeans")
install.packages("multcompView")
install.packages("multcomp")
library("glmmTMB")
require("glmmTMB")
library("bbmle") ## for AICtab
library("ggplot2")
## cosmetic
theme_set(theme_bw()+
            theme(panel.spacing=grid::unit(0,"lines")))

#example set

#JB SUGGESTS https://journal.r-project.org/archive/2017/RJ-2017-066/RJ-2017-066.pdf
require(glmmTMB)
require(car)
fit_zipoisson <- glmmTMB(Individuals~TREATMENT + SEASONYEAR
                           + (1|BLOCK)+(1|BLOCKPLOT),
                         data=DATAglmm,
                         ziformula=~1,
                         family=poisson)
summary(fit_zipoisson)

fit_zinbinom1 <- update(fit_zipoisson,family=nbinom1)
summary(fit_zinbinom1)

fit_zinbinom2 <- update(fit_zipoisson,family=nbinom2)
summary(fit_zinbinom2)


AICtab(fit_zipoisson,fit_zinbinom2,fit_zinbinom1)
#best fit is zinbinom1

#Post-hoc analysis can be conducted with the emmeans package.
library(multcompView)
require(multcompView)
library(multcomp)
require(multcomp)
library(emmeans)
require(emmeans)
marginal = emmeans(fit_zinbinom1,
                   ~ TREATMENT)
pairs(marginal,
      adjust="sidak")
CLD = cld(marginal,
          alpha=0.05,
          Letters=letters,
          adjust="sidak")


