
#read in data

library(readr)
IndividualsBG2 <- read_csv("Data/IndividualsBG2.csv")
View(IndividualsBG2)
#dataset does not include invaded control plots (only counted 2 seasons)


DATAglmm <- IndividualsBG2


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

#followed: https://cran.r-project.org/web/packages/glmmTMB/vignettes/glmmTMB.pdf

install.packages("glmmTMB")
citeNatbib("glmmTMB")
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

#using car anova https://cran.r-project.org/web/packages/glmmTMB/vignettes/model_evaluation.pdf
library(car)

Anova(fit_zinbinom1,type="III")
  
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
      adjust="tukey")
cld(marginal,
    alpha=0.05,
    Letters=letters,  ### Use lower-case letters for .group
    adjust="tukey")

marginal = emmeans(fit_zinbinom1,
                   ~ SEASONYEAR)
pairs(marginal,
      adjust="tukey")
cld(marginal,
    alpha=0.05,
    Letters=letters,  ### Use lower-case letters for .group
    adjust="tukey")
