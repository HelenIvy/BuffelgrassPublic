install.packages("glmmTMB")
citeNatbib("glmmTMB")
install.packages("car")
install.packages("MASS")
install.packages("bbmle")
install.packages("emmeans")
install.packages("multcompView")
install.packages("multcomp")
#read in data

library(readr)
Individuals_INV_CNTRL <- read_csv("Data/Individuals INV CNTRL.csv")
View(Individuals_INV_CNTRL)

DATABGICglmm <- Individuals_INV_CNTRL


#Set categorical factors for main effects:
DATABGICglmm <- within(DATABGICglmm, {
  BLOCK<-as.factor(Block)
  PLOT<-as.factor(Plot)
  BLOCKPLOT<-factor(BlockPlot)
  TREATMENT<-factor(Treatment)
  YEAR<-factor(Year)
  SEASONYEAR<-factor(SeasonYear)
  
})
#If use YEAR is categorical, if use Year is continous
summary(DATABGICglmm)


#followed: https://cran.r-project.org/web/packages/glmmTMB/vignettes/glmmTMB.pdf

library("glmmTMB")
require("glmmTMB")
library("bbmle") ## for AICtab
library("ggplot2")
## cosmetic
theme_set(theme_bw()+
            theme(panel.spacing=grid::unit(0,"lines")))


require(glmmTMB)
require(car)
mpoi <- glmmTMB(Individuals~TREATMENT  SEASONYEAR 
                                                   + (1|BLOCK)+(1|BLOCKPLOT),
                         data=DATABGICglmm,
                         ziformula=~1,
                         family=poisson)

summary(mpoi)

mzinbinom1 <- update(mpoi,family=nbinom1)
summary(mzinbinom1)



AICtab(mpoi,mzinbinom1)
#best and only one to converge is mzinbinom1

#using car anova https://cran.r-project.org/web/packages/glmmTMB/vignettes/model_evaluation.pdf
library(car)
Anova(mzinbinom1)
Anova(mzinbinom1,type="III")
  
#Post-hoc analysis can be conducted with the emmeans package.
library(multcompView)
require(multcompView)
library(multcomp)
require(multcomp)
library(emmeans)
require(emmeans)



marginal = emmeans(mzinbinom1,
                   ~ SEASONYEAR)
pairs(marginal,
      adjust="tukey")
cld(marginal,
    alpha=0.05,
    Letters=letters,  ### Use lower-case letters for .group
    adjust="tukey")
