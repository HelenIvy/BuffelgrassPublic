install.packages("glmmTMB")
citeNatbib("glmmTMB")
install.packages("car")
install.packages("MASS")
install.packages("bbmle")
install.packages("emmeans")
install.packages("multcompView")
install.packages("multcomp")
#read in data

#READ IN DATA WITHOUT FALL 2020 AND WITH BASELINE AS NEW COLUMN

library(readr)
Individuals_INV_CNTRLnoFALL20 <- read_csv("Data/Individuals INV CNTRLnoFALL20.csv")
View(Individuals_INV_CNTRLnoFALL20)

DATABGICglmm <- Individuals_INV_CNTRLnoFALL20


#Set categorical factors for main effects:
DATABGICglmm <- within(DATABGICglmm, {
  BLOCK<-as.factor(Block)
  PLOT<-as.factor(Plot)
  BLOCKPLOT<-factor(BlockPlot)
  TREATMENT<-factor(Treatment)
  YEAR<-factor(Year)
  SEASONYEAR<-factor(SeasonYear)
  
})
#use YEAR AS categorical
summary(DATABGICglmm)

#REMOVE BASELINE YEAR FROM DATASET
DATABGICglmmB <- subset(DATABGICglmm, SEASONYEAR!= 'Spring2020')


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
mpoi2 <- glmmTMB(Individuals~TREATMENT + SEASONYEAR +BaselinePECI
                                                   + (1|BLOCK),
                         data=DATABGICglmmB,
                         ziformula=~1,
                         family=poisson)

summary(mpoi2)

mzinbinom1b <- update(mpoi2,family=nbinom1)
summary(mzinbinom1b)
#binomial model does not converge, so poisson is the best model



#using car anova https://cran.r-project.org/web/packages/glmmTMB/vignettes/model_evaluation.pdf
library(car)
Anova(mpoi2)
Anova(mpoi2,type="III")
  
#Post-hoc analysis can be conducted with the emmeans package.
library(multcompView)
require(multcompView)
library(multcomp)
require(multcomp)
library(emmeans)
require(emmeans)



marginal = emmeans(mpoi2,
                   ~ SEASONYEAR)
pairs(marginal,
      adjust="tukey")
cld(marginal,
    alpha=0.05,
    Letters=letters,  ### Use lower-case letters for .group
    adjust="tukey")
marginal = emmeans(mpoi2,
                   ~ TREATMENT)
pairs(marginal,
      adjust="tukey")
cld(marginal,
    alpha=0.05,
    Letters=letters,  ### Use lower-case letters for .group
    adjust="tukey")

marginal = emmeans(mpoi2,
                   ~ BaselinePECI )
pairs(marginal,
      adjust="tukey")
cld(marginal,
    alpha=0.05,
    Letters=letters,  ### Use lower-case letters for .group
    adjust="tukey")
