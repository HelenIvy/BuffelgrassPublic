
#read in data
library(readxl)
library(readxl)
library(readxl)
library(readr)
BG_Individuals <- read_csv("Data/BG Individuals.csv")
View(BG_Individuals)


DATABGIC <- BG_Individuals

#Set categorical factors for main effects:
DATABGIC <- within(DATABGIC, {
  BLOCK<-factor(Block)
  BLOCKPLOT<-factor(BlockPlot)
  TREATMENT<-factor(Treatment)
  YEAR<-factor(year)
  SEASONYEAR<-factor(seasonyear)
  
})
#If use YEAR is categorical, if use Year is continous
summary(DATABGIC)


#repeated measures(1\SUBJECT) for the random subject effect 

#for examples see https://bookdown.org/ndphillips/YaRrr/repeated-measures-anova-using-the-lme4-package.html and https://stats.stackexchange.com/questions/58745/using-lmer-for-repeated-measures-linear-mixed-effect-model
# fits an lmer model
library(lme4)
# Loads the library
library(redres)
require(lme4)
library(car)
require(car)
require(tidyr)



#Zero-inflated poisson model and compared via Analysis of Deviance using an F test.


install.packages("pscl")
install.packages("car")
install.packages("rcompanion")
install.packages("multcompView")
install.packages("rcompanion")
install.packages("emmeans")
install.packages("R2admb")
install.packages("glmmADMB", 
                 repos=c("http://glmmadmb.r-forge.r-project.org/repos",
                         getOption("repos")),
                 type="source")


require(pscl)

summary(m1 <- zeroinfl(Individuals ~TREATMENT*SEASONYEAR, data=DATA))
summary(m2 <- zeroinfl(Individuals ~TREATMENT*SEASONYEAR + (1/BLOCK), data=DATA))
summary(m3 <- zeroinfl(Individuals ~TREATMENT*SEASONYEAR + BLOCK, data=DATA))
summary(m4 <- zeroinfl(Individuals ~TREATMENT*SEASONYEAR, data=DATA, dist = "poisson"))
require(glmmADMB)
summary(mm1 <- glmmadmb(Individuals ~TREATMENT*SEASONYEAR + (1|BLOCK), 
                        data=DATA, zeroInflation = TRUE, family = "poisson"))

anova(m1)
