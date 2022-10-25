
#read in data
library(readxl)
library(readxl)
library(readxl)
library(readr)

BG_IndividualsTreatFreq <- read_csv("Data/BG IndividualsTreatFreq.csv")
View(BG_IndividualsTreatFreq)

DATABGIC2 <- BG_IndividualsTreatFreq
   

#Set categorical factors for main effects:
DATABGIC2 <- within(DATABGIC2, {
  BLOCK<-factor(Block)
  BLOCKPLOT<-factor(BlockPlot)
  TREATMENT<-factor(Treatment)
  TREATFREQ<- factor(TreatFreq)
  YEAR<-factor(year)
  SEASONYEAR<-factor(seasonyear)
  
})
#If use YEAR is categorical, if use Year is continous
summary(DATABGIC2)

                 

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
#used log+1 transform m2b
m1 <- lmer(Individuals ~TreatFreq*SEASONYEAR + (1|BLOCKPLOT)+(1|BLOCK), data = DATABGIC2)
summary (m1)

m2 <- lmer(SQRTIndividuals ~TreatFreq*SEASONYEAR + (1|BLOCKPLOT)+(1|BLOCK), data = DATABGIC2)
summary (m2)
m22 <- lmer(LOGP1Individuals ~TreatFreq*SEASONYEAR + (1|BLOCKPLOT)+(1|BLOCK), data = DATABGIC2)
summary (m22)
#added model with treatment frequency instead of season year
       
                 
m2c<- lmer(LOGP1Individuals ~TreatFreq*SEASONYEAR + (1|BLOCKPLOT)+(1|BLOCK), data = DATABGIC2)
summary (m2b)

m3 <- glmer(Individuals ~TreatFreq + (1|BLOCKPLOT) + (1 | BLOCK), 
             data= DATABGIC2, family = poisson(link="log"))

Anova(m1)

Anova(m2)
Anova(m2b)
Anova(m22)
Anova(m3)
#studentized residuals
# fits an lmer model
library(lme4)
# Loads the library
library(redres)
require(lme4)

# computes the default residuals (raw conditional)
rc_resids <- compute_redres(m1)

# computes the Pearson marginal residuals
pm_resids <- compute_redres(m1, type = "pearson_mar")

# computes the studentized conditional residuals
sc_resids <- compute_redres(m1, type = "std_cond")

# puts the residuals in a data frame with observed response and prints the first six rows
resids <- data.frame(DATABGIC2, rc_resids, pm_resids, sc_resids)
head(resids) 
#plot_redres creates a plot (using ggplot2) of the residuals versus the fitted values given a model and a specified residual type. All residual types listed for redres work with plot_redres.

# creates a plot of the conditional studentized residuals versus the fitted values
plot_redres(m1, type = "std_cond")
#m1 residuals are skewed <

#residuals m2;

rc_resids <- compute_redres(m2)
pm_resids <- compute_redres(m2, type = "pearson_mar")
sc_resids <- compute_redres(m2, type = "std_cond")
resids <- data.frame(DATABGIC2, rc_resids, pm_resids, sc_resids)
head(resids) 
plot_redres(m2, type = "std_cond")
#m2 still slightly v shaped

#residuals m2b;

rc_resids <- compute_redres(m2b)
pm_resids <- compute_redres(m2b, type = "pearson_mar")
sc_resids <- compute_redres(m2b, type = "std_cond")
resids <- data.frame(DATABGIC2, rc_resids, pm_resids, sc_resids)
head(resids) 
plot_redres(m2b, type = "std_cond")

#m2b best but a little football shaped

#residuals m22
rc_resids <- compute_redres(m22)
pm_resids <- compute_redres(m22, type = "pearson_mar")
sc_resids <- compute_redres(m22, type = "std_cond")
resids <- data.frame(DATABGIC2, rc_resids, pm_resids, sc_resids)
head(resids) 
plot_redres(m22, type = "std_cond")
#m2c residuals look great!

library(emmeans)
require(emmeans)
library(multcomp)
require(multcomp)



#posthoc differences m1 - treatment and year significant
marginal = emmeans(m1,
                   ~ TREATMENT*SEASONYEAR)
pairs(marginal,
      adjust="tukey")
cld(marginal,
    alpha=0.05,
    Letters=letters,  ### Use lower-case letters for .group
    adjust="tukey")
marginal = emmeans(m1,
                   ~ TREATMENT)
pairs(marginal,
      adjust="tukey")
cld(marginal,
    alpha=0.05,
    Letters=letters,  ### Use lower-case letters for .group
    adjust="tukey")

marginal = emmeans(m2b,
                   ~ TREATMENT*SEASONYEAR)
pairs(marginal,
      adjust="tukey")
cld(marginal,
    alpha=0.05,
    Letters=letters,  ### Use lower-case letters for .group
    adjust="tukey")


marginal = emmeans(m2b,
                   ~ TREATMENT)
pairs(marginal,
      adjust="tukey")
cld(marginal,
    alpha=0.05,
    Letters=letters,  ### Use lower-case letters for .group
    adjust="tukey")

marginal = emmeans(m22,
                   ~ TreatFreQ)
pairs(marginal,
      adjust="tukey")
cld(marginal,
    alpha=0.05,
    Letters=letters,  ### Use lower-case letters for .group
    adjust="tukey")
