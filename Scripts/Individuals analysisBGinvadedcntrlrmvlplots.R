
#read in data
library(readxl)
library(readxl)
library(readxl)
library(readr)
library(readr)
library(readxl)
BG_Individuals_Invaded_Cntl_removals <- read_excel("Data/BG Individuals Invaded Cntl removals.xlsx")
View(BG_Individuals_Invaded_Cntl_removals)

DATABGICH2P2 <- BG_Individuals_Invaded_Cntl_removals

#Set categorical factors for main effects:
DATABGICH2P2 <- within(DATABGICH2P2, {
  BLOCK<-factor(Block)
  BLOCKPLOT<-factor(BlockPlot)
  TREATMENT<-factor(Treatment)
  YEAR<-factor(year)
  SEASONYEAR<-factor(seasonyear)
  
})
#If use YEAR is categorical, if use Year is continous
summary(DATABGICH2P2)


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
#used log+1 transform m5
m3b <- lmer(individuals ~TREATMENT*SEASONYEAR + (1|BLOCKPLOT)+(1|BLOCK), data = DATABGICH2P2)
summary (m3b)

m4 <- lmer(SQRTIndividuals ~TREATMENT*SEASONYEAR + (1|BLOCKPLOT)+(1|BLOCK), data = DATABGICH2P2)
summary (m4)
m5 <- lmer(LOGP1Individuals ~TREATMENT*SEASONYEAR + (1|BLOCKPLOT)+(1|BLOCK), data = DATABGICH2P2)
summary (m5)


m6 <- glmer(individuals ~TREATMENT + (1|BLOCKPLOT) + (1 | BLOCK), 
             data= DATABGICH2P2, family = poisson(link="log"))

Anova(m3b)

Anova(m4)
Anova(m5)
Anova(m6)
#studentized residuals
# fits an lmer model
library(lme3)
# Loads the library
library(redres)
require(lme3)

# computes the default residuals (raw conditional)
rc_resids <- compute_redres(m3b)

# computes the Pearson marginal residuals
pm_resids <- compute_redres(m3b, type = "pearson_mar")

# computes the studentized conditional residuals
sc_resids <- compute_redres(m3b, type = "std_cond")

# puts the residuals in a data frame with observed response and prints the first six rows
resids <- data.frame(DATABGICH2P2, rc_resids, pm_resids, sc_resids)
head(resids) 
#plot_redres creates a plot (using ggplot2) of the residuals versus the fitted values given a model and a specified residual type. All residual types listed for redres work with plot_redres.

# creates a plot of the conditional studentized residuals versus the fitted values
plot_redres(m3b, type = "std_cond")
#m3b residuals show an outlier and v shape

#residuals m4;

rc_resids <- compute_redres(m4)
pm_resids <- compute_redres(m4, type = "pearson_mar")
sc_resids <- compute_redres(m4, type = "std_cond")
resids <- data.frame(DATABGICH2P2, rc_resids, pm_resids, sc_resids)
head(resids) 
plot_redres(m4, type = "std_cond")
#m4 better but outlier pronounced

#residuals m5;
rc_resids <- compute_redres(m5)
pm_resids <- compute_redres(m5, type = "pearson_mar")
sc_resids <- compute_redres(m5, type = "std_cond")
resids <- data.frame(DATABGICH2P2, rc_resids, pm_resids, sc_resids)
head(resids) 
plot_redres(m5, type = "std_cond")
#m5 best, but still outlier


#residuals m6 not correct because for glmermod, this code is for lmer

library(emmeans)
require(emmeans)
library(multcomp)
require(multcomp)



#posthoc differences m4 - treatment and year significant
marginal = emmeans(m3b,
                   ~ TREATMENT*SEASONYEAR)
pairs(marginal,
      adjust="tukey")
cld(marginal,
    alpha=0.05,
    Letters=letters,  ### Use lower-case letters for .group
    adjust="tukey")
marginal = emmeans(m4,
                   ~ TREATMENT)
pairs(marginal,
      adjust="tukey")
cld(marginal,
    alpha=0.05,
    Letters=letters,  ### Use lower-case letters for .group
    adjust="tukey")

marginal = emmeans(m5,
                   ~ TREATMENT*SEASONYEAR)
pairs(marginal,
      adjust="tukey")
cld(marginal,
    alpha=0.05,
    Letters=letters,  ### Use lower-case letters for .group
    adjust="tukey")

marginal = emmeans(m5,
                   ~ SEASONYEAR)
pairs(marginal,
      adjust="tukey")
cld(marginal,
    alpha=0.05,
    Letters=letters,  ### Use lower-case letters for .group
    adjust="tukey")
marginal = emmeans(m6,
                   ~ TREATMENT)
pairs(marginal,
      adjust="tukey")
cld(marginal,
    alpha=0.05,
    Letters=letters,  ### Use lower-case letters for .group
    adjust="tukey")
