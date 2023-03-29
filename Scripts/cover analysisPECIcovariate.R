
#read in data
library(readxl)
Plant_community_coverPECIcovariate <- read_excel("Data/Plant community coverPECIcovariate.xlsx", 
                                                 sheet = "CUT2")
View(Plant_community_coverPECIcovariate)


#in this data set, 2021-2022 cover for invaded control plots is included, but PECI cover is n/a for those plots. 
#To compare native plant community, there is "nativecovernocontrol" which also includes those plots for those years as na
#2018 is removed and PECI baseline added

DATACoverNO <- Plant_community_coverPECIcovariate

# remove na in r - remove rows - na.omit function / option
ompleterecords <- na.omit(DATACoverNO)

#Set categorical factors for main effects:
 DATACoverNO <- within( DATACoverNO, {
  BLOCK<-factor(Block)
  BLOCKPLOT<-factor(BlockPlot)
  TREATMENT<-factor(Treatment)
  YEAR<-factor(Year)
  
  
})
#If use YEAR is categorical, if use Year is continous
summary( DATACoverNO)


#repeated measures(1\SUBJECT) for the random subject effect 

#for examples see https://bookdown.org/ndphillips/YaRrr/repeated-measures-anova-using-the-lme4-package.html and https://stats.stackexchange.com/questions/58745/using-lmer-for-repeated-measures-linear-mixed-effect-model
# fits an lmer model
library(lme4)
# Loads the library
library(redres)
require(lme4)


#used sqrt for PECI cover

m2a <- lmer(SQRTPECIcover ~TREATMENT*YEAR + (1|BLOCKPLOT)+(1|BLOCK), data =  DATACoverNO)
summary (m2a)
m2b <- lmer(SQRTPECIcover ~TREATMENT*YEAR + (1|BLOCK)+(1|BaselinePECI), data =  DATACoverNO)
summary (m2b)

#native total, native annual, non-native, and P. ciliare standing dead cover were square root transformed to meet assumptions of normality. 

library(car)
require(car)
Anova(m2a)
Anova(m2b)


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
resids <- data.frame(DATA$Stacked_inds_removed_added_within_season, rc_resids, pm_resids, sc_resids)
head(resids) 
#plot_redres creates a plot (using ggplot2) of the residuals versus the fitted values given a model and a specified residual type. All residual types listed for redres work with plot_redres.

# creates a plot of the conditional studentized residuals versus the fitted values
plot_redres(m1, type = "std_cond")


#residuals m2a;

rc_resids <- compute_redres(m2)
pm_resids <- compute_redres(m2, type = "pearson_mar")
sc_resids <- compute_redres(m2, type = "std_cond")
resids <- data.frame(DATACoverNO, rc_resids, pm_resids, sc_resids)
head(resids) 
plot_redres(m2a, type = "std_cond")


library(emmeans)
require(emmeans)
library(multcomp)
require(multcomp)



#posthoc differences m1 - treatment and year significant
marginal = emmeans(m2a,
                   ~ TREATMENT*YEAR)
pairs(marginal,
      adjust="tukey")
cld(marginal,
    alpha=0.05,
    Letters=letters,  ### Use lower-case letters for .group
    adjust="tukey")

marginal = emmeans(m2a,
                   ~ TREATMENT)
pairs(marginal,
      adjust="tukey")
cld(marginal,
    alpha=0.05,
    Letters=letters,  ### Use lower-case letters for .group
    adjust="tukey")
marginal = emmeans(m2a,
                   ~ YEAR)
pairs(marginal,
      adjust="tukey")
cld(marginal,
    alpha=0.05,
    Letters=letters,  ### Use lower-case letters for .group
    adjust="tukey")

marginal = emmeans(m2b,
                   ~ TREATMENT*YEAR)
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
marginal = emmeans(m2b,
                   ~ YEAR)
pairs(marginal,
      adjust="tukey")
cld(marginal,
    alpha=0.05,
    Letters=letters,  ### Use lower-case letters for .group
    adjust="tukey")