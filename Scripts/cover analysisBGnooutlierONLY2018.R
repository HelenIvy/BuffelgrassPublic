
#read in data
library(readxl)
library(readxl)
Plant_community_cover_exportNOoutlier2018ONLY <- read_excel("Z:/Shared/Field Institute/Research/1 Current Projects/NNP Pennisetum removal experiments/Data/Analysis/BG 2022/Plant community cover exportNOoutlier2018ONLY.xlsx")
View(Plant_community_cover_exportNOoutlier2018ONLY)


DATA2018 <- Plant_community_cover_exportNOoutlier2018ONLY

# remove na in r - remove rows - na.omit function / option
ompleterecords <- na.omit(DATA2018)

#Set categorical factors for main effects:
 DATA2018 <- within( DATA2018, {
  BLOCK<-factor(Block)
  BLOCKPLOT<-factor(BlockPlot)
  TREATMENT<-factor(Treatment)
  YEAR<-factor(Year)
  
  
})
#If use YEAR is categorical, if use Year is continous
summary( DATA2018)


#repeated measures(1\SUBJECT) for the random subject effect 

#for examples see https://bookdown.org/ndphillips/YaRrr/repeated-measures-anova-using-the-lme4-package.html and https://stats.stackexchange.com/questions/58745/using-lmer-for-repeated-measures-linear-mixed-effect-model
# fits an lmer model
library(lme4)
# Loads the library
library(redres)
require(lme4)


m1 <- lmer(PECICover ~TREATMENT +(1|BLOCK), data =  DATA2018)
summary (m1)
m2 <- lmer(SQRTPECIcover ~TREATMENT +(1|BLOCK), data =  DATA2018)
summary (m2)

library(car)
require(car)
Anova(m1)
Anova(m2)


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


#residuals m2;

rc_resids <- compute_redres(m2)
pm_resids <- compute_redres(m2, type = "pearson_mar")
sc_resids <- compute_redres(m2, type = "std_cond")
resids <- data.frame(DATA$Stacked_inds_removed_added_within_season, rc_resids, pm_resids, sc_resids)
head(resids) 
plot_redres(m2, type = "std_cond")



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

marginal = emmeans(m2,
                   ~ TREATMENT)
pairs(marginal,
      adjust="tukey")
cld(marginal,
    alpha=0.05,
    Letters=letters,  ### Use lower-case letters for .group
    adjust="tukey")

marginal = emmeans(m5,
                   ~ YEAR)
pairs(marginal,
      adjust="tukey")
cld(marginal,
    alpha=0.05,
    Letters=letters,  ### Use lower-case letters for .group
    adjust="tukey")

marginal = emmeans(m9,
                   ~ YEAR)
pairs(marginal,
      adjust="tukey")
cld(marginal,
    alpha=0.05,
    Letters=letters,  ### Use lower-case letters for .group
    adjust="tukey")
