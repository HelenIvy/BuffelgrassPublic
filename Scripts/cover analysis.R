
#read in data
library(readxl)
library(readxl)
library(readxl)
Plant_community_cover <- read_excel("Data/Plant community cover.xlsx", 
                                    sheet = "Cut")
View(Plant_community_cover)
#in this data set, 2021-2022 cover for invaded control plots is included, but PECI cover is n/a for those plots. To compare native plant community, there is "nativecovernocontrol" which also includes those plots for those years as na

DATACoverNO <- Plant_community_cover

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
m2 <- lmer(SQRTPECIcover ~TREATMENT*YEAR + (1|BLOCKPLOT)+(1|BLOCK), data =  DATACoverNO)
summary (m2)
#native total, native annual, non-native, and P. ciliare standing dead cover were square root transformed to meet assumptions of normality. 

m4 <- lmer(SQRTNativecover ~TREATMENT*YEAR + (1|BLOCKPLOT)+(1|BLOCK), data =  DATACoverNO)
summary (m4)
m4b <- lmer(SQRTNativecoverNOcnt2122 ~TREATMENT*YEAR + (1|BLOCKPLOT)+(1|BLOCK), data =  DATACoverNO)
summary (m4b)

m5 <- lmer(nativeperennialcover ~TREATMENT*YEAR + (1|BLOCKPLOT)+(1|BLOCK), data =  DATACoverNO)
summary (m5)


m8 <- lmer(SQRTnativeannualcover  ~TREATMENT*YEAR + (1|BLOCKPLOT)+(1|BLOCK), data =  DATACoverNO)
summary (m8)

m10 <- lmer(SQRTNNcovernoPECI  ~TREATMENT*YEAR + (1|BLOCKPLOT)+(1|BLOCK), data =  DATACoverNO)
summary (m10)
m11 <- lmer(SQRTPECIStandingdead  ~TREATMENT*YEAR + (1|BLOCKPLOT)+(1|BLOCK), data =  DATACoverNO)
summary (m11)

library(car)
require(car)
Anova(m1)
Anova(m2)
Anova(m3)
Anova(m4)
Anova(m4b)
Anova(m5)
Anova(m6)
Anova(m7)
Anova(m8)
Anova(m9)
Anova(m10)
Anova(m11)

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
resids <- data.frame(DATACoverNO, rc_resids, pm_resids, sc_resids)
head(resids) 
plot_redres(m2, type = "std_cond")

#residuals m3;

rc_resids <- compute_redres(m3)
pm_resids <- compute_redres(m3, type = "pearson_mar")
sc_resids <- compute_redres(m3, type = "std_cond")
resids <- data.frame(DATACoverNO, rc_resids, pm_resids, sc_resids)
head(resids) 
plot_redres(m3, type = "std_cond")

#residuals m4;

rc_resids <- compute_redres(m4)
pm_resids <- compute_redres(m4, type = "pearson_mar")
sc_resids <- compute_redres(m4, type = "std_cond")
resids <- data.frame(DATACoverNO, rc_resids, pm_resids, sc_resids)
head(resids) 
plot_redres(m4, type = "std_cond")

#residuals m5;
#looks better than the sqrt transform;

rc_resids <- compute_redres(m5)
pm_resids <- compute_redres(m5, type = "pearson_mar")
sc_resids <- compute_redres(m5, type = "std_cond")
resids <- data.frame(DATACoverNO, rc_resids, pm_resids, sc_resids)
head(resids) 
plot_redres(m5, type = "std_cond")

#residuals m6;

rc_resids <- compute_redres(m6)
pm_resids <- compute_redres(m6, type = "pearson_mar")
sc_resids <- compute_redres(m6, type = "std_cond")
resids <- data.frame(DATACoverNO, rc_resids, pm_resids, sc_resids)
head(resids) 
plot_redres(m6, type = "std_cond")

#residuals m7;
#looks terrible;
rc_resids <- compute_redres(m7)
pm_resids <- compute_redres(m7, type = "pearson_mar")
sc_resids <- compute_redres(m7, type = "std_cond")
resids <- data.frame(DATACoverNO, rc_resids, pm_resids, sc_resids)
head(resids) 
plot_redres(m7, type = "std_cond")

#residuals m8;
#looks great;
rc_resids <- compute_redres(m8)
pm_resids <- compute_redres(m8, type = "pearson_mar")
sc_resids <- compute_redres(m8, type = "std_cond")
resids <- data.frame(DATACoverNO, rc_resids, pm_resids, sc_resids)
head(resids) 
plot_redres(m8, type = "std_cond")

#residuals m9;
rc_resids <- compute_redres(m9)
pm_resids <- compute_redres(m9, type = "pearson_mar")
sc_resids <- compute_redres(m9, type = "std_cond")
resids <- data.frame(DATACoverNO, rc_resids, pm_resids, sc_resids)
head(resids) 
plot_redres(m9, type = "std_cond")

#residuals m10;
rc_resids <- compute_redres(m10)
pm_resids <- compute_redres(m10, type = "pearson_mar")
sc_resids <- compute_redres(m10, type = "std_cond")
resids <- data.frame(DATACoverNO, rc_resids, pm_resids, sc_resids)
head(resids) 
plot_redres(m10, type = "std_cond")

#residuals m11;
rc_resids <- compute_redres(m11)
pm_resids <- compute_redres(m11, type = "pearson_mar")
sc_resids <- compute_redres(m11, type = "std_cond")
resids <- data.frame(DATACoverNO, rc_resids, pm_resids, sc_resids)
head(resids) 
plot_redres(m11, type = "std_cond")

library(emmeans)
require(emmeans)
library(multcomp)
require(multcomp)



#posthoc differences m1 - treatment and year significant
marginal = emmeans(m2,
                   ~ TREATMENT*YEAR)
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

marginal = emmeans(m4,
                   ~ TREATMENT)
pairs(marginal,
      adjust="tukey")
cld(marginal,
    alpha=0.05,
    Letters=letters,  ### Use lower-case letters for .group
    adjust="tukey")
marginal = emmeans(m4,
                   ~ TREATMENT*YEAR)
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
marginal = emmeans(m5,
                   ~ TREATMENT)
pairs(marginal,
      adjust="tukey")
cld(marginal,
    alpha=0.05,
    Letters=letters,  ### Use lower-case letters for .group
    adjust="tukey")

marginal = emmeans(m8,
                   ~ YEAR)
pairs(marginal,
      adjust="tukey")
cld(marginal,
    alpha=0.05,
    Letters=letters,  ### Use lower-case letters for .group
    adjust="tukey")
marginal = emmeans(m10,
                   ~ YEAR)
pairs(marginal,
      adjust="tukey")
cld(marginal,
    alpha=0.05,
    Letters=letters,  ### Use lower-case letters for .group
    adjust="tukey")
