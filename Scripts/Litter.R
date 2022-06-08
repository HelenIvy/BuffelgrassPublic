
#read in data
library(readxl)
library(readxl)
library(readxl)
Litter <- read_excel("Data/Litter.xlsx")
View(Litter)


DATALitter <- Litter

# remove na in r - remove rows - na.omit function / option
ompleterecords <- na.omit(DATALitter)

#Set categorical factors for main effects:
 DATALitter <- within( DATALitter, {
  BLOCK<-factor(Block)
  BLOCKPLOT<-factor(BlockPlot)
  TREATMENT<-factor(Treatment)
  YEAR<-factor(Year)
  
  
})
#If use YEAR is categorical, if use Year is continous
summary( DATALitter)


#repeated measures(1\SUBJECT) for the random subject effect 

#for examples see https://bookdown.org/ndphillips/YaRrr/repeated-measures-anova-using-the-lme4-package.html and https://stats.stackexchange.com/questions/58745/using-lmer-for-repeated-measures-linear-mixed-effect-model
# fits an lmer model
library(lme4)
# Loads the library
library(redres)
require(lme4)


m1l <- lmer(Litter ~TREATMENT*YEAR + (1|BLOCKPLOT)+(1|BLOCK), data =  DATALitter)
summary (m1l)
m1lb <- lmer(Litter ~YEAR + (1|BLOCKPLOT)+(1|BLOCK), data =  DATALitter)
summary (m1lb)
m2l <- lmer(Logp1Litter ~TREATMENT*YEAR + (1|BLOCKPLOT)+(1|BLOCK), data =  DATALitter)
summary (m2l)
m3l <- lmer(SQRTLitter       ~TREATMENT*YEAR + (1|BLOCKPLOT)+(1|BLOCK), data =  DATALitter)
summary (m3l)

library(car)
require(car)
Anova(m1l)
Anova(m2l)
Anova(m3l)

#studentized residuals
# fits an lmer model
library(lme4)
# Loads the library
library(redres)
require(lme4)

# computes the default residuals (raw conditional)
rc_resids <- compute_redres(m1l)

# computes the Pearson marginal residuals
pm_resids <- compute_redres(m1l, type = "pearson_mar")

# computes the studentized conditional residuals
sc_resids <- compute_redres(m1l, type = "std_cond")

# puts the residuals in a data frame with observed response and prints the first six rows
resids <- data.frame(DATALitter, rc_resids, pm_resids, sc_resids)
head(resids) 
#plot_redres creates a plot (using ggplot2) of the residuals versus the fitted values given a model and a specified residual type. All residual types listed for redres work with plot_redres.

# creates a plot of the conditional studentized residuals versus the fitted values
plot_redres(m1l, type = "std_cond")


#residuals m2l;

rc_resids <- compute_redres(m2l)
pm_resids <- compute_redres(m2l, type = "pearson_mar")
sc_resids <- compute_redres(m2l, type = "std_cond")
resids <- data.frame(DATALitter, rc_resids, pm_resids, sc_resids)
head(resids) 
plot_redres(m2l, type = "std_cond")

#residuals m3l;

rc_resids <- compute_redres(m3l)
pm_resids <- compute_redres(m3l, type = "pearson_mar")
sc_resids <- compute_redres(m3l, type = "std_cond")
resids <- data.frame(DATALitter, rc_resids, pm_resids, sc_resids)
head(resids) 
plot_redres(m3l, type = "std_cond")


library(emmeans)
require(emmeans)
library(multcomp)
require(multcomp)



#posthoc differences m1 - treatment and year significant

marginal = emmeans(m1lb,
                   ~ YEAR)
pairs(marginal,
      adjust="tukey")
cld(marginal,
    alpha=0.05,
    Letters=letters,  ### Use lower-case letters for .group
    adjust="tukey")

