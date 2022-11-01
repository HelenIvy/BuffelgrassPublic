update.packages(ask = FALSE, checkBuilt = TRUE)
#read in data

library(readxl)
Measuredcosts <- read_excel("Data/Measuredcosts.xlsx")
View(Measuredcosts)

DATACosts <- Measuredcosts

# remove na in r - remove rows - na.omit function / option
ompleterecords <- na.omit(DATACosts)

#Set categorical factors for main effects:
 DATACosts <- within( DATACosts, {


  BLOCK<-factor(Block)
  BLOCKPLOT<-factor(BlockPlot)
  TREATMENT<-factor(Treatment)
  YEAR<-factor(Year)
  
})
#If use YEAR is categorical, if use Year is continous
summary( DATACosts)


#repeated measures(1\SUBJECT) for the random subject effect 

#for examples see https://bookdown.org/ndphillips/YaRrr/repeated-measures-anova-using-the-lme4-package.html and https://stats.stackexchange.com/questions/58745/using-lmer-for-repeated-measures-linear-mixed-effect-model
# fits an lmer model
library(readr)
library(redres)
require(lme4)
require(pscl)

m1c <- lmer(Measuredcosts ~TREATMENT*YEAR + (1|BLOCKPLOT)+(1|BLOCK), data =  DATACosts)
summary (m1c)
m2c <- lmer(SQRTmcosts ~TREATMENT*YEAR + (1|BLOCKPLOT)+(1|BLOCK), data =  DATACosts)
summary (m2c)



library(car)
require(car)
Anova(m1c)
Anova(m2c)


#studentized residuals
# Loads the library
install.packages("redres")
install.packages("scales") 
library(redres)
require(lme4)

#residuals m1c;
#looks bad;
rc_resids <- compute_redres(m1c)
pm_resids <- compute_redres(m1c, type = "pearson_mar")
sc_resids <- compute_redres(m1c, type = "std_cond")
resids <- data.frame(DATACosts, rc_resids, pm_resids, sc_resids)
head(resids) 
plot_redres(m1c, type = "std_cond")

#residuals m2c;
#looks good;
rc_resids <- compute_redres(m2c)
pm_resids <- compute_redres(m2c, type = "pearson_mar")
sc_resids <- compute_redres(m2c, type = "std_cond")
resids <- data.frame(DATACosts, rc_resids, pm_resids, sc_resids)
head(resids) 
plot_redres(m2c, type = "std_cond")


library(emmeans)
require(emmeans)
library(multcomp)
require(multcomp)



#posthoc differences m2 - treatment and year significant

marginal = emmeans(m2c,
                   ~ TREATMENT*YEAR)
pairs(marginal,
      adjust="tukey")
cld(marginal,
    alpha=0.05,
    Letters=letters,  ### Use lower-case letters for .group
    adjust="tukey")

marginal = emmeans(m2c,
                   ~ TREATMENT)
pairs(marginal,
      adjust="tukey")
cld(marginal,
    alpha=0.05,
    Letters=letters,  ### Use lower-case letters for .group
    adjust="tukey")

marginal = emmeans(m2c,
                   ~ YEAR)
pairs(marginal,
      adjust="tukey")
cld(marginal,
    alpha=0.05,
    Letters=letters,  ### Use lower-case letters for .group
    adjust="tukey")

