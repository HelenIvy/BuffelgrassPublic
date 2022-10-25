
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
#used log+1 transform m2b
m1 <- lmer(Individuals ~TREATMENT*SEASONYEAR + (1|BLOCKPLOT)+(1|BLOCK), data = DATABGIC)
summary (m1)

m2 <- lmer(SQRTIndividuals ~TREATMENT*SEASONYEAR + (1|BLOCKPLOT)+(1|BLOCK), data = DATABGIC)
summary (m2)
m2b <- lmer(LOGP1Individuals ~TREATMENT*SEASONYEAR + (1|BLOCKPLOT)+(1|BLOCK), data = DATABGIC)
summary (m2b)

m3 <- glmer(Individuals ~TREATMENT + (1|BLOCKPLOT) + (1 | BLOCK), 
             data= DATABGIC, family = poisson(link="log"))

#need zero inflated model with an adjustment for variance n- need interaction effect and block and plot in model - zinfl

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

summary(m11 <- zeroinfl(Individuals ~TREATMENT*SEASONYEAR, + (1|BLOCKPLOT)+(1|BLOCK), data=DATABGIC))
summary(m11 <- zeroinfl(Individuals ~TREATMENT*SEASONYEAR, + (1/BLOCK), data=DATABGIC))
summary(m14 <- zeroinfl(Individuals ~TREATMENT*SEASONYEAR, data=DATABGIC, dist = "poisson"))
library(car)
require(car)
Anova(m11,
      type="II",
      test="Chisq")

Anova(m14)

Anova(m2)
Anova(m2b)
Anova(m2c)
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
resids <- data.frame(DATABGIC, rc_resids, pm_resids, sc_resids)
head(resids) 
#plot_redres creates a plot (using ggplot2) of the residuals versus the fitted values given a model and a specified residual type. All residual types listed for redres work with plot_redres.

# creates a plot of the conditional studentized residuals versus the fitted values
plot_redres(m1, type = "std_cond")
#m1 residuals are skewed <

#residuals m2;

rc_resids <- compute_redres(m2)
pm_resids <- compute_redres(m2, type = "pearson_mar")
sc_resids <- compute_redres(m2, type = "std_cond")
resids <- data.frame(DATABGIC, rc_resids, pm_resids, sc_resids)
head(resids) 
plot_redres(m2, type = "std_cond")
#m2 still slightly v shaped

#residuals m2b;

rc_resids <- compute_redres(m2b)
pm_resids <- compute_redres(m2b, type = "pearson_mar")
sc_resids <- compute_redres(m2b, type = "std_cond")
resids <- data.frame(DATABGIC, rc_resids, pm_resids, sc_resids)
head(resids) 
plot_redres(m2b, type = "std_cond")





#m2b best but a little football shaped

#residuals m2c
rc_resids <- compute_redres(m2c)
pm_resids <- compute_redres(m2c, type = "pearson_mar")
sc_resids <- compute_redres(m2c, type = "std_cond")
resids <- data.frame(DATABGIC, rc_resids, pm_resids, sc_resids)
head(resids) 
plot_redres(m2c, type = "std_cond")
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

marginal = emmeans(m2c,
                   ~ TreatFreQ)
pairs(marginal,
      adjust="tukey")
cld(marginal,
    alpha=0.05,
    Letters=letters,  ### Use lower-case letters for .group
    adjust="tukey")
