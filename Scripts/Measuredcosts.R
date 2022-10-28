
#read in data

library(readxl)
Measured_costs <- read_excel("Data/Measured costs.xlsx")
View(Measured_costs)


DATACosts <- Measured_costs

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
library(lme4)
# Loads the library
library(redres)
require(lme4)
library(glmmADMB)
require(glmmADMB)
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
#I ended up only including time and supplies in the MS because they were similar graphs and because the decimalhours was not normal, it had unequal variance and transforming did not work sufficiently;

m1c <- lmer(Timeandsupplies ~TREATMENT*YEAR + (1|BLOCKPLOT)+(1|BLOCK), data =  DATACosts)
summary (m1c)

m2c <- lmer(SQRTCosts ~TREATMENT*YEAR + (1|BLOCKPLOT)+(1|BLOCK), data =  DATACosts)
summary (m2c)



library(car)
require(car)
Anova(m1c)
Anova(m2c)


#studentized residuals
# fits an lmer model
library(lme4)
# Loads the library
library(redres)
require(lme4)
#residuals for m1c - looks good!;

# computes the default residuals (raw conditional)
rc_resids <- compute_redres(m1c)

# computes the Pearson marginal residuals
pm_resids <- compute_redres(m1c, type = "pearson_mar")

# computes the studentized conditional residuals
sc_resids <- compute_redres(m1c, type = "std_cond")

# puts the residuals in a data frame with observed response and prints the first six rows
resids <- data.frame(DATACosts, rc_resids, pm_resids, sc_resids)
head(resids) 
#plot_redres creates a plot (using ggplot2) of the residuals versus the fitted values given a model and a specified residual type. All residual types listed for redres work with plot_redres.

# creates a plot of the conditional studentized residuals versus the fitted values
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

