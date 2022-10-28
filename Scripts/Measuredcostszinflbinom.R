
#read in data
library(readxl)

library(readxl)
Measured_costs <- read_excel("Data/Measured costs.xlsx")
View(Measured_costs)


DATACosts <- Measured_costs

# remove na in r - remove rows - na.omit function / option
ompleterecords <- na.omit(DATACosts)

#Set categorical factors for main effects:
 DATACosts <- within( DATACosts, {


  BLOCK<-factor(Block)
  PLOT<-factor(Plot)
  BLOCKPLOT<-factor(BlockPlot)
  TREATMENT<-factor(Treatment)
  YEAR<-factor(Year)
  
})
#If use YEAR is categorical, if use Year is continous
summary( DATACosts)

# remove na in r - remove rows - na.omit function / option
ompleterecords <- na.omit(DATACosts)
#repeated measures(1\SUBJECT) for the random subject effect 




#I ended up only including time and supplies and not hours in the MS because they were similar graphs;


require(glmmTMB)
require(car)
fit_zipoisson <- glmmTMB(Timeandsupplies~TREATMENT*YEAR
                         + (1|BLOCK)+(1|PLOT),
                         data=DATACosts,
                         ziformula=~1,
                         family=poisson)

summary(fit_zipoisson)

fit_zinbinom1b <- update(fit_zipoisson,family=nbinom1)
summary(fit_zinbinom1b)


fit_zinbinom2 <- update(fit_zipoisson,family=nbinom2)
summary(fit_zinbinom2)


AICtab(fit_zipoisson,fit_zinbinom2,fit_zinbinom1b)
#best fit is fit_zipoisson the binomial models did not converge

#using car anova https://cran.r-project.org/web/packages/glmmTMB/vignettes/model_evaluation.pdf
library(car)

Anova(fit_zipoisson,type="III")
Anova(fit_zipoisson)

#Post-hoc analysis can be conducted with the emmeans package.
library(multcompView)
require(multcompView)
library(multcomp)
require(multcomp)
library(emmeans)
require(emmeans)


marginal = emmeans(fit_zipoisson,
                   ~ TREATMENT)
pairs(marginal,
      adjust="tukey")
cld(marginal,
    alpha=0.05,
    Letters=letters,  ### Use lower-case letters for .group
    adjust="tukey")

marginal = emmeans(fit_zipoisson,
                   ~ YEAR)
pairs(marginal,
      adjust="tukey")
cld(marginal,
    alpha=0.05,
    Letters=letters,  ### Use lower-case letters for .group
    adjust="tukey")
