
#read in data
library(readxl)
Measured_costsINV_CNTL <- read_excel("Data/Measured costsINV CNTL.xlsx")
View(Measured_costsINV_CNTL)


DATACostsIC <- Measured_costsINV_CNTL

# remove na in r - remove rows - na.omit function / option
ompleterecords <- na.omit(DATACostsIC)

#Set categorical factors for main effects:
 DATACostsIC <- within( DATACostsIC, {


  BLOCK<-factor(Block)
  BLOCKPLOT<-factor(BlockPlot)
  TREATMENT<-factor(Treatment)
  YEAR<-factor(Year)
  
})
#If use YEAR is categorical, if use Year is continous
summary( DATACostsIC)


#BLOCKPLOT nested in BLOCK for the random variable for split plot design

library(redres)
require(lme4)


#used sqrt for PECI cover
m1 <- lmer(Measuredcosts ~TREATMENT*YEAR + (1|BLOCKPLOT)+(1|BLOCK), data =  DATACostsIC)
summary (m1)
Anova(m1)
m2 <- lmer(SQRTmeasured ~TREATMENT*YEAR + (1|BLOCKPLOT)+(1|BLOCK), data =  DATACostsIC)
summary (m2)
Anova(m2)

#residuals m1;

rc_resids <- compute_redres(m1)
pm_resids <- compute_redres(m1, type = "pearson_mar")
sc_resids <- compute_redres(m1, type = "std_cond")
resids <- data.frame(DATACostsIC, rc_resids, pm_resids, sc_resids)
head(resids) 
plot_redres(m1, type = "std_cond")
#residuals m1;

rc_resids <- compute_redres(m2)
pm_resids <- compute_redres(m2, type = "pearson_mar")
sc_resids <- compute_redres(m2, type = "std_cond")
resids <- data.frame(DATACostsIC, rc_resids, pm_resids, sc_resids)
head(resids) 
plot_redres(m2, type = "std_cond")

#I ended up only including time and supplies and not hours in the MS because they were similar graphs;


require(glmmTMB)
require(car)
fit_zipoisson2 <- glmmTMB(Measuredcosts ~TREATMENT+YEAR
                         + (1|BLOCK)+(1|BLOCKPLOT),
                         data=DATACostsIC,
                         ziformula=~1,
                         family=poisson)

summary(fit_zipoisson2)

fit_zinbinom1 <- update(fit_zipoisson2,family=nbinom1)
summary(fit_zinbinom1)


fit_zinbinom2 <- update(fit_zipoisson2,family=nbinom2)
summary(fit_zinbinom2)


AICtab(fit_zipoisson2,fit_zinbinom2,fit_zinbinom1)
#best fit is zinbinom1



AICtab(fit_zipoisson2,fit_zinbinom2,fit_zinbinom1b)
#best fit is fit_zipoisson the binomial models did not converge

#using car anova https://cran.r-project.org/web/packages/glmmTMB/vignettes/model_evaluation.pdf
library(car)

Anova(fit_zipoisson2,type="III")
Anova(fit_zipoisson2)

#Post-hoc analysis can be conducted with the emmeans package.
library(multcompView)
require(multcompView)
library(multcomp)
require(multcomp)
library(emmeans)
require(emmeans)


marginal = emmeans(fit_zipoisson2,
                   ~ TREATMENT)
pairs(marginal,
      adjust="tukey")
cld(marginal,
    alpha=0.05,
    Letters=letters,  ### Use lower-case letters for .group
    adjust="tukey")

marginal = emmeans(fit_zipoisson2,
                   ~ YEAR)
pairs(marginal,
      adjust="tukey")
cld(marginal,
    alpha=0.05,
    Letters=letters,  ### Use lower-case letters for .group
    adjust="tukey")
