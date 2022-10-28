
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
  PLOT<-factor(Plot)
  TREATMENT<-factor(Treatment)
  YEAR<-factor(Year)
  
})
#If use YEAR is categorical, if use Year is continous
summary(DATACostsIC)


#PLOT nested in BLOCK for the random variable for split plot design

library(redres)
require(lme4)


#NORMAL transformation looks ok
m1 <- lmer(Measuredcosts ~TREATMENT*YEAR + (1|PLOT)+(1|BLOCK), data =  DATACostsIC)
summary (m1)
Anova(m1)


#residuals m1;

rc_resids <- compute_redres(m1)
pm_resids <- compute_redres(m1, type = "pearson_mar")
sc_resids <- compute_redres(m1, type = "std_cond")
resids <- data.frame(DATACostsIC, rc_resids, pm_resids, sc_resids)
head(resids) 
plot_redres(m1, type = "std_cond")
#residuals m2;

rc_resids <- compute_redres(m2)
pm_resids <- compute_redres(m2, type = "pearson_mar")
sc_resids <- compute_redres(m2, type = "std_cond")
resids <- data.frame(DATACostsIC, rc_resids, pm_resids, sc_resids)
head(resids) 
plot_redres(m2, type = "std_cond")

#I ended up only including time and supplies and not hours in the MS because they were similar graphs;

#Post-hoc analysis can be conducted with the emmeans package.
library(multcompView)
require(multcompView)
library(multcomp)
require(multcomp)
library(emmeans)
require(emmeans)


marginal = emmeans(m1,
                   ~ TREATMENT)
pairs(marginal,
      adjust="tukey")
cld(marginal,
    alpha=0.05,
    Letters=letters,  ### Use lower-case letters for .group
    adjust="tukey")

marginal = emmeans(m1,
                   ~ YEAR)
pairs(marginal,
      adjust="tukey")
cld(marginal,
    alpha=0.05,
    Letters=letters,  ### Use lower-case letters for .group
    adjust="tukey")
