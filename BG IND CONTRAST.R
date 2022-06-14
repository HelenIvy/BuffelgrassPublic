
#read in data
library(readxl)
library(readxl)
library(readxl)
BG_Individuals <- read_excel("Z:/Shared/Field Institute/Research/1 Current Projects/NNP Pennisetum removal experiments/Data/Analysis/BG 2022/BG Individuals.xlsx")
View(BG_Individuals)

DATABGIC <- BG_Individuals

#Set categorical factors for main effects:
DATABGIC <- within(DATABGIC, {
  BLOCK<-factor(Block)
  BLOCKPLOT<-factor(BlockPlot)
  TREATMENT<-factor(Code)
  YEAR<-factor(year)
  SEASONYEAR<-factor(seasonyear)
  
})
#If use YEAR is categorical, if use Year is continous
summary(DATABGIC)


install.packages("contrast")
install.packages("dplyr")
library(contrast)
library(dplyr)
library(multcomp)

#try 1 from https://stats.oarc.ucla.edu/r/faq/how-can-i-test-contrasts-in-r/

m1 <- lm(LOGP1Individuals ~ factor(Code) * factor(seasonyear), data = DATABGIC)
summary(m1)
# step 2 can't get this one to work.
library(multcomp)
K <- matrix(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
              0, 0, 0, 0, 0, 0, 0 ,1, -1, 0, 
              0, 0))
t <- glht(m1, linfct = K)
summary(t)

#2 contrast statement between herbicide 2x and 3x using code from https://cran.r-project.org/web/packages/contrast/vignettes/contrast.html
library(contrast)
DATABGIC %>% 
  group_by(SEASONYEAR, TREATMENT) %>% 
  count()

lm_fit_1 <- lm(LOGP1Individuals ~ (SEASONYEAR + TREATMENT)^2, data = DATABGIC)
summary(lm_fit_1)

trt_effect<-
  contrast(lm_fit_1, 
           list(SEASONYEAR = "Spring2022", TREATMENT = "H1x1x"),
           list(SEASONYEAR = "Spring2022", TREATMENT = "H2x1x"))
print(trt_effect, X = TRUE)
#OR TRY THIS
trt_effect <-
  contrast(
    lm_fit_1,
    list(seasonyear = levels(DATABGIC$Spring2022), TREATMENT = "H1x1x"),
    list(seasonyear = levels(DATABGIC$Spring2022), TREATMENT = "H2x1x")
  )
#or my version
trt_effect <-
  contrast(
    lm_fit_1,
    list(SEASONYEAR = "SEASONYEARSpring2022", TREATMENT="TREATMENTH1x"),
    list(SEASONYEAR = "SEASONYEARSpring2022", TREATMENT="TREATMENTH1x1X")
  )