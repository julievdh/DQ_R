
library(agricolae)
require(PMCMR)
library(readxl)

setwd("~/Documents/R/DQ_R")
data <- read_excel('Cd Calculations - with success rate.xlsx',3)

# test for homogeneity of variance: Levene's test (better for non-normal data)
bartlett.test(data$CD ~ data$Condition) # Method 1
# bartlett.test(CDAS ~ data$Tag) # Method 2
bartlett.test(data$Duration ~ data$Condition) # Duration
bartlett.test(data$Uo ~ data$Condition) # initial velocity

# equal variance in all parameters in different conditions. 

# Multiple comparison for CFD data
# from GlideDataPlots.m
vel <- c(1,2,3,4,5,6)
CFD_Cd_notag <- c(0.0127,0.0106,0.0097,0.0094,0.0093,0.0092)
CFD_Cd_tag <- c(0.0132,0.0111,0.0102,0.0099,0.0098,0.0097)
CFD_Cd_tag2 <- c(0.0159,0.0144,0.0132,0.0129,0.0128,0.0127)
CFD_Cd_tag4 <- c(0.0186,0.0169,0.0162,0.0160,0.0157,0.0158)

allvel <- as.factor(rep(vel,4))
tag <- as.factor(c(0,0,0,0,0,0,1,1,1,1,1,1,2,2,2,2,2,2,4,4,4,4,4,4))
allCFD <- c(CFD_Cd_notag,CFD_Cd_tag,CFD_Cd_tag2,CFD_Cd_tag4)
# ANOVA with Tukey's HSD on CFD data
CFD.linear.model <- lm(allCFD ~ allvel+tag)
CFD.aov <- anova(CFD.linear.model)

tukdur <-TukeyHSD(aov(CFD.linear.model))

#####-----  ANOVA for experimental data: drag coefficient
tag <- as.factor(data$Condition)
Ind <- as.factor(data$Animal)
CdCorr <- data$CD

Cdlm <- lm(CdCorr ~ tag+Ind)
anova(Cdlm)
tukCd <-TukeyHSD(aov(Cdlm))

Durlm <- lm(data$Duration ~ tag+Ind) 
anova(Durlm)
Ulm <- lm(data$Uo ~ tag+Ind)
anova(Ulm)