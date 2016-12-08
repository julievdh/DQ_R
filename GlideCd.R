library(R.matlab)
library(agricolae)
require(PMCMR)
library(xlsx)

setwd("~/Documents/MATLAB/DQ/DQ2013/Glides")
data <- readMat('Data_ReadNew.mat')
Cdmean <- data$Cd.mn
CDAS <- data$CDAS
setwd("~/Documents/R/DQ_R")
newdata <- read.xlsx("drag_coefficients_table.xlsx", 2)  # read 2nd sheet

# test for homogeneity of variance: Levene's test (better for non-normal data)
bartlett.test(Cdmean ~ data$Tag) # Method 1
bartlett.test(CDAS ~ data$Tag) # Method 2
bartlett.test(t(data$dur) ~ data$Tag) # Duration

bartlett.test(CdCorr ~ Tag, data = newdata)

# Since variance increases with tag condition, use nonparametric kruskal-wallis
# to test for effect of tag
kruskal.test(Cdmean ~ data$Tag) # Method 1
kruskal.test(CDAS ~ data$Tag) # Method 2
posthoc.kruskal.dunn.test(x = CDAS, g = data$Tag,p.adjust.method = "none") # post-hoc dunn test
kruskal.test(t(data$dur)~ data$Tag) # Duration
posthoc.kruskal.dunn.test(x = t(data$dur), g = data$Tag,p.adjust.method = "none")

kruskal.test(CdCorr ~ Tag,data = newdata) # Newdata
posthoc.kruskal.dunn.test(x = newdata$CdCorr, g = newdata$Tag, p.adjust.method = "none") # post-hoc dunn test





# Any difference in initial velocity?
kruskal.test(data$sspeed~ data$Tag)
# Difference in final velocity?
kruskal.test(data$espeed~ data$Tag)


Tag <- as.factor(data$Tag)
foldchange <- matrix(NA, nrow=1, ncol=3)
foldchange[1] <- mean(Cdmean[which(Tag == 1)])/mean(Cdmean[which(Tag == 0)])
foldchange[2] <- mean(Cdmean[which(Tag == 3)])/mean(Cdmean[which(Tag == 0)])
foldchange[3] <- mean(Cdmean[which(Tag == 5)])/mean(Cdmean[which(Tag == 0)])
foldchange[4] <- mean(CDAS[which(Tag == 1)])/mean(CDAS[which(Tag == 0)])
foldchange[5] <- mean(CDAS[which(Tag == 3)])/mean(CDAS[which(Tag == 0)])
foldchange[6] <- mean(CDAS[which(Tag == 5)])/mean(CDAS[which(Tag == 0)])

plot(c(1,2,3),foldchange[4:6],col = 'blue')
points(c(1,2,3),foldchange[1:3],col = 'black')

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
# ANOVA with Tukey's HSD
CFD.linear.model <- lm(allCFD ~ allvel+tag)
CFD.aov <- anova(CFD.linear.model)

tukdur <-TukeyHSD(aov(CFD.linear.model))

## ANOVA FOR NEW DATA (don't have to use KW based on equal variance)
tag <- as.factor(newdata$Tag)
Ind <- as.factor(newdata$Ind)
CdCorr <- newdata$CdCorr

Cdlm <- lm(CdCorr ~ tag+Ind)
anova(Cdlm)
tukdur <-TukeyHSD(aov(Cdlm))
