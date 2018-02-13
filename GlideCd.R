
library(agricolae)
library(readxl)

setwd("~/Documents/R/DQ_R")
data <- read_excel('Cd Calculations - with success rate.xlsx',3)

# test for homogeneity of variance: Levene's test (better for non-normal data)
bartlett.test(data$CD ~ data$Condition) # Method 1
# bartlett.test(CDAS ~ data$Tag) # Method 2
bartlett.test(data$Duration ~ data$Condition) # Duration
bartlett.test(data$Uo ~ data$Condition) # initial velocity

# equal variance in all parameters in different conditions. 

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

# Multiple comparison for CFD data
data <- read_excel('Cd Calculations - with success rate.xlsx',6)

tag <- as.factor(data$Condition)

# ANOVA with Tukey's HSD on CFD data
CFD.linear.model <- lm(data$Cd ~ tag)
CFD.aov <- anova(CFD.linear.model)

tukdur <-TukeyHSD(aov(CFD.linear.model))

#### -- Success rate
data <- read_excel('Cd Calculations - with success rate.xlsx',7)

ggplot(data, aes(factor(Condition),Rate, fill = Animal)) + 
  theme_bw() +
  geom_bar(stat = "identity", position = "dodge", colour = "black") +
  scale_fill_manual(values = c("grey","white"),name = " ", breaks = c(1,2), labels = c("Hoku","Liho")) +
  theme(legend.position=c(0.9, 0.9)) +
  xlab(" ") +
  ylab("Success Rate") +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14))
