# Now with data from perspective filter from Alex

library(R.matlab)
library(agricolae)

# trial mean swimming speeds from perspective filter
trialmn <- c(3.5128,  3.3899, 2.5079,
             3.3292,	4.0356,	2.4800,	
             3.6582,	4.2392,	2.5079,	
             3.7376,	3.9828,	2.5324)

# individual and tag condition are factors
# Kolohe = 1, Liko = 2, Lono = 3, Nainoa = 4
Ind <- c(1,1,1,2,2,2,3,3,3,4,4,4)
# O = no tag; 1 = tag; 5 = tag + 4;
Condition <- c(1,0,5,1,0,5,1,0,5,1,0,5)

Indv <- as.factor(Ind)
Cond <- as.factor(Condition)


# set up linear model
linear.model <- lm(trialmn ~ Indv + Cond)
aov <- anova(linear.model)

TukeyHSD(aov(linear.model))

# plot
plot(trialmn ~ Indv*Cond)

# Calculate percent slow down
(mean(trialmn[Cond==5])-mean(trialmn[Cond==0]))/mean(trialmn[Cond==0])
(mean(trialmn[Cond==5])-mean(trialmn[Cond==1]))/mean(trialmn[Cond==1])


#####################

# set up linear model
linear.model <- lm(trialmn ~ Ind + Cond)
aov <- anova(linear.model)

TukeyHSD(aov(linear.model))

# plot
plot(trialmn ~ Ind*Cond)

## Make Boat Trials Only
trialmnB <- trialmn[Condition > 9]
IndB <- as.factor(Ind[Condition > 9])
CondB <- as.factor(Condition[Condition > 9])


# set up linear model
linear.model <- lm(trialmnB ~ IndB + CondB)
aov <- anova(linear.model)

TukeyHSD(aov(linear.model))

# plot
plot(trialmnB ~ IndB*CondB)