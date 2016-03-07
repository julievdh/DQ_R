library(R.matlab)
library(agricolae)

# trial mean lap times
trialmn <- c(22.4346153846154,    39.1156250000000,  32.2573684210526,
             25.2772727272727,	28.7019047619048,	26.5613043478261,	27.2952380952381,	
             42.2686666666667,	25.0495833333333,	35.0311111111111,	27.6572727272727,	
             26.2870833333333,	26.1421739130435,	38.8281250000000,	25.7356521739130,	
             35.3317647058824,	30.9170000000000,	29.2157142857143,	22.9676923076923,	
             40.8013333333333,	35.6823529411765,	25.6600000000000,	25.3225000000000,	
             27.0918181818182,	26.5308333333333,	26.0750000000000,	27.9263636363636,	
             27.5481818181818,	38.9487500000000,	35.3250000000000,	28.0836363636364)

# individual and tag condition are factors
Ind <- c(3,1,2,4,1,3,1,1,2,3,4,3,1,3,4,1,3,2,3,3,1,3,4,4,3,3,4,3,1,4,2)
# O = no tag; 1 = tag; 5 = tag + 4; 10 = boat, no tag; 11 = boat, tag; 13 = boat, tag + 2
Condition <- c(0,5,1,0,0,1,1,5,0,5,1,0,1,5,0,0,1,1,0,5,1,1,11,10,11,10,13,13,5,5,0)

# Make non-boat trials only
trialmn <- trialmn[Condition < 10]
Ind <- Ind[Condition < 10]
Condition <- Condition[Condition < 10]
Ind <- as.factor(Ind)
Condition <- as.factor(Condition)


# set up linear model
linear.model <- lm(trialmn ~ Ind + Condition)
aov <- anova(linear.model)

TukeyHSD(aov(linear.model))

# plot
plot(trialmn ~ Ind*Condition)

# Calculate percent increase in lap duration
(mean(trialmn[Condition==5])-mean(trialmn[Condition==0]))/mean(trialmn[Condition==0])
(mean(trialmn[Condition==5])-mean(trialmn[Condition==1]))/mean(trialmn[Condition==1])
