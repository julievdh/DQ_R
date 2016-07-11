library(R.matlab)
library(agricolae)

setwd("~/Documents/MATLAB/DQ/DQ2013/Glides")
data <- readMat('Data_ReadNew.mat')
Cdmean <- data$Cd.mn
CDAS <- data$CDAS

# test for homogeneity of variance: Levene's test (better for non-normal data)
bartlett.test(Cdmean ~ data$Tag) # Method 1
bartlett.test(CDAS ~ data$Tag) # Method 2
bartlett.test(t(data$dur) ~ data$Tag) # Duration

# Since variance increases with tag condition, use nonparametric kruskal-wallis
# to test for effect of tag
kruskal.test(Cdmean ~ data$Tag) # Method 1
kruskal.test(CDAS ~ data$Tag) # Method 2
kruskal.test(t(data$dur)~ data$Tag) # Duration

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



