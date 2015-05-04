library(R.matlab)
library(agricolae)

setwd("~/Documents/MATLAB/DQ/DQ2013/Glides")
data <- readMat('GlideData_Redo.mat')

# drag coefficients measured from video from two separate points on the body
Cd1 <- data$mav[,3]
Cd2 <- data$mav[,4]
Cdboth <- cbind(Cd1,Cd2)
Cdmean <- rowMeans(Cdboth)

# individual and tag condition are factors
Ind <- as.factor(data$mav[,2])
Tag <- as.factor(data$mav[,1])

# set up linear model
linear.model <- lm(Cdmean ~ Ind + Tag)
aov <- anova(linear.model)

linear.modelduration <- lm(data$dur ~ Ind + Tag)
aov2 <- anova(linear.modelduration)

TukeyHSD(aov(linear.modelduration))

require(vioplot)
require(devtools)
require(digest)
require(beanplot)

setwd("~/Documents/R/DQ2013/AnalysisFigures") # set directory to figures

pdf("GlideCd_beanplot.pdf",width = 5,height = 5)
op <- par(mfrow = c(2,1),
          oma = c(3,0,0,0) + 0.1,
          mar = c(0,4,3,0) + 0.1)

beanplot(Cdmean ~ Ind*Tag, ll = 0.04,
         side = "both", ylab="Drag Coefficient, Cd",
         col = list("grey", c("white", "black")),
         axes=F, beanlinewd = 1.5)
axis(1, at=c(1, 2, 3, 4),  labels=c("No Tag", "Tag", "Tag + 2", "Tag + 4"))
axis(2)
legend("topleft", fill = c("grey", "white"),
       legend = c("Hoku", "Liho"), box.lty=0)



beanplot(data$dur ~ Ind*Tag, ll = 0.04,
         side = "both", ylab="Glide Duration (sec)",
         col = list("grey", c("white", "black")),
         axes=F, ylim = c(-20,120),
         beanlinewd = 1.5)
text(c(1,2,3,4),c(-10,-10,-10,-10),labels = c("a","a","ab","b"))
axis(1, at=c(1, 2, 3, 4),  labels=c("No Tag", "Tag", "Tag + 2", "Tag + 4"))
axis(2, at = c(0,60,120))


par(op)
dev.off()

# test for homogeneity of variance: Levene's test (better for non-normal data)
bartlett.test(Cdmean ~ Tag)
bartlett.test(data$dur ~ Tag)

# Since variance increases with tag condition, use nonparametric kruskal-wallis
# to test for effect of tag
kruskal.test(Cdmean ~ Tag)
kruskal.test(data$dur ~ Tag)

foldchange <- matrix(NA, nrow=1, ncol=3)
foldchange[1] <- mean(Cdmean[which(Tag == 1)])/mean(Cdmean[which(Tag == 0)])
foldchange[2] <- mean(Cdmean[which(Tag == 3)])/mean(Cdmean[which(Tag == 0)])
foldchange[3] <- mean(Cdmean[which(Tag == 5)])/mean(Cdmean[which(Tag == 0)])
plot(c(1,2,3),foldchange)