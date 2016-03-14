# set directory
setwd("~/Documents/NOPPTagDrag/DolphinQuest2013/Glides/")

# Import file
dat <- read.csv("GlideSuccess.csv",header=TRUE)

library(ggplot2)
library(plyr)
library(reshape2)

data <- data.frame(dat$Individual.1[1:8], dat$Condition.1[1:8], dat$Overall.Rate[1:8])
colnames(data) <- c("ID","Tag","Success")

data$ID <- as.factor(data$ID)
data$Tag <- as.factor(c("No Tag","No Tag","Tag","Tag","Tag + 4","Tag + 4","Tag + 8","Tag + 8"))

setwd("~/Documents/R/DQ_R/AnalysisFigures") # set directory to figures

pdf("GlideSuccess.pdf",width = 5,height = 3)

ggplot(data, aes(factor(Tag),Success, fill = ID)) + 
  theme_bw() +
  geom_bar(stat = "identity", position = "dodge", colour = "black") +
  scale_fill_manual(values = c("grey","white"),name = " ", breaks = c(1,2), labels = c("Hoku","Liho")) +
  theme(legend.position=c(0.9, 0.8)) +
  xlab(" ") +
  ylab("Success Rate") +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14))

dev.off()
