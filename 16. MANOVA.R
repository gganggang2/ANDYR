library(car); library(ggplot2); library(MASS); library(pastecs); library(reshape); library(WRS2)
install.packages("mvoutlier")
install.packages("mvnormtest")
library(mvoutlier); library(mvnormtest)

ocdData <- read.delim("OCD.dat", header = TRUE)
View(ocdData)
ocdData$Group <- factor(ocdData$Group, levels = c("CBT", "BT", "No Treatment Control"), labels = c("CBT", "BT", "NT"))
str(ocdData)
p <- ggplot(data = ocdData, aes(x = Actions, y = Thoughts, group = Group))
p + facet_wrap( ~ Group) + geom_point() + geom_smooth(method = "lm")

ocdMelt<-melt(ocdData, id = c("Group"), measured = c("Actions", "Thoughts"))
names(ocdMelt)<-c("Group", "Outcome_Measure", "Frequency")
ocdBar <- ggplot(ocdMelt, aes(Group, Frequency, fill = Outcome_Measure))
ocdBar + stat_summary(fun.y = mean, geom = "bar", position = "dodge") + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", position=position_dodge(width=0.90), width = 0.2) + 
  labs(x = "Treatment Group", y = "Number of Thoughts/Actions", fill = "Outcome Measure") + 
  scale_y_continuous(breaks = seq(0, 20, by = 2))

ocdBar + geom_boxplot()
by(ocdData$Actions, ocdData$Group, stat.desc, basic = FALSE)
by(ocdData$Thoughts, ocdData$Group, stat.desc, basic = FALSE)

by(ocdData[, 2:3], ocdData$Group, cov)
cbt <- ocdData[1:10, 2:3]
cbt
mshapiro.test(t(cbt))
aq.plot(ocdData[, 2:3])
outlierm <- ocdData[-26,]
rmnt <- outlierm[21:29, 2:3]
mshapiro.test(t(rmnt))

# contrast ----------------------------------------------------------------

contrasts(ocdData$Group) <- contr.treatment(3, base = 3)

# 대비 직접 설정
CBT_vs_NT <- c(1,0,0)
BT_vs_NT <- c(0,1,0)
contrasts(ocdData$Group) <- cbind(CBT_vs_NT, BT_vs_NT)

# MANOVA ------------------------------------------------------------------

outcome <- cbind(ocdData$Actions, ocdData$Thoughts)
ocdModel <- manova(outcome~Group, data = ocdData)
summary(ocdModel, intercept = TRUE, test = "Roy")
outcome
ocdData[, 2:3]

summary.aov(ocdModel)
??WRS

# DFA ---------------------------------------------------------------------

ocdDFA <- lda(Group ~ Actions + Thoughts, data = ocdData)
predict(ocdDFA)
plot(ocdDFA)
par(mfrow=c(1,1))
