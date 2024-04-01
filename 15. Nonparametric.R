install.packages("clinfun")
install.packages("pgirmess")
library(clinfun); library(car); library(ggplot2); library(pastecs); library(pgirmess)
setwd("/Users/seongminpark/Desktop/DS/ANDYR/Data files")
drugData <- read.delim("Drug.dat", header = TRUE)

shapiro.test(subset(drugData$sundayBDI, drugData$drug == "Ecstasy"))
shapiro.test(subset(drugData$sundayBDI, drugData$drug == "Alcohol"))

by(drugData[,c(2:3)], drugData$drug, stat.desc, basic=FALSE, norm=TRUE)
leveneTest(drugData$sundayBDI, drugData$drug, center = "mean")
leveneTest(drugData$wedsBDI, drugData$drug, center = "mean")
drugData$drug <- as.factor(drugData$drug)
str(drugData$drug)

# Wilcox Test -------------------------------------------------------------

sunModel <- wilcox.test(sundayBDI ~ drug, data = drugData, exact = FALSE)
wedModel <- wilcox.test(wedsBDI ~ drug, data = drugData, exact = FALSE, correct = FALSE)
sunModel
wedModel

# cannot compute exact p-value with ties: 동순위 점수들이 있어서 그 접근방식을 사용할 수 없다
drugData$BDIdiff <- drugData$sundayBDI - drugData$wedsBDI
shapiro.test(subset(drugData$BDIdiff, drugData$drug == "Alcohol"))
shapiro.test(subset(drugData$BDIdiff, drugData$drug == "Ecstasy"))

alcoholData<-subset(drugData, drug == "Alcohol")
ecstasyData<-subset(drugData, drug == "Ecstasy")
alcoholModel<-wilcox.test(alcoholData$wedsBDI, alcoholData$sundayBDI,  paired = TRUE, correct= FALSE)
ecstasyModel<-wilcox.test(ecstasyData$wedsBDI, ecstasyData$sundayBDI, paired = TRUE, correct= FALSE)
alcoholModel
ecstasyModel

# kruskal-Wallis ----------------------------------------------------------

rank <- rank(c(drugData$sundayBDI, drugData$wedsBDI))
EcstasyRank <- rank[1:20]
AlcoholRank <- rank[21:40]
sum(EcstasyRank)
sum(AlcoholRank)
sum(1:40)

Soya <- read.delim("Soya.dat")
summary(Soya)
?attr
Soya$Soya <- as.factor(Soya$Soya)
levels(SoyaData$Soya)
SoyaData <- Soya
SoyaData$Soya <- factor(SoyaData$Soya, levels = levels(SoyaData$Soya)[c(4,1,2,3)])

attach(SoyaData)
kruskal.test(Sperm~Soya)
SoyaData$Ranks <- rank(SoyaData$Sperm)
SoyaData
by(SoyaData$Ranks, SoyaData$Soya, mean)

ggplot(SoyaData, aes(x = Soya, y = Sperm)) + 
  geom_boxplot()
kruskalmc(Sperm~Soya, data = SoyaData, cont = "two-tailed")

jonckheere.test(SoyaData$Sperm, as.numeric(SoyaData$Soya))
levels(SoyaData$Soya)

# Friedman's ANOVA --------------------------------------------------------

dietData<-read.delim("Diet.dat", header = TRUE)
dietCompleteCases <- na.omit(dietData)
friedman.test(as.matrix(dietData))
friedmanmc(as.matrix(dietData))

