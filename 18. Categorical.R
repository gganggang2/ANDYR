install.packages("gmodels")
library(gmodels); library(MASS)

setwd("/Users/seongminpark/Desktop/DS/ANDYR/Data files")
catsData <- read.delim("cats.dat", header = TRUE)
head(catsData, 5)
str(catsData)

food <- c(10, 28)
affection <- c(114, 48)
catsTable <- cbind(food, affection)

# chi-sqaure cross table test ---------------------------------------------

CrossTable(catsData$Training, catsData$Dance, fisher = TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")
CrossTable(catsTable, fisher = TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")

# log linear --------------------------------------------------------------

CatReg <- read.delim("CatRegression.dat")
summary(lm(LnObserved ~ Training*Dance, data = CatReg))
summary(lm(LnObserved ~ Training + Dance, data = CatReg))

catsDogs <- read.delim("CatsandDogs.dat", header = TRUE)
catsDogs
justCats <- subset(catsDogs, Animal == "Cat")
justDogs <- subset(catsDogs, Animal == "Dog")

# 교차표 만들기
CrossTable(justCats$Training, justCats$Dance, sresid = TRUE, prop.t = FALSE, chisq = TRUE, fisher = TRUE, prop.c = FALSE, prop.chisq = TRUE, format = "SPSS")
CrossTable(justDogs$Training, justDogs$Dance, sresid = TRUE, prop.t = FALSE, chisq = TRUE, fisher = TRUE, prop.c = FALSE, prop.chisq = TRUE, format = "SPSS")

catTable <- xtabs(~ Training + Dance, data = justCats)
catTable
catSaturated <- loglm(~ Training + Dance + Training:Dance, data = catTable, fit = TRUE)
summary(catSaturated)
catNoInteraction <- loglm( ~ Training + Dance, data = catTable, fit = TRUE)
mosaicplot(catSaturated$fit, shade = TRUE, main = "Cats: Saturated Model")
summary(catNoInteraction)

CDCT <- xtabs(~ Animal + Training + Dance, data = catsDogs)
CDCT
caturated <- loglm(~ Animal * Training * Dance, data = CDCT)
summary(caturated)
threeway <- loglm(~ Animal + Training + Dance + Animal:Training + Animal:Dance + Dance:Training, data = CDCT)
threeway <- update(caturated, .~. - Animal:Training:Dance)
summary(threeway)
anova(caturated, threeway)
mosaicplot(CDCT, shade = TRUE, main = "Cats and Dogs")
