setwd("/Users/seongminpark/Desktop/DS/ANDYR/Data files")
library(car); library(compute.es); library(ggplot2); library(multcomp);
library(pastecs); library(reshape); library(WRS2)

gogglesData <- read.csv("goggles.csv", header = TRUE)
str(gogglesData)
gogglesData$alcohol <- factor(gogglesData$alcohol, levels = c("None", "2 Pints", "4 Pints"))
gogglesData$gender <- factor(gogglesData$gender, levels = c("Female", "Male"))

# Create Factor Variable --------------------------------------------------

gender <- gl(2, 24, labels = c("Female", "Male"))
alcohol <- gl(3, 8, 48, labels = c("None", "2 Pints", "4 Pints"))


# 2-way ANOVA -------------------------------------------------------------

attach(gogglesData)

line <- ggplot(gogglesData, aes(alcohol, attractiveness, colour = gender))
line + stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group= gender)) + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + 
  labs(x = "Alcohol Consumption", y = "Mean Attractiveness of Date (%)", colour = "Gender") 

line + facet_wrap( ~ gender) + geom_boxplot()

by(gogglesData$attractiveness, gogglesData$gender, stat.desc)
by(gogglesData$attractiveness, gogglesData$alcohol, stat.desc)
by(gogglesData$attractiveness, list(gogglesData$gender, gogglesData$alcohol), stat.desc)

leveneTest(gogglesData$attractiveness, gogglesData$gender, center = median)
leveneTest(gogglesData$attractiveness, gogglesData$alcohol, center = median)
leveneTest(gogglesData$attractiveness, interaction(gogglesData$alcohol, gogglesData$gender), center = median)

contrasts(gogglesData$alcohol) <- cbind(c(2,-1,-1), c(0,-1,1))
contrasts(gogglesData$gender) <- c(-1,1)

Model <- aov(attractiveness ~ gender + alcohol + gender:alcohol, data = gogglesData)
Anova(Model, type = "III")

ggplot(gogglesData, aes(alcohol, attractiveness, fill = alcohol)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar")

ggplot(gogglesData, aes(gender, attractiveness, fill = gender)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar")

summary.lm(Model)
plot(Model)

# simple variable factorization
gogglesData$simple <- gl(6,8)
gogglesData$simple <- factor(gogglesData$simple, levels = c(1:6), 
                             labels = c("F_None", "F_2pints", "F_4pints", "M_none", "M_2pints", "M_4pints"))
head(gogglesData, 10)

alcEffect1<-c(-2, 1, 1, -2, 1, 1)
alcEffect2<-c(0, -1, 1, 0, -1, 1)
gender_none<-c(-1, 0, 0, 1, 0, 0)
gender_twoPint<-c(0, -1, 0, 0, 1, 0)
gender_fourPint<-c(0, 0, -1, 0, 0, 1)

simpleEff <- cbind(alcEffect1, alcEffect2, gender_none, gender_twoPint, gender_fourPint)
contrasts(gogglesData$simple) <- simpleEff
gogglesData$simple

simpleEffectModel <- aov(attractiveness ~ simple, data = gogglesData)
summary.lm(simpleEffectModel)

# Robust 2-way ANOVA ------------------------------------------------------

gogglesData$row <- rep(1:8, 6)
gogglesData
gogglesMelt <- melt(gogglesData, id = c("row", "gender", "alcohol"),
                    measured = c("attractiveness"))
gogglesMelt <- gogglesMelt[1:48,]
gogglesWide <- cast(gogglesMelt, row ~ gender + alcohol)
gogglesWide$row <- NULL
gogglesWide

t2way(attractiveness ~ gender*alcohol, data = gogglesData)
pbad2way(attractiveness ~ gender*alcohol, data = gogglesData)
mcp2atm(attractiveness ~ gender*alcohol, data = gogglesData)
mcp2a(attractiveness ~ gender*alcohol, data = gogglesData)
