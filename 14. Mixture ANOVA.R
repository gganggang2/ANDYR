library(ez); library(ggplot2); library(nlme); library(pastecs); library(reshape); library(tidyverse)
dataData <- read.delim("/Users/seongminpark/Desktop/DS/ANDYR/Data files/LooksOrPersonality.dat", header = TRUE)
head(dataData, 2)

# Long Format Data by melt()
speedData <- melt(dataData, id = c("participant", "gender"), 
                  measured = c("att_high", "av_high", "ug_high", "att_some", "av_some", "ug_some", "att_none", "av_none", "ug_none"))
head(speedData, 21)
names(speedData) <- c("participant", "gender", "groups", "dateRating")
speedData$personality <- gl(3, 60, labels = c("Charismatic", "Average", "Dullard"))
speedData$looks <- gl(3, 20, 180, labels = c("Attractive", "Average", "Ugly"))
speedData <- speedData[, c("participant", "gender", "groups", "dateRating", "looks", "personality")]

by(speedData$dateRating, list(speedData$looks, speedData$personality, speedData$gender), stat.desc, basic = FALSE)

SomevsNone <- c(1,1,-2)
HivsAv <- c(-1,1,0) # 1, -1, 0이어야 하는거 아냐?
contrasts(speedData$personality) <- cbind(SomevsNone, HivsAv)
AttvsUgly <- c(1,1,-2)
AttvsAv <- c(-1,1,0)
contrasts(speedData$looks) <- cbind(AttvsUgly, AttvsAv)
attr(speedData$looks, "contrasts")

AttvsAv <- c(1,0,0)
UglyvsAv <- c(0,0,1)
contrasts(speedData$looks) <- cbind(AttvsAv, UglyvsAv)
attr(speedData$looks, "contrasts")
HighvsAv <- c(1,0,0)
DullvsAv <- c(0,0,1)
contrasts(speedData$personality) <- cbind(HighvsAv, DullvsAv)

baseline <- lme(dateRating ~ 1, random = ~1|participant/looks/personality, data = speedData, method = "ML")
looksM <- update(baseline, .~. + looks)
genderM <- update(looksM, .~. + personality + gender)
looks_personality <- update(genderM, .~. + looks:gender + personality:gender + looks:personality)
speedDateModel <- update(looks_personality, .~. + looks:personality:gender)
anova(baseline, looksM, genderM, looks_personality, speedDateModel)
speedDateModel2 <- update(baseline, .~. + looks*personality*gender) # This is better!!!
anova(speedDateModel, speedDateModel2)

ggplot(data = speedData, aes(x = personality, y = dateRating)) + 
  stat_summary(geom = "pointrange", fun.data = mean_cl_boot) +
  stat_summary(geom = "bar", fun.y = mean, alpha = 0.5)

by(speedData$dateRating, speedData$personality, mean)

ggplot(data = speedData, aes(x = personality, y = dateRating, group = looks)) +
  stat_summary(geom = "errorbar", fun.data = mean_cl_boot, width = 0.2, aes(color = looks)) +
  stat_summary(geom = "line", fun.y = mean, aes(color = looks)) +
  stat_summary(geom = "point", fun.y = mean, aes(color = looks))

by(speedData$dateRating, list(speedData$looks,speedData$gender), mean)

ggplot(data = speedData, aes(x = personality, y = dateRating, group = looks)) +
  stat_summary(geom = "errorbar", fun.data = mean_cl_boot, width = 0.2, aes(color = looks)) +
  stat_summary(geom = "line", fun.y = mean, aes(color = looks)) +
  stat_summary(geom = "point", fun.y = mean, aes(color = looks)) +
  facet_wrap(~gender)

summary(speedDateModel2)