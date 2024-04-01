install.packages("ez")
library(ez); library(ggplot2); library(multcomp); library(nlme); library(pastecs); library(reshape); library(WRS2)
setwd("/Users/seongminpark/Desktop/DS/ANDYR/Data files")
bushData <- read.delim("Bushtucker.dat", header = TRUE)

# Melt() -> 긴 형식의 자료 만들기
longBush <- melt(bushData, id = "participant", measured = c("stick_insect", "kangaroo_testicle", "fish_eye", "witchetty_grub"))
names(longBush) <- c("Participant", "Animal", "Retch")
longBush$Animal <- factor(longBush$Animal, labels = c("Stick Insect", "Kangaroo Testicle", "Fish Eye", "Witchetty Grub"))
longBush <- longBush[order(longBush$Participant),]
longBush
str(longBush)

?gl
?factor

# Repeated Factor ANOVA ---------------------------------------------------

attach(longBush)
ggplot(data = longBush, aes(x = Animal, y = Retch)) + geom_boxplot()
ggplot(data = longBush, aes(x = Animal, y = Retch)) + 
  stat_summary(geom = "bar", fun.y = mean, fill = "white", color = "black") +
  stat_summary(geom = "pointrange", fun.data = mean_cl_boot) # 와 이거 쓰는 순서가 중요하구나
by(longBush$Retch, longBush$Animal, stat.desc)

cont1 <- c(1,-1,-1,1)
cont2 <- c(0,-1,1,0)
cont3 <- c(-1,0,0,1)
contrasts(longBush$Animal) <- cbind(cont1, cont2, cont3)
longBush$Animal
attr(longBush$Animal, "contrasts")

bushModel <- ezANOVA(data = longBush, dv = .(Retch), wid = .(Participant), within = .(Animal), detailed = TRUE, type = 3)
bushModel

pairwise.t.test(longBush$Retch, longBush$Animal, p.adjust.method = "bonferroni", paired = TRUE)
bushModel <- lme(Retch ~ Animal, random = ~1|Participant/Animal, data = longBush, method = "ML")
summary(bushModel)
baseline <- lme(Retch ~ 1, random = ~1|Participant/Animal, data = longBush, method = "ML")
baseline
anova(baseline, bushModel) # 이거는 두 '모형' 을 비교할 때 쓰나? aov랑 어케 다르지?
postHocs <- glht(bushModel, linfct = mcp(Animal = "Tukey"))
summary(postHocs)
confint(postHocs)


# Repeated Factor ANOVA 2 -------------------------------------------------

attData <- read.delim("Attitude.dat", header = TRUE)
View(attData)

longAtt <- melt(attData, id = "participant", 
                measured = c("beerpos", "beerneg", "beerneut", "winepos", "wineneg", "wineneut", "waterpos", "waterneg", "waterneut"))
names(longAtt) <- c("participant", "groups", "attitude")
longAtt$drink <- gl(3, 60, labels = c("Beer", "Wine", "Water"))
longAtt$imagery <- gl(3, 20, 180, labels = c("Positive", "Negative", "Neutral"))
longAtt[order(longAtt$participant), ]
ggplot(data = longAtt, aes(x = drink, y = attitude)) + geom_boxplot() + facet_wrap( ~ imagery)
by(longAtt$attitude, list(longAtt$drink, longAtt$imagery), stat.desc, basic = FALSE)

AlcoholvsWater <- c(1,1,-2)
BeervsWine <- c(-1,1,0)
contrasts(longAtt$drink) <- cbind(AlcoholvsWater, BeervsWine)
NegativevsOther <- c(1,-2,1)
PositivevsNeutral <- c(-1,0,1)
contrasts(longAtt$imagery) <- cbind(NegativevsOther, PositivevsNeutral)
attr(longAtt$drink, "contrasts")
attr(longAtt$imagery, "contrasts")
str(longAtt)

ggplot(data = longAtt, aes(x = imagery, y = attitude)) + 
  stat_summary(geom = "bar", fun.y = mean, alpha = 0.2) +
  stat_summary(geom = "pointrange", fun.data = mean_cl_boot) 

ggplot(data = longAtt, aes(x = drink, y = attitude, color = imagery)) + 
  stat_summary(geom = "point", fun.y = mean) +
  stat_summary(geom = "errorbar", fun.data = mean_cl_boot, width = 0.2) + 
  stat_summary(geom = "line", aes(group = imagery))
# ggplot2 마스터한다~~~

pairwise.t.test(longAtt$attitude, longAtt$groups, p.adjust.methods = "bonferroni", paired = TRUE)

baseline <- lme(attitude ~ 1, random = ~1|participant/drink/imagery, data = longAtt, method = "ML")
drinkModel <- lme(attitude ~ drink, random = ~1|participant/drink/imagery, data = longAtt, method = "ML")
drinkModel <- update(baseline, .~. + drink)
imageryModel <- update(drinkModel, .~. + imagery)
attModel <- update(imageryModel, .~. + drink:imagery)
anova(baseline, drinkModel, imageryModel, attModel)
summary(attModel)

