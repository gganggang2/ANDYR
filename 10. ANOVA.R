setwd("/Users/seongminpark/Desktop/DS/ANDYR/Data files")
dummy <- read.delim("dummy.dat")
str(dummy)
attach(dummy)
summary(lm(libido ~dummy1 + dummy2))
Contrast <- read.delim("Contrast.dat")
detach(dummy); attach(Contrast)
summary(lm(libido~dummy1+dummy2))

install.packages("multcomp")
library(multcomp)
library(car)
library(ggplot2)
library(pastecs)
library(WRS2)

libido <- c(3,2,1,1,4,5,2,4,2,3,7,4,5,3,6)
dose <- gl(3,5, labels = c("Placebo", "Low Dose", "High Dose"))
viagraData <- data.frame(dose, libido)

# Bootstrap error bar: fun.data = data에 대한 function, fun.y = y에 대한 function, 이를 계산해서 plot함
line <- ggplot(viagraData, aes(dose, libido))
line + stat_summary(fun.y = mean, geom = "line", size = 1, aes(group=1), colour = "#FF6633") + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2, size = 0.75, colour = "#990000") + 
  stat_summary(fun.y = mean, geom = "point", size = 4, colour = "#990000") + 
  stat_summary(fun.y = mean, geom = "point", size = 3, colour = "#FF6633") + 
  labs(x = "Dose of Viagra", y = "Mean Libido")

# Descriptive Statistics -> by(변수, 그룹, 출력))
by(viagraData$libido, viagraData$dose, stat.desc)

# Levene Test
leveneTest(viagraData$libido, viagraData$dose, center = median)

# ANOVA
viagraModel <- lm(libido~dose, data = viagraData)
viagraModel <- aov(libido~dose, data = viagraData)
summary(viagraModel)
par(mfrow = c(2,2))
plot(viagraModel)

# Welch's F
oneway.test(libido ~ dose, data = viagraData)

# Robust ANOVA
viagraData
viagraWide <- unstack(viagraData, libido ~ dose)

# Contrast
summary.lm(viagraModel)
summary(viagraModel)
contrasts(viagraData$dose) <- contr.helmert(3)

contrast1 <- c(-2,1,1)
contrast2 <- c(0,-1,1)
contrasts(viagraData$dose) <- cbind(contrast1, contrast2)
viagraData$dose
summary.lm(aov(libido ~ dose, data = viagraData))

contrasts(viagraData$dose) <- contr.poly(3)
summary.lm(aov(libido~dose, data = viagraData))

# Post hoc
attach(viagraData)
pairwise.t.test(libido, dose, paired = FALSE, p.adjust.method = "bonferroni")
postHocs <- glht(viagraModel, linfct = mcp(dose = "Tukey"))
summary(postHocs)
confint(postHocs)
