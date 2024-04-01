install.packages("car")
install.packages("compute.es")
install.packages("effects")
install.packages("WRS", repos = "http://R-Forge.R-project.org")
install.packages("pastecs")
library(car); library(compute.es); library(effects); library(ggplot2); library(multcomp); library(pastecs); library(WRS2)

# ANCOVA ------------------------------------------------------------------

setwd("/Users/seongminpark/Desktop/DS/ANDYR/Data files")
viagraData <- read.delim("ViagraCovariate.dat", header = TRUE)
attach(viagraData)

# 자가진단: Are there any fancier methods compared to this one?
by(viagraData$libido, viagraData$dose, FUN = mean)
by(viagraData$libido, viagraData$dose, FUN = sd)
by(viagraData$partnerLibido, viagraData$dose, FUN = mean)
by(viagraData$partnerLibido, viagraData$dose, FUN = sd)

ggplot(viagraData, aes(y = libido, group = dose)) + 
  geom_boxplot()
by(viagraData$libido, viagraData$dose, stat.desc)
by(viagraData$partnerLibido, viagraData$dose, stat.desc)

viagraData$dose <- as.factor(dose)
leveneTest(viagraData$libido, viagraData$dose, center = median)

summary(lm(partnerLibido~dose))
summary(aov(partnerLibido~dose))

# SHOCK: Order different, Result different! Because R compute the sum of square by the order of predict variables.
## BUT this is only the issue for the fitness, not the matter for coefficients.
viagraModel <- aov(libido~dose + partnerLibido) # dose first
summary(viagraModel)
summary(aov(libido~partnerLibido + dose)) # PartnerLibido first

contrasts(viagraData$dose) <- cbind(c(-2,1,1), c(0,-1,1))
viagraModel <- aov(libido ~ partnerLibido + dose, data = viagraData)
car::Anova(viagraModel, type = "III")

adjustedMeans <- effect("dose", viagraModel, se = TRUE)
summary.lm(viagraModel)
ggplot(viagraData, aes(x = partnerLibido, y = libido)) + 
  geom_point(size = 2) + 
  geom_smooth(method = lm, color = "black")

# Post Hoc
postHocs <- multcomp::glht(viagraModel, linfct = mcp(dose = "Tukey"))
summary(postHocs)
confint(postHocs)

plot(viagraModel)
summary(aov(libido~dose))

ggplot(data = viagraData, aes(x = partnerLibido, y = libido, fill = dose)) +
  geom_point() + 
  geom_smooth(method = lm, aes(color = dose))

hoRS <- update(viagraModel, .~. + partnerLibido:dose)
Anova(hoRS, type = "III")
summary(hoRS)

# robust ANCOVA -----------------------------------------------------------

invisibilityData <- read.delim("CloakofInvisibility.dat", header = TRUE)
invisibilityData$cloak <- factor(invisibilityData$cloak, levels = c(1:2), labels = c("No Cloak", "Cloak"))

library(reshape)
restructuredData<-melt(invisibilityData, id = c("cloak"), measured = c("mischief1", "mischief2")) 
names(restructuredData)<-c("cloak", "Time", "mischief")
ggplot(restructuredData, aes(x = cloak, y = mischief)) + 
  facet_wrap( ~ Time) +
  geom_boxplot()

noCloak <- subset(invisibilityData, cloak == "No Cloak")
invisCloak <- subset(invisibilityData, cloak == "Cloak")

covGrp1<-noCloak$mischief1
dvGrp1<-noCloak$mischief2
covGrp2<-invisCloak$mischief1
dvGrp2<-invisCloak$mischief2

ancova(covGrp1, dvGrp1, covGrp2, dvGrp2)
ancboot(covGrp1, dvGrp1, covGrp2, dvGrp2, nboot = 2000)
