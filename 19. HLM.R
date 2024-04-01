install.packages("car");install.packages("ggplot2");install.packages("nlme");install.packages("reshape")
library(car);library(ggplot2);library(nlme);library(reshape)
setwd("/Users/seongminpark/Desktop/DS/ANDYR/Data files")
surgeryData = read.delim("Cosmetic Surgery.dat", header = TRUE)

pgrid <- ggplot(surgeryData, aes(Base_QoL, Post_QoL))
pgrid + geom_point(aes(colour = Surgery_Text)) + 
  geom_smooth(aes(colour = Surgery_Text), method = "lm", se = F) + 
  facet_wrap(~Clinic, ncol = 5) + # Clinic 10개별로 나눠서 보여줌
  labs(x = "Quality of Life (Baseline)", y = "Quality of Life (After Surgery)")

surgeryANOVA<-aov(Post_QoL~Surgery, data = surgeryData)
summary(surgeryANOVA)

surgeryLinearModel<-lm(Post_QoL~Surgery, data = surgeryData)
summary(surgeryLinearModel)

surgeryANCOVA<-aov(Post_QoL~Base_QoL + Surgery, data = surgeryData)
summary(surgeryANCOVA)

surgeryLinearModel<-lm(Post_QoL~Surgery + Base_QoL, data = surgeryData)
summary(surgeryLinearModel)


# HLM ---------------------------------------------------------------------

interceptOnly <-gls(Post_QoL~1, data = surgeryData, method = "ML")
summary(interceptOnly)

randomInterceptOnly <-lme(Post_QoL~1, data = surgeryData, random = ~1|Clinic, method = "ML")
summary(randomInterceptOnly)

logLik(interceptOnly)*-2
logLik(randomInterceptOnly)*-2

anova(interceptOnly, randomInterceptOnly)

randomInterceptSurgery <-lme(Post_QoL~Surgery, data = surgeryData, random = ~1|Clinic, method = "ML")
summary(randomInterceptSurgery)
randomInterceptSurgeryQoL <-lme(Post_QoL~Surgery + Base_QoL, data = surgeryData, random = ~1|Clinic, method = "ML")
summary(randomInterceptSurgeryQoL)

anova(randomInterceptOnly, randomInterceptSurgery, randomInterceptSurgeryQoL)

addRandomSlope<-lme(Post_QoL~Surgery + Base_QoL, data = surgeryData, random = ~Surgery|Clinic, method = "ML")
summary(addRandomSlope)
anova(randomInterceptSurgeryQoL,addRandomSlope)

plot(addRandomSlope)

addReason<-lme(Post_QoL~Surgery + Base_QoL + Reason, data = surgeryData, random = ~Surgery|Clinic, method = "ML")
addReason<-update(addRandomSlope, .~. + Reason)
summary(addReason)

fsummary(finalModel)
intervals(finalModel, 0.95)

anova(addRandomSlope, addReason, finalModel)

physicalSubset<- surgeryData$Reason==1 
cosmeticSubset<-surgeryData$Reason==0

physicalModel<-lme(Post_QoL~Surgery + Base_QoL, data = surgeryData, random = ~Surgery|Clinic, subset= physicalSubset, method = "ML")
cosmeticModel<-lme(Post_QoL~Surgery + Base_QoL, data = surgeryData, random = ~Surgery|Clinic, subset= cosmeticSubset, method = "ML")
summary(physicalModel)
summary(cosmeticModel)

# Growth Model ------------------------------------------------------------

sfData <- read.delim("Honeymoon Period.dat", header = TRUE)
plot(sfData)
restructuredData <- melt(sfData, id = "Person",
                         measures = c("Satisfaction_6_Months", "Satisfaction_12_Months", "Satisfaction_18_Months"))
names(restructuredData) <- c("Person", "Time", "Satisfaction")
str(restructuredData)
restructuredData$Time <- as.numeric(restructuredData$Time) - 1

intercept <- gls(Satisfaction ~ 1, data = restructuredData, method = "ML", na.action = na.exclude)
randomIntercept <- lme(Satisfaction ~ 1, data = restructuredData, random = ~1|Person, method = "ML", na.action = na.exclude, 
                       control = list(opt = "optim")) 
timeRI <- update(randomIntercept, .~. + Time)
timeRS <- update(timeRI, random = ~Time|Person)
?corClasses
ARModel <- update(timeRS, correlation = corAR1(0, form = ~Time|Person)) # why not works? 1차 자기회귀 공분산 구조
anova(intercept, randomIntercept, timeRI, timeRS)
summary(ARModel)
intervals(ARModel)

timeQuad <- update(ARModel, .~. + I(Time^2))
timeCubic <- update(timeQuad, .~. + I(Time^3))
anova(ARModel, timeQuad, timeCubic)
summary(timeCubic)
intervals(timeCubic)

polyModel <- update(ARModel, .~ poly(Time, 3)) # 위의 시간 별 변수들은 서로 상관되어 있으나 poly() 시간변수는 완전 독립


