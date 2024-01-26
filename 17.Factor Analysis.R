install.packages("corpcor"); install.packages("GPArotation"); install.packages("psych")
library(corpcor); library(GPArotation); library(psych)

raqData <- read.delim("raq.dat", header = TRUE)
raqMatrix <- cor(raqData)
raqData
raqMatrix[2,2]
round(raqMatrix, 2)

# Bartlett Test : 상관행렬이 Identity Matrix인지에 대한 유의성 검정
cortest.bartlett(raqData)
cortest.bartlett(raqMatrix, n = 2571)

KMO(raqData)
det(raqMatrix)

# PCA by principal()
pc1 <- principal(raqData, nfactors = 23, rotate = "none")
pc1
length(raqMatrix[,1])

plot(pc1$values, type = "b")

pc2 <- principal(raqMatrix, nfactors = 4, rotate = "none")
pc2
factor.model(pc2$loadings) # 재산출된 상관행렬 (using factor loadings)
?factor.model
factor.residuals(raqMatrix, pc2$loadings) # 잔차행렬

residuals <- factor.residuals(raqMatrix, pc2$loadings)
residuals <- as.matrix(residuals[upper.tri(residuals)])
residuals
large.resid <- abs(residuals) > 0.05
sum(large.resid)
sum(large.resid) / nrow(residuals)

sqrt(mean(residuals^2)) # 잔차평균제곱근
hist(residuals)
shapiro.test(residuals)
mean(pc2$communality)

pc3 <- principal(raqData, nfactors = 4, rotate = "varimax")
pc3
print.psych(pc3, cut = 0.3, sort = TRUE)

pc4 <- principal(raqMatrix, nfactors = 4, rotate = "oblimin")
pc4
print.psych(pc4, cut = 0.3, sort = TRUE)

# Pattern Matrix는  각 요인을 X로 놓고 각 변수를 Y로 놓고 돌린 회귀계수들이고, Structure Matrix는 요인과 변수의 상관계수임.

pc5 <- principal(raqData, nfactors = 4, rotate = "oblimin", scores = TRUE)
head(pc5$scores)
cor(pc5$scores)
raqData <- cbind(raqData, pc5$scores)

# 신뢰도분석

computerFear <- raqData[, c(6,7,10,13,14,15,18)]
statisticsFear <- raqData[,c(1,3,4,5,12,16,20,21)]
mathFear <- raqData[, c(8,11,17)]
peerEvaluation <- raqData[,c(2,9,19,22,23)]
alpha(statisticsFear, keys = c(1,-1,1,1,1,1,1,1)) # 요로코롬 역코딩 표시를 해준다.

