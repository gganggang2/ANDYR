library(tidyverse)
setwd("/Users/seongminpark/Desktop/DS/ANDYR/Data files")
spiderLong <- as.data.frame(read.delim("spiderLong.dat"))
spiderWide <- as.data.frame(read.delim("spiderWide.dat"))
spiderWide$pMean <- (spiderWide$picture + spiderWide$real) / 2

# Error bar ---------------------------------------------------------------

# Grand Mean
grandMean <- mean(c(spiderWide$picture, spiderWide$real))
spiderWide$adj <- grandMean - spiderWide$pMean

spiderWide$picture_adj <- spiderWide$picture + spiderWide$adj
spiderWide$real_adj <- spiderWide$real + spiderWide$adj
spiderWide$adj_mean <- (spiderWide$picture_adj + spiderWide$real_adj)/2
spiderWide$adj_mean

# t-test ------------------------------------------------------------------

# Independent t-test vs Dependent t-test

summary(lm(Anxiety~Group, data = spiderLong))
Group <- gl(2, 12, labels = c("picture", "real spider"))
Anxiety <- c(30,35,45,40,50,35,55,25,30,45,40,50,40,35,50,55,65,55,50,35,30,50,60,39)
spiderLong_2 <- data.frame(Group, Anxiety)
spiderLong_2

ggplot(data = spiderLong, aes(x = Group, y = Anxiety)) + 
  geom_boxplot()
by(spiderLong$Anxiety, spiderLong$Group, stat.desc, basic = FALSE, norm = TRUE)
# by(data를, Group별로, 기술통계량을, 기초통계량을, 정규성검정을)

spiderT <- t.test(Anxiety~Group, data = spiderLong, paired = FALSE)
# 기본 Welch test. 이는 표본의 크기가 다를 때 등분산 검정이 잘 작동하지 않는 것을 조정해주는 것임

t.test(spiderWide$real, spiderWide$picture)
spiderT$statistic[[1]] # use [[1]] to extract the value from the list

spiderWide <- read.delim("spiderWide.dat")
stat.desc(spiderWide, basic = FALSE, norm = TRUE)
diff <- spiderWide$real - spiderWide$picture
stat.desc(diff, basic = FALSE, norm = TRUE) # It uses shapiro-test

spiderT2 <- t.test(spiderWide$real, spiderWide$picture, paired = TRUE)
spiderT2
