install.packages('NbClust')
install.packages('kohonen')
library(cluster)
library(NbClust)
library(kohonen)
library(ggplot2)
library(gridExtra)
library(scales)
data <- read.csv('C:/Users/DJA/Desktop/DataScience/총_지역적인구분석.csv')
head(data)
colnames(data)

cor(data$소멸비율, data[,3:7])


data1 <- scale(data[3:7])
data2 <- subset(data, select = c(지역, 결과값, 소멸비율))
data3 <- cbind(data2, data1)
head(data3)

# 최적의 군집 수 찾기 : 방법1
sd <- data3[sample(1:nrow(data3),100),-1]
d <- dist(sd, method = "euclidean")
fit <- hclust(d, method="complete")
plot(fit)
rect.hclust(fit, k=3, border = "red")

# 최적의 군집 수 찾기 : 방법2
wss <- 0; 
set.seed(1)
for(i in 1:15) wss[i] <- kmeans(subset(data3, select=-c(지역, 결과값, 소멸비율)), centers=i)$tot.withinss
plot(1:15, wss, type="b", xlab="# of clusters", ylab="Within group sum of squares")


km1 <- kmeans(subset(data3, select=-c(지역, 결과값, 소멸비율)), centers=3)
str(km1)
km1

#km2 <- kmeans(subset(data3, select=-c(지역, 결과값, 소멸비율, 결혼이민자)), centers=3)
#str(km2)
#km2

#km3 <- kmeans(subset(data3, select=-c(지역, 결과값, 소멸비율, 결혼이민자, 생활서비스)), centers=3)
#km3

data3$cluster1 <- as.factor(km1$cluster)
#data$cluster2 <- as.factor(km2$cluster)
#data$cluster3 <- as.factor(km3$cluster)

clusplot(subset(data3, select=-c(지역, 결과값, 소멸비율)), km1$cluster, main="cluster 3 : All variables")

#clusplot(subset(data3, select=-c(지역, 결과값, 소멸비율, 결혼이민자, 생활서비스)), km1$cluster, main="cluster 3")

qplot(관광.여가.오락, 생활서비스, colour=cluster1, data=data3)
qplot(학문.교육, 생활서비스, colour=cluster1, data=data3)

plot(subset(data3, select=-c(지역, 결과값, 소멸비율)), col=km1$cluster)

#table(data3$결과값, data3$cluster1)
#table(data$결과값, data$cluster2)
#table(data$결과값, data$cluster3)
colnames(data3)

# 군집별 군집화변수의 밀도 : 방법1
p1 <- qplot(학문.교육, fill=cluster1, alpha=.5, data=data3, geom="density") + scale_alpha(guide="none")
p2 <- qplot(생활서비스, fill=cluster1, alpha=.5, data=data3, geom="density") + theme(legend.position="none")
p3 <- qplot(관광.여가.오락, fill=cluster1, alpha=.5, data=data3, geom="density") + theme(legend.position="none")
p4 <- qplot(부동산, fill=cluster1, alpha=.5, data=data3, geom="density") + theme(legend.position="none")
grid.arrange(p1, p2, p3, p4, ncol=2, nrow=2)

x<- ggplot(data3, aes(factor(1), fill=cluster1))
x + geom_bar(width=1) + coord_polar(theta='y')




