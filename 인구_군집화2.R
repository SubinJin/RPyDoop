getwd()
setwd('C:/Users/DJA/Desktop/DataScience')
data <- read.csv('C:/Users/DJA/Desktop/DataScience/분석변수데이터/지역적인구변수2.csv')
head(data)
#cor(data$소멸비율, data[,3:7])
#K-평균 클러스터링의 목적에 적합하게끔 데이터를 전처리한다.
data2<-data[,3:7] #목표변수 제외
# K-평균 클러스터링을 진행한다. 먼저 적합한 클러스터의 개수 k 개를 선택해보기로 한다.

install.packages('caret')
library(caret)
set.seed(9876)
inTrain <- createDataPartition(y=data$소멸비율, p=0.7, list=FALSE)
training <- data[inTrain,]
testing <- data[-inTrain,]

training.data <- scale(training[,3:7])
summary(training.data)

data.kmeans <- kmeans(training.data, centers = 3, iter.max = 10000)
data.kmeans$centers

training$cluster<-as.factor(data.kmeans$cluster)
qplot(의료, 부동산, colour=cluster,data=training)

training

table(training$소멸지역여부, training$cluster)










