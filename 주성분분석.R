setwd('C:/Users/DJA/Desktop/DataScience/')
# ************************************************
# -- 주성분분석을 활용한 선형회귀분석
# -- 1)요인분석 - 주성분분석(PCA)
#    요인분석을하고 회귀분석에서 다중공선성을 회피하기 위해서 함. 
#    요인점수를 저장하여 회귀분석에 사용하면 됨
# 여러 많은 변수에 대해서 주성분(Principal Component)이라는 새로운 변수를 생성하여 기존 변수들보다 차원을 요약, 축소하는 기법.
#예를 들어, X1, X2, X3 ... X10 까지 10개의 변수를 주성분 분석을 통해 P1, P2로 차원을 축소하는 것이다.
# 즉, P1과 P2만으로 원래 10개의 변수를 거의 다 설명 할 수 있다는 것임.
#첫번째 주성분 P1은 데이터의 변동(분산, variance)을 가장 많이 설명 할 수 있는 것을
#선택하고, 나머지는 P1과 수직인 주성분을 선택함.
#따라서 주성분들 끼리는 서로 수직이기 때문에 다중공선성도 해결할 수 있음.
# ************************************************

library(readxl)
haha <- read_excel('C:/Users/DJA/Desktop/DataScience/인구분석상가_2017_1.xlsx')

library(psych)
install.packages('nFactors')
library(nFactors)
names(haha)

cor(haha[3:12])

attach(haha)
hehe <- cbind(소멸비율, 관광여가오락, 부동산, 생활서비스, 스포츠, 음식, 학문교육)
hehe <-na.omit(hehe)
#데이터가 한쪽에 편향되어 있기 때문에 log변환을 해주고, y변수가 될 소멸비율은 따로 넣어줌
hehe
#s.haha <- scale(hehe[,2:8])
#s.haha
log.haha <- log(hehe[,2:7])
log.haha
log2.haha <- log(log.haha)
ir.haha <- haha[,3]
ir.haha

#1) 주성분 분석은 'prcomp'라는 함수로 가능. 데이터를 표준화 작업.
#center = T는 중앙을 0으로, scale.=T 는 분산을 1로
ir.pca <- prcomp(log2.haha,center = T, scale. = T) 

print(ir.pca) 
plot(ir.pca, type='l')

#그림에서 3번부터 점차 기울기가 확 꺾이는 것을 알 수 있으며 이때, 기울기가 꺾이는 3번은 'elbow point'라고 하며, 이 포인트를 기준으로 그 위의 주성분을 주로 선택
# plot만으로는 주성분을 선택하기 부족하기때문에 꼭 summary를 통해 확인 과정 필요

summary(ir.pca) 

#첫번째로 Variance가 가장 큰 PC1을 선택합니다. 그리고 나머지는 가장 밑에 Cumulative Proportion을 보면 PC1부터 주성분이 하나씩추가될 수록 원래의 변수를 얼마나 설명하는 지 알 수 있음.
# 즉, PC1은 원래 변수를 92% 설명하며, PC2가 추가되면 95%까지 설명
#PC2 까지 해서 95%를 설명할 수 있으므로 주성분으로 PC1과 PC2를 선택!

#이제 회귀분석을 하기 위해 주성분을 처리

#위에 print 로 보았을 때, 
PC1 =  0.4160318 * 관광여가오락 + 0.4142951 * 부동산 + 0.4172206 * 생활서비스 + 0.3873857*스포츠 + 0.4021000*음식 + 0.4116454*학문교육  
#으로 이루어진 선형조합 이라고 했기때문에 각 주성분을 위의 형태에 맞게 계산
#따라서 원래 데이터와 선형계수를 메트릭스 곱으로 선형조합에 맞게 만들어 줌.
# %*% 이 메트릭스 곱, ir.pca$rotaion에 선형계수

PRC <- as.matrix(log.haha) %*% ir.pca$rotation
head(PRC)

# 주성분 분석 'eigen'을 쓰는방법도 있음. 이건 pca의 갯수를 지정 할 수 있음.
#e_value <- eigen(cor(x)) 
#e_value
#PCA <- principal(x, nfactors=4, rotate='varimax') 
#print(PCA, digits=3, sort=TRUE) 
#PCA
#Fscores <- PCA$scores 
#View(Fscores) #첫번째 요인의 점수, 두 번째 요인의 점수.. 등 
#haha <- cbind(haha, Fscores)  
#View(haha)

#********************************************
#2) 선형회귀분석
# 이제 회귀분석을 하기 위해 y변수를 합치고 factor처리 후 보기 좋게 y변수 넣음
#********************************************
train1 <- cbind(haha[,2], ir.haha, as.data.frame(PRC))
train1[,1] <- as.factor(train1[,1])
train1
colnames(train1)[1] <- "소멸지역"
head(train1)

#주성분 분석을 통해 선택한 PC1과 PC2를 가지고 회귀분석 실행
fit1 <- lm(소멸비율~PC1+PC2+PC3, data=train1) 
summary(fit1)
# 이 회귀 모델의 예측력이 좋은지 확인하기 위해 predict 함수로 예측
fit1_pred <- predict(fit1, newdata = train1)
fit1_pred
#결과가 소수로 나오기 때문에 반올림으로 정리해 준 후, 원래 y변수(Species)와 비교해 주기 위해 소멸위험지역, 소멸위기지역, 괜춘지역으로 나눔.
b <- fit1_pred
fit1_pred <- b
a <- fit1_pred[fit1_pred < 0.7]
b<- fit1_pred[ 1.5 >= fit1_pred & fit1_pred >= 0.7]
c <- fit1_pred[1.5 < fit1_pred]
a[] <- '소멸위험지역'
b[] <- '소멸위기지역'
c[] <- '소멸안정지역'
pred_result<-c(a,b,c)

real_result <- as.matrix(haha$결과값)
real_result[1]<-NULL 
table(pred_result, real_result)

#a에 원래 y변수를 넣어 준 후 table함수로 비교


