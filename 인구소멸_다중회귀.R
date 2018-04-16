#선형회귀
#F-test는 회귀분석 모델 전체에 대해 이것이 통계적으로 의미가 있는지를 결정하기 위해 사용하며, 회귀분석 모델에서 F-Statistic의 p-value의 값이 0.05보다 작은 경우 회귀식 전체는 유의하다고 볼 수 있음
#선형회귀 모형적합

library(readxl)
haha <- read_excel('C:/Users/DJA/Desktop/DataScience/인구분석상가_2017.xlsx')
#haha$지역
#hahahoho <- subset(haha, 지역 == '경상북도 군위군' )
#hahahoho
summary(haha)


# multiple r-squared와 adjusted R-squard는 모형의 적합도를 나타냄. Multiple R-squared:  0.3747 반응변수의 총 변동 중 37.77%가 선형회귀 모형을 설명됨. 결정계수는 0에서 1사이의 값이고, 1에 가까울수록 설명변수의 설명력을 높여줌.
lm(소멸비율 ~ 관광여가오락+생활서비스+부동산+소매+숙박+스포츠+음식+학문교육, data=haha)
summary(lm(소멸비율 ~ 관광여가오락+생활서비스+부동산+소매+숙박+스포츠+음식+학문교육, data=haha)) #학문교육 0.048

lm(소멸비율 ~ 숙박+스포츠+음식+학문교육, data=haha)
summary(lm(소멸비율 ~ 숙박+스포츠+학문교육, data=haha))
lm(총인구수 ~ 숙박+스포츠+음식+학문교육, data=haha)
summary(lm(총인구수 ~ 숙박+스포츠+학문교육, data=haha))
lm(소멸비율 ~ 관광여가오락, data=haha)
summary(lm(소멸비율 ~ 관광여가오락, data=haha))

#학문교육이 가장 p-value의 유의미성이 높게 나옴.
summary(lm(소멸비율~학문교육, data=haha))
lm_haha <- lm(소멸비율~학문교육, data=haha)
predict(lm_haha)

#여러 설명변수를 통해 수량형 반응변수를 예측하기 위한 방법 - 그나마 생활서비스와 스포츠가 제일 pvalue의 적합도 검정에서 좋게 나왔다.
#소멸비율 = 0.878 + (-0.001) * 생활서비스 + (-0.01) * 스포츠 

attach(haha)
cor(소멸비율, 학문교육)
cor(소멸비율, 관광여가오락)
cor(소멸비율, 소매)
cor(소멸비율, 음식)
cor(소멸비율, 부동산)

plot(소멸비율~생활서비스)

#공분산 - cov()
cov(소멸비율, 생활서비스)

#상관계수
install.packages('corrplot')
library(corrplot)
cov(소멸비율, 학문교육)
cor1 <- round(cor(소멸비율,학문교육),2)
hehe <- haha[,3:12]
cor_test <- cor(hehe) #상관행렬 생성
cor_test <- round(cor(hehe), 2)
cor_test
corrplot(cor_test, method='number') #상관행렬 그래프.
corrplot.mixed(cor_test)
corrplot(cor_test,method="number", type='lower')
corrplot(cor_test, method='ellipse', cl.ratio = .1 , cl.align = "l")

#상관계수 가설검증
#p-value는 p-value < 2.2e-16 이며 오류가 날 확률은 매우 작으며 서로 상관관계가 있다고 할 수 있다.
cor.test(소멸비율, 생활서비스)
plot(소멸비율 ~생활서비스)
#plot(amuse[2:7]) #산포도 작성
abline(lm(소멸비율~생활서비스), col='red', lwd=2)






