#install.packages('xlsx')
library('readxl')
install.packages('anytime')
library(anytime)
colors()
top <- read_excel('C:/Users/DJA/Desktop/DataScience/Best10.xlsx')



plot(top)
plot(top$년월, top$`경상북도 의성군 춘산면`, type='l')
par(new=T)
plot(top$년월, top$`경상남도 합천군 봉산면`, type='l', col='red')
par(new=T)
plot(top$년월, top$`경상남도 거창군 가조면`, type='l', col='blue')
par(new=T)
plot(top$년월, top$`인천광역시 서구 신현원창동`, type='l', col='orange')
par(new=T)
plot(top$년월, top$`경상북도 청송군 안덕면`, type='l', col='tomato1')
par(new=T)
plot(top$년월, top$`인천광역시 서구 가좌2동`, type='l', col='violetred1')
par(new=T)

str(top)

Date <- anydate(top$년월)
str(top)

library(zoo)

dates <- as.yearmon(2008 + seq(0,119)/12)
dates


a <- zoo(top$`경상북도 의성군 춘산면`, dates)
str(a)
b <- as.matrix(a)
c<- as.vector(b)
d <- ts(log(c), frequency = 8) # ts = 역행렬로 바꿔야함

plot(a)
plot(d,xlab = '월', ylab= '소멸비율')

#전체를 기준으로 잘라서 추세를 그래프로 확인
best1 <- zoo(top$`인천광역시 서구 신현원창동`, top$년월)
plot(best1)
str(best1)
best1
#시간에 따른 변화량 확인 - 차분(현재값-이전값)
plot(diff(d))
acf(d)
head(d)

Box.test(d, type='Ljung-Box') #box-pierce검증
Box.test

#이동평균법으로 시계열 그래프를 좀 더 부드럽게 표시
#데이터들을 몇개씩 묶어서 평균을 내어 새로운 값 구함.
#이동평균으로 새로운 값을 구하면 복잡한 선들이 좀더 부드럽게 표시 - 재략적인 패턴 파악 쉽다.
plot(a)
par(mfrow=c(2,2))
roll2 <- rollapply(a, 2, mean) #숫자는 몇개씩 묶어서 평균을 낼 것이다.
roll4 <- rollapply(a, 4, mean)
roll6 <- rollapply(a, 6, mean)
roll8 <- rollapply(a, 8, mean)
plot(roll2); plot(roll4); 
plot(roll6); plot(roll8);

#이동평균의 묶음 width수는 얼마나 좋을까?
#adf를 통해 구한 자기상관수를 사용하자.
par(mfrow=c(1,1))
roll11 <- rollapply(a, 11, mean)
plot(roll11)

#예측값 계산. - 시계열 모형 생성 : ARIMA모델
#R에서는 auto.arima 만능함수를 사용.
install.packages("forecast")
library(forecast)
library(TTR)
fit1 <- auto.arima(ts(log(c), frequency = 12))
plot(forecast(fit1))
fit1 <- auto.arima(ts(log(c), frequency=12))
fit2 <- auto.arima(ts(log(c), frequency=12))

#생선된 모형으로 값 예측
v <- predict(fit2, n.ahead=12) #향후 12달예측.
v

plot(a)


#인천 인구 감소

plot(top$년월, top$`인천광역시 서구 신현원창동`, type='l')
par(new=T)
plot(top$년월, top$`인천광역시 서구 가좌2동`, type='l', col='red')
par(new=T)
plot(top$년월, top$`인천광역시 서구 가정3동`, type='l',col='blue')
par(new=T)
plot(top$년월, top$`인천광역시 서구 가정1동`, type='l', col='brown')
par(new=T)
plot(top$년월, top$`인천광역시 서구 석남2동`, type='l', col='orange')


