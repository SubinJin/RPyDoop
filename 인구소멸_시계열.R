library(readxl)
library(zoo)
#군단위 인구소멸 데이터
gu <- read.csv('C:/Users/DJA/Desktop/DataScience/인구수_b20.csv')
dates <- as.yearmon(2008 + seq(0,119)/12)
dates
plot(gu$수, gu$전라남도.고흥군, type='l')

#군구단위 중 전라남도.고흥군의 시계열 그래프
a <- zoo(gu$전라남도.고흥군, dates)
plot(a)
a

#ts함수 : 관찰시기를 2008로 시작
gu <- ts(a, start = c(2008))
print(a)
attributes(a)

#데이터셋을 Seasonality, Trend, random 요소로 분해한 그래프
plot(stl(a, s.window="periodic"))

#시계열 자료를 안정적 시계열로 변환
#“차분” (diff)과 “로그함수”(log)사용. 로그 후 차분한 다음 tseries 패키지에 내장되어 있는 adf.test 함수로 시계열이 안정적 시계열인지 여부를 확인하면 됨.
#stationary p-value 0.01나옴. 나쁘지 않음..ㅎㅎ
library(tseries)
adf.test(diff(log(a)), alternative="stationary", k=0) 

#ACF/PACF차트나 auto.arima 함수를 사용하여 최적화된 파라미터를 찾는다.ARIMA는 AR 모형과 MA 모형을 결합한 것. 그래서 ARIMA 모델에서는 세가지 모형을 위한 세가지 파라미터 (p, d, q)가 필요.
#파라미터를 구하기 위해서는 AR(p)모형의 p차수 MA(q)의 q차수 그리고 트랜드를 제거하여 안정시계열로 만들기 위한 I(d)의 차분 차수 d를 결정하기 위해 KPSS test4 , ACF, PACF를 그려봄
#하지만 R에서는 auto.arima를 사용해 자동으로 p, d, q를 결정
library(forecast)
auto.arima(diff(log(a)))
tsdiag(auto.arima(diff(log(a))))

#ariam 모형만들기
#auto.arima 함수를 사용해서 구한 파라미터가 모형의 가정을 만족하는 지를 보여주는 그림. 세가지 그래프 모두 뚜렷한 패턴(점점 증가한다거나, 줄어든다거나 하는)이 없는 것으로 보아 대체로 가정을 만족한다고 볼 수 있음.
fit <- arima(log(a), c(0, 1,1), 
             seasonal = list(order = c(2, 0, 0),period = 24))


#arima 모형으로 미래추이 예측
#모델을 사용한 예측에는 predict 함수를 사용. fit 모델을 사용해서 향후 10년간 (12개월 * 10년)을 예측한 객체를 pred라고 지정하고 미래 추이는 해당 객체의 $pred 항목에 저장. 
pred <- predict(fit, n.ahead = 670*12)
tail(pred)
ts.plot(a,2.718^pred$pred, log = "y", lty = c(1,3))


