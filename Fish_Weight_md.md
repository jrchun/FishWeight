**물고기 무게예측 분석**
========================

본 문서는 '물고기 무게예측 분석'(Fish Weight)을 마크다운형식으로 편집하여,
Github에 업로드 하기 위하여 작성된 문서입니다.
데이터 출처 : 확인 필요.

\*<https://www.kaggle.com/c/bike-sharing-demand>

------------------------------------------------------------------------

분석 과정 목차
--------------

1.  변수 정의
2.  분석 과정
    -   데이터 구조 확인
    -   사전 가설 수립(Make insight)
    -   EDA / Data preprocessing
    -   Modeling
    -   MSE Checking
3.  결론

------------------------------------------------------------------------

변수 정의
---------

``` r
rm(list = ls())
```

``` r
setwd('C:\\github\\Project\\FishWeight')
data <- read.table('fish.txt')
colnames(data) <- c('Obs', 'Species', 'Weight', 'Length1', 'Length2',
                  'Length3', 'Height', 'Width', 'Sex')
str(data)
```

    ## 'data.frame':    159 obs. of  9 variables:
    ##  $ Obs    : int  1 2 3 4 5 6 7 8 9 10 ...
    ##  $ Species: int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Weight : num  242 290 340 363 430 450 500 390 450 500 ...
    ##  $ Length1: num  23.2 24 23.9 26.3 26.5 26.8 26.8 27.6 27.6 28.5 ...
    ##  $ Length2: num  25.4 26.3 26.5 29 29 29.7 29.7 30 30 30.7 ...
    ##  $ Length3: num  30 31.2 31.1 33.5 34 34.7 34.5 35 35.1 36.2 ...
    ##  $ Height : num  38.4 40 39.8 38 36.6 39.2 41.1 36.2 39.9 39.3 ...
    ##  $ Width  : num  13.4 13.8 15.1 13.3 15.1 14.2 15.3 13.4 13.8 13.7 ...
    ##  $ Sex    : int  NA NA NA NA NA NA NA NA NA NA ...

data.all*S**p**e**c**i**e**s* &lt; −*a**s*.*f**a**c**t**o**r*(*d**a**t**a*.*a**l**l*Species) \#Species : 종 이므로 팩터로 변경

which(is.na(data.all$Weight)==TRUE) \#Weight를 추정하는 분석이므로 Weight가 0, row인 row는 삭제한다. data.all &lt;- data.all\[-14, \] which(data.all$Weight == 0) data.all &lt;- data.all\[-46, \]

str(data.all) \# 데이터 : 물고기의 성질 \# 목적 : 물고기의 무게 예측 \# Train data : 1~120 , Test data : 121~159

### 분석방법

그냥 적합하기.
==============

상관관계 확인해보기 LAsso, ridge regression
===========================================

예쁜 파생변수 만들어 보기
=========================

#### Sol 1. 그냥 NA값 많은 성별데이터 삭제하고 나머지 변수 CentralImputation넣고 회귀돌리기.

library(DMwR) data.cen &lt;- centralImputation(data.all) \#DMwR패키지의 centralImputation 함수를 활용해서 모든 결측값을 중앙값 & 최빈값으로 치호

set.seed(29) A &lt;- sort(sample(1:nrow(data.cen), nrow(data.cen)\*0.75)) train1 &lt;- data.cen\[A, \] test1 &lt;- data.cen\[-A, \]

관측순서는 결과와 상관X, 성별은 결측값이 너무 많으므로 삭제한다!
================================================================

train1 &lt;- train1\[, -c(1, ncol(train1))\] test1 &lt;- test1\[, -c(1, ncol(test1))\]

fit1 &lt;- lm(Weight ~., data = train1) summary(fit1) par(mfrow = c(2,2)) plot(fit1)

library(Metrics) \#MSE확인. pred1 &lt;- predict(fit1, newdata = test1) sum((test1*W**e**i**g**h**t* − *p**r**e**d*1)<sup>2</sup>)/*l**e**n**g**t**h*(*p**r**e**d*1)*m**s**e*(*t**e**s**t*1Weight, pred1) rmse(test1$Weight, pred1) \#\#MSE가 12761.23 가 나옴을 알 수 있다! \# 구데기 모델.

#### Sol 2. 변수들간의 특징 살펴보기

set.seed(29) A &lt;- sort(sample(1:nrow(data.all), nrow(data.all)\*0.75)) train2 &lt;- data.all\[A, \] test2 &lt;- data.all\[-A, \]

성별 죽이기
===========

train2 &lt;- train2\[, -c(1, ncol(train2))\] test2 &lt;- test2\[, -c(1, ncol(test2))\]

상관관계 확인해보기(숫자데이터)
===============================

library(corrplot) numvars &lt;- which(sapply(train2, is.numeric)) \#숫자형 변수들의 Index numvars &lt;- as.vector(numvars)

숫자형 변수들의 각각의 Correlation을 계산하자. (pairwise 사용)
==============================================================

train.cor &lt;- cor(train2\[, numvars\], use = 'pairwise.complete.obs')

계산한 상관계수로 그래프 그리기.
================================

corrplot.mixed(train.cor, tl.col="black", tl.pos = "lt") par(mfrow = c(1,1)) \#Length1, 2, 3각각의 상관계수가 너무 높다!! -&gt; 다중공선성의 위험. \# Length3만 사용하기 head(train2) train2 &lt;- train2\[, -c(3, 4)\] test2 &lt;- test2\[, -c(3, 4)\]

fit2 &lt;- lm(Weight ~ ., data = train2) summary(fit2) pred2 &lt;- as.vector(predict(fit2, newdata = test2))

MSE &lt;- sum((test2*W**e**i**g**h**t* − *p**r**e**d*2)<sup>2</sup>)/*l**e**n**g**t**h*(*p**r**e**d*2)*m**s**e*(*t**e**s**t*2Weight, pred2)

MSE가 12933.69, RMSE가 113.7264
===============================

왜 MSE가 더 올라갔지? : 상관계수가 높은 변수를 제외한다고 해서, 반드시 예측력이 높아지는 건 아니다!
===================================================================================================

### Sol 3 : LASSO, ridge, elastic regression 사용해보기

set.seed(29) A &lt;- sort(sample(1:nrow(data.all), nrow(data.all)\*0.75)) train3 &lt;- data.all\[A, \] test3 &lt;- data.all\[-A, \]

성별 죽이기
===========

train3 &lt;- train3\[, -c(1, ncol(train3))\] test3 &lt;- test3\[, -c(1, ncol(test3))\]

LASSO / ridge / elastic을 모두 사용할 수 있는 mpath 패키지 사용.
================================================================

library(mpath)

lasso
=====

set.seed(29) fit3.lasso.cv10 &lt;- cv.glmreg(Weight ~ ., family = "gaussian", alpha = 1, \# lasso penalty data = train3) fit3.lasso.cv10 abline(v = log(fit3.lasso.cv10$lambda.optim), col = "red", lty = 3)

fit3.lasso &lt;- glmreg(Weight ~ ., family = "gaussian", alpha = 1, data = train3, lambda = fit3.lasso.cv10$lambda.optim) summary(fit3.lasso) \#해석 불가.

MSE 계산
========

pred3.lasso &lt;- predict(fit3.lasso, newx = test3) mse(test3*W**e**i**g**h**t*, *p**r**e**d*3.*l**a**s**s**o*)*s**u**m*((*t**e**s**t*3Weight - pred3.lasso)^2) / NROW(test3) sqrt(sum((test3$Weight - pred3.lasso)^2) / NROW(test3)) \#MSE = 17197.56 / RMSE = 131.1395

ridge
=====

set.seed(29) fit3.ridge.cv &lt;- cv.glmreg(Weight ~ ., family = "gaussian", alpha = 0, \# ridge penalty data = train3, lambda = seq(0.01, 1, by = 0.01))

abline(v = log(fit3.ridge.cv$lambda.optim), col = "red", lty = 2)

fit3.ridge &lt;- glmreg(Weight ~ ., family = "gaussian", alpha = 0, data = train3, lambda = fit3.ridge.cv$lambda.optim)

MSE 계산
========

pred3.ridge &lt;- predict(fit3.ridge, newx = test3) mse(test3*W**e**i**g**h**t*, *p**r**e**d*3.*r**i**d**g**e*)*s**u**m*((*t**e**s**t*3Weight - pred3.ridge)^2) / NROW(test3) sqrt(sum((test3$Weight - pred3.ridge)^2) / NROW(test3))

MSE = 16719.57 / RMSE = 129.3042
================================

elatic net
==========

nested cv is needed
===================

a &lt;- as.list(seq(0.1, 1.0, by = 0.1)) ncv &lt;- sapply(a, function(a) { print(a) set.seed(29) cv10 &lt;- cv.glmreg(Weight ~ ., family = "gaussian", plot.it = F, \# cv-plot alpha = a, \# lasso penalty data = train3) c(loglik = cv10*c**v*\[*c**v*10lambda.which\], lambda = cv10$lambda.optim) }) \#내가 구한 lambda와 그에 따른 loglikellyhood, 그리고 그때 가정한 alpha를 하나로 설정 res &lt;- cbind.data.frame(t(ncv), alpha = unlist(a)) res

opt &lt;- res\[which.max(res\[,1\]),\] \#alpha가 1, 그때의 가능도가 가장 높다. opt

fit3.elastic &lt;- glmreg(Weight ~ ., family = "gaussian", alpha = opt*a**l**p**h**a*, *l**a**m**b**d**a* = *o**p**t*lambda, data = train3)

MSE 계산
========

pred3.elastic &lt;- predict(fit3.elastic, newx = test3) mse(test3*W**e**i**g**h**t*, *p**r**e**d*3.*e**l**a**s**t**i**c*)*s**u**m*((*t**e**s**t*3Weight - pred3.elastic)^2) / NROW(test3) sqrt(sum((test3$Weight - pred3.elastic)^2) / NROW(test3))

MSE = 17197.56 / RMSE = 131.1395
================================

Elastic을 써도 결국 LASSO랑 같은 값이 나온다. : 최적의 알파가 1로 나오기 때문!!
===============================================================================

### LaSSO / Ridge / Elastic net을 써보았지만, MSE의 엄청난 감소는 확인 할 수 없었다.

#### 창의적으로 해결해보기.

library(DMwR) data.cen &lt;- centralImputation(data.all) \#DMwR패키지의 centralImputation 함수를 활용해서 모든 결측값을 중앙값 & 최빈값으로 치호

set.seed(29) A &lt;- sort(sample(1:nrow(data.cen), nrow(data.cen)\*0.75)) train4 &lt;- data.cen\[A, \] test4 &lt;- data.cen\[-A, \]

관측순서는 결과와 상관X, 성별은 결측값이 너무 많으므로 삭제한다!
================================================================

train4 &lt;- train4\[, -c(1, ncol(train4))\] test4 &lt;- test4\[, -c(1, ncol(test4))\]

fit4 &lt;- lm(Weight ~., data = train4) \#stepwise 기법으로 변수선택하기. step(fit4, methods = 'both') fit4.step &lt;- lm(Weight ~ Species + Length1 + Length2 + Height, data = train4)

MSE 계산
========

pred4.step &lt;- predict(fit4.step, newdata = test4) pred4.step &lt;- as.vector(pred4.step) mse(test4*W**e**i**g**h**t*, *p**r**e**d*4.*s**t**e**p*)*s**u**m*((*t**e**s**t*4Weight - pred4.step)^2) / NROW(test4) sqrt(sum((test4$Weight - pred4.step)^2) / NROW(test4))

MSE = 12859.61 / RMSE = 113.4002
================================

해본 방법중에서 가장 낮은 MSE를 찾았다..!!
==========================================

회귀분석의 가정 만족하는지 확인해보기!
======================================

par(mfrow = c(2,2)) plot(fit4.step) \#정규성을 만족하지 않음....

이 포뮬러로 정규성 맞추기. Weight ~ Species + Length1 + Length2 + Height
========================================================================

train4.step &lt;- train4\[, -c(5, 7)\] test4.step &lt;- test4\[, -c(5, 7)\] data4.step &lt;- rbind(train4.step, test4.step)

par(mfrow = c(1,1)) hist(data4.step*W**e**i**g**h**t*)*h**i**s**t*(*l**o**g*(*d**a**t**a*4.*s**t**e**p*Weight)) hist((data4.step$Weight)^(2/5)) \# 분하지만 sqrt일때 가장 정규성을 잘 만족하는 것 처럼 보인다.

SQRT transformation + Stepwise method
-------------------------------------

fit4.step.trans &lt;- lm((Weight)^(2/5) ~ ., data = train4.step) par(mfrow = c(2,2)) plot(fit4.step.trans) pred4.step.trans &lt;- predict(fit4.step.trans, newdata = test4.step)

MSE
===

sum((((test4.step$Weight))-(pred4.step.trans)^(5/2))^2) / nrow(test4.step) \#RMSE sqrt(sum((((test4.step$Weight))-(pred4.step.trans)<sup>(5/2))</sup>2) / nrow(test4.step))

install.packages('powerTransform')

Box-cox transformation
======================

install.packages('MASS') library(MASS) par(mfrow = c(1,1)) bc &lt;- boxcox(fit1) lambda &lt;- bc*x*\[*w**h**i**c**h*.*m**a**x*(*b**c*y)\] fit1.bc &lt;- lm((Weight)^(lambda) ~ ., data = train1) par(mfrow = c(2,2)) plot(fit1.bc) pred1.bc &lt;- predict(fit1.bc, newdata = test1)

shapiro.test(sqrt(train1$Weight))

par(mfrow = c(1, 2)) shapiro.test((train1*W**e**i**g**h**t*)<sup>(</sup>*l**a**m**b**d**a*))*q**q**n**o**r**m*((*t**r**a**i**n*1Weight)^(lambda))

shapiro.test((train1*W**e**i**g**h**t*))*q**q**n**o**r**m*((*t**r**a**i**n*1Weight))

MSE
===

sum((((test1$Weight))-(pred1.bc)^(1/lambda))^2) / nrow(test1) \#RMSE sqrt(sum((((test1$Weight))-(pred1.bc)<sup>(1/lambda))</sup>2) / nrow(test1))

LASSO + Box-Cox
---------------

library(mpath) set.seed(29) fit1.lasso.cv10 &lt;- cv.glmreg((Weight)^(lambda) ~ ., family = "gaussian", alpha = 1, \# lasso penalty data = train1) fit1.lasso.cv10 abline(v = log(fit1.lasso.cv10$lambda.optim), col = "red", lty = 3)

fit1.lasso &lt;- glmreg((Weight)^(lambda) ~ ., family = "gaussian", alpha = 1, data = train1, lambda = fit1.lasso.cv10$lambda.optim)

par(mfrow = c(2,2)) plot(fit1.lasso) pred1.bc.lasso &lt;- predict(fit1.lasso, newx = test1)

MSE
===

sum((((test1$Weight))-(pred1.bc.lasso)^(1/lambda))^2) / nrow(test1) \#RMSE sqrt(sum((((test1$Weight))-(pred1.bc.lasso)<sup>(1/lambda))</sup>2) / nrow(test1))

LASSO + Box-cox transformation으로 MSE를 낮추는데 성공!
-------------------------------------------------------
