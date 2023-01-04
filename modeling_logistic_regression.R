#환경설정
require(data.table)
require(dplyr)
library(leaps)
library(bestglm)
require(car)
require(MASS)
require(caret)
options(scipen=3)

smp = fread("smp_lr2.csv")
smp  %>% head


lr = smp [,-c(1:2)]

#split
set.seed(5543)
ind = sample(2, nrow(lr), replace=TRUE, prob=c(0.7,0.3))
train = lr[ ind==1,]
test = lr[ind ==2,]

#피팅
fit = glm(신청_여부~., family = binomial, train)

#wald
summary(fit)

#lrt
Anova = as.data.frame( (Anova(fit)) )
o = order(Anova[,3], decreasing=FALSE)
Anova[o,]
Anova(fit)

#글로벌 검정
1- pchisq(1076589-855200,776594-776573)

#근속기간 제외
train2= train %>% select(-근속기간)

fit2 = glm(신청_여부~., family = binomial, train2)
summary(fit2)

#다중공선성 확인
vif(fit2)
#다중공선성 문제 없음 (rule of thumb 통계값인 5보다 크지 않음)


# 피처셀렉션
##1. AIC
stepAIC(fit2)

##2. bic
#train2 %>% mutate_if(is.integer, factor)

train2$근로형태 = factor(train2$근로형태)
train2$고용형태 = factor(train2$고용형태)
train2$주거소유형태 = factor(train2$주거소유형태)
train2$대출목적 = factor(train2$대출목적)
train2$개인회생 = factor(train2$개인회생)
train2$신청_여부 = factor(train2$신청_여부)

#train2$기대출과다_여부 = as.integer(train2$기대출과다_여부)
#train2$기대출_여부 = as.integer(train2$기대출_여부)
train2 %>% str


bestglm(train2, family=binomial, IC="BIC")

##################################
#1. threshold
#
prop = sum(train$신청_여부) / nrow(train) # 0.5003303

#2. 
pred= as.numeric(fitted(fit2)> prop)
#평가
confusionMatrix(as.factor(pred), as.factor(train$신청_여부), positive="1")

#테스트 피팅

pred_test = predict(fit2, test, type="response")
prop_test = sum(test$신청_여부)/nrow(test)


pred_test2 = as.numeric(pred_test > prop_test)
confusionMatrix(as.factor(pred_test2), as.factor(test$신청_여부), positive="1")
 
