

library(tidyverse)

df = read_csv("data/risk_classification_bias.csv")



# high_risk: 응급실 방문 횟수, 재입원율, 사망률, 그 밖의 질병 관리 상태를 나타내는 임상 지표 등으로부터 의학적 필요를 추정하여 상위 10%인 경우 1, 그렇지 않은 경우 0

# race
# sex
# age
# chronic_count: 만성 질환의 수
# dx_count: 진단받은 질병의 수
# drug_count: 처방받은 약의 수
# inpatient_history: (과거 2년간) 입원 이력이 있는지 여부(0 또는 1)
# past_cost: 과거 2년간 의료비 지출


# 인종별 high risk group의 비율은 얼마인가? 인종 간에 차이가 있는가?

df %>%
  group_by(race) %>%
  summarise(mean_prop = mean(high_risk)) 

chisq.test(table(df$race, df$high_risk))

# 인종 간 과거 의료비 지출에 차이가 있는가? 

df %>%
  group_by(race) %>%
  summarise(mean_cost = mean(past_cost))


# high risk 군을 분류하기 위해 로지스틱 회귀 모델을 적합하라(단, 예측 변수로 race 는 제외할 것). 

m_logit <- glm(high_risk ~ age + chronic_count + dx_count + 
                 drug_count + inpatient_history + past_cost, 
               data = df, family = binomial)


# high risk 분류와 연관성이 있는 예측 변수들에는 어떤 것들이 있는가?  

summary(m_logit)

# 위 모델에서 만성 질환 수(chronic_count)의 오즈비는 얼마인가? 

exp(coef(m_logit))


# 각 환자에서 위 모델에 의한 high risk 분류 확률을  구하라. 

prob = predict(m_logit, type = "response")
df = cbind(df, prob)

# ROC curve 를 그리고, AUC 값을 구하라. 인종 간의 AUC 값에 차이가 있는가? 
library(ROCR) 

pred = prediction(predictions = df$prob, labels = df$high_risk)
perf = performance(pred, "tpr", "fpr")
plot(perf)
 
auc = performance(pred, "auc")
auc@y.values

pred_white = prediction(predictions = df[df$race == "White",]$prob, labels = df[df$race == "White",]$high_risk)
auc_white = performance(pred_white, "auc")
auc_white@y.values

pred_black = prediction(predictions = df[df$race == "Black",]$prob, labels = df[df$race == "Black",]$high_risk)
auc_black = performance(pred_black, "auc")
auc_black@y.values

# race 를 제외한 로지스틱 회귀 모델에서 black 과 white 인종 각각에서 AUC 값에 차이가 나타나는 이유는 무엇일까? 전체 데이터에서의 AUC 값은 각 인종에서의 AUC 값보다 낮다. 왜 그럴까?

# 모델을 만들 때 race 변수를 제외했더라도, 데이터셋 내의 다른 변수들(age, chronic_count, dx_count 등)의 분포가 인종 그룹별로 다르기 때문입니다.
# 
# 모델은 이들 변수와 true_need 간의 관계를 학습하여 전체 데이터에 가장 잘 맞는 단일 방정식을 만듭니다. 그러나 각 그룹이 가지고 있는 변수의 특성이 미묘하게 다르기 때문에, 이 단일 방정식은 White 그룹과 Black 그룹에 대해 각각 최적의 성능을 내지 못할 수 있습니다.
# 
# 예를 들어, Black 그룹은 만성 질환 수(chronic_count)가 더 많거나 다른 임상 지표의 분포가 다를 수 있습니다. 모델은 이러한 차이를 race 변수가 아닌 다른 변수들의 조합을 통해 간접적으로 학습하고, 결과적으로 예측 성능이 각 그룹마다 다르게 나타나게 됩니다.
# 
# 
# 이 현상은 심슨의 역설이라는 통계적 현상의 한 예시입니다. 전체 데이터를 합쳤을 때 나타나는 경향이 개별 하위 그룹의 경향과 다르게 나타나는 경우에 발생합니다.
# 
# 모델의 예측 확률을 기준으로 AUC를 계산할 때, 전체 데이터는 White 그룹과 Black 그룹이라는 서로 다른 특성을 가진 두 집단이 혼합된 상태입니다.
# 
# 이로 인해 모델은 전체 집단에 대한 예측 순위(ranking)를 매길 때 두 그룹 사이의 경계에서 혼란을 겪을 수 있습니다. 반면, 각 그룹을 분리하여 AUC를 계산하면, 모델은 상대적으로 더 균질한(homogeneous) 데이터 내에서 예측 순위를 매기게 되므로, 훨씬 높은 성능을 보이게 됩니다.
# 
# 결론적으로, 전체 데이터의 AUC 값은 두 그룹의 이질적인 특성이 한데 섞여 나타나는 **'평균값의 함정'**에 빠지기 때문에, 각 그룹별로 계산한 AUC 값보다 낮게 나오는 것입니다. 이는 모델의 예측력이 각 그룹 내에서는 유효하지만, 그룹 간의 차이를 통합적으로 다루는 데는 한계가 있음을 보여주는 현상입니다.



## 확률 임계값을 0.5로 하고, 분류 예측 모델의 민감도/특이도/정확도 를 구하라. 

thr <- 0.5
df <- df %>% 
  mutate(pred_label = as.integer(prob >= thr))
table(df$pred_label, df$high_risk)

# race 를 포함한 로지스틱 회귀 모델을 만들고, 기존 모델과 비교해보라. 

m_logit2 <- glm(high_risk ~ race + age + chronic_count + dx_count + 
                 drug_count + inpatient_history + past_cost, 
               data = df, family = binomial)
summary(m_logit2)

prob2 = predict(m_logit2, type = "response")
df = cbind(df, prob2)
pred2 = prediction(predictions = df$prob2, labels = df$high_risk)
perf2 = performance(pred2, "tpr", "fpr")
plot(perf2)

auc2 = performance(pred2, "auc")
auc2@y.values





