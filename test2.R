

library(tidyverse)

df = read_csv("data/risk_classification_bias.csv")

str(df)

df$race = factor(df$race)
df$sex = factor(df$sex)
df$inpatient_history = factor(df$inpatient_history)
df$high_risk = factor(df$high_risk)

summary(df)


tab = table(df$race, df$high_risk)

chisq.test(tab)

df %>%
  ggplot(aes(race, past_cost)) + 
  geom_boxplot()

t.test(past_cost ~ race, data = df)

# high risk 군을 분류하기 위해 로지스틱 회귀 모델을 적합하라(단, 예측 변수로 race 는 제외할 것). 

df2 = df %>% select(-race)

my_model = glm(high_risk ~ ., data = df2, family = "binomial")

library(sjPlot)
plot_model(my_model, sort.est = T)

summary(my_model)

exp(coef(my_model))

prob = predict(my_model, df2, type = "response")

# ROC curve 를 그리고, AUC 값을 구하라. 
library(ROCR)

pred_obj = prediction(predictions = prob, 
           labels = df2$high_risk)
perf_obj = performance(pred_obj, "tpr", "fpr")
plot(perf_obj)    
auc = performance(pred_obj, "auc")
auc@y.values

# 인종 간의 AUC 값에 차이가 있는가?
# subset Black 
df_black = df[df$race == "Black",] %>% 
  select(-race) 

my_model_black = glm(high_risk ~ ., data = df_black, family = "binomial")

prob_black = predict(my_model_black, df_black, type = "response")

pred_obj_black = prediction(predictions = prob_black, 
                            labels = df_black$high_risk)
perf_obj_black = performance(pred_obj_black, "tpr", "fpr")
plot(perf_obj_black)    
auc_black = performance(pred_obj_black, "auc")
auc_black@y.values


# subset White 
df_white = df[df$race == "White",] %>% 
  select(-race) 

my_model_white = glm(high_risk ~ ., data = df_white, family = "binomial")

prob_white = predict(my_model, df_white, type = "response")

pred_obj_white = prediction(predictions = prob_white, 
                            labels = df_white$high_risk)
perf_obj_white = performance(pred_obj_white, "tpr", "fpr")
plot(perf_obj_white)    
auc_white = performance(pred_obj_white, "auc")
auc_white@y.values


# 확률 임계값을 0.5로 하고, 분류 예측 모델의 민감도/특이도/정확도 를 구하라. 

# cutoff 값
target_cutoff <- 0.5

# sens
perf <- performance(pred_obj, "sens", "cutoff")
cutoffs <- perf@x.values[[1]]
sens_values <- perf@y.values[[1]]
index <- which.min(abs(cutoffs - target_cutoff))
sens_at_cutoff <- sens_values[index]
cat("Sensitivity at cutoff", target_cutoff, ":", sens_at_cutoff, "\n")

# spec 
perf <- performance(pred_obj, "spec", "cutoff")
cutoffs <- perf@x.values[[1]]
spec_values <- perf@y.values[[1]]
index <- which.min(abs(cutoffs - target_cutoff))
spec_at_cutoff <- spec_values[index]
cat("Specificity at cutoff", target_cutoff, ":", spec_at_cutoff, "\n")

# acc
perf <- performance(pred_obj, "acc", "cutoff")
cutoffs <- perf@x.values[[1]]
acc_values <- perf@y.values[[1]]
index <- which.min(abs(cutoffs - target_cutoff))
acc_at_cutoff <- acc_values[index]
cat("Accuracy at cutoff", target_cutoff, ":", acc_at_cutoff, "\n")

# race 를 포함한 로지스틱 회귀 모델을 만들고, 기존 모델과 비교해보라(Odds ratios, ROC curve, AUC, etc) 

my_model = glm(high_risk ~ ., data = df, family = "binomial")

plot_model(my_model, sort.est = T)

summary(my_model)
exp(coef(my_model))

prob = predict(my_model, df, type = "response")

# ROC curve 를 그리고, AUC 값을 구하라. 
pred_obj = prediction(predictions = prob, 
                      labels = df$high_risk)
perf_obj = performance(pred_obj, "tpr", "fpr")
plot(perf_obj)    
auc = performance(pred_obj, "auc")
auc@y.values


# lasso regression 

library(GGally)
ggpairs(df)

library(glmnet)

x = model.matrix(high_risk ~ ., df)[,-1] # as.matrix, dummy variables 
y = df$high_risk

lasso_mod = glmnet(x, y, family = "binomial", alpha = 1, standardize = T)
plot(lasso_mod)

set.seed(1)
cv_lasso = cv.glmnet(x, y, family = "binomial", alpha = 1, nfolds = 10)

coef(cv_lasso, s = "lambda.min")

cv_lasso$lambda.min

plot(cv_lasso)





