# Pima indians diabetes dataset 

df = read.csv("data/diabetes.csv")
str(df)
df$Outcome = factor(df$Outcome)

# The datasets consist of several medical predictor (independent) variables and one target (dependent) variable, Outcome. 
# Independent variables include the number of pregnancies the patient has had, their BMI, insulin level, age, and so on.

# Number of times pregnant
# Plasma glucose concentration after 2 hours in an oral glucose tolerance test
# Diastolic blood pressure (mm Hg)
# Body mass index (weight in kg/(height in m)^2)
# Triceps skin fold thickness (mm)
# 2-Hour serum insulin (mu U/ml)
# Diabetes pedigree function


## EDA 

summary(df)


## Replace zero w/ NA in multiple columns 

library(dplyr)
temp = df %>%
  mutate(across(2:6, ~na_if(.x, 0)))
summary(temp)

## Replace NA w/ mean value in multiple columns 

library(tidyr) # replace_na 
temp2 = temp %>%
  mutate(across(2:6, ~replace_na(.x, round(mean(.x, na.rm = T)))))
summary(temp2)

## EDA: distribution 
library(GGally) 
ggpairs(temp2)

## EDA: correlation coefficients 
library(ggcorrplot)
r = cor(temp2[,-9])
pmat = cor_pmat(r)
ggcorrplot(r, 
           hc.order = TRUE, 
           type = "lower",
           lab = TRUE, 
           p.mat = pmat)

# logistic regression  
lr_fit = glm(Outcome ~ ., data = temp2, family = "binomial")
summary(lr_fit)


# Stepwise feature selection
library(MASS)
step_model = stepAIC(lr_fit, direction = "both", trace = F)
summary(step_model)

## 

library(sjPlot)
plot_model(step_model, sort.est = T)

## 
prob = predict(step_model, newdata = temp2, type = "response")
pred = rep(0, length(prob))
pred[prob > 0.5] = 1

mean(pred == temp2$Outcome); table(pred, temp2$Outcome)


## 회귀모델의 시각화      
library(visreg) 
library(ggpubr)
p = visreg(step_model, scale = "response", gg = T) # gg = T; ggplot2 as the engine 
ggarrange(plotlist = p)

for more informmation about visreg, visit.
https://pbreheny.github.io/visreg/reference/visreg.html

for more information about ggpubr  
http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/81-ggplot2-easy-way-to-mix-multiple-graphs-on-the-same-page/
  

# linear discriminant analysis 

lda_fit = lda(Outcome ~ ., data = temp2)
plot(lda_fit)

pred = predict(lda_fit, newdata = temp2)
pred = pred$class

## LDA: to evaluate model performance

-   accuracy = 1- error rate\
-   confusion matrix

mean(pred == df$Outcome); table(pred, df$Outcome) 



