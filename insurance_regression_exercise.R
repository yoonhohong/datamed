
df = read.csv("data/insurance.csv")
head(df)

str(df)

df = within(df, {
  sex = factor(sex)
  smoker = factor(smoker)
  region = factor(region)
})

summary(df)

# Medical Cost Personal Datasets
# https://www.kaggle.com/datasets/mirichoi0218/insurance
# 
# age: age of primary beneficiary (피보험자의 나이)
# sex: insurance contractor gender, female, male 
# bmi: Body mass index, providing an understanding of body, weights that are relatively high or low relative to height, objective index of body weight (kg/m^2) using the ratio of height to weight, ideally 18.5 to 24.9
# children: Number of children covered by health insurance / Number of dependents
# smoker: Smoking
# region: the beneficiary's residential area in the US, northeast, southeast, southwest, northwest.
# charges: Individual medical costs billed by health insurance

## 선형회귀 모델 적합  
lm_fit = lm(charges ~ ., data = df)
summary(lm_fit)

## 선형회귀 모델 적합  
lm_fit2 = lm(charges ~ age + bmi + children + smoker + region, data = df)
summary(lm_fit2)

## 공선성: 상관계수 
library(ggcorrplot)
r = cor(df[,-c(2,5,6)])

ggcorrplot(r, 
           hc.order = TRUE, 
           type = "lower",
           lab = TRUE)
cor_pmat(r)

## 공선성: 상관계수   
library(GGally) 
ggcorr(df, label = T)
ggpairs(df)

## 공선성: VIF (variance inflation factor)
library(olsrr)
ols_vif_tol(lm_fit)

for more information, visit 
https://cran.r-project.org/web/packages/olsrr/vignettes/regression_diagnostics.html

## 회귀모델의 시각화      
library(visreg) 
library(ggpubr)
p = visreg(lm_fit2, gg = TRUE) 
ggarrange(plotlist = p)

for more informmation about visreg, visit.
https://pbreheny.github.io/visreg/reference/visreg.html

for more information about ggpubr  
http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/81-ggplot2-easy-way-to-mix-multiple-graphs-on-the-same-page/
  

## 회귀모델의 시각화 
visreg(lm_fit2, "smoker", by = "region")

# Variance inflation factors measure the inflation in the variances of the parameter estimates due to collinearities that exist among the predictors. It is a measure of how much the variance of the estimated regression coefficient βk
# is “inflated” by the existence of correlation among the predictor variables in the model. A VIF of 1 means that there is no correlation among the kth predictor and the remaining predictor variables, and hence the variance of βk
# is not inflated at all. The general rule of thumb is that VIFs exceeding 4 warrant further investigation, while VIFs exceeding 10 are signs of serious multicollinearity requiring correction.
# 
# Percent of variance in the predictor that cannot be accounted for by other predictors.


