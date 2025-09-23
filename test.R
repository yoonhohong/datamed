
set.seed(1)
pop = rnorm(1000, mean = 0, sd = 10) 

mean(pop)
sd(pop)

sampl = sample(pop, 100, replace = F) 

mean(sampl)
sem = sd(sampl)/sqrt(length(sampl))

mean(sampl) - 2*sem 
mean(sampl) + 2*sem 

mean(pop) - 2*sem 
mean(pop) + 2*sem 


# 

temp = list()
for (i in 1:1000){
  set.seed(i)
  temp[[i]] = sample(sampl, 100, replace = T)
}

class(temp[1])
class(temp[[1]])

temp2 = c()
for (i in 1:1000){
  temp2[i] = mean(temp[[i]])
}

sd(temp2)


library(carData)

head(TitanicSurvival)
dim(TitanicSurvival)
names(TitanicSurvival)

summary(TitanicSurvival)

df = TitanicSurvival[complete.cases(TitanicSurvival),]

str(df)

mod_logistic = glm(survived ~ ., data = df, family = "binomial")

summary(mod_logistic)
exp(coef(mod_logistic))


saveRDS(mod_logistic, "data/logistic_model.rds")

mod = readRDS("data/logistic_model.rds")


load("test.RData")

index = sample(nrow(df), round(nrow(df)/3), replace = F)

testData = df[index,]
trainData = df[-index,]


mod = glm(survived ~ ., data = trainData
          , family = "binomial")  


prob = predict(mod, testData, type = "response")

pred = factor(ifelse(prob > 0.5, "yes", "no"))
obs = testData$survived

mean(pred == obs)

table(pred, obs)

library(ROCR)

pred_obj = prediction(predictions = prob, labels = obs) 

perf = performance(pred_obj, "tpr", "fpr")

plot(perf)

perf = performance(pred_obj, "auc")

perf@y.values











