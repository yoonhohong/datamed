
library(ggplot2)

df = read.csv("data/wisconsin_breast_cancer.csv")

# 주성분 분석을 한 변수 데이터만 분리 
feature = df[,3:12] # 1,2 열은 각각 환자 ID, 진단(B: 양성, M:악성)

# 주성분 분석 
pca = prcomp(feature, scale = T) # 변수 값을 표준화(평균을 빼고, 표준편차로 나누어줌) 
result = pca$x # 각 환자별 주성분(PC1, PC2, ... PC10) 값 

# 시각화 
pc12 = result[,1:2] # PC1, PC2 선택 
df12 = data.frame(diagnosis = df$diagnosis, pc12) 
ggplot(data = df12, aes(x = PC1, y = PC2, col = diagnosis)) + geom_point() 








