# antihypertensive_dementia; data cleaning  

library(tidyverse)

# 데이터 읽기
df <- readr::read_csv("data/antihypertensive_dementia.csv") 
glimpse(df)

# 기본 점검: 결측치 
na_count <- sapply(df, function(x) sum(is.na(x)))
na_count

# 정제 & 가공 
df <- df %>%
  # 범주형 변수(형변환, 레벨 조정) 
  mutate(sex = fct_collapse(factor(sex),
                       Male = c("M","Male"),
                       Female = c("F","Female")),
    insurance_quartile = factor(insurance_quartile),
    depression_dx = factor(depression_dx),
    medication_adherence = fct_collapse(factor(medication_adherence), High = c("H", "High"), Low = c("L", "Low")), 
    dementia_dx = fct_collapse(factor(dementia_dx),
                               Yes = c("yes", "Yes"), 
                               No = c("no", "No"))
  )


# 정제 & 가공 
# 이상치 완화
df$bmi = ifelse(df$bmi < 15, 15, df$bmi)

# 결측치(missing data) 처리 
df = df[complete.cases(df),]

# 저장 
write_csv(df, "data/antihypertensive_dementia_clean.csv")


