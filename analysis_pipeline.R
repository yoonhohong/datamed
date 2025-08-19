# antihypertensive_dementia: end-to-end 분석 스크립트
# -------------------------------------------------------
# 환경 준비
# install.packages(c("tidyverse","tableone","janitor","survival","survminer","broom","ggthemes","forcats"))
library(tidyverse)
library(tableone)
library(janitor)
library(survival)
library(survminer)
library(broom)
library(ggthemes)

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
df <- df %>%
  # 연속형 변수를 범주형 변수로 변환 
  # 범주형 변수를 숫자형 변수로 변환 
  mutate(age_group = cut(age, 
                         breaks = c(60,65,70,75,80,85,90, Inf), 
                         right = FALSE), 
         dementia_dx_code = 
                  ifelse(dementia_dx == "No", 0, 1))

all(df$dementia_dx_code == df$event)


# 정제 & 가공 
# 이상치 완화
df$bmi = ifelse(df$bmi < 15, 15, df$bmi)


# 표 (baseline characteristics)
df = df[complete.cases(df),]

# 예) 범주형: 카이제곱 vs Fisher
tbl <- table(df$medication_adherence, df$dementia_dx)
tbl 

prop.table(tbl, margin = 1)

df %>%
  ggplot(aes(x = medication_adherence, 
             fill = dementia_dx)) +
  geom_bar(position = "fill") 

chisq.test(tbl)       # 기대도수 충분하면 OK
fisher.test(tbl)      # 표본 적거나 희귀범주면 이게 더 안전

# 예) age: t-test vs Wilcoxon
t.test(age ~ medication_adherence, data=df)
wilcox.test(bmi ~ medication_adherence, data=df)

df %>%
  ggplot(aes(age, group = medication_adherence, fill = medication_adherence)) + 
  geom_density(alpha = 0.5)

df %>%
  ggplot(aes(x = medication_adherence, 
             y = age)) +
  geom_boxplot() + 
  labs(x="Medication adherence", y="Age")

df %>%
  group_by(medication_adherence) %>%
  summarize(mean_age = mean(age), 
            sd = sd(age))

# 7) Kaplan–Meier & 로그순위
fit_km <- survfit(Surv(time_to_dementia, event) ~ 
                    medication_adherence, 
                  data = df)
fit_km 

plot_KM = ggsurvplot(fit_km, data = df, 
           risk.table = TRUE, 
           conf.int = TRUE)

plot_KM 

survdiff(Surv(time_to_dementia, event) ~ 
           medication_adherence, data = df)

# 8) Cox 회귀
cox_formula <- as.formula(Surv(time_to_dementia, event) ~ 
                            medication_adherence + 
                            age + 
                            sex +
                            num_comorbidities + 
                            insurance_quartile + 
                            outpatient_visits +
                            bmi + depression_dx + 
                            cognitive_score)
fit_cox <- coxph(cox_formula, data = df)
summary(fit_cox)

# 비례위험 가정 점검
zph <- cox.zph(fit_cox)
zph
plot(zph)  # 그래프 확인 원하면 주석 해제

# 이미지 저장 
ggsave(plot_KM$plot, filename = "img/km_curve.png", 
       width = 7, height = 5, dpi = 300)


