# Cox Proportional Hazards Regression

## 1. 개요
Cox proportional hazards regression (콕스 비례위험 회귀)는 **생존분석(survival analysis)**에서 가장 널리 사용되는 모형 중 하나로, 
독립변수(공변량, covariates)가 사건 발생 위험(hazard)에 미치는 영향을 평가하는데 사용된다.

- 종속변수: 생존 시간(time-to-event)과 사건 발생 여부(event indicator)
- 독립변수: 연속형, 범주형 모두 가능
- 장점: 생존 시간의 분포(예: 지수분포, 와이블 분포 등)를 가정하지 않아도 된다 (semi-parametric model).

---

## 2. 위험 함수 (Hazard Function)
시간 \(t\)에서 사건이 발생할 **순간 위험도**는 다음과 같이 정의된다.

\[
h(t) = \lim_{\Delta t \to 0} \frac{P(t \leq T < t + \Delta t \mid T \geq t)}{\Delta t}
\]

여기서,  
- \(T\): 사건 발생 시간 (random variable)  
- \(h(t)\): 위험 함수 (hazard function)  

---

## 3. Cox 모형 수식

콕스 모형은 다음과 같이 표현된다.

\[
h(t \mid X) = h_0(t) \exp(\beta_1 X_1 + \beta_2 X_2 + \cdots + \beta_p X_p)
\]

- \(h_0(t)\): 기준 위험 함수(baseline hazard), 특정 기준군에서의 위험도  
- \(\beta_j\): 공변량 \(X_j\)의 회귀 계수  
- \(\exp(\beta_j)\): 공변량 \(X_j\)의 **위험비(hazard ratio, HR)**  

---

## 4. 추정 방법

- 모수적 방법과 달리 \(h_0(t)\)의 형태를 명시하지 않고, **partial likelihood (부분우도)**를 통해 \(\beta\)를 추정한다.
- 추정된 계수 \(\beta\)는 각 변수의 상대적 위험도를 설명한다.

---

## 5. 해석

- \(HR = \exp(\beta) > 1\): 해당 변수 증가 시 사건 발생 위험 증가  
- \(HR = \exp(\beta) < 1\): 해당 변수 증가 시 사건 발생 위험 감소  
- \(HR = 1\): 영향 없음  

예: 항고혈압제 지속 복용 여부(1=복용, 0=비복용)  
- \(HR = 0.74\): 지속 복용군의 치매 발생 위험이 비복용군 대비 26% 낮음.  

---

## 6. 비례위험 가정 (Proportional Hazards Assumption)

Cox 모형의 기본 가정은 **위험비(HR)가 시간에 따라 일정하다**는 것이다.

- 즉, 두 집단의 위험비는 시간이 흘러도 변하지 않는다.  
- 이를 검증하기 위해 Schoenfeld 잔차(Schoenfeld residuals) 분석 등을 사용한다.

---

## 7. R 코드 예시

```r
# survival 패키지 로드
library(survival)

# 데이터: antihypertensive_dementia_clean.csv 불러오기
df <- read.csv("antihypertensive_dementia_clean.csv")

# 생존 객체 생성
surv_obj <- Surv(time = df$followup_time, event = df$dementia_dx)

# Cox proportional hazards regression
cox_model <- coxph(surv_obj ~ medication_adherence + age + sex + bmi, data = df)

# 결과 요약
summary(cox_model)
```

---

## 8. 결론

- Cox 모형은 임상 연구에서 약물 효과, 예후 인자 등을 평가하는 핵심적인 분석 방법이다.
- HR의 해석은 상대적 위험도를 의미하며, 임상적 해석과 결합해야 의미가 있다.
