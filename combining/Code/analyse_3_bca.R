library(tidyverse)
library(gtsummary)
library(survival)
library(survminer)

# Создаем датасет с BCA данными
df_bca_294 <- wl_df_with_bca %>%
  filter(!is.na(bone)) %>%  # фильтруем только случаи с BCA (все 294)
  mutate(
    # Создаем факторы
    mortality = factor(surv_status, 
                       levels = c(0, 1), 
                       labels = c("Alive", "Deceased")),
    sex = factor(sex, levels = c("M", "W"), 
                 labels = c("Male", "Female")),
    
    # Категории MELD
    meld_cat = cut(lab_meld,
                   breaks = c(0, 15, 25, Inf),
                   labels = c("Low", "Medium", "High")),
    
    # Категории возраста
    age_cat = cut(age,
                  breaks = c(0, 40, 55, 70, Inf),
                  labels = c("<40", "40-55", "55-70", ">70"))
  )

# 1. Базовая характеристика когорты
print("Характеристика BCA когорты:")
df_bca_294 %>%
  summarise(
    n_total = n(),
    n_cirrhosis = sum(str_detect(diagnosis_grouped, "Cirrhosis")),
    n_deaths = sum(mortality == "Deceased"),
    mean_age = mean(age, na.rm = TRUE),
    mean_meld = mean(lab_meld, na.rm = TRUE),
    males = sum(sex == "Male"),
    females = sum(sex == "Female")
  ) %>%
  print()

# 2. Распределение по диагнозам
print("\nРаспределение по диагнозам:")
df_bca_294 %>%
  count(diagnosis_grouped, sort = TRUE) %>%
  mutate(
    pct = round(n/sum(n)*100, 1),
    cum_pct = cumsum(pct)
  ) %>%
  print()

# 3. Создаем таблицу характеристик с BCA параметрами
tbl_bca <- df_bca_294 %>%
  filter(str_detect(diagnosis_grouped, "Cirrhosis")) %>%
  select(
    mortality,
    # Демография
    age_cat, sex,
    # Клинические параметры
    meld_cat, bmi,
    # Осложнения
    has_ascites, has_varices, has_encephalopathy, has_splenomegaly,
    # BCA параметры
    bone, muscle, sat, vat, imat, eat, pat, tat,
    # Диагноз
    diagnosis_grouped
  ) %>%
  tbl_summary(
    by = mortality,
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = all_continuous() ~ 1
  ) %>%
  add_p() %>%
  add_overall() %>%
  add_n()

# 4. Анализ выживаемости
surv_fit <- survfit(Surv(surv_time, surv_status) ~ diagnosis_grouped, 
                    data = df_bca_294)

# График выживаемости
ggsurvplot(surv_fit,
           data = df_bca_294,
           pval = TRUE,
           conf.int = TRUE,
           risk.table = TRUE,
           xlab = "Time (days)",
           ylab = "Survival probability",
           title = "Survival by Diagnosis Group")

# 5. Корреляции между BCA параметрами
bca_cors <- df_bca_294 %>%
  select(bone, muscle, sat, vat, imat, eat, pat, tat) %>%
  cor()

print("\nКорреляции между BCA параметрами:")
print(round(bca_cors, 2))

# 6. Логистическая регрессия для предикторов смертности
log_model <- glm(mortality ~ age_cat + sex + meld_cat + 
                   bone + muscle + sat + vat + imat + eat + pat + tat +
                   has_ascites + has_varices + has_encephalopathy + has_splenomegaly,
                 data = df_bca_294,
                 family = binomial())

# Таблица результатов регрессии
tbl_regression(log_model, 
               exponentiate = TRUE) %>%
  modify_caption("**Predictors of Mortality**")

# 7. Сравнение BCA параметров по типам цирроза
bca_by_cirrhosis <- df_bca_294 %>%
  filter(str_detect(diagnosis_grouped, "Cirrhosis")) %>%
  group_by(diagnosis_grouped) %>%
  summarise(
    n = n(),
    bone_mean = mean(bone),
    muscle_mean = mean(muscle),
    sat_mean = mean(sat),
    vat_mean = mean(vat),
    imat_mean = mean(imat),
    eat_mean = mean(eat),
    pat_mean = mean(pat),
    tat_mean = mean(tat)
  ) %>%
  arrange(desc(n))

print("\nBCA параметры по типам цирроза:")
print(bca_by_cirrhosis)

# Сохраняем результаты
saveRDS(df_bca_294, "bca_294_analyzed.rds")
