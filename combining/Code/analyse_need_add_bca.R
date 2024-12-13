library(tidyverse)
library(gtsummary)
library(gt)

# 1. Определяем группы переменных
binary_vars <- c(
  "surv_status", "has_ascites", "has_varices", 
  "has_encephalopathy", "has_splenomegaly"
)

ordinal_vars <- c(
  "meld_group", "age_group", "period"
)

numeric_vars <- c(
  "age", "lab_meld", "bmi",
  "bone", "muscle", "sat", "vat", 
  "imat", "eat", "pat", "tat"
)

# 2. Создаем правильные метки для переменных
var_labels <- list(
  mortality = "Mortality",
  age = "Age",
  lab_meld = "MELD Score",
  bmi = "BMI",
  bone = "Bone tissue",
  muscle = "Muscle tissue",
  sat = "Subcutaneous fat",
  vat = "Visceral fat",
  imat = "Intramuscular fat",
  eat = "Epicardial fat",
  pat = "Paraaortic fat",
  tat = "Total adipose tissue",
  has_ascites = "Ascites",
  has_varices = "Varices",
  has_encephalopathy = "Encephalopathy",
  has_splenomegaly = "Splenomegaly",
  meld_group = "MELD category",
  age_group = "Age group",
  period = "Time period"
)

# 3. Подготавливаем данные для анализа
df_analysis <- df_bca_294 %>%
  mutate(
    # Создаем mortality из surv_status
    mortality = factor(surv_status, 
                       levels = c(0, 1),
                       labels = c("Alive", "Deceased")),
    
    # Бинарные переменные в факторы
    across(c(has_ascites, has_varices, 
             has_encephalopathy, has_splenomegaly),
           ~factor(.x, levels = c(FALSE, TRUE), 
                   labels = c("No", "Yes"))),
    
    # MELD группы
    meld_group = cut(lab_meld,
                     breaks = c(-Inf, 15, 25, Inf),
                     labels = c("Low", "Medium", "High")),
    
    # Возрастные группы
    age_group = cut(age,
                    breaks = c(-Inf, 40, 55, 70, Inf),
                    labels = c("<40", "40-55", "55-70", ">70"))
  )

# 4. Создаем таблицу характеристик
tbl_characteristics <- df_analysis %>%
  select(
    mortality,
    # Демография
    age, age_group,
    # Клинические параметры
    lab_meld, meld_group, bmi,
    # BCA параметры
    bone, muscle, sat, vat, imat, eat, pat, tat,
    # Осложнения
    has_ascites, has_varices, has_encephalopathy, has_splenomegaly
  ) %>%
  tbl_summary(
    by = mortality,
    missing = "no",
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = all_continuous() ~ 1,
    label = var_labels
  ) %>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 3)) %>%
  add_overall() %>%
  add_n() %>%
  modify_header(label = "**Characteristic**") %>%
  modify_spanning_header(all_stat_cols() ~ "**Mortality Status**") %>%
  modify_caption("**Table 1. Patient characteristics by mortality status**")

# 5. Создаем логистическую регрессию
log_model <- glm(mortality ~ age + lab_meld + bmi +
                   bone + muscle + sat + vat + imat + eat + pat + tat +
                   has_ascites + has_varices + has_encephalopathy + has_splenomegaly,
                 data = df_analysis,
                 family = binomial())

# 6. Таблица результатов регрессии
tbl_regression <- tbl_regression(
  log_model,
  exponentiate = TRUE,
  label = var_labels
) %>%
  modify_header(label = "**Predictor**") %>%
  modify_caption("**Table 2. Logistic regression results**")

# 7. Объединяем таблицы
tbl_merged <- tbl_merge(
  tbls = list(tbl_characteristics, tbl_regression),
  tab_spanner = c("**Univariate analysis**", "**Multivariate analysis**")
)

# 8. Сохраняем результаты
tbl_merged %>%
  as_gt() %>%
  gtsave("bca_analysis_table.html")

# 9. Выводим основные результаты
print(tbl_characteristics)
print(tbl_regression)
