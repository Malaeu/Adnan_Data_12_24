library(tidyverse)
library(caret)
library(pROC)
library(gtsummary)
library(gt)

# Загружаем и фильтруем данные только с BCA
df_bca <- readRDS("wl_df_with_bca.rds") %>%
  filter(!is.na(bone))  # фильтруем только случаи с BCA данными

# Проверяем данные
print("Статистика по BCA данным:")
df_bca %>%
  summarise(
    total_bca_patients = n(),
    cirrhosis_patients = sum(str_detect(diagnosis_grouped, "Cirrhosis")),
    deaths = sum(surv_status == 1),
    mean_age = mean(age, na.rm = TRUE),
    mean_meld = mean(lab_meld, na.rm = TRUE)
  ) %>%
  print()

# Подготавливаем данные для анализа
df_analysis <- df_bca %>%
  filter(str_detect(diagnosis_grouped, "Cirrhosis")) %>%
  mutate(
    # Создаем факторы
    mortality = factor(surv_status, 
                       levels = c(0, 1), 
                       labels = c("Alive", "Deceased")),
    sex = factor(sex, levels = c("M", "W"), 
                 labels = c("Male", "Female")),
    
    # Категоризируем непрерывные переменные
    meld_cat = cut(lab_meld,
                   breaks = c(0, 15, 25, Inf),
                   labels = c("Low", "Medium", "High")),
    
    age_cat = cut(age,
                  breaks = c(0, 40, 55, 70, Inf),
                  labels = c("<40", "40-55", "55-70", ">70")),
    
    # Стандартизируем BCA переменные
    across(c(bone, muscle, sat, vat, imat, eat, pat, tat),
           ~scale(.), .names = "scaled_{.col}")
  )

# Создаем таблицу характеристик
tbl_univariate <- df_analysis %>%
  select(
    # Исход
    mortality,
    # Демография
    age_cat, sex,
    # Клинические параметры
    meld_cat, bmi,
    # Осложнения
    has_ascites, has_varices, has_encephalopathy, has_splenomegaly,
    # BCA параметры (используем стандартизированные)
    starts_with("scaled_"),
    # Диагноз
    diagnosis_grouped
  ) %>%
  tbl_summary(
    by = mortality,
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = all_continuous() ~ 2
  ) %>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 3)) %>%
  add_overall() %>%
  add_n() %>%
  modify_header(label = "**Characteristic**") %>%
  modify_spanning_header(all_stat_cols() ~ "**Mortality Status**") %>%
  modify_caption("**Table 1. Patient characteristics by mortality status (BCA cohort)**")

# Логистическая регрессия
log_model <- glm(mortality ~ age_cat + sex + meld_cat + 
                   scaled_bone + scaled_muscle + scaled_sat + 
                   scaled_vat + scaled_imat + scaled_eat + 
                   scaled_pat + scaled_tat +
                   has_ascites + has_varices + 
                   has_encephalopathy + has_splenomegaly,
                 data = df_analysis,
                 family = binomial())

# Таблица мультивариатного анализа
tbl_multivariate <- log_model %>%
  tbl_regression(
    exponentiate = TRUE
  ) %>%
  modify_header(label = "**Predictor**") %>%
  modify_caption("**Table 2. Logistic regression results (BCA cohort)**")

# Объединяем таблицы
tbl_merged <- tbl_merge(
  tbls = list(tbl_univariate, tbl_multivariate),
  tab_spanner = c("**Univariate analysis**", "**Multivariate analysis**")
)

# ROC анализ
pred_probs <- predict(log_model, type = "response")
roc_obj <- roc(df_analysis$mortality, pred_probs)
auc_value <- auc(roc_obj)

# График ROC кривой
png("roc_curve_bca.png", width = 800, height = 800)
plot(roc_obj, main = paste("ROC Curve (AUC =", round(auc_value, 3), ")"))
dev.off()

# Корреляционный анализ BCA параметров
bca_cors <- df_analysis %>%
  select(starts_with("scaled_")) %>%
  cor()

# Тепловая карта корреляций
library(corrplot)
png("bca_correlations.png", width = 1000, height = 1000)
corrplot(bca_cors, method = "color", 
         type = "upper", 
         addCoef.col = "black",
         tl.col = "black",
         tl.srt = 45)
dev.off()

# Сохраняем результаты
tbl_merged %>%
  as_gt() %>%
  gtsave("bca_analysis_results.html")

# Выводим основные результаты
print("Основные результаты анализа BCA когорты:")
print(paste("Количество пациентов:", nrow(df_analysis)))
print(paste("AUC модели:", round(auc_value, 3)))
print("\nЗначимые предикторы (p < 0.05):")
summary(log_model)$coefficients %>%
  as.data.frame() %>%
  rownames_to_column("Variable") %>%
  filter(`Pr(>|z|)` < 0.05) %>%
  arrange(`Pr(>|z|)`) %>%
  print()
