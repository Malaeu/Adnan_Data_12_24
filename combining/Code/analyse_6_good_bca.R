library(tidyverse)
library(gtsummary)
library(corrplot)
library(survival)
library(survminer)
library(pROC)

# 1. Связь BCA с тяжестью заболевания
# Создаем категории MELD
df_analysis <- df_analysis %>%
  mutate(
    meld_cat = cut(lab_meld,
                   breaks = c(0, 15, 25, Inf),
                   labels = c("Low MELD (≤15)", 
                              "Medium MELD (16-25)", 
                              "High MELD (>25)"))
  )

# Анализ BCA по категориям MELD
bca_by_meld <- df_analysis %>%
  group_by(meld_cat) %>%
  summarise(
    n = n(),
    across(c(bone, muscle, sat, vat, imat, eat, pat, tat),
           list(
             mean = ~mean(., na.rm = TRUE),
             sd = ~sd(., na.rm = TRUE)
           ),
           .names = "{.col}_{.fn}")
  )

print("BCA параметры по категориям MELD:")
print(bca_by_meld)

# Корреляционный анализ
clinical_correlations <- df_analysis %>%
  select(lab_meld, child_pugh_score, 
         bone, muscle, sat, vat, imat, eat, pat, tat) %>%
  cor(use = "pairwise.complete.obs")

# Визуализация корреляций
png("bca_clinical_correlations.png", width = 1000, height = 1000)
corrplot(clinical_correlations, 
         method = "color", 
         type = "upper", 
         addCoef.col = "black",
         tl.col = "black",
         tl.srt = 45)
dev.off()

# 2. Предикторы выживаемости

# Создаем композитные индексы BCA
df_analysis <- df_analysis %>%
  mutate(
    muscle_fat_ratio = muscle / (sat + vat),
    bone_muscle_ratio = bone / muscle,
    visceral_subcut_ratio = vat / sat
  )

# Cox regression для BCA параметров
cox_model <- coxph(Surv(surv_time, surv_status) ~ 
                     scale(bone) + scale(muscle) + 
                     scale(sat) + scale(vat) + 
                     scale(imat) + scale(eat) + 
                     scale(pat) + scale(tat) +
                     age + sex + lab_meld,
                   data = df_analysis)

print("\nCox regression results:")
print(summary(cox_model))

# 3. Влияние осложнений

# Анализ BCA у пациентов с осложнениями
complications_analysis <- df_analysis %>%
  pivot_longer(
    cols = c(has_ascites, has_varices, 
             has_encephalopathy, has_splenomegaly),
    names_to = "complication",
    values_to = "has_complication"
  ) %>%
  group_by(complication, has_complication) %>%
  summarise(
    n = n(),
    bone_mean = mean(bone, na.rm = TRUE),
    muscle_mean = mean(muscle, na.rm = TRUE),
    sat_mean = mean(sat, na.rm = TRUE),
    vat_mean = mean(vat, na.rm = TRUE),
    death_rate = mean(surv_status, na.rm = TRUE) * 100
  )

print("\nBCA параметры при осложнениях:")
print(complications_analysis)

# 4. Временные тренды

# Анализ изменений BCA во времени
time_analysis <- df_analysis %>%
  mutate(
    year = format(date_of_wl, "%Y")
  ) %>%
  group_by(year) %>%
  summarise(
    n = n(),
    across(c(bone, muscle, sat, vat, imat, eat, pat, tat),
           list(
             mean = ~mean(., na.rm = TRUE)
           ),
           .names = "{.col}_{.fn}"),
    death_rate = mean(surv_status, na.rm = TRUE) * 100
  )

print("\nВременные тренды BCA параметров:")
print(time_analysis)

# Создаем прогностическую модель
# Разделяем данные на тренировочный и тестовый наборы
set.seed(123)
train_index <- sample(1:nrow(df_analysis), 0.7 * nrow(df_analysis))
train_data <- df_analysis[train_index, ]
test_data <- df_analysis[-train_index, ]

# Логистическая регрессия для предсказания смертности
log_model <- glm(surv_status ~ 
                   scale(bone) + scale(muscle) + 
                   scale(sat) + scale(vat) + 
                   scale(imat) + scale(eat) + 
                   scale(pat) + scale(tat) +
                   age + sex + lab_meld +
                   has_ascites + has_varices + 
                   has_encephalopathy + has_splenomegaly,
                 data = train_data,
                 family = binomial())

# ROC анализ
predictions <- predict(log_model, newdata = test_data, type = "response")
roc_obj <- roc(test_data$surv_status, predictions)

print("\nМодель предсказания смертности:")
print(paste("AUC:", round(auc(roc_obj), 3)))

# Сохраняем результаты
results_list <- list(
  bca_meld = bca_by_meld,
  correlations = clinical_correlations,
  cox_model = cox_model,
  complications = complications_analysis,
  time_trends = time_analysis,
  prediction_model = log_model,
  roc_analysis = roc_obj
)

saveRDS(results_list, "bca_analysis_results.rds")
results_list
