library(tidyverse)
library(survival)

# 1. Сначала проверим данные
print("Количество наблюдений с полными данными:")
df_complete %>%
  summarise(
    total = n(),
    complete_bca = sum(!is.na(bone)),
    complete_clinical = sum(!is.na(catecholamine_cat) & 
                              !is.na(dialysis_cat) & 
                              !is.na(icu_cat) & 
                              !is.na(ventilation_cat)),
    complete_lab = sum(!is.na(lab_meld) & !is.na(child_pugh_score))
  ) %>%
  print()

# 2. Создаем датасет только с нужными переменными и без NA
model_data <- df_complete %>%
  select(
    surv_status, surv_time,
    # BCA параметры
    bone, muscle, sat, vat, imat,
    # Клинические параметры
    catecholamine_cat, dialysis_cat, icu_cat, ventilation_cat,
    # Базовые характеристики
    age, sex, lab_meld, child_pugh_score,
    # Осложнения
    has_ascites, has_varices, has_encephalopathy, hcc
  ) %>%
  na.omit()  # удаляем строки с NA

# 3. Создаем индексы на чистых данных
model_data <- model_data %>%
  mutate(
    # BCA индексы
    muscle_fat_ratio = muscle / (sat + vat),
    body_comp_index = (muscle + bone) / (sat + vat + imat),
    
    # Клинический индекс
    clinical_severity = case_when(
      dialysis_cat %in% c("1", "2") ~ 3,
      icu_cat %in% c("1", "2") ~ 2,
      catecholamine_cat %in% c("1", "2") ~ 2,
      ventilation_cat %in% c("1", "2") ~ 2,
      TRUE ~ 0
    ) +
      case_when(
        lab_meld > 25 ~ 3,
        lab_meld > 15 ~ 2,
        TRUE ~ 1
      ),
    
    # Комбинированный индекс
    combined_risk_index = (muscle_fat_ratio * 0.3 + 
                             body_comp_index * 0.3 +
                             clinical_severity * 0.4)
  )

# 4. Создаем модель
risk_model <- glm(surv_status ~ combined_risk_index + 
                    catecholamine_cat + dialysis_cat + icu_cat +
                    ventilation_cat + age + sex + has_ascites +
                    has_varices + has_encephalopathy + hcc +
                    child_pugh_score + lab_meld,
                  data = model_data,
                  family = binomial())

# 5. Добавляем предсказания
model_data <- model_data %>%
  mutate(
    risk_score = predict(risk_model, type = "response"),
    risk_level = case_when(
      risk_score < 0.2 ~ "Very Low Risk",
      risk_score < 0.4 ~ "Low Risk",
      risk_score < 0.6 ~ "Medium Risk",
      risk_score < 0.8 ~ "High Risk",
      TRUE ~ "Very High Risk"
    )
  )

# 6. Анализируем результаты
risk_distribution <- model_data %>%
  group_by(risk_level) %>%
  summarise(
    n = n(),
    deaths = sum(surv_status == 1),
    death_rate = round(mean(surv_status) * 100, 1),
    
    # Интенсивная терапия
    dialysis_pct = mean(dialysis_cat %in% c("1", "2")) * 100,
    icu_pct = mean(icu_cat %in% c("1", "2")) * 100,
    ventilation_pct = mean(ventilation_cat %in% c("1", "2")) * 100,
    catecholamine_pct = mean(catecholamine_cat %in% c("1", "2")) * 100,
    
    # Клинические параметры
    mean_meld = mean(lab_meld),
    mean_child_pugh = mean(child_pugh_score),
    mean_muscle_fat = mean(muscle_fat_ratio)
  )

print("\nРаспределение по уровням риска:")
print(risk_distribution)

# 7. Оцениваем качество модели
library(pROC)
roc_obj <- roc(model_data$surv_status, model_data$risk_score)
print(paste("\nAUC:", round(auc(roc_obj), 3)))

# 8. Сохраняем результаты
results <- list(
  model = risk_model,
  model_data = model_data,
  distribution = risk_distribution,
  roc = roc_obj,
  auc = auc(roc_obj)
)

saveRDS(results, "risk_model_results.rds")

# 9. Выводим важность предикторов
importance <- summary(risk_model)$coefficients %>%
  as.data.frame() %>%
  rownames_to_column("predictor") %>%
  filter(`Pr(>|z|)` < 0.05) %>%
  arrange(`Pr(>|z|)`)

print("\nЗначимые предикторы:")
print(importance)

results
