# 1. Сначала создадим полный датасет без NA для моделирования
df_complete <- df_analysis %>%
  select(surv_status, optimal_index, age, sex, 
         has_ascites, has_varices) %>%
  na.omit()

# 2. Создаем модель на полных данных
risk_model <- glm(surv_status ~ optimal_index + age + sex + 
                    has_ascites + has_varices,
                  data = df_complete,
                  family = binomial())

# 3. Добавляем предсказания и категории риска
df_analysis <- df_analysis %>%
  mutate(
    # Сначала проверяем все нужные переменные
    has_complete_data = !is.na(optimal_index) & !is.na(age) & 
      !is.na(sex) & !is.na(has_ascites) & 
      !is.na(has_varices),
    
    # Рассчитываем риск только для полных случаев
    risk_score = if_else(has_complete_data,
                         predict(risk_model, 
                                 newdata = cur_data(),
                                 type = "response"),
                         NA_real_),
    
    # Категоризируем риск
    risk_level = case_when(
      is.na(risk_score) ~ "Unknown",
      risk_score < 0.2 ~ "Very Low Risk",
      risk_score < 0.4 ~ "Low Risk",
      risk_score < 0.6 ~ "Medium Risk",
      risk_score < 0.8 ~ "High Risk",
      TRUE ~ "Very High Risk"
    )
  )

# 4. Анализируем распределение риска
risk_distribution <- df_analysis %>%
  count(risk_level) %>%
  mutate(pct = n/sum(n) * 100)

print("Распределение категорий риска:")
print(risk_distribution)

# 5. Анализируем смертность по категориям риска
mortality_by_risk <- df_analysis %>%
  filter(risk_level != "Unknown") %>%
  group_by(risk_level) %>%
  summarise(
    n = n(),
    deaths = sum(surv_status == 1, na.rm = TRUE),
    death_rate = round(mean(surv_status, na.rm = TRUE) * 100, 1),
    median_survival = median(surv_time[surv_status == 1], na.rm = TRUE)
  ) %>%
  arrange(factor(risk_level, 
                 levels = c("Very Low Risk", "Low Risk", 
                            "Medium Risk", "High Risk", 
                            "Very High Risk")))

print("\nСмертность по категориям риска:")
print(mortality_by_risk)

# 6. Анализируем BCA параметры по категориям риска
bca_by_risk <- df_analysis %>%
  filter(risk_level != "Unknown") %>%
  group_by(risk_level) %>%
  summarise(
    n = n(),
    muscle_fat_ratio = mean(muscle_fat_ratio, na.rm = TRUE),
    body_comp_index = mean(body_comp_index, na.rm = TRUE),
    mean_meld = mean(lab_meld, na.rm = TRUE)
  ) %>%
  arrange(factor(risk_level, 
                 levels = c("Very Low Risk", "Low Risk", 
                            "Medium Risk", "High Risk", 
                            "Very High Risk")))

print("\nBCA параметры по категориям риска:")
print(bca_by_risk)

# 7. Определяем оптимальные пороговые значения
cutoff_analysis <- df_analysis %>%
  filter(!is.na(risk_score)) %>%
  summarise(
    across(c(muscle_fat_ratio, body_comp_index, optimal_index),
           list(
             low_risk = ~quantile(., probs = 0.2, na.rm = TRUE),
             medium_risk = ~quantile(., probs = 0.5, na.rm = TRUE),
             high_risk = ~quantile(., probs = 0.8, na.rm = TRUE)
           ),
           .names = "{.col}_{.fn}")
  )

print("\nПороговые значения для клинического использования:")
print(cutoff_analysis)
