library(tidyverse)
library(survival)
library(survminer)

# 1. Динамическая оценка риска
# Создаем временные точки для анализа
df_dynamic <- df_analysis %>%
  mutate(
    time_period = cut(surv_time, 
                      breaks = c(0, 30, 90, 180, 365, Inf),
                      labels = c("1m", "3m", "6m", "1y", ">1y")),
    
    # Нормализуем BCA параметры относительно начальных значений
    muscle_norm = muscle / mean(muscle, na.rm = TRUE),
    fat_norm = (sat + vat) / mean(sat + vat, na.rm = TRUE),
    meld_norm = lab_meld / mean(lab_meld, na.rm = TRUE)
  )

# Анализ изменения риска во времени
time_risk_analysis <- df_dynamic %>%
  group_by(time_period) %>%
  summarise(
    n_patients = n(),
    death_rate = mean(surv_status == 1, na.rm = TRUE),
    mean_muscle = mean(muscle_norm, na.rm = TRUE),
    mean_fat = mean(fat_norm, na.rm = TRUE),
    mean_meld = mean(meld_norm, na.rm = TRUE)
  )

print("Динамика риска по периодам:")
print(time_risk_analysis)

# 2. Стратификация по дополнительным факторам
stratification_analysis <- df_analysis %>%
  mutate(
    age_group = cut(age, breaks = c(0, 40, 55, 70, Inf),
                    labels = c("<40", "40-55", "55-70", ">70")),
    meld_group = cut(lab_meld, 
                     breaks = c(0, 15, 25, Inf),
                     labels = c("Low MELD", "Medium MELD", "High MELD")),
    bmi_group = cut(bmi,
                    breaks = c(0, 18.5, 25, 30, Inf),
                    labels = c("Underweight", "Normal", "Overweight", "Obese"))
  ) %>%
  group_by(age_group, meld_group, bmi_group) %>%
  summarise(
    n = n(),
    death_rate = mean(surv_status == 1, na.rm = TRUE),
    mean_muscle = mean(muscle, na.rm = TRUE),
    mean_fat = mean(sat + vat, na.rm = TRUE),
    .groups = 'drop'
  )

print("\nСтратификация риска:")
print(stratification_analysis)

# 3. Анализ чувствительности модели
sensitivity_analysis <- function(data, parameter, range = seq(0.8, 1.2, by = 0.1)) {
  results <- map_df(range, function(x) {
    modified_data <- data
    modified_data[[parameter]] <- data[[parameter]] * x
    
    # Пересчитываем риск
    risk <- calculate_risk(
      muscle = modified_data$muscle,
      fat = modified_data$sat + modified_data$vat,
      meld = modified_data$lab_meld,
      age = modified_data$age
    )
    
    tibble(
      modification = x,
      mean_risk = mean(unlist(risk$risk_score), na.rm = TRUE)
    )
  })
  
  return(results)
}

# Проводим анализ чувствительности для основных параметров
sensitivity_results <- list(
  muscle = sensitivity_analysis(df_analysis, "muscle"),
  fat = sensitivity_analysis(df_analysis, "sat"),
  meld = sensitivity_analysis(df_analysis, "lab_meld")
)

print("\nАнализ чувствительности:")
print(sensitivity_results)

# 4. Рекомендации по снижению риска
generate_recommendations <- function(risk_score, bca_params) {
  recommendations <- list()
  
  # Анализируем мышечную массу
  if (bca_params$muscle < median(df_analysis$muscle, na.rm = TRUE)) {
    recommendations$muscle <- "Рекомендуется увеличение мышечной массы через физические упражнения и питание"
  }
  
  # Анализируем жировую массу
  if (bca_params$fat > median(df_analysis$sat + df_analysis$vat, na.rm = TRUE)) {
    recommendations$fat <- "Рекомендуется снижение жировой массы через диету и физическую активность"
  }
  
  # Анализируем MELD
  if (bca_params$meld > 15) {
    recommendations$meld <- "Требуется тщательный мониторинг функции печени"
  }
  
  return(recommendations)
}

# Пример использования рекомендаций
example_recommendations <- generate_recommendations(
  risk_score = 0.5,
  bca_params = list(
    muscle = median(df_analysis$muscle, na.rm = TRUE),
    fat = median(df_analysis$sat + df_analysis$vat, na.rm = TRUE),
    meld = median(df_analysis$lab_meld, na.rm = TRUE)
  )
)

print("\nПример рекомендаций:")
print(example_recommendations)

# 5. Создаем функцию для динамического наблюдения
track_risk_changes <- function(initial_values, follow_up_values) {
  # Рассчитываем изменения
  changes <- list(
    muscle_change = (follow_up_values$muscle - initial_values$muscle) / initial_values$muscle * 100,
    fat_change = (follow_up_values$fat - initial_values$fat) / initial_values$fat * 100,
    meld_change = (follow_up_values$meld - initial_values$meld) / initial_values$meld * 100
  )
  
  # Оцениваем тренд риска
  risk_trend <- case_when(
    changes$muscle_change > 5 & changes$fat_change < 0 ~ "Улучшение",
    changes$muscle_change < -5 | changes$meld_change > 10 ~ "Ухудшение",
    TRUE ~ "Стабильно"
  )
  
  return(list(
    changes = changes,
    trend = risk_trend,
    recommendations = generate_recommendations(NA, follow_up_values)
  ))
}

# Сохраняем результаты анализа
results_dynamic <- list(
  time_analysis = time_risk_analysis,
  stratification = stratification_analysis,
  sensitivity = sensitivity_results,
  recommendation_function = generate_recommendations,
  tracking_function = track_risk_changes
)

saveRDS(results_dynamic, "dynamic_risk_analysis.rds")
