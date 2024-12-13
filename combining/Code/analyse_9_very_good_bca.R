# 1. Сначала делаем базовый анализ выживаемости
library(tidyverse)
library(survival)
library(survminer)

# Создаем Kaplan-Meier кривые
km_fit <- survfit(Surv(surv_time, surv_status) ~ risk_level, 
                  data = df_analysis %>% filter(risk_level != "Unknown"))

# График выживаемости
surv_plot <- ggsurvplot(
  km_fit,
  data = df_analysis %>% filter(risk_level != "Unknown"),
  pval = TRUE,
  conf.int = TRUE,
  risk.table = TRUE,
  xlab = "Time (days)",
  ylab = "Survival probability",
  title = "Survival by Risk Level"
)

print(surv_plot)


# 4. Простой калькулятор риска
calculate_risk <- function(muscle, fat, meld, age) {
  # Рассчитываем основные индексы
  muscle_fat_ratio <- muscle / fat
  
  # Предсказываем риск (упрощенная версия)
  risk_score <- (meld * 0.05 + 
                   age * 0.02 - 
                   muscle_fat_ratio * 0.1)
  
  # Определяем категорию риска
  risk_level <- case_when(
    risk_score < 0.2 ~ "Low Risk",
    risk_score < 0.6 ~ "Medium Risk",
    TRUE ~ "High Risk"
  )
  
  return(list(
    risk_score = risk_score,
    risk_level = risk_level
  ))
}

# Пример использования
example <- calculate_risk(
  muscle = median(df_analysis$muscle, na.rm = TRUE),
  fat = median(df_analysis$sat + df_analysis$vat, na.rm = TRUE),
  meld = median(df_analysis$lab_meld, na.rm = TRUE),
  age = median(df_analysis$age, na.rm = TRUE)
)

print("Пример расчета риска:")
print(example)

