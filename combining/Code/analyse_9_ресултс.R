# Визуализация выживаемости по периодам
ggplot(time_risk_analysis %>% filter(!is.na(time_period)), 
       aes(x = time_period, y = death_rate)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = paste0(round(death_rate*100, 1), "%")), 
            vjust = -0.5) +
  theme_minimal() +
  labs(title = "Смертность по периодам наблюдения",
       x = "Период",
       y = "Смертность (%)")

# Ключевые находки
print("Критические периоды:")
critical_periods <- time_risk_analysis %>%
  filter(!is.na(time_period)) %>%
  arrange(desc(death_rate)) %>%
  select(time_period, death_rate, mean_meld)

print(critical_periods)


# Анализ высокорисковых групп
high_risk_groups <- stratification_analysis %>%
  filter(death_rate > 0.5) %>%
  arrange(desc(death_rate))

print("Группы высокого риска:")
print(high_risk_groups)

# Создаем композитный индекс риска
risk_index <- stratification_analysis %>%
  mutate(
    risk_score = death_rate * (mean_fat/mean_muscle) * n/sum(n),
    risk_category = case_when(
      risk_score > quantile(risk_score, 0.75) ~ "High Risk",
      risk_score > quantile(risk_score, 0.25) ~ "Medium Risk",
      TRUE ~ "Low Risk"
    )
  )

# Визуализация чувствительности
sensitivity_plot <- bind_rows(
  sensitivity_results$muscle %>% mutate(parameter = "Muscle"),
  sensitivity_results$fat %>% mutate(parameter = "Fat"),
  sensitivity_results$meld %>% mutate(parameter = "MELD")
) %>%
  ggplot(aes(x = modification, y = mean_risk, color = parameter)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Чувствительность параметров",
       x = "Изменение параметра",
       y = "Средний риск")

print(sensitivity_plot)

# Создаем функцию для детальных рекомендаций
detailed_recommendations <- function(bca_params, meld_score) {
  tibble(
    parameter = c("Muscle", "Fat", "MELD"),
    current_value = c(bca_params$muscle, bca_params$fat, meld_score),
    target_value = c(
      median(df_analysis$muscle, na.rm = TRUE),
      median(df_analysis$sat + df_analysis$vat, na.rm = TRUE),
      15
    ),
    recommendation = case_when(
      parameter == "Muscle" & current_value < target_value ~ 
        "Увеличить мышечную массу: физ.нагрузки + протеин",
      parameter == "Fat" & current_value > target_value ~
        "Снизить жировую массу: диета + аэробные нагрузки",
      parameter == "MELD" & current_value > target_value ~
        "Консультация гепатолога, контроль функции печени",
      TRUE ~ "Поддерживать текущие показатели"
    )
  )
}

# Пример использования
example_recommendations <- detailed_recommendations(
  bca_params = list(
    muscle = 6000,
    fat = 10000
  ),
  meld_score = 20
)

print("Пример рекомендаций:")
print(example_recommendations)
