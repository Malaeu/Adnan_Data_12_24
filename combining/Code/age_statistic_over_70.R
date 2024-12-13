# Детальный анализ пациентов ≥70 лет
elderly_analysis <- wl_df_updated %>%
  filter(age >= 70) %>%
  select(
    age, sex, diagnosis_grouped, lab_meld, 
    surv_time, surv_status, received_ltx,
    has_ascites, has_varices, has_encephalopathy,
    year, period
  )

# Базовая статистика
elderly_summary <- elderly_analysis %>%
  summarise(
    n = n(),
    mean_age = round(mean(age, na.rm = TRUE), 1),
    max_age = round(max(age, na.rm = TRUE), 1),
    male_pct = round(mean(sex == "M", na.rm = TRUE) * 100, 1),
    mean_meld = round(mean(lab_meld, na.rm = TRUE), 1),
    death_rate = round(mean(surv_status, na.rm = TRUE) * 100, 1),
    transplant_rate = round(mean(received_ltx, na.rm = TRUE) * 100, 1),
    median_survival_days = round(median(surv_time, na.rm = TRUE), 0)
  )

print("Статистика по пациентам ≥70 лет:")
print(elderly_summary)

# Распределение диагнозов
diagnosis_dist <- elderly_analysis %>%
  count(diagnosis_grouped, sort = TRUE) %>%
  mutate(
    pct = round(n/sum(n)*100, 1),
    cumulative_pct = cumsum(pct)
  )

print("\nРаспределение диагнозов:")
print(diagnosis_dist)

# Осложнения
complications <- elderly_analysis %>%
  summarise(
    ascites_rate = round(mean(has_ascites, na.rm = TRUE) * 100, 1),
    varices_rate = round(mean(has_varices, na.rm = TRUE) * 100, 1),
    enceph_rate = round(mean(has_encephalopathy, na.rm = TRUE) * 100, 1)
  )

print("\nЧастота осложнений (%):")
print(complications)

# Распределение по годам
year_dist <- elderly_analysis %>%
  count(year) %>%
  mutate(pct = round(n/sum(n)*100, 1))

print("\nРаспределение по годам:")
print(year_dist)

# Индивидуальные случаи
individual_cases <- elderly_analysis %>%
  arrange(desc(age)) %>%
  select(age, sex, diagnosis_grouped, lab_meld, 
         surv_status, received_ltx, year)

print("\nИндивидуальные случаи (топ-10 по возрасту):")
print(head(individual_cases, 10))

# Сравнение с другими возрастными группами
age_comparison <- wl_df_updated %>%
  group_by(age_group) %>%
  summarise(
    n = n(),
    mean_meld = round(mean(lab_meld, na.rm = TRUE), 1),
    death_rate = round(mean(surv_status, na.rm = TRUE) * 100, 1),
    transplant_rate = round(mean(received_ltx, na.rm = TRUE) * 100, 1),
    median_survival = round(median(surv_time, na.rm = TRUE), 0)
  )

print("\nСравнение возрастных групп:")
print(age_comparison)

# Визуализация выживаемости по возрастным группам
library(survminer)
fit <- survfit(Surv(surv_time, surv_status) ~ age_group, data = wl_df_updated)

ggsurvplot(fit,
           data = wl_df_updated,
           pval = TRUE,
           conf.int = TRUE,
           risk.table = TRUE,
           xlab = "Time (days)",
           ylab = "Survival probability",
           title = "Выживаемость по возрастным группам",
           palette = "Set2",
           risk.table.height = 0.25)
