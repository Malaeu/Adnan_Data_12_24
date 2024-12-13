
# Сначала посмотрим на распределение пациентов с и без данных body composition:
#   
#   r

wl_combined %>%
  mutate(has_bca = !is.na(bone)) %>%
  group_by(has_bca) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  print()

# Теперь давай сравним основные характеристики пациентов с и без данных body composition:
#   
#   r

compare_groups <- wl_combined %>%
  mutate(has_bca = !is.na(bone)) %>%
  group_by(has_bca) %>%
  summarise(
    avg_age = mean(age, na.rm = TRUE),
    avg_bmi = mean(bmi, na.rm = TRUE),
    male_percent = mean(sex == "M") * 100,
    avg_lab_meld = mean(as.numeric(lab_meld_listing), na.rm = TRUE)
  )

print(compare_groups)

Визуализируем различия:
  
  r

library(ggplot2)

ggplot(wl_combined, aes(x = !is.na(bone), y = age)) +
  geom_boxplot() +
  labs(title = "Распределение возраста для пациентов с и без данных BCA",
       x = "Есть данные BCA", y = "Возраст")

ggplot(wl_combined, aes(x = !is.na(bone), y = bmi)) +
  geom_boxplot() +
  labs(title = "Распределение BMI для пациентов с и без данных BCA",
       x = "Есть данные BCA", y = "BMI")

Теперь давай посмотрим на корреляции между новыми переменными body composition:
  
  r

library(corrplot)

bca_vars <- c("bone", "muscle", "sat", "vat", "imat", "eat", "pat", "tat")
cor_matrix <- wl_combined %>%
  select(all_of(bca_vars)) %>%
  cor(use = "pairwise.complete.obs")

corrplot(cor_matrix, method = "color")

# Создадим несколько интересных визуализаций:
#   
#   r

# Scatter plot: muscle vs fat
ggplot(wl_combined, aes(x = muscle, y = sat)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Соотношение мышечной и жировой ткани",
       x = "Мышечная ткань", y = "Подкожный жир")

# Box plot: распределение tat по группам крови
ggplot(wl_combined, aes(x = blood_type, y = tat)) +
  geom_boxplot() +
  labs(title = "Распределение общего жира по группам крови",
       x = "Группа крови", y = "Общий жир (tat)")

# Scatter plot: BMI vs tat с разделением по полу
ggplot(wl_combined, aes(x = bmi, y = tat, color = sex)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Соотношение BMI и общего жира с разделением по полу",
       x = "BMI", y = "Общий жир (tat)")

# Наконец, давай посмотрим, есть ли связь между body composition и выживаемостью:
#   
#   r

# Создадим переменную выживаемости (в днях)
wl_combined <- wl_combined %>%
  mutate(survival_days = case_when(
    !is.na(survival_days_until_ltx) ~ survival_days_until_ltx,
    !is.na(survival_days_from_wl_until_death) ~ survival_days_from_wl_until_death,
    TRUE ~ as.numeric(difftime(last_contact, date_of_wl, units = "days"))
  ))

# Визуализируем связь между tat и выживаемостью
ggplot(wl_combined, aes(x = tat, y = survival_days)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Связь между общим жиром и выживаемостью",
       x = "Общий жир (tat)", y = "Дни выживаемости")



# Статистические тесты для проверки значимости различий между группами:
#   
#   r

library(tidyverse)
library(rstatix)

# Функция для проведения t-теста и вывода результатов
run_t_test <- function(data, var, group) {
  test <- t.test(data[[var]] ~ data[[group]])
  return(data.frame(
    variable = var,
    t_statistic = test$statistic,
    p_value = test$p.value
  ))
}

# Проверим различия в body composition между мужчинами и женщинами
bca_vars <- c("bone", "muscle", "sat", "vat", "imat", "eat", "pat", "tat")
sex_tests <- map_df(bca_vars, ~run_t_test(wl_combined, .x, "sex"))
print("Различия в body composition между полами:")
print(sex_tests)

# Проверим различия в body composition между группами крови
blood_type_tests <- map_df(bca_vars, function(var) {
  anova_result <- aov(as.formula(paste(var, "~ blood_type")), data = wl_combined)
  summary_result <- summary(anova_result)
  data.frame(
    variable = var,
    f_statistic = summary_result[[1]]$`F value`[1],
    p_value = summary_result[[1]]$`Pr(>F)`[1]
  )
})
print("Различия в body composition между группами крови:")
print(blood_type_tests)

Модели для прогнозирования выживаемости с учетом параметров body composition:
  
  r

library(survival)
library(survminer)

# Создадим переменную выживаемости (если еще не создана)
wl_combined <- wl_combined %>%
  mutate(
    survival_time = case_when(
      !is.na(survival_days_until_ltx) ~ survival_days_until_ltx,
      !is.na(survival_days_from_wl_until_death) ~ survival_days_from_wl_until_death,
      TRUE ~ as.numeric(difftime(last_contact, date_of_wl, units = "days"))
    ),
    event = ifelse(death == 1, 1, 0)
  )

# Создадим модель Кокса для каждой переменной body composition
cox_models <- map(bca_vars, function(var) {
  formula <- as.formula(paste("Surv(survival_time, event) ~", var))
  coxph(formula, data = wl_combined)
})

# Выведем результаты моделей
cox_results <- map_df(bca_vars, function(var) {
  model <- cox_models[[var]]
  summary <- summary(model)
  data.frame(
    variable = var,
    hazard_ratio = summary$conf.int[, "exp(coef)"],
    lower_ci = summary$conf.int[, "lower .95"],
    upper_ci = summary$conf.int[, "upper .95"],
    p_value = summary$coefficients[, "Pr(>|z|)"]
  )
})
print("Результаты моделей Кокса для переменных body composition:")
print(cox_results)

# Визуализация кривых выживаемости для одной из значимых переменных (например, muscle)
significant_var <- cox_results$variable[which.min(cox_results$p_value)]
fit <- survfit(Surv(survival_time, event) ~ cut(wl_combined[[significant_var]], breaks = 3), data = wl_combined)
ggsurvplot(fit, data = wl_combined, risk.table = TRUE, pval = TRUE,
           title = paste("Кривые выживаемости для", significant_var))

Исследование связи body composition с другими важными клиническими показателями:
  
  r

# Выберем важные клинические показатели
clinical_vars <- c("age", "bmi", "lab_meld_listing", "child_pugh_score")

# Создадим корреляционную матрицу
cor_matrix <- wl_combined %>%
  select(all_of(c(bca_vars, clinical_vars))) %>%
  mutate(across(where(is.character), as.numeric)) %>%
  cor(use = "pairwise.complete.obs")

# Визуализируем корреляционную матрицу
library(corrplot)
corrplot(cor_matrix, method = "color", type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45)

# Проведем линейную регрессию для каждой пары BCA и клинического показателя
regression_results <- expand.grid(bca_var = bca_vars, clinical_var = clinical_vars) %>%
  rowwise() %>%
  mutate(
    formula = paste(bca_var, "~", clinical_var),
    model = list(lm(as.formula(formula), data = wl_combined)),
    r_squared = summary(model)$r.squared,
    p_value = anova(model)$`Pr(>F)`[1]
  ) %>%
  select(-formula, -model)

print("Результаты линейной регрессии между BCA и клиническими показателями:")
print(regression_results)

# Визуализация наиболее значимых связей
top_associations <- regression_results %>%
  arrange(p_value) %>%
  head(3)

for (i in 1:nrow(top_associations)) {
  p <- ggplot(wl_combined, aes_string(x = top_associations$clinical_var[i], y = top_associations$bca_var[i])) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", se = FALSE, color = "red") +
    labs(title = paste("Связь между", top_associations$clinical_var[i], "и", top_associations$bca_var[i]))
  print(p)
}