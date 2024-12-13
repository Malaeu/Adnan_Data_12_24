vars_to_impute <- vars_to_impute[!vars_to_impute %in% c("patient_number", "acsn_nr")]
str(wl_combined[vars_to_impute])
summary(wl_combined[vars_to_impute])

missing_counts <- sapply(numeric_data[vars_to_impute], function(x) sum(is.na(x)))
print(missing_counts)

# Копируем данные
wl_combined_clean <- wl_combined

# Заменяем NA в survival_days_until_ltx на максимальное значение survival_time
max_survival_time <- max(wl_combined_clean$survival_time, na.rm = TRUE)
wl_combined_clean$survival_days_until_ltx[is.na(wl_combined_clean$survival_days_until_ltx)] <- max_survival_time

# Заполняем death_on_the_list_without_ltx модой
mode_death_on_list <- as.numeric(names(sort(table(wl_combined_clean$death_on_the_list_without_ltx), decreasing = TRUE)[1]))
wl_combined_clean$death_on_the_list_without_ltx[is.na(wl_combined_clean$death_on_the_list_without_ltx)] <- mode_death_on_list

# Заполняем ltx
wl_combined_clean$ltx[is.na(wl_combined_clean$ltx)] <- ifelse(!is.na(wl_combined_clean$survival_days_until_ltx), 1, 0)

# Заполняем height, weight, bmi медианой
for (var in c("height", "weight", "bmi", "lab_meld_listing", "platelets_listing")) {
  wl_combined_clean[[var]][is.na(wl_combined_clean[[var]])] <- median(wl_combined_clean[[var]], na.rm = TRUE)
}

# Заполняем survival_time медианой
wl_combined_clean$survival_time[is.na(wl_combined_clean$survival_time)] <- median(wl_combined_clean$survival_time, na.rm = TRUE)

# Используем event вместо death
wl_combined_clean$event[is.na(wl_combined_clean$event)] <- wl_combined_clean$death[is.na(wl_combined_clean$event)]

# Удаляем столбец death
wl_combined_clean$death <- NULL

# Проверяем оставшиеся пропущенные значения
missing_summary <- sapply(wl_combined_clean, function(x) sum(is.na(x)))
print(missing_summary)






numeric_summary_long <- numeric_summary_long %>%
  mutate(missing = 566 - n,  # Предполагаем, что 566 - это общее количество наблюдений
         total = 566,
         pct_missing = (missing / total) * 100)

print(numeric_summary_long)

vars_to_impute <- numeric_summary_long %>%
  filter(!is.na(n) & !is.na(missing)) %>%
  filter(pct_missing < 50) %>%
  pull(variable)

cat("Переменные для импутации:", paste(vars_to_impute, collapse = ", "), "\n")

library(mice)

# Используем vars_to_impute_direct, так как это наиболее надежный метод
imputed_data <- mice(wl_combined[vars_to_impute], m = 5, maxit = 50, method = 'pmm', seed = 500)

# Заменяем импутированные значения в исходном датафрейме
wl_combined[vars_to_impute_direct] <- complete(imputed_data, 1)
library(corrplot)

# Выбираем только числовые переменные
numeric_vars <- sapply(wl_combined[vars_to_impute], is.numeric)
numeric_data <- wl_combined[vars_to_impute][, numeric_vars]

# Вычисляем корреляционную матрицу
cor_matrix <- cor(numeric_data, use = "pairwise.complete.obs")

# Визуализируем корреляционную матрицу
corrplot(cor_matrix, method = "color", type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

print(vars_to_impute)
str(numeric_data)

library(survival)
library(survminer)

# Анализ выживаемости
surv_obj <- Surv(wl_combined_clean$survival_time, wl_combined_clean$event)
km_fit <- survfit(surv_obj ~ 1)
print(ggsurvplot(km_fit, data = wl_combined_clean, risk.table = TRUE, 
                 title = "Кривая выживаемости Каплана-Мейера"))

# Анализ факторов, влияющих на выживаемость
cox_model <- coxph(surv_obj ~ age + bmi + lab_meld_listing + platelets_listing, data = wl_combined_clean)
print(summary(cox_model))

# Визуализация влияния факторов на выживаемость
print(ggforest(cox_model, data = wl_combined_clean))

# Анализ влияния переменных на lab_meld_listing
lm_model <- lm(lab_meld_listing ~ age + bmi + platelets_listing + bone + muscle + sat + vat, data = wl_combined_clean)
print(summary(lm_model))

# Визуализация зависимости lab_meld_listing от возраста и BMI
print(ggplot(wl_combined_clean, aes(x = age, y = lab_meld_listing, color = bmi)) +
        geom_point() +
        geom_smooth(method = "lm", se = FALSE) +
        labs(title = "Зависимость MELD score от возраста и BMI",
             x = "Возраст", y = "MELD score", color = "BMI"))

# Выбираем числовые переменные
numeric_vars <- sapply(wl_combined_clean, is.numeric)
numeric_vars <- names(numeric_vars)[numeric_vars]
numeric_vars <- setdiff(numeric_vars, c("event", "survival_time"))

# Выбираем категориальные переменные
categorical_vars <- sapply(wl_combined_clean, function(x) is.factor(x) | is.character(x))
categorical_vars <- names(categorical_vars)[categorical_vars]

# Функция для проверки количества уровней
check_levels <- function(x) {
  if(is.factor(x) || is.character(x)) {
    return(length(unique(x)) >= 2)
  }
  return(TRUE)
}

# Применяем функцию ко всем категориальным переменным
valid_categorical_vars <- sapply(wl_combined_clean[categorical_vars], check_levels)
categorical_vars <- names(valid_categorical_vars)[valid_categorical_vars]

print(paste("Валидные категориальные переменные:", paste(categorical_vars, collapse = ", ")))

# Убираем переменные, которые не нужны для анализа
vars_to_exclude <- c("patient_number", "date_of_ct", "acsn_nr", "ct_accession_nr_2_intern", 
                     "ct_accession_nr_extern", "date_of_wl", "date_of_nt", "last_contact", 
                     "date_of_ltx", "date_of_birth", "date_of_death", "unique_id", "etnr")

numeric_vars <- setdiff(numeric_vars, vars_to_exclude)
categorical_vars <- setdiff(categorical_vars, vars_to_exclude)

print(paste("Числовые переменные:", paste(numeric_vars, collapse = ", ")))
print(paste("Категориальные переменные:", paste(categorical_vars, collapse = ", ")))


for (var in categorical_vars) {
  levels_count <- length(unique(wl_combined_clean[[var]]))
  print(paste(var, ":", levels_count, "уровней"))
}

for (var in c(numeric_vars, categorical_vars)) {
  na_count <- sum(is.na(wl_combined_clean[[var]]))
  unique_count <- length(unique(wl_combined_clean[[var]]))
  print(paste(var, "- NA:", na_count, ", Уникальных значений:", unique_count))
}

na_threshold <- 0.3 * nrow(wl_combined_clean)  # 30% от общего числа наблюдений

valid_vars <- c()
for (var in c(numeric_vars, categorical_vars)) {
  if (sum(is.na(wl_combined_clean[[var]])) < na_threshold) {
    valid_vars <- c(valid_vars, var)
  } else {
    print(paste("Удалена переменная из-за большого количества NA:", var))
  }
}

print(paste("Количество валидных переменных:", length(valid_vars)))
print("Валидные переменные:")
print(valid_vars)
for (var in intersect(categorical_vars, valid_vars)) {
  wl_combined_clean[[var]] <- as.factor(wl_combined_clean[[var]])
}

library(survival)

cox_formula <- as.formula(paste("Surv(survival_time, event) ~", 
                                paste(valid_vars, collapse = " + ")))

cox_model_full <- tryCatch({
  coxph(cox_formula, data = wl_combined_clean)
}, error = function(e) {
  print(paste("Ошибка при построении полной модели:", e$message))
  return(NULL)
})

if (!is.null(cox_model_full)) {
  print(summary(cox_model_full))
} else {
  print("Полная модель не может быть построена. Попробуем пошаговый подход.")
  
  # Пошаговое построение модели
  base_model <- coxph(Surv(survival_time, event) ~ 1, data = wl_combined_clean)
  
  for (var in valid_vars) {
    tryCatch({
      new_formula <- update(formula(base_model), paste(". ~ . +", var))
      new_model <- coxph(new_formula, data = wl_combined_clean)
      base_model <- new_model
      print(paste("Добавлена переменная:", var))
    }, error = function(e) {
      print(paste("Ошибка при добавлении переменной:", var, "-", e$message))
    })
  }
  
  print(summary(base_model))
  cox_model_full <- base_model
}

print(head(wl_combined_clean[c("survival_time", "event")]))
print(summary(wl_combined_clean[c("survival_time", "event")]))
for (var in interesting_vars) {
  print(paste("Уникальные значения для", var, ":"))
  print(table(wl_combined_clean[[var]]))
}
simple_cox <- coxph(Surv(survival_time, event) ~ age + sex + blood_type + lab_meld_listing, data = wl_combined_clean)
print(summary(simple_cox))

library(survminer)

# Для возраста (разделим на две группы по медиане)
median_age <- median(wl_combined_clean$age, na.rm = TRUE)
wl_combined_clean$age_group <- ifelse(wl_combined_clean$age > median_age, "High", "Low")

surv_fit_age <- survfit(Surv(survival_time, event) ~ age_group, data = wl_combined_clean)
print(ggsurvplot(surv_fit_age, data = wl_combined_clean, 
                 title = "Кривая выживаемости по возрасту",
                 pval = TRUE, risk.table = TRUE))

# Для MELD score (разделим на две группы по медиане)
median_meld <- median(wl_combined_clean$lab_meld_listing, na.rm = TRUE)
wl_combined_clean$meld_group <- ifelse(wl_combined_clean$lab_meld_listing > median_meld, "High", "Low")

surv_fit_meld <- survfit(Surv(survival_time, event) ~ meld_group, data = wl_combined_clean)
print(ggsurvplot(surv_fit_meld, data = wl_combined_clean, 
                 title = "Кривая выживаемости по MELD score",
                 pval = TRUE, risk.table = TRUE))

# Для пола
surv_fit_sex <- survfit(Surv(survival_time, event) ~ sex, data = wl_combined_clean)
print(ggsurvplot(surv_fit_sex, data = wl_combined_clean, 
                 title = "Кривая выживаемости по полу",
                 pval = TRUE, risk.table = TRUE))

# Для группы крови
surv_fit_blood <- survfit(Surv(survival_time, event) ~ blood_type, data = wl_combined_clean)
print(ggsurvplot(surv_fit_blood, data = wl_combined_clean, 
                 title = "Кривая выживаемости по группе крови",
                 pval = TRUE, risk.table = TRUE))

print(colSums(is.na(wl_combined_clean)))



library(survminer)
print(ggforest(cox_model_full, data = wl_combined_clean))


interesting_vars <- c("sex", "blood_type", "primary_diagnosis", "hcc", "child_pugh_score", "exceptional_meld")

for (var in intersect(interesting_vars, valid_vars)) {
  surv_fit <- survfit(Surv(survival_time, event) ~ wl_combined_clean[[var]], data = wl_combined_clean)
  
  print(ggsurvplot(surv_fit, data = wl_combined_clean, 
                   title = paste("Кривая выживаемости по", var),
                   pval = TRUE, risk.table = TRUE))
  
  log_rank <- survdiff(Surv(survival_time, event) ~ wl_combined_clean[[var]], data = wl_combined_clean)
  print(paste("Лог-ранк тест для", var, ":"))
  print(log_rank)
}
numeric_vars_for_analysis <- intersect(numeric_vars, valid_vars)

for (var in numeric_vars_for_analysis) {
  median_value <- median(wl_combined_clean[[var]], na.rm = TRUE)
  wl_combined_clean[[paste0(var, "_group")]] <- ifelse(wl_combined_clean[[var]] > median_value, "High", "Low")
  
  surv_fit <- survfit(Surv(survival_time, event) ~ wl_combined_clean[[paste0(var, "_group")]], data = wl_combined_clean)
  
  print(ggsurvplot(surv_fit, data = wl_combined_clean, 
                   title = paste("Кривая выживаемости по", var),
                   pval = TRUE, risk.table = TRUE))
  
  log_rank <- survdiff(Surv(survival_time, event) ~ wl_combined_clean[[paste0(var, "_group")]], data = wl_combined_clean)
  print(paste("Лог-ранк тест для", var, ":"))
  print(log_rank)
}