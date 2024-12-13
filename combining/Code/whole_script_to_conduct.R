library(tidyverse)
library(lubridate)
library(mice)

# Шаг 1: Загрузка данных
wl <- readRDS("wl.rds")
wl_v <- readRDS("wl_v.rds")
wl_adnan <- readRDS("wl_adnan.rds")
wl_bca <- readRDS("wl_bca.rds")

# Шаг 2: Предобработка дат
process_dates <- function(df) {
  df %>%
    mutate(across(contains("date"), ~as.Date(as.character(.))))
}

wl <- process_dates(wl)
wl_v <- process_dates(wl_v)
wl_adnan <- process_dates(wl_adnan)

# Проверка корректности дат
check_dates <- function(df) {
  date_cols <- df %>% select(contains("date")) %>% names()
  for (col in date_cols) {
    if (any(df[[col]] > Sys.Date(), na.rm = TRUE)) {
      warning(paste("Колонка", col, "содержит даты из будущего. Проверь эту хуйню!"))
    }
  }
}

check_dates(wl)
check_dates(wl_v)
check_dates(wl_adnan)

# Шаг 3: Создание unique_id
create_unique_id <- function(df) {
  df %>%
    mutate(unique_id = paste(sex, date_of_birth, blood_type, sep = "_"))
}

wl <- create_unique_id(wl)
wl_v <- create_unique_id(wl_v)
wl_adnan <- create_unique_id(wl_adnan)

# Шаг 4: Объединение wl и wl_v
wl_combined <- bind_rows(wl, wl_v) %>% distinct()

# Шаг 5: Добавление etnr к wl_adnan
etnr_data <- wl_combined %>% select(unique_id, etnr)
wl_adnan <- wl_adnan %>% left_join(etnr_data, by = "unique_id")

# Шаг 6: Фильтрация взрослых пациентов
wl_adnan_adult <- wl_adnan %>%
  mutate(age = as.numeric(difftime(Sys.Date(), date_of_birth, units = "days")) / 365.25) %>%
  filter(age >= 18)

# Проверка возраста
if (any(wl_adnan_adult$age < 18, na.rm = TRUE)) {
  stop("Бля, у нас все еще есть пациенты младше 18 лет. Проверь эту хуйню!")
}

# Шаг 7: Объединение с данными body composition
wl_combined <- wl_adnan_adult %>%
  left_join(wl_bca, by = "acsn_nr")

# Шаг 8: Создание переменной выживаемости
wl_combined <- wl_combined %>%
  mutate(
    survival_time = case_when(
      !is.na(survival_days_until_ltx) ~ survival_days_until_ltx,
      !is.na(survival_days_from_wl_until_death) ~ survival_days_from_wl_until_death,
      TRUE ~ as.numeric(difftime(last_contact, date_of_wl, units = "days"))
    ),
    event = ifelse(death == 1, 1, 0)
  )

# Проверка выживаемости
if (any(wl_combined$survival_time < 0, na.rm = TRUE)) {
  warning("Есть отрицательные значения выживаемости. Проверь эту хуйню!")
}

# Шаг 9: Обработка пропущенных значений (пример для числовых переменных)
numeric_summary <- wl_combined %>%
  select_if(is.numeric) %>%
  summarise(across(everything(), 
                   list(n = ~sum(!is.na(.)),
                        n_missing = ~sum(is.na(.)),
                        pct_missing = ~mean(is.na(.)) * 100)))

# Преобразуем в правильный длинный формат
numeric_summary_long <- numeric_summary %>%
  pivot_longer(everything(), 
               names_to = c("variable", "stat"), 
               names_pattern = "(.+)_(.+)") %>%
  pivot_wider(names_from = stat, values_from = value)

print(head(numeric_summary_long, 10))



# Ручной выбор переменных для импутации
vars_to_impute <- names(wl_combined)[sapply(wl_combined, function(x) mean(is.na(x)) < 0.5 & is.numeric(x))]

cat("\nПеременные для импутации (ручной метод):", paste(vars_to_impute, collapse = ", "), "\n")

# Проверяем, все ли выбранные переменные действительно числовые
numeric_vars <- sapply(wl_combined[vars_to_impute], is.numeric)
vars_to_impute <- names(numeric_vars)[numeric_vars]

cat("\nЧисловые переменные для импутации:", paste(vars_to_impute, collapse = ", "), "\n")

if (length(vars_to_impute) == 0) {
  cat("Нет числовых переменных для импутации. Все переменные имеют более 50% пропусков или проблемы с данными.\n")
} else {
  # Пробуем импутацию
  tryCatch({
    imputed_data <- mice(wl_combined[vars_to_impute], m = 5, maxit = 50, method = 'pmm', seed = 500)
    wl_combined[vars_to_impute] <- complete(imputed_data, 1)
    cat("\nИмпутация успешно выполнена.\n")
  }, error = function(e) {
    cat("\nОшибка при импутации:", e$message, "\n")
    cat("Применяем заполнение медианой.\n")
    for (var in vars_to_impute) {
      wl_combined[[var]] <- ifelse(is.na(wl_combined[[var]]), 
                                   median(wl_combined[[var]], na.rm = TRUE),
                                   wl_combined[[var]])
    }
  })
}

# Проверяем оставшиеся пропущенные значения
missing_summary <- wl_combined %>%
  summarise(across(everything(), ~sum(is.na(.))))

cat("\nКоличество оставшихся пропущенных значений:\n")
print(missing_summary)

# Выводим статистику по импутированным переменным
imputed_stats <- wl_combined %>%
  select(all_of(vars_to_impute)) %>%
  summary()

cat("\nСтатистика по импутированным переменным:\n")
print(imputed_stats)

# Определим категориальные переменные
cat_vars <- c("blood_type", "sex", "primary_diagnosis", "hcc", "child_pugh_score", "exceptional_meld")

# Заполним пропуски в категориальных переменных модой (наиболее частым значением)
for (var in cat_vars) {
  if (var %in% names(wl_combined)) {
    mode_value <- names(sort(table(wl_combined[[var]]), decreasing = TRUE))[1]
    wl_combined[[var]] <- ifelse(is.na(wl_combined[[var]]), mode_value, wl_combined[[var]])
    wl_combined[[var]] <- as.factor(wl_combined[[var]])
  }
}

bca_vars <- c("bone", "muscle", "sat", "vat", "imat", "eat", "pat", "tat")

for (var in bca_vars) {
  wl_combined[[var]] <- ifelse(is.na(wl_combined[[var]]), 
                               median(wl_combined[[var]], na.rm = TRUE),
                               wl_combined[[var]])
}

missing_summary <- wl_combined %>%
  summarise(across(everything(), ~sum(is.na(.))))

cat("\nКоличество оставшихся пропущенных значений:\n")
print(missing_summary)
summary_stats <- wl_combined %>%
  select(-c(date_of_ct, date_of_wl, date_of_nt, last_contact, date_of_ltx, date_of_birth, date_of_death)) %>%
  summary()

print(summary_stats)


library(ggplot2)

# Гистограмма возраста
ggplot(wl_combined, aes(x = age)) +
  geom_histogram(bins = 30, fill = "blue", alpha = 0.7) +
  labs(title = "Распределение возраста")

# Boxplot BMI по группам крови
ggplot(wl_combined, aes(x = blood_type, y = bmi)) +
  geom_boxplot() +
  labs(title = "Распределение BMI по группам крови")

# Scatter plot: возраст vs lab_meld_listing
ggplot(wl_combined, aes(x = age, y = lab_meld_listing)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Зависимость MELD от возраста")




wl_combined[numeric_vars] <- complete(imputed_data, 1)

# Шаг 10: Сохранение финального датасета
saveRDS(wl_combined, "wl_combined_final.rds")

# Вывод основной информации о датасете
cat("Финальный датасет создан. Основная информация:\n")
cat("Количество строк:", nrow(wl_combined), "\n")
cat("Количество столбцов:", ncol(wl_combined), "\n")
cat("Количество пациентов с данными body composition:", sum(!is.na(wl_combined$bone)), "\n")
cat("Диапазон возрастов:", min(wl_combined$age), "-", max(wl_combined$age), "\n")
cat("Распределение по полу:\n")
print(table(wl_combined$sex))
cat("Распределение по группам крови:\n")
print(table(wl_combined$blood_type))
