

# Сначала ставим нужные пакеты
pacman::p_load(
  tidyverse,    # для манипуляций с данными
  lubridate,    # для работы с датами
  funModeling,  # для df_status
  janitor       # для clean_names
)


df_status(wl)

# Функция для красивого вывода процентов пропущенных значений
print_missing <- function(df) {
  df %>%
    summarise(across(everything(), ~sum(is.na(.))/length(.)*100)) %>%
    gather() %>%
    arrange(desc(value)) %>%
    filter(value > 0) %>%
    mutate(value = round(value, 2)) %>%
    rename(
      "Переменная" = key,
      "% пропущенных" = value
    )
}

# Чистим данные
wl_clean <- wl %>%
  # Конвертируем character в factor где нужно
  mutate(across(c(sex, blood_type, rh, primary_diagnosis, diagnose_2, 
                  diagnose_3, relisting_diagnosis, relisting_flag,
                  urgency, todesursache, za_wl, diagnosis,
                  abdominal_surgeries, portal_vein_thrombosis,
                  infections), as.factor)) %>%
  
  # Конвертируем character в numeric
  mutate(
    weight = as.numeric(weight),
    ltx_number = as.numeric(ltx_number)
  ) %>%
  
  # Конвертируем все даты
  mutate(across(ends_with("date") | contains("date_of") | 
                  matches("todesdatum|last_contact"), 
                ~as.Date(as.character(.)))) %>%
  
  # Создаем status и time
  mutate(
    status = if_else(!is.na(todesdatum), 1L, 0L),
    time = as.numeric(difftime(last_contact, date_of_wl, units = "days"))
  )

# Проверяем пропущенные значения
missing_summary <- print_missing(wl_clean)

# Базовая статистика по числовым переменным
numeric_summary <- wl_clean %>%
  select_if(is.numeric) %>%
  summary()

# Частоты для категориальных переменных
factor_summary <- wl_clean %>%
  select_if(is.factor) %>%
  map(~table(., useNA = "ifany"))

# Выводим результаты
cat("\n🔍 Топ-10 переменных с пропущенными значениями:\n")
print(head(missing_summary, 10))

cat("\n📊 Статистика по числовым переменным:\n")
print(numeric_summary)

cat("\n🎯 Проверка преобразования:\n")
str(wl_clean)
