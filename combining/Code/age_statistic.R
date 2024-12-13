year_distribution <- wl_adnan_adult_fixed %>%
  mutate(birth_year = as.numeric(format(date_of_birth, "%Y"))) %>%
  group_by(birth_year) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

cat("\nТоп-10 годов рождения:\n")
print(head(year_distribution, 10))
cat("\nМинимальный год рождения:", min(year_distribution$birth_year), "\n")
cat("Максимальный год рождения:", max(year_distribution$birth_year), "\n")

cat("\nФинальная статистика:\n")
cat("Общее количество пациентов:", nrow(wl_adnan_adult_fixed), "\n")
cat("Количество пациентов с etnr:", sum(!is.na(wl_adnan_adult_fixed$etnr)), "\n")
cat("Количество уникальных etnr:", n_distinct(wl_adnan_adult_fixed$etnr, na.rm = TRUE), "\n")

# Проверим распределение по полу
sex_distribution <- wl_adnan_adult_fixed %>%
  group_by(sex) %>%
  summarise(count = n())

cat("\nРаспределение по полу:\n")
print(sex_distribution)

# Проверим распределение по группам крови
blood_type_distribution <- wl_adnan_adult_fixed %>%
  group_by(blood_type) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

cat("\nРаспределение по группам крови:\n")
print(blood_type_distribution)


wl_adnan_adult_fixed <- wl_adnan_adult_fixed %>%
  mutate(age = as.numeric(format(Sys.Date(), "%Y")) - as.numeric(format(date_of_birth, "%Y")))

age_distribution <- wl_adnan_adult_fixed %>%
  group_by(age) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

cat("\nТоп-10 возрастов:\n")
print(head(age_distribution, 10))

cat("\nСредний возраст:", mean(wl_adnan_adult_fixed$age), "\n")
cat("Медианный возраст:", median(wl_adnan_adult_fixed$age), "\n")

wl_adnan_adult_fixed <- wl_adnan_adult_fixed %>%
  mutate(age = as.numeric(format(Sys.Date(), "%Y")) - as.numeric(format(date_of_birth, "%Y")))

# Проверим, что колонка добавилась
cat("Первые несколько значений возраста:\n")
print(head(wl_adnan_adult_fixed$age))

age_distribution <- wl_adnan_adult_fixed %>%
  group_by(age) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

cat("\nТоп-10 возрастов:\n")
print(head(age_distribution, 10))

cat("\nСредний возраст:", mean(wl_adnan_adult_fixed$age), "\n")
cat("Медианный возраст:", median(wl_adnan_adult_fixed$age), "\n")

decade_distribution <- wl_adnan_adult_fixed %>%
  mutate(decade = floor(as.numeric(format(date_of_birth, "%Y")) / 10) * 10) %>%
  group_by(decade) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

cat("\nРаспределение по десятилетиям:\n")
print(decade_distribution)



strange_ages <- wl_adnan_adult_fixed %>%
  filter(age < 0 | age > 100)

cat("\nКоличество странных возрастов:", nrow(strange_ages), "\n")
if (nrow(strange_ages) > 0) {
  cat("Примеры странных возрастов:\n")
  print(head(strange_ages %>% select(patient_number, date_of_birth, age)))
}

saveRDS(wl_adnan_adult_fixed, "wl_adnan_adult_cleaned_final.rds")
cat("\nОбновленный датасет сохранен в файл 'wl_adnan_adult_cleaned_final.rds'\n")


wl_adnan_adult_18plus <- wl_adnan_adult_fixed %>%
  filter(age >= 18)

cat("Количество пациентов до фильтрации:", nrow(wl_adnan_adult_fixed), "\n")
cat("Количество пациентов после фильтрации (18+):", nrow(wl_adnan_adult_18plus), "\n")
cat("Количество удаленных пациентов:", nrow(wl_adnan_adult_fixed) - nrow(wl_adnan_adult_18plus), "\n")


# Возрастное распределение
cat("\nОбновленная возрастная статистика:\n")
cat("Средний возраст:", mean(wl_adnan_adult_18plus$age), "\n")
cat("Медианный возраст:", median(wl_adnan_adult_18plus$age), "\n")

age_distribution <- wl_adnan_adult_18plus %>%
  group_by(age) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

cat("\nТоп-10 возрастов (18+):\n")
print(head(age_distribution, 10))

# Распределение по десятилетиям
decade_distribution <- wl_adnan_adult_18plus %>%
  mutate(decade = floor(as.numeric(format(date_of_birth, "%Y")) / 10) * 10) %>%
  group_by(decade) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

cat("\nОбновленное распределение по десятилетиям:\n")
print(decade_distribution)

# Распределение по полу
sex_distribution <- wl_adnan_adult_18plus %>%
  group_by(sex) %>%
  summarise(count = n())

cat("\nОбновленное распределение по полу:\n")
print(sex_distribution)

# Распределение по группам крови
blood_type_distribution <- wl_adnan_adult_18plus %>%
  group_by(blood_type) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

cat("\nОбновленное распределение по группам крови:\n")
print(blood_type_distribution)


saveRDS(wl_adnan_adult_18plus, "wl_adnan_adult_18plus_cleaned.rds")
cat("\nОбновленный датасет (18+) сохранен в файл 'wl_adnan_adult_18plus_cleaned.rds'\n")

library(corrplot)
numeric_vars <- wl_adnan_adult_18plus %>% select_if(is.numeric)
cor_matrix <- cor(numeric_vars, use = "complete.obs")
corrplot(cor_matrix, method = "color")


numeric_summary <- wl_adnan_adult_18plus %>%
  select_if(is.numeric) %>%
  summarise_all(list(
    n = ~sum(!is.na(.)),
    n_missing = ~sum(is.na(.)),
    pct_missing = ~mean(is.na(.)) * 100
  )) %>%
  pivot_longer(everything(), names_to = c("variable", ".value"), names_pattern = "(.+)_(.+)")

print(numeric_summary)

cat("Структура wl_adnan_adult_18plus:\n")
str(wl_adnan_adult_18plus)

cat("\nСтруктура wl_bca:\n")
str(wl_bca)

cat("acsn_nr в wl_adnan_adult_18plus:", "acsn_nr" %in% names(wl_adnan_adult_18plus), "\n")
cat("acsn_nr в wl_bca:", "acsn_nr" %in% names(wl_bca), "\n")



# Теперь объединим датасеты:
#   
#   r

wl_combined <- wl_adnan_adult_18plus %>%
  left_join(wl_bca, by = "acsn_nr")

cat("Количество строк в wl_adnan_adult_18plus:", nrow(wl_adnan_adult_18plus), "\n")
cat("Количество строк в wl_bca:", nrow(wl_bca), "\n")
cat("Количество строк в объединенном датасете:", nrow(wl_combined), "\n")

# Проверим, сколько строк успешно объединились:
#   
#   r

matched_rows <- wl_combined %>%
  filter(!is.na(bone))  # Предполагаем, что 'bone' - это одна из колонок из wl_bca

cat("Количество строк с успешным объединением:", nrow(matched_rows), "\n")
cat("Процент успешно объединенных строк:", nrow(matched_rows) / nrow(wl_combined) * 100, "%\n")

# Посмотрим на структуру нового объединенного датасета:
#   
#   r

str(wl_combined)

# Сохраним новый объединенный датасет:
#   
#   r

saveRDS(wl_combined, "wl_combined_adult_18plus_bca.rds")
cat("Объединенный датасет сохранен в файл 'wl_combined_adult_18plus_bca.rds'\n")