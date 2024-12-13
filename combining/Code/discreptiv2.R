library(tidyverse)

# Поэтапная проверка данных
df_bca_294 <- wl_df_with_bca %>%
  filter(!is.na(bone))  # берем всех с BCA данными

# 1. Сначала посмотрим общие цифры
print("Общая статистика:")
df_bca_294 %>%
  summarise(
    total_patients = n(),
    cirrhosis_patients = sum(str_detect(diagnosis_grouped, "Cirrhosis")),
    non_cirrhosis = sum(!str_detect(diagnosis_grouped, "Cirrhosis")),
    deaths = sum(surv_status == 1, na.rm = TRUE),
    alive = sum(surv_status == 0, na.rm = TRUE),
    missing_status = sum(is.na(surv_status))
  ) %>%
  print()

# 2. Распределение по диагнозам
print("\nРаспределение по диагнозам:")
df_bca_294 %>%
  count(diagnosis_grouped, sort = TRUE) %>%
  mutate(
    pct = round(n/sum(n)*100, 1),
    cum_pct = cumsum(pct)
  ) %>%
  print()

# 3. Статистика только для циррозов
print("\nСтатистика для пациентов с циррозом:")
df_bca_294 %>%
  filter(str_detect(diagnosis_grouped, "Cirrhosis")) %>%
  summarise(
    total_cirrhosis = n(),
    deaths = sum(surv_status == 1, na.rm = TRUE),
    alive = sum(surv_status == 0, na.rm = TRUE),
    missing_status = sum(is.na(surv_status))
  ) %>%
  print()

# 4. Детальная проверка по типам цирроза
print("\nДетальная статистика по типам цирроза:")
df_bca_294 %>%
  filter(str_detect(diagnosis_grouped, "Cirrhosis")) %>%
  group_by(diagnosis_grouped) %>%
  summarise(
    total = n(),
    deaths = sum(surv_status == 1, na.rm = TRUE),
    alive = sum(surv_status == 0, na.rm = TRUE),
    missing = sum(is.na(surv_status))
  ) %>%
  arrange(desc(total)) %>%
  print()

# 5. Создаем правильно отфильтрованный датасет
df_cirrhosis_bca <- df_bca_294 %>%
  filter(
    str_detect(diagnosis_grouped, "Cirrhosis"),  # только циррозы
    !is.na(surv_status)  # только с известным статусом
  ) %>%
  mutate(
    mortality = factor(surv_status, 
                       levels = c(0, 1), 
                       labels = c("Alive", "Deceased"))
  )

# 6. Проверяем финальные цифры
print("\nФинальная статистика после фильтрации:")
df_cirrhosis_bca %>%
  summarise(
    total = n(),
    deceased = sum(mortality == "Deceased"),
    alive = sum(mortality == "Alive")
  ) %>%
  print()

# Теперь создаем правильную таблицу характеристик
tbl_characteristics <- df_cirrhosis_bca %>%
  select(
    mortality,
    age, sex, lab_meld, bmi,
    has_ascites, has_varices, has_encephalopathy, has_splenomegaly,
    bone, muscle, sat, vat, imat, eat, pat, tat,
    diagnosis_grouped
  ) %>%
  tbl_summary(
    by = mortality,
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = all_continuous() ~ 1
  ) %>%
  add_p() %>%
  add_overall() %>%
  add_n()

print(tbl_characteristics)
