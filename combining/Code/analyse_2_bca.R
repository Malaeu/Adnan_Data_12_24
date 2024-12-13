library(tidyverse)
library(gtsummary)
library(gt)

# Загружаем и фильтруем данные только с BCA
df_bca <- wl_df_with_bca %>%
  filter(!is.na(bone))  # фильтруем только случаи с BCA данными

# Подготавливаем данные для анализа
df_analysis <- df_bca %>%
  filter(str_detect(diagnosis_grouped, "Cirrhosis")) %>%
  mutate(
    # Создаем факторы
    mortality = factor(surv_status, 
                       levels = c(0, 1), 
                       labels = c("Alive", "Deceased")),
    sex = factor(sex, levels = c("M", "W"), 
                 labels = c("Male", "Female")),
    
    # Категоризируем непрерывные переменные
    meld_cat = cut(lab_meld,
                   breaks = c(0, 15, 25, Inf),
                   labels = c("Low", "Medium", "High")),
    
    age_cat = cut(age,
                  breaks = c(0, 40, 55, 70, Inf),
                  labels = c("<40", "40-55", "55-70", ">70"))
  )

# Создаем таблицу характеристик
tbl_univariate <- df_analysis %>%
  select(
    # Исход
    mortality,
    # Демография
    age_cat, sex,
    # Клинические параметры
    meld_cat, bmi,
    # Осложнения
    has_ascites, has_varices, has_encephalopathy, has_splenomegaly,
    # BCA параметры (оригинальные значения)
    bone, muscle, sat, vat, imat, eat, pat, tat,
    # Диагноз
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
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 3)) %>%
  add_overall() %>%
  add_n() %>%
  modify_header(label = "**Characteristic**") %>%
  modify_spanning_header(all_stat_cols() ~ "**Mortality Status**") %>%
  modify_caption("**Table 1. Patient characteristics by mortality status (BCA cohort)**")

# Выводим таблицу
print(tbl_univariate)

# Базовая статистика
print("Статистика по когорте BCA:")
df_analysis %>%
  summarise(
    n_total = n(),
    n_deaths = sum(mortality == "Deceased"),
    death_rate = mean(mortality == "Deceased") * 100,
    mean_age = mean(age, na.rm = TRUE),
    mean_meld = mean(lab_meld, na.rm = TRUE),
    mean_bmi = mean(bmi, na.rm = TRUE)
  ) %>%
  print()

# Статистика по BCA параметрам
print("\nСтатистика по BCA параметрам:")
df_analysis %>%
  summarise(
    across(c(bone, muscle, sat, vat, imat, eat, pat, tat),
           list(
             mean = ~mean(., na.rm = TRUE),
             sd = ~sd(., na.rm = TRUE),
             min = ~min(., na.rm = TRUE),
             max = ~max(., na.rm = TRUE)
           ),
           .names = "{.col}_{.fn}")
  ) %>%
  print()

# Корреляции между BCA параметрами
bca_cors <- df_analysis %>%
  select(bone, muscle, sat, vat, imat, eat, pat, tat) %>%
  cor(use = "pairwise.complete.obs")

print("\nКорреляции между BCA параметрами:")
print(round(bca_cors, 2))

# Сохраняем результаты
tbl_univariate %>%
  as_gt() %>%
  gtsave("bca_characteristics.html")
