library(tidyverse)
library(gtsummary)
library(funModeling)


df_status(df_bca_all)


# Создаем таблицу для всех 294 пациентов
tbl_all <- df_bca_all %>%
  select(
    mortality,
    age_cat, sex, meld_cat, bmi,
    has_ascites, has_varices, has_encephalopathy, has_splenomegaly,
    bone, muscle, sat, vat, imat, eat, pat, tat,
    diagnosis_grouped
  ) %>%
  tbl_summary(
    by = mortality,
    missing = "no",
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = all_continuous() ~ 1
  ) %>%
  add_overall() %>%
  add_n() %>%
  add_p(
    test = list(
      all_continuous() ~ "wilcox.test",
      all_categorical() ~ "chisq.test"
    )
  )

# Выводим таблицу
print(tbl_all)

# Сохраняем таблицу в HTML
tbl_all %>%
  as_gt() %>%
  gtsave("bca_analysis_table.html")

# Дополнительный анализ BCA параметров
bca_analysis <- df_bca_all %>%
  group_by(mortality) %>%
  summarise(
    across(c(bone, muscle, sat, vat, imat, eat, pat, tat),
           list(
             mean = ~mean(., na.rm = TRUE),
             sd = ~sd(., na.rm = TRUE),
             median = ~median(., na.rm = TRUE),
             q25 = ~quantile(., 0.25, na.rm = TRUE),
             q75 = ~quantile(., 0.75, na.rm = TRUE)
           ),
           .names = "{.col}_{.fn}")
  )

print("\nДетальный анализ BCA параметров:")
print(bca_analysis)

# Нормализованные значения BCA
df_bca_normalized <- df_bca_all %>%
  mutate(
    across(c(bone, muscle, sat, vat, imat, eat, pat, tat),
           list(
             norm = ~scale(.)
           ),
           .names = "{.col}_norm")
  )

# Логистическая регрессия с нормализованными значениями
log_model <- glm(mortality ~ bone_norm + muscle_norm + sat_norm + 
                   vat_norm + imat_norm + eat_norm + pat_norm + tat_norm +
                   age_cat + sex + meld_cat,
                 data = df_bca_normalized,
                 family = binomial())

# Выводим результаты регрессии
tbl_regression <- tbl_regression(log_model, 
                                 exponentiate = TRUE) %>%
  bold_p() %>%
  bold_labels()

print("\nРезультаты логистической регрессии:")
print(tbl_regression)

# Создаем индексы соотношений
bca_ratios <- df_bca_all %>%
  mutate(
    muscle_to_fat = muscle / (sat + vat),
    bone_to_fat = bone / (sat + vat),
    visceral_to_subcut = vat / sat,
    muscle_to_visceral = muscle / vat
  ) %>%
  group_by(mortality) %>%
  summarise(
    across(c(muscle_to_fat, bone_to_fat, 
             visceral_to_subcut, muscle_to_visceral),
           list(
             mean = ~mean(., na.rm = TRUE),
             sd = ~sd(., na.rm = TRUE)
           ))
  )

print("\nАнализ соотношений BCA параметров:")
print(bca_ratios)
