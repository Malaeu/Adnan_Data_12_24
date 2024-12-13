library(tidyverse)

# 1. Проверяем etnr в обоих датасетах
etnr_check <- list(
  wl_df = wl_df_updated %>% 
    summarise(
      n_total = n(),
      n_etnr = sum(!is.na(etnr)),
      n_unique_etnr = n_distinct(etnr)
    ),
  bca_df = wl_combined_adult_18plus_bca %>% 
    summarise(
      n_total = n(),
      n_etnr = sum(!is.na(etnr)),
      n_unique_etnr = n_distinct(etnr)
    )
)

print("Проверка etnr в датасетах:")
print(etnr_check)

# 2. Проверяем пересечение etnr
etnr_overlap <- inner_join(
  wl_df_updated %>% select(etnr) %>% distinct(),
  wl_combined_adult_18plus_bca %>% select(etnr) %>% distinct(),
  by = "etnr"
)

print("\nКоличество пересекающихся etnr:")
print(nrow(etnr_overlap))

# 3. Объединяем датасеты
merged_df <- wl_df_updated %>%
  left_join(
    wl_combined_adult_18plus_bca %>%
      select(etnr, bone, muscle, sat, vat, imat, eat, pat, tat,
             date_of_ct, acsn_nr),  # выбираем только нужные колонки из BCA
    by = "etnr"
  )

# 4. Проверяем результат объединения
merge_check <- merged_df %>%
  summarise(
    n_total = n(),
    n_with_bca = sum(!is.na(bone)),
    pct_with_bca = round(mean(!is.na(bone)) * 100, 1),
    n_with_ct = sum(!is.na(date_of_ct))
  )

print("\nРезультаты объединения:")
print(merge_check)

# 5. Проверяем распределение BCA данных по группам диагнозов
bca_by_diagnosis <- merged_df %>%
  group_by(diagnosis_grouped) %>%
  summarise(
    n_total = n(),
    n_with_bca = sum(!is.na(bone)),
    pct_with_bca = round(mean(!is.na(bone)) * 100, 1)
  ) %>%
  arrange(desc(n_total))

print("\nРаспределение BCA данных по диагнозам:")
print(bca_by_diagnosis)

# 6. Сохраняем объединенный датасет
saveRDS(merged_df, "wl_df_with_bca.rds")

# 7. Базовая статистика по BCA переменным
bca_stats <- merged_df %>%
  summarise(
    across(c(bone, muscle, sat, vat, imat, eat, pat, tat),
           list(
             n = ~sum(!is.na(.)),
             mean = ~mean(., na.rm = TRUE),
             sd = ~sd(., na.rm = TRUE),
             median = ~median(., na.rm = TRUE)
           ),
           .names = "{.col}_{.fn}")
  )

print("\nСтатистика по BCA переменным:")
print(bca_stats)
