# Создаем финальный датасет с нужными переменными
df_analysis <- merged_df %>%
  filter(!is.na(bone)) %>%  # только пациенты с BCA данными
  select(
    # ID
    etnr,
    
    # Демография
    age, sex, bmi,
    
    # BCA параметры
    bone, muscle, sat, vat, imat, eat, pat, tat,
    date_of_ct,
    
    # Клинические параметры основные
    lab_meld, lab_meld_listing,
    child_pugh_score,
    
    # Лабораторные показатели
    inr_listing,
    bili_listing,
    crea_listing,
    sodium_listing,
    platelets_listing,
    
    # Осложнения
    has_ascites,
    has_varices,
    has_encephalopathy,
    has_splenomegaly,
    hcc,
    portal_vein_thrombosis.y,
    
    # Интенсивная терапия
    dialysis_cat,
    days_of_dialysis,
    icu_cat,
    days_of_icu,
    ventilation_cat,
    days_of_ventilation,
    catecholamine_cat,
    
    # Временные параметры и исходы
    date_of_wl,
    date_of_ltx,
    todesdatum,
    last_contact,
    surv_time,
    surv_status,
    survival_days_until_ltx,
    survival_days_from_wl_until_death,
    
    # Дополнительные параметры
    exceptional_meld,
    cause_of_death
  )

# Проверяем данные
print("Базовая статистика:")
df_analysis %>%
  summarise(
    n_total = n(),
    deaths = sum(surv_status == 1, na.rm = TRUE),
    transplanted = sum(!is.na(date_of_ltx)),
    mean_age = mean(age, na.rm = TRUE),
    mean_meld = mean(lab_meld, na.rm = TRUE),
    mean_meld_listing = mean(lab_meld_listing, na.rm = TRUE),
    hcc_cases = sum(hcc == 1, na.rm = TRUE)
  ) %>%
  print()

# Создаем таблицу характеристик
tbl_characteristics <- df_analysis %>%
  select(
    # Исход
    surv_status,
    
    # Основные характеристики
    age, sex, bmi,
    lab_meld, lab_meld_listing, child_pugh_score,
    
    # BCA параметры
    bone, muscle, sat, vat, imat, eat, pat, tat,
    
    # Осложнения
    has_ascites, has_varices, has_encephalopathy, has_splenomegaly,
    hcc, portal_vein_thrombosis.y,
    
    # Интенсивная терапия
    dialysis_cat, icu_cat, ventilation_cat, catecholamine_cat
  ) %>%
  tbl_summary(
    by = surv_status,
    missing = "no",
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = all_continuous() ~ 1
  ) %>%
  add_overall() %>%
  add_n() %>%
  add_p()

print(tbl_characteristics)

# Сохраняем датасет
saveRDS(df_analysis, "bca_analysis_dataset.rds")

# Корреляции между BCA и клиническими параметрами
bca_correlations <- df_analysis %>%
  select(bone, muscle, sat, vat, imat, eat, pat, tat,
         lab_meld, lab_meld_listing, child_pugh_score) %>%
  cor(use = "pairwise.complete.obs")

print("\nКорреляции между BCA и клиническими параметрами:")
print(round(bca_correlations, 2))

