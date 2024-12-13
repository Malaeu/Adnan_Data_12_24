library(tidyverse)

# Векторизованная функция для анализа базового заболевания
analyze_base_disease <- Vectorize(function(text) {
  if (is.na(text)) return(NA_character_)
  
  text_lower <- tolower(text)
  
  # Проверяем базовое заболевание
  if (grepl("virus|hbv|hcv|viral|hepatitis", text_lower)) {
    return("Viral")
  } else if (grepl("alkohol|ethyl|c2|äthyl", text_lower)) {
    return("Alcoholic")
  } else if (grepl("nash|steato|fett", text_lower)) {
    return("NASH")
  } else if (grepl("auto|aih", text_lower)) {
    return("Autoimmune")
  } else if (grepl("metabol|stoffwechsel|wilson", text_lower)) {
    return("Metabolic")
  } else if (grepl("toxic|medikament|drug", text_lower)) {
    return("Toxic")
  } else if (grepl("kryptogen|cryptogen", text_lower)) {
    return("Cryptogenic")
  } else {
    return("Unknown")
  }
})

# Векторизованная функция для чистки диагнозов
clean_diagnosis <- Vectorize(function(text) {
  if (is.na(text)) return(NA_character_)
  
  # Словарь диагнозов
  diagnoses <- c(
    # Рак печени
    "HCC|Hepatozellul.*Karzinom|[Cc]ancer|[Tt]umor|[Kk]arzinom|Hepatoblastom" = "Hepatocellular Carcinoma",
    
    # Острая печеночная недостаточность
    "akutes Leberversagen|ALF|acute.*hepatic|acute.*liver" = "Acute Liver Failure",
    
    # Циррозы
    "C2|[Aa]lkohol|äthyl" = "Alcoholic Cirrhosis",
    "NASH|[Ff]ettleber" = "NASH Cirrhosis",
    "kryptogen.*Cirrhose|kryptogen.*Zirrhose" = "Cryptogenic Cirrhosis",
    "HBV|Hepatitis B" = "Hepatitis B Cirrhosis",
    "HCV|Hepatitis C" = "Hepatitis C Cirrhosis",
    
    # Аутоиммунные
    "Autoimmunhepatitis|AIH" = "Autoimmune Hepatitis",
    "PSC" = "Primary Sclerosing Cholangitis",
    "PBC" = "Primary Biliary Cirrhosis",
    
    # Врожденные
    "Gallengangsatresie|GGA" = "Biliary Atresia",
    "Morbus Wilson|Wilson" = "Wilson's Disease",
    "Caroli" = "Caroli Disease",
    "PFIC|Byler" = "Progressive Familial Intrahepatic Cholestasis",
    "Alagille" = "Alagille Syndrome",
    
    # Другие
    "CF|[Cc]ystische Fibrose" = "Cystic Fibrosis",
    "ITBL" = "Ischemic Type Biliary Lesions",
    "Re-Listung|RE-Listung" = "Re-listing",
    "polycystische" = "Polycystic Liver Disease",
    "Budd Chiari" = "Budd-Chiari Syndrome",
    "SSC" = "Secondary Sclerosing Cholangitis",
    "toxische" = "Toxic Liver Disease"
  )
  
  # Ищем совпадения
  for (pattern in names(diagnoses)) {
    if (grepl(pattern, text, ignore.case = TRUE)) {
      return(diagnoses[pattern])
    }
  }
  
  # Для неопределенных циррозов
  if (grepl("[Cc]irrhose|[Cc]irrhosis|[Zz]irrhose", text, ignore.case = TRUE)) {
    return("Cirrhosis - Etiology Unknown")
  }
  
  return("Other")
})

# Исправленная функция обработки
process_diagnoses <- function(df) {
  # Сначала создаем временный датафрейм для подсчета редких случаев
  temp_df <- df %>%
    mutate(
      diagnosis_to_clean = coalesce(diagnosis, primary_diagnosis),
      diagnosis_clean = clean_diagnosis(diagnosis_to_clean),
      base_disease = analyze_base_disease(diagnosis_to_clean),
      diagnosis_final = case_when(
        diagnosis_clean == "Cirrhosis - Etiology Unknown" ~ 
          paste(base_disease, "Cirrhosis"),
        TRUE ~ diagnosis_clean
      )
    )
  
  # Определяем редкие диагнозы
  rare_diagnoses <- temp_df %>%
    count(diagnosis_final) %>%
    filter(n < 10) %>%
    pull(diagnosis_final)
  
  # Теперь финальная обработка
  df_processed <- temp_df %>%
    mutate(
      diagnosis_grouped = case_when(
        is.na(diagnosis_final) ~ "Unknown",
        diagnosis_final %in% rare_diagnoses ~ "Other Rare Diseases",
        TRUE ~ diagnosis_final
      )
    )
  
  return(df_processed)
}

# Применяем обработку
wl_processed <- process_diagnoses(wl)

# Смотрим результаты
diagnosis_stats <- wl_processed %>%
  count(diagnosis_grouped, sort = TRUE) %>%
  mutate(
    pct = n/sum(n) * 100,
    pct = round(pct, 1),
    cumulative_pct = cumsum(pct)
  )

print("Топ-15 диагнозов:")
print(head(diagnosis_stats, 15))

# Анализируем базовые заболевания для циррозов
cirrhosis_analysis <- wl_processed %>%
  filter(str_detect(diagnosis_clean, "Cirrhosis")) %>%
  count(base_disease, sort = TRUE) %>%
  mutate(pct = round(n/sum(n)*100, 1))

print("\nРаспределение этиологии циррозов:")
print(cirrhosis_analysis)

# Создаем сводную таблицу
summary_table <- wl_processed %>%
  group_by(diagnosis_grouped) %>%
  summarise(
    n = n(),
    pct = round(n/nrow(.)*100, 1),
    n_cirrhosis = sum(str_detect(diagnosis_clean, "Cirrhosis"), na.rm = TRUE),
    pct_cirrhosis = round(mean(str_detect(diagnosis_clean, "Cirrhosis"), na.rm = TRUE)*100, 1)
  ) %>%
  arrange(desc(n))

print("\nПодробная статистика:")
print(summary_table)

# Визуализация
library(ggplot2)

# График топ-15 диагнозов
p1 <- ggplot(head(diagnosis_stats, 15), 
             aes(x = reorder(diagnosis_grouped, n), y = pct)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = paste0(pct, "%")), hjust = -0.1) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Топ-15 диагнозов",
       x = "Диагноз",
       y = "Процент пациентов (%)")

# График распределения этиологии циррозов
p2 <- ggplot(cirrhosis_analysis, 
             aes(x = reorder(base_disease, n), y = pct)) +
  geom_bar(stat = "identity", fill = "darkred") +
  geom_text(aes(label = paste0(pct, "%")), hjust = -0.1) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Распределение этиологии циррозов",
       x = "Базовое заболевание",
       y = "Процент пациентов (%)")

# Показываем графики
print(p1)
print(p2)

# Сохраняем обработанный датафрейм
saveRDS(wl_processed, "wl_df.rds")
# Сохраняем исходный и обработанный датафреймы с датой
saveRDS(wl, paste0("wl_original_", Sys.Date(), ".rds"))
saveRDS(wl_processed, paste0("wl_processed_", Sys.Date(), ".rds"))
# Создаем атрибуты с информацией об обработке
attr(wl_processed, "processed_date") <- Sys.time()
attr(wl_processed, "version") <- "1.0"
attr(wl_processed, "notes") <- "Cleaned and standardized diagnoses"
#Теперь сохраняем с метаданными
saveRDS(wl_processed, "wl_df.rds")
# Для проверки можем сразу загрузить и посмотреть
wl_df <- readRDS("wl_df.rds")

# Проверяем что сохранилось
str(wl_df)

# Быстрая сводка по основным диагнозам
print("Проверка сохраненных данных:")
wl_df %>%
  count(diagnosis_grouped, sort = TRUE) %>%
  mutate(
    pct = round(n/sum(n)*100, 1),
    cumulative_pct = cumsum(pct)
  ) %>%
  head(10) %>%
  print()


# Сводка по циррозам
wl_df %>%
  # Фильтруем только циррозы
  filter(str_detect(diagnosis_grouped, "Cirrhosis")) %>%
  # Считаем частоты
  count(diagnosis_grouped) %>%
  # Добавляем проценты
  mutate(
    pct = round(n/sum(n)*100, 1),
    cumulative_pct = cumsum(pct)
  ) %>%
  # Сортируем по частоте
  arrange(desc(n)) %>%
  # Добавляем красивые названия колонок
  rename(
    "Этиология цирроза" = diagnosis_grouped,
    "Количество" = n,
    "Процент" = pct,
    "Накопленный %" = cumulative_pct
  ) %>%
  print()

# Визуализация
wl_df %>%
  filter(str_detect(diagnosis_grouped, "Cirrhosis")) %>%
  ggplot(aes(x = reorder(diagnosis_grouped, table(diagnosis_grouped)[diagnosis_grouped]), 
             fill = diagnosis_grouped)) +
  geom_bar() +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Распределение этиологии циррозов",
    x = "Тип цирроза",
    y = "Количество пациентов"
  ) +
  theme(legend.position = "none") +
  geom_text(stat = 'count', 
            aes(label = paste0(round(..count../sum(..count..)*100, 1), "%")),
            hjust = -0.1)
# Анализ сопутствующих осложнений по типам цирроза
cirrhosis_complications <- wl_df %>%
  filter(str_detect(diagnosis_grouped, "Cirrhosis")) %>%
  group_by(diagnosis_grouped) %>%
  summarise(
    total_cases = n(),
    ascites = sum(str_detect(diagnosis, "scites"), na.rm = TRUE),
    varices = sum(str_detect(diagnosis, "arizen|arices"), na.rm = TRUE),
    encephalopathy = sum(str_detect(diagnosis, "ncephalo"), na.rm = TRUE),
    splenomegaly = sum(str_detect(diagnosis, "pleno"), na.rm = TRUE)
  ) %>%
  mutate(across(ascites:splenomegaly, 
                ~round(.x/total_cases*100, 1),
                .names = "{.col}_pct")) %>%
  arrange(desc(total_cases))

print("\nОсложнения по типам цирроза (%):")
print(cirrhosis_complications)


library(rstatix)
library(tidyverse)

# Функция для создания длинного формата данных по конкретному осложнению
create_long_data <- function(df, complication) {
  df %>%
    filter(total_cases >= 20) %>%  # берем группы с достаточным размером
    select(diagnosis_grouped, total_cases, {{complication}}) %>%
    mutate(
      no_complication = total_cases - {{complication}},
      complication_name = deparse(substitute(complication))
    ) %>%
    select(-total_cases) %>%
    gather(status, count, -diagnosis_grouped, -complication_name)
}

# Создаем данные для каждого осложнения
ascites_data <- create_long_data(cirrhosis_complications, ascites)
varices_data <- create_long_data(cirrhosis_complications, varices)
encephalopathy_data <- create_long_data(cirrhosis_complications, encephalopathy)
splenomegaly_data <- create_long_data(cirrhosis_complications, splenomegaly)

# Функция для проведения теста хи-квадрат и попарных сравнений
analyze_complication <- function(data, complication_name) {
  # Создаем таблицу сопряженности
  cont_table <- xtabs(count ~ diagnosis_grouped + status, data = data)
  
  # Общий тест хи-квадрат
  chi_test <- chisq.test(cont_table)
  
  # Попарные сравнения с поправкой на множественные сравнения
  pairwise <- pairwise.prop.test(
    cont_table[,1],
    rowSums(cont_table),
    p.adjust.method = "bonferroni"
  )
  
  # Форматируем результаты
  list(
    complication = complication_name,
    chi_square = chi_test$statistic,
    p_value = chi_test$p.value,
    pairwise = pairwise
  )
}

# Проводим анализ для каждого осложнения
results_ascites <- analyze_complication(ascites_data, "Ascites")
results_varices <- analyze_complication(varices_data, "Varices")
results_enceph <- analyze_complication(encephalopathy_data, "Encephalopathy")
results_spleno <- analyze_complication(splenomegaly_data, "Splenomegaly")

# Выводим результаты
print_results <- function(results) {
  cat("\n=== ", results$complication, " ===\n")
  cat("Chi-square статистика:", round(results$chi_square, 2), "\n")
  cat("P-value:", format.pval(results$p_value, digits = 3), "\n")
  
  # Выводим значимые попарные различия
  p_values <- results$pairwise$p.value
  sig_pairs <- which(p_values < 0.05, arr.ind = TRUE)
  
  if(nrow(sig_pairs) > 0) {
    cat("\nЗначимые различия между группами (p < 0.05):\n")
    rownames <- rownames(p_values)
    colnames <- colnames(p_values)
    for(i in 1:nrow(sig_pairs)) {
      cat(sprintf("%s vs %s: p = %.3f\n",
                  colnames[sig_pairs[i,2]],
                  rownames[sig_pairs[i,1]],
                  p_values[sig_pairs[i,1], sig_pairs[i,2]]))
    }
  }
}

# Выводим результаты для каждого осложнения
print_results(results_ascites)
print_results(results_varices)
print_results(results_enceph)
print_results(results_spleno)

# Визуализация различий
ggplot(cirrhosis_complications %>% 
         filter(total_cases >= 20) %>%
         gather(key = "complication", value = "percentage",
                ascites_pct:splenomegaly_pct)) +
  geom_bar(aes(x = reorder(diagnosis_grouped, -percentage), 
               y = percentage, 
               fill = complication), 
           stat = "identity", 
           position = "dodge") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Частота осложнений по типам цирроза",
       x = "Тип цирроза",
       y = "Процент пациентов (%)",
       fill = "Осложнение") +
  scale_fill_brewer(palette = "Set2")

# Обновляем датафрейм без Unknown Cirrhosis
cirrhosis_clean <- cirrhosis_complications %>%
  filter(diagnosis_grouped != "Unknown Cirrhosis" & total_cases >= 20)

# Показываем обновленную таблицу
print("Обновленная статистика по циррозам:")
print(cirrhosis_clean)


# Анализ кластеров осложнений
complications_patterns <- wl_df %>%
  filter(str_detect(diagnosis_grouped, "Cirrhosis"),
         diagnosis_grouped != "Unknown Cirrhosis") %>%
  group_by(diagnosis_grouped) %>%
  summarise(
    n = n(),
    # Считаем частые комбинации осложнений
    ascites_varices = mean(str_detect(diagnosis, "scites") & 
                             str_detect(diagnosis, "arizen|arices"), na.rm = TRUE),
    ascites_enceph = mean(str_detect(diagnosis, "scites") & 
                            str_detect(diagnosis, "ncephalo"), na.rm = TRUE),
    varices_spleno = mean(str_detect(diagnosis, "arizen|arices") & 
                            str_detect(diagnosis, "pleno"), na.rm = TRUE)
  ) %>%
  mutate(across(starts_with(c("ascites", "varices")), ~round(.*100, 1)))

severity_analysis <- wl_df %>%
  filter(str_detect(diagnosis_grouped, "Cirrhosis"),
         diagnosis_grouped != "Unknown Cirrhosis") %>%
  group_by(diagnosis_grouped) %>%
  summarise(
    mean_meld = mean(lab_meld, na.rm = TRUE),
    median_meld = median(lab_meld, na.rm = TRUE),
    high_meld_pct = mean(lab_meld > 20, na.rm = TRUE) * 100
  )

demographics <- wl_df %>%
  filter(str_detect(diagnosis_grouped, "Cirrhosis"),
         diagnosis_grouped != "Unknown Cirrhosis") %>%
  group_by(diagnosis_grouped) %>%
  summarise(
    n = n(),
    age_mean = mean(as.numeric(difftime(date_of_wl, date_of_birth, units = "days"))/365.25, na.rm = TRUE),
    male_pct = mean(sex == "M", na.rm = TRUE) * 100,
    bmi_mean = mean(bmi, na.rm = TRUE)
  )

library(survival)
library(survminer)

# Анализ выживаемости
survival_analysis <- wl_df %>%
  filter(str_detect(diagnosis_grouped, "Cirrhosis"),
         diagnosis_grouped != "Unknown Cirrhosis") %>%
  rename(
    surv_time = time,  # переименовываем существующую колонку
    surv_status = status
  )

# Фиттим модель Каплана-Мейера
km_fit <- survfit(Surv(surv_time, surv_status) ~ diagnosis_grouped, 
                  data = survival_analysis)

# Красивый график
ggsurvplot(km_fit,
           data = survival_analysis,
           pval = TRUE,           # показать p-value
           conf.int = TRUE,       # показать доверительные интервалы
           risk.table = TRUE,     # добавить таблицу риска
           xlab = "Time (days)",
           ylab = "Survival probability",
           title = "Kaplan-Meier Survival Curves by Cirrhosis Type",
           palette = "Set2",      # цветовая палитра
           ggtheme = theme_minimal(),
           risk.table.height = 0.25)

# Медианная выживаемость по группам
print("Медианная выживаемость по группам:")
print(summary(km_fit)$table)

# Log-rank тест
log_rank <- survdiff(Surv(surv_time, surv_status) ~ diagnosis_grouped, 
                     data = survival_analysis)
print("\nLog-rank test:")
print(log_rank)

# Попарные сравнения с поправкой на множественные сравнения
pairwise_survdiff <- pairwise_survdiff(Surv(surv_time, surv_status) ~ diagnosis_grouped,
                                       data = survival_analysis,
                                       p.adjust.method = "bonferroni")
print("\nПопарные сравнения:")
print(pairwise_survdiff)

# Дополнительная статистика
survival_stats <- survival_analysis %>%
  group_by(diagnosis_grouped) %>%
  summarise(
    n = n(),
    events = sum(surv_status),
    median_time = median(surv_time),
    mean_time = mean(surv_time),
    event_rate = round(events/n * 100, 1)
  ) %>%
  arrange(desc(n))

print("\nСтатистика по группам:")
print(survival_stats)


trends_analysis <- wl_df %>%
  filter(str_detect(diagnosis_grouped, "Cirrhosis"),
         diagnosis_grouped != "Unknown Cirrhosis") %>%
  mutate(year = format(date_of_wl, "%Y")) %>%
  group_by(year, diagnosis_grouped) %>%
  summarise(n = n()) %>%
  group_by(year) %>%
  mutate(pct = n/sum(n) * 100)
