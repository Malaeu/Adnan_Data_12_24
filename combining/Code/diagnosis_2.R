library(tidyverse)

# 1. Функция для чистки диагнозов
clean_diagnosis <- Vectorize(function(text) {
  if (is.na(text)) return(NA_character_)
  
  # Словарь диагнозов (немецкий -> английский стандарт)
  diagnoses <- c(
    # Циррозы
    "C2|Alkohol|äthyl" = "Alcoholic Cirrhosis",
    "NASH" = "NASH Cirrhosis",
    "kryptogen.*Cirrhose|kryptogen.*Zirrhose" = "Cryptogenic Cirrhosis",
    
    # Вирусные гепатиты
    "HBV|Hepatitis B" = "Hepatitis B Cirrhosis",
    "HCV|Hepatitis C" = "Hepatitis C Cirrhosis",
    
    # Аутоиммунные заболевания
    "Autoimmunhepatitis|AIH" = "Autoimmune Hepatitis",
    "PSC" = "Primary Sclerosing Cholangitis",
    "PBC" = "Primary Biliary Cirrhosis",
    
    # Опухоли
    "HCC|Hepatozellul.*Karzinom" = "Hepatocellular Carcinoma",
    "Hepatoblastom" = "Hepatoblastoma",
    
    # Врожденные заболевания
    "Gallengangsatresie|GGA" = "Biliary Atresia",
    "Morbus Wilson|Wilson" = "Wilson's Disease",
    "Caroli" = "Caroli Disease",
    "PFIC|Byler" = "Progressive Familial Intrahepatic Cholestasis",
    "Alagille" = "Alagille Syndrome",
    
    # Другие специфические состояния
    "CF|[Cc]ystische Fibrose" = "Cystic Fibrosis",
    "akutes Leberversagen|ALF" = "Acute Liver Failure",
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
  
  # Если не нашли, берём первую часть до разделителя
  first_part <- str_extract(text, "^[^\\\\,\\.\\(]+")
  if (!is.na(first_part)) {
    return(trimws(first_part))
  }
  
  return("Other")
})

# 2. Функция для определения осложнений
get_complications <- function(text) {
  if (is.na(text)) return(rep(FALSE, 6))
  
  text_lower <- tolower(text)
  
  c(
    cirrhosis = str_detect(text_lower, "irrhose|irrhosis|zirrhose"),
    ascites = str_detect(text_lower, "scites|aszites"),
    varices = str_detect(text_lower, "arizen|arices|varizen"),
    splenomegaly = str_detect(text_lower, "pleno|spleno"),
    pruritus = str_detect(text_lower, "pruritus|juckreiz"),
    encephalopathy = str_detect(text_lower, "encephalo|enzephalo")
  )
}

# 3. Основная функция обработки
process_diagnoses <- function(df) {
  df_processed <- df %>%
    # Чистим диагнозы
    mutate(
      # Используем diagnosis или primary_diagnosis если diagnosis пустой
      diagnosis_to_clean = coalesce(diagnosis, primary_diagnosis),
      diagnosis_clean = clean_diagnosis(diagnosis_to_clean),
      
      # Добавляем осложнения
      complications = map(diagnosis_to_clean, get_complications),
      
      # Разворачиваем осложнения в отдельные колонки
      has_cirrhosis = map_lgl(complications, ~.x[1]),
      has_ascites = map_lgl(complications, ~.x[2]),
      has_varices = map_lgl(complications, ~.x[3]),
      has_splenomegaly = map_lgl(complications, ~.x[4]),
      has_pruritus = map_lgl(complications, ~.x[5]),
      has_encephalopathy = map_lgl(complications, ~.x[6])
    ) %>%
    select(-complications) %>% # убираем временную колонку
    
    # Группируем редкие диагнозы
    mutate(
      diagnosis_grouped = case_when(
        is.na(diagnosis_clean) ~ "Unknown",
        diagnosis_clean %in% (count(., diagnosis_clean) %>% 
                                filter(n < 10) %>% 
                                pull(diagnosis_clean)) ~ "Other Rare Diseases",
        TRUE ~ diagnosis_clean
      ),
      diagnosis_grouped = factor(diagnosis_grouped)
    )
  
  return(df_processed)
}

# 4. Функция для создания отчета
create_diagnosis_report <- function(df) {
  # Общая статистика по диагнозам
  diagnosis_stats <- df %>%
    count(diagnosis_grouped, sort = TRUE) %>%
    mutate(
      pct = n/sum(n) * 100,
      pct = round(pct, 1),
      cumulative_pct = cumsum(pct)
    )
  
  # Статистика по осложнениям
  complications_stats <- df %>%
    summarise(
      across(starts_with("has_"), 
             list(
               n = ~sum(., na.rm = TRUE),
               pct = ~mean(., na.rm = TRUE) * 100
             ),
             .names = "{.col}_{.fn}"
      )
    ) %>%
    pivot_longer(everything(),
                 names_to = c("complication", "metric"),
                 names_pattern = "has_(.*)_(.*)",
                 values_to = "value") %>%
    pivot_wider(names_from = metric,
                values_from = value) %>%
    mutate(pct = round(pct, 1))
  
  # Создаем графики
  p1 <- ggplot(head(diagnosis_stats, 15), 
               aes(x = reorder(diagnosis_grouped, n), y = pct)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    geom_text(aes(label = paste0(pct, "%")), hjust = -0.1) +
    coord_flip() +
    theme_minimal() +
    labs(title = "Top 15 Diagnoses",
         x = "Diagnosis",
         y = "Percentage of Patients (%)")
  
  p2 <- ggplot(complications_stats, 
               aes(x = reorder(complication, pct), y = pct)) +
    geom_bar(stat = "identity", fill = "darkred") +
    geom_text(aes(label = paste0(pct, "%")), hjust = -0.1) +
    coord_flip() +
    theme_minimal() +
    labs(title = "Complications",
         x = "Complication",
         y = "Percentage of Patients (%)")
  
  # Возвращаем список с результатами
  return(list(
    processed_data = df,
    diagnosis_stats = diagnosis_stats,
    complications_stats = complications_stats,
    plots = list(
      diagnosis_plot = p1,
      complications_plot = p2
    )
  ))
}

# 5. Применяем всё к данным
wl_processed <- process_diagnoses(wl)
results <- create_diagnosis_report(wl_processed)

# 6. Выводим результаты
print("Топ-15 диагнозов:")
print(head(results$diagnosis_stats, 15))

print("\nСтатистика осложнений:")
print(results$complications_stats)

# 7. Показываем графики
print(results$plots$diagnosis_plot)
print(results$plots$complications_plot)
