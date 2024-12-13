library(tidyverse)

# Функция для извлечения основного диагноза
clean_diagnosis <- Vectorize(function(text) {
  if (is.na(text)) return(NA_character_)
  
  # Основные категории диагнозов и их стандартизированные имена
  diagnoses <- c(
    "HCC" = "Hepatocellular Carcinoma",
    "PSC" = "Primary Sclerosing Cholangitis",
    "PBC" = "Primary Biliary Cirrhosis",
    "NASH" = "Non-alcoholic Steatohepatitis",
    "HBV" = "Hepatitis B Cirrhosis",
    "HCV" = "Hepatitis C Cirrhosis",
    "Autoimmunhepatitis|AIH" = "Autoimmune Hepatitis",
    "C2" = "Alcoholic Cirrhosis",
    "Gallengangsatresie|GGA" = "Biliary Atresia",
    "Morbus Wilson" = "Wilson's Disease",
    "Caroli" = "Caroli Disease",
    "PFIC|Byler" = "Progressive Familial Intrahepatic Cholestasis",
    "Alagille" = "Alagille Syndrome",
    "CF|cystische Fibrose" = "Cystic Fibrosis",
    "akutes Leberversagen|ALF" = "Acute Liver Failure",
    "ITBL" = "Ischemic Type Biliary Lesions",
    "Re-Listung|RE-Listung" = "Re-listing",
    "polycystische" = "Polycystic Liver Disease",
    "kryptogen" = "Cryptogenic Cirrhosis",
    "toxische" = "Toxic Liver Disease",
    "Budd Chiari" = "Budd-Chiari Syndrome"
  )
  
  # Проверяем каждый паттерн
  for (pattern in names(diagnoses)) {
    if (grepl(pattern, text, ignore.case = TRUE)) {
      return(diagnoses[pattern])
    }
  }
  
  # Если не нашли совпадений, берем первую часть до разделителя
  first_part <- str_extract(text, "^[^\\\\,\\.\\(]+")
  if (!is.na(first_part)) {
    return(trimws(first_part))
  }
  
  return("Other")
})

# Применяем функцию к датасету
wl_clean <- wl %>%
  mutate(
    diagnosis_clean = clean_diagnosis(diagnosis),
    diagnosis_clean = factor(diagnosis_clean)
  )

# Создаем дополнительные признаки
wl_clean <- wl_clean %>%
  mutate(
    has_cirrhosis = str_detect(tolower(diagnosis), "irrhose|irrhosis"),
    has_ascites = str_detect(tolower(diagnosis), "scites"),
    has_varices = str_detect(tolower(diagnosis), "arizen|arices"),
    has_splenomegaly = str_detect(tolower(diagnosis), "pleno|spleno"),
    has_pruritus = str_detect(tolower(diagnosis), "pruritus"),
    has_encephalopathy = str_detect(tolower(diagnosis), "encephalo|enzephalo")
  )

# Смотрим результаты
diagnosis_summary <- wl_clean %>%
  count(diagnosis_clean, sort = TRUE) %>%
  mutate(
    pct = n/sum(n) * 100,
    pct = round(pct, 1)
  )

# Выводим статистику
print("Распределение диагнозов:")
print(diagnosis_summary)

# Статистика по дополнительным признакам
complications_summary <- wl_clean %>%
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

print("\nОсложнения:")
print(complications_summary)

# График распределения диагнозов
ggplot(diagnosis_summary, 
       aes(x = reorder(diagnosis_clean, n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Распределение диагнозов",
       x = "Диагноз",
       y = "Количество пациентов")

# График осложнений
ggplot(complications_summary, 
       aes(x = reorder(complication, pct), y = pct)) +
  geom_bar(stat = "identity", fill = "darkred") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Частота осложнений",
       x = "Осложнение",
       y = "Процент пациентов")

