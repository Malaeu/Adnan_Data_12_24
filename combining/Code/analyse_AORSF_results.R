# Новый воркфлоу без Cox модели
workflow <- function(data) {
  # Готовим данные
  prepared_data <- make_recipe(data) %>%
    prep() %>%
    juice()
  
  # Сплитим датасет
  set.seed(42)
  split <- initial_split(prepared_data, prop = 0.7)
  train_data <- training(split)
  test_data <- testing(split)
  
  # Фитим только ORSF и RSF
  models <- list(
    orsf = fit_orsf(
      trn = train_data,
      vars = setdiff(names(train_data), c("time", "status")),
      tst = test_data,
      predict_horizon = 365
    ),
    
    rsf = ranger(
      Surv(time, status) ~ ., 
      data = train_data,
      num.trees = 500,
      mtry = floor(sqrt(ncol(train_data) - 2)), # -2 для time и status
      min.node.size = 5,
      splitrule = "extratrees",
      importance = "permutation"
    )
  )
  
  return(list(
    models = models,
    train = train_data,
    test = test_data
  ))
}

# Запускаем обновленный воркфлоу
results <- workflow(bca_analysis_dataset_clean)
results

# Вытаскиваем importance scores
importance_scores <- importance(results$models$rsf)
# Сортируем и берем топ-20
top_20 <- sort(importance_scores, decreasing = TRUE)[1:20]
# Красивый plot
library(ggplot2)
data.frame(
  variable = names(top_20),
  importance = top_20
) %>%
  ggplot(aes(x = reorder(variable, importance), y = importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Топ-20 важных переменных",
    x = "Переменные",
    y = "Важность"
  )


# Комментарий: начинаем с чистого листа
bca_analysis_dataset_clean <- bca_analysis_dataset %>%
  # 1. Сначала правильно конвертим has_ переменные
  mutate(
    # Конвертим logical в factor с правильными уровнями
    has_ascites = factor(has_ascites, levels = c(FALSE, TRUE), labels = c("No", "Yes")),
    has_varices = factor(has_varices, levels = c(FALSE, TRUE), labels = c("No", "Yes")),
    has_encephalopathy = factor(has_encephalopathy, levels = c(FALSE, TRUE), labels = c("No", "Yes")),
    has_splenomegaly = factor(has_splenomegaly, levels = c(FALSE, TRUE), labels = c("No", "Yes")),
    
    # 2. Конвертим числовые строки в numeric
    inr_listing = as.numeric(str_replace(inr_listing, ",", ".")),
    bili_listing = as.numeric(str_replace(bili_listing, ",", ".")),
    crea_listing = as.numeric(str_replace(crea_listing, ",", ".")),
    sodium_listing = as.numeric(str_replace(sodium_listing, ",", ".")),
    days_of_icu = as.numeric(str_replace(days_of_icu, ",", ".")),
    days_of_ventilation = as.numeric(str_replace(days_of_ventilation, ",", ".")),
    
    # 3. Остальные категориальные в factor
    across(c(portal_vein_thrombosis.y, dialysis_cat, icu_cat,
             ventilation_cat, catecholamine_cat), as.factor)
  )

# Проверяем has_ переменные
str(bca_analysis_dataset_clean[c("has_ascites", "has_varices", 
                                 "has_encephalopathy", "has_splenomegaly")])

# Если всё ок, импьютим NA
bca_analysis_dataset_clean <- bca_analysis_dataset_clean %>%
  # Для numeric - медиана
  mutate(across(where(is.numeric), 
                ~ifelse(is.na(.), median(., na.rm = TRUE), .))) %>%
  # Для factor - мода
  mutate(across(where(is.factor), 
                ~factor(ifelse(is.na(.), names(which.max(table(.))), as.character(.)))))

# Финальная проверка
df_status(bca_analysis_dataset_clean)

# Комментарий: убираем все даты, ID и лишние переменные
bca_analysis_dataset_clean <- bca_analysis_dataset_clean %>%
  select(
    -etnr,                    # номер
    -date_of_ct,             # дата
    -date_of_wl,             # дата
    -date_of_ltx,            # дата
    -todesdatum,             # дата
    -last_contact,           # дата
    -survival_days_until_ltx, # не нужно для анализа
    -survival_days_from_wl_until_death, # не нужно для анализа
    -cause_of_death,         # текстовое поле
    
  )

# Проверяем что осталось
df_status(bca_analysis_dataset_clean)

# Комментарий: фиксим time, заменяя нули и значения меньше 1 на 1 (минимальное допустимое значение)
bca_analysis_dataset_clean <- bca_analysis_dataset_clean %>%
  mutate(
    time = case_when(
      time <= 0 ~ 1,  # Заменяем все значения <= 0 на 1
      TRUE ~ time     # Остальные оставляем как есть
    )
  )

# Проверяем что с time теперь всё ок
summary(bca_analysis_dataset_clean$time)

# Теперь фитим модель
survival_fit <- orsf(
  data = bca_analysis_dataset_clean,
  formula = Surv(time, status) ~ .,
  n_tree = 500
)

# Смотрим важность переменных
var_importance <- orsf_vi_negate(survival_fit)
print("Топ-10 важных предикторов:")
print(sort(var_importance, decreasing = TRUE)[1:10])
# Комментарий: сохраняем очищенный датасет с расширением .rds
saveRDS(bca_analysis_dataset_clean, "~/Dokumente/Statistik/Adnan/Daten/DR_Adnan_Statistik/cl_adnan/Data_Sets/combining/bca_analysis_dataset_clean.rds")

# Можем проверить что сохранилось
file.exists("bca_analysis_dataset_clean.rds")

# И даже попробовать загрузить обратно для проверки
test_load <- readRDS("bca_analysis_dataset_clean.rds")
dim(test_load)  # Должны увидеть те же размерности

# Комментарий: фитим финальную модель
survival_fit <- orsf(
  data = bca_analysis_dataset_clean,
  formula = Surv(time, status) ~ .,
  n_tree = 500
)

# Сохраняем и модель тоже
saveRDS(survival_fit, "~/Dokumente/Statistik/Adnan/Daten/DR_Adnan_Statistik/cl_adnan/Data_Sets/combining/survival_model.rds")

# Смотрим результаты
print("Важность переменных:")
var_importance <- orsf_vi_negate(survival_fit)
print(sort(var_importance, decreasing = TRUE)[1:10])

# Комментарий: делаем partial plots для топ-5 предикторов правильно
top5_vars <- c("days_of_ventilation", "imat", "has_varices", 
               "lab_meld", "has_splenomegaly")

# Комментарий: используем summarize_uni для топ-5 переменных
model_summary <- orsf_summarize_uni(survival_fit, n_variables = 5)
print(model_summary)

# Можем также посмотреть общую производительность модели
print("C-index модели:")
print(survival_fit)

# И взаимодействия между важными переменными
interactions <- orsf_vint(survival_fit, 
                          predictors = c("days_of_ventilation", "imat", 
                                         "has_varices", "lab_meld", 
                                         "has_splenomegaly"),
                          n_thread = 1)  # Используем 1 поток для стабильности
print("Важные взаимодействия:")
print(interactions)
