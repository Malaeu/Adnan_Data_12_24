#
library(rsample)
library(aorsf)
library(ranger)


prepare_data <- function(df) {
  # Выбираем нужные переменные
  selected_vars <- c(
    # Демография
    "age", "sex", "bmi",
    
    # Композиция тела
    "bone", "muscle", "sat", "vat", "imat",
    
    # Клинические показатели
    "lab_meld", "child_pugh_score", 
    "has_ascites", "has_varices",
    
    # Медицинский статус
    "icu_cat", "ventilation_cat", "catecholamine_cat",
    
    # Целевые переменные
    "surv_time", "surv_status"
  )
  
  # Создаем датасет для анализа
  analysis_df <- df[, selected_vars] %>%
    # Обработка пропущенных значений
    mutate(
      # Медианная импутация для числовых
      across(where(is.numeric), ~if_else(is.na(.), median(., na.rm = TRUE), .)),
      
      # Модальная импутация для категориальных
      across(where(is.character), ~if_else(is.na(.), mode(.), .)),
      
      # Конвертация категориальных в факторы
      across(c("sex", "icu_cat", "ventilation_cat", "catecholamine_cat"), as.factor),
      
      # Логические в факторы
      across(c("has_ascites", "has_varices"), ~factor(., levels = c(FALSE, TRUE)))
    )
  
  return(analysis_df)
}

# 2. Создаем рецепт предобработки
create_recipe <- function(df) {
  recipe(surv_status ~ ., data = df) %>%
    # Нормализация числовых
    step_normalize(all_numeric_predictors()) %>%
    # Dummy-кодирование категориальных
    step_dummy(all_nominal_predictors()) %>%
    # Удаление highly correlated
    step_corr(all_numeric_predictors(), threshold = 0.9)
}

# 3. Фитим модель
fit_survival_model <- function(df, recipe) {
  # Разбиваем на train/test
  set.seed(42)
  split <- initial_split(df, prop = 0.7)
  train_data <- training(split)
  test_data <- testing(split)
  
  # Подготавливаем данные через рецепт
  prepped_recipe <- prep(recipe)
  train_processed <- bake(prepped_recipe, new_data = train_data)
  test_processed <- bake(prepped_recipe, new_data = test_data)
  
  # Фитим ORSF модель
  model <- fit_orsf(
    trn = train_processed,
    vars = setdiff(names(train_processed), c("surv_time", "surv_status")),
    predict_horizon = 365  # 1 год
  )
  
  return(list(
    model = model,
    train = train_processed,
    test = test_processed,
    recipe = prepped_recipe
  ))
}

# 4. Считаем SHAP-подобные значения
calculate_importance <- function(model_results) {
  shap_like <- create_shap_like_values(
    data = model_results$train,
    model = model_results$model,
    n_bootstrap = 100
  )
  
  return(shap_like)
}

# Использование:
# Запускаем весь пайплайн
df_prepared <- prepare_data(bca_analysis_dataset)
recipe <- create_recipe(df_prepared)
model_results <- fit_survival_model(df_prepared, recipe)
importance_results <- calculate_importance(model_results)

# Визуализируем результаты
plot_shap_like(importance_results)
