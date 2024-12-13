# Комментарий: Подрубаем все нужные библиотеки через pacman
if (!require(pacman)) install.packages("pacman")
pacman::p_load(
  rsample, recipes, survival, ranger, aorsf, dplyr, 
  tidyr, ggplot2, pROC, survminer, lubridate
)

# Комментарий: Улучшенная функция для очистки данных
clean_dataset <- function(data) {
  data %>%
    # Убираем ненужные колонки и даты
    select(-matches("(etnr|date|survival_days|cause_of_death|todesdatum|last_contact)")) %>%
    # Конвертируем логические в факторы
    mutate(across(
      starts_with("has_"),
      ~factor(.x, levels = c(FALSE, TRUE), labels = c("No", "Yes"))
    )) %>%
    # Конвертируем числовые
    mutate(across(
      c(matches("(inr|bili|crea|sodium)_listing"), 
        "days_of_icu", "days_of_ventilation"),
      ~as.numeric(as.character(.x))
    )) %>%
    # Убираем все POSIXct колонки
    select_if(~!inherits(., "POSIXct"))
}

# Комментарий: Рецепт для препроцессинга
make_recipe <- function(data) {
  recipe(time + status ~ ., data) %>%
    update_role(ID, new_role = 'ID') %>%
    step_novel(all_nominal(), -all_outcomes()) %>%
    step_impute_median(all_numeric(), -all_outcomes()) %>%
    step_impute_mode(all_nominal(), -all_outcomes()) %>%
    step_nzv(all_predictors()) %>%
    step_dummy(all_nominal(), -all_outcomes(), one_hot = FALSE)
}

# Комментарий: Функция для тюнинга ORSF
tune_orsf_survival <- function(train_data, test_data, formula_str, n_splits = 100) {
  # Проверяем и фильтруем данные по времени
  train_data <- train_data %>% 
    filter(time > 0)
  
  test_data <- test_data %>% 
    filter(time > 0)
  
  print(paste("После фильтрации времени > 0:", 
              "train:", nrow(train_data), 
              "test:", nrow(test_data)))
  
  # Убираем константные колонки
  var_info <- sapply(train_data, function(x) length(unique(x)))
  train_data <- train_data[, var_info > 1]
  test_data <- test_data[, names(train_data)]
  
  # Проверяем и убираем проблемные колонки
  train_data <- train_data %>%
    select_if(~!inherits(., "POSIXct"))
  
  test_data <- test_data %>%
    select_if(~!inherits(., "POSIXct"))
  
  # Проверяем данные перед тюнингом
  print("Проверка данных перед тюнингом:")
  print(summary(train_data$time))
  print(table(train_data$status))
  
  # Обновленная сетка параметров
  param_grid <- expand.grid(
    n_tree = c(1000, 2000),  # Больше деревьев для стабильности
    mtry = c(
      floor(sqrt(ncol(train_data)-3)),  # стандартный sqrt(p)
      floor((ncol(train_data)-3)/3),    # p/3
      floor((ncol(train_data)-3)/2)     # p/2
    ),
    leaf_min_obs = c(5, 10, 15),  # Больше вариантов для размера листа
    split_rule = c("logrank", "cstat"),
    # Добавляем параметры для работы с дисбалансом
    split_min_events = c(3, 5),    # Меньше минимальных событий
    leaf_min_events = c(1, 2)      # Меньше событий в листе
  )
  
  results <- list()
  
  for(i in 1:n_splits) {
    # Monte-Carlo CV
    set.seed(i)
    split_idx <- sample(nrow(train_data), size = floor(0.7 * nrow(train_data)))
    cv_train <- train_data[split_idx, ]
    cv_test <- train_data[-split_idx, ]
    
    # Проверяем CV сплиты
    if(any(cv_train$time <= 0) || any(cv_test$time <= 0)) {
      print("Пропускаем сплит из-за некорректного времени")
      next
    }
    
    # Остальной код тот же, но добавляем новые параметры в fit
    for(j in 1:nrow(param_grid)) {
      current_params <- param_grid[j,]
      
      tryCatch({
        model <- orsf(
          data = cv_train,
          formula = as.formula(formula_str),
          n_tree = current_params$n_tree,
          mtry = current_params$mtry,
          leaf_min_obs = current_params$leaf_min_obs,
          split_rule = as.character(current_params$split_rule),
          split_min_events = current_params$split_min_events,
          leaf_min_events = current_params$leaf_min_events,
          importance = "none",
          oobag_pred_type = "none",
          n_thread = 0
        )
        
        # Оцениваем на CV тесте
        pred <- predict(model, cv_test)
        c_index <- concordance(
          Surv(cv_test$time, cv_test$status) ~ pred
        )$concordance
        
        results[[length(results) + 1]] <- data.frame(
          n_tree = current_params$n_tree,
          mtry = current_params$mtry,
          leaf_min_obs = current_params$leaf_min_obs,
          split_rule = as.character(current_params$split_rule),
          c_index = c_index,
          split = i
        )
      }, error = function(e) {
        print(paste("Ошибка при фите:", e$message))
      })
    }
    
    if(i %% 10 == 0) {
      print(paste("Завершено", i, "из", n_splits, "сплитов"))
    }
  }
  
  # Проверяем, есть ли результаты
  if(length(results) == 0) {
    stop("Не удалось получить результаты тюнинга")
  }
  
  # Находим лучшие параметры
  results_df <- bind_rows(results) %>%
    group_by(n_tree, mtry, leaf_min_obs, split_rule) %>%
    summarise(
      mean_c_index = mean(c_index),
      sd_c_index = sd(c_index),
      n_success = n(),
      .groups = 'drop'
    ) %>%
    arrange(desc(mean_c_index))
  
  print("Результаты тюнинга:")
  print(results_df)
  
  best_params <- results_df[1,]
  
  # В финальной модели используем все оптимальные параметры
  final_model <- orsf(
    data = train_data,
    formula = as.formula(formula_str),
    n_tree = best_params$n_tree,
    mtry = best_params$mtry,
    leaf_min_obs = best_params$leaf_min_obs,
    split_rule = as.character(best_params$split_rule),
    split_min_events = best_params$split_min_events,
    leaf_min_events = best_params$leaf_min_events,
    importance = "permute",
    n_thread = 0
  )
  
  return(list(
    model = final_model,
    tuning_results = results_df,
    best_params = best_params
  ))
}

# Комментарий: Обновленная функция fit_orsf
fit_orsf <- function(data, vars, test_data, horizon) {
  print("Тюнинг ORSF...")
  
  # Проверяем данные
  print("Проверка данных:")
  print(summary(data$time))
  print(table(data$status))
  
  # Убираем константные колонки из vars
  var_info <- sapply(data[vars], function(x) length(unique(x)))
  vars <- vars[var_info > 1]
  
  formula_str <- paste("Surv(time, status) ~", paste(vars, collapse = " + "))
  print("Используемая формула:")
  print(formula_str)
  
  tuned_orsf <- tune_orsf_survival(data, test_data, formula_str)
  
  print("Лучшие параметры:")
  print(tuned_orsf$best_params)
  
  return(tuned_orsf$model)
}



fit_cph <- function(data, vars, test_data, horizon) {
  formula_str <- paste("Surv(time, status) ~", paste(vars, collapse = " + "))
  
  coxph(
    as.formula(formula_str),
    data = data,
    ties = "efron",
    control = coxph.control(iter.max = 100)
  )
}

fit_rsf <- function(data, vars, test_data, horizon) {
  formula_str <- paste("Surv(time, status) ~", paste(vars, collapse = " + "))
  
  ranger(
    formula = as.formula(formula_str),
    data = data,
    num.trees = 1000,
    mtry = floor(sqrt(length(vars))),
    min.node.size = 5,
    splitrule = "extratrees",
    importance = "permutation",
    num.threads = parallel::detectCores() - 1
  )
}

# Комментарий: Функция для фита всех моделей
fit_models <- function(train_data, test_data, vars) {
  print("Фитим ORSF...")
  orsf_model <- fit_orsf(train_data, vars, test_data, 365)
  
  print("Фитим Cox...")
  cph_model <- fit_cph(train_data, vars, test_data, 365)
  
  print("Фитим RSF...")
  rsf_model <- fit_rsf(train_data, vars, test_data, 365)
  
  list(
    orsf = orsf_model,
    cph = cph_model,
    rsf = rsf_model
  )
}

# Комментарий: Функция предсказания
predict_model <- function(model, test_data, model_type) {
  tryCatch({
    if (model_type == "rsf") {
      pred <- predict(model, data = test_data)$predictions
    } else if (model_type == "cph") {
      pred <- predict(model, newdata = test_data, type = "risk")
    } else if (model_type == "orsf") {
      pred <- predict(model, test_data)$predictions
    }
    return(pred)
  }, error = function(e) {
    print(paste("Ошибка в предсказании для модели", model_type, ":", e$message))
    return(NULL)
  })
}

# Комментарий: Проверка данных
check_data <- function(data) {
  if ("surv_time" %in% names(data) && "surv_status" %in% names(data)) {
    data <- data %>%
      rename(
        time = surv_time,
        status = surv_status
      )
  } else if (!all(c("time", "status") %in% names(data))) {
    stop("Нужны колонки time/status или surv_time/surv_status")
  }
  
  print("Проверка типов данных:")
  print(str(data[1:10]))
  
  return(data)
}

# Комментарий: Основной workflow
main_workflow <- function(data) {
  # Проверяем данные
  data <- check_data(data)
  
  # Чистим данные
  print("Чистим данные...")
  clean_data <- clean_dataset(data)
  clean_data$ID <- seq_len(nrow(clean_data))
  
  # Разбиваем выборку
  print("Разбиваем данные...")
  set.seed(42)
  split <- initial_split(clean_data, prop = 0.7)
  train_raw <- training(split)
  test_raw <- testing(split)
  
  # Готовим рецепт
  print("Готовим рецепт...")
  recipe_obj <- make_recipe(train_raw)
  recipe_prepped <- prep(recipe_obj, training = train_raw)
  
  # Применяем рецепт
  print("Применяем рецепт...")
  train_data <- bake(recipe_prepped, new_data = train_raw)
  test_data <- bake(recipe_prepped, new_data = test_raw)
  
  print("Размерности наборов данных:")
  print(paste("Тренировочный:", dim(train_data)[1], "x", dim(train_data)[2]))
  print(paste("Тестовый:", dim(test_data)[1], "x", dim(test_data)[2]))
  
  # Фитим модели
  print("Фитим модели...")
  vars <- setdiff(names(train_data), c("time", "status", "ID"))
  models <- fit_models(train_data, test_data, vars)
  
  # Оцениваем результаты
  print("Считаем метрики...")
  model_metrics <- sapply(names(models), function(model_name) {
    if (!is.null(models[[model_name]])) {
      preds <- predict_model(models[[model_name]], test_data, model_name)
      if (!is.null(preds)) {
        c_index <- concordance(
          Surv(test_data$time, test_data$status) ~ preds
        )$concordance
        return(c(c_index = c_index))
      }
    }
    return(c(c_index = NA))
  })
  
  print("Метрики моделей:")
  print(model_metrics)
  
  # Сохраняем предсказания
  predictions <- list()
  for (model_name in names(models)) {
    if (!is.null(models[[model_name]])) {
      preds <- predict_model(models[[model_name]], test_data, model_name)
      predictions[[model_name]] <- preds
    }
  }
  
  list(
    models = models,
    train = train_data,
    test = test_data,
    recipe = recipe_prepped,
    metrics = model_metrics,
    predictions = predictions
  )
}

# Запускаем
results <- main_workflow(bca_analysis_dataset)

# Визуализация результатов
if (!is.null(results$predictions)) {
  predictions_df <- data.frame(
    time = results$test$time,
    status = results$test$status
  )
  
  for (model_name in names(results$predictions)) {
    if (!is.null(results$predictions[[model_name]])) {
      predictions_df[[paste0(model_name, "_pred")]] <- results$predictions[[model_name]]
    }
  }
  
  # ROC кривые
  print("Строим ROC кривые...")
  for (model_name in names(results$predictions)) {
    if (!is.null(results$predictions[[model_name]])) {
      roc_obj <- roc(predictions_df$status ~ predictions_df[[paste0(model_name, "_pred")]])
      plot(roc_obj, main = paste("ROC curve for", model_name))
      print(paste("AUC for", model_name, ":", auc(roc_obj)))
    }
  }
}
