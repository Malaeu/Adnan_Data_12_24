library(aorsf)
library(dplyr)

tune_orsf_survival <- function(data, n_splits = 500) {
  
  # Параметры для тюнинга на основе документации
  param_grid <- expand.grid(
    n_tree = c(500, 1000),  # Рекомендуется минимум 500
    mtry = c(floor(sqrt(ncol(data)-3)), floor((ncol(data)-3)/3)),  # sqrt(p) или p/3
    leaf_min_obs = c(5, 10),  # Дефолт 5
    split_rule = c("logrank", "cstat"),  # Основные правила для survival
    split_min_stat = c(3.84, 5)  # Дефолт 3.84 для logrank
  )
  
  results <- list()
  
  for(i in 1:n_splits) {
    # Monte-Carlo CV как в статье
    set.seed(i)
    split_idx <- sample(nrow(data), size = floor(0.7 * nrow(data)))
    train_data <- data[split_idx, ]
    test_data <- data[-split_idx, ]
    
    for(j in 1:nrow(param_grid)) {
      current_params <- param_grid[j,]
      
      # Фитим модель с текущими параметрами
      model <- orsf(
        data = train_data,
        formula = Surv(time, status) ~ .,
        n_tree = current_params$n_tree,
        mtry = current_params$mtry,
        leaf_min_obs = current_params$leaf_min_obs,
        split_rule = as.character(current_params$split_rule),
        split_min_stat = current_params$split_min_stat,
        importance = "none",  # Ускоряем тюнинг
        oobag_pred_type = "none",  # Ускоряем тюнинг
        n_thread = 0  # Автоматический выбор потоков
      )
      
      # Оцениваем на тестовых данных
      pred <- predict(model, test_data)
      c_index <- concordance(
        Surv(test_data$time, test_data$status) ~ pred
      )$concordance
      
      results[[length(results) + 1]] <- c(
        current_params,
        c_index = c_index,
        split = i
      )
    }
    
    if(i %% 50 == 0) {
      message(sprintf("Completed %d of %d splits", i, n_splits))
    }
  }
  
  # Преобразуем результаты в tibble
  results_df <- bind_rows(results) %>%
    group_by(n_tree, mtry, leaf_min_obs, split_rule, split_min_stat) %>%
    summarise(
      mean_c_index = mean(c_index),
      sd_c_index = sd(c_index),
      .groups = 'drop'
    ) %>%
    arrange(desc(mean_c_index))
  
  return(list(
    results = results_df,
    best_params = results_df[1,]
  ))
}
