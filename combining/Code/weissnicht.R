# Создаем пустой датафрейм для хранения результатов
vif_results <- data.frame(Variable1 = character(), Variable2 = character(), VIF = numeric())

# Список всех числовых переменных, включая survival_time
all_numeric_vars <- names(wl_combined_clean)[numeric_vars]

# Переменные, которые мы уже добавили в модель
added_vars <- c()

for (var1 in all_numeric_vars) {
  added_vars <- c(added_vars, var1)
  
  for (var2 in all_numeric_vars) {
    # Пропускаем, если переменные одинаковые или var2 уже в модели
    if (var1 == var2 || var2 %in% added_vars) {
      next
    }
    
    # Строим формулу
    formula <- as.formula(paste("survival_time ~", paste(c(added_vars, var2), collapse = " + ")))
    
    # Строим модель
    model <- tryCatch({
      lm(formula, data = wl_combined_clean)
    }, error = function(e) {
      print(paste("Ошибка при добавлении переменных:", var1, "и", var2, "-", e$message))
      return(NULL)
    })
    
    if (!is.null(model)) {
      # Рассчитываем VIF
      vif_value <- tryCatch({
        vif(model)
      }, error = function(e) {
        print(paste("Ошибка при расчете VIF для переменных:", var1, "и", var2, "-", e$message))
        return(NA)
      })
      
      # Добавляем результаты в датафрейм
      vif_results <- rbind(vif_results, data.frame(Variable1 = var1, Variable2 = var2, VIF = vif_value))
    }
  }
}

# Выводим результаты
print(vif_results)

# Выбираем переменные для модели
selected_vars <- c("age", "lab_meld_listing", "child_pugh_score", "body_composition", 
                   "days_of_icu", "days_of_ventilation", "amount_of_catecholamine",
                   "inr_listing", "bili_listing", "crea_listing", "sodium_listing")

# Строим модель
simplified_model <- lm(survival_time ~ ., data = wl_combined_clean[, c("survival_time", selected_vars)])

# Выводим результаты
print(summary(simplified_model))

# Рассчитываем VIF
print(vif(simplified_model))

