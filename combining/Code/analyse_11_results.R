library(tidyverse)
library(rms)
library(pROC)

# 1. Подготовка данных
model_data_clean <- model_data %>%
  mutate(
    # Факторы с указанием всех возможных уровней
    sex = factor(sex, levels = c("M", "W")),
    dialysis_status = factor(dialysis_cat, levels = c("0", "1", "2")),
    has_varices = factor(has_varices, levels = c(FALSE, TRUE)),
    hcc = factor(hcc, levels = c(0, 1))
  )

# 2. Проверка уровней факторов
print("Уровни факторов в model_data_clean:")
sapply(model_data_clean[c("sex", "dialysis_status", "has_varices", "hcc")], levels)

# 3. Разделение на обучающую и тестовую выборки
set.seed(123)
train_index <- sample(1:nrow(model_data_clean), 0.7 * nrow(model_data_clean))
train_data <- model_data_clean[train_index, ]
test_data <- model_data_clean[-train_index, ]

# 2. Создание design матрицы БЕЗ dialysis_status2
vars <- c("lab_meld", "sex", "has_varices", "hcc", "muscle_fat_ratio")

train_design <- model.matrix(~ . , data = train_data[, vars])[, -1]
train_data_design <- cbind(train_data$surv_status, train_design)
colnames(train_data_design)[1] <- "surv_status"
train_data_design <- as.data.frame(train_data_design)

test_design <- model.matrix(~ . , data = test_data[, vars])[, -1]
test_data_design <- cbind(test_data$surv_status, test_design)
colnames(test_data_design)[1] <- "surv_status"
test_data_design <- as.data.frame(test_data_design)

formula <- as.formula(paste("surv_status ~", paste(colnames(train_design), collapse = "+")))

# 3. Создание модели
f <- lrm(formula, data = train_data_design)

# 4. Предсказание и оценка
test_pred <- predict(f, newdata = test_data_design, type = "fitted")
test_roc <- roc(test_data_design$surv_status, test_pred)

print(paste("Validation AUC:", round(auc(test_roc), 3)))

# 7. Сохранение результатов
results <- list(
  model = f,
  validation = list(auc = auc(test_roc)),
  design_matrices = list(train = train_design, test = test_design)
)

saveRDS(results, "mortality_risk_model_design_matrix.rds")

