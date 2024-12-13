library(tidyverse)
library(caret)
library(pROC)
library(parallel)
library(doParallel)
library(car)
library(gtsummary)
library(gt)
library(funModeling)
# Загружаем данные
df <- readRDS("wl_df_with_bca.rds")

# Определяем группы переменных для анализа
binary_vars <- c(
  "surv_status", "has_ascites", "has_varices",
  "has_encephalopathy", "has_splenomegaly",
  "is_relisting", "received_ltx"
)

ordinal_vars <- c(
  "age_group", "meld_group", "bmi_group", "period"
)

numeric_vars <- c(
  "age", "lab_meld", "bmi", "bone", "muscle",
  "sat", "vat", "imat", "eat", "pat", "tat"
)

# Подготавливаем данные
df_analysis <- df %>%
  # Фильтруем только случаи с циррозом
  filter(str_detect(diagnosis_grouped, "Cirrhosis")) %>%
  mutate(
    # Бинарные переменные в факторы
    across(all_of(binary_vars),
           ~factor(., levels = c(FALSE, TRUE), labels = c("No", "Yes"))),

    # Преобразуем пол
    sex = factor(sex, levels = c("M", "W"), labels = c("Male", "Female")),

    # Числовые переменные
    across(all_of(numeric_vars), as.numeric)
  )

# Создаем метки для переменных
var_labels <- list(
  surv_status ~ "Mortality",
  age ~ "Age",
  sex ~ "Sex",
  lab_meld ~ "MELD score",
  bmi ~ "BMI",
  has_ascites ~ "Ascites",
  has_varices ~ "Varices",
  has_encephalopathy ~ "Encephalopathy",
  has_splenomegaly ~ "Splenomegaly",
  bone ~ "Bone tissue",
  muscle ~ "Muscle tissue",
  sat ~ "Subcutaneous adipose tissue",
  vat ~ "Visceral adipose tissue",
  imat ~ "Intermuscular adipose tissue",
  eat ~ "Epicardial adipose tissue",
  pat ~ "Paraaortic adipose tissue",
  tat ~ "Total adipose tissue",
  diagnosis_grouped ~ "Cirrhosis type"
)

# Создаем таблицу характеристик
tbl_univariate <- df_analysis %>%
  select(surv_status, age, sex, lab_meld, bmi,
         has_ascites:has_splenomegaly,
         bone:tat, diagnosis_grouped) %>%
  tbl_summary(
    by = surv_status,
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = all_continuous() ~ 1,
    label = var_labels
  ) %>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 3)) %>%
  add_overall() %>%
  add_n() %>%
  modify_header(label = "**Characteristic**") %>%
  modify_spanning_header(all_stat_cols() ~ "**Mortality**") %>%
  modify_caption("**Table 1. Patient characteristics by mortality status**")

# Создаем модель логистической регрессии
log_model <- df_analysis %>%
  select(surv_status, age, sex, lab_meld, bmi,
         has_ascites:has_splenomegaly,
         bone:tat, diagnosis_grouped) %>%
  glm(surv_status ~ ., data = ., family = binomial())

# Создаем таблицу мультивариатного анализа
tbl_multivariate <- log_model %>%
  tbl_regression(
    exponentiate = TRUE,
    label = var_labels
  ) %>%
  modify_header(label = "**Predictor**") %>%
  modify_caption("**Table 2. Logistic regression results**")

# Объединяем таблицы
tbl_merged <- tbl_merge(
  tbls = list(tbl_univariate, tbl_multivariate),
  tab_spanner = c("**Univariate analysis**", "**Multivariate analysis**")
)

# Выводим и сохраняем результаты
tbl_merged %>%
  as_gt() %>%
  gtsave("cirrhosis_analysis_table.html")

# Функция для бутстрап-анализа
run_bootstrap_analysis <- function(df, num_iterations = 1000) {
  results <- vector("list", num_iterations)

  for(i in 1:num_iterations) {
    # Бутстрап-выборка
    boot_indices <- sample(1:nrow(df), replace = TRUE)
    boot_data <- df[boot_indices, ]

    # Модель
    boot_model <- glm(surv_status ~ ., data = boot_data, family = binomial())

    # Сохраняем коэффициенты
    results[[i]] <- coef(boot_model)

    if(i %% 100 == 0) cat("Completed iteration:", i, "\n")
  }

  return(results)
}

# Запускаем бутстрап
bootstrap_results <- run_bootstrap_analysis(
  df_analysis %>%
    select(surv_status, age, sex, lab_meld, bmi,
           has_ascites:has_splenomegaly,
           bone:tat)
)

# Анализируем результаты бутстрапа
bootstrap_summary <- do.call(rbind, bootstrap_results) %>%
  as.data.frame() %>%
  summarise(across(everything(),
                   list(
                     mean = mean,
                     sd = sd,
                     ci_lower = ~quantile(., 0.025),
                     ci_upper = ~quantile(., 0.975)
                   )))

print("Bootstrap analysis results:")
print(bootstrap_summary)
