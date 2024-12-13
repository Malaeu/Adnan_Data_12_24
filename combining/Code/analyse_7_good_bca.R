# Создаем различные композитные индексы
df_analysis <- df_analysis %>%
  mutate(
    # 1. Мышечно-жировые индексы
    muscle_fat_ratio = muscle / (sat + vat),
    muscle_visceral_ratio = muscle / vat,
    muscle_subcut_ratio = muscle / sat,
    
    # 2. Индекс саркопенического ожирения
    sarcopenic_obesity_index = muscle / (vat + imat),
    
    # 3. Общий индекс состава тела
    body_comp_index = (muscle + bone) / (sat + vat + imat),
    
    # 4. Нормализованные значения (z-scores)
    across(c(bone, muscle, sat, vat, imat, eat, pat, tat),
           list(z = ~scale(.)), 
           .names = "{.col}_z"),
    
    # 5. Композитный z-score
    bca_composite_z = (muscle_z - (sat_z + vat_z + imat_z)/3),
    
    # 6. Категоризация индексов
    muscle_fat_cat = cut(muscle_fat_ratio,
                         breaks = quantile(muscle_fat_ratio, 
                                           probs = c(0, 0.33, 0.66, 1),
                                           na.rm = TRUE),
                         labels = c("Low", "Medium", "High"))
  )

# Анализируем связь индексов со смертностью
indices_analysis <- df_analysis %>%
  select(surv_status, 
         muscle_fat_ratio, muscle_visceral_ratio, muscle_subcut_ratio,
         sarcopenic_obesity_index, body_comp_index, bca_composite_z) %>%
  pivot_longer(cols = -surv_status,
               names_to = "index",
               values_to = "value") %>%
  group_by(index, surv_status) %>%
  summarise(
    n = n(),
    mean_value = mean(value, na.rm = TRUE),
    sd_value = sd(value, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  pivot_wider(names_from = surv_status,
              values_from = c(n, mean_value, sd_value))

print("Анализ композитных индексов по статусу выживаемости:")
print(indices_analysis)

# Cox регрессия для индексов
cox_indices <- coxph(Surv(surv_time, surv_status) ~ 
                       muscle_fat_ratio + sarcopenic_obesity_index + 
                       body_comp_index + bca_composite_z +
                       age + sex + lab_meld,
                     data = df_analysis)

print("\nCox regression для композитных индексов:")
print(summary(cox_indices))

# ROC анализ для каждого индекса
library(pROC)
roc_list <- list()
auc_values <- data.frame(index = character(),
                         auc = numeric(),
                         ci_lower = numeric(),
                         ci_upper = numeric())

for(index in c("muscle_fat_ratio", "sarcopenic_obesity_index", 
               "body_comp_index", "bca_composite_z")) {
  roc_obj <- roc(df_analysis$surv_status, df_analysis[[index]])
  ci_obj <- ci.auc(roc_obj)
  
  auc_values <- rbind(auc_values,
                      data.frame(index = index,
                                 auc = as.numeric(auc(roc_obj)),
                                 ci_lower = ci_obj[1],
                                 ci_upper = ci_obj[3]))
  roc_list[[index]] <- roc_obj
}

print("\nAUC для композитных индексов:")
print(auc_values %>% arrange(desc(auc)))

# Анализ индексов по категориям MELD
meld_indices <- df_analysis %>%
  group_by(meld_cat) %>%
  summarise(
    n = n(),
    across(c(muscle_fat_ratio, sarcopenic_obesity_index,
             body_comp_index, bca_composite_z),
           list(
             mean = ~mean(., na.rm = TRUE),
             sd = ~sd(., na.rm = TRUE)
           ))
  )

print("\nИндексы по категориям MELD:")
print(meld_indices)

# Создаем прогностическую модель с индексами
log_model_indices <- glm(surv_status ~ 
                           muscle_fat_ratio + sarcopenic_obesity_index +
                           body_comp_index + bca_composite_z +
                           age + sex + lab_meld +
                           has_ascites + has_varices,
                         data = df_analysis,
                         family = binomial())

# Оцениваем важность предикторов
importance <- abs(coef(log_model_indices))[-1]  # исключаем интерсепт
importance_df <- data.frame(
  predictor = names(importance),
  importance = importance
) %>%
  arrange(desc(importance))

print("\nВажность предикторов в модели:")
print(importance_df)

# Визуализация распределения индексов
library(ggplot2)

# График для muscle_fat_ratio
p1 <- ggplot(df_analysis, aes(x = muscle_fat_ratio, fill = factor(surv_status))) +
  geom_density(alpha = 0.5) +
  labs(title = "Распределение muscle/fat ratio по статусу выживаемости") +
  theme_minimal()

# Сохраняем результаты
indices_results <- list(
  indices_analysis = indices_analysis,
  cox_model = cox_indices,
  roc_analysis = auc_values,
  meld_stratification = meld_indices,
  prediction_model = log_model_indices
)

saveRDS(indices_results, "bca_indices_analysis.rds")

